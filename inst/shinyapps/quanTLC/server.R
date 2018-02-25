
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#### Setup ####
require(quanTLC,quietly=TRUE)
require(rhandsontable)
require(pracma)

options(shiny.maxRequestSize=1000*1024^2)

dimension = data.frame(row.names = c(
  "Plate length [mm]",
  "Plate width [mm]",
  "Cropping [mm]",
  "Migration front [mm]",
  "Distance to lower edge [mm]",
  "First application position [mm]",
  "Band length [mm]",
  "Distance between tracks [mm]",
  "Edge cut [mm]",
  "Number of bands"),
  Value = as.numeric(c(100,100,0,70,8,27,6,2,2,5)))

default_preprocess_options = list(
  Smoothing=list(window.size = 15,poly.order=1,diff.order=0),
  Warping=list(warpmethod = "ptw",ptw.warp.ref = 1),
  Baseline.correction=list(method="als",lambda.1=5,p=0.05,maxit.1=20),
  medianFilter=3,
  gammaCorrection=2
)

shinyServer(function(input, output,session) {
  source("Preprocess.function.R")
  source("server_preproces.R",local = T) ## in a different file as too big, come from rTLC
  source("chrom.pict.R")
  source("f.eat.image.R")
  source("f.integrate_extracted.R")
  source("f.plot.array.R")
  source("f.read.image.R")
  source("raster.R")
  source("f.index_to_hrf.R")
  updateTextInput(session, "Integration_compound", value = "Compound")

  reac = reactiveValues(
    image = f.read.image("www/plate_168_dev_RT_10ms.jpg"),
    image.name = NULL,
    img_click = 0,
    batch = NULL,
    dimension = dimension,
    convention=F,
    double=F,
    nbr.band=NULL,
    extracted = NULL,
    Preprocess.order = NULL,
    Preprocess.options = default_preprocess_options,
    preprocessed = NULL,
    Integration = list(PeakList = data.frame()),
    model=NULL,
    Integration_selection = NULL ## this one is not to be saved
  )
  output$downloadCheckpoint <- downloadHandler(
    filename = "quanTLC.Rdata",
    content = function(con) {
      assign('data',list(image = reac$image,
                         image.name = reac$image.name,
                         batch = reac$batch,
                         dimension = reac$dimension,
                         convention=reac$convention,
                         double = reac$double,
                         nbr.band=reac$nbr.band,
                         extracted = reac$extracted,
                         Preprocess.order = reac$Preprocess.order,
                         Preprocess.options = reac$Preprocess.options,
                         preprocessed = reac$preprocessed,
                         Integration = reac$Integration,
                         model=reac$model
      ))
      save(list='data',file=con)
    }
  )
  observeEvent(input$Input_image,{
    if(grepl("Rdata",input$Input_image$name)){
      load(input$Input_image$datapath)
      reac$image = data$image
      reac$image.name = data$image.name
      reac$batch = data$batch
      reac$dimension = data$dimension
      if(!is.null(data$convention)){reac$convention=data$convention}
      if(!is.null(data$double)){reac$double=data$double}
      if(!is.null(data$nbr.band)){reac$nbr.band = data$nbr.band}
      reac$extracted = data$extracted
      reac$Preprocess.order = data$Preprocess.order
      reac$Preprocess.options = data$Preprocess.options
      reac$preprocessed = data$preprocessed
      if(!is.null(reac$Integration)){
        reac$Integration = data$Integration
      }else{
        reac$Integration = list(PeakList = data.frame())
        shinyalert("Warning!", "Rdata file from a previous version, integration not saved.", type = "warning", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
      }
      updateCheckboxInput(session,"Input_convention",value=reac$convention)
      updateCheckboxInput(session,"Input_double",value=reac$double)
      reac$model=data$model
      
      ## preprocess options
      updateSelectizeInput(session,"Preprocess.order",
                           selected=reac$Preprocess.order)
      updateNumericInput(session,"window.size",value = reac$Preprocess.options$Smoothing$window.size)
      updateNumericInput(session,"poly.order",value = reac$Preprocess.options$Smoothing$poly.order)
      updateNumericInput(session,"diff.order",value = reac$Preprocess.options$Smoothing$diff.order)
      updateSelectizeInput(session,"baseline", "Type of baseline",
                           selected = reac$Preprocess.options$Baseline.correction$method)
      for(i in names(reac$Preprocess.options$Baseline.correction)){
        if(i != "method"){
          updateNumericInput(session,i,value = as.numeric(reac$Preprocess.options$Baseline.correction[i]))
        }
      }
      updateSelectizeInput(session,"warpmethod","Warping method",#choices=(c("ptw",'dtw')),
                           selected = reac$Preprocess.options$Warping$warpmethod)
      if(reac$Preprocess.options$Warping$warpmethod == "ptw"){
        updateNumericInput(session,"ptw.warp.ref",value = reac$Preprocess.options$Warping$ptw.warp.ref)
      }else{
        updateNumericInput(session,"dtw.warp.ref",value = reac$Preprocess.options$Warping$dtw.warp.ref)
        updateCheckboxInput(session,"dtw.split",value = reac$Preprocess.options$Warping$dtw.split)
      }
      
      ## integration options
      updateNumericInput(session,"Integration_nups",value = data$Integration$Integration_nups)
      updateNumericInput(session,"Integration_nups",value = data$Integration$Integration_nups)
      updateNumericInput(session,"Integration_nups",value = data$Integration$Integration_nups)
      updateNumericInput(session,"Integration_nups",value = data$Integration$Integration_nups)
        
      
    }else{
      reac$image =f.read.image(input$Input_image$datapath)
      reac$image.name = input$Input_image$name
      reac$convention = F
      reac$double = F
      reac$nbr.band = NULL
      reac$batch = NULL
      reac$extracted = NULL
      reac$Preprocess.order = NULL
      reac$Preprocess.options = NULL
      reac$preprocessed = NULL
      reac$Integration = list(PeakList = data.frame())
      reac$model=NULL
    }
    
  })

  observeEvent(input$Demo_file,{
    if(input$Demo_file != "None"){
      load(paste0("www/",input$Demo_file))
      reac$image = data$image
      reac$image.name = data$image.name
      reac$batch = data$batch
      reac$dimension = data$dimension
      reac$convention = data$convention
      if(!is.null(data$double)) reac$double = data$double
      if(!is.null(data$double)){reac$nbr.band = data$nbr.band}else{reac$nbr.band = reac$dimension["Number of bands",]}
      reac$extracted = data$extracted
      reac$Preprocess.order = data$Preprocess.order
      reac$Preprocess.options = data$Preprocess.options
      reac$preprocessed = data$preprocessed
      reac$Integration = data$Integration
      reac$model=data$model
      
      updateCheckboxInput(session,"Input_convention",value=reac$convention)
      updateCheckboxInput(session,"Input_double",value=reac$double)
      
      ## need to update preprocess options also
      updateSelectizeInput(session,"Preprocess.order",
                           selected=reac$Preprocess.order)
      # if("Smoothing" %in% reac$Preprocess.options){
      updateNumericInput(session,"window.size",value = reac$Preprocess.options$Smoothing$window.size)
      updateNumericInput(session,"poly.order",value = reac$Preprocess.options$Smoothing$poly.order)
      updateNumericInput(session,"diff.order",value = reac$Preprocess.options$Smoothing$diff.order)
      # }
      # if("Baseline.correction" %in% reac$Preprocess.options){
      updateSelectizeInput(session,"baseline", "Type of baseline", #choices=c("als","fillPeaks","irls","lowpass","medianWindow","modpolyfit","peakDetection","rollingBall"),
                           selected = reac$Preprocess.options$Baseline.correction$method)
      for(i in names(reac$Preprocess.options$Baseline.correction)){
        if(i != "method"){
          updateNumericInput(session,i,value = as.numeric(reac$Preprocess.options$Baseline.correction[i]))
        }
      }
      updateSelectizeInput(session,"warpmethod","Warping method",#choices=(c("ptw",'dtw')),
                           selected = reac$Preprocess.options$Warping$warpmethod)
      if(reac$Preprocess.options$Warping$warpmethod == "ptw"){
        updateNumericInput(session,"ptw.warp.ref",value = reac$Preprocess.options$Warping$ptw.warp.ref)
      }else{
        updateNumericInput(session,"dtw.warp.ref",value = reac$Preprocess.options$Warping$dtw.warp.ref)
        updateCheckboxInput(session,"dtw.split",value = reac$Preprocess.options$Warping$dtw.split)
      }
    }
  })
  #### Input ####
  output$Input_dimension = renderRHandsontable({
    rhandsontable(reac$dimension, rowHeaderWidth = 200) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })
  observeEvent(input$Input_action,{
    reac$dimension$Value = as.numeric(hot_to_r(input$Input_dimension)$Value)
    nbr.band=reac$dimension["Number of bands",]
    largeur = reac$dimension["Plate length [mm]",]
    dist.gauche=reac$dimension["First application position [mm]",]
    tolerance=reac$dimension["Edge cut [mm]",]
    band=reac$dimension["Band length [mm]",]
    ecart=reac$dimension["Distance between tracks [mm]",]
    Cropping=reac$dimension["Cropping [mm]",]
    # validate( ## need shinyalert
    #   need(nbr.band < ceiling((largeur-dist.gauche)/(band+ecart)),"Too much bands."),
    #   need(nbr.band >= 1,"Not enough bands.")
    # )
    if(input$Input_convention){ # this put everybody back to linomat convention
      dist.gauche<-dist.gauche-band/2
      ecart<-ecart-band
    }
    if(nbr.band >= ceiling((largeur-dist.gauche)/(band+ecart))){
      shinyalert("Error!", "Too much bands.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
    }else if(nbr.band < 1){
      shinyalert("Error!", "Not enough bands.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
    }else{
      reac$extracted = f.eat.image(reac$image,"linomat",largeur=largeur,dist.gauche=dist.gauche,band = band,
                                   ecart = ecart,tolerance = tolerance,nbr.band = nbr.band,cropping = Cropping,double = input$Input_double)
      reac$convention = input$Input_convention
      reac$double = input$Input_double
      reac$nbr.band = if(reac$double){nbr.band*2}else{nbr.band}
      reac$batch = data.frame(Track = paste0("Track ",seq(reac$nbr.band)),Standard = rep(T,reac$nbr.band),quantity = seq(reac$nbr.band))
      colnames(reac$batch)[3] = "Quantity [AU]"
      reac$Preprocess.order = NULL
      reac$Preprocess.options = default_preprocess_options
      reac$preprocessed = NULL
      reac$Integration =list(PeakList = data.frame())
      reac$model=NULL
    }
    
  })
  output$Input_plot_raster = renderPlot({
    validate(need(!is.null(reac$image),"Upload the image chromatogram."))
    validate(need(!is.null(input$Input_dimension),"Not ready yet."))
    par(mar=c(0,0,0,0),xaxs="i",yaxs="i")

    dimension = dimension
    dimension$Value = as.numeric(hot_to_r(input$Input_dimension)$Value)

    Cropping = dimension["Cropping [mm]",]
    nbr.band=dimension["Number of bands",]
    largeur = dimension["Plate length [mm]",]
    dist.gauche=dimension["First application position [mm]",]
    tolerance=dimension["Edge cut [mm]",]
    band=dimension["Band length [mm]",]
    ecart=dimension["Distance between tracks [mm]",]
    width = dimension["Plate width [mm]",]
    Zf = dimension["Migration front [mm]",]
    dist.bas = dimension["Distance to lower edge [mm]",]
    # cropping correction
    largeur = largeur - 2 * Cropping
    dist.gauche = dist.gauche - Cropping
    if(input$Input_convention){ # this put everybody back to linomat convention
      dist.gauche<-dist.gauche-band/2
      ecart<-ecart-band
    }
    
    validate(
      need(nbr.band < ceiling((largeur-dist.gauche)/(band+ecart)),"Too much bands."),
      need(nbr.band >= 1,"Not enough bands.")
      )
    

    raster(reac$image)#,xlim= c(0,200/largeur*dim(reac$image)[2]))
    if(reac$double){
      abline(h=dim(reac$image)[1]/width*dist.bas)
      abline(h=dim(reac$image)[1]/width*(width-dist.bas))
      abline(h=dim(reac$image)[1]/width*Zf)
    }else{
      abline(h=dim(reac$image)[1]/width*dist.bas)
      abline(h=dim(reac$image)[1]/width*Zf)
    }

    for(i in c(0:(nbr.band-1))){
      abline(v=(dim(reac$image)[2]/largeur*((dist.gauche+tolerance)+i*(band+ecart))),col="green")
      abline(v=(dim(reac$image)[2]/largeur*((dist.gauche+band-tolerance)+i*(band+ecart))),col="red")
    }
  })
  observeEvent(input$Input_plot_raster_click,{
    reac$dimension$Value = as.numeric(hot_to_r(input$Input_dimension)$Value)
    nbr.band=reac$dimension["Number of bands",]
    largeur = reac$dimension["Plate length [mm]",]
    band=reac$dimension["Band length [mm]",]
    dist.gauche=reac$dimension["First application position [mm]",]
    if(reac$img_click == 0){
      reac$dimension["First application position [mm]",] = largeur/dim(reac$image)[2]*as.numeric(input$Input_plot_raster_click$x)
      updateCheckboxInput(session,"Input_convention",value=T)
      reac$img_click = 1
    }else{
      reac$dimension["Distance between tracks [mm]",] = (largeur/dim(reac$image)[2]*as.numeric(input$Input_plot_raster_click$x)-dist.gauche)/(nbr.band-1)
      reac$img_click = 0
    }
    
    # shinyalert("Oops!", "Feature not yet implemented.", type = "error")
  })

  ### Preprocess ####
  output$Preprocess_plot_chrom_before = renderPlot({
    validate(need(!is.null(reac$extracted),"Extract the video densitograms."),
             need(input$Preprocess_plot_chrom_select > 0 && input$Preprocess_plot_chrom_select <= reac$nbr.band,"Wrong track selection"))
    width = reac$dimension["Plate width [mm]",];if(reac$double){width=0.5*width}
    Zf = reac$dimension["Migration front [mm]",]
    dist.bas = reac$dimension["Distance to lower edge [mm]",]
    par(mar=c(3,3,2,0),mgp=c(1.5,0.5,0))
    f.plot.array(reac$extracted,id = as.numeric(input$Preprocess_plot_chrom_select),
                 hauteur = width,Zf = Zf,dist.bas = dist.bas,reconstruct = T,main="Before preprocessing")
  })
  output$Preprocess_plot_chrom_after = renderPlot({
    validate(
      need(input$window.size %% 2 == 1, "The window size must be an odd value."),
      need(input$window.size > input$poly.order, "The window size must be greater than the polynomial order."),
      need(input$poly.order > input$diff.order, "The polynomial order must be greater than the differential order."),
      need(input$Preprocess_plot_chrom_select > 0 && input$Preprocess_plot_chrom_select <= reac$nbr.band,"Wrong track selection.")
    )
    validate(need(!is.null(reac$preprocessed),"Preprocess the video densitograms."))
    width = reac$dimension["Plate width [mm]",];if(reac$double){width=0.5*width}
    Zf = reac$dimension["Migration front [mm]",]
    dist.bas = reac$dimension["Distance to lower edge [mm]",]
    par(mar=c(2.5,2.5,2,0),mgp=c(1.5,0.5,0))
    f.plot.array(reac$preprocessed,id = as.numeric(input$Preprocess_plot_chrom_select),
                 hauteur = width,Zf = Zf,dist.bas = dist.bas,reconstruct = F,main="After preprocessing")
    abline(h=0)
  })

  outputOptions(output, "Preprocess_ui_1", suspendWhenHidden = FALSE)

  observeEvent(input$Preprocess_action,{
    # validate( ## need shinyalert
    #   need(input$window.size %% 2 == 1, "The window size must be an odd value."),
    #   need(input$window.size > input$poly.order, "The window size must be greater than the polynomial order."),
    #   need(input$poly.order > input$diff.order, "The polynomial order must be greater than the differential order."),
    #   need(!is.null(reac$extracted),"Video densitograms not extracted.")
    # )
    if(input$window.size %% 2 != 1){
      shinyalert("Error!", "The window size must be an odd value.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
    }else if(!(input$window.size > input$poly.order)){
      shinyalert("Error!", "The window size must be greater than the polynomial order.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
    }else if(!(input$poly.order > input$diff.order)){
      shinyalert("Error!", "The polynomial order must be greater than the differential order.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
    }else if(is.null(reac$extracted)){
      shinyalert("Error!", "Video densitograms not extracted.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
    }else{
      Smoothing <- list(window.size = input$window.size,poly.order=input$poly.order,diff.order=input$diff.order)
      if(input$warpmethod == 'ptw'){
        Warping <- list(warpmethod = input$warpmethod,
                        ptw.warp.ref = as.numeric(input$ptw.warp.ref)
        )
      }
      if(input$warpmethod == 'dtw'){
        Warping <- list(warpmethod = input$warpmethod,
                        dtw.warp.ref = as.numeric(input$dtw.warp.ref),
                        dtw.split = input$dtw.split
        )
      }
      if(input$baseline == "als"){Baseline <- list(method=input$baseline,lambda.1=input$lambda.1,p=input$p,maxit.1=input$maxit.1)}
      if(input$baseline == "fillPeaks"){Baseline <- list(method=input$baseline,lambda.2=input$lambda.2,hwi=input$hwi,it=input$it,int=input$int)}
      if(input$baseline == "irls"){Baseline <- list(method=input$baseline,lambda1=input$lambda1,lambda2=input$lambda2,maxit.2=input$maxit.2,wi=input$wi)}
      if(input$baseline == "lowpass"){Baseline <- list(method=input$baseline,steep=input$steep,half=input$half)}
      if(input$baseline == "medianWindow"){Baseline <- list(method=input$baseline,hwm=input$hwm,hws=input$hws,end=input$end)}
      if(input$baseline == "modpolyfit"){Baseline <- list(method=input$baseline,degree=input$degree,tol=input$tol,rep=input$rep)}
      if(input$baseline == "peakDetection"){Baseline <- list(method=input$baseline,left=input$left,right=input$right,lwin=input$lwin,rwin=input$rwin)}
      if(input$baseline == "rfBaseline"){Baseline <- list(method=input$baseline)}
      if(input$baseline == "rollingBall"){Baseline <- list(method=input$baseline,wm=input$wm,ws=input$ws)}
      reac$Preprocess.options = list(Smoothing=Smoothing,Warping=Warping,Baseline.correction=Baseline,
                                     medianFilter=input$preprocess.medianfilter,gammaCorrection=input$preprocess.gammacorrection)
  
      reac$Preprocess.order = input$Preprocess.order
      reac$preprocessed = f.preprocess(reac$extracted,preprocess.order = reac$Preprocess.order,preprocess.option = reac$Preprocess.options)
      reac$batch = reac$batch[,1:3]
      reac$Integration = list(PeakList = data.frame())
      reac$model=NULL
    }
  })


  ### Integration ####
  output$Integration_ui_1 = renderUI({
    tagList(
      numericInput("Integration_nups","Minimum number of increasing steps before a peak is reached",10,min=1),
      numericInput("Integration_ndowns","Minimum number of decreasing steps after the peak",10,min=1),
      numericInput("Integration_minpeakheight","The minimum (absolute) height a peak has to have to be recognized as such",0.01,min=0),
      numericInput("Integration_npeaks","The number of peaks to return",5,min=1),
      actionButton("Integration_action_auto","Perform automatic integration",icon=icon("flask")),
      actionButton("Integration_show", "Show peak list",icon = icon("edit")),hr(),
      bsModal("IntegrationModal", "Peak list", "Integration_show", size = "large",
              # p("peak list incoming")
              dataTableOutput("PeakList")
      ),
      numericInput("Integration_hrf_tol","hRF tolerance",5),
      checkboxInput("Integration_area_height","Use peak height",F)
      
    )
  })

  outputOptions(output, "Integration_ui_1", suspendWhenHidden = FALSE)
 
  
  observeEvent(input$Integration_action_auto,{
    if(is.null(reac$preprocessed)){
      shinyalert("Error!", "Preprocessed the chromatograms.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
    }else{
      reac$model=list()
      reac$Integration = list(
        Integration_nups = input$Integration_nups,
        Integration_ndowns = input$Integration_ndowns,
        Integration_minpeakheight = input$Integration_minpeakheight,
        Integration_npeaks = input$Integration_npeaks,
        PeakList= data.frame() ## colnames() = c("Track","Channel","Start","End","Max","hRF","Height","Area")
      )
      reac$batch = reac$batch[,1:3]
      width = reac$dimension["Plate width [mm]",];if(reac$double){width=0.5*width}
      Zf = reac$dimension["Migration front [mm]",]
      dist.bas = reac$dimension["Distance to lower edge [mm]",]
      
      for(channel in seq(4)){
        for(i in seq(nrow(reac$preprocessed))){
          m = findpeaks(reac$preprocessed[i,,channel],
                        nups = input$Integration_nups, ndowns = input$Integration_ndowns,
                        zero = "0", peakpat = NULL, minpeakheight = input$Integration_minpeakheight,
                        minpeakdistance = 1, threshold = 0, npeaks = input$Integration_npeaks, sortstr = FALSE)
          ## colnames(reac$Integration$PeakList) = c("Track","Channel","Start","End","Max","hRF","Height","Area")
          if(!is.null(m)){
            for(j in seq_len(nrow(m))){
              reac$Integration$PeakList = rbind(
                reac$Integration$PeakList,
                c(i,channel,
                  f.index_to_hrf(m[j,4],width,dist.bas,Zf,reac$preprocessed),
                  f.index_to_hrf(m[j,3],width,dist.bas,Zf,reac$preprocessed),
                  f.index_to_hrf(m[j,2],width,dist.bas,Zf,reac$preprocessed),
                  m[j,1],sum(reac$preprocessed[i,m[j,4]:m[j,3],channel]),
                  m[j,4],m[j,3],m[j,2])
              )
            }
          }
        }
      }
      
      colnames(reac$Integration$PeakList) = c("Track","Channel","Start hRf","End hRf","hRf","Height","Area","Start","End","Max")
      reac$Integration$PeakList = reac$Integration$PeakList[reac$Integration$PeakList$hRf > 0 & reac$Integration$PeakList$hRf < 100,]
    }
    
  })
  output$PeakList = renderDataTable({
    validate(need(nrow(reac$Integration$PeakList) > 0,"Perform the integration."))
    d = reac$Integration$PeakList[,1:7]
    d$Channel = c("red","green","blue","gray")[d$Channel]
    d
  })
  
  Map(function(channel) {
    output[[paste0("Integration_plot_",channel)]] = renderPlot({
      validate(need(!is.null(reac$preprocessed),"Preprocess the video densitograms."),
               need(input$Preprocess_plot_chrom_select > 0 && input$Preprocess_plot_chrom_select <= reac$nbr.band,"Wrong track selection."))
      width = reac$dimension["Plate width [mm]",];if(reac$double){width=0.5*width}
      Zf = reac$dimension["Migration front [mm]",]
      dist.bas = reac$dimension["Distance to lower edge [mm]",]
      par(mar=c(2.5,2.5,2,0),mgp=c(1.5,0.5,0))
      f.plot.array(reac$preprocessed,id = as.numeric(input$Integration_plot_chrom_select),
                   hauteur = width,Zf = Zf,dist.bas = dist.bas,reconstruct = F,channel = channel,main=c("Red channel","Green channel","Blue channel","Grayscale")[channel])
      for(i in seq_len(nrow(reac$Integration$PeakList))){
        if(reac$Integration$PeakList$Channel[i] == channel && reac$Integration$PeakList$Track[i] == input$Integration_plot_chrom_select){
          abline(v=reac$Integration$PeakList[i,8:10],col=c("green","red","black"))
        }
      }
      abline(h=0)
    })
  },
  seq(4))
  
  #### Stat ####
  observeEvent(input$click_Integration_plot_1,{
    reac$Integration_selection = list(channel = 1,x = round(input$click_Integration_plot_1$x))
  })
  observeEvent(input$click_Integration_plot_2,{
    reac$Integration_selection = list(channel = 2,x = round(input$click_Integration_plot_2$x))
  })
  observeEvent(input$click_Integration_plot_3,{
    reac$Integration_selection = list(channel = 3,x = round(input$click_Integration_plot_3$x))
  })
  observeEvent(input$click_Integration_plot_4,{
    reac$Integration_selection = list(channel = 4,x = round(input$click_Integration_plot_4$x))
  })
  observeEvent(reac$Integration_selection,{
    # validate(need(nrow(reac$Integration$PeakList)>0,"Perform the integration first"))
    if(nrow(reac$Integration$PeakList)==0){
      shinyalert("Error!", "Perform the integration first.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
    }else{
      width = reac$dimension["Plate width [mm]",];if(reac$double){width=0.5*width}
      Zf = reac$dimension["Migration front [mm]",]
      dist.bas = reac$dimension["Distance to lower edge [mm]",]
      truc = rep(0,nrow(reac$preprocessed))
      rows = c()
      for(i in seq(nrow(reac$preprocessed))){
        df = reac$Integration$PeakList[reac$Integration$PeakList$Channel == reac$Integration_selection$channel & reac$Integration$PeakList$Track == i,]
        row = which.min(abs(df$Max - reac$Integration_selection$x))
        if(length(row) > 0){
          rows = c(rows,df$hRf[row])
          if(abs(df$hRf[row] - f.index_to_hrf(reac$Integration_selection$x,width,dist.bas,Zf,reac$preprocessed)) <= input$Integration_hrf_tol){
            truc[i] = df[row,if(input$Integration_area_height){"Height"}else{"Area"}]
          }
        }
      }
      # c(red=1,green=2,blue=3,gray=4)
      compound = paste0(if(input$Integration_area_height){"Height "}else{"Area "},c("red","green","blue","gray")[reac$Integration_selection$channel] ," - hRF ",round(mean(rows))," [AU]")
      batch = hot_to_r(input$Stat_batch)
      if(compound %in% colnames(batch)){
        shinyalert("Error!", "Peak already selected.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
      }else if(0 %in% truc[batch$Standard]){
        shinyalert("Error!", "Peak not found in at least one standard.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
      }else{
        reac$batch = cbind(batch,truc)
        
        data = data.frame(x=reac$batch[reac$batch[,"Standard"],"Quantity [AU]"],y=truc[reac$batch[,"Standard"]])
        reac$model[[compound]] = calibrate(y~x, data, test.higher.orders = T, max.order = 2, p.crit = 0.05, 
                                           F.test = "partial", method = "qr", model = T)
        truc = inversePredictCalibrate(reac$model[[compound]],truc)[,2] %>% round(4)
        reac$batch = cbind(reac$batch,truc)
        colnames(reac$batch)[(ncol(reac$batch)-1):(ncol(reac$batch))] = c(compound,paste0("Prediction ",compound))
      }
    }
  })

  
  output$Stat_batch = renderRHandsontable({
    validate(need(!is.null(reac$extracted),"Extract the chromatograms."))
    reac$batch$`Quantity [AU]` = as.numeric(reac$batch$`Quantity [AU]`)
    reac$batch$Track = as.character(reac$batch$Track)
    truc = rhandsontable(reac$batch,rowHeaders = NULL) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    if(ncol(reac$batch) >3){
      truc %>% hot_col(colnames(reac$batch), readOnly = TRUE)
    }else{
      truc
    }
  })
  output$Stat_plot_select = renderUI({
    validate(need(!is.null(reac$batch),"Extract the Chromatograms."))
    validate(need(ncol(reac$batch) > 3,"Select at least one peak."))
    choices = colnames(reac$batch)[seq(from=4,by=2,length.out = (ncol(reac$batch)-3)/2)]
    selectizeInput("Stat_plot_select","Select peak",choices = choices,selected=choices[length(choices)])
  })
  output$Stat_plot = renderPlot({
    validate(need(length(reac$model) > 0,"Select at least one peak."))
    validate(need(!is.null(input$Stat_plot_select),"Not ready yet."))
    data = data.frame(x=reac$batch[,"Quantity [AU]"],y=reac$batch[,input$Stat_plot_select])
    data$x[!reac$batch$Standard] = reac$batch[,paste0("Prediction ",input$Stat_plot_select)][!reac$batch$Standard]
    plot(x = data$x,y=data$y,xlab = "Quantity [AU]",ylab = "Intensity [AU]",pch = 4,col=(!reac$batch$Standard)+1,main=input$Stat_plot_select)
    timevalues <- seq(min(data$y), max(data$y), by = abs(min(data$y) - max(data$y))/10)
    pred <- inversePredictCalibrate(reac$model[[input$Stat_plot_select]],timevalues)[,2]
    lines(pred,timevalues)
  })
  output$Stat_summary = renderPrint({
    validate(need(length(reac$model) > 0,"Select at least one peak."))
    validate(need(!is.null(input$Stat_plot_select),"Not ready yet."))
    model = reac$model[[input$Stat_plot_select]]
    print(summary(model))
    if(is.null(model$model$`I(x^2)`)){
      truc = coef(summary(model))
      cat(paste0("LOD: ",round(abs(3.3*truc[1,2]/truc[2,1]),4)," [AU]\n\n"))
      cat(paste0("LOQ: ",round(abs(10*truc[1,2]/truc[2,1]),4)," [AU]"))
    }else{
      cat("LOD and LOQ not available for quadratic models")
    }
  })
  
  observeEvent(input$Stat_remove_all,{
    if(is.null(reac$batch)){
      shinyalert("Error!", "Extract the chromatograms.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
    }else if(ncol(reac$batch) > 3){
      reac$model = list()
      reac$batch = reac$batch[,1:3]
    }else{
      shinyalert("Error!", "No peak to remove.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
    }
  })
  observeEvent(input$Stat_remove_last,{
    if(is.null(reac$batch)){
      shinyalert("Error!", "Extract the chromatograms.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
    }else if(ncol(reac$batch) > 3){
      reac$model[[length(reac$model)]] = NULL
      reac$batch = reac$batch[,1:(ncol(reac$batch)-2)]
    }else{
      shinyalert("Error!", "No peak to remove.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
    }
  })
  
  #### Report ####
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("quanTLC", sep = '.', switch(
        input$reportformat, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    content = function(file) {
      src <- normalizePath('report.Rmd')
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$reportformat,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  output$downloadChrom <- downloadHandler(
    filename = 'quanTLC.zip',
    content = function(file) {
      fs <- c()
      channel <- c(red=1,green=2,blue=3,gray=4)
      for(i in names(channel)){
        path <- paste0("before_preprocess_",i,'.csv')
        fs <- c(fs,path)
        truc = reac$extracted[,dim(reac$extracted)[2]:1,channel[i]]
        width = reac$dimension["Plate width [mm]",];if(reac$double){width=0.5*width}
        Zf = reac$dimension["Migration front [mm]",]
        dist.bas = reac$dimension["Distance to lower edge [mm]",]
        colnames(truc) = paste0(round(seq(-dist.bas/(Zf-dist.bas),(width-dist.bas)/(Zf-dist.bas),length.out=dim(truc)[2]),3))
        write.csv(truc,file=path,row.names = F,col.names = F,sep=';')
      }
      for(i in names(channel)){
        path <- paste0("after_preprocess_",i,'.csv')
        fs <- c(fs,path)
        truc = reac$preprocessed[,dim(reac$preprocessed)[2]:1,channel[i]]
        width = reac$dimension["Plate width [mm]",];if(reac$double){width=0.5*width}
        Zf = reac$dimension["Migration front [mm]",]
        dist.bas = reac$dimension["Distance to lower edge [mm]",]
        colnames(truc) = paste0(round(seq(-dist.bas/(Zf-dist.bas),(width-dist.bas)/(Zf-dist.bas),length.out=dim(truc)[2]),3))
        write.csv(truc,file=path,row.names = F,col.names = F,sep=';')
      }
      
      tempFile <- tempfile(fileext = ".zip")
      zip(zipfile=tempFile, files=fs)
      file.rename(tempFile, file)
    },
    contentType = "application/zip"
  )

})

