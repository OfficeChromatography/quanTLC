
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
require(quanTLC,quietly=TRUE)
require(rhandsontable)

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
  updateTextInput(session, "Integration_compound", value = "Compound")

  reac = reactiveValues(
    image = f.read.image("www/plate_168_dev_RT_10ms.jpg"),
    image.name = NULL,
    batch = NULL,
    dimension = dimension,
    extracted = NULL,
    Preprocess.order = NULL,
    Preprocess.options = default_preprocess_options,
    preprocessed = NULL,
    Integration_start = c(),
    Integration_stop=c(),
    Integration_mode = NULL,
    model=NULL,
    Stat_quadratic = NULL,
    # Stat_origin = NULL,
    Stat_column = NULL
  )
  output$downloadCheckpoint <- downloadHandler(
    filename = "quanTLC.Rdata",
    content = function(con) {
      assign('data',list(image = reac$image,
                         image.name = reac$image.name,
                         batch = reac$batch,
                         dimension = reac$dimension,
                         extracted = reac$extracted,
                         Preprocess.order = reac$Preprocess.order,
                         Preprocess.options = reac$Preprocess.options,
                         preprocessed = reac$preprocessed,
                         Integration_start = reac$Integration_start,
                         Integration_stop=reac$Integration_stop,
                         Integration_mode = reac$Integration_mode,
                         model=reac$model,
                         Stat_quadratic = reac$Stat_quadratic,
                         # Stat_origin = reac$Stat_origin,
                         Stat_column = reac$Stat_column
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
      reac$extracted = data$extracted
      reac$Preprocess.order = data$Preprocess.order
      reac$Preprocess.options = data$Preprocess.options
      reac$preprocessed = data$preprocessed
      reac$Integration_start = data$Integration_start
      reac$Integration_stop=data$Integration_stop
      reac$Integration_mode = data$Integration_mode
      reac$model=data$model
      reac$Stat_quadratic = data$Stat_quadratic
      # reac$Stat_origin = data$Stat_origin
      reac$Stat_column = data$Stat_column
      
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
      # }
      # if("Warping" %in% reac$Preprocess.options){
        updateSelectizeInput(session,"warpmethod","Warping method",#choices=(c("ptw",'dtw')),
                             selected = reac$Preprocess.options$Warping$warpmethod)
        if(reac$Preprocess.options$Warping$warpmethod == "ptw"){
          updateNumericInput(session,"ptw.warp.ref",value = reac$Preprocess.options$Warping$ptw.warp.ref)
        }else{
          updateNumericInput(session,"dtw.warp.ref",value = reac$Preprocess.options$Warping$dtw.warp.ref)
          updateCheckboxInput(session,"dtw.split",value = reac$Preprocess.options$Warping$dtw.split)
        }
      # }
      
    }else{
      reac$image =f.read.image(input$Input_image$datapath)
      reac$image.name = input$Input_image$name
      reac$batch = NULL
      reac$extracted = NULL
      reac$Preprocess.order = NULL
      reac$Preprocess.options = NULL
      reac$preprocessed = NULL
      reac$Integration_start = c()
      reac$Integration_stop=c()
      reac$Integration_mode = NULL
      reac$model=NULL
      reac$Stat_quadratic = NULL
      # reac$Stat_origin = NULL
      reac$Stat_column = NULL
    }
    
  })
  observeEvent(input$Demo_file,{
      load("www/quanTLC.Rdata")
      reac$image = data$image
      reac$image.name = data$image.name
      reac$batch = data$batch
      reac$dimension = data$dimension
      reac$extracted = data$extracted
      reac$Preprocess.order = data$Preprocess.order
      reac$Preprocess.options = data$Preprocess.options
      reac$preprocessed = data$preprocessed
      reac$Integration_start = data$Integration_start
      reac$Integration_stop=data$Integration_stop
      reac$Integration_mode = data$Integration_mode
      reac$model=data$model
      reac$Stat_quadratic = data$Stat_quadratic
      # reac$Stat_origin = data$Stat_origin
      reac$Stat_column = data$Stat_column
      
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
    validate(
      need(nbr.band < ceiling((largeur-dist.gauche)/(band+ecart)),"Too much bands"),
      need(nbr.band >= 1,"Not enough bands")
    )
    
    reac$extracted = f.eat.image(reac$image,if(input$Input_convention){"ATS-4"}else{"linomat"},largeur=largeur,dist.gauche=dist.gauche,band = band,
                                 ecart = ecart,tolerance = tolerance,nbr.band = nbr.band,cropping = Cropping)
    reac$batch = data.frame(Track = paste0("Track ",seq(nbr.band)),Standard = rep(T,nbr.band),quantity = seq(nbr.band))
    colnames(reac$batch)[3] = "Quantity [AU]"
    reac$Preprocess.order = NULL
    reac$Preprocess.options = default_preprocess_options
    reac$preprocessed = NULL
    reac$Integration_start = c()
    reac$Integration_stop=c()
    reac$Integration_mode = NULL
    # reac$Integration_table=NULL
    reac$model=NULL
    reac$Stat_quadratic = NULL
    # reac$Stat_origin = NULL
    reac$Stat_column = NULL
  })
  output$Input_plot_raster = renderPlot({
    validate(need(!is.null(reac$image),"Upload the image chromatogram"))
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
      need(nbr.band < ceiling((largeur-dist.gauche)/(band+ecart)),"Too much bands"),
      need(nbr.band >= 1,"Not enough bands")
      )
    

    raster(reac$image)#,xlim= c(0,200/largeur*dim(reac$image)[2]))

    abline(h=dim(reac$image)[1]/width*dist.bas)
    abline(h=dim(reac$image)[1]/width*Zf)
    for(i in c(0:(nbr.band-1))){
      abline(v=(dim(reac$image)[2]/largeur*((dist.gauche+tolerance)+i*(band+ecart))),col="green")
      abline(v=(dim(reac$image)[2]/largeur*((dist.gauche+band-tolerance)+i*(band+ecart))),col="red")
    }
  })

  ### Preprocess ####
  output$Preprocess_plot_chrom_before = renderPlot({
    validate(need(!is.null(reac$extracted),"Extract the video densitograms"),
             need(input$Preprocess_plot_chrom_select > 0 && input$Preprocess_plot_chrom_select <= reac$dimension["Number of bands",],"Wrong track selection"))
    width = reac$dimension["Plate width [mm]",]
    Zf = reac$dimension["Migration front [mm]",]
    dist.bas = reac$dimension["Distance to lower edge [mm]",]
    par(mar=c(3,3,2,0),mgp=c(1.5,0.5,0))
    f.plot.array(reac$extracted,id = as.numeric(input$Preprocess_plot_chrom_select),
                 hauteur = width,Zf = Zf,dist.bas = dist.bas,reconstruct = T,main="Before preprocessing")
  })
  output$Preprocess_plot_chrom_after = renderPlot({
    validate(
      need(input$window.size %% 2 == 1, "The window size must be an odd value"),
      need(input$window.size > input$poly.order, "The window size must be greater than the polynomial order"),
      need(input$poly.order > input$diff.order, "The polynomial order must be greater than the differential order"),
      need(input$Preprocess_plot_chrom_select > 0 && input$Preprocess_plot_chrom_select <= reac$dimension["Number of bands",],"Wrong track selection")
    )
    validate(need(!is.null(reac$preprocessed),"Preprocess the video densitograms"))
    width = reac$dimension["Plate width [mm]",]
    Zf = reac$dimension["Migration front [mm]",]
    dist.bas = reac$dimension["Distance to lower edge [mm]",]
    par(mar=c(2.5,2.5,2,0),mgp=c(1.5,0.5,0))
    f.plot.array(reac$preprocessed,id = as.numeric(input$Preprocess_plot_chrom_select),
                 hauteur = width,Zf = Zf,dist.bas = dist.bas,reconstruct = F,main="After preprocessing")
    abline(h=0)
  })

  outputOptions(output, "Preprocess_ui_1", suspendWhenHidden = FALSE)

  observeEvent(input$Preprocess_action,{
    validate(
      need(input$window.size %% 2 == 1, "The window size must be an odd value"),
      need(input$window.size > input$poly.order, "The window size must be greater than the polynomial order"),
      need(input$poly.order > input$diff.order, "The polynomial order must be greater than the differential order"),
      need(!is.null(reac$extracted),"Video densitograms not extracted")
    )
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
    nbr.band=reac$dimension["Number of bands",]
    reac$batch = data.frame(Track = paste0("Track ",seq(nbr.band)),Standard = rep(T,nbr.band),quantity = seq(nbr.band))
    colnames(reac$batch)[3] = "Quantity [AU]"
    reac$Integration_start = c()
    reac$Integration_stop=c()
    reac$Integration_mode = NULL
    # reac$Integration_table=NULL
    reac$model=NULL
    reac$Stat_quadratic = NULL
    # reac$Stat_origin = NULL
    reac$Stat_column = NULL
  })


  ### Integration ####
  output$Integration_ui_1 = renderUI({
    tagList(
    # checkboxInput("Integration_auto","Automatic integration",F),
    # # conditionalPanel("!input.Integration_auto",
    # #                  p("The peak will be selected based on the manual integration")),
    # conditionalPanel("input.Integration_auto",
    #                  p("Not available yet"))
    # ,
    checkboxInput("Integration_area_height","Use peak height",F)
    # ,radioButtons("Integration_channel","Channel",choices = c("red"=1,"green"=2,"blue"=3,"gray"=4)),
    # checkboxInput("Integration_height","Use height instead of area",F)
    )
  })
  outputOptions(output, "Integration_ui_1", suspendWhenHidden = FALSE)
  
  observeEvent(input$Integration_action,{
    brush = input$brush.Integration_plot_chrom
    validate(need(!is.null(brush$xmin),"Brush the plot to select a peak"))
    reac$Integration_start = rep(round(brush$xmax),nrow(reac$batch))
    reac$Integration_stop = rep(round(brush$xmin),nrow(reac$batch))
    
    # width = reac$dimension["Plate width",]
    # Zf = reac$dimension["Migration front",]
    # dist.bas = reac$dimension["Distance to lower edge",]
    # comp_name = round(seq((width-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(reac$preprocessed)[2])[mean(c(brush$xmin,brush$xmax))],2)
    # comp_name = paste0("Intensity-","area-","Rf_",comp_name)
    comp_name = "Intensity"
    
    truc = apply(reac$preprocessed[,round(brush$xmin):round(brush$xmax),],c(1,3),if(input$Integration_area_height){max}else{sum})
    colnames(truc) = paste0(comp_name,c(" red [AU]"," green [AU]"," blue [AU]"," gray [AU]"))
    updateTextInput(session, "Integration_compound", value = paste0("Compound ",length(reac$Integration_start)+1))
    reac$batch = cbind(reac$batch[,1:3],truc)
    reac$Integration_mode = if(input$Integration_area_height){"height"}else{"area"}
    reac$model = NULL
    reac$Stat_quadratic = NULL
    # reac$Stat_origin = NULL
    reac$Stat_column = NULL
  })
  output$Integration_plot_chrom = renderPlot({
    validate(need(!is.null(reac$preprocessed),"Preprocess the video densitograms"),
             need(input$Preprocess_plot_chrom_select > 0 && input$Preprocess_plot_chrom_select <= reac$dimension["Number of bands",],"Wrong track selection"))
    width = reac$dimension["Plate width [mm]",]
    Zf = reac$dimension["Migration front [mm]",]
    dist.bas = reac$dimension["Distance to lower edge [mm]",]
    par(mar=c(2.5,2.5,2,0),mgp=c(1.5,0.5,0))
    f.plot.array(reac$preprocessed,id = as.numeric(input$Integration_plot_chrom_select),
                 hauteur = width,Zf = Zf,dist.bas = dist.bas,reconstruct = F)
    abline(v=reac$Integration_start[as.numeric(input$Integration_plot_chrom_select)],col="green")
    abline(v=reac$Integration_stop[as.numeric(input$Integration_plot_chrom_select)],col="red")
    abline(h=0)
  })
  observeEvent(input$brush.Integration_one_by_one_chrom,{
    brush = input$brush.Integration_one_by_one_chrom
    i = as.numeric(input$Integration_one_by_one_select)
    reac$Integration_start[i] = round(brush$xmax)
    reac$Integration_stop[i]= round(brush$xmin)
    truc = apply(reac$preprocessed[,round(brush$xmin):round(brush$xmax),],c(1,3),if(reac$Integration_mode == "height"){max}else{sum})
    reac$batch = reac$batch[,1:7]
    reac$batch[i,4:7] = truc[i,]
    # print(truc)
  })
  output$Integration_one_by_one_chrom = renderPlot({
    validate(need(!is.null(reac$preprocessed),"Preprocess the video densitograms"),
             need(input$Preprocess_plot_chrom_select > 0 && input$Preprocess_plot_chrom_select <= reac$dimension["Number of bands",],"Wrong track selection"))
    i = as.numeric(input$Integration_one_by_one_select)
    width = reac$dimension["Plate width [mm]",]
    Zf = reac$dimension["Migration front [mm]",]
    dist.bas = reac$dimension["Distance to lower edge [mm]",]
    par(mar=c(2.5,2.5,2,0),mgp=c(1.5,0.5,0))
    f.plot.array(reac$preprocessed,id = i,
                 hauteur = width,Zf = Zf,dist.bas = dist.bas,reconstruct = F)
    abline(v=reac$Integration_start[i],col="green")
    abline(v=reac$Integration_stop[i],col="red")
    abline(h=0)
  })
  observeEvent(input$Integration_one_by_one_previous,{
    if(input$Integration_one_by_one_select > 1){
      updateNumericInput(session,"Integration_one_by_one_select",value = input$Integration_one_by_one_select-1)
    }
  })
  observeEvent(input$Integration_one_by_one_next,{
    if(input$Integration_one_by_one_select < nrow(reac$batch)){
      updateNumericInput(session,"Integration_one_by_one_select",value = input$Integration_one_by_one_select+1)
    }
  })

  #### Stat ####
  output$Stat_batch = renderRHandsontable({
    validate(need(ncol(reac$batch) >3,"Do the integration"))
    reac$batch$`Quantity [AU]` = as.numeric(reac$batch$`Quantity [AU]`)
    rhandsontable(reac$batch,rowHeaders = NULL) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_col(colnames(reac$batch)[c(1,4:ncol(reac$batch))], readOnly = TRUE)
  })
  output$Stat_column = renderUI({
    validate(need(ncol(reac$batch) >3,"Do the integration"))
    selectizeInput("Stat_column","Select channel",choice=colnames(reac$batch)[4:7],selected = if(!is.null(reac$Stat_column)){reac$Stat_column}else{colnames(reac$batch)[4]})
  })
  observeEvent(input$Stat_action,{
    validate(need(ncol(reac$batch) >3,"Do the integration"))
    reac$batch = hot_to_r(input$Stat_batch)[,1:7]
    data = data.frame(x=reac$batch[,"Quantity [AU]"],x2=(reac$batch[,"Quantity [AU]"])^2,y=reac$batch[,input$Stat_column],y2=(reac$batch[,input$Stat_column])^2)
    
    # if(!input$Stat_quadratic && input$Stat_origin){
    #   form = formula(x ~ y +1)
    # }else if(!input$Stat_quadratic && !input$Stat_origin){
    #   form = formula(x ~ y)
    # }else if(input$Stat_quadratic && !input$Stat_origin){
    #   form = formula(x ~ y +y2+1)
    # }else if(input$Stat_quadratic && input$Stat_origin){
    #   form = formula(x ~ y +y2)
    # }
    if(!input$Stat_quadratic){
      form = formula(x ~ y)
    }else if(input$Stat_quadratic){
      form = formula(x ~ y +y2)
    }
    reac$model = lm(form,data = data,subset = reac$batch$Standard)
    truc = predict(reac$model,data)
    
    reac$batch[,input$Stat_column] = reac$batch[,input$Stat_column]
    reac$batch[["Prediction [AU]"]] = truc
    reac$Stat_quadratic = if(input$Stat_quadratic){"quadratic"}else{"linear"}
    # reac$Stat_origin = input$Stat_origin
    reac$Stat_column = input$Stat_column
  })
  output$Stat_plot = renderPlot({
    validate(need(!is.null(reac$model),"Apply the batch"))
    
    data = data.frame(x=reac$batch[,"Quantity [AU]"],y=reac$batch[,reac$Stat_column])
    data$x[!reac$batch$Standard] = reac$batch[,"Prediction [AU]"][!reac$batch$Standard]
    data$x2=data$x^2
    data$y2 = data$y^2
    plot(x = data$x,y=data$y,xlab = "Quantity [AU]",ylab = reac$Stat_column,pch = 4,col=(!reac$batch$Standard)+1)
    # abline(reac$model)
    timevalues <- seq(min(data$y), max(data$y), by = abs(min(data$y) - max(data$y))/10)
    pred <- predict(reac$model,data.frame(y=timevalues, y2=timevalues^2))
    lines(pred,timevalues)
  })
  output$Stat_summary = renderPrint({
    validate(need(!is.null(reac$model),"Apply the batch"))
    print(summary(reac$model))
    if(reac$Stat_quadratic == "linear"){
      truc = coef(summary(reac$model))
      cat(paste0("LOD: ",round(abs(3.3*truc[1,2]/truc[2,1]),4)," [AU]\n\n"))
      cat(paste0("LOQ: ",round(abs(10*truc[1,2]/truc[2,1]),4)," [AU]"))
    }else{
      cat("LOD and LOQ not available for quadratic models")
    }
  })
  
  #### Report ####
  
  output$Report_reac = renderPrint({
    print(reac$Stat_quadratic)
    print(reac$Stat_column)
    print(reac$Integration_mode)
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("quanTLC", sep = '.', switch(
        input$reportformat, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      # owd <- setwd(tempdir())
      # on.exit(setwd(owd))
      # file.copy(src, 'report.Rmd')
      
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
        width = reac$dimension["Plate width [mm]",]
        Zf = reac$dimension["Migration front [mm]",]
        dist.bas = reac$dimension["Distance to lower edge [mm]",]
        colnames(truc) = paste0(round(seq(-dist.bas/(Zf-dist.bas),(width-dist.bas)/(Zf-dist.bas),length.out=dim(truc)[2]),3))
        write.csv(truc,file=path,row.names = F,col.names = F,sep=';')
      }
      for(i in names(channel)){
        path <- paste0("after_preprocess_",i,'.csv')
        fs <- c(fs,path)
        truc = reac$preprocessed[,dim(reac$preprocessed)[2]:1,channel[i]]
        width = reac$dimension["Plate width [mm]",]
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
