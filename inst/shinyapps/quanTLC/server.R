
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
require(quanTLC,quietly=TRUE)


dimension = data.frame(row.names = c("Plate width",
                                     "Migration front",
                                     "Distance to lower edge",
                                     "Plate length",
                                     "First application position",
                                     "Band length",
                                     "Distance between track",
                                     "Edge cut",
                                     "Number of band",
                                     "Cropping"),
                       Value = as.numeric(c(100,70,8,100,27,6,2,2,5,0)))

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
    Integration_table=NULL,
    model=NULL
  )
  observeEvent(input$Input_image,{
    reac$image =f.read.image(input$Input_image$datapath)
    reac$image.name = input$Input_image$name
    reac$batch = NULL
    reac$extracted = NULL
    reac$Preprocess.order = NULL
    reac$Preprocess.options = NULL
    reac$preprocessed = NULL
    reac$Integration_start = c()
    reac$Integration_stop=c()
    reac$Integration_table=NULL
    reac$model=NULL
    file.rename(input$Input_image$datapath,paste0("/srv/shiny-server/rtlc/data/image/",Sys.time(),"_quanTLC_",seq(length(input$Input_image$datapath)),".jpg"))
  })

  #### Input ####
  output$Input_dimension = renderRHandsontable({
    rhandsontable(reac$dimension, rowHeaderWidth = 200)
  })
  observeEvent(input$Input_action,{
    reac$dimension$Value = as.numeric(hot_to_r(input$Input_dimension)$Value)
    nbr.band=reac$dimension["Number of band",]
    largeur = reac$dimension["Plate length",]
    dist.gauche=reac$dimension["First application position",]
    tolerance=reac$dimension["Edge cut",]
    band=reac$dimension["Band length",]
    ecart=reac$dimension["Distance between track",]
    Cropping=reac$dimension["Cropping",]
    reac$extracted = f.eat.image(reac$image,if(input$Input_convention){"ATS-4"}else{"linomat"},largeur=largeur,dist.gauche=dist.gauche,band = band,
                                 ecart = ecart,tolerance = tolerance,nbr.band = nbr.band,cropping = Cropping)
    reac$batch = data.frame(Track = paste0("Track ",seq(nbr.band)),Standard = rep(T,nbr.band),quantity = seq(nbr.band))
    reac$Preprocess.order = NULL
    reac$Preprocess.options = default_preprocess_options
    reac$preprocessed = NULL
    reac$Integration_start = c()
    reac$Integration_stop=c()
    reac$Integration_table=NULL
    reac$model=NULL
  })
  output$Input_plot_raster = renderPlot({
    validate(need(!is.null(reac$image),"upload the image chromatogram"))
    par(mar=c(0,0,0,0),xaxs="i",yaxs="i")

    dimension = dimension
    # dimension$Value = as.numeric(hot_to_r(input$Input_dimension)$Value)

    Cropping = dimension["Cropping",]
    nbr.band=dimension["Number of band",]
    largeur = dimension["Plate length",]
    dist.gauche=dimension["First application position",]
    tolerance=dimension["Edge cut",]
    band=dimension["Band length",]
    ecart=dimension["Distance between track",]
    width = dimension["Plate width",]
    Zf = dimension["Migration front",]
    dist.bas = dimension["Distance to lower edge",]
    # cropping correction
    largeur = largeur - 2 * Cropping
    dist.gauche = dist.gauche - Cropping
    if(input$Input_convention){ # this put everybody back to linomat convention
      dist.gauche<-dist.gauche-band/2
      ecart<-ecart-band
    }

    raster(reac$image)#,xlim= c(0,200/largeur*dim(reac$image)[2]))

    abline(h=dim(reac$image)[1]/width*dist.bas)
    abline(h=dim(reac$image)[1]/width*Zf)
    for(i in c(0:(nbr.band-1))){
      abline(v=(dim(reac$image)[2]/largeur*((dist.gauche+tolerance)+i*(band+ecart))),col="green")
      abline(v=(dim(reac$image)[2]/largeur*((dist.gauche+band-tolerance)+i*(band+ecart))),col="red")
    }
  })
  # output$Input_plot_chrom = renderPlot({
  #   validate(need(!is.null(reac$extracted),"extract the video densitograms"))
  #   width = reac$dimension["Plate width",]
  #   Zf = reac$dimension["Migration front",]
  #   dist.bas = reac$dimension["Distance to lower edge",]
  #
  #   f.plot.array(reac$extracted,id = as.numeric(input$Input_plot_chrom_select),
  #                hauteur = width,Zf = Zf,dist.bas = dist.bas,reconstruct = T)
  # })

  ### Preprocess ####
  output$Preprocess_plot_chrom_before = renderPlot({
    validate(need(!is.null(reac$extracted),"extract the video densitograms"))
    width = reac$dimension["Plate width",]
    Zf = reac$dimension["Migration front",]
    dist.bas = reac$dimension["Distance to lower edge",]
    par(mar=c(2.5,2.5,2,0),mgp=c(1.5,0.5,0))
    f.plot.array(reac$extracted,id = as.numeric(input$Preprocess_plot_chrom_select),
                 hauteur = width,Zf = Zf,dist.bas = dist.bas,reconstruct = T,main="Before preprocessing")
  })
  output$Preprocess_plot_chrom_after = renderPlot({
    validate(need(!is.null(reac$preprocessed),"preprocess the video densitograms"))
    width = reac$dimension["Plate width",]
    Zf = reac$dimension["Migration front",]
    dist.bas = reac$dimension["Distance to lower edge",]
    par(mar=c(2.5,2.5,2,0),mgp=c(1.5,0.5,0))
    f.plot.array(reac$preprocessed,id = as.numeric(input$Preprocess_plot_chrom_select),
                 hauteur = width,Zf = Zf,dist.bas = dist.bas,reconstruct = F,main="After preprocessing")
  })

  #   preprocessModal <- function(failed = FALSE) {
  #     modalDialog(
  #       uiOutput("Preprocess_ui_1")
  #       ,footer = tagList(
  #         modalButton("Cancel"),
  #         actionButton("preprocess_ok", "OK")
  #       )
  #       , easyClose = T
  #     )
  #   }
  #   observeEvent(input$Preprocess_show, {
  #     showModal(preprocessModal())
  #   })

  #   observeEvent(input$preprocess_ok, {
  #     Smoothing <- list(window.size = input$window.size,poly.order=input$poly.order,diff.order=input$diff.order)
  #     if(input$warpmethod == 'ptw'){
  #       Warping <- list(warpmethod = input$warpmethod,
  #                       ptw.warp.ref = as.numeric(input$ptw.warp.ref)
  #       )
  #     }
  #     if(input$warpmethod == 'dtw'){
  #       Warping <- list(warpmethod = input$warpmethod,
  #                       dtw.warp.ref = as.numeric(input$dtw.warp.ref),
  #                       dtw.split = input$dtw.split
  #       )
  #     }
  #     if(input$baseline == "als"){Baseline <- list(method=input$baseline,lambda.1=input$lambda.1,p=input$p,maxit.1=input$maxit.1)}
  #     if(input$baseline == "fillPeaks"){Baseline <- list(method=input$baseline,lambda.2=input$lambda.2,hwi=input$hwi,it=input$it,int=input$int)}
  #     if(input$baseline == "irls"){Baseline <- list(method=input$baseline,lambda1=input$lambda1,lambda2=input$lambda2,maxit.2=input$maxit.2,wi=input$wi)}
  #     if(input$baseline == "lowpass"){Baseline <- list(method=input$baseline,steep=input$steep,half=input$half)}
  #     if(input$baseline == "medianWindow"){Baseline <- list(method=input$baseline,hwm=input$hwm,hws=input$hws,end=input$end)}
  #     if(input$baseline == "modpolyfit"){Baseline <- list(method=input$baseline,degree=input$degree,tol=input$tol,rep=input$rep)}
  #     if(input$baseline == "peakDetection"){Baseline <- list(method=input$baseline,left=input$left,right=input$right,lwin=input$lwin,rwin=input$rwin)}
  #     if(input$baseline == "rfBaseline"){Baseline <- list(method=input$baseline)}
  #     if(input$baseline == "rollingBall"){Baseline <- list(method=input$baseline,wm=input$wm,ws=input$ws)}
  #     reac$Preprocess.options = list(Smoothing=Smoothing,Warping=Warping,Baseline.correction=Baseline,
  #                                    medianFilter=input$preprocess.medianfilter,gammaCorrection=input$preprocess.gammacorrection)
  #     toggleModal(session, "preprocessModal", toggle = "close")
  #   })
  #
  #   observeEvent(input$preprocess_cancel, {
  #     toggleModal(session, "preprocessModal", toggle = "close")
  #   })

  outputOptions(output, "Preprocess_ui_1", suspendWhenHidden = FALSE)

  observeEvent(input$Preprocess_action,{
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
    reac$Integration_start = c()
    reac$Integration_stop=c()
    reac$Integration_table=NULL
    reac$model=NULL
  })


  ### Integration ####
  output$Integration_ui_1 = renderUI({
    tagList(
    checkboxInput("Integration_auto","Automatic integration",F),
    # conditionalPanel("!input.Integration_auto",
    #                  p("The peak will be selected based on the manual integration")),
    conditionalPanel("input.Integration_auto",
                     p("The peaks will be selected if")),
    radioButtons("Integration_channel","Channel",choices = c("red"=1,"green"=2,"blue"=3,"grey"=4)),
    checkboxInput("Integration_height","Use height instead of area",F)
    )
  })
  observeEvent(input$Integration_action,{
    brush = input$brush.Integration_plot_chrom
    reac$Integration_start = c(reac$Integration_start,round(brush$xmin))
    reac$Integration_stop = c(reac$Integration_stop,round(brush$xmax))
    truc = apply(reac$preprocessed[,round(brush$xmin):round(brush$xmax),],c(1,3),if(input$Integration_area_height){max}else{sum})
    colnames(truc) = paste0(input$Integration_compound,c(" red"," green"," blue"," grey"))
    updateTextInput(session, "Integration_compound", value = paste0("Compound ",length(reac$Integration_start)+1))
    reac$Integration_table = cbind(reac$Integration_table,truc)
  })
  output$Integration_plot_chrom = renderPlot({
    validate(need(!is.null(reac$preprocessed),"preprocess the video densitograms"))
    width = reac$dimension["Plate width",]
    Zf = reac$dimension["Migration front",]
    dist.bas = reac$dimension["Distance to lower edge",]
    par(mar=c(2.5,2.5,2,0),mgp=c(1.5,0.5,0))
    f.plot.array(reac$preprocessed,id = as.numeric(input$Integration_plot_chrom_select),
                 hauteur = width,Zf = Zf,dist.bas = dist.bas,reconstruct = F)
    abline(v=reac$Integration_start,col="green")
    abline(v=reac$Integration_stop,col="red")
  })
  output$Integration_table = renderTable({
    reac$Integration_table
  })

  #### Stat ####
  output$Stat_batch = renderRHandsontable({
    rhandsontable(reac$batch,rowHeaders = NULL)
  })
  output$Stat_column = renderUI({
    validate(need(ncol(reac$Integration_table)!=0,"Do the integration"))
    selectizeInput("Stat_column","Compound and channel to use",choice=colnames(reac$Integration_table))
  })
  observeEvent(input$Stat_action,{
    reac$batch = hot_to_r(input$Stat_batch)
    data = data.frame(x=reac$batch$quantity,y=reac$Integration_table[,input$Stat_column])
    reac$model = lm(y ~ x,data = data,subset = reac$batch$Standard)
    truc = c()
    for(i in reac$Integration_table[,input$Stat_column]){
      truc = c(truc,inverse.predict(reac$model,i)$Prediction)
    }
    reac$batch[,input$Stat_column] = reac$Integration_table[,input$Stat_column]
    reac$batch$predict = truc
  })
  output$Stat_plot = renderPlot({
    validate(need(!is.null(reac$model),"Do the integration"))
    chemCal::calplot(reac$model)
  })
  output$Stat_summary = renderPrint({
    print(summary(reac$model))
    print(paste0("LOD: ",lod(reac$model)[1]))
    print(paste0("LOQ: ",loq(reac$model)[1]))
  })

  #### end ####


})
