
data_extraction_ui = function(id){
  sidebarLayout(
    sidebarPanel(width=3,
                 # h3("Input"),
                 fileInput(NS(id,"Input_image"),"Select a chromatogram image or a Rdata file"),
                 selectInput(NS(id,"Demo_file"),"Select a demonstration file",choices = c("None",dir("www",pattern = ".Rdata")),selected="None"),
                 rHandsontableOutput(NS(id,"Input_dimension")),
                 checkboxInput(NS(id,"Input_convention"),"Change to distance calculated from the middle of the band",F),
                 checkboxInput(NS(id,"Input_double"),"Development from both sides",F),
                 bsTooltip(NS(id,"Input_double"),"By convention, the first application will be taken from the top right side to obtain the same number of bands on each side."),
                 actionButton(NS(id,"Input_action"),"Extract the video densitograms",icon=icon("flask"))
    ),
    mainPanel(width=9,
              plotOutput(NS(id,"Input_plot_raster"),click = NS(id,"Input_plot_raster_click"),dblclick = NS(id,"Input_plot_raster_dblclick")),
              bsTooltip(NS(id,"Input_plot_raster"),"Once the number of band, band length and edge cut is set, click in the middle of the first band and of the last band to calculate automatically the first application position and distance between tracks.")
    )
  )
}

data_extraction_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #### Input ####
    
    
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
        # if(grepl(".tif",input$Input_image$datapath,ignore.case = T)){
        #   shinyalert("Warning!", "Tiff files may cause problems, you may need to convert to jpeg, bmp or png.", type = "warning", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
        # }
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
      # if(nbr.band >= ceiling((largeur-dist.gauche)/(band+ecart))){
      if(largeur < dist.gauche+band*nbr.band+ecart*(nbr.band-1)){
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
        need(largeur > dist.gauche+band*nbr.band+ecart*(nbr.band-1),"Too much bands."),
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
    })
  })
}