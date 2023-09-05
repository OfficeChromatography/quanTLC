library(shinyBS)


data_preprocessing_ui = function(id){
  sidebarLayout(
    sidebarPanel(width=3,
                 actionButton(NS(id,"Preprocess_show"), "Preprocessing options",icon = icon("edit")),
                 selectizeInput(NS(id,'Preprocess.order'),'Select the preprocessing algorithms (order is important)',
                                choices=c("Negative peak inversion" = "Negatif",
                                          'Smoothing' = 'Smoothing',
                                          'Baseline correction' = 'Baseline.correction','Warping' = 'Warping'),
                                selected='',multiple=T),
                 actionButton(NS(id,"Preprocess_action"),"Apply the preprocesses",icon=icon("flask")),
                 bsModal("preprocessModal", "Preprocessing options", NS(id,"Preprocess_show"), size = "large",
                         uiOutput(NS(id,"Preprocess_ui_1"))
                 )
                 
    ),
    mainPanel(width=9,
              numericInput(NS(id,"Preprocess_plot_chrom_select"),"Selection of the track",1),
              column(6,
                     plotOutput(NS(id,"Preprocess_plot_chrom_before"))
              ),
              column(6,
                     plotOutput(NS(id,"Preprocess_plot_chrom_after"))
              )
    )
  )
}

data_preprocessing_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    
    output$Preprocess_ui_1 = renderUI({
      column(12,
             column(4,
                    # h4("Median filter"),
                    # numericInput('preprocess.medianfilter','Half-size of the filtering window',3),
                    # h4('Gamma correction'),
                    # numericInput('preprocess.gammacorrection','Value',2),
                    h4("Smoothing"),
                    helpText(   a("Help for this feature",target="_blank",
                                  href="https://www.rdocumentation.org/packages/prospectr/versions/0.1.3/topics/savitzkyGolay?")
                    ),
                    helpText(   a("Wikipedia link",target="_blank",
                                  href="https://en.wikipedia.org/wiki/Savitzky%E2%80%93Golay_filter")
                    ),
                    numericInput(ns("window.size"),"Size of the window",15,min=3,max=NA,step=2),
                    numericInput(ns("poly.order"),"Polynomial order",2),
                    numericInput(ns("diff.order"),"Differentiation order",0)
             ),
             column(4,
                    h4("Baseline"),
                    helpText(   a("Help for this feature",target="_blank",
                                  href="http://cran.r-project.org/web/packages/baseline/baseline.pdf")
                    ),
                    selectizeInput(ns("baseline"), "Type of baseline", choices=c("als","fillPeaks","irls","lowpass","medianWindow","modpolyfit","peakDetection","rollingBall"),select=NULL),
                    conditionalPanel(condition="input.baseline=='als'",ns = ns,
                                     numericInput(ns("lambda.1"),"lambda: 2nd derivative constraint",5),
                                     numericInput(ns("p"),"p: weighting of positive residuals",0.05),
                                     numericInput(ns("maxit.1"),"maxit: maximum number of iterations",20)
                    ),
                    conditionalPanel(condition="input.baseline=='fillPeaks'",ns = ns,
                                     numericInput(ns("lambda.2"),"lambda: 2nd derivative constraint for primary smoothing",6),
                                     numericInput(ns("hwi"),"hwi: half width of local windows",100),
                                     numericInput(ns("it"),"it: number of iterations in suppression loop",10),
                                     numericInput(ns("int"),"int: number of buckets to divide spectra into",200)
                    ),
                    conditionalPanel(condition="input.baseline=='irls'",ns = ns,
                                     numericInput(ns("lambda1"),"lambda1: 2nd derivative constraint for primary smoothing",5),
                                     numericInput(ns("lambda2"),"lambda2: 2nd derivative constraint for secondary smoothing",9),
                                     numericInput(ns("maxit.2"),"maxit: maximum number of iterations",200),
                                     numericInput(ns("wi"),"wi: weighting of positive residuals",0.05)
                    ),
                    conditionalPanel(condition="input.baseline=='lowpass'",ns = ns,
                                     numericInput(ns("steep"),"steep: steepness of filter curve",2),
                                     numericInput(ns("half"),"half: half way point of filter curve",5)
                    ),
                    conditionalPanel(condition="input.baseline=='medianWindow'",ns = ns,
                                     numericInput(ns("hwm"),"hwm: window half width for local medians",300),
                                     numericInput(ns("hws"),"hws: window half width for local smoothing",5),
                                     checkboxInput(ns("end"),"end: original endpoint handling",F)
                    ),
                    conditionalPanel(condition="input.baseline=='modpolyfit'",ns = ns,
                                     numericInput(ns("degree"),"degree: degree of polynomial",4),
                                     numericInput(ns("tol"),"tol: tolerance of difference between iterations",0.001),
                                     numericInput(ns("rep"),"rep: maximum number of iterations",100)
                    ),
                    conditionalPanel(condition="input.baseline=='peakDetection'",ns = ns,
                                     numericInput(ns("left"),"left: smallest window size for peak widths",5),
                                     numericInput(ns("right"),"right: largest window size for peak widths",100),
                                     numericInput(ns("lwin"),"lwin: Smallest window size for minimums and medians in peak removed spectra",5),
                                     numericInput(ns("rwin"),"rwin: Largest window size for minimums and medians in peak removed spectra",10),
                                     numericInput(ns("snminimum"),"snminimum: Minimum signal to noise ratio for accepting peaks",10)
                    ),
                    conditionalPanel(condition="input.baseline=='rollingBall'",ns = ns,
                                     numericInput(ns("wm"),"wm: Width of local window for minimization/maximization",200),
                                     numericInput(ns("ws"),"ws: Width of local window for smoothing",200)
                    )
             ),
             column(4,
                    h4("Warping"),
                    helpText(   a("Wikipedia link",target="_blank",
                                  href="https://en.wikipedia.org/wiki/Dynamic_time_warping")
                    ),
                    selectizeInput(ns("warpmethod"),"Warping method",choices=(c("ptw",'dtw')),selected="ptw"),
                    conditionalPanel(condition="input.warpmethod=='ptw'",ns = ns,
                                     helpText(   a("Help for this feature",target="_blank",
                                                   href="https://www.rdocumentation.org/packages/ptw/versions/1.9-11/topics/ptw")
                                     ),
                                     numericInput(ns('ptw.warp.ref'),"Track of reference",1)
                    ),
                    conditionalPanel(condition="input.warpmethod=='dtw'",ns = ns,
                                     helpText(   a("Help for this feature",target="_blank",
                                                   href="https://www.rdocumentation.org/packages/dtw/versions/1.18-1/topics/dtw?")
                                     ),
                                     numericInput(ns('dtw.warp.ref'),"Track of reference",1),
                                     checkboxInput(ns('dtw.split'),'Do the alignment on the 4 channels separately.',F)
                    )
             )
      )
    })
    
    output$Preprocess_plot_chrom_before = renderPlot({
      validate(need(!is.null(reac$extracted),"Extract the video densitograms."))
      validate(need(input$Preprocess_plot_chrom_select > 0 && input$Preprocess_plot_chrom_select <= reac$nbr.band,"Wrong track selection."))
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
        need(input$poly.order > input$diff.order, "The polynomial order must be greater than the differential order.")
      )
      validate(need(!is.null(reac$preprocessed),"Preprocess the video densitograms."))
      validate(need(input$Preprocess_plot_chrom_select > 0 && input$Preprocess_plot_chrom_select <= reac$nbr.band,"Wrong track selection."))
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
  })
}
