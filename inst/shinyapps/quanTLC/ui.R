require(quanTLC,quietly=TRUE)
require(rhandsontable)

fluidPage(
  titlePanel("",windowTitle = "quanTLC"),
  tabsetPanel(type="pills",
              tabPanel("Input and preprocessing",
                       tags$head(tags$style(HTML(".shiny-output-error-validation {color: red;font-size: 24px}"))),
                       # useShinyalert(),
                       sidebarLayout(
                         sidebarPanel(width=3,
                                      # h3("Input"),
                                      fileInput("Input_image","Select a chromatogram image or a Rdata file"),
                                      selectInput("Demo_file","Select a demonstration file",choices = c("None",dir("www",pattern = ".Rdata")),selected="None"),
                                      # fileInput("Input_method","Rdata method file save in a previous session"),
                                      rHandsontableOutput("Input_dimension"),
                                      checkboxInput("Input_convention","Change to distance calculated from the middle of the band",F),
                                      checkboxInput("Input_double","Development from both sides",F),
                                      bsTooltip("Input_double","By convention, the first application will be taken from the top right side to obtain the same number of bands on each side."),
                                      actionButton("Input_action","Extract the video densitograms",icon=icon("flask"))

                         ),
                         mainPanel(width=9,
                                   plotOutput("Input_plot_raster",click = "Input_plot_raster_click",dblclick = "Input_plot_raster_dblclick"),
                                   bsTooltip("Input_plot_raster","Once the number of band, band length and edge cut is set, click in the middle of the first band and of the last band to calculate automatically the first application position and distance between tracks.")
                         )
                       ),
                       sidebarLayout(
                         sidebarPanel(width=3,
                                      actionButton("Preprocess_show", "Preprocessing options",icon = icon("edit")),
                                      selectizeInput('Preprocess.order','Select the preprocessing algorithms (order is important)',
                                                     choices=c("Negative peak inversion" = "Negatif",#"Gamma correction" = 'gammaCorrection',
                                                               'Smoothing' = 'Smoothing',
                                                               'Baseline correction' = 'Baseline.correction','Warping' = 'Warping'),
                                                     selected='',multiple=T),
                                      actionButton("Preprocess_action","Apply the preprocesses",icon=icon("flask")),
                                      bsModal("preprocessModal", "Preprocessing options", "Preprocess_show", size = "large",
                                              uiOutput("Preprocess_ui_1")
                                      )

                         ),
                         mainPanel(width=9,
                                   numericInput("Preprocess_plot_chrom_select","Selection of the track",1),
                                   column(6,
                                          plotOutput("Preprocess_plot_chrom_before")
                                   ),
                                   column(6,
                                          plotOutput("Preprocess_plot_chrom_after")
                                   )
                         )
                       )
              ),
              tabPanel("Integration and statistics",
                       sidebarLayout(
                         sidebarPanel(width = 3,
                           uiOutput("Integration_ui_1")
                           
                         ),
                         mainPanel(width=9,
                           numericInput("Integration_plot_chrom_select","Selection of the track",1),
                           column(6,
                                  plotOutput("Integration_plot_1",click = "click_Integration_plot_1",height = "200px"),
                                  plotOutput("Integration_plot_2",click = "click_Integration_plot_2",height = "200px")
                                  ),
                           column(6,
                                  plotOutput("Integration_plot_3",click = "click_Integration_plot_3",height = "200px"),
                                  plotOutput("Integration_plot_4",click = "click_Integration_plot_4",height = "200px")
                                  )
                           
                         )
                       ),
                       sidebarLayout(
                         sidebarPanel(width=6,
                                      rHandsontableOutput("Stat_batch"),hr(),
                                      uiOutput("Stat_column"),
                                      # checkboxInput("Stat_quadratic","Use quadratic regression",F),
                                      # checkboxInput("Stat_origin","Pass by the origin)",F),
                                      # actionButton("Stat_action","Apply the batch",icon=icon("flask")),
                                      actionButton("Stat_remove_all","Remove all selected peaks",icon=icon("remove")),
                                      actionButton("Stat_remove_last","Remove last selected peak",icon=icon("remove")),
                                      verbatimTextOutput("Stat_summary")
                         ),
                         mainPanel(width=6,
                                   uiOutput("Stat_plot_select"),
                                   plotOutput("Stat_plot")
                         )
                       )
              ),
              tabPanel("Report",
                       sidebarLayout(
                         sidebarPanel(width=6,
                                      checkboxGroupInput("Report_options","Include in report",
                                                         choices = c(
                                                           "Chromatogram"
                                                           ,"Dimension table"
                                                           ,"Preprocessing options"
                                                           ,"Integration options"
                                                           ,"Video-densitograms"
                                                           ,"Peak list"
                                                           ,"Peak list for each sample"
                                                           ,"Regression results"
                                                         ),
                                                         selected = c(
                                                           "Chromatogram"
                                                           ,"Dimension table"
                                                           ,"Preprocessing options"
                                                           ,"Integration options"
                                                           ,"Video-densitograms"
                                                           ,"Peak list"
                                                           ,"Peak list for each sample"
                                                           ,"Regression results"
                                                           )),
                                      radioButtons('reportformat', 'Document format', c('PDF', 'HTML', "MS Word"='Word'),
                                                   inline = TRUE),
                                      downloadButton('downloadReport',label = "Report"),
                                      hr(),
                                      downloadButton("downloadCheckpoint",label = "Save data"),
                                      p("Upload this Rdata file instead of the chromatogram"),
                                      hr(),
                                      downloadButton("downloadChrom",label = "Export as CSV format")
                         ),
                         mainPanel()
                       )
              ),
              tabPanel("Help",
                       helpText(   a("Instruction for local installation",target="_blank",
                                     href="https://github.com/DimitriF/quantlc")
                       ),
                       h4("PDF manual"),
                       downloadButton("pdf_manual",label = "PDF"),
                       h4("Video manual"),
                       HTML('<video src="out.mp4" controls/>')
              )
  )

)
