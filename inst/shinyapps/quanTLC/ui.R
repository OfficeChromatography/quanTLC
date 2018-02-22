require(quanTLC,quietly=TRUE)
require(rhandsontable)

fluidPage(
  titlePanel("",windowTitle = "quanTLC"),
  tabsetPanel(type="pills",
              tabPanel("Input and preprocessing",
                       tags$head(tags$style(HTML(".shiny-output-error-validation {color: red;font-size: 24px}"))),
                       sidebarLayout(
                         sidebarPanel(width=3,
                                      # h3("Input"),
                                      fileInput("Input_image","Select a chromatogram image or a Rdata file"),
                                      actionButton("Demo_file","Load the demonstration file"),
                                      # fileInput("Input_method","Rdata method file save in a previous session"),
                                      rHandsontableOutput("Input_dimension"),
                                      checkboxInput("Input_convention","Change to distance calculated from the middle of the band",F),
                                      actionButton("Input_action","Extract the video densitograms",icon=icon("flask"))

                         ),
                         mainPanel(width=9,
                                   plotOutput("Input_plot_raster")
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
                         sidebarPanel(
                           # actionButton("Integration_show", "Integration options",icon = icon("edit")),
                           # bsModal("IntegrationModal", "Integration options", "Integration_show", size = "large",
                           #         uiOutput("Integration_ui_1")
                           # ),
                           uiOutput("Integration_ui_1"),
                           # textInput("Integration_compound","Compound name","Compound"),
                           actionButton("Integration_action","Select the peak",icon=icon("flask")),
                           actionButton("Integration_one_by_one_show","Integration one by one",icon=icon("edit")),
                           bsModal("Integration_one_by_one_Modal", "Integration one by one", "Integration_one_by_one_show", size = "large",
                                   div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton("Integration_one_by_one_previous","Previous")),
                                   div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput("Integration_one_by_one_select","Selection of the track",1)),
                                   div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton("Integration_one_by_one_next","Next")),
                                   plotOutput("Integration_one_by_one_chrom",brush = brushOpts(
                                     id = "brush.Integration_one_by_one_chrom",
                                     direction = "x",
                                     resetOnNew = T
                                   )),
                                   p("Brush each plot separatly and the integration will be automatically updated")
                           )
                         ),
                         mainPanel(
                           numericInput("Integration_plot_chrom_select","Selection of the track",1),
                           plotOutput("Integration_plot_chrom",brush = brushOpts(
                             id = "brush.Integration_plot_chrom",
                             direction = "x",
                             resetOnNew = T
                           ))
                           # ,tableOutput("Integration_table")
                         )
                       ),
                       sidebarLayout(
                         sidebarPanel(width=6,
                                      rHandsontableOutput("Stat_batch"),
                                      uiOutput("Stat_column"),
                                      checkboxInput("Stat_quadratic","Use quadratic regression",F),
                                      # checkboxInput("Stat_origin","Pass by the origin)",F),
                                      actionButton("Stat_action","Apply the batch",icon=icon("flask")),
                                      verbatimTextOutput("Stat_summary")
                         ),
                         mainPanel(width=6,
                                   plotOutput("Stat_plot")
                         )
                       )
              ),
              tabPanel("Report",
                       sidebarLayout(
                         sidebarPanel(width=6,
                                      checkboxGroupInput("Report_options","Include in report",choices = c("Chromatogram","Dimension table","Preprocessing options","Integration options","Statistic options","Video-densitograms","Model summary","Batch","Calibration curve"),
                                                         selected = c(
                                                           "Chromatogram"
                                                           ,"Dimension table"
                                                           ,"Preprocessing options"
                                                           ,"Integration options"
                                                           ,"Statistic options"
                                                           ,"Model summary"
                                                           ,"Video-densitograms"
                                                           ,"Batch"
                                                           ,"Calibration curve"
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
                       h4("Video manual"),
                       HTML('<video src="out.mp4" controls/>')
              )
  )

)
