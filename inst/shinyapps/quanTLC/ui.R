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
                                      fileInput("Input_image","Select a chromatogram image"),
                                      # fileInput("Input_method","Rdata method file save in a previous session"),
                                      checkboxInput("Input_convention","Change for convention from the interior of the band",F),
                                      rHandsontableOutput("Input_dimension"),
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
                                                     choices=c("Negatif" = "Negatif","Gamma correction" = 'gammaCorrection','Smoothing' = 'Smoothing',
                                                               'Baseline correction' = 'Baseline.correction','Warping' = 'Warping'),
                                                     selected='',multiple=T),
                                      actionButton("Preprocess_action","Apply the preprocesses",icon=icon("flask")),
                                      bsModal("preprocessModal", "Preprocessing options", "Preprocess_show", size = "large",
                                              uiOutput("Preprocess_ui_1")
                                      )

                         ),
                         mainPanel(width=9,
                                   numericInput("Preprocess_plot_chrom_select","Track to plot",1),
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
                           actionButton("Integration_action","Select the peak",icon=icon("flask"))
                         ),
                         mainPanel(
                           numericInput("Integration_plot_chrom_select","Track to plot",1),
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
                                      checkboxInput("Stat_quadratic","Use a quadratic model",F),
                                      checkboxInput("Stat_origin","Pass by the origin)",F),
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
                                      checkboxGroupInput("Report_options","Inculde in report",choices = c("Chromatogram","Dimension table","Preprocessing options","Integration options","Statistic options","Video-densitograms","Model summary","Batch","Calibration curve"),
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
                                      radioButtons('reportformat', 'Document format', c('PDF', 'HTML', "MS word"='Word'),
                                                   inline = TRUE),
                                      downloadButton('downloadReport')
                         ),
                         mainPanel(width=6,
                                   verbatimTextOutput("Report_reac"),
                                   p("incoming")
                         )
                       )
              ),
              tabPanel("Video manual",
                       HTML('<video src="out.mp4" controls/>')
              )
  )

)
