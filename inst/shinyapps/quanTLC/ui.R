require(quanTLC,quietly=TRUE)
require(rhandsontable)
source('module_data_extraction.R',local = T)
source('module_data_preprocessing.R',local = T)
source('module_data_integration.R',local = T)
source('module_report.R',local=T)
source('module_help.R',local=T)



fluidPage(
  titlePanel("",windowTitle = "quanTLC"),
  tags$head(tags$style(HTML(".shiny-output-error-validation {color: red;font-size: 24px}"))),
  tabsetPanel(type="pills",
              tabPanel("Input and preprocessing",
                       data_extraction_ui('data_extraction'),
                       data_preprocessing_ui('data_preprocessing')
              ),
              tabPanel("Integration and statistics",
                       data_integration_ui('data_integration')
              ),
              report_ui('report'),
              help_ui('help')
  )

)
