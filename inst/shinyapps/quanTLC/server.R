
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
  source('module_data_extraction.R',local = T)
  source('module_data_preprocessing.R',local = T)
  source('module_data_integration.R',local = T)
  source('module_report.R',local=T)
  source('module_help.R',local=T)
  # updateTextInput(session, "Integration_compound", value = "Compound")
  
  ## this object will store the data at each step of the analysis
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
  #### Input ####
  data_extraction_server('data_extraction')
  ### Preprocess ####
  data_preprocessing_server('data_preprocessing')
  ### Integration and Stat ####
  data_integration_server('data_integration')
  #### Report and help ####
  report_server('report')
  help_server('help')

})

