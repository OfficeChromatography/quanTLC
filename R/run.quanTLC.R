##' Function to launch the quanTLC app
##'
##' @param port if not NULL will deploy on the local network on this port, just look for the IP of the host and go for example on 192.168.1.20:port to access the app from anywhere in the network
##' @author Dimitri Fichou
##' @examples
##' run.quanTLC()
##' @export
##'

run.quanTLC <- function(port=NULL, launch.browser=T) {
  appDir <- system.file("shinyapps", "quanTLC", package = "quanTLC")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  if(is.null(port)){
    shiny::runApp(appDir, display.mode = "normal",launch.browser = launch.browser)
  }else{
    shiny::runApp(appDir, display.mode = "normal",
                  launch.browser = launch.browser,host = '0.0.0.0',port=port)
  }
}
