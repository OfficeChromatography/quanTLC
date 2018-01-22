##' function to plot an image
##' @param data an array, or a matrix
##' @param main an optionnal title
##' @param Title.dim should the dimension be indicated on the plot
##' @param ... Arguments to be passed to methods
##' @examples
##' data <- f.read.image(source='www/rTLC_demopicture.JPG',height=128)
##' data %>% raster(main='HPTLC plate',Title.dim=T)
##' @author Dimitri Fichou
##' @export

raster <- function(data,main='',Title.dim=F,interpolate = F,...){ # a 3D array
  # par(mar=c(0,0,2,0),oma=c(0,0,2,0))
  if(length(dim(data))>2){if(dim(data)[3] > 3){data <- data[,,c(1,2,3)]}}
  # print('SOM.cluster.plot.picture')
  # print(dim(data))
  if(Title.dim == T){main=paste0('Dimensions: ',paste(dim(data),collapse='-'),'\n',main)}
  plot(c(0,dim(data)[2]),c(0,dim(data)[1]), type='n',ylab="",xlab="",main=main,...)
  rasterImage(data,0,0,dim(data)[2],dim(data)[1],interpolate=interpolate)

}
