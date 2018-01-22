##' Function to read image files
##'
##' @param source the path of the file
##' @param height redimension the file
##' @param Normalize should the data be normalized, i.e. force the values on each channel between 0 and 1
##' @param ls.format boolean to keep the pictures in list format
##' @examples
##' data <- f.read.image('www/rTLC_demopicture.JPG',height=256)
##' data %>% raster()
##' @author Dimitri Fichou
##' @export
f.read.image<-function(source,height=NULL,Normalize=F,ls.format=F){
  ls <- list()
  for(i in source){
    try(data<-readTIFF(i,native=F),silent = T) # we could use the magic number instead of try here
    try(data<-readJPEG(source=i,native=F),silent = T)
    try(data<-readPNG(source=i,native=F),silent = T)
    if(!is.null(height)){
      data <- redim.array(data,height)
    }
    if(Normalize == T){data <- data %>% normalize}
    ls[[i]]<- data
  }
  if(ls.format == F){
    data <- abind(ls,along=2)
  }else{
    data <- ls
  }
  return(data)
}
