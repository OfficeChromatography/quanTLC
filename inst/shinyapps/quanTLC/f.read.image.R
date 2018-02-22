f.read.image<-function(source,height=NULL,Normalize=F,ls.format=F){
  ls <- list()
  for(i in source){
    try(data<-readTIFF(i,native=F),silent = T) # we could use the magic number instead of try here
    try(data<-readJPEG(source=i,native=F),silent = T)
    try(data<-readPNG(source=i,native=F),silent = T)
    try(data<-read.bmp(i, Verbose = FALSE)/255,silent = T)
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
