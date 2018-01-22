##' Function to plot the 3 channel of an array as chromatograms,
##'
##' @param data a 3D array of dim[3] = 3
##' @param x the x coordinate to look at
##' @param normalization either or not normalisation should happen before plotting the chromatograms
##' @param edge to apply a mean and avoid noise
##' @param ... extra parameters to be passed to the plot function
##' @author Dimitri Fichou
##' @export
##'

chrom.pict <- function(data,x,normalization=T,edge=0,...){
  if(length(dim(data)) == 3){
    data.save <- data
    if(edge != 0){
      data <- array(apply(data[dim(data)[1]:1,(x-edge):(x+edge),seq(3)],c(1,3),mean),dim=c(dim(data)[1],1,3))
    }else{
      data <- array(data[dim(data)[1]:1,x,seq(3)],dim=c(dim(data)[1],1,3))
    }
    # par(mar=c(0,0,0,0))
    plot(c(0,dim(data)[1]),c(0,1.2), type='n',ylab="",xlab="",bty='n', ##input$Kohonen.1.height = 128
         main='')
    data %>% aperm(c(2,1,3)) %>% rasterImage(0,1.1,dim(data)[1] ,1.2) ##input$Kohonen.1.height = 128
    color <- c('red','green','blue','black')
    if(normalization==T){data <- data.save %>% normalize}else{data <- data.save}
    if(edge != 0){
      data <- array(apply(data[dim(data)[1]:1,(x-edge):(x+edge),seq(3)],c(1,3),mean),dim=c(dim(data)[1],1,3))
    }else{
      data <- array(data[dim(data)[1]:1,x,seq(3)],dim=c(dim(data)[1],1,3))
    }
    for(i in seq(dim(data)[3])){
      par(new=T)
      plot(y=data[,,i],x=seq(0,1,length.out=dim(data)[1]),type='l',ylab="",xlab="",ylim=c(0,1.2),col=color[i],xaxt="n",...)
    }
  }else{
    plot(data[dim(data)[1]:1,x],type='l',...)
  }
}
