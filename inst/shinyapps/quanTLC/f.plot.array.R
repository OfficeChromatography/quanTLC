
f.plot.array<-function(data,id,hauteur,Zf,dist.bas,reconstruct=T,inverse=F,ylim.raster=1.3,main=NULL,...){
  xlim = c(dim(data)[2],0)
  if(reconstruct==T){
    plot(as.vector(data[id,,1]),
         ylim=c(0,ylim.raster),xlim=xlim,main=main,xlab=expression(italic(R)['F']),ylab='Pixel intensity [AU]',xaxt="n",
         type="l",col="red",...)
    par(new=T)
    plot(as.vector(data[id,,2]),
         ylim=c(0,ylim.raster),xlim=xlim,xlab='',ylab='',xaxt="n",yaxt="n",
         type="l",col="green",...)
    par(new=T)
    plot(as.vector(data[id,,3]),
         ylim=c(0,ylim.raster),xlim=xlim,xlab='',ylab='',xaxt="n",yaxt="n",
         type="l",col="blue",...)
    if(inverse==F){
      data.plot<-round(array(data[id,,c(1,2,3)],dim=c(1,dim(data)[2],3))*256)/256
      rasterImage(data.plot,0 , 1.1, dim(data)[2], 1.3)
    }else{
      data.plot<-round(array(data[id,dim(data)[2]:1,c(1,2,3)],dim=c(1,dim(data)[2],3))*256)/256
      rasterImage(data.plot, 0 , 1.1, dim(data)[2], 1.3)
    }
  }else{
    plot(as.vector(data[id,,1]),
         ylim=c(min(data),max(data)),xlim=xlim,main=main,xlab=expression(italic(R)['F']),ylab='Pixel intensity [AU]',xaxt="n",
         type="l",col="red",...)
    par(new=T)
    plot(as.vector(data[id,,2]),
         ylim=c(min(data),max(data)),xlim=xlim,xlab='',ylab='',xaxt="n",yaxt="n",
         type="l",col="green",...)
    par(new=T)
    plot(as.vector(data[id,,3]),
         ylim=c(min(data),max(data)),xlim=xlim,xlab='',ylab='',xaxt="n",yaxt="n",
         type="l",col="blue",...)
    # par(new=T)
    # plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,4]),
    #      ylim=c(min(data),max(data)),xlim=xlim,xlab='',ylab='',xaxt="n",yaxt="n",
    #      type="l",col="black",...)
  }
  # mtext(side = 1, expression(italic(R)['F']), line = 1.5,cex.axis=0.9,...)
  # mtext(side = 2, "Pixel intensity [AU]", line = 1.5,cex.axis=0.9,...)
  axis(side=1,at=dim(data)[2]-seq(dim(data)[2]/hauteur*dist.bas,dim(data)[2]/hauteur*Zf,length.out = 11),labels=seq(0,1,length.out = 11))
}
