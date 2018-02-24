
f.plot.array<-function(data,id,hauteur,Zf,dist.bas,reconstruct=T,ylim.raster=1.3,main=NULL,xlim=NULL,channel = NULL,...){
  colors = c("red","green","blue","gray")
  if(is.null(xlim)){xlim = c(dim(data)[2],0)}
  if(reconstruct){
    ylim=c(0,ylim.raster)
  }else{
    ylim=c(min(data),max(data))
  }
  for(i in seq(4)){
    if(is.null(channel)){
      plot(as.vector(data[id,,i]),ylim=ylim,xlim=xlim,xlab="",ylab='',xaxt="n",yaxt="n",type="l",col=colors[i],...)
      
      if(reconstruct){
        data.plot<-round(array(data[id,,c(1,2,3)],dim=c(1,dim(data)[2],3))*256)/256
        rasterImage(data.plot,0 , 1.1, dim(data)[2], ylim.raster)
      }
      par(new=T)
    }else{
      if(i == channel){
        plot(as.vector(data[id,,i]),ylim=ylim,xlim=xlim,xlab="",ylab='',xaxt="n",yaxt="n",type="l",col=colors[i],...)
        # 
        # if(reconstruct){
        #   data.plot<-round(array(data[id,,c(1,2,3)],dim=c(1,dim(data)[2],3))*256)/256
        #   rasterImage(data.plot,0 , 1.1, dim(data)[2], ylim.raster)
        # }
        par(new=T)
      }
    }
  }
  
  title(main = main, xlab = expression(italic(hR)['F']), ylab = "Pixel intensity [AU]")
  axis(side=1,at=dim(data)[2]-seq(dim(data)[2]/hauteur*dist.bas,dim(data)[2]/hauteur*Zf,length.out = 11),labels=seq(0,1,length.out = 11)*100)
  axis(side=2)
  # plot(1,1,type="n",main=main,xlab=expression(italic(hR)['F']),ylab='Pixel intensity [AU]')
}
