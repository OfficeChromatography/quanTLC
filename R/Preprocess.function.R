##' function to do the full preprocess
##' @param data an array, or a matrix
##' @param preprocess.order preprocess.order
##' @param preprocess.option preprocess.option
##' @param training.data training.data
##' @param ... Arguments to be passed to methods
##' @author Dimitri Fichou
##' @export
f.preprocess <- function(data,preprocess.order,preprocess.option,training.data=data){
  for(i in preprocess.order){
    if(i == 'Warping'){
      data <- eval(parse(text=paste0(i,'(data,preprocess.option,training.data)')))
    }
    if(i == 'gammaCorrection'){
      data <- data^preprocess.option$gammaCorrection
    }
    if(i == 'Baseline.correction'){
      data <- eval(parse(text=paste0(i,'(data,preprocess.option)')))
    }
    if(i == 'Smoothing'){
      data <- eval(parse(text=paste0(i,'(data,preprocess.option)')))
    }
    if(i == "Negatif"){
      data = 1-data
    }
    if(i == 'Baseline.correction'){
      data <- eval(parse(text=paste0(i,'(data,preprocess.option)')))
    }
  }
  return(data)
}

Baseline.correction <- function(data,input){
  input <- input$Baseline
  data.tot <- data
  for(i in seq(4)){
    data<-data.tot[,,i]
    if(is.vector(data)){
      dim(data) = c(1,length(data))
    }
    if(input$method == "als"){data<-baseline::baseline(data,method=input$method,lambda=input$lambda.1,p=input$p,maxit=input$maxit.1)}
    if(input$method == "fillPeaks"){data<-baseline::baseline(data,method=input$method,lambda=input$lambda.2,hwi=input$hwi,it=input$it,int=input$int)}
    if(input$method == "irls"){data<-baseline::baseline(data,method=input$method,lambda1=input$lambda1,lambda2=input$lambda2,maxit=input$maxit.2,wi=input$wi)}
    if(input$method == "lowpass"){data<-baseline::baseline(data,method=input$method,steep=input$steep,half=input$half)}
    if(input$method == "medianWindow"){data<-baseline::baseline(data,method=input$method,hwm=input$hwm,hws=input$hws,end=input$end)}
    if(input$method == "modpolyfit"){data<-baseline::baseline(data,method=input$method,degree=input$degree,tol=input$tol,rep=input$rep)}
    if(input$method == "peakDetection"){data<-baseline::baseline(data,method=input$method,left=input$left,right=input$right,lwin=input$lwin,rwin=input$rwin)}
    if(input$method == "rfBaseline"){data<-baseline::baseline(data,method=input$method)}
    if(input$method == "rollingBall"){data<-baseline::baseline(data,method=input$method,wm=input$wm,ws=input$ws)}
    data.tot[,,i]<-getCorrected(data)
  }
  return(data.tot)
}

Smoothing <- function(data,input){
  input <- input$Smoothing
  data.a<-aaply(data[,,1],.margins=c(1),.fun=savitzkyGolay,w=input$window.size,p=input$poly.order,m=input$diff.order)
  data.b<-aaply(data[,,2],.margins=c(1),.fun=savitzkyGolay,w=input$window.size,p=input$poly.order,m=input$diff.order)
  data.c<-aaply(data[,,3],.margins=c(1),.fun=savitzkyGolay,w=input$window.size,p=input$poly.order,m=input$diff.order)
  data.d<-aaply(data[,,4],.margins=c(1),.fun=savitzkyGolay,w=input$window.size,p=input$poly.order,m=input$diff.order)
  return(abind(data.a,data.b,data.c,data.d,along=3))
}

Warping <- function(data,input,training.data){
  input <- input$Warping
  if(input$warpmethod == "ptw"){
    truc <- c(init.coef=input$ptw.init.coef,warp.type=input$ptw.warp.type,
              optim.crit=input$ptw.optim.crit,trwdth=input$ptw.trwdth)
    data <- do.ptw(data=data,ref=input$ptw.warp.ref,training.data =training.data)
  }
  if(input$warpmethod=='dtw'){
    data <- do.dtw(data=data,ref=input$dtw.warp.ref,training.data =training.data,split=input$dtw.split)
  }
  return(data)
}

do.dtw <- function(data,ref,training.data,split=T){
  if(split == T){
    for(i in seq(4)){
      for(j in seq(nrow(data))){
        al <- dtw(data[j,,i],training.data[ref,,i])
        truc <- redim(rbind(data[j,al$index1,i],training.data[ref,al$index2,i]),2,ncol(data))[1,]
        data[j,,i] <- truc
      }
    }
  }else{
    for(j in seq(nrow(data))){
      al <- dtw(data[j,,],training.data[ref,,])
      truc <- redim(data[j,al$index1,],ncol(data),4)
      data[j,,] <- truc
    }
  }
  return(data)
}

do.ptw <- function(data,ref,training.data){
  data[,,1]<-ptw(ref = training.data[ref,,1],data[,,1])$warped.sample
  data[,,2]<-ptw(ref = training.data[ref,,2],data[,,2])$warped.sample
  data[,,3]<-ptw(ref = training.data[ref,,3],data[,,3])$warped.sample
  data[,,4]<-ptw(ref = training.data[ref,,4],data[,,4])$warped.sample
  # data<-abind(data.a,data.b,data.c,data.d,along=3)
  data[is.na(data)] <- 0
  return(data)
}
