
f.integrate_extracted <- function(data,channel,negatif=F,correct.baseline=F,start,end,wm=50,ws=5,plotting=F){
  if(length(start) != length(end)){
    stop("start and end do not have the same lenght")
  }
  data = data[,,channel]
  if(negatif==T){data <- 1-data}
  if(correct.baseline==T){
    data = baseline(spectra = data,method = "rollingBall",wm=wm,ws=ws) %>% getCorrected()
  }
  store = matrix(NA,nrow = nrow(data),ncol=length(start))
  for(j in seq(length(start))){
    for(i in seq(nrow(data))){
      store[i,j] = sum(data[i,start[j]:end[j]])
    }
  }
  if(plotting==T){
    for(i in seq(nrow(data))){
      if(i == 1){
        plot(data[i,],type="l",ylim=c(0,max(data)),xlab="pixel indexes",ylab="Intensity (A.U.)")
      }else{
        par(new=T)
        plot(data[i,],type="l",ylim=c(0,max(data)),xlab="",ylab="")
      }
    }
    abline(v=start,col="green")
    abline(v=end,col="red")
  }
  store
}



