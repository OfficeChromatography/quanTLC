##' function to integrate peaks from 3d chromato array
##'
##' @param data an array as returned by the function f.eat.image
##' @param channel numeric for the channel to use (1 for red, 2 for green and 3 for blue)
##' @param correct.baseline use or not the rolling ball algorithm to correct the baseline
##' @param negatif apply a transformation to put the data in negatif, needed for white light pictures and for some extracted featuress from the network
##' @param start starting pixel index
##' @param end stopping pixel index
##' @param wm Width of local window for minimization/maximization
##' @param ws Width of local window for smoothing
##' @param plotting should a plot be display to check the integration
##' @param ... Arguments to be passed to methods
##' @examples
##' data <- f.read.image(source='www/rTLC_demopicture.JPG',height=128)
##' ## incoming
##' @author Dimitri Fichou
##' @export

# data <- f.read.image("inst/extdata/rTLC_demopicture.JPG",256)
# dim.table <- rbind(c(1,2,50,60,100,110),c(1,2,70,80,100,110))
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



