##' Function to extract chromatograms from an 3d array, or a 2d matrix
##'
##' @param data input array or matrix
##' @param conv convention for the dimensions, should the dimension correspond to the left part of the band or the center of the band
##' @param largeur plate length in mm
##' @param dist.gauche distance from the left side of the plate in mm
##' @param band band widthin mm
##' @param ecart gap between band in mm
##' @param tolerance mm to remove on each side of the band in order to take only the center
##' @param cropping mm to remove on the left part of the plate in the largeur and dist.gauche arameter in order to counter the intempestive camag cropping
##' @param nbr.band if not specified, the software assume it itself, otherwise will force the number of band, could lead to an error if not enough space
##' @param plotting will plot the raster to check the extraction
##' @examples
##' data <- f.read.image('www/rTLC_demopicture.JPG',height=256)
##' data <- f.eat.image(data,conv="linomat",largeur = 200,dist.gauche=20,band=6,ecart=2,tolerance=1,cropping = 0,nbr.band=20,plotting=T)
##' data %>% raster()
##' @author Dimitri Fichou
##' @export
f.eat.image<-function(data,conv="linomat",largeur=200,dist.gauche=20,band=6,ecart=2,tolerance=1,cropping = 0,nbr.band=NULL,plotting=F){
  if(length(dim(data)) == 2){ # array coertion
    data = array(data,dim=c(dim(data),1))
  }
  # cropping correction
  largeur = largeur - 2 * cropping
  dist.gauche = dist.gauche - cropping
  if(conv != "linomat"){ # this put everybody back to linomat convention
    dist.gauche<-dist.gauche-band/2
    ecart<-ecart-band
  }
  if(is.null(nbr.band)){
    nbr.band<-round((largeur-2*dist.gauche)/(band+ecart))
  }

  store <- array(dim = c(nbr.band,dim(data)[1],dim(data)[3]))

  for(j in seq(dim(data)[3])){
    for(i in c(0:(nbr.band-1))){
      store[i+1,,j] <-apply(data[,(dim(data)[2]/largeur*((dist.gauche+tolerance)+i*(band+ecart))):(dim(data)[2]/largeur*((dist.gauche+band-tolerance)+i*(band+ecart))),j],1,mean)
    }
  }
  data = store
  store = array(0,dim=c(dim(data)[1:2],4))
  store[,,1:3] = data
  store[,,4] = apply(data,1:2,mean)
  if(plotting){
    raster(data)
    for(i in c(0:(nbr.band-1))){
      abline(v=(dim(data)[2]/largeur*((dist.gauche+tolerance)+i*(band+ecart))),col="green")
      abline(v=(dim(data)[2]/largeur*((dist.gauche+band-tolerance)+i*(band+ecart))),col="red")
    }
  }
  return(store)
}
