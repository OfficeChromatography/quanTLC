
f.eat.image<-function(data,conv="linomat",largeur=200,dist.gauche=20,band=6,ecart=2,tolerance=1,cropping = 0,nbr.band=NULL,double = F){
  if(length(dim(data)) == 2){ # array coertion
    data = array(data,dim=c(dim(data),3))
  }
  if(dim(data)[3] == 4){# check if gray channel not already present
    data = data[,,1:3]
  }
  if(double){
    data=abind(data[(dim(data)[1]/2+1):dim(data)[1],1:dim(data)[2],],data[(dim(data)[1]/2):1,dim(data)[2]:1,],along=3)
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
  if(double){
    store = abind(store[,,1:3],store[,,4:6],along=1)
  }
  data = store
  store = array(0,dim=c(dim(data)[1:2],4))
  store[,,1:3] = data
  store[,,4] = apply(data,1:2,mean)
  return(store)
}
