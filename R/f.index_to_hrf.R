##' Function used to determine hrf from index given experiment parameters
##'
##' @param index index
##' @param width width of the plate
##' @param dist.bas dist.bas
##' @param Zf Zf
##' @param data image of chromatograms
##' @author Dimitri Fichou
##' @export
f.index_to_hrf = function(index,width,dist.bas,Zf,data){
  round(seq((width-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2])[index],2)*100
}