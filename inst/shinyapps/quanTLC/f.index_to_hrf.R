# f.index_to_hrf

f.index_to_hrf = function(index,width,dist.bas,Zf,data){
  round(seq((width-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2])[index],2)*100
}