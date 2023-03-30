
calcdistland=function(data,land)
{
  #compute distance between observation location of data
  #and point source location of list land
  tmp=data[,c("X","Y")]
  Distobs=list()
  p=length(land)
  for(i in 1:p){
    Distobs[[i]]=t(apply(tmp,1,function(x){sqrt((x[1]-land[[i]][,"X"])^2+(x[2]-land[[i]][,"Y"])^2)}))
  }
  names(Distobs)=paste("Dist",1:p,sep="")
  return(Distobs)
}


