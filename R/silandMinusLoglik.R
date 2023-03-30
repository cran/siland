silandMinusLoglik<-function(d,Dist,land,data,formula,sif,family,w)
{
  #options(warn=-1)
  #print(d)
  #compute the minus loglikelihood for parameter
  # of fis fucntion, that is the mean distance
  #data are local observations
  #land are list of landscape variables
  for(i in 1:length(d))
  {
    if(d[i]<0)
    {
      mloglik=10^6
      return(mloglik)
    }
    #if(d[i]>2000 )
    #{
    #  mloglik=10^6
    #  return(mloglik)
    #}
  }
  #w=min(dist(land[[1]][1:10,c("X","Y")]))
  #w=sqrt(abs(diff(sort(unique(land[[1]][,1]))[1:2])*diff(sort(unique(land[[1]][,2]))[1:2])))
  #w=min(dist(raster::coordinates(land[[1]])[1:500,],upper = T))
  #calcselraster<-NULL
  #Dist=calcdist2(data,land,rmax=rmax)
  #landcontri<<-landcontri
  if(length(data)==1)
  {
    landcontri=calcscontri(distmoy=d,Distobs=Dist,sif=sif,w=w)
    #print(d)
    colnames(landcontri)=names(land)
    newdata=as.data.frame(cbind(data[[1]],landcontri))
    colnames(newdata)=c(colnames(data[[1]]),names(land[[1]]))
  }
  else
  {
    #matB=calcscontri(distmoy=d,Distobs=Dist,sif=sif,w=w)
    #matcontri=NULL
    landcontri=calcscontri(distmoy=d,Distobs=Dist,sif=sif,w=w)
    #colnames(landcontri)=landvars
    newdata=NULL
    for(i in 1:length(data))
    {
      newdata=rbind(newdata,data[[i]])
    }
    newdata=cbind(newdata,landcontri)
    colnames(newdata)=c(colnames(data[[1]]),names(land[[1]]))

  }



  #resout=glm(formula,data=newdata,family=family)
  #newdata<<-newdata
  #if( inherits(rr <- try(glm(as.formula(formula),data=newdata,family=family), silent = TRUE), "try-error"))
  #  mloglik= 10^6
  #else
  #  mloglik=as.numeric(-logLik(rr))
  rr=glm(as.formula(formula),data=newdata,family=family)
  mloglik=as.numeric(-logLik(rr))
  # options(warn=0)
  invisible(return(mloglik))
}

