calcscontri=function(distmoy,Distobs,w=1,sif="exponential")
{
  #compute contributions of sources that arise to each observation location.
  #nT the number of dataset / landscape
  #p the number of landscape variable
  #n the number of observed points
  nT=length(Distobs)
  p=length(Distobs[[1]])
  n=rep(0,nT)
  for(i in 1:nT)
  n[i]=length(Distobs[[i]][[1]])

  if (length(distmoy)==1){distmoy=rep(distmoy,p)}

  scontri=matrix(0,ncol=p,nrow=sum(n))


  if(sif=="exponential"){
    for(t in 1:nT)
    for(l in 1:p)
      {
      tmp=unlist(lapply(Distobs[[t]][[l]],function(x,vard=distmoy[l]){sum(fdispE(x,vard))}))
      tmp[is.na(tmp)]<-0
      if(t==1)
        ind=1:n[1]
      else
        ind=c(sum(n[1:(t-1)]) +1 ) :  c(sum(n[1:t]))

      scontri[ind,l]<-as.vector(tmp)

    }
    }


   if(sif=="gaussian"){
    for(t in 1:nT)
    for(l in 1:p)
      {
      tmp= unlist(lapply(Distobs[[t]][[l]],function(x,vard=distmoy[l]){sum(fdispG(x,vard))}))
      tmp[is.na(tmp)]<-0
      if(t==1)
        ind=1:n[1]
      else
        ind=c(sum(n[1:(t-1)]) +1 ) :  c(sum(n[1:t]))
      scontri[ind,l]<-as.vector(tmp)
    }
   }

  if(sif=="uniform"){
    for(t in 1:nT)
    for(l in 1:p)
      {
      tmp= unlist(lapply(Distobs[[t]][[l]],function(x,vard=distmoy[l]){sum(fdispU(x,vard))}))
      tmp[is.na(tmp)]<-0
      if(t==1)
        ind=1:n[1]
      else
        ind=c(sum(n[1:(t-1)]) +1 ) :  c(sum(n[1:t]))
      scontri[ind,l]<-tmp
    }
  }



  #--------------------




  scontric=scontri*w^2

  return(scontric)
}

