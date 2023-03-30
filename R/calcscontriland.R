


calcscontriland=function(distmoy,Distobs,w=1,sif="exponential")
{
  #compute contributions of sources that arise to each observation location.
  p=length(Distobs)
  n=nrow(Distobs[[1]])
  if (length(distmoy)==1){distmoy=rep(distmoy,p)}
  contri=list(NULL)
  if(sif=="exponential")
    for(i in 1:p) contri[[i]]= fdispE(Distobs[[i]],distmoy[i])

  if(sif=="gaussian")
    for(i in 1:p) contri[[i]]= fdispG(Distobs[[i]],distmoy[i])

  if(sif=="uniform")
    for(i in 1:p) contri[[i]]= fdispU(Distobs[[i]],distmoy[i])

  scontri=matrix(0,ncol=p,nrow=n)
  for(i in 1:p) scontri[,i]=apply(as.matrix(contri[[i]]),1,sum)

  scontric=scontri*w^2

  return(scontric)
}

