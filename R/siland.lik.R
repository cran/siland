siland.lik<-function(res,land,data,varnames=NULL,seqd=seq(2,2000,length=10))
{

  model=res$formula
 # if(res$modelType=="GLMM")
#    stop("This function don't take into account mixed effects")
#  if(res$modelType=="LMM")
#    stop("This function don't take into account mixed effects")

#varnames are variables used to explore variations around sif parameters
   if(is.null(varnames))
    varnames=colnames(res$landcontri)

  resfind=findInterval(res$paramSIF,range(seqd))
  if(sum(resfind)==length(res$paramSIF))
    seqd=sort(c(seqd,res$paramSIF))

  if(class(land)[1]!="list")
    land=list(land)
  sfGIS=lapply(land,st_as_sf)

  if(class(data)[1]!="list")
    data=list(data)

  if(length(data)!=length(land))
    stop("The number of datasets for argument data have to be equal to the number of GIS objects for argument land")



  loc.sf=list(NULL)
  for(i in 1:length(data))
  {
    tmp=data[[i]][,c("X","Y")]
    loc.sf[[i]]=st_as_sf(tmp,coords = c("X","Y"))
    st_crs(loc.sf[[i]])<-st_crs(sfGIS[[i]])
  }


  landnames=names(land[[1]])
  datanames<-names(data[[1]])
  allvars=all.vars(res$formula)
  vary=allvars[1]
  varx=allvars[-1]
  #localvars=varx[varx%in%datanames]
  landvars=varx[varx%in%landnames]


  #datanames<-names(data[[1]])
  #landnames=names(sfGIS[[1]])
  #order varnames in the same way that landnames
  #varnames=landnames[which(landnames%in%varnames)]

  #if(sum(varnames%in%landnames)<length(varnames))
   # stop("Error: Some varnames are not variable in object land ")


  #extract landscape variables
  #localvars=datanames[datanames%in%allvars]
  #landvars=landnames[landnames%in%allvars]
  #localvars=allvars[allvars%in%datanames]
  #landvars=allvars[allvars%in%landnames]
  #if(is.null(varnames))
  #  varnames=landvars


  landraster=list(NULL)
  for(l in 1:length(land))
    landraster[[l]]=landtoraster(landgis=land[[l]],landname = landvars,wd=res$wd)

  if(length(land)>1)
    names(landraster)=names(land)

  DD=calcdist(data=data,landr=landraster,landgis=land ,rmax=res$maxD,border=res$border)
  #DD=calcdist(data,landraster,rmax=res$maxD)


  nvars=length(varnames)
  matlik=matrix(0,nrow=nvars, ncol=length(seqd))
  selk= which(landvars %in% varnames)
  #print(varnames)
  for(k in 1:nvars)
  {
    cat(paste("Likelihood computing for ",varnames[k]))
    cat("\n")
    newd=res$paramSIF
    for(i in 1:length(seqd))
    {
      newd[selk[k]]=seqd[i]

      if(res$modelType=="GLM")
        matlik[k,i]= silandMinusLoglik(newd,Dist=DD,land=landraster,data=data,formula=model,sif=res$sif,family=res$family,w=res$wd)
      if(res$modelType=="LMM")
        matlik[k,i]= silandMinusLoglikLMM(newd,Dist=DD,land=landraster, data=data,formula=model,sif=res$sif,family=res$family,w=res$wd)
      if(res$modelType=="GLMM")
        matlik[k,i]= silandMinusLoglikGLMM(newd,Dist=DD,land=landraster,data=data,formula=model,sif=res$sif,family=res$family,w=res$wd)
    }
  }
 #matlik<<-matlik

  colnames(matlik)=seqd
  rownames(matlik)=varnames
  #matlik=cbind(id=1:nvars,t(matlik))
  matlik=rbind(matlik,"Estimated Model"=-res$loglik)
  matlik2=reshape2::melt(matlik)
  colnames(matlik2)=c("Var1","Var2","value")


  linee=c("estimated model"=2)
  cols=c("estimated model"="darkorange")
 #if(sum(resfind)!=length(res$paramSIF))
  #options(warn=-1)
  mycolR=palette()

  pp=ggplot(matlik2, aes_string(x="Var2",y="value",group="Var1"))+geom_line(aes_string(color="Var1"))+
    xlab("SIF parameter")+ylab("- Log-likelihood")+xlim(range(seqd))+
    theme(legend.position="top",legend.title=element_blank())+
    geom_vline(xintercept=res$paramSIF[selk],color=mycolR[1:nvars],lty=2)+
    geom_hline(yintercept=-res$loglik,color="darkorange")+
    scale_color_manual(values=c(mycolR[1:nvars],"darkorange"))
  #plot(pp)
  #if(sum(resfind)!=length(res$paramSIF))
  # options(warn=0)

  return(pp)
  #invisible(return(matlik))
}

