siland<-function(formula,land,data,family="gaussian",sif="exponential",init=100,border=F,wd=50,maxD=3000)
{
  #land : a list of gis files (in case of several years) or one gis file (for one year)
  #for the presence  of a landscape variable. The length of land list is the number of landscape varibale
  #data : a dataframe gives the variable of interest. Local variables have to be givent also in
  #this datraframe. The location of the obervations have to be given by varible "X" and "Y"
  # check arguments

### Check some arguments ------------


  if (is.character(family))
  {
    if(!(family%in%c("gaussian","poisson","binomial")))
      stop("Problem: family argument has to be \"gaussian\",\"poisson\" or \"binomial\" ")
    family <- get(family, mode = "function", envir = parent.frame())
  }
  if (is.function(family))
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }

  #if(!is.data.frame(data))
  #  stop("data argument have to be a dataframe.")

  if(!(sif%in%c("gaussian","exponential","uniform")))
    stop("Problem: sif argument has to be \"gaussian\", \"exponential\" or \"uniform\" ")


#Define type of model------------------

  model=as.formula(formula)
  modelType="GLM"
  termMix=findbars(model)
  if(!is.null(termMix))
  {
    if(isTRUE(all.equal(family,gaussian())))
      modelType="LMM"
    else
      modelType="GLMM"
  }

#---check for data et land------

  if(class(land)[1]!="list")
    land=list(land)

  if(class(data)[1]!="list")
    data=list(data)

  if(length(data)!=length(land))
    stop("The number of datasets for argument data have to be equal to the number of GIS objects for argument land")

   if(length(land)>1)
   {
     if(is.null(names(land)))
       stop("No name for elements of list for argument land")
     if(is.null(names(data)))
       stop("No name for elements of list for argument data")
     if(sum(names(data)!=names(land)))
       stop("Names for elements of list for arguments data and land have to be the same")
   }

  #test on column names for data
  datanames=colnames(data[[1]])
  if(length(data)>1)
  {
    for(i in 2:(length(data)))
    {
      tmpvari=colnames(data[[i]])
      if(sum(tmpvari==datanames)!=length(datanames))
        stop("Column names for data are not the same.")
    }
  }

  if(sum(datanames%in%c("X","Y"))!=2)
    stop("Error: colnames X and Y for observations are not available in data argument")

#--- extract local and landscapes variables ------------
  landnames=names(land[[1]])
  allvars=all.vars(model)
  vary=allvars[1]
  varx=allvars[-1]
  localvars=varx[varx%in%datanames]
  landvars=varx[varx%in%landnames]

  if(length(border)==1)
    border=rep(border,length(landvars))

  cat("Local variables: ")
  cat(paste(localvars,sep=" "))
  cat("\n")
  #extract landscape variables
  if(length(landvars)==0)
    stop("No landscape variable in the model")
  cat("Landscape variables: ")
  cat(paste(landvars,sep=" "))
  cat("\n")

#----search for interaction terms and define model0 ----------
  labelsFormula=labels(terms(nobars(model)))
  termMix=findbars(model)
  sel=NULL
  for(j in 1:length(labelsFormula))
  {
    finteraction=sum(grep(":",labelsFormula[j]))
    if(finteraction==0)
      sel=c(sel,labelsFormula[j]%in%datanames)
    else
    {
      u=unlist(strsplit(labelsFormula[j],":"))
      if(sum(u%in%landnames)>0)
        sel=c(sel,F)
      else
        sel=c(sel,T)
    }
  }
  if(is.null(termMix))
  {
    if(sum(sel)>0)
      model0=as.formula( paste(paste(vary,"~",sep=""),paste(labelsFormula[sel],collapse="+") ,sep="" ) )
    else
      model0=as.formula(paste(paste(vary,"~",sep=""),paste("1",collapse="+"),sep="" ) )
  }
  if(!is.null(termMix))
  {
    if(sum(sel)>0)
      model0=as.formula( paste(paste(vary,"~",sep="") ,paste(labelsFormula[sel],collapse="+"),"+",paste(parens(termMix),collapse="+")  ,sep=" " ) )
    else
      model0=as.formula( paste(paste(vary,"~",sep="") ,paste(parens(termMix),collapse="+")  ,sep=" " ) )
  }

  cat("Model: ")
  print(model,showEnv=F)
  cat("Model0: ")
  print(model0,showEnv=F)


#---initialisation algorithm---------------
  if(is.null(init))
    init=rep(50,length(landvars))
  if(length(init)==1)
    init=rep(init,length(landvars))
  if(length(init)>1 & (length(init)!=length(landvars)))
  stop("Problem : length(init) and number of landscape variables have to be equal.")

#--- projection for crs land---------
  #transform dataframe data in sf object
  #transform dataframe data in sf object
  #and set crs
  for(i in 1:length(land))
  {
    crsinit=st_crs(land[[i]])
    if(is.na(crsinit))
    {
      st_crs(land[[i]])<-"EPSG:2154"
    }
  }

  loc.sf=list(NULL)
  for(i in 1:length(data))
  {
    tmp=data[[i]][,c("X","Y")]
    loc.sf[[i]]=st_as_sf(tmp,coords = c("X","Y"))
    st_crs(loc.sf[[i]])<-st_crs(land[[i]])
    #if(st_is_longlat(land[[i]]))
    #{
    #  loc.sf[[i]]=st_transform(loc.sf[[i]],2154)
    #  land[[i]]=st_transform(land[[i]],2154)
    #}
  }

#------compute raster from sig files-------
  landraster=list(NULL)
  for(l in 1:length(land))
    landraster[[l]]=landtoraster(landgis=land[[l]],landname = landvars,wd=wd)

  if(length(land)>1)
    names(landraster)=names(land)

#DD is a list equal to the number of lanlscape variable
#DD[[i]] is a list and gives the distance between each observation point in data
#and all pixels for landscape variable i
#The length of DD[[i]][j] corresponds to the number of pixel for the variable i
#that are in a buffer size les than rmax from the observed point data[j,]

DD=calcdist(data=data,landr=landraster,landgis=land ,rmax=maxD,border=border)

#DD<<-DD
#--- define the function myfun for likelhood optimisation
  if(modelType=="GLM")
  {
    #data1=data[[1]]
    myfun=function(d)
    {
    return(silandMinusLoglik(d,Dist=DD,data=data,land=landraster,formula=model,sif=sif,family=family,w=wd))
    }
  }
  if(modelType=="LMM")
  {
    #data1=data[[1]]
    myfun=function(d)
    {
      return(silandMinusLoglikLMM(d,Dist=DD,data=data,land=landraster,formula=model,sif=sif,family=family,w=wd))
    }
  }
  if(modelType=="GLMM")
  {
    #data1=data[[1]]
    myfun=function(d)
    {
      return(silandMinusLoglikGLMM(d,Dist=DD,data=data,land=landraster,formula=model,sif=sif,family=family,w=wd))
    }
  }

#--opimisation depends on number of landscape variable
#--that is the length of list landraster

  if(length(landraster[[1]])>1)
  {
    #resoptim=optim(init,myfun,lower=rep(0,length(land)),method="L-BFGS-B")
    #options(warn=-1)
    #ppar=rep(1/100,length=length(init))
    #print(init)
    resoptim=optim(init,myfun)
    #resoptim=optim(par=init,myfun,method="BFGS")
    #resoptim=nlm(myfun,init)
    #options(warn=0)
  }

  if(length(landraster[[1]])==1)
  {
    #resoptim=optimize(myfun,interval=c(0,30000))
    #resoptim$par=resoptim$minimum
    resoptim=nlm(myfun,p=init)
    resoptim$par=resoptim$estimate

   # options(warn=-1)
	#resoptim=optim(init,myfun,method="Brent",lower=1,upper=2000)
	#options(warn=0)
  }

#--extract results from optimisation result
#print("End Optimisation")
  paramSIF=resoptim$par
  names(paramSIF)=paste("SIF.",landvars,sep="")
  landcontri=calcscontri(distmoy=paramSIF,Distobs=DD,sif=sif,w=wd)
  colnames(landcontri)=landvars

#-----------
  newdata=NULL
  for(i in 1:length(data))
  {
    newdata=rbind(newdata,data[[i]])
  }
  newdata=cbind(newdata,landcontri)
  #colnames(newdata)=c(colnames(data[[1]]),names(land[[1]]))


#-----------
  if(modelType=="GLM")
    restmp=glm(model,data=newdata,family=family )
  if(modelType=="LMM")
    restmp=lmer(model, data=newdata,REML=F )
  if(modelType=="GLMM")
    restmp=glmer(model,data=newdata,family=family )

  fit=predict(restmp)
  err=residuals(restmp)
  loglik=as.vector(logLik(restmp))
  sd.error=NA
  if(family$family=="gaussian" & modelType=="GLM")
    sd.error=sqrt(summary(restmp)$dispersion)
  if(family$family=="gaussian" &  modelType=="LMM")
    sd.error=summary(restmp)$sigma
  rand.StdDev=NULL

  if(modelType=="GLM")
  {
  coeflm=restmp$coef
  resestim=c(coeflm,paramSIF)
  }
  if(modelType=="LMM"||modelType=="GLMM")
  {
    coeflm=summary(restmp)$coef[,1]
    resestim=c(coeflm,paramSIF)
    rand.StdDev=VarCorr(restmp)
  }

  nparam=length(resestim)
  if(family$family!="gaussian")
    AIC=2*nparam-2*loglik
  else
    AIC=2*(nparam+1)-2*loglik

#--- Estimation for Model0 with only local variables
  if(modelType=="GLM")
    {
    res0=glm(as.formula(model0),data=newdata,family=family)
    }
  if(modelType=="LMM")
    {
    res0=lmer(as.formula(model0),data=newdata,REML=F)
    }
  if(modelType=="GLMM")
    {
    res0=glmer(as.formula(model0),data=newdata,family=family)
    }
  loglik0=as.vector(logLik(res0))
  AIC0=as.vector(AIC(res0))

#---- p.value computing for model with no landsacpe----
  if(family$family!="gaussian")
    pval0=1-pchisq(2*(loglik-loglik0),2*length(landraster))
  else
    pval0=1-pchisq(2*(loglik-loglik0),2*length(landraster))
  if(modelType=="GLM"& family$family=="gaussian")
    nparam=length(resestim)+1
  if(modelType=="GLM" & family$family !="gaussian")
    nparam=length(resestim)
  if(modelType=="LMM" || modelType=="GLMM")
    nparam=length(resestim)+nrow(as.data.frame(rand.StdDev))

#---warning if wd is too big
  if(sum(paramSIF<(3*wd)))
  {
    warning("It is recommended that wd is three times smaller than the estimated SIF mean distance. \nA new estimation with smaller wd should be more appropriate (see argument wd in siland).\n")
  }

#---warning if maxD is too small
  if(3*max(paramSIF)>maxD)
  {
    warning("It is recommended that maxD is three times greater than the estimated SIF mean distance. \nA new estimation with greater maxD should be more appropriate (see argument maxD in siland).\n")
  }

#--- return
  ressiland=list(coefficients=resestim,paramSIF=paramSIF, formula=model,landcontri=landcontri,loglik=loglik,loglik0=loglik0,result=restmp, fitted=fit,
                 sif=sif,resoptim=resoptim,result=restmp,AIC=AIC,AIC0=AIC0, nparam=nparam,pval0=pval0,family=family,
                 sd.error=sd.error,modelType=modelType,rand.StdDev=rand.StdDev,err=err,newdata=newdata,border=border,wd=wd,maxD=maxD)
  attr(ressiland,"class") <- "siland"
  return(ressiland)
}



