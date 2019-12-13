## ----echo = FALSE, message = FALSE--------------------------------------------
knitr::opts_chunk$set(collapse=T,
                      cache=T,
                      eval = F)

#knitr::opts_chunk$set(eval = FALSE)
# options(width = 120, max.print = 100)

## ----message=F,warning=F, results='hide',eval=F-------------------------------
#  landdata=st_read(dsn = "FILE",layer="NAME")

## -----------------------------------------------------------------------------
#  library(siland)
#  data(dataSiland)
#  data(landSiland)

## -----------------------------------------------------------------------------
#  str(dataSiland)
#  str(landSiland)

## ----fig.align='center',fig.width=7-------------------------------------------
#  library(ggplot2)
#  library(sf)
#  L1pol=st_geometry(landSiland[landSiland$L1==1,])#extract an sf object with only polygons of type L1
#  L2pol=st_geometry(landSiland[landSiland$L2==1,])#extract an sf object with only polygons of type L2
#  ggplot(landSiland)+
#    geom_sf(colour="grey",fill="white")+
#    geom_sf(data=L1pol,fill="red")+
#    geom_sf(data=L2pol,fill="blue")+
#    geom_point(data=dataSiland, aes(X,Y),col="green")

## -----------------------------------------------------------------------------
#  resB1=Bsiland(obs~x1+L1+L2,land=landSiland,data=dataSiland)
#  resB1

## -----------------------------------------------------------------------------
#  summary(resB1)

## -----------------------------------------------------------------------------
#  resB1$parambuffer

## ----fig.align='center',fig.width=7-------------------------------------------
#  plotBsiland.land(x=resB1,land=landSiland,data=dataSiland)
#  plotBsiland.land(x=resB1,land=landSiland,data=dataSiland,var=2)

## ----fig.align='center',fig.width=7-------------------------------------------
#  resB2=Bsiland(obs~x1+L1+L2,land=landSiland,data=dataSiland,family=gaussian,border=T)
#  resB2$parambuffer
#  plotBsiland.land(x=resB2,land=landSiland,data=dataSiland)
#  plotBsiland.land(x=resB2,land=landSiland,data=dataSiland,var=2)

## ----fig.align='center',fig.width=7-------------------------------------------
#  Bsiland.lik(resB1,land= landSiland, data=dataSiland,varnames=c("L1","L2"))

## -----------------------------------------------------------------------------
#  resF1=Fsiland(obs~x1+L1+L2,land=landSiland,data=dataSiland)
#  resF1
#  summary(resF1)

## ----fig.align='center',fig.width=7-------------------------------------------
#  plotFsiland.sif(resF1)

## ----fig.align='center',fig.width=7-------------------------------------------
#  plotFsiland.land(x=resF1,land=landSiland,data=dataSiland)

## ----fig.align='center',fig.width=7-------------------------------------------
#  plotFsiland.land(x=resF1,land=landSiland,data=dataSiland,var=2)

## ----fig.align='center',fig.width=7-------------------------------------------
#  par(mfrow=c(2,1))
#  resB1$parambuffer
#  resF1$paramSIF
#  plot(resB1$buffer[,1],resF1$landcontri[,1],xlab="Buffer contribution",ylab="FIS contribution", main="Variable L1")
#  abline(0,1)
#  plot(resB1$buffer[,2],resF1$landcontri[,2],xlab="Buffer contribution",ylab="FIS contribution", main="Variable L2")
#  abline(0,1)

## -----------------------------------------------------------------------------
#  resB1$AIC
#  resF1$AIC

## -----------------------------------------------------------------------------
#  resB3=Bsiland(obs~x1+L1+L2+(1|Id),land=landSiland,data=dataSiland)
#  summary(resB3)
#  resF3=Fsiland(obs~x1+L1+L2+(1|Id),land=landSiland,data=dataSiland)
#  summary(resF3)

## -----------------------------------------------------------------------------
#  #Model with main and interaction effect
#  resF4=Fsiland(obs~x1*L1+L2,land=landSiland,data=dataSiland)
#  #Model with only interaction effect
#  resF5=Fsiland(obs~x1:L1+L2,land=landSiland,data=dataSiland)
#  

## -----------------------------------------------------------------------------
#  landSilandY1=landSiland
#  landSilandY2=landSiland
#  #landSilandY is a list with the landscape for each year
#  landSilandY=list(landSilandY1,landSilandY2)
#  dataSilandY1=dataSiland
#  dataSilandY2=dataSiland
#  dataSilandY1$year=factor("2018")
#  dataSilandY2$year=factor("2019")
#  head(dataSilandY1)
#  head(dataSilandY2)
#  dataSilandY=list(dataSilandY1,dataSilandY2)
#  resY=Bsiland(obs~year+x1+L1+L2, land = landSilandY,data=dataSilandY)
#  resY
#  summary(resY)

## -----------------------------------------------------------------------------
#  summary(resB1$result)
#  BIC(resB1$result)
#  fitted(resF1$result)[1:10]
#  residuals(resF1$result)[1:10]
#  class(resB1$result)
#  class(resF1$result)
#  

