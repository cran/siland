## ----echo = FALSE, message = FALSE--------------------------------------------
knitr::opts_chunk$set(collapse=T,cache=F,eval=T)
#
#knitr::opts_chunk$set(eval = FALSE)
# options(width = 120, max.print = 100)

## ----message=F,warning=F, results='hide',eval=F-------------------------------
#  landdata=st_read(dsn = "FILE",layer="NAME")

## -----------------------------------------------------------------------------
library(siland)
data(dataSiland)
data(landSiland)

## ----eval=T-------------------------------------------------------------------
data("likresB1","resB1","resB2","resB3","resF1","resF3","resF4","resF5.1","resF5.2","resY")

## -----------------------------------------------------------------------------
str(dataSiland)
str(landSiland)

## ----fig.align='center',fig.width=7,eval=T------------------------------------
library(ggplot2)
library(sf)
L1pol=st_geometry(landSiland[landSiland$L1==1,])#extract an sf object with only polygons of type L1 
L2pol=st_geometry(landSiland[landSiland$L2==1,])#extract an sf object with only polygons of type L2 
ggplot(landSiland)+
  geom_sf(colour="grey",fill="white")+
  geom_sf(data=L1pol,fill="red")+
  geom_sf(data=L2pol,fill="blue")+
  geom_point(data=dataSiland, aes(X,Y),col="green")


## ----eval=F-------------------------------------------------------------------
#  resB1=Bsiland(obs~x1+L1+L2,land=landSiland,data=dataSiland)

## ----eval=T-------------------------------------------------------------------
resB1

## ----eval=T-------------------------------------------------------------------
summary(resB1)

## ----eval=T-------------------------------------------------------------------
resB1$parambuffer

## ----fig.align='center',fig.width=7,eval=T------------------------------------
plotBsiland.land(x=resB1,land=landSiland,data=dataSiland)

plotBsiland.land(x=resB1,land=landSiland,data=dataSiland,var=2)

## ----fig.align='center',fig.width=7,eval=F------------------------------------
#  resB2=Bsiland(obs~x1+L1+L2,land=landSiland,data=dataSiland,family=gaussian,border=T)

## ----fig.align=T,fig.width=7,eval=F-------------------------------------------
#  resB2$parambuffer
#  plotBsiland.land(x=resB2,land=landSiland,data=dataSiland)
#  plotBsiland.land(x=resB2,land=landSiland,data=dataSiland,var=2)

## ----fig.align='center',fig.width=7,eval=F------------------------------------
#  likresB1=Bsiland.lik(resB1,land= landSiland,data=dataSiland,varnames=c("L1","L2"))

## ----fig.align='center',fig.width=7,eval=T------------------------------------
likresB1

## ----eval=F-------------------------------------------------------------------
#  resF1=Fsiland(obs~x1+L1+L2,land=landSiland,data=dataSiland)

## ----eval=T-------------------------------------------------------------------
resF1
summary(resF1)

## ----fig.align='center',fig.width=7,eval=T------------------------------------
plotFsiland.sif(resF1)

## ----fig.align='center',fig.width=7,eval=F------------------------------------
#  plotFsiland.land(x=resF1,land=landSiland,data=dataSiland)

## ----fig.align='center',fig.width=7,eval=F------------------------------------
#  plotFsiland.land(x=resF1,land=landSiland,data=dataSiland,var=2)

## ----fig.align='center',fig.width=7,eval=T------------------------------------

resB1$parambuffer
resF1$paramSIF
plot(resB1$buffer[,1],resF1$landcontri[,1],xlab="Buffer contribution",ylab="FIS contribution", main="Variable L1")
abline(0,1)
plot(resB1$buffer[,2],resF1$landcontri[,2],xlab="Buffer contribution",ylab="FIS contribution", main="Variable L2")
abline(0,1)

## -----------------------------------------------------------------------------
resB1$AIC
resF1$AIC

## ----eval=F-------------------------------------------------------------------
#  resB3=Bsiland(obs~x1+L1+L2+(1|Id),land=landSiland,data=dataSiland)

## ----eval=T-------------------------------------------------------------------
summary(resB3)

## ----eval=F-------------------------------------------------------------------
#  resF3=Fsiland(obs~x1+L1+L2+(1|Id),land=landSiland,data=dataSiland)

## ----eval=T-------------------------------------------------------------------
summary(resF3)

## ----eval=F-------------------------------------------------------------------
#  #Model with main and interaction effect
#  resF4=Fsiland(obs~x1*L1+L2,land=landSiland,data=dataSiland)

## ----eval=T-------------------------------------------------------------------
resF4

## ----eval=F-------------------------------------------------------------------
#  #Model with only interaction effect
#  resF5.1=Fsiland(obs~x1:L1+L2,land=landSiland,data=dataSiland)

## ----eval=T-------------------------------------------------------------------
resF5.1

## ----eval=F-------------------------------------------------------------------
#  resF5.2=Fsiland(obs~x1:L1+L2,land=landSiland,data=dataSiland,wd=15)

## ----eval=T-------------------------------------------------------------------
resF5.2

## ----eval=F-------------------------------------------------------------------
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

## ----eval=T-------------------------------------------------------------------
resY
summary(resY)

## ----eval=T-------------------------------------------------------------------
summary(resB1$result)
BIC(resB1$result)
fitted(resF1$result)[1:10]
residuals(resF1$result)[1:10]
class(resB1$result)
class(resF1$result)


