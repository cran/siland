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


## ----eval=T-------------------------------------------------------------------
res=siland(obs~x1+L1+L2,land=landSiland,data=dataSiland,wd=30)

## ----eval=T-------------------------------------------------------------------
res


## ----eval=T-------------------------------------------------------------------
summary(res)

## ----fig.align='center',fig.width=7,eval=T------------------------------------
plotsiland.sif(res)

## ----fig.align='center',fig.width=7,eval=F------------------------------------
#  plotsiland.land(x=res,land=landSiland,data=dataSiland)

## ----fig.align='center',fig.width=7,eval=T------------------------------------
plotsiland.land(x=res,land=landSiland,data=dataSiland,var=2)

## ----fig.align='center',fig.width=7,eval=T------------------------------------
likres=siland.lik(res,land= landSiland,data=dataSiland,varnames=c("L1","L2"))

## ----fig.align='center',fig.width=7,eval=T------------------------------------
likres

## ----eval=T-------------------------------------------------------------------
res2=siland(obs~x1+L1+L2+(1|Id),land=landSiland,data=dataSiland,wd=30)

## ----eval=T-------------------------------------------------------------------
summary(res2)

## ----eval=T-------------------------------------------------------------------
#Model with main and interaction effect
res3=siland(obs~x1*L1+L2,land=landSiland,data=dataSiland,wd=30)

## ----eval=T-------------------------------------------------------------------
res3

## ----eval=T-------------------------------------------------------------------
res3.1=siland(obs~x1*L1+L2,land=landSiland,data=dataSiland,wd=30)

## ----eval=T-------------------------------------------------------------------
res3.1

## ----eval=T-------------------------------------------------------------------
landSilandY1=landSiland
landSilandY2=landSiland
#landSilandY is a list with the landscape for each year
landSilandY=list(year1=landSilandY1,year2=landSilandY2)
dataSilandY1=dataSiland
dataSilandY2=dataSiland
dataSilandY1$year=factor("2018")
dataSilandY2$year=factor("2019")
head(dataSilandY1)
head(dataSilandY2)
dataSilandY=list(year1=dataSilandY1,year2=dataSilandY2)
#dataSilandY is a list with the observed data for each year
resY=siland(obs~year+x1+L1+L2, land = landSilandY,data=dataSilandY,wd=30)
resY

## ----eval=T-------------------------------------------------------------------
resY
summary(resY)

## ----eval=T-------------------------------------------------------------------
summary(res$result)
BIC(res$result)
fitted(res$result)[1:10]
residuals(res$result)[1:10]
class(res$result)
class(res$result)


