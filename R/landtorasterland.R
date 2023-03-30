landtorasterland<-function(landgis,landname,wd,data=NULL)
{
  if(!is.null(data))
    border=T
  else
    border=F

  if(border==F)
  {
    lobs=list(NULL)
    extentLand=st_bbox(landgis)
    for (i in 1:length(landname))
    {

    #r=raster(landgis,nrow=round((extentLand["ymax"]-extentLand["ymin"])/wd), ncol=round((extentLand["xmax"]-extentLand["xmin"])/wd),ext=extent(landgis))
    r=raster(nrow=round((extentLand["ymax"]-extentLand["ymin"])/wd), ncol=round((extentLand["xmax"]-extentLand["xmin"])/wd),ext=extent(landgis))
    raster::crs(r)<-st_crs(landgis)
    rland=fasterize(landgis,r,field=landname[i],fun='max')
    rlandpos=as.data.frame(rasterToPoints(rland))
    #only pixels different from zero are kept
    rlandpos=rlandpos[rlandpos[,3]!=0,]
    colnames(rlandpos)=c("X","Y",landname[i])
    lobs[[i]]=rlandpos
    }
   resraster=lobs
  }



  names(resraster)=landname
  return(landtable=resraster)

}
