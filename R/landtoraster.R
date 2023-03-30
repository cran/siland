landtoraster<-function(landgis,landname,wd)
{
  #landgis : sf object
  #landname : vector of character

  #if(!is.null(data))
  #  border=T
  #else
  #  border=F

  landraster=list(NULL)

  extentLand=st_bbox(landgis)
    for (i in 1:length(landname))
    {
      r=raster(nrow=round((extentLand["ymax"]-extentLand["ymin"])/wd), ncol=round((extentLand["xmax"]-extentLand["xmin"])/wd),ext=extent(landgis))
      raster::crs(r)<-st_crs(landgis)
      rland=fasterize(landgis,r,field=landname[i],fun='max')
      rland[is.na(rland)]=0
      #rlandpos=as.data.frame(rasterToPoints(rland))
      #only pixels different from zero are kept
      #rlandpos=rlandpos[rlandpos[,3]!=0,]
      #colnames(rlandpos)=c("X","Y",landname[i])
      landraster[[i]]=rland
    }

  names(landraster)=landname
  return(landraster=landraster)


}
