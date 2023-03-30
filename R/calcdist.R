calcdist=function(data,landr,landgis,rmax=7000,border=F)
{
  #compute distance between observation location of data
  #and pixel source location of each  landscape variable and for each landscape in list land
  #IMPORTANT : data , landr and landgis are list with the same length
  #
  nT=length(data)
  if(nT==1)
    nvarland=length(landr[[1]])
  else
    nvarland=length(landr[[1]])
  landname=names(landr[[1]])

  #----------------------------------------------
  ##first step : search for pixel around each observed data point
  ## in a buffer less than rmax. For each landscape variable p, ony
  ## pixel with value equal to one are stored
  #-------------------------------------------------
  #case nT==1
 # print("step1")
  if(nT==1)
  {
    uu=list(NULL)
    #coordC is a list, and each element of the list
    #corresponds to the observed data point in data
    coordC=list(NULL)
    for(p in 1:nvarland)
    {
      uutmp=raster::extract(landr[[1]][[p]],data[[1]][,c("X","Y")],buffer=rmax,cellnumbers=T)
      #uutmp is a list. The length of the list is the number of rows for data[[1]]
      #uutmp gives the numero of cell around each obersved data in a buffer equal to rmax

      #if border True, for each field, we delete pixel into the field
      if(border[p]==T)
      {
        landgis_tmp=landgis[[1]][landname[p]]
        loc.sf=st_as_sf(as.data.frame(data[[1]]),coords = c("X","Y"))
        st_crs(loc.sf)<-st_crs(landgis_tmp)
        stinter=st_intersects(loc.sf,landgis_tmp)
        #geosel=unlist(lapply(stinter,function(x){if(length(x)==1) return(x) else return(-1000)} ))
        #if(sum(geosel==-1000)>0)
        #  stop("Some observations are located outside of the boudaries of GIS landscape")
        cell_into_field=lapply(stinter,function(x){
          if(length(x)>0)
            rr=unlist(raster::cellFromPolygon(landr[[1]][[p]],p=as(landgis_tmp[x,],"Spatial")))
          else
            rr=NULL
          return(rr)
        })

        #uutmp2=uutmp #uutmp2 initialisation
        #for(j in 1:length(uutmp))
        #{
          #print(j)
        #  allpixel=uutmp[[j]][,"cell"]
        #  pixelfield=cell_into_field[[j]]
        #  uutmp2[[j]]=uutmp[[j]][!c(allpixel%in%pixelfield),]
        #}

        uutmp2=mapply(function(ll1,ll2){
          allpixel=ll1[,"cell"]
          pixelfield=cell_into_field
          return(ll1[!c(allpixel%in%pixelfield),])},
        ll1=uutmp,ll2=cell_into_field)

      uutmp=uutmp2
      }#end if border==T

      #only value equal to 1 in raster are kept
      uu=lapply(uutmp,function(x){
        if(!is.logical(x)){return(x[x[,"value"]==1 & !is.na(x[,"value"]) ,])}
        else(return(NULL))
      })

      #extract coordinates of pixels for landscape variable p
      coordC[[p]]=lapply(uu,function(x) {
        res=NULL
        if(class(x)[1] =="matrix") {res=raster::xyFromCell(landr[[1]][[p]],cell=x[,"cell"])}
        if(is.vector(x)) {res=raster::xyFromCell(landr[[1]][[p]],cell=x["cell"])}
        return(res)
      })
    }#end for p in 1:nvarland
  }#end if nT==1

  if(nT>1)
  {
    coordC=list(NULL)
    for(t in 1:nT)
    {

      uutmp=list(NULL)
      uu=list(NULL)
      coordC[[t]]=list(NULL)
      for(p in 1:nvarland)
      {
        uutmp[[t]]=raster::extract(landr[[t]][[p]],data[[t]][,c("X","Y")],buffer=rmax,cellnumbers=T)
        if(border[p]==T)
        {
          landgis_tmp=landgis[[t]][landname[p]]
          loc.sf=st_as_sf(as.data.frame(data[[t]]),coords = c("X","Y"))
          st_crs(loc.sf)<-st_crs(landgis_tmp)
          stinter=st_intersects(loc.sf,landgis_tmp)
          #geosel=unlist(lapply(stinter,function(x){if(length(x)==1) return(x) else return(-1000)} ))
          #if(sum(geosel==-1000)>0)
          #  stop("Some observations are located outside of the boudaries of GIS landscape")
          cell_into_field=lapply(stinter,function(x){
            if(length(x)>0)
              rr=unlist(raster::cellFromPolygon(landr[[t]][[p]],p=as(landgis_tmp[x,],"Spatial")))
            else
              rr=NULL
            return(rr)
          })

          #uutmp2=uutmp[[t]]
          #for(j in 1:length(uutmp[[t]]))
          #{
          #  allpixel=uutmp[[t]][[j]][,"cell"]
          #  pixelfield=cell_into_field[[j]]
          #  uutmp2[[j]]=uutmp[[t]][[j]][!c(allpixel%in%pixelfield),]
          #}
          #uutmp[[t]]=uutmp2

          uutmp2=mapply(function(ll1,ll2){
            allpixel=ll1[,"cell"]
            pixelfield=cell_into_field
            return(ll1[!c(allpixel%in%pixelfield),])},
            ll1=uutmp[[t]],ll2=cell_into_field)

          uutmp[[t]]=uutmp2

        }#end for boder


        uu[[t]]=lapply(uutmp[[t]],function(x){
          res=NULL
          if(class(x)[1] =="matrix") {res=x[x[,"value"]==1 & !is.na(x[,"value"]) ,]}
          if(is.vector(x)){res=x[x["value"]==1 & !is.na(x["value"]) ]}
          return(res)}
        )

        coordC[[t]][[p]]=lapply(uu[[t]],function(x) {
          res=NULL
          if(class(x)[1] =="matrix"){res=raster::xyFromCell(landr[[t]][[p]],cell=x[,"cell"])}
          if(is.vector(x)){res=raster::xyFromCell(landr[[t]][[p]],cell=x["cell"])}
          return(res)}
        )
      }#end for p
    }#end for (t in 1:nT)
  }#end case nT>1

  #--------------------------------------------------------
  #second step : just add the coordinates of the observed point
  # to each group of point... this step is done to complte easily  distance
  # between the observed point and selected pixels
  #-------------------------------------------------------------
  #print("step2")
  if(nT==1){
    datawithpoint=list(NULL)
    for(p in 1:nvarland)
    {
      datawithpoint[[p]]=list(NULL)
      for(i in 1:nrow(data[[1]]))
        datawithpoint[[p]][[i]]=rbind(as.matrix(data[[1]][i,c("X","Y")]),coordC[[p]][[i]][,1:2])
    }}

  if(nT>1)
  {
    datawithpoint=list(NULL)
    for(t in 1: nT)
    {
      datawithpoint[[t]]=list(NULL)
      for(p in 1:nvarland)
      {
        datawithpoint[[t]][[p]]=list(NULL)
        for(i in 1:nrow(data[[t]]))
          datawithpoint[[t]][[p]][[i]]=rbind(as.matrix(data[[t]][i,c("X","Y")]),coordC[[t]][[p]][[i]][,1:2])
      }
    }
  }
 # print("step3")
  #---------------------------------------
  #thrid step : compute the distance between the selected points around
  #  the observed point and the oberved point. The distances are computed
  #with the fucntion rdist from package fiels
  #Distobs is a list and the length of Distobs is equal to nT
  #----------------------------------------------

  if(nT==1){
    Distobs=list(NULL)
    for(p in 1:nvarland)
      Distobs[[p]]=lapply(datawithpoint[[p]],function(x){
        tmp=NA
        if(nrow(x)>2)
          tmp=as.vector(fields::rdist(matrix(x[1,],ncol=2),x[-1,]))
        if(nrow(x)==2)
          tmp=as.vector(fields::rdist(matrix(x[1,],ncol=2),matrix(x[2,],ncol=2)))
        return(tmp)
      }
      )
    #names(Distobs)=paste("Dist",1:p,sep="")
    names(Distobs)=names(landr)
    Distobs=list(Distobs)
  }

  if(nT>1)
  {
    Distobs=list(NULL)
    for(t in 1:nT)
    {
      Distobs[[t]]=list(NULL)
      for(p in 1:nvarland)
        Distobs[[t]][[p]]=lapply(datawithpoint[[t]][[p]],function(x){
          tmp=NA
          if(nrow(x)>2)
            tmp=as.vector(fields::rdist(matrix(x[1,],ncol=2),x[-1,]))
          if(nrow(x)==2)
            tmp=as.vector(fields::rdist(matrix(x[1,],ncol=2),matrix(x[2,],ncol=2)))
          return(tmp)
        }
        )
      #names(Distobs[[t]])=paste("Dist",1:p,sep="")
      names(Distobs[[t]])=names(landr[[t]])
    }
    names(Distobs)=names(landr)
  }

  #Distobs is a list
  #if length(Distobs=1), nT=1
  #if length(Distobs=k), nT=k
  return(Distobs)
}
