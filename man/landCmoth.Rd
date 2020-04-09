\name{landCmoth}
\alias{landCmoth}
\docType{data}
\title{
Landscape of dataCmoth observations
}
\description{
This data set contains description of landscape associated to \command{dataCmoth} data. 3 Landscape variables are considered conventional orchards, organic orchards and vignards. Anonymized data issued from Ricci et al. (2011) study.

landCmoth is a sf object and gives the positions for three landscape variables: conv, org and vine.

}
\usage{data("landCmoth")}
\format{
  A sf object with 11157 observations of 4 variables :
  
  \tabular{rlll}{
    [,1] \tab\code{conv}   \tab numeric \tab presence/absence of conventional orchard (0/1) \cr
    [,2] \tab \code{org} \tab numeric \tab presence/absence of organic orchard (0/1)\cr
    [,3] \tab\code{vine}    \tab numeric \tab presence/absence of vignards (0/1)\cr
    [,4] \tab \code{geometry}   \tab sfc_MULTIPOLYGON \tab location of polygons (anonymized)
  }
}
\references{
Ricci B., Franck P., Bouvier J.-C., Casado D. and Lavigne C. (2011). Effects of hedgerow characteristics on intra-orchard distribution of larval codling moth. Agriculture, Ecosystems & Environment. 140. 395-400. 10.1016/j.agee.2011.01.001. 
}



\keyword{datasets}
