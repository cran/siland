\name{landtoraster}
\alias{landtoraster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transformation of an sf object into raster
}
\description{
Function transforming an object of class sf into raster (points grid) with \command{wd}, the pixel size of the raster. \command{landname} gives the variable names for which raster points have to be computed.
}
\usage{
landtoraster(landgis, landname, wd)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{landgis}{
an object of class sf
}
  \item{landname}{
  vector of names of landscape variable for which rasters are computed.
}
  \item{wd}{
numeric, pixels size of raster
}


}


\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
a list of dataframes. The components of the list correspond to variables specifed in argument landname. Each dataframe gives the X and Y locations of pixels in raster.
}


\author{
O. Martin
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
\donttest{
data(dataSiland)
data(landSiland)
head(dataSiland)
head(landSiland)
u=landtoraster(landSiland,c("L1","L2"),wd=20)
names(u)
du1=as.data.frame(raster::rasterToPoints(u[[1]]))
du2=as.data.frame(raster::rasterToPoints(u[[2]]))
plot(du1[,1:2],type="n")
sel1=du1[,3]==1
sel2=du2[,3]==1
points(du1[sel1,c(1,2)],pch=".",col=1)
points(du2[sel2,c(1,2)],pch=".",col=2)


}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
