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
landtoraster(landgis, landname, wd, data = NULL)
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
  \item{data}{
a dataframe with X and Y columns, locations.
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
u1=landtoraster(landSiland,c("L1","L2"),wd=20)
names(u1)
plot(u1[[1]][,c("X","Y")],pch=".")
u2=landtoraster(landSiland,c("L1","L2"),wd=20,data=dataSiland)
points(u2[[1]][,c("X","Y")],pch=".",col=2)
points(dataSiland[,c("X","Y")],pch=16,cex=0.6,col=3)


}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
