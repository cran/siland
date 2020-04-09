\name{landSiland}
\alias{landSiland}
\docType{data}
\title{
Simulated landscape.
}
\description{
Simulated landscape associated to dataSiland, containing two landscape variables, named L1 and L2.
}
\usage{data("landSiland")}
\format{
  A \code{sf} object with 4884 observations on 3 variables.


  \tabular{rlll}{
    [,1] \tab  L1     \tab numeric \tab presence/absence of L1 (0/1) \cr
    [,2] \tab L2  \tab numeric \tab presence/absence of L2 (0/1)\cr
    [,3] \tab geometry   \tab sfc_MULTIPOLYGON \tab polygon location\cr

  }
  }




\examples{
%\donttest{
data(landSiland)
plot(landSiland$geometry)
plot(landSiland[landSiland$L1==1,]$geometry,col=2,add=TRUE)
plot(landSiland[landSiland$L2==1,]$geometry,col=3,add=TRUE)



%}
}
\keyword{datasets}
