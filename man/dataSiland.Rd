\name{dataSiland}
\alias{dataSiland}
\docType{data}
\title{
Simulated data
}
\description{
Simulated data of response variable and local variables.
}

\usage{data("dataSiland")}
\format{
  A data frame with 100 observations on 5 variables.

\tabular{rlll}{
    [,1] \tab X   \tab numeric \tab location on X-axis \cr
    [,2] \tab Y \tab numeric \tab location on Y-axis\cr
    [,3] \tab x1    \tab numeric \tab a continous local variable\cr
    [,4] \tab Id   \tab numeric \tab identifier\cr
    [,5] \tab obs    \tab numeric \tab response variable
  }
  }

\examples{
\donttest{
data(dataSiland)
plot(dataSiland[,c("X","Y")])
}
}
\keyword{datasets}
