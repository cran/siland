\name{Fsiland.quantile}
\alias{Fsiland.quantile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quantile of  spatial influence function (SIF)
}
\description{
function computing quantiles of SIF, i.e. the radius of disk on which the integrated influence of the SIF is equal to given proportions, \command{p}. Calculations are performed for the estimated SIF of each landscape variable in \command{x}.The \bold{area of medium influence and significant influence of a landscape variable}, that we defined as the disc containing 50\% and 95\% of the influence of the SIF (neglecting 50\% and 5\% of its broader effect) respectively can be computed using argument \command{ p = 0.5} and \command{ p = 0.95}, respectively.
}
\usage{
Fsiland.quantile(x, p = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class Fsiland resulting from function \command{Fsiland}
}
  \item{p}{
a vector of probabilities
}
}

\value{
A matrix of the radii of disks on wich the integrated influences of the SIF estimated for each landscape variable of \command{x} (in columns) are equal to each probability of the vector \command{p} (in rows).
}

\author{
O. Martin and F. Carpentier
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
\donttest{
data(dataSiland)
data(landSiland)
resF=Fsiland(obs~x1+L1+L2,land=landSiland,data=dataSiland,sif="exponential")
Fsiland.quantile(x=resF,p=c(0.5,0.95))
}

}

