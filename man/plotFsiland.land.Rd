\name{plotFsiland.land}
\alias{plotFsiland.land}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Map of Fsiland result
}
\description{
Function creating a map of Fsiland result, i.e. estimatied spatial influence of each landscape variable or for the cumulative effect of all variables.
}
\usage{
plotFsiland.land(x, land, data, var = 0, lw = 100, xlim=NULL, ylim=NULL,plot=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object resulting from  \command{Fsiland}
}
  \item{land}{
the object of class sf describing the landscape variable, i.e. the \command{land} argument in the call to the \command{Fsiland} function that computed \command{x}
}
  \item{data}{
the dataframe describing response variable and the local variables, i.e. the  \command{data} argument in the call to the Fsiland function that computed \command{x}.
}
  \item{var}{
 a vector containing the numbers (numeric) of the mapped landscape variables. If \command{var=0} (by default), the cumulative effect of all landscape variables is represented.
}
  \item{lw}{
the number of pixels on x-axis and y-axis for the graphical representation. A too high value can lead to memory allocation problems. By default lw=100.
}
\item{xlim}{
a length-2 numeric vector, lower and upper limits of x-axis.
}
\item{ylim}{
a length-2 numeric vector, lower and upper limits of y-axis.
}

\item{plot}{
a logical (TRUE by default). If TRUE, the result is a graphic. If FALSE, the result is a dataframe of the	cumulative spatial influence of the landscape variable(s) var.
}

}

\value{
an object of class ggplot
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Fsiland
}
\examples{
\donttest{
data(dataSiland)
data(landSiland)
resF=Fsiland(obs~x1+L1+L2,land=landSiland,data=dataSiland,sif="exponential")
plotFsiland.land(x=resF,land=landSiland,data=dataSiland)
plotFsiland.land(x=resF,land=landSiland,data=dataSiland,var=1,lw=20)
plotFsiland.land(x=resF,land=landSiland,data=dataSiland,var=2,lw=20)


}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
