\name{plotBsiland.land}
\alias{plotBsiland.land}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Map of Bsiland result
}
\description{
Function creating a map of Bsiland result, i.e. buffers estimations for each landscape variable. 
}
\usage{
plotBsiland.land(x, land, data, var = 1,landviz=F )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object resulting from  \command{Bsiland}
}
  \item{land}{
the object of class sf describing the landscape variable, i.e. the \command{land} argument in the call to the \command{Bsiland} function that computed \command{x}
}
  \item{data}{
the dataframe describing response variable and the local variables, i.e. the  \command{data} argument in the call to the Bsiland function that computed \command{x}.
}
  \item{var}{
  a vector containing the numbers (numeric) of the mapped landscape variables. 
}
  \item{landviz}{
If landviz=T (by default), polygons where the variable \command{var} is observed are colored in gray.
}


}

\value{
a ggplot of class of the landscape effect estimated in the \command{Bsiland} object x i.e. a map of buffers. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Bsiland
}
\examples{
\donttest{
data(dataSiland)
data(landSiland)
resB=Bsiland(obs~x1+L1+L2,land=landSiland,data=dataSiland)
plotBsiland.land(x=resB,land=landSiland,data=dataSiland)
plotBsiland.land(x=resB,land=landSiland,data=dataSiland,var=2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
