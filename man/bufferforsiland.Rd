\name{bufferforsiland}
\alias{bufferforsiland}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Percentage of landscape variable in buffers.
}
\description{
Function computing the percentages of landscape variables in buffers of given sizes (i.e. radii). }

\usage{
bufferforsiland(d, sfGIS, loc.sf, landnames, border = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
a vector of the distances of the buffer sizes. The length of \command{d} has to be equal to that of \command{landnames}.
}
  \item{sfGIS}{
an object of class sf containing the landscape variables (land uses). Landscape variables are equal to 1 (presence) or 0 (absence).
}
  \item{loc.sf}{
an object of class sf or a dataframe containing locations of buffers centers. For dataframe, locations have to be given in columns "X" and "Y".
}
  \item{landnames}{
a vector of characters of the names of the landscape variables
}
  \item{border}{
a logical (\command{FALSE} by default). If \command{border=FALSE}  buffers are computed from the locations points given in \command{loc.sf} If \command{border=TRUE} buffers  are computed from the border of the polygon that contain the points in \command{loc.sf}.


}
}

\value{
a dataframe containing the percentages of the landscape variables named \command{landnames} (in columns) in buffers of sizes described in \command{d} and centered on locations described in \command{loc.sf} (in rows).


}
\author{
Martin O.
}


\examples{
\donttest{
data(dataSiland)
data(landSiland)
dataSiland[1:10,c("X","Y")]
landSiland$L1
landSiland$L2

res=bufferforsiland(c(100,200),sfGIS=landSiland, loc.sf=dataSiland, landnames=c("L1","L2"))
res
}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
