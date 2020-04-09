\name{AIC.Fsiland}
\alias{AIC.Fsiland}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Akaike's An Information Criterion for siland package
}
\description{
Function calculating thue usual Akaike's 'An Information Criterion' for an object of class Fsiland, according to the formula -2*loglikelihood+k*npar, with npar the number of parameters of the model and k = 2.
}

\usage{
\method{AIC}{Fsiland}(object,...,k=2)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ an object of class Fsiland}
  \item{\dots}{Dots are not take into account in this version}
  \item{k}{not used}
}

\value{ AIC value
}
\references{
Sakamoto, Y., Ishiguro, M., and Kitagawa G. (1986). Akaike Information Criterion Statistics. D. Reidel Publishing Company.

}
\author{
O. Martin
}



