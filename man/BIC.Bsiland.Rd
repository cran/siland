\name{BIC.Bsiland}
\alias{BIC.Bsiland}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian Information criterion
}
\description{
Function computing the Bayesian Information Criterion for an object of class Bsiland, according to the formula -2*loglikelihood+k*npar, with npar the number of parameters of the model and k = log(n) (n being the number of observations).
}

\usage{
\method{BIC}{Bsiland}(object,...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ an object of class Fsiland}
  \item{\dots}{Dots are not take into account in this version}
}

\value{
BIC value
}

\author{
O. Martin
}
