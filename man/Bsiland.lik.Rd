\name{Bsiland.lik}
\alias{Bsiland.lik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
-(Loglikelihood) as a function of buffer sizes.
}
\description{
Function investigating eventual optimization problems during numerical optimization of function Bsiland by plotting the -(loglikelihhod) against buffer sizes of each landscape variable.


}
\usage{
Bsiland.lik(res, land,data, varnames = NULL, seqd = seq(2, 2000, length = 10))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
an object resulting from \command{Bsiland}
}

  \item{land}{
the object of class sf describing the landscape variable, i.e. the \command{land} argument in the call to the Bsiland function that computed \command{res}.
}

  \item{data}{
  the dataframe describing response variable and the local variables, i.e. the  \command{data} argument in the call to the Bsiland function that computed \command{res}.
}
  \item{varnames}{
a vector containing the names (characters) of studied landscape variables. If varnames is \command{NULL} (by default), all landscape variables of the \command{res} model are considered.
}
  \item{seqd}{
a vector of buffer sizes (positive values) for which the -loglikelihood is computed.
}
}
\details{
As with all numerical maximisation procedures, optimization problems may arise. The function \command{Bsiland.lik} allows to point out possible problems of optimization.

\command{Bsiland.lik} plots the -Loglikelihood against buffers sizes. During the Bsiland procedure, the estimation of buffer sizes is made by maximazing the likelihood i.e. by minimizing the -Loglikelihood. The orange horizontal line indicates the minimal value of -Loglikelihood obtained during the estimation (\command{res} result).
The other continous curves represent the -loglikehood as a function of buffer sizes for each studied landscape variable (considering the other parameters of the model at their estimated value in \command{res}). The dotted lines indicate the values of buffer size estimated (\command{res} result).

When the minization correctly occurs, the minimal values of the -loglikelihoods for each landscape variable are equal to the value of the estimated minimal -Loglikelihood.  This means that the value of each continuous curves is minimal at its intersection with the orange horizontal line. This also means that dotted lines intersect their associated continuous lines at their minimum.

If it is not the case, the minimizing procedure has failed and it is necessary to proceed again with an estimation with different initialisation values. This is possible with the argument \command{init} in function  \command{Bsiland}.

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

a ggplot of the -(loglikelihhod) against buffer sizes of each landscape variables.
}

\author{
O. Martin and F. Carpentier
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
\donttest{
data(dataSiland)
data(landSiland)
data(resB1)
#resB1 has been pre-computed and is the result from the following command
#resB1=Bsiland(obs~x1+L1+L2,land=landSiland,data=dataSiland)

Bsiland.lik(resB1,dataSiland,land=landSiland,varnames=c("L1","L2"),seqd=seq(5,50,length=10))

# Illustration of failed estimation caused by wrong initial values.
# In a such case, the estimation should be done again ,
# by  calling Bsiland with init=c(250,250) for instance.

#resB_fail=Bsiland(obs~x1+L1+L2,land=landSiland,data=dataSiland,init=c(2000,2000))
#Bsiland.lik(resB_fail,dataSiland,land=landSiland,varnames=c("L1","L2"),seqd=seq(5,3000,length=20))

}
}



