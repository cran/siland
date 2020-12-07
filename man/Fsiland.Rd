\name{Fsiland}
\alias{Fsiland}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimation of landscape influence using Fsiland method (Spatial Influence Function)
}
\description{
Function estimating landscape effect (and eventual local effects) using Fsiland method, i.e. estimating simultaneously : the SIF parameter (i.e. mean distance of SIF) and the effect of each landscape variable (and the effect of each local variable).

}


\usage{
Fsiland(formula,land,data,family ="gaussian",sif="exponential", init = 100,
 border=F,wd=30)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
a symbolic description (see lm() or glm()) of the model to be fitted. The details of model specification are given under 'Details'.
}
  \item{land}{
an object of class sf containing the landscape variables.
}
  \item{data}{
a dataframe containing the response variable and the local variables.
}

\item{family}{
the distribution of response variable. \command{family} can be "gaussian", "poisson" or "binomial" and the associated link function are identity, log and logit respectively.
}
\item{sif}{
the family of the Spatial Influence Function. \command{sif} can be "exponential", "gaussian" or "uniform".
}
  \item{init}{
  a vector indicating the starting values for SIF parameters in the estimation procedure. By default, starting values of SIF parameters of all landscape variable are equal to 100.

}
  \item{border}{
  a logical indicating wether the spatial influence of the polygon where observations are located is taken into account.
If \command{border=FALSE}, all pixels of a landscape variable have a spatial influence whether or not they belong to the polygon where the observation is located. If \command{border=TRUE}, only pixels outside the polygon of the observation, i.e. from the border of the polygon have an influence on this observation.

}

\item{wd}{
a numeric, the size of pixels.
}

%%  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
%%}
}


\details{
The Fsiland method is based on the model:
\deqn{ E(Y_i)= \mu + \sum_l \alpha_l x_{l,i}+ \sum_k \beta_k (\sum_r f_{\delta_k}(d_{r,i})z^k_r) }
with
\itemize{
\item Y_i, response variable,
\item x_{l,i}, local variable l,
\item r, the discritzation of study site in pixels r
\item z^k_r, the value of the landscape variable k at the pixel r. If the landscape variable k is a presence/absence variable, z^k_r is equal to one or zero.
\item \eqn{\mu}, the intercept
\item \eqn{\alpha_l} and \eqn{\beta_k}, the effects of local and landscape variables, respectively.
\item \eqn{f_{\delta_k}},the SIF associated with the landscape variable k with parameter \eqn{\delta_k} (the mean distance of the SIF, see detail further)
\item d_{r,i} the distance between the center of pixel r and the observation at site i.
}

All parameters, \eqn{\mu}, \eqn{ {\alpha_1,\dots,\alpha_L}}, \eqn{ {\beta_1,\dots,\beta_K}}  but also \eqn{ {\delta_1,\dots,\delta_K}} SIF parameters of the landscape variables are simultaneously estimated by likelihood maximization.

Models for \var{Fsiland} have the form \command{response ~ terms}  where \command{response} is the columns name of the response variable in \command{data} and \command{terms} is a series of terms which specifies the names of landscape and local explanatory variables. The explanatory variables described in \command{data} are considered as local in the model, those described in the sf object \command{land} are considered as landscape variables.

For local variables, random effects can be considered using the syntax \command{(1|x)} similar to package lme4 (see lmer() function in package lme4). Interaction terms are not yet implemented in \command{Fsiland} contrary to \command{Bsiland}.

The explanatory variables are added using the symbol \command{"+"}. Interaction terms can be considered using the usual symbols  \command{"*"} or  \command{":"}. Notice that only interactions between local x local and local x landscape variables are considered.

Two families of SIF are currently implemented in the siland package, exponential and Gaussian families defined as  \eqn{f_\delta(d)=2/(\pi\delta^2)exp(-2d/\delta)} and  \eqn{f\delta(d)=1/(2\delta \sqrt \pi)exp(-d\pi/(2\delta))^2}, respectively. By definition,  \eqn{\delta} represents the mean distance of these SIF.

The computation of the spatial influence of landscape variables is based on an approximation through the description of landscape variables distribution along a regular grid (modelisation of spatial influence of each pixel of landscape variable). The choice of \command{wd}, the size of pixels can have an impact on the estimation. The smaller the pixels, the more accurate the approximation, but also the longer the computation time. In our experience, we recommend using a pixel size at most three times smaller than the smallest estimated mean distance among the landscape variables SIF.

}


\value{
Fsiland returns an object of type Fsiland, a list :

\item{coefficients}{a vector of the estimated coefficients}
\item{paramSIF}{a vector of the estimated parameters of SIF}
\item{formula}{an object of class formula that indicates the model}
\item{landcontri}{a dataframe of estimated cumulative influence of each landscape variable (in columns) on each observation (in rows).}
\item{loglik}{the estimated maximum loglikelihood}
\item{loglik0}{the estimated maximum loglikelihood of the local model (no landscape variable)}
\item{result}{a lm/glm/lmer object that corresponds to the model estimate knowing the estimated SIF parameters of landscape variables. }
\item{fitted}{the fitted values}
\item{sif}{the family of the SIF}
\item{resoptim}{an object of class optim or optimize giving informations about the optimization procedure see optim() or optimize() for further details.}
\item{AIC}{the akaike information criterion of the model}
\item{AIC0}{the akaike information criterion of the local model (no landscape variable)}
\item{nparam}{the number of parameters }
\item{pval0}{the p.value of the test of the landscape effect (the global effect of all spatial variables). Issued from the likelihood ratio test of the (complete) model vs the local model (complete model without landscape variables).}
\item{family}{the family distribution for the model}
\item{sd.error}{the estimated standard error for gaussian family, NA in other case}
\item{model.Type}{the type of local model: GLM for generalised model, LMM for linear mixed model or GLMM for generalised linear mixed model}
\item{rand.StdDev}{the estimated standard deviation of random effects for LMM or GLMM}
\item{err}{the estimated residuals}
\item{border}{a logical indicating the value used for estimation}
\item{wd}{a number indicating the size of pixels}
}










\author{
Carpentier, F. and Martin, O.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\references{
Carpentier, F., & Martin, O. (2019). SILand: an R package for estimating the spatial influence of landscape. bioRxiv, 692566.

   }

\examples{
\donttest{
data(dataSiland)
data(landSiland)
resF=Fsiland(obs~x1+L1+L2,land=landSiland,data=dataSiland,sif="exponential")
resF
resF$AIC
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }%
