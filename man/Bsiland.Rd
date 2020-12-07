\name{Bsiland}
\alias{Bsiland}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimation of landscape influence using Bsiland method (Buffers)
}
\description{
Function estimating landscape effect (and eventual local effects) using Bsiland method, i.e. estimating simultaneously : the buffer size (radius) and the effect of each landscape variable (and the effect of each local variable).
}


\usage{
Bsiland(formula, land, data, family = "gaussian", init = 200, border = F)
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
  \item{init}{
a vector indicating the starting values for buffer sizes in the estimation procedure. By default, starting values of buffer sizes of all landscape variable are equal to 200.
}
  \item{border}{
a logical indicating wether buffers are computed from the observation locations ( \command{border=FALSE)} or from the border of the polygon where observations are located ( \command{border=TRUE)})}
}

\details{
The Bsiland method is based on the model:
\deqn{ E(Y_i)= \mu + \sum_l \alpha_l x_{l,i}+ \sum_k \beta_k p^k_{\delta_k,i}}
with
\itemize{
\item Y_i, response variable,
\item x_{l,i}, local variable l,
\item \eqn{p^k_{\delta_k,i}}, the percentage of the landscape variable k in a buffer of radius \eqn{\delta_k}, centered on site i
\item \eqn{\mu}, the intercept
\item \eqn{\alpha_l} and \eqn{\beta_k}, the effects of local and landscape variables, respectively.
}

All parameters, \eqn{\mu}, \eqn{ {\alpha_1,\dots,\alpha_L}}, \eqn{ {\beta_1,\dots,\beta_K}}  but also \eqn{ {\delta_1,\dots,\delta_K}} radii of the buffers of the landscape variables are simultaneously estimated by likelihood maximization.

Models for \var{Bsiland} have the form \command{response ~ terms}  where \command{response} is the columns name of the response variable in \command{data} and \command{terms} is a series of terms which specifies the names of landscape and local explanatory variables. The explanatory variables described in \command{data} are considered as local in the model, those described in the sf object \command{land} are considered as landscape variables.

For local variables, random effects can be considered using the syntax \command{(1|x)} similar to package lme4 (see lmer() function in package lme4).

The explanatory variables are added using the symbol \command{"+"}. Interaction terms can be considered using the usual symbols  \command{"*"} or  \command{":"}. Notice that only interactions between local x local and local x landscape variables are considered.

}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Bsiland returns an object of type Bsiland, a list :

\item{coefficients}{a vector of the estimated coefficients}
\item{parambuffer}{a vector of the estimated buffer radii}
\item{formula}{an object of class formula that indicates the model}
\item{buffer}{a dataframe indicating the percentages of landscape variables in buffer of estimated sizes centered on observations. The landscape variables are in columns, the observations in rows.}
\item{loglik}{the estimated maximum loglikelihood}
\item{loglik0}{the estimated maximum loglikelihood of the local model (no landscape variable)}
\item{fitted}{the fitted values}
\item{resoptim}{an object of class optim or optimize giving informations about the optimization procedure see \command{optim()} or \command{optimize()} for further details.}
\item{result}{a lm/glm/lmer type object that corresponds to the model estimate knowing the estimated buffer sizes of landscape variables}

\item{AIC}{the akaike information criterion of the model}
\item{AIC0}{the akaike information criterion of the local model (no landscape variable)}
\item{nparam}{the number of parameters}
\item{pval0}{the p.value of the test of the landscape effect (the global effect of all spatial variables). Issued from the likelihood ratio test of the (complete) model vs the local model (complete model without landscape variables).}


\item{family}{the family distribution of the model}
\item{sd.error}{the estimated standard error for gaussian family, NA in other case}
\item{model.Type}{the type of local model: GLM for generalised model, LMM for linear mixed model or GLMM for generalised linear mixed model}
\item{rand.StdDev}{the estimated standard deviations of the random effects for LMM or GLMM}
\item{err}{the estimated residuals}
\item{newdata}{a dataframe containing the local variables and the percentages of the landscape variables in buffers of estimated buffer sizes for each observation (in rows).}
\item{border}{a logical indicating the value used for estimation}

}


\author{
O. Martin and F. Carpentier
}

\references{
Carpentier, F., & Martin, O. (2019). SILand: an R package for estimating the spatial influence of landscape. bioRxiv, 692566.

   }

\examples{
\donttest{
data(dataSiland)
data(landSiland)
resB=Bsiland(obs~x1+L1+L2,land=landSiland,data=dataSiland,init = c(50))
resB
summary(resB)

}

}

%% ~Make other sections like Warning with \section{Warning }{....} ~



