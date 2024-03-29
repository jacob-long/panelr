% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wb_gee.R
\name{wbgee}
\alias{wbgee}
\title{Panel regression models fit with GEE}
\usage{
wbgee(
  formula,
  data,
  id = NULL,
  wave = NULL,
  model = "w-b",
  cor.str = c("ar1", "exchangeable", "unstructured"),
  detrend = FALSE,
  use.wave = FALSE,
  wave.factor = FALSE,
  min.waves = 2,
  family = gaussian,
  balance.correction = FALSE,
  dt.random = TRUE,
  dt.order = 1,
  weights = NULL,
  offset = NULL,
  interaction.style = c("double-demean", "demean", "raw"),
  scale = FALSE,
  scale.response = FALSE,
  n.sd = 1,
  calc.fit.stats = TRUE,
  ...
)
}
\arguments{
\item{formula}{Model formula. See details for crucial
info on \code{panelr}'s formula syntax.}

\item{data}{The data, either a \code{panel_data} object or \code{data.frame}.}

\item{id}{If \code{data} is not a \code{panel_data} object, then the name of the
individual id column as a string. Otherwise, leave as NULL, the default.}

\item{wave}{If \code{data} is not a \code{panel_data} object, then the name of the
panel wave column as a string. Otherwise, leave as NULL, the default.}

\item{model}{One of \code{"w-b"}, \code{"within"}, \code{"between"},
\code{"contextual"}. See details for more on these options.}

\item{cor.str}{Any correlation structure accepted by \code{\link[geepack:geeglm]{geepack::geeglm()}}.
Default is "ar1", most useful alternative is "exchangeable". "unstructured"
may cause problems due to its computational complexity.}

\item{detrend}{Adjust within-subject effects for trends in the predictors?
Default is FALSE, but some research suggests this is a better idea
(see Curran and Bauer (2011) reference).}

\item{use.wave}{Should the wave be included as a predictor? Default is
FALSE.}

\item{wave.factor}{Should the wave variable be treated as an unordered
factor instead of continuous? Default is FALSE.}

\item{min.waves}{What is the minimum number of waves an individual must
have participated in to be included in the analysis? Default is \code{2} and
any valid number is accepted. \code{"all"} is also acceptable if you want to
include only complete panelists.}

\item{family}{Use this to specify GLM link families. Default is \code{gaussian},
the linear model.}

\item{balance.correction}{Correct between-subject effects for unbalanced
panels following the procedure in Curran and Bauer (2011)? Default is
FALSE.}

\item{dt.random}{Should the detrending procedure be performed with a
random slope for each entity? Default is TRUE but for short panels
FALSE may be better, fitting a trend for all entities.}

\item{dt.order}{If detrending using \code{detrend}, what order polynomial
would you like to specify for the relationship between time and the
predictors? Default is 1, a linear model.}

\item{weights}{If using weights, either the name of the column in the data
that contains the weights or a vector of the weights.}

\item{offset}{this can be used to specify an \emph{a priori} known
    component to be included in the linear predictor during
    fitting. This should be \code{NULL} or a numeric vector of length
    equal to the number of cases.  One or more \code{\link{offset}}
    terms can be included in the formula instead or as well, and if more
    than one is specified their sum is used.  See \code{\link{model.offset}}.}

\item{interaction.style}{The best way to calculate interactions in within
models is in some dispute. The conventional way (\code{"demean"}) is to first
calculate the product of the variables involved in the interaction before
those variables have their means subtracted and then subtract the mean of
the product from the product term (see Schunk and Perales (2017)).
Giesselmann and Schmidt-Catran (2020) show this method carries
between-entity differences that within models are designed to model out.
They suggest an alternate method (\code{"double-demean"}) in which the product
term is first calculated using the de-meaned lower-order variables and
then the subject means are subtracted from this product term. Another
option is to simply use the product term of the de-meaned variables
(\code{"raw"}), but Giesselmann and Schmidt-Catran (2020) show this method
biases the results towards zero effect. The default is \code{"double-demean"}
but if emulating other software is the goal, \code{"demean"} might be
preferred.}

\item{scale}{If \code{TRUE}, reports standardized regression
coefficients by scaling and mean-centering input data (the latter can be
changed via the \code{scale.only} argument). Default is \code{FALSE}.}

\item{scale.response}{Should the response variable also be rescaled? Default
is \code{FALSE}.}

\item{n.sd}{How many standard deviations should you divide by for
standardization? Default is 1, though some prefer 2.}

\item{calc.fit.stats}{Calculate fit statistics? Default is TRUE, but
occasionally poor-fitting models might trip up here.}

\item{...}{Additional arguments provided to \code{\link[geepack:geeglm]{geepack::geeglm()}}.}
}
\value{
A \code{wbgee} object, which inherits from \code{geeglm}.
}
\description{
Fit "within-between" and several other regression variants
for panel data via generalized estimating equations.
}
\details{
See the documentation for \code{\link[=wbm]{wbm()}} for many details on formula syntax and
other arguments.
}
\examples{
if (requireNamespace("geepack")) {
  data("WageData")
  wages <- panel_data(WageData, id = id, wave = t)
  model <- wbgee(lwage ~ lag(union) + wks | blk + fem | blk * lag(union),
           data = wages)
  summary(model)
}

}
\references{
Allison, P. (2009). \emph{Fixed effects regression models}.
Thousand Oaks, CA: SAGE Publications.
https://doi.org/10.4135/9781412993869.d33

Bell, A., & Jones, K. (2015). Explaining fixed effects: Random effects
modeling of time-series cross-sectional and panel data.
\emph{Political Science Research and Methods}, \emph{3}, 133–153.
https://doi.org/10.1017/psrm.2014.7

Curran, P. J., & Bauer, D. J. (2011). The disaggregation of within-person
and between-person effects in longitudinal models of change.
\emph{Annual Review of Psychology}, \emph{62}, 583–619.
https://doi.org/10.1146/annurev.psych.093008.100356

Giesselmann, M., & Schmidt-Catran, A. W. (2020). Interactions in fixed
effects regression models. \emph{Sociological Methods & Research}, 1–28.
https://doi.org/10.1177/0049124120914934

McNeish, D. (2019). Effect partitioning in cross-sectionally clustered data
without multilevel models. \emph{Multivariate Behavioral Research},
Advance online publication. https://doi.org/10.1080/00273171.2019.1602504

McNeish, D., Stapleton, L. M., & Silverman, R. D. (2016). On the unnecessary
ubiquity of hierarchical linear modeling. \emph{Psychological Methods}, \emph{22},
114-140. https://doi.org/10.1037/met0000078

Schunck, R., & Perales, F. (2017). Within- and between-cluster effects in
generalized linear mixed models: A discussion of approaches and the
\code{xthybrid} command. \emph{The Stata Journal}, \emph{17}, 89–115.
https://doi.org/10.1177/1536867X1701700106
}
\author{
Jacob A. Long
}
