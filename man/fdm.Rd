% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diffs.R
\name{fdm}
\alias{fdm}
\title{Estimate first differences models using GLS}
\usage{
fdm(
  formula,
  data,
  id = NULL,
  wave = NULL,
  use.wave = FALSE,
  min.waves = 1,
  variance = c("toeplitz-1", "constrained", "unconstrained"),
  error.type = c("CR2", "CR1S"),
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

\item{use.wave}{Should the wave be included as a predictor? Default is
FALSE.}

\item{min.waves}{What is the minimum number of waves an individual must
have participated in to be included in the analysis? Default is \code{2} and
any valid number is accepted. \code{"all"} is also acceptable if you want to
include only complete panelists.}

\item{variance}{One of \code{"toeplitz-1"}, \code{"constrained"}, or \code{"unconstrained"}.
The toeplitz variance specification estimates a single error variance and
a single lag-1 error correlation with other lags having zero correlation.
The constrained model assumes no autocorrelated errors or heteroskedastic
errors. The unconstrained option allows separate variances for every
period as well as every lag of autocorrelation. This can be very
computationally taxing as periods increase and will be inefficient when
not necessary. See Allison (2019) for more.}

\item{error.type}{Either "CR2" or "CR1S". See the \code{clubSandwich} package for
more details.}

\item{...}{Ignored.}
}
\description{
The function fits first difference models using GLS estimation.
}
\examples{

if (requireNamespace("clubSandwich")) {
  data("teen_poverty")
  # Convert to long format
  teen <- long_panel(teen_poverty, begin = 1, end = 5)
  model <- fdm(hours ~ lag(pov) + spouse, data = teen)
  summary(model)
}

}
\references{
Allison, P. D. (2019). Asymmetric fixed-effects models for panel data.
\emph{Socius}, \emph{5}, 1-12. https://doi.org/10.1177/2378023119826441
}
