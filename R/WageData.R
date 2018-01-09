#' Earnings data from the Panel Study of Income Dynamics
#'
#' These data come from the years 1976-1982 in the Panel Study of
#' Income Dynamics (PSID), with information about the demographics
#' and earnings of 595 individuals.
#'
#' @format A data frame with 4165 rows and 14 variables:
#' \describe{
#'   \item{id}{Unique identifier for each survey respondent}
#'   \item{t}{A number corresponding to each wave of the survey, 1 through 7}
#'   \item{wks}{Weeks worked in the past year}
#'   \item{lwage}{Natural logarithm of earnings in the past year}
#'   \item{union}{Binary indicator whether respondent is a member of union
#'    (1 = union member)}
#'   \item{ms}{Binary indicator for whether respondent is married (1 = married)}
#'   \item{occ}{Binary indicator for whether respondent is a blue collar (= 0) or
#'    white collar (= 1) worker.}
#'   \item{ind}{Binary indicator for whether respondent works in
#'    manufacturing (= 1)}
#'   \item{south}{Binary indicator for whether respondent lives in the South (= 1)}
#'   \item{smsa}{Binary indicator for whether respondent lives in a
#'    standard metropolitan area (SMSA; = 1)}
#'   \item{fem}{Binary indicator for whether respondent is female (= 1)}
#'   \item{blk}{Binary indicator for whether respondent is African-American (= 1)}
#'   \item{ed}{Years of education}
#'   \item{exp}{Years in the workforce.}
#' }
#' @source
#'  These data are all over the place. This particular file was
#'  downloaded from Richard Williams at
#'  \url{http://www3.nd.edu/~rwilliam/statafiles/wages.dta}, though he doesn't
#'  claim ownership of these data.
#'
#'  The data were shared as a supplement to Baltagi (2005) at
#'  \url{http://www.wiley.com/legacy/wileychi/baltagi3e/data_sets.html}.
#'
#'  They were also shared as a supplement to Greene (2008) at
#'  \url{http://pages.stern.nyu.edu/~wgreene/Text/Edition6/tablelist6.htm}.
#'
#'  The data are also available in numerous other locations, including in
#'  slightly different formats as \code{\link[plm]{Wages}} in the \pkg{plm}
#'  package and \code{\link[AER]{PSID7682}} in the \pkg{AER} package.
#'
"WageData"
