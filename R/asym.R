#' @title Estimate asymmetric effects models using first differences
#' @description The function fits the asymmetric effects first difference
#'  model described in Allison (2019) using GLS estimation.
#' @inheritParams wbm
#' @param variance One of `"toeplitz-1"`, `"constrained"`, or `"unconstrained"`.
#'   The toeplitz variance specification estimates a single error variance and
#'   a single lag-1 error correlation with other lags having zero correlation.
#'   The constrained model assumes no autocorrelated errors or heteroskedastic
#'   errors. The unconstrained option allows separate variances for every 
#'   period as well as every lag of autocorrelation. This can be very 
#'   computationally taxing as periods increase and will be inefficient when 
#'   not necessary. See Allison (2019) for more.
#' @param error.type Either "CR2" or "CR1S". See the `clubSandwich` package for
#'  more details.
#' @param ... Ignored.
#' @references 
#' Allison, P. D. (2019). Asymmetric fixed-effects models for panel data.
#' *Socius*, *5*, 1-12. https://doi.org/10.1177/2378023119826441
#' @examples 
#' 
#' data("teen_poverty")
#' # Convert to long format
#' teen <- long_panel(teen_poverty, begin = 1, end = 5)
#' model <- asym(hours ~ lag(pov) + spouse, data = teen)
#' summary(model)
#' 
#' @rdname asym
#' @importFrom stats BIC AIC na.omit
#' @export 
asym <- function(formula, data, id = NULL, wave = NULL, use.wave = FALSE,
                 min.waves = 1, 
                 variance = c("toeplitz-1", "constrained", "unconstrained"),
                 error.type = c("CR2", "CR1S"), ...) {
  
  if (!requireNamespace("nlme")) need_package("nlme")
  if (!requireNamespace("clubSandwich")) need_package("clubSandwich")
  if (!requireNamespace("car")) need_package("car")
  
  the_call <- match.call()
  the_env <- parent.frame()
  
  formula <- Formula::Formula(formula)
  
  # Send to helper function for data prep
  prepped <- diff_data(formula = formula, data = data, id = id, 
                       wave = wave,  min.waves = min.waves, 
                       weights = NULL, use.wave = use.wave,
                       asym = TRUE)
  
  e <- prepped$e
  pf <- prepped$pf
  data <- e$data
  wave <- prepped$wave
  id <- prepped$id
  dv <- prepped$dv
  weights <- prepped$weights
  
  # Use helper function to generate formula to pass to lme4
  fin_formula <- as.formula(e$fin_formula)
  if (!is.null(pf$constants)) {
    constants <- paste(pf$constants, collapse = " - ")
    up_form <- as.formula(paste(". ~ . -", constants))
    fin_formula <- update(fin_formula, up_form)
  }
  
  if (variance[1] == "unconstrained") {
    cor_form <- as.formula(paste("~ 1 |", id))
    var_form <- as.formula(paste("~ 1 |", wave))
    corr <- nlme::corSymm(form = cor_form)       # unstructured covariance
    weights <- nlme::varIdent(form = var_form)
  } else if (variance[1] == "constrained") {
    cor_form <- as.formula(paste(" ~ 1 |", id))
    corr <- nlme::corARMA(value = -.9999, form = cor_form, p = 0, q = 1,
                          fixed = TRUE)
    weights <- NULL
  } else if (variance[1] == "toeplitz-1") {
    cor_form <- as.formula(paste(" ~ 1 |", id))
    corr <- nlme::corARMA(form = cor_form, p = 0, q = 1)
    weights <- NULL
  }
  
  gls_mod <- nlme::gls(fin_formula, data = data,
                       na.action = na.omit, 
                       correlation = corr, weights = weights)

  gls_mod$call$model <- substitute(fin_formula)
  
  the_vcov <- vcov_CR(gls_mod, cluster = data[[id]], type = error.type[1],
                      data = data)
  coef_table <- as.data.frame(clubSandwich::coef_test(gls_mod, vcov = the_vcov,
                                        test = "naive-t", cluster = data[[id]]))
  coef_table <- as.data.frame(coef_table)
  if ("tstat" %nin% names(coef_table)) { # old version of clubSandwich
    names(coef_table) <- c("estimate", "std.error", "p.value")
    coef_table["statistic"] <- coef_table$estimate / coef_table$std.error
  } else {
    names(coef_table) <- c("estimate", "std.error", "statistic", "p.value")
    coef_table["term"] <- rownames(coef_table)
  }
  coef_table$term <- gsub("plus__", "\\+", coef_table$term)
  coef_table$term <- gsub("minus__", "\\-", coef_table$term)
  
  if (length(e$asym_list) > 0) {
    asym_diffs <- test_asyms(gls_mod, e$asym_list, the_vcov)
    for (var in names(e$asym_list)) {
      if (stringr::str_detect(var, "^lag_.*_$")) {
        var <- stringr::str_replace(var, "^lag_(.*)_$", "\\1")
      }
      rownames(coef_table) <- stringr::str_replace(
        rownames(coef_table), paste0("lag_", var, "_"), paste0("lag(", var, ")")
      )
      asym_diffs$term <- stringr::str_replace(
        asym_diffs$term, paste0("lag_", var, "_"), paste0("lag(", var, ")")
      )
    }
  } else {
    asym_diffs <- NULL
  }
  
  mod_info <- list(dv = dv, min.wave = prepped$minwave, 
                   max.wave = prepped$maxwave, 
                   num_distinct = prepped$num_distinct,
                   AIC = AIC(gls_mod), BIC = BIC(gls_mod), 
                   variance = variance[1],
                   errors = paste0("Cluster-robust (", error.type[1], ")"))
  gls_mod$mod_info <- mod_info
  gls_mod$coef_table <- coef_table
  gls_mod$asym_diffs <- asym_diffs
  colnames(the_vcov) <- rownames(coef_table)
  rownames(the_vcov) <- rownames(coef_table)
  gls_mod$vcov <- the_vcov
  
  # Make it a fdm object
  class(gls_mod) <- c("asym", "fdm", "gls")
  gls_mod
}

#' @export
summary.asym <- function(object, ...) {
  
  dots <- list(...)
  if ("digits" %in% names(dots)) {
    digits <- dots$digits
  } else {
    digits <- getOption("jtools-digits", 2)
  }
  
  x <- object
  
  mod_fit <- paste0(bold("MODEL FIT:\n"),
                    italic("AIC = "), round(x$mod_info$AIC, digits),
                    italic(", BIC = "), round(x$mod_info$BIC, digits),  "\n")
  
  if (x$mod_info$variance == "toeplitz-1") {
    corstruct <- object$modelStruct$corStruct
    class(corstruct) <- "corARMA"
    theta <- coef(corstruct, unconstrained = FALSE)
    variance <- paste0(italic("Variance structure: "), x$mod_info$variance,
                       " (theta = ", round(theta, digits), ")")
  } else {
    variance <- paste0(italic("Variance structure: "), x$mod_info$variance)
  }
  
  mod_info <- paste0(bold("MODEL INFO:\n"),
                     italic("Entities: "), x$mod_info$num_distinct, "\n",
                     italic("Time periods: "), paste0(x$mod_info$min.wave, "-",
                                                      x$mod_info$max.wave), "\n",
                     italic("Dependent variable: "), x$mod_info$dv, "\n",
                     italic("Model type: "),
                     "Linear asymmetric effects (first differences)\n",
                     variance
  )
  
  mod_info_list <- list(N = x$mod_info$num_distinct, 
                        min_wave = x$mod_info$min.wave, 
                        max_wave = x$mod_info$max.wave, 
                        variance = x$mod_info$variance, AIC = x$mod_info$AIC,
                        BIC = x$mod_info$BIC)
  
  coef_table <- x$coef_table
  names(coef_table) <- sapply(names(coef_table), function(x) {switch(x,
    "estimate" = "Est.",
    "std.error" = "S.E.",
    "p.value" = "p",
    "statistic" = "t val.",
    x
  )})
  rownames(coef_table) <- coef_table$term
  # rownames(coef_table) <- coef_table$term
  coef_table <- coef_table[c("Est.", "S.E.", "t val.", "p")]
  
  if (!is.null(x$asym_diffs)) {
    names(x$asym_diffs) <- c("variable", "chi^2", "p")
  }
  
  out <- list(mod_info = mod_info, coef_table = coef_table, digits = digits,
              asym_diffs = x$asym_diffs, errors = x$mod_info$errors,
              mod_info_list = mod_info_list)
  class(out) <- "summary.asym"
  out
}

#' @export
print.summary.asym <- function(x, ...) {
  cat(x$mod_info, "\n\n")
  
  cat(italic("Standard errors:"), x$errors, "\n")
  print(md_table(x$coef_table, digits = x$digits, sig.digits = FALSE,
                 format = getOption("panelr.table.format", "multiline")))

  if (!is.null(x$asym_diffs)) {
    cat("\n", bold("Tests of asymmetric effects:\n"), sep = "")
    rownames(x$asym_diffs) <- x$asym_diffs$variable
    x$asym_diffs <- x$asym_diffs %not% "variable"
    print(md_table(x$asym_diffs, digits = x$digits, sig.digits = FALSE,
                   format = getOption("panelr.table.format", "multiline")))
  }
}

test_asyms <- function(model, asyms, vcov = NULL, escape = TRUE) {
  output <- data.frame(term = rep(NA, length(asyms)), 
                       statistic = rep(NA, length(asyms)),
                       p.value = rep(NA, length(asyms)))
  for (var in names(asyms)) {
    index <- which(names(asyms) == var)
    output[index, "term"] <- var
    if (escape) {
      test_term <- paste0("plus__", var, " = -minus__", var)
    } else {
      test_term <- paste0("`+", var, "` = -`-", var, "`")
    }
    the_test <- as.data.frame(car::lht(model, test_term, vcov. = vcov))
    output[index, "statistic"] <- the_test[2, "Chisq"]
    output[index, "p.value"] <- the_test[2, "Pr(>Chisq)"]
  }
  output
}
