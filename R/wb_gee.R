#' @title Panel regression models fit with GEE
#' @description Fit "within-between" and several other regression variants
#'   for panel data via generalized estimating equations.
#' 
#' @inheritParams wbm
#' 
#' @param cor.str Any correlation structure accepted by [geepack::geeglm()].
#'  Default is "ar1", most useful alternative is "exchangeable". "unstructured"
#'  may cause problems due to its computational complexity.
#' 
#' @param ... Additional arguments provided to [geepack::geeglm()].
#'
#' @inheritParams jtools::scale_mod
#' @inheritParams jtools::summ.merMod
#' 
#' @return A `wbgee` object, which inherits from `geeglm`.
#' @author Jacob A. Long
#' @details
#'
#' See the documentation for [wbm()] for many details on formula syntax and
#' other arguments.
#'
#' @examples
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#' model <- wbgee(lwage ~ lag(union) + wks | blk + fem | blk * lag(union),
#'          data = wages)
#' summary(model)
#'
#' @references
#'
#' Allison, P. (2009). *Fixed effects regression models*.
#' Thousand Oaks, CA: SAGE Publications.
#' https://doi.org/10.4135/9781412993869.d33
#'
#' Bell, A., & Jones, K. (2015). Explaining fixed effects: Random effects
#' modeling of time-series cross-sectional and panel data.
#' *Political Science Research and Methods*, *3*, 133–153.
#' https://doi.org/10.1017/psrm.2014.7
#'
#' Curran, P. J., & Bauer, D. J. (2011). The disaggregation of within-person
#' and between-person effects in longitudinal models of change.
#' *Annual Review of Psychology*, *62*, 583–619.
#' https://doi.org/10.1146/annurev.psych.093008.100356
#' 
#' Giesselmann, M., & Schmidt-Catran, A. (2018). Interactions in fixed effects
#' regression models (Discussion Papers of DIW Berlin No. 1748). 
#' *DIW Berlin, German Institute for Economic Research*. 
#' Retrieved from https://ideas.repec.org/p/diw/diwwpp/dp1748.html
#'
#' McNeish, D. (2019). Effect partitioning in cross-sectionally clustered data
#'  without multilevel models. *Multivariate Behavioral Research*, 
#'  Advance online publication. https://doi.org/10.1080/00273171.2019.1602504
#'
#' McNeish, D., Stapleton, L. M., & Silverman, R. D. (2016). On the unnecessary
#'  ubiquity of hierarchical linear modeling. *Psychological Methods*, *22*,
#'  114-140. https://doi.org/10.1037/met0000078
#'
#' Schunck, R., & Perales, F. (2017). Within- and between-cluster effects in
#' generalized linear mixed models: A discussion of approaches and the 
#' `xthybrid` command. *The Stata Journal*, *17*, 89–115. 
#' https://doi.org/10.1177/1536867X1701700106
#'
#' @export

wbgee <- function(formula, data, id = NULL, wave = NULL,
                model = "w-b", 
                cor.str = c("ar1", "exchangeable", "unstructured"),
                detrend = FALSE, use.wave = FALSE,
                wave.factor = FALSE, min.waves = 2, family = gaussian,
                balance.correction = FALSE, dt.random = TRUE, dt.order = 1,
                weights = NULL, offset = NULL, 
                interaction.style = c("double-demean", "demean", "raw"),
                scale = FALSE, scale.response = FALSE, n.sd = 1, ...) {
  
  if (!requireNamespace("geepack")) need_package("geepack")
  
  the_call <- match.call()
  the_call[[1]] <- substitute(wbgee)
  the_env <- parent.frame()
  
  if (any(c(detrend, balance.correction))) {
    if (!requireNamespace("tidyr") | !requireNamespace("purrr")) {
      stop_wrap("To use the 'detrend' or 'balance_correction' arguments, you 
                must have the 'tidyr' and 'purrr' packages installed.")
    }
  }
  
  formula <- Formula::Formula(formula)
  interaction.style <- match.arg(interaction.style,
                                 c("double-demean", "demean", "raw"))
  
  # Send to helper function for data prep
  prepped <- wb_prepare_data(formula = formula, data = data, id = id,
                             wave = wave, model = model, detrend = detrend,
                             use.wave = use.wave, wave.factor = wave.factor,
                             min.waves = min.waves,
                             balance_correction = balance.correction,
                             dt_random = dt.random, dt_order = dt.order,
                             weights = !! enquo(weights),
                             offset = !! enquo(offset), 
                             demean.ints = interaction.style == "double-demean",
                             old.ints = interaction.style == "demean")
  
  e <- prepped$e
  pf <- prepped$pf
  data <- e$data
  wave <- prepped$wave
  id <- prepped$id
  dv <- prepped$dv
  weights <- prepped$weights
  offset <- prepped$offset
  
  
  if (scale == TRUE) {
    
    vars <- names(data)[names(data) %nin% c(wave, id)]
    if (scale.response == FALSE) {vars <- vars[vars != dv]} 
    data <-
      jtools::gscale(data, vars = vars, n.sd = n.sd, binary.inputs = "0/1")
    
  }
  
  if (wave.factor == TRUE) {
    data[[wave]] <- as.factor(data[[wave]])
  }
  
  # Use helper function to generate formula to pass to lme4
  fin_formula <- as.formula(e$fin_formula)  
  
  if (!is.null(offset)) {
    offset[!is.finite(offset)] <- NA
  }
  
  if ("id" %in% names(data) & "id" != id) {
    warn_wrap("The variable name 'id' is reserved for wbgee models. Variable
              'id' has been overwritten with '", id, "' for fitting the model.")
  }
  data$id <- data[[id]]
  if ("wave" %in% names(data) & "wave" != wave) {
    warn_wrap("The variable name 'wave is reserved for wbgee models. Variable
              'wave' has been overwritten with '", wave,
              "' for fitting the model.")
  }
  data$wave <- data[[wave]]
  
  # not ready for primetime
  # if (cor.str[1] == "toeplitz") {
  #   # generating the design matrix for the unstructured correlation
  #   zcor <- geepack::genZcor(clusz = table(data$id), waves = data$wave, 
  #                            corstrv = 4)
  #   # defining the Toeplitz structure 
  #   zcor.toep <- matrix(NA, nrow(zcor), 3)
  #   zcor.toep[, 1] <- apply(zcor[, c(1, 4, 6)], 1, sum)
  #   zcor.toep[, 2] <- apply(zcor[, c(2, 5)], 1, sum)
  #   zcor.toep[, 3] <- zcor[, 3]
  #   zcor <- zcor.toep
  #   cor.str <- "userdefined"
  # } else {
  #   zcor <- NULL
  # }
  
  # Conditionally choose lme4 function based on family argument
  fit <- geepack::geeglm(fin_formula, data = data, weights = weights, 
                         offset = offset, id = id, waves = wave,
                         corstr = cor.str[1], family = family, ...)
  
  ints <- e$cross_ints
  
  fit_sum <- summary(fit)
  coefs <- fit_sum$coefficients
  names(coefs) <- c("Est.", "S.E.", "z val.", "p")
  # Switching from Wald to Z
  coefs[,3] <- coefs[,1]/coefs[,2]
  
  fit$coefs <- coefs
  
  fit$orig_data <- prepped$orig_data
  # if (wave %nin% all.vars(fin_formula)) {
  #   data <- data[names(data) %nin% wave]
  # } 
  fit$frame <- as.data.frame(data)
  attr(fit$frame, "terms") <- terms 
  attr(fit$frame, "formula") <- formula(fit)  
  
  qics <- qic(fit)
  
  fit$call_info <- list(dv = dv, id = id, wave = wave,
                        num_distinct = prepped$num_distinct,
                        varying = c(pf$varying, e$within_ints),
                        constants = pf$constants,
                        meanvars = pf$meanvars, model = model,
                        max_wave = prepped$maxwave, min_wave = prepped$minwave,
                        ints = ints, env = the_env,
                        mf_form = prepped$mf_form,
                        use.wave = use.wave, detrend = detrend,
                        dt_order = dt.order, dt_random = dt.random,
                        balance_correction = balance.correction, pf = pf,
                        qic = qics["QIC"], qicu = qics["QICu"],
                        cic = qics["CIC"], cor.str = cor.str[1],
                        alpha = fit_sum$corr)
  
  fit$call <- the_call
  class(fit) <- c("wbgee", class(fit))
  return(fit)
  
}

#' @export
#' @importFrom stats family
#' @importFrom crayon bold italic

summary.wbgee <- function(object, ...) {
  
  dots <- list(...)
  if ("digits" %in% names(dots)) {
    digits <- dots$digits
  } else {
    digits <- getOption("jtools-digits", 2)
  }
  
  x <- object
  as_gee <- object
  class(as_gee) <- class(as_gee) %not% "wbgee"
  s <- summary(as_gee)
  x2 <- x$call_info
  
  mod_info <- paste0(bold("MODEL INFO:\n"),
                     italic("Entities: "), x2$num_distinct, "\n",
                     italic("Time periods: "), paste0(x2$min_wave, "-",
                                                      x2$max_wave), "\n",
                     italic("Dependent variable: "), x2$dv, "\n")
  if (family(x)$family == "gaussian") {
    mod_info <- paste0(mod_info, italic("Model type:"), " Linear GEE\n")
  } else {
    mod_info <- paste0(mod_info, italic("Model family: "), family(x)$family,
                      ", ", italic("Link: "), family(x)$link, "\n", 
                      italic("Estimation: "), "GEE \n")
  }
  mod_info <- paste0(mod_info, italic("Variance: "), x2$cor.str, 
                     ifelse(x2$cor.str %in% c("ar1", "exchangeable"),
                            yes = paste0(" (alpha = ", round(x2$alpha, digits),
                                         ")\n"), no = "\n"))
  
  
  # Name the model
  est_name <- x2$model
  if (x2$model == "w-b") {est_name <- "within-between"}
  if (est_name == "random") {est_name <- "between"}
  if (est_name == "fixed") {est_name <- "within"}
  
  est_info <- paste(italic("Specification: "), est_name, "\n\n", sep = "")
  
  mod_fit <- paste0(bold("MODEL FIT:"),
                   "\n", italic("QIC = "), round(x2$qic, digits),
                   ", ", italic("QICu = "), round(x2$qicu, digits),
                   ", ", italic("CIC = "), round(x2$cic, digits), "\n\n")

  # For glance method
  mod_info_list <- list(min_wave = x2$min_wave, max_wave = x2$max_wave,
                        N = x2$num_distinct, qic = x2$qic, qicu = x2$qicu,
                        cic = x2$cic, model = est_name)
  
  coefs <- x$coefs
  rownames(coefs) <- gsub("`", "", rownames(coefs), fixed = TRUE)
  if (!is.null(x2$ints)) {
    x2$ints <- gsub("`", "", x2$ints, fixed = TRUE)
  }
  
  varying <- x2$varying
  if (est_name == "within") {
    varying <- c("(Intercept)", varying, x2$ints)
    if (x2$use.wave == TRUE) {varying <- c(varying, x2$wave)}
    x2$ints <- NULL
  }
  
  coefs <- as.data.frame(coefs, make.names = FALSE)
  rows <- rownames(coefs)
  
  if (length(varying) > 0 & est_name != "between") {
    within_table <- coefs[rownames(coefs) %in% varying,]
    coefs <- coefs[rownames(coefs) %nin% varying,]
    rows <- rows %not% varying
  } else {
    within_table <- NULL
  }
  
  if (length(x2$ints) > 0) {
    ints_table <- coefs[rownames(coefs) %in% x2$ints,]
    coefs <- coefs[rownames(coefs) %nin% x2$ints,]
    rows <- rows %not% x2$ints
  } else {
    ints_table <- NULL
  }
  
  if (est_name != "within") {
    between_table <- coefs
  } else {
    between_table <- NULL
  }
  
  out <- list(within_table = within_table, between_table = between_table,
              mod_info = mod_info, mod_fit = mod_fit,
              model = x2$model, est_name = est_name,
              est_info = est_info, ints_table = ints_table,
              digits = digits, mod_info_list = mod_info_list)
  class(out) <- "summary.wbgee"
  return(out)
  
}

#' @export

print.summary.wbgee <- function(x, ...) {
  
  cat(x$mod_info)
  
  cat(x$est_info)
  
  cat(x$mod_fit)
  
  if (x$est_name != "between" & !is.null(x$within_table) && 
      nrow(x$within_table) > 0) {
    
    if (x$est_name != "within") {
      cat(bold("WITHIN EFFECTS:\n"))
    }
    print(md_table(as.data.frame(x$within_table), digits = x$digits,
                   sig.digits = FALSE, 
                   format = getOption("panelr.table.format", "multiline")))
    cat("\n")
    
  }
  
  if (x$est_name != "contextual" & !is.null(x$between_table) && 
      nrow(x$between_table) > 0) {
    
    cat(bold("BETWEEN EFFECTS:\n"))
    print(md_table(x$between_table, digits = x$digits, sig.digits = FALSE,
                   format = getOption("panelr.table.format", "multiline")))
    cat("\n")
    
  } else if (x$est_name == "contextual" & !is.null(x$between_table) && 
             nrow(x$between_table) > 0) {
    
    cat(bold("CONTEXTUAL EFFECTS:\n"))
    print(md_table(x$between_table, digits = x$digits, sig.digits = FALSE,
                   format = getOption("panelr.table.format", "multiline")))
    cat("\n")
    
  }
  
  if (!is.null(x$ints_table) && nrow(x$ints_table) > 0) {
    
    cat(bold("CROSS-LEVEL INTERACTIONS:\n"))
    print(md_table(x$ints_table, digits = x$digits, sig.digits = FALSE,
                   format = getOption("panelr.table.format", "multiline")))
    cat("\n")
    
  }
  
}


#' @export 

print.wbgee <- function(x, ...) {
  
  print(summary(x))
  
}

#' @title Tidy methods for `wbgee` models
#' @description `panelr` provides methods to access `wbgee` data in a tidy format
#' @rdname wbgee_tidiers
#' @param x A `wbgee` object.
#' @param conf.int Logical indicating whether or not to include a confidence
#'  interval in the tidied output. Defaults to `FALSE`.
#' @param conf.level The confidence level to use for the confidence interval if
#'  `conf.int = TRUE`. Must be strictly greater than 0 and less than 1. Defaults
#'  to 0.95, which corresponds to a 95 percent confidence interval.
#' @param ... Ignored
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(broom::tidy, wbgee)
#' } else {
#'   export(tidy.wbgee)
#' }

tidy.wbgee <- function(x, conf.int = FALSE, conf.level = .95, ...) {
  
  if (!requireNamespace("broom")) {
    stop_wrap("You must have the broom package to use tidy methods.")
  }
  
  # Going to get the organized values from the summary function
  sum <- summary(x)
  # Getting their rownames before they are dropped by dplyr
  terms <- c(rownames(sum$within_table), rownames(sum$between_table),
             rownames(sum$ints_table))
  # Binding these tables together but saving their category to the .id variable
  params <- dplyr::bind_rows(within = sum$within_table, 
                             between = sum$between_table, 
                             interactions = sum$ints_table, .id = "group")
  # Adding those rownames as a column
  params$term <- terms
  # Renaming the other columns to fit the tidy model
  switchv <- Vectorize(function(a) {
    switch(a,
           "Est." = "estimate",
           "z val." = "statistic",
           "S.E." = "std.error",
           "p" = "p.value",
           a)
    
  }, "a")
  names(params) <- switchv(names(params))
  
  # Getting confidence intervals if requested
  if (conf.int == TRUE) {
    ints <- as.data.frame(confint(x, level = conf.level))
    # Renaming the columns to fit the tidy model
    names(ints) <- c("conf.low", "conf.high")
    # Renaming the terms to remove the backticks to match the params d.f.
    ints$term <- stringr::str_remove_all(rownames(ints), "`")
    # Put things together
    params <- dplyr::left_join(params, ints, by = "term")
  }
  return(tibble::as_tibble( # Return a tibble
    # Only return the relevant columns
    params %just% c("term", "estimate", "statistic", "std.error", 
                    "conf.low", "conf.high", "p.value", "group")
  ))
}

#' @rdname wbgee_tidiers
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(broom::glance, wbgee)
#' } else {
#'   export(glance.wbgee)
#' }
glance.wbgee <- function(x, ...) {
  sum <- summary(x)
  mod_info_list <- sum$mod_info_list
  mod_info_list[sapply(mod_info_list, is.null)] <- NA
  return(tibble::as_tibble(mod_info_list))
}

#' @export
vcov.wbgee <- function(object, ...) {
  as_gee <- object
  class(as_gee) <- "geeglm"
  out <- summary(as_gee)$cov.scaled
  rownames(out) <- names(coef(object))
  colnames(out) <- names(coef(object))
  out
}

#' @export
#' @importFrom stats qnorm qt vcov
confint.wbgee <- function(object, parm, level = 0.95, cov = NULL, 
                          df.residual = NULL, ...) {
  cf <- coef(object)
  pnames <- names(cf)
  if (missing(parm))
    parm <- pnames
  else if (is.numeric(parm))
    parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  
  format_percentiles <- function(probs, digits) {
    paste0(format(100 * probs, trim = TRUE, scientific = FALSE, 
                  digits = digits), "%")
  }
  pct <- format_percentiles(a, 3)
  
  if (is.null(df.residual)) {
    fac <- qnorm(a)
  } else {
    fac <- qt(a, df.residual)
  }
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  if (is.null(cov)) {
    ses <- sqrt(diag(vcov(object)))[parm]
  } else {
    ses <- sqrt(diag(cov))[parm]
  }
  ci[] <- cf[parm] + ses %o% fac
  ci
}

# adapted from MESS package
qic <- function (object, tol = .Machine$double.eps, ...) {
  invert <- if ("MASS" %in% loadedNamespaces()) {
    get("ginv", asNamespace("MASS"))
  }
  else {
    solve
  }
  compute_qic <- function(object) {
    mu <- object$fitted.values
    y <- object$y
    type <- family(object)$family
    quasi <- switch(type, poisson = sum((y * log(mu)) - mu), 
                    gaussian = sum(((y - mu)^2)/-2),
                    binomial = sum(y * log(mu/(1 - mu)) + log(1 - mu)),
                    Gamma = sum(-y/(mu - log(mu))),
                    warn_wrap("Error calculating QIC: distribution not 
                              recognized"))
    if (is.character(quasi)) return(NULL)
    object$call$corstr <- "independence"
    object$call$zcor <- NULL
    model.indep <- eval(object, parent.frame())
    AIinverse <- invert(model.indep$geese$vbeta.naiv, tol = tol)
    Vr <- object$geese$vbeta
    trace <- sum(diag(AIinverse %*% Vr))
    params <- length(coef(object))
    kpm <- params + length(object$geese$alpha)
    QIC <- -2 * (quasi - trace)
    QICu <- -2 * (quasi - params)
    QICC <- QIC + (2 * kpm * (kpm + 1))/(length(object$residuals) - 
                                           kpm - 1)
    output <- c(QIC, QICu, quasi, trace, params, QICC)
    names(output) <- c("QIC", "QICu", "Quasi Lik", 
                       "CIC", "params", "QICC")
    output
  }
  compute_qic(object)
}

#' @importFrom stats model.matrix delete.response terms coef model.frame 
#' @importFrom stats family  
predict_gee <- function(model, .vcov = vcov(model), newdata = NULL,
                        se.fit = TRUE, dispersion = NULL, terms = NULL,
                        type = c("link", "response", "terms"),
                        na.action = na.pass, ...) {
  
  if (is.null(newdata)) {newdata <- model.frame(model)}
  
  tt <- terms(model)
  Terms <- delete.response(tt)
  m <- model.frame(Terms, newdata, na.action = na.action,
                   xlev = model$xlevels)
  m.mat <- model.matrix(Terms, m, contrasts.arg = model$contrasts)
  m.coef <- coef(model)
  
  offset <- rep(0, nrow(m.mat))
  if (!is.null(off.num <- attr(tt, "offset"))) {
    for (i in off.num) {
      offset <- offset + eval(attr(tt, "variables")[[i + 1]], newdata)
    }
  }
  
  if (!is.null(model$call$offset)) {
    offset <- offset + eval(model$call$offset, newdata)
  }
  
  n <- length(model$residuals)
  p <- model$rank
  p1 <- seq_len(p)
  piv <- if (p) {qr(model)$pivot[p1]}
  
  if (p < ncol(m.mat) && !(missing(newdata) || is.null(newdata))) {
    warning("prediction from a rank-deficient fit may be misleading")
  }
  
  fit <- drop(m.mat[, piv, drop = FALSE] %*% m.coef[piv])
  
  if (!is.null(offset)) {
    fit <- fit + offset
  }
  
  # fit <- as.vector(m.mat %*% m.coef)
  se.fit <- sqrt(diag(m.mat %*% .vcov %*% t(m.mat)))
  
  type <- type[1]
  
  switch(type, response = {
    se.fit <- se.fit * abs(family(model)$mu.eta(fit))
    fit <- family(model)$linkinv(fit)
  }, link = , terms = )
  
  return(list(fit = fit, se.fit = se.fit))
  
}