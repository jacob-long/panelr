#' @title Asymmetric effects models fit with GEE
#' @description Fit "within-between" and several other regression variants
#'   for panel data via generalized estimating equations.
#' 
#' @inheritParams wbgee
#' 
#' @return An `asym_gee` object, which inherits from `wbgee` and `geeglm`.
#' @author Jacob A. Long
#' @details
#'
#' See the documentation for [wbm()] for many details on formula syntax and
#' other arguments.
#'
#' @examples
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#' model <- asym_gee(lwage ~ lag(union) + wks, data = wages)
#' summary(model)
#'
#' @references
#' 
#' Allison, P. D. (2019). Asymmetric fixed-effects models for panel data.
#' *Socius*, *5*, 1-12. https://doi.org/10.1177/2378023119826441
#'
#' McNeish, D. (2019). Effect partitioning in cross-sectionally clustered data
#'  without multilevel models. *Multivariate Behavioral Research*, 
#'  Advance online publication. https://doi.org/10.1080/00273171.2019.1602504
#'
#' McNeish, D., Stapleton, L. M., & Silverman, R. D. (2016). On the unnecessary
#'  ubiquity of hierarchical linear modeling. *Psychological Methods*, *22*,
#'  114-140. https://doi.org/10.1037/met0000078
#'
#' @export

asym_gee <- function(formula, data, id = NULL, wave = NULL,
                     cor.str = c("ar1", "exchangeable", "unstructured"),
                     use.wave = FALSE, wave.factor = FALSE, min.waves = 1,
                     family = gaussian, weights = NULL, offset = NULL,  ...) {
  
  if (!requireNamespace("geepack")) need_package("geepack")
  if (!requireNamespace("car")) need_package("car")
  
  the_call <- match.call()
  the_call[[1]] <- substitute(asym_gee)
  the_env <- parent.frame()
  
  formula <- Formula::Formula(formula)
  
  ignore.lhs <- as.character(substitute(family))[1] != "gaussian"
  # ignore.lhs <- TRUE
  # Send to helper function for data prep
  prepped <- diff_data(formula = formula, data = data, id = id, 
                       wave = wave,  min.waves = min.waves, 
                       weights = !! enquo(weights),
                       offset = !! enquo(offset), use.wave = use.wave, 
                       asym = TRUE, ignore.lhs = ignore.lhs, escape = FALSE)
  
  e <- prepped$e
  pf <- prepped$pf
  data <- e$data
  wave <- prepped$wave
  id <- prepped$id
  dv <- prepped$dv
  weights <- prepped$weights
  offset <- prepped$offset
  
  if (wave.factor == TRUE) {
    data[[wave]] <- as.factor(data[[wave]])
  }
  
  # Use helper function to generate formula to pass to lme4
  fin_formula <- as.formula(e$fin_formula)  
  if (!is.null(pf$constants)) {
    constants <- paste(pf$constants, collapse = " - ")
    up_form <- as.formula(paste(". ~ . -", constants))
    fin_formula <- update(fin_formula, up_form)
  }
  
  if (!is.null(offset)) {
    offset[!is.finite(offset)] <- NA
  }
  
  if ("id" %in% names(data) & "id" != id) {
    warn_wrap("The variable name 'id' is reserved for asym_gee models. Variable
              'id' has been overwritten with '", id, "' for fitting the model.")
  }
  data$id <- data[[id]]
  if ("wave" %in% names(data) & "wave" != wave) {
    warn_wrap("The variable name 'wave is reserved for asym_gee models. Variable
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

  fit_sum <- summary(fit)
  coefs <- fit_sum$coefficients
  names(coefs) <- c("Est.", "S.E.", "z val.", "p")
  # Switching from Wald to Z
  coefs[,3] <- coefs[,1]/coefs[,2]
  
  fit$coefs <- coefs
  rownames(fit$coefs) <- un_bt(rownames(fit$coefs))
  fit$asym_diffs <- test_asyms(fit, e$asym_list, vcov = vcov.wbgee(fit),
                               escape = FALSE)
  
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
                        max_wave = prepped$maxwave, min_wave = prepped$minwave,
                        env = the_env, mf_form = prepped$mf_form,
                        use.wave = use.wave, pf = pf,
                        qic = qics["QIC"], qicu = qics["QICu"],
                        cic = qics["CIC"], cor.str = cor.str[1],
                        alpha = fit_sum$corr)
  
  fit$call <- the_call
  class(fit) <- c("asym_gee", "wbgee", class(fit))
  return(fit)
  
}

#' @export
#' @importFrom stats family
#' @importFrom crayon bold italic

summary.asym_gee <- function(object, ...) {
  
  dots <- list(...)
  if ("digits" %in% names(dots)) {
    digits <- dots$digits
  } else {
    digits <- getOption("jtools-digits", 2)
  }
  
  x <- object
  as_gee <- object
  class(as_gee) <- class(as_gee) %not% c("wbgee", "asym_gee")
  s <- summary(as_gee)
  x2 <- x$call_info
  
  mod_info <- paste0(bold("MODEL INFO:\n"),
                     italic("Entities: "), x2$num_distinct, "\n",
                     italic("Time periods: "), paste0(x2$min_wave, "-",
                                                      x2$max_wave), "\n",
                     italic("Dependent variable: "), x2$dv, "\n")
  if (family(x)$family == "gaussian") {
    mod_info <- paste0(mod_info, italic("Model family:"), " Linear \n")
  } else {
    mod_info <- paste0(mod_info, italic("Model family: "), family(x)$family,
                       ", ", italic("Link: "), family(x)$link, "\n")
  }
  mod_info <- paste0(mod_info, italic("Variance: "), x2$cor.str, 
                     ifelse(x2$cor.str %in% c("ar1", "exchangeable"),
                            yes = paste0(" (alpha = ", round(x2$alpha, digits),
                                         ")\n"), no = "\n"))
  
  est_info <- paste0(italic("Specification: "),
                     "Asymmetric effects (via GEE)\n\n")
  
  mod_fit <- paste0(bold("MODEL FIT:"),
                    "\n", italic("QIC = "), round(x2$qic, digits),
                    ", ", italic("QICu = "), round(x2$qicu, digits),
                    ", ", italic("CIC = "), round(x2$cic, digits), "\n\n")
  
  # For glance method
  mod_info_list <- list(min_wave = x2$min_wave, max_wave = x2$max_wave,
                        N = x2$num_distinct, qic = x2$qic, qicu = x2$qicu,
                        cic = x2$cic)
  
  coefs <- x$coefs
  rownames(coefs) <- gsub("`", "", rownames(coefs), fixed = TRUE)
  coefs <- as.data.frame(coefs, make.names = FALSE)
  rows <- rownames(coefs)
  
  out <- list(coef_table = x$coefs, asym_diffs = x$asym_diffs,
              mod_info = mod_info, mod_fit = mod_fit,
              est_info = est_info, digits = digits,
              mod_info_list = mod_info_list)
  class(out) <- "summary.asym_gee"
  return(out)
  
}

#' @export

print.summary.asym_gee <- function(x, ...) {
  
  cat(x$mod_info)
  
  cat(x$est_info)
  
  cat(x$mod_fit)
  
  print(md_table(as.data.frame(x$coef_table), digits = x$digits,
                 sig.digits = FALSE,  
                 format = getOption("panelr.table.format", "multiline")))
  cat("\n")
  
  if (length(x$asym_diffs) > 0) {
    rownames(x$asym_diffs) <- x$asym_diffs$term
    x$asym_diffs <- x$asym_diffs %not% "term"
    names(x$asym_diffs) <- c("chi^2", "p")
    cat(bold("Tests of asymmetric effects:\n"))
    print(md_table(as.data.frame(x$asym_diffs), digits = x$digits, 
                   sig.digits = FALSE,  
                   format = getOption("panelr.table.format", "multiline")))
  }
  
}


#' @export 

print.asym_gee <- function(x, ...) {
  
  print(summary(x))
  
}

#' @rdname wbgee_tidiers
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(broom::tidy, asym_gee)
#' } else {
#'   export(tidy.asym_gee)
#' }

tidy.asym_gee <- function(x, conf.int = FALSE, conf.level = .95, ...) {
  
  if (!requireNamespace("broom")) {
    stop_wrap("You must have the broom package to use tidy methods.")
  }

  # Getting their rownames before they are dropped by dplyr
  params <- x$coefs
  # Adding those rownames as a column
  params$term <- un_bt(rownames(params))
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
                    "conf.low", "conf.high", "p.value")
  ))
}
