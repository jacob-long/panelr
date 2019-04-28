#' @title Estimate first differences models using GLS
#' @description The function fits first difference models using GLS estimation.
#' @inheritParams asym
#' @references 
#' Allison, P. D. (2019). Asymmetric fixed-effects models for panel data.
#' *Socius*, *5*, 1-12. https://doi.org/10.1177/2378023119826441
#' @examples 
#' 
#' data("teen_poverty")
#' # Convert to long format
#' teen <- long_panel(teen_poverty, begin = 1, end = 5)
#' model <- fdm(hours ~ lag(pov) + spouse, data = teen)
#' summary(model)
#' 
#' @rdname fdm
#' @export 
fdm <- function(formula, data, id = NULL, wave = NULL, use.wave = FALSE,
                min.waves = 1, 
                variance = c("toeplitz-1", "constrained", "unconstrained"), 
                error.type = c("CR2", "CR1S"), ...) {
  
  if (!requireNamespace("nlme")) need_package("nlme")
  if (!requireNamespace("clubSandwich")) need_package("clubSandwich")
  
  the_call <- match.call()
  the_env <- parent.frame()
  
  formula <- Formula::Formula(formula)
  
  # Send to helper function for data prep
  prepped <- diff_data(formula = formula, data = data, id = id, 
                       wave = wave,  min.waves = min.waves, 
                       weights = NULL, use.wave = use.wave, escape = TRUE)
  
  e <- prepped$e
  pf <- prepped$pf
  data <- e$data
  wave <- prepped$wave
  id <- prepped$id
  dv <- prepped$dv

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
  
  gls_mod <- nlme::gls(fin_formula, data = as.data.frame(data),
                       na.action = na.omit, 
                       correlation = corr, weights = weights)
  
  gls_mod$call$model <- substitute(fin_formula)
  # gls_mod$call <- gls_mod$call[names(gls_mod$call) %not% "data"]
  the_vcov <- vcov_CR(gls_mod, cluster = data[[id]], type = error.type[1],
                      data = data)
  coef_table <- clubSandwich::coef_test(gls_mod, vcov = the_vcov,
                                        test = "naive-t", cluster = data[[id]])
  names(coef_table) <- c("estimate", "std.error", "p.value")
  coef_table["statistic"] <- coef_table$estimate / coef_table$std.error
  
  mod_info <- list(dv = dv, min.wave = prepped$minwave, 
                   max.wave = prepped$maxwave, 
                   num_distinct = prepped$num_distinct,
                   AIC = AIC(gls_mod), BIC = BIC(gls_mod), 
                   variance = variance[1], errors = error.type[1])
  gls_mod$mod_info <- mod_info
  gls_mod$coef_table <- coef_table
  colnames(the_vcov) <- rownames(coef_table)
  rownames(the_vcov) <- rownames(coef_table)
  gls_mod$vcov <- the_vcov
  # Make it a fdm object
  class(gls_mod) <- c("fdm", "gls")
  gls_mod
}

#' @export
summary.fdm <- function(object, ...) {
  
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
                     variance
  )
  
  mod_info_list <- list(N = x$mod_info$num_distinct, 
                        min_wave = x$mod_info$min.wave, 
                        max_wave = x$mod_info$max.wave, 
                        variance = x$mod_info$variance, AIC = x$mod_info$AIC,
                        BIC = x$mod_info$BIC)
  
  coef_table <- as.data.frame(x$coef_table)
  names(coef_table) <- c("Est.", "S.E.", "p", "t val.")
  # rownames(coef_table) <- coef_table$term
  coef_table <- coef_table[c("Est.", "S.E.", "t val.", "p")]
      
  out <- list(mod_info = mod_info, coef_table = coef_table, digits = digits,
              mod_fit = mod_fit, errors = x$mod_info$errors,
              mod_info_list = mod_info_list)
  class(out) <- "summary.fdm"
  out
}

print.summary.fdm <- function(x, ...) {
  cat(x$mod_info, "\n\n")
  cat(x$mod_fit, "\n")
  cat(italic("Standard errors:"), x$errors, "\n")
  print(md_table(x$coef_table, digits = x$digits, sig.digits = FALSE,
                 format = getOption("panelr.table.format", "multiline")))
}

#' Generate differenced and asymmetric effects data
#' 
#' This is an interface to the internal functions that process data for
#' [fdm()], [asym()], and [asym_gee()].
#' 
#' @inheritParams asym
#' @inheritParams wbgee
#' @param asym Return asymmetric effects transformed data? Default is FALSE.
#' @param cumulative Return cumulative positive/negative differences, most
#'  useful for fixed effects estimation and/or generalized linear models? 
#'  Default is FALSE.
#' @param escape.names Return only synactically valid variable names? 
#'  Default is FALSE.
#'  
#' @export
make_diff_data <- function(formula, data, id = NULL, wave = NULL,
                           use.wave = FALSE, min.waves = 1, weights = NULL,
                           offset = NULL, asym = FALSE, cumulative = FALSE, 
                           escape.names = FALSE, ...) {
  the_call <- match.call()
  the_env <- parent.frame()
  
  formula <- Formula::Formula(formula)
  
  # Send to helper function for data prep
  prepped <- diff_data(formula = formula, data = data, id = id, 
                       wave = wave,  min.waves = min.waves, 
                       weights = !! enquo(weights), offset = !! enquo(offset), 
                       use.wave = use.wave, ignore.lhs = cumulative, 
                       asym = asym, escape = escape.names)
  
  prepped$e$data
}

#' @importFrom stats contr.treatment "contrasts<-"
diff_data <- function(formula, data, id = NULL, wave = NULL, min.waves = 2, 
                      weights = NULL, offset = NULL, use.wave = FALSE, 
                      asym = FALSE, ignore.lhs = FALSE, escape = TRUE, ...) {
    
  # Get data prepped
  if ("panel_data" %in% class(data)) {
    id <- attr(data, "id")
    wave <- attr(data, "wave")
  }
  
  if ("data.frame" %nin% class(data)) {
    stop("data argument must be a data frame.")
  }
  
  # Coerce to panel_data
  data <- panel_data(data, id = !! sym(id), wave = !! sym(wave))
  # Save this input data separately
  orig_data <- data
  
  # Get the weights argument like lm() does (can be name or object)
  weights <- eval_tidy(enquo(weights), data)
  # Append to data with special name
  if (!is.null(weights)) {data[".weights"] <- weights}
  # Get the offset argument like lm() does (can be name or object)
  offset <- eval_tidy(enquo(offset), data)
  # Append to data with special name
  if (!is.null(offset)) {data[".offset"] <- offset}
  
  # Get the left-hand side
  dv <- as.character((attr(formula, "lhs")))
  # Pass to helper function
  pf <- wb_formula_parser(formula, dv, data)
  
  # if (!is.null(pf$constants)) {
  #   warn_wrap("Constants are ignored with first difference models")
  # }
  
  # check for wave in formula
  wave_terms <- if (!is.numeric(data[[wave]])) expand_labels(data, wave) else {
    wave
  }
  if (any(wave_terms %in% pf$varying)) {
    pf$varying <- pf$varying %not% wave_terms
    use.wave <- TRUE
  }
  
  # Create formula to pass to model_frame
  mf_form <- paste(" ~ ",
                   paste(pf$allvars, collapse = " + "),
                   collapse = ""
  )
  # Escape non-syntactic variables that are in the data
  mf_form <- formula_ticks(mf_form, names(pf$data))
  
  # Add weights to keep it in the DF
  if (!is.null(weights)) {
    mf_form <- paste(mf_form, "+", ".weights")
  }
  
  # Pass to special model_frame function that respects tibble groupings
  data <- model_frame(update(as.formula(mf_form),
                             as.formula(paste("~ . -", id, "-", wave))), 
                      data = pf$data)
  
  pf$varying <- un_bt(pf$varying)
  
  # Send to helper that will demean, etc.
  e <- diff_model(pf, dv, data, use.wave, asym, ignore.lhs, escape)
  
  # Drop missing cases, collect some metadata 
  e$data <- complete_cases(e$data, min.waves = min.waves)
  num_distinct <- length(unique(e$data[[id]]))
  maxwave <- max(e$data[[wave]])
  minwave <- min(e$data[[wave]])
  if (use.wave == TRUE & !is.numeric(e$data[[wave]])) {
    e$data[[wave]] <- droplevels(e$data[[wave]])
    contrasts(e$data[[wave]]) <- contr.treatment(levels(e$data[[wave]]))
  }
  
  # Modify weights object to reflect missing data drops
  if (!is.null(weights)) {
    weights <- e$data$.weights
  }
  
  # Modify weights object to reflect missing data drops
  if (!is.null(offset)) {
    offset <- e$data$.offset
  }
  
  list(e = e, num_distinct = num_distinct, maxwave = maxwave,
       minwave = minwave, weights = weights,
       pf = pf, id = id, wave = wave, dv = dv, orig_data = orig_data,
       mf_form = mf_form)
    
}

diff_model <- function(pf, dv, data, use.wave = FALSE, asym = FALSE,
                       ignore.lhs = FALSE, escape = TRUE) {
  
  # Extract wave and id
  wave <- get_wave(data)
  id <- get_id(data)
  
  vars <- pf$varying
  if (ignore.lhs == FALSE) vars <- c(dv, vars)
  for (var in vars %not% wave) {
    lag_var <- paste0(bt(var), " - lag(`", var, "`)")
    data <- mutate(data, 
                   !! var := !! parse_expr(lag_var)
            )
  }
  
  asym_list <- list()
  if (asym == TRUE) {
    for (var in c(pf$varying) %not% wave) {
      plus_var <- if (escape) paste0("plus__", var) else paste0("+", var)
      minus_var <- if (escape) paste0("minus__", var) else paste0("-", var)
      plus_expr <- paste0(bt(var), "* (", bt(var), "> 0)")
      neg_expr <- paste0("-", bt(var), "* (", bt(var), "< 0)")
      data <- mutate(data,
                     !! plus_var := !! parse_expr(plus_expr),
                     !! minus_var := !! parse_expr(neg_expr)
              )
      asym_list[[var]] <- c(plus_var, minus_var)
    } 
  }
  
  # Put the pieces together
  if (asym == FALSE) {
    the_terms <- make_names(un_bt(c(pf$varying, pf$wint_labs, pf$cint_labs)), 
                            int = TRUE)
    the_terms <- str_replace_all(the_terms, "_by_", "\\*")
    names(data) <- make_names(names(data))
    # Deal with interactions
    if (!is.null(c(pf$wint_labs, pf$cint_labs))) {
      ints <- asym_ints(data = data, pf = pf, asym_list = list())
      data <- ints$data
      names(data) <- make_names(un_bt(names(data)), int = TRUE)
      the_terms <- c(the_terms, ints$new_terms)
    }
  } else {
    the_terms <- unlist(asym_list)
    # Drop non-varying ones for monotonically increasing/decreasing vars
    varyings <- are_varying(data)
    # Want to let user know what's going on with those
    which_not_varying <- the_terms %just% un_bt(names(varyings %just% FALSE))
    if (length(which_not_varying) > 0) {
      for (var in which_not_varying) {
        plus <- if (escape) "plus__" else "^\\+"
        minus <- if (escape) "minus__" else "^\\-"
        if (stringr::str_detect(var, paste0("^", plus))) {
          bare_var <- stringr::str_extract(var, paste0("(?<=(", plus, ")).*"))
          msg_wrap(bare_var, " does not increase over time so +", bare_var, 
                   " is not included in the model.")
        } else if (stringr::str_detect(var, paste0("^", minus))) {
          bare_var <- stringr::str_extract(var, paste0("(?<=(", minus, ")).*"))
          msg_wrap(bare_var, " does not decrease over time so -", bare_var, 
                   " is not included in the model.")
        }
      }
    }
    # Drop those terms now
    the_terms <- the_terms %not% un_bt(names(varyings %just% FALSE))
    # Edit the list of asymmetric effects
    asym_list <- lapply(asym_list, function(x) {
      x %not% un_bt(names(varyings %just% FALSE))
    })
    lengths <- sapply(asym_list, length) == 2
    asym_list <- asym_list[lengths]
    if (escape) {
      asym_list <- lapply(asym_list, make_names)
      names(asym_list) <- make_names(names(asym_list))
      the_terms <- make_names(the_terms)
      names(data) <- make_names(names(data))
    } else {
      the_terms <- bt(the_terms)
    }
    # Deal with interactions
    if (!is.null(c(pf$wint_labs, pf$cint_labs))) {
      ints <- asym_ints(data = data, pf = pf, asym_list = asym_list,
                        escape = escape)
      data <- ints$data
      if (escape) {
        names(data) <- make_names(names(data))
      }
      the_terms <- c(the_terms, ints$new_terms)
    }
  }
  
  # Now do cumulative sums if appropriate
  if (asym == TRUE & ignore.lhs == TRUE) {
    for (var in c(pf$varying) %not% wave) {
      plus_var <- if (escape) paste0("plus__", var) else paste0("+", var)
      minus_var <- if (escape) paste0("minus__", var) else paste0("-", var)
      # First need to set the wave 1 values to 0 instead of NA
      set_zero_plus <- paste0("case_when(", wave, " == ", get_periods(data)[1],
                              " ~ 0, TRUE ~", bt(plus_var), ")")
      set_zero_neg <- paste0("case_when(", wave, " == ", get_periods(data)[1],
                              " ~ 0, TRUE ~", bt(minus_var), ")")
      plus_expr <- paste0("cumsum(", bt(plus_var), ")")
      neg_expr <- paste0("cumsum(", bt(minus_var), ")")
      data <- mutate(data,
                     !! plus_var := !! parse_expr(set_zero_plus),
                     !! minus_var := !! parse_expr(set_zero_neg),
                     !! plus_var := !! parse_expr(plus_expr),
                     !! minus_var := !! parse_expr(neg_expr)
      )
    }
  }
  
  fin_formula <- paste(bt(dv), "~", paste(the_terms, collapse = " + "))
  if (use.wave == TRUE) fin_formula <- paste(fin_formula, "+", wave)
  
  out <- list(data = data, fin_formula = fin_formula, asym_list = asym_list)
  return(out)
  
}

asym_ints <- function(data, pf, asym_list, escape = TRUE) {
  all_ints <- c(pf$wint_labs, pf$cint_labs)
  all_terms <- NULL
  for (int in all_ints) {
    name_make <- if (escape) make_names else function(x) return(x)
    splits <- as.list(name_make(un_bt(str_split(int, "\\*")[[1]])))
    names(splits) <- splits
    for (spl in names(splits)) {
      if (un_bt(spl) %in% names(asym_list)) {
        splits[[spl]] <- asym_list[[un_bt(spl)]]
      }
    }
    term_grid <- expand.grid(splits)
    term_grid <- as.data.frame(lapply(term_grid, bt))
    new_terms <- rep(NA, nrow(term_grid))
    for (r in 1:nrow(term_grid)) {
      new_terms[r] <- paste0(unlist(term_grid[r, , drop = TRUE]), collapse = "*")
      # data <- mutate(data,
      #   !! un_bt(new_terms[r]) := !! parse_expr(new_terms[r])
      # )
    }
    all_terms <- c(all_terms, new_terms)
  }
  list(data = data, new_terms = all_terms)
}

#' @export
coef.fdm <- function(object, ...) {
  out <- object$coef_table$estimate
  names(out) <- rownames(object$coef_table)
  out
}

#' @export
vcov.fdm <- function(object, ...) {
  object$vcov
}

#' @export
confint.fdm <- function(object, parm = NULL, level = .95, ...) {
  confint.wbgee(object, parm = parm, level = level, ...)
}

#' @title Tidy methods for `fdm` and `asym` models
#' @description `panelr` provides methods to access `fdm` and `asym` data in a
#'  tidy format
#' @rdname fdm_tidiers
#' @param x An `fdm` or `asym` object.
#' @param conf.int Logical indicating whether or not to include a confidence
#'  interval in the tidied output. Defaults to `FALSE`.
#' @param conf.level The confidence level to use for the confidence interval if
#'  `conf.int = TRUE`. Must be strictly greater than 0 and less than 1. Defaults
#'  to 0.95, which corresponds to a 95 percent confidence interval.
#' @param ... Ignored
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(broom::tidy, fdm)
#' } else {
#'   export(tidy.fdm)
#' }

tidy.fdm <- function(x, conf.int = FALSE, conf.level = .95, ...) {
  
  if (!requireNamespace("broom")) {
    stop_wrap("You must have the broom package to use tidy methods.")
  }
  
  params <- x$coef_table
  params$term <- rownames(params)
  
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

#' @rdname fdm_tidiers
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(broom::glance, fdm)
#' } else {
#'   export(glance.fdm)
#' }
glance.fdm <- function(x, ...) {
  sum <- summary(x)
  mod_info_list <- sum$mod_info_list
  mod_info_list[sapply(mod_info_list, is.null)] <- NA
  return(tibble::as_tibble(mod_info_list))
}

