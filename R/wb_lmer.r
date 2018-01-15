#' @title Panel regression models
#' @description Fit "within-between" and several other regression variants
#'   for panel data in a multi-level modeling framework.
#' @param formula Model formula. See details for crucial
#'   info on `panelr`'s formula syntax.
#' @param data The data, either a `panel_data` object or `data.frame`.
#' @param id If `data` is not a `panel_data` object, then the name of the
#'   individual id column. Otherwise, leave as NULL, the default.
#' @param wave If `data` is not a `panel_data` object, then the name of the
#'   panel wave column. Otherwise, leave as NULL, the default.
#' @param model One of `"w-b"`, `"within"`, `"between"`,
#'   `"contextual"`, or `"stability"`. See details for more on these options.
#' @param use.wave Should the wave be included as a predictor? Default is
#'   FALSE.
#' @param wave.factor Should the wave variable be treated as an unordered
#'   factor instead of continuous? Default is FALSE.
#' @param min.waves What is the minimum number of waves an individual must
#'   have participated in to be included in the analysis? Default is `2` and
#'   any valid number is accepted. `"all"` is also acceptable if you want to
#'   include only complete panelists.
#' @param family Use this to specify GLM link families. Default is `gaussian`,
#'   the linear model.
#' @param dynamic Include the lagged dependent variable? Caution is advised
#'   if you want to do this, but it is hard to fit this model via the formula
#'   syntax. Default is FALSE.
#' @param pR2 Calculate a pseudo R-squared? Default is FALSE because it
#'   often adds a great deal of computation time if sample sizes are medium
#'   size or larger.
#' @param pvals Calculate p values? Default is TRUE but for some complex
#'   linear models, this may take a long time to compute using the `pbkrtest`
#'   package.
#' @param weights If using weights, either the name of the column in the data
#'   that contains the weights or a vector of the weights.
#' @param ... Additional arguments provided to [lme4::lmer()],
#'   [lme4::glmer()], or [lme4::glmer.nb()].
#' @return A `wbm` object.
#' @author Jacob A. Long
#' @details
#'
#' **Formula syntax**
#'
#' The within-between models, and multilevel panel models more generally,
#' distinguish between time-varying and time-invariant predictors. These are,
#' as they sound, variables that are either measured repeatedly (in every wave)
#' in the case of time-varying predictors or only once in the case of
#' time-invariant predictors. You need to specify these separately in the
#' formula to tell the model which variables you expect to change over time and
#' which will not. The primary way of doing so is via the `|` operator.
#'
#' As an example, we can look at the [WageData] included in this
#' package. We will create a model that predicts the logarithm of the
#' individual's wages (`lwage`) with their union status (`union`), which can
#' change over time, and their race (`blk`; dichotomized as black or
#' non-black),
#' which does not change throughout the period of study. Our formula will look
#' like this:
#'
#' ```
#' lwage ~ union | blk
#' ```
#'
#' We put time-varying variables before the first `|` and time-invariant
#' variables afterwards. You can specify lags like `lag(union)` for time-varying
#' variables; for more than 1 lag, include the number: `lag(union, 2)`.
#'
#' After the first `|` go the time-invariant variables. Note that if you put a
#' time-varying variable here, only the first wave measure will be used — in
#' some cases this will be what you want. You may also take a time-varying
#' variable --- let's say weeks worked (`wks`) --- and use `imean(wks)` to
#' include the individual's mean across all waves as a predictor while omitting
#' the per-wave measures.
#'
#' There is also a place for a second `|`. Here you can specify cross-level
#' interactions (within-level interactions can be specified here as well).
#' If I wanted the interaction term for `union` and `blk` --- to see whether
#' the effect of union status depended on one's race --- I would specify the
#' formula this way:
#'
#' ```
#' lwage ~ union | blk | union * blk
#' ```
#'
#' Another use for the post-second `|` section of the formula is for changing
#' the random effects specification. By default, only a random intercept is
#' specified in the call to [lme4::lmer()]/[lme4::glmer()]. If you would like
#' to specify other random slopes, include them here using the typical `lme4`
#' syntax:
#'
#' ```
#' lwage ~ union | blk | (union | id)
#' ```
#'
#' Note that if your random slope term has non-alphanumeric characters (like
#' if you want a random slope for `lag(union)`, then *for the random effect
#' specification only*, you need to put that term in backticks. For example,
#'
#' ```
#' lwage ~ lag(union) | blk | (`lag(union)` | id)
#' ```
#'
#' This is just a limitation of the way the formulas are dealt with by
#' `panelr`.
#'
#' One last thing to know: If you want to use the second `|` but not the first,
#' put a 1 or 0 after the first, like this:
#'
#' ```
#' lwage ~ union | 1 | (union | id)
#' ```
#'
#' Of course, with no time-invariant variables, you need no `|` operators at
#' all.
#'
#' **Models**
#'
#' As a convenience, `wbm` does the heavy lifting for specifying the
#' within-between model correctly. Of course, as a side effect it only
#' takes a few easy tweaks to specify the model slightly differently. You
#' can change this behavior with the `model` argument.
#'
#' By default, the argument is `"w-b"` (equivalently, `"within-between"`).
#' This means, for each time-varying predictor, you have two types of
#' variables in the model. The "between" effect is represented by the
#' individual-level mean for each entity (e.g., each respondent to a panel
#' survey). The "within" effect is represented by each wave's measure *with
#' the individual-level mean* subtracted. Some refer to this as "de-meaning."
#' Thinking in a Hausman test framework --- with the within-between model as
#' described here --- you should expect the within and between
#' coefficients to be the same if a random effects model were appropriate.
#'
#' The contextual model is very similar (use argument `"contextual"`). In
#' some situations, this will be more intuitive to interpret. Empirically,
#' the only difference compared to the within-between specification is that
#' the contextual model does not subtract the individual-level means from the
#' wave-level measures. This also changes the interpretation of the
#' between-subject coefficients: In the contextual model, they are the
#' *difference* between the within and between effects. If there's no
#' difference between within and between effects, then, the coefficients will
#' be 0.
#'
#' To fit a random effects model, use either `"between"` or `"random"`. This
#' involves no de-meaning and no individual-level means whatsoever.
#'
#' To fit a fixed effects model, use either `"within"` or `"fixed"`. Any
#' between-subjects terms in the formula will be ignored. The time-varying
#' variables will be de-meaned, but the individual-level mean is not included
#' in the model.
#'
#' Another option is what I'm calling `"stability"`, which is a non-standard
#' term. This is another convenience, this being one you could do yourself
#' through the formula syntax. The idea is that while the within effect and
#' predicting change is great, sometimes you want to really drill down on how
#' people that are *generally* high or low on the construct differ from each
#' other. The "stability" specification creates interaction terms with the
#' individual level means and the time variable, giving you something like a
#' growth curve model but with the particular question of whether the growth
#' trend depends on the average level of a time-varying variable. This can be
#' particularly informative when you are concerned that your time-varying
#' variable changes so infrequently that there just isn't enough variation
#' to glean anything from the within effect.
#'
#' @examples
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#' model <- wbm(lwage ~ lag(union) + wks | blk + fem | blk * lag(union),
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
#' @export
#' @rdname wbm
#' @seealso [wbm_stan()] for a Bayesian estimation option.
#' @importFrom stats as.formula gaussian terms

wbm <- function(formula, data, id = NULL, wave = NULL,
                model = "w-b", use.wave = FALSE, wave.factor = FALSE,
                min.waves = 2, family = gaussian, dynamic = FALSE,
                pR2 = FALSE, pvals = TRUE, weights = NULL, ...) {

  # Get data prepped
  if (class(data)[1] == "panel_data") {
    id <- "id"
    wave <- "wave"
  }

  wave <- as.character(substitute(wave))

  id <- as.character(substitute(id))

  if (!("data.frame" %in% class(data))) {
    stop("data argument must be a data frame.")
  }

  data <- panel_data(data, id = id, wave = wave)

  # Make sure lme4 is installed
  if (requireNamespace("lme4", quietly = TRUE) == FALSE) {
    stop("You must have the lme4 package installed to use this function.")
  }

  ## Dealing with formula
  formula <- as.formula(substitute(quote(formula)))
  if (as.character(formula[[1]]) != "~") {
    stop("Invalid formula. Include the outcome variable on the left side
         followed by `~` and then the predictors.")
  }

  weights <- as.character(substitute(weights))
  if (length(weights) == 0) {
    weights <- NULL
  }

  dv <- as.character(formula[[2]])
  formula <- as.character(formula)[[3]]
  # Pass to helper function
  pf <- wb_formula_parser(formula, dv)

  # Create formula to pass to model_frame
  mf_form <- paste(paste0(dv, " ~ "),
                   paste(pf$allvars, collapse = " + "),
                   " + ",
                   paste(pf$meanvars, collapse = " + "))
  # Add weights to keep it in the DF
  if (!is.null(weights)) {
    mf_form <- paste(mf_form, "+", weights)
  }
  # Add lagged DV
  if (dynamic == TRUE) {
    mf_form <- paste0(mf_form, " + lag(", dv, ")", " + imean(", dv, ")")
  }

  # Pass to special model_frame function that respects tibble groupings
  data <- model_frame(as.formula(mf_form), data = data)

  data <- complete_cases(data, min.waves = min.waves)
  num_distinct <- length(unique(data$id))
  maxwave <- max(data["wave"])
  minwave <- min(data["wave"])
  if (!is.null(weights)) {
    weights <- data[[weights]]
  }

  e <- wb_model(model, pf, dv, data, dynamic)

  data <- e$data

  if (use.wave == TRUE) {
    e$fin_formula <- paste(e$fin_formula, "+", "wave")
  }

  if (wave.factor == TRUE) {
    data[,wave] <- as.factor(data[,wave])
  }

  if (dynamic == TRUE) {
    e$fin_formula <- paste0(e$fin_formula, " + lag(", dv, ")")
  }

  if (pf$conds > 2) {
    res <- lme4::findbars(as.formula(paste("~", pf$cross_ints_form)))
    refs <- lme4::mkReTrms(res, data)$cnms

    if (any(names(refs) == "id")) {

      inds <- which(names(refs) == "id")

      if (any(unlist(refs[inds]) == "(Intercept)")) {
        no_add <- TRUE
      } else {
        no_add <- FALSE
      }

    } else {
      no_add <- FALSE
    }

  } else {
    no_add <- FALSE
  }

  if (no_add == FALSE) {
    e$fin_formula <- paste0(e$fin_formula, " + (1 | id)")
  }

  fin_formula <- formula_ticks(e$fin_formula, c(pf$varying, pf$meanvars,
                                               pf$constants))

  fin_formula <- as.formula(fin_formula)

  int_indices <- which(attr(terms(fin_formula), "order") >= 2)
  ints <- attr(terms(fin_formula),"term.labels")[int_indices]
  ints <- ints[!(ints %in% e$stab_terms)]
  unbt_ints <- gsub("`", "", ints, fixed = TRUE)
  ints <- ints[!(unbt_ints %in% e$stab_terms)]

  if (as.character(substitute(family)) == "gaussian") {

    fit <- lme4::lmer(fin_formula, data = data,
                        weights = weights, ...)

  } else if (as.character(substitute(family)) == "negbinomial") {

    fit <- lme4::glmer.nb(fin_formula, data = data,
                            weights = weights, ...)

  } else {

    fit <- lme4::glmer(fin_formula, data = data, family = family,
                         weights = weights, ...)

  }

  # Getting jtools summary info so it isn't re-run every time summary()
  # is called
  t0 <- Sys.time()
  j <- suppressMessages(jtools::j_summ(fit, pvals = pvals, r.squared = pR2))
  t1 <- Sys.time()
  if (t1 - t0 > 5) {
    message("If wbm is taking too long to run, you can try setting ",
            "pvals = FALSE or pR2 = FALSE.")
  }

  j2 <- attributes(j)

  out <- list(model = fit, data = data, fin_formula = fin_formula)

  out <- structure(out, dv = dv, id = id, wave = wave,
              num_distinct = num_distinct,
              varying = pf$varying, model = model,
              stab_terms = e$stab_terms,
              max_wave = maxwave, min_wave = minwave, ints = ints,
              pvals = pvals, pR2 = pR2, jsumm = j, jatts = j2,
              dynamic = dynamic)

  class(out) <- "wbm"

  return(out)

}

#' @export
#' @importFrom stats family

summary.wbm <- function(object, ...) {

  dots <- list(...)
  if ("digits" %in% names(dots)) {
    digits <- digits
  } else {
    digits <- getOption("jtools-digits", 2)
  }

  x <- object

  x2 <- attributes(x)
  j <- x2$jsumm
  j2 <- x2$jatts

  entity_icc <- j$gvars[j$gvars[,"Group"] == "id",]
  entity_icc <- as.numeric(entity_icc["ICC"])
  entity_icc <- round(entity_icc, digits)

  mod_info <- paste0("MODEL INFO:\n",
                    "Entities: ", lme4::ngrps(x$model), "\n",
                    "Time periods: ", paste0(x2$min_wave, "-",
                                            x2$max_wave), "\n",
                    "Dependent variable: ", x2$dv, "\n")
  if (family(x$model)$family == "gaussian") {
    mod_info <- paste0(mod_info, "Model type: Linear mixed effects\n")
  } else {
    mod_info <- paste(mod_info, "Model family: ", family(x$model)$family,
        ", Link: ", family(x$model)$link, "\n", sep = "")
  }

  # Name the model
  est_name <- x2$model
  if (x2$model == "w-b") {est_name <- "within-between"}
  if (est_name == "random") {est_name <- "between"}

  est_info <- paste("Specification: ", est_name, "\n\n", sep = "")

  mod_fit <- paste("MODEL FIT: ",
      "\n", "AIC = ", round(j2$aic, j2$digits),
      ", BIC = ", round(j2$bic, j2$digits), "\n", sep = "")
  if (x2$pR2 == TRUE) {
    mod_fit <- paste0(mod_fit, "Pseudo R-squared (fixed effects) = ",
          round(j2$rsq[1], j2$digits),
        "\n", "Pseudo R-squared (total) = ",
        round(j2$rsq[2], j2$digits), "\n\n")
  } else {
    mod_fit <- paste(mod_fit, "\n")
  }

  if (x2$pvals == TRUE) {
    ps <- j$coeftable[,"p"]
  }
  coefs <- round(j$coeftable, digits)
  rownames(coefs) <- gsub("`", "", rownames(coefs), fixed = TRUE)
  if (!is.null(x2$ints)) {
    x2$ints <- gsub("`", "", x2$ints, fixed = TRUE)
  }

  varying <- x2$varying
  if (est_name == "within") {
    varying <- c("(Intercept)", varying)
  }
  if (x2$dynamic == TRUE) {
    varying <- c(varying, paste0("lag(", x2$dv, ")"))
  }

  if (x2$pvals == TRUE) {

    cnames <- colnames(coefs)
    coefs <- cbind(coefs, rep(0, nrow(coefs)))
    colnames(coefs) <- c(cnames, "")

    sigstars <- c()
    for (y in 1:nrow(coefs)) {
      if (ps[y] > 0.1) {
        sigstars[y] <- ""
      } else if (ps[y] <= 0.1 & ps[y] > 0.05) {
        sigstars[y] <- "."
      } else if (ps[y] > 0.01 & ps[y] <= 0.05) {
        sigstars[y] <- "*"
      } else if (ps[y] > 0.001 & ps[y] <= 0.01) {
        sigstars[y] <- "**"
      } else if (ps[y] <= 0.001) {
        sigstars[y] <- "***"
      }
    }

    coefs[,5] <- sigstars
    coefs <- as.table(coefs)

  } else {

    coefs <- as.table(coefs)

  }

  rows <- rownames(coefs)

  if (length(varying) > 0 & est_name != "between") {

    if (length(varying) == 1) {

      # Can't have single row table
      vec <- coefs[rownames(coefs) %in% varying,]
      vec <- t(as.matrix(vec))
      rownames(vec) <- varying

      within_table <- as.table(vec)

    } else {

      within_table <- coefs[rownames(coefs) %in% varying,]

    }

    coefs <- coefs[!(rownames(coefs) %in% varying),]
    rows <- rows[!(rows %in% varying)]

    if (length(x2$stab_terms) > 0) {

      stabs <- coefs[rownames(coefs) %in% x2$stab_terms,]
      coefs <- coefs[!(rownames(coefs) %in% x2$stab_terms),]
      rows <- rows[!(rows %in% x2$stab_terms)]

    }

  } else {

    within_table <- NULL

  }

  if (length(x2$ints) > 0) {

    if (length(x2$ints) == 1) {# Can't have single row table

      vec <- coefs[rownames(coefs) %in% x2$ints,]
      vec <- t(as.matrix(vec))
      rownames(vec) <- x2$ints

      ints_table <- as.table(vec)

    } else {

      ints_table <- coefs[rownames(coefs) %in% x2$ints,]

    }

    coefs <- coefs[!(rownames(coefs) %in% x2$ints),]
    rows <- rows[!(rows %in% x2$ints)]

  } else {

    ints_table <- NULL

  }

  if (is.null(nrow(coefs)) && est_name != "within") { # Can't have single row

    if (dim(coefs)[1] > 0) {
      vec <- coefs
      vec <- t(as.matrix(vec))
      rownames(vec) <- rows

      between_table <- as.table(vec)
    } else {
      between_table <- NULL
    }

  } else if (est_name != "within") {

    between_table <- as.table(coefs)

  } else {

    between_table <- NULL

  }

  if (x2$model == "stability") {

    if (length(x2$stab_terms) == 1) { # Can't have single row table

      vec <- stabs
      vec <- t(as.matrix(vec))
      rownames(vec) <- x2$stab_terms

      time_trends <- as.table(vec)

    } else {

      time_trends <- stabs

    }

  } else {

    time_trends <- NULL

  }

  if (lme4::isLMM(x$model) == TRUE && j2$pvals == TRUE) {

    if (j2$pbkr == TRUE) {

      df_msg <- paste("p values calculated using Kenward-Roger df =",
                      round(j2$df, digits), "\n")

    } else {

      df_msg <- paste("p values calculated using df =",
                      round(j2$df, digits), "\n")

    }

  } else {

    df_msg <- NULL

  }

  j$rcoeftable[,"Std.Dev."] <-
    as.character(round(as.numeric(j$rcoeftable[,"Std.Dev."]), digits))
  the_table <- as.table(j$rcoeftable)
  rownames(the_table) <- rep("", nrow(the_table))
  ranef_table <- the_table

  out <- list(ranef_table = ranef_table, time_trends = time_trends,
              within_table = within_table, between_table = between_table,
              entity_icc = entity_icc, mod_info = mod_info, mod_fit = mod_fit,
              model = x2$model, est_name = est_name,
              est_info = est_info, ints_table = ints_table, df_msg = df_msg)
  class(out) <- "summary.wbm"
  return(out)

}

#' @export

print.summary.wbm <- function(x, ...) {

  cat(x$mod_info)

  cat(x$est_info)

  cat(x$mod_fit)

  if (x$est_name != "between" & !is.null(x$within_table)) {

    cat("WITHIN EFFECTS:\n")
    print(x$within_table)
    cat("\n")

    cat("Within-entity ICC =", x$entity_icc, "\n\n")

  }

  if (x$est_name != "contextual" & !is.null(x$between_table)) {

    cat("BETWEEN EFFECTS:\n")
    print(x$between_table)
    cat("\n")

  } else if (x$est_name == "contextual" & !is.null(x$between_table)) {

    cat("CONTEXTUAL EFFECTS:\n")
    print(x$between_table)
    cat("\n")

  }

  if (x$model == "stability") {

    cat("BETWEEN-ENTITY TIME TRENDS:\n")
    print(x$time_trends)
    cat("\n")
  }

  if (!is.null(x$ints_table)) {

    cat("INTERACTIONS:\n")
    print(x$ints_table)
    cat("\n")

  }

  if (!is.null(x$df_msg)) {

    cat(x$df_msg, "\n")

  }

  cat("RANDOM EFFECTS:\n")
  print(x$ranef_table)

}


# wlm <- function(formula, data, id = NULL, wave = NULL,
#                 use.wave = FALSE, wave.factor = FALSE,
#                 min.waves = 2, family = gaussian,
#                 pvals = TRUE, weights = NULL, ...) {
#
#
#   model <- "within" # it's just always within now
#
#   # Get data prepped
#   if (class(data)[1] == "panel_data") {
#     id <- "id"
#     wave <- "wave"
#   }
#
#   wave <- as.character(substitute(wave))
#
#   id <- as.character(substitute(id))
#
#   if (!("data.frame" %in% class(data))) {
#     stop("data argument must be a data frame.")
#   }
#
#   data <- panel_data(data, id = id, wave = wave)
#
#   # Make sure lme4 is installed
#   if (requireNamespace("lme4", quietly = TRUE) == FALSE) {
#     stop("You must have the lme4 package installed to use this function.")
#   }
#
#   ## Dealing with formula
#   formula <- as.formula(substitute(quote(formula)))
#   if (as.character(formula[[1]]) != "~") {
#     stop("Invalid formula. Include the outcome variable on the left side
#          followed by `~` and then the predictors.")
#   }
#
#   weights <- as.character(substitute(weights))
#   if (length(weights) == 0) {
#     weights <- NULL
#   }
#
#   dv <- as.character(formula[[2]])
#   formula <- as.character(formula)[[3]]
#   # Pass to helper function
#   pf <- wb_formula_parser(formula, dv)
#
#   # Create formula to pass to model_frame
#   mf_form <- paste(paste0(dv, " ~ "),
#                    paste(pf$allvars, collapse = " + "),
#                    " + ",
#                    paste(pf$meanvars, collapse = " + "), " + ",
#                    paste0("imean(",dv,")"))
#   # Add weights to keep it in the DF
#   if (!is.null(weights)) {
#     mf_form <- paste(mf_form, "+", weights)
#   }
#   # Pass to special model_frame function that respects tibble groupings
#   data <- model_frame(as.formula(mf_form), data = data)
#   data[dv] <- data[dv] - data[paste0("imean(",dv,")")]
#
#   data <- complete_cases(data, min.waves = min.waves)
#   num_distinct <- length(unique(data$id))
#   maxwave <- max(data["wave"])
#   minwave <- min(data["wave"])
#   if (!is.null(weights)) {
#     weights <- data[[weights]]
#   }
#
#   e <- wb_model(model, pf, dv, data)
#
#   data <- e$data
#
#   if (use.wave == TRUE) {
#     e$fin_formula <- paste(e$fin_formula, "+", "wave")
#   }
#
#   if (wave.factor == TRUE) {
#     data[,wave] <- as.factor(data[,wave])
#   }
#
#   fin_formula <- formula_ticks(e$fin_formula, pf$varying)
#   fin_formula <- paste(fin_formula, "- 1")
#
#   fin_formula <- as.formula(fin_formula)
#
#   model <- lm(fin_formula, data = data, weights = weights, ...)
#
#
#   # Getting jtools summary info so it isn't re-run every time summary()
#   # is called
#   j <- jtools::j_summ(model, pvals = pvals)
#   j2 <- attributes(j)
#
#   out <- list(model = model, data = data, fin_formula = fin_formula)
#
#   out <- structure(out, dv = dv, id = id, wave = wave,
#                    num_distinct = num_distinct,
#                    varying = pf$varying, model = model,
#                    max_wave = maxwave, min_wave = minwave,
#                    pvals = pvals, jsumm = j, jatts = j2)
#
#   class(out) <- "wlm"
#
#   return(out)
#
# }

# summary.wlm <- function(x, digits = getOption("jtools-digits", 2), ...) {
#
#   x2 <- attributes(x)
#   j <- x2$jsumm
#   j2 <- x2$jatts
#
#   mod_info <- paste0("MODEL INFO:\n",
#                      # "Entities: ", lme4::ngrps(x$model), "\n",
#                      "Time periods: ", paste0(x2$min_wave, "-",
#                                               x2$max_wave), "\n",
#                      "Dependent variable: ", x2$dv, "\n")
#
#   mod_info <- paste0(mod_info, "Model type: OLS\n")
#
#   # Name the model
#   est_name <- x2$model
#
#   est_info <- paste("model: ", est_name, "\n\n", sep = "")
#
#   mod_fit <- paste0("MODEL FIT: ",
#                    "\n", "R-squared = ", round(j2$rsq, j2$digits), "\n\n")
#
#
#   if (x2$pvals == TRUE) {
#     ps <- j$coeftable[,"p"]
#   }
#   coefs <- round(j$coeftable, digits)
#   rownames(coefs) <- gsub("`", "", rownames(coefs), fixed = TRUE)
#
#   varying <- x2$varying
#   if (est_name == "within") {
#     varying <- c("(Intercept)", varying)
#   }
#
#   if (x2$pvals == TRUE) {
#
#     cnames <- colnames(coefs)
#     coefs <- cbind(coefs, rep(0, nrow(coefs)))
#     colnames(coefs) <- c(cnames, "")
#
#     sigstars <- c()
#     for (y in 1:nrow(coefs)) {
#       if (ps[y] > 0.1) {
#         sigstars[y] <- ""
#       } else if (ps[y] <= 0.1 & ps[y] > 0.05) {
#         sigstars[y] <- "."
#       } else if (ps[y] > 0.01 & ps[y] <= 0.05) {
#         sigstars[y] <- "*"
#       } else if (ps[y] > 0.001 & ps[y] <= 0.01) {
#         sigstars[y] <- "**"
#       } else if (ps[y] <= 0.001) {
#         sigstars[y] <- "***"
#       }
#     }
#
#     coefs[,5] <- sigstars
#     coefs <- as.table(coefs)
#
#   } else {
#
#     coefs <- as.table(coefs)
#
#   }
#
#   rows <- rownames(coefs)
#
#   if (length(varying) == 1) {
#
#     # Can't have single row table
#     vec <- coefs[rownames(coefs) %in% varying,]
#     vec <- t(as.matrix(vec))
#     rownames(vec) <- varying
#
#     within_table <- as.table(vec)
#
#   } else {
#
#     within_table <- coefs[rownames(coefs) %in% varying,]
#
#   }
#
#   out <- list(within_table = within_table, mod_info = mod_info,
#               mod_fit = mod_fit,
#               model = x2$model, est_name = est_name,
#               est_info = est_info)
#   class(out) <- "summary.wlm"
#   return(out)
#
# }


# print.summary.wlm <- function(x, ...) {
#
#   cat(x$mod_info)
#
#   cat(x$est_info)
#
#   cat(x$mod_fit)
#
#   if (x$est_name != "between" & !is.null(x$within_table)) {
#
#     cat("WITHIN EFFECTS:\n")
#     print(x$within_table)
#     cat("\n")
#
#   }
#
#
#
# }




# wb_lme <- function(formula, data, id = NULL, wave = NULL, model = "w-b",
#                    use.wave = TRUE, wave.factor = FALSE,
#                    min.waves = 2, model.cor = TRUE, weights = NULL) {
#
#   # Get data prepped
#   if (class(data)[1] == "panel_data") {
#     id <- "id"
#     wave <- "wave"
#   }
#
#   wave <- as.character(substitute(wave))
#
#   id <- as.character(substitute(id))
#
#   if (!("data.frame" %in% class(data))) {
#     stop("data argument must be a data frame.")
#   }
#
#   data <- panel_data(data, id = id, wave = wave)
#
#   # Make sure lme4 is installed
#   if (requireNamespace("lme4", quietly = TRUE) == FALSE) {
#     stop("You must have the lme4 package installed to use this function.")
#   }
#
#   ## Dealing with formula
#   formula <- as.formula(substitute(quote(formula)))
#   if (as.character(formula[[1]]) != "~") {
#     stop("Invalid formula. Include the outcome variable on the left side
#          followed by `~` and then the predictors.")
#   }
#
#   weights <- as.character(substitute(weights))
#   if (length(weights) == 0) {
#     weights <- NULL
#   }
#
#   dv <- as.character(formula[[2]])
#   formula <- as.character(formula)[[3]]
#   # Pass to helper function
#   pf <- wb_formula_parser(formula, dv)
#
#   # Create formula to pass to model_frame
#   mf_form <- paste(paste0(dv, " ~ "),
#                    paste(pf$allvars, collapse = " + "),
#                    " + ",
#                    paste(pf$meanvars, collapse = " + "))
#   # Add weights to keep it in the DF
#   if (!is.null(weights)) {
#     mf_form <- paste(mf_form, "+", weights)
#   }
#   # Pass to special model_frame function that respects tibble groupings
#   data <- model_frame(as.formula(mf_form), data = data)
#
#   data <- complete_cases(data, min.waves = min.waves)
#   num_distinct <- length(unique(data$id))
#   maxwave <- max(data["wave"])
#   minwave <- min(data["wave"])
#   if (!is.null(weights)) {
#     weights <- data[[weights]]
#   }
#
#   e <- wb_model(model, pf, dv, data)
#
#   data <- e$data
#
#   if (use.wave == TRUE) {
#     e$fin_formula <- paste(e$fin_formula, "+", "wave")
#   }
#
#   if (wave.factor == TRUE) {
#     data[,wave] <- as.factor(data[,wave])
#   }
#
#   fin_formula <- formula_esc(e$fin_formula, c(pf$varying, pf$meanvars,
#                                                 pf$constants))
#
#   names(data) <- make.names(names(data))
#   fin_formula <- as.formula(fin_formula)
#
#   int_indices <- which(attr(terms(fin_formula), "order") >= 2)
#   ints <- attr(terms(fin_formula),"term.labels")[int_indices]
#   ints <- ints[!(ints %in% e$stab_terms)]
#   unbt_ints <- gsub("`", "", ints, fixed = TRUE)
#   ints <- ints[!(unbt_ints %in% e$stab_terms)]
#
#   rand_form <- as.formula(paste("~ 1 |", id))
#   cor_form <- as.formula(paste("~", wave, "|", id))
#
#   cont <- nlme::lmeControl(maxIter = 1000, msMaxIter = 1000, niterEM = 1000)
#
#   if (model.cor == TRUE) {
#     model <- nlme::lme(fin_formula, random = rand_form,
#                        nlme::corAR1(form = cor_form), data = data,
#                        na.action = na.omit, control = cont)
#   } else {
#     model <- nlme::lme(fin_formula, random = rand_form, data = data,
#                        na.action = na.omit, control = cont)
#   }
#
#   out <- list(model = model, data = data, fin_formula = fin_formula,
#               dv = dv, id = id, wave = wave,
#               model_cor = model.cor, num_distinct = num_distinct,
#               varying = pf$varying, model = model,
#               stab_terms = e$stab_terms)
#
#   class(out) <- "wb_lme"
#
#   return(out)
#
#   }



# summary.wb_lme <- function(x, ...) {
#
#   cat("MODEL INFO:\n")
#   cat("Entities:", x$model$dims$ngrps[x$id], "\n")
#   cat("Time periods:", length(unique(x$data[,x$wave])), "\n")
#   cat("Dependent variable:", x$dv, "\n")
#   cat("Model type: Linear mixed effects\n")
#
#   # Name the model
#   est_name <- x$model
#   if (x$model == "w-b") {est_name <- "within-between"}
#   if (x$model == "stability") {
#     est_name <- "within-between with between-entity time trends"
#   }
#   cat("model: ", est_name, "\n\n", sep = "")
#
#   cat("MODEL FIT:\n")
#   cat("AIC =", summary(x$model)$AIC, "\n")
#   cat("BIC =", summary(x$model)$BIC, "\n\n")
#
#   coefs <- summary(x$model)$tTable
#   coefs <- coefs[,c("Value","Std.Error","t-value","p-value")]
#   pvals <- coefs[,"p-value"]
#   coefs <- round(coefs, 3)
#   coefs <- cbind(coefs, rep(0, nrow(coefs)))
#   colnames(coefs) <- c("Est.", "S.E.", "t-value", "p", "")
#
#   sigstars <- c()
#   for (y in 1:nrow(coefs)) {
#     if (pvals[y] > 0.1) {
#       sigstars[y] <- ""
#     } else if (pvals[y] <= 0.1 & pvals[y] > 0.05) {
#       sigstars[y] <- "."
#     } else if (pvals[y] > 0.01 & pvals[y] <= 0.05) {
#       sigstars[y] <- "*"
#     } else if (pvals[y] > 0.001 & pvals[y] <= 0.01) {
#       sigstars[y] <- "**"
#     } else if (pvals[y] <= 0.001) {
#       sigstars[y] <- "***"
#     }
#   }
#
#   coefs[,5] <- sigstars
#   coefs <- as.table(coefs)
#   rows <- rownames(coefs)
#
#   if (length(x$varying) > 0 & est_name != "random") {
#
#     cat("WITHIN EFFECTS:\n")
#     if (length(x$varying) == 1) { # Can't have single row table
#
#       vec <- coefs[rownames(coefs) %in% x$varying,]
#       vec <- t(as.matrix(vec))
#       rownames(vec) <- x$varying
#
#       print(as.table(vec))
#
#     } else {
#
#       print(coefs[rownames(coefs) %in% x$varying,])
#
#     }
#     cat("\n")
#
#     coefs <- coefs[!(rownames(coefs) %in% x$varying),]
#     rows <- rows[!(rows %in% x$varying)]
#
#     if (length(x$stab_terms) > 0) {
#
#       stabs <- coefs[rownames(coefs) %in% x$stab_terms,]
#       coefs <- coefs[!(rownames(coefs) %in% x$stab_terms),]
#       rows <- rows[!(rows %in% x$stab_terms)]
#
#     }
#
#   }
#
#   if (est_name != "random") { cat("BETWEEN EFFECTS:\n") }
#   if (is.null(nrow(coefs))) { # Can't have single row table
#
#     vec <- coefs
#     vec <- t(as.matrix(vec))
#     rownames(vec) <- rows
#
#     print(as.table(vec))
#
#   } else {
#
#     print(as.table(coefs))
#
#   }
#   cat("\n")
#
#   if (x$model == "stability") {
#
#     cat("BETWEEN-ENTITY TIME TRENDS:\n")
#     if (length(x$stab_terms) == 1) { # Can't have single row table
#
#       vec <- stabs
#       vec <- t(as.matrix(vec))
#       rownames(vec) <- x$stab_terms
#
#       print(as.table(vec))
#
#     } else {
#
#       print(stabs)
#
#     }
#
#
#
#     cat("\n")
#
#   }
#
#   if (x$model_cor == TRUE) {
#     ar1 <- coef(x$model$modelStruct$corStruct, unconstrained = FALSE)
#     cat("Autocorrelation estimate =", round(ar1,3), "\n")
#   }
#
# }

