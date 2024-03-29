#' @title Panel regression models fit via multilevel modeling
#' @description Fit "within-between" and several other regression variants
#'   for panel data in a multilevel modeling framework.
#' @param formula Model formula. See details for crucial
#'   info on `panelr`'s formula syntax.
#' @param data The data, either a `panel_data` object or `data.frame`.
#' @param id If `data` is not a `panel_data` object, then the name of the
#'   individual id column as a string. Otherwise, leave as NULL, the default.
#' @param wave If `data` is not a `panel_data` object, then the name of the
#'   panel wave column as a string. Otherwise, leave as NULL, the default.
#' @param model One of `"w-b"`, `"within"`, `"between"`,
#'   `"contextual"`. See details for more on these options.
#' @param detrend Adjust within-subject effects for trends in the predictors?
#'   Default is FALSE, but some research suggests this is a better idea 
#'   (see Curran and Bauer (2011) reference).
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
#' @param balance.correction Correct between-subject effects for unbalanced 
#'   panels following the procedure in Curran and Bauer (2011)? Default is 
#'   FALSE.
#' @param dt.random Should the detrending procedure be performed with a
#'   random slope for each entity? Default is TRUE but for short panels
#'   FALSE may be better, fitting a trend for all entities.
#' @param dt.order If detrending using `detrend`, what order polynomial 
#'   would you like to specify for the relationship between time and the
#'   predictors? Default is 1, a linear model.
#' @param pR2 Calculate a pseudo R-squared? Default is TRUE, but in some cases
#'   may cause errors or add computation time.
#' @param pvals Calculate p values? Default is TRUE but for some complex
#'   linear models, this may take a long time to compute using the `pbkrtest`
#'   package.
#' @param t.df For linear models only. User may choose the method for 
#'   calculating the degrees of freedom in t-tests. Default is 
#'   `"Satterthwaite"`, but you may also choose `"Kenward-Roger"`. 
#'   Kenward-Roger standard errors/degrees of freedom requires the `pbkrtest`
#'   package. 
#' @param weights If using weights, either the name of the column in the data
#'   that contains the weights or a vector of the weights.
#'   
#' @param dt_random Deprecated. Equivalent to `dt.random`.
#' @param dt_order Deprecated. Equivalent to `dt.order`.
#' @param balance_correction Deprecated. Equivalent to `balance.correction`.
#'   
#' @inheritParams lme4::glmer
#' 
#' @param ... Additional arguments provided to [lme4::lmer()],
#'   [lme4::glmer()], or [lme4::glmer.nb()].
#'   
#' @param interaction.style The best way to calculate interactions in within
#'  models is in some dispute. The conventional way (`"demean"`) is to first
#'  calculate the product of the variables involved in the interaction before
#'  those variables have their means subtracted and then subtract the mean of 
#'  the product from the product term (see Schunk and Perales (2017)). 
#'  Giesselmann and Schmidt-Catran (2020) show this method carries 
#'  between-entity differences that within models are designed to model out.
#'  They suggest an alternate method (`"double-demean"`) in which the product
#'  term is first calculated using the de-meaned lower-order variables and 
#'  then the subject means are subtracted from this product term. Another 
#'  option is to simply use the product term of the de-meaned variables
#'  (`"raw"`), but Giesselmann and Schmidt-Catran (2020) show this method 
#'  biases the results towards zero effect. The default is `"double-demean"`
#'  but if emulating other software is the goal, `"demean"` might be 
#'  preferred.
#'     
#'
#' @inheritParams jtools::scale_mod
#' @inheritParams jtools::summ.merMod
#' 
#' @return A `wbm` object, which inherits from `merMod`.
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
#' `lwage ~ union | blk`
#'
#' Put time-varying variables before the first `|` and time-invariant
#' variables afterwards. You can specify lags like `lag(union)` for time-varying
#' variables; for more than 1 lag, include the number: `lag(union, 2)`.
#'
#' After the first `|` go the time-invariant variables. Note that if you put a
#' time-varying variable here, what you get is the observed value rather than
#' one adjusted to isolate within-entity effects. You may also take a 
#' time-varying variable --- let's say weeks worked (`wks`) --- and use
#' `imean(wks)` to include the individual's mean across all waves as a 
#' predictor while omitting the per-wave measures.
#'
#' There is also a place for a second `|`. Here you can specify cross-level
#' interactions (within-level interactions can be specified here as well).
#' If I wanted the interaction term for `union` and `blk` --- to see whether
#' the effect of union status depended on one's race --- I would specify the
#' formula this way:
#'
#' `lwage ~ union | blk | union * blk`
#'
#' Another use for the post-second `|` section of the formula is for changing
#' the random effects specification. By default, only a random intercept is
#' specified in the call to [lme4::lmer()]/[lme4::glmer()]. If you would like
#' to specify other random slopes, include them here using the typical `lme4`
#' syntax:
#'
#' `lwage ~ union | blk | (union | id)`
#' 
#' You can also include the wave variable in a random effects term to 
#' specify a latent growth curve model:
#' 
#' `lwage ~ union | blk + t | (t | id)`
#'
#' One last thing to know: If you want to use the second `|` but not the first,
#' put a 1 or 0 after the first, like this:
#'
#' `lwage ~ union | 1 | (union | id)`
#'
#' Of course, with no time-invariant variables, you need no `|` operators at
#' all.
#'
#' **Models**
#'
#' As a convenience, `wbm` does the heavy lifting for specifying the
#' within-between model correctly. As a side effect it only
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
#' Giesselmann, M., & Schmidt-Catran, A. (2018). Interactions in fixed effects
#' regression models (Discussion Papers of DIW Berlin No. 1748). 
#' *DIW Berlin, German Institute for Economic Research*. 
#' Retrieved from https://ideas.repec.org/p/diw/diwwpp/dp1748.html
#'
#' Schunck, R., & Perales, F. (2017). Within- and between-cluster effects in
#' generalized linear mixed models: A discussion of approaches and the 
#' `xthybrid` command. *The Stata Journal*, *17*, 89–115. 
#' https://doi.org/10.1177/1536867X1701700106
#'
#' @export
#' @import jtools
#' @rdname wbm
#' @seealso [wbm_stan()] for a Bayesian estimation option.
#' @importFrom stats as.formula gaussian terms confint drop.terms reformulate
#' @importFrom stats model.matrix

wbm <- function(formula, data, id = NULL, wave = NULL,
                model = "w-b", detrend = FALSE, use.wave = FALSE,
                wave.factor = FALSE, min.waves = 2, family = gaussian,
                balance.correction = FALSE, dt.random = TRUE, dt.order = 1,
                pR2 = TRUE, pvals = TRUE, t.df = "Satterthwaite", 
                weights = NULL, offset = NULL, 
                interaction.style = c("double-demean", "demean", "raw"),
                scale = FALSE, scale.response = FALSE, n.sd = 1,
                dt_random = dt.random, dt_order = dt.order,
                balance_correction = balance.correction, ...) {
  
  the_call <- match.call()
  the_call[[1]] <- substitute(wbm)
  the_env <- parent.frame()
  
  if (any(c(detrend, balance_correction))) {
    if (!requireNamespace("tidyr") | !requireNamespace("purrr")) {
      stop_wrap("To use the 'detrend' or 'balance_correction' arguments, you 
                must have the 'tidyr' and 'purrr' packages installed.")
    }
  }
  
  # Deal with deprecated arguments
  if (dt_random != dt.random) dt.random <- dt_random
  if (dt_order != dt.order) dt.order <- dt_order
  if (balance_correction != balance.correction) {
    balance.correction <- balance_correction
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
  fin_formula <-  prepare_lme4_formula(e$fin_formula, pf, data, use.wave, wave,
                                       id, c(e$int_means, e$within_ints),
                                       e$cross_ints, dv)  
  
  if (!is.null(offset)) {
    offset[!is.finite(offset)] <- NA
  }

  # Conditionally choose lme4 function based on family argument
  if (as.character(substitute(family))[1] == "gaussian") {
      fit <- lme4::lmer(fin_formula, data = data, weights = weights,
                        offset = offset, ...)
  } else if (as.character(substitute(family))[1] == "negbinomial") {
      fit <- lme4::glmer.nb(fin_formula, data = data, weights = weights,
                            offset = offset, ...)
  } else {
      fit <- lme4::glmer(fin_formula, data = data, family = family,
                          weights = weights, offset = offset, ...)
  }

  # Getting jtools summary info so it isn't re-run every time summary()
  # is called
  dont_use_satterthwaite <- 
    as.character(substitute(family))[1] == "gaussian" &&
      (lme4::isREML(fit) == FALSE | check_lmerModTest(fit)) 
  if (dont_use_satterthwaite == TRUE & t.df == "Satterthwaite") {
    t.df <- "residual"
    sfit <- fit
  } else {
    if (lme4::isLMM(fit) & t.df == "Satterthwaite") {
      t.df <- NULL
      sfit <- lmerTest::as_lmerModLmerTest(fit)
    } else sfit <- fit
  }
  t0 <- Sys.time()
  j <- suppressMessages(
    jtools::j_summ(sfit, pvals = pvals, r.squared = pR2, t.df = t.df))
  t1 <- Sys.time()
  if (t1 - t0 > 5) {
    msg_wrap("If wbm is taking too long to run, you can try setting 
             pvals = FALSE.")
  }
  # Need to take vars out of call so I can update the summ object outside this
  # environment
  jcall <- getCall(j)
  jcall$pvals <- substitute(pvals)
  jcall$r.squared <- substitute(pR2)
  jcall$t.df <- substitute(t.df)
  attr(j, "call") <- jcall
  # check if pseudo-R2 calculation failed
  if (is.na(attr(j, "rsqs")[1]) | length(attr(j, "rsqs")) == 0) pR2 <- FALSE
  
  ints <- e$cross_ints

  j2 <- attributes(j)
  # Drop redundant model from the summ object
  # j$model <- NULL
  # class(j) <- c("summ.wbm", class(j))

  merMod_call <- getCall(fit)
  terms <- attr(fit@frame, "terms")

  if (lme4::isLMM(fit)) {
    out <- as(object = fit, Class = "wblm")
  } else {
    out <- as(object = fit, Class = "wbglm")
  }

  out@orig_data <- prepped$orig_data
  if (wave %nin% all.vars(fin_formula)) {
    data <- data[names(data) %nin% wave]
  } 
  out@frame <- as.data.frame(data)
  attr(out@frame, "terms") <- terms 
  attr(out@frame, "formula") <- formula(fit)
  attr(out@frame, "wave") <- prepped$wave
  attr(out@frame, "id") <- prepped$id

  out@call_info <- list(dv = dv, id = id, wave = wave,
              num_distinct = prepped$num_distinct,
              varying = c(pf$varying, e$within_ints), constants = pf$constants,
              meanvars = pf$meanvars, model = model,
              interactions = c(e$within_ints, ints),
              max_wave = prepped$maxwave, min_wave = prepped$minwave,
              ints = ints, pvals = pvals, pR2 = pR2, env = the_env,
              mf_form = prepped$mf_form,
              use.wave = use.wave, wave.factor = wave.factor,
              detrend = detrend, dt_order = dt_order,
              dt_random = dt_random, balance_correction = balance_correction,
              pf = pf, merMod_call = merMod_call, 
              interaction.style = interaction.style)

  out@call <- the_call
  out@summ <- j
  out@summ_atts <- j2

  return(out)

}

#' @export
#' @importFrom stats family
#' @importFrom crayon bold italic

summary.wbm <- function(object, ...) {

  dots <- list(...)
  if ("digits" %in% names(dots)) {
    digits <- dots$digits
  } else {
    digits <- getOption("jtools-digits", 2)
  }

  x <- object

  x2 <- x@call_info
  j <- x@summ
  j2 <- x@summ_atts

  entity_icc <- j$gvars[j$gvars[,"Group"] == x2$id,]
  entity_icc <- as.numeric(entity_icc["ICC"])
  entity_icc <- round(entity_icc, digits)

  mod_info <- paste0(bold("MODEL INFO:\n"),
                    italic("Entities: "), lme4::ngrps(x), "\n",
                    italic("Time periods: "), paste0(x2$min_wave, "-",
                                            x2$max_wave), "\n",
                    italic("Dependent variable: "), x2$dv, "\n")
  if (family(x)$family == "gaussian") {
    mod_info <- paste0(mod_info, italic("Model type:"), " Linear mixed effects\n")
  } else {
    mod_info <- paste(mod_info, italic("Model family: "), family(x)$family,
        ", ", italic("Link: "), family(x)$link, "\n", sep = "")
  }

  # Name the model
  est_name <- x2$model
  if (x2$model == "w-b") {est_name <- "within-between"}
  if (est_name == "random") {est_name <- "between"}
  if (est_name == "fixed") {est_name <- "within"}

  est_info <- paste(italic("Specification: "), est_name, "\n\n", sep = "")

  mod_fit <- paste(bold("MODEL FIT:"),
      "\n", italic("AIC = "), round(j2$aic, j2$digits),
      ", ", italic("BIC = "), round(j2$bic, j2$digits), "\n", sep = "")
  if (x2$pR2 == TRUE) {
    mod_fit <- paste0(mod_fit, italic("Pseudo-R\u00B2 (fixed effects) = "),
          round(j2$rsqs$Marginal, j2$digits),
        "\n", italic("Pseudo-R\u00B2 (total) = "),
        round(j2$rsqs$Conditional, j2$digits),
        italic("\nEntity ICC = "), entity_icc, "\n\n")
    
  } else {
    mod_fit <- paste(mod_fit, italic("Entity ICC = "), entity_icc, "\n\n")
  }
  
  # For glance method
  mod_info_list <- list(min_wave = x2$min_wave, max_wave = x2$max_wave,
                        N = lme4::ngrps(x)[x2$id], aic = j2$aic, bic = j2$bic,
                        pR2_fe = if (x2$pR2) j2$rsqs$Marginal else NULL,
                        pR2_total = if (x2$pR2)j2$rsqs$Conditional else NULL,
                        model = est_name)

  coefs <- j$coeftable
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

  if (lme4::isLMM(x) == TRUE & j2$pvals == TRUE) {

    if (j2$p_calc == "k-r") {
      df_msg <- italic(str_wrap("p values calculated using Kenward-Roger
                                standard errors and d.f."), "\n")
    } else if (j2$p_calc == "s") {
      df_msg <- italic("p values calculated using Satterthwaite d.f.\n")
    } else {
      df_msg <- italic(
        paste("p values calculated using df =", round(j2$df, digits), "\n")
      )
    }

  } else {
    df_msg <- NULL
  }
  
  ranef_table <- as.data.frame(j$rcoeftable)
  ranef_table[, 3] <- as.numeric(as.character(ranef_table[, 3]))

  out <- list(ranef_table = ranef_table,
              within_table = within_table, between_table = between_table,
              entity_icc = entity_icc, mod_info = mod_info, mod_fit = mod_fit,
              model = x2$model, est_name = est_name,
              est_info = est_info, ints_table = ints_table, df_msg = df_msg,
              digits = digits, mod_info_list = mod_info_list)
  class(out) <- "summary.wbm"
  return(out)

}

#' @export

print.summary.wbm <- function(x, ...) {

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

  if (!is.null(x$df_msg)) {

    cat(x$df_msg, "\n")

  }

  cat(bold("RANDOM EFFECTS:\n"))
  print(md_table(x$ranef_table, digits = x$digits, row.names = FALSE,
                 align = "c",
                 format = getOption("panelr.table.format", "multiline")))

}


#' @export 

print.wbm <- function(x, ...) {
  
  print(summary(x))
  
}

#' @title Tidy methods for `wbm` models
#' @description `panelr` provides methods to access `wbm` data in a tidy format
#' @rdname wbm_tidiers
#' @inheritParams broom.mixed::lme4_tidiers
#' @param effects A character vector including one or more of "fixed"
#' (fixed-effect parameters); "ran_pars" (variances and covariances or
#' standard deviations and correlations of random effect terms);
#' "ran_vals" (conditional modes/BLUPs/latent variable estimates); or
#' "ran_coefs" (predicted parameter values for each group, as returned by
#' `coef.merMod`.
#' @examples 
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#' model <- wbm(lwage ~ lag(union) + wks, data = wages)
#' if (requireNamespace("broom.mixed")) {
#'   broom.mixed::tidy(model)
#' }
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(generics::tidy, wbm)
#' } else {
#'   export(tidy.wbm)
#' }

tidy.wbm <- function(x, conf.int = FALSE, conf.level = .95,
                     effects = c("fixed", "ran_pars"), conf.method = "Wald",
                     ran_prefix = NULL, ...) {
  
  if (!requireNamespace("broom.mixed")) {
    stop_wrap("You must have the broom.mixed package to use tidy methods.")
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
           "t val." = "statistic",
           "z val." = "statistic",
           "S.E." = "std.error",
           "p" = "p.value",
           a)
    
  }, "a")
  names(params) <- switchv(names(params))
  
  # Getting confidence intervals if requested
  if (conf.int == TRUE) {
    ints <- as.data.frame(confint(x, level = conf.level, method = conf.method))
    # Renaming the columns to fit the tidy model
    names(ints) <- c("conf.low", "conf.high")
    # Renaming the terms to remove the backticks to match the params d.f.
    ints$term <- stringr::str_remove_all(rownames(ints), "`")
    # Put things together
    params <- dplyr::left_join(params, ints, by = "term")
  }
  # Get the random effects if requested
  if ("ran_pars" %in% effects) {
    ran_pars <- broom.mixed::tidy(as(x, switch(class(x), "wblm" = "lmerMod",
                                             "wbglm" = "glmerMod")),
                            effects = "ran_pars", conf.method = conf.method,
                            conf.level = conf.level, ran_prefix = ran_prefix,
                            ...)
    params <- dplyr::bind_rows(params, ran_pars)
  }
  return(tibble::as_tibble( # Return a tibble
    # Only return the relevant columns
    params %just% c("term", "estimate", "statistic", "std.error", 
                    "conf.low", "conf.high", "p.value", "group")
    ))
}

#' @rdname wbm_tidiers
#' @inheritParams broom.mixed::lme4_tidiers
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(generics::glance, wbm)
#' } else {
#'   export(glance.wbm)
#' }
glance.wbm <- function(x, ...) {
  sum <- summary(x)
  mod_info_list <- sum$mod_info_list
  mod_info_list[sapply(mod_info_list, is.null)] <- NA
  return(tibble::as_tibble(mod_info_list))
}

#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(jtools::summ, wbm)
#' } else {
#'   export(summ.wbm)
#' }
summ.wbm <- function(model, ...) {
  out <- update_summ(model@summ, ...)
  class(out) <- c("summ.wbm", class(out))
  out$wbm <- model
  out
}

#' @rdname wbm_tidiers
#' @inheritParams broom.mixed::lme4_tidiers
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(generics::glance, summ.wbm)
#' } else {
#'   export(glance.summ.wbm)
#' }
glance.summ.wbm <- function(x, ...) {
  glance.wbm(x$wbm)
}

#' @rdname wbm_tidiers
#' @inheritParams broom.mixed::lme4_tidiers
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(generics::tidy, summ.wbm)
#' } else {
#'   export(tidy.summ.wbm)
#' }
tidy.summ.wbm <- function(x, ...) {
  class(x) <- class(x) %not% "summ.wbm"
  generics::tidy(x, ...)
}

#' @importFrom utils getFromNamespace
update_summ <- function(summ, call.env, ...) {
  
  call <- getCall(summ)
  # Now get the argument names for that version of summ
  summ_formals <- formals(getFromNamespace(class(summ)[1], "jtools"))
  
  extras <- as.list(match.call())
  indices <- which(names(extras) %in% names(summ_formals))
  extras <- extras[indices]
  
  existing <- !is.na(match(names(extras), names(call)))
  for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
  if (any(!existing)) {
    call <- c(as.list(call), extras[!existing])
    call <- as.call(call)
  }
  
  env <- attr(summ, "env")
  call$model <- summ$model
  eval(call, env, parent.frame())
  
}
