#' @title Bayesian estimation of within-between models
#' @description A near-equivalent of [wbm()] that instead uses Stan,
#'   via \pkg{rstan} and \pkg{brms}.
#' @inheritParams wbm
#'
#' @param model.cor Do you want to model residual autocorrelation?
#'   This is often appropriate for linear models (`family = gaussian`).
#'   Default is FALSE to be consistent with [wbm()], reduce
#'   runtime, and avoid warnings for non-linear models.
#' @param fit_model Fit the model? Default is TRUE. If FALSE, only the model
#'   code is returned.
#' @param chains How many Markov chains should be used? Default is 3, to leave
#'   you with one unused thread if you're on a typical dual-core machine.
#' @param iter How many iterations, including warmup? Default is 2000, leaving
#'   1000 per chain after warmup. For some models and data, you may need quite
#'   a few more.
#' @param scale Standardize predictors? This can speed up model fit. Default
#'   is FALSE.
#' @param save_ranef Save random effect estimates? This can be crucial for
#'   predicting from the model and for certain post-estimation procedures.
#'   On the other hand, it drastically increases the size of the resulting
#'   model. Default is FALSE.
#' @param ... Additional arguments passed on to [brms::brm()]. This can include
#'   specification of priors.
#' @return A `wbm_stan` object, which is a list containing a `model` object
#'   with the `brm` model and a `stan_code` object with the model code.
#'
#'   If `fit_model = FALSE`, insead a list is returned containing a `stan_code`
#'   object and a `stan_data` object, leaving you with the tools you need to
#'   run the model yourself using `rstan`.
#'
#' @author Jacob A. Long
#' @details See [wbm()] for details on the formula syntax, model types,
#'   and some other stuff.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  data("WageData")
#'  wages <- panel_data(WageData, id = id, wave = t)
#'  model <- wbm_stan(lwage ~ lag(union) + wks | blk + fem | blk * lag(union),
#'            data = wages, chains = 1, iter = 2000)
#'  summary(model)
#'  }
#' }
#' @export
#' @rdname wbm_stan
#' @seealso [wbm()]
#'
#' @importFrom stats as.formula gaussian terms

wbm_stan <- function(formula, data, id = NULL, wave = NULL, model = "w-b",
                   use.wave = FALSE, wave.factor = FALSE, min.waves = 2,
                   model.cor = FALSE, family = gaussian, dynamic = FALSE,
                   fit_model = TRUE, chains = 3, iter = 2000, scale = FALSE,
                   save_ranef = FALSE, weights = NULL, ...) {

  # Get data prepped
  if (class(data)[1] == "panel_data") {
    id <- "id"
    wave <- "wave"
  }

  wave <- as.character(substitute(wave))

  id <- as.character(substitute(id))

  weights <- as.character(substitute(weights))
  if (length(weights) == 0) {
    weights <- NULL
  }

  if (!("data.frame" %in% class(data))) {
    stop("data argument must be a data frame.")
  }

  data <- panel_data(data, id = id, wave = wave)

  # Make sure lme4 is installed
  if (requireNamespace("lme4", quietly = TRUE) == FALSE) {
    stop("You must have the nlme package installed to use this function.")
  }

  ## Dealing with formula
  formula <- as.formula(substitute(quote(formula)))
  if (as.character(formula[[1]]) != "~") {
    stop("Invalid formula. Include the outcome variable on the left side
         followed by `~` and then the predictors.")
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

  e <- wb_model(model, pf, dv, data, dynamic)

  data <- e$data

  if (use.wave == TRUE) {
    e$fin_formula <- paste(e$fin_formula, "+", "wave")
  }

  if (wave.factor == TRUE) {
    data[,wave] <- as.factor(data[,wave])
  }

  if (dynamic == TRUE) {
    e$fin_formula <- paste0(e$fin_formula, " + lag.", dv, ".")
  }

  fin_formula <- formula_esc(e$fin_formula,
                                   c(pf$varying,
                                     pf$meanvars,
                                     pf$constants))

  names(data) <- make.names(names(data))

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
    fin_formula <- paste0(fin_formula, " + (1 | id)")
  }

  # Give brms the weights in the desired formula syntax
  if (!is.null(weights)) {
    weights <- make.names(weights) # Also give syntactically valid name for wts
    lhs <- paste(dv, "~")
    new_lhs <- paste0(dv, " | weights(", weights, ") ~ ")
    fin_formula <- sub(lhs, new_lhs, fin_formula, fixed = TRUE)
  }

  fin_formula <- as.formula(fin_formula)

  int_indices <- which(attr(terms(fin_formula), "order") >= 2)
  ints <- attr(terms(fin_formula),"term.labels")[int_indices]
  ints <- ints[!(ints %in% e$stab_terms)]
  unbt_ints <- gsub("`", "", ints, fixed = TRUE)
  ints <- ints[!(unbt_ints %in% e$stab_terms)]

  cor_form <- as.formula(paste("~ wave | id"))

  if (scale == TRUE) {

    scale_names <- names(data)
    scale_names <- scale_names[!(names(data) %in% c("id","wave",dv,weights))]
    data <- jtools::gscale(x = scale_names, data = data, n.sd = 1,
                           binary.inputs = "0/1")

  }

  # Give users the option to just get code + data for this
  if (fit_model == TRUE) {

    if (model.cor == TRUE) {
      model <- brms::brm(fin_formula,
                         autocor = brms::cor_ar(formula = cor_form),
                         data = data,
                         chains = chains, iter = iter,
                         family = family,
                         save_ranef = save_ranef, ...)
    } else {
      model <- brms::brm(fin_formula, data = data,
                         chains = chains, iter = iter,
                         family = family, save_ranef = save_ranef, ...)
    }

    out <- list(model = model, data = data, fin_formula = fin_formula,
                dv = dv, id = id, wave = wave,
                num_distinct = num_distinct,
                varying = pf$varying, model = model,
                stab_terms = e$stab_terms,
                max_wave = maxwave, min_wave = minwave, ints = ints,
                model_cor = model.cor)

    class(out) <- "wbm_stan"

    return(out)

  } else {

    if (model.cor == TRUE) {
      standat <- brms::make_standata(fin_formula,
                         autocor = brms::cor_ar(formula = cor_form),
                         data = data,
                         chains = chains, iter = iter,
                         family = family,
                         save_ranef = save_ranef, ...)

      stancode <-
        brms::make_stancode(fin_formula,
                            autocor = brms::cor_ar(formula = cor_form),
                            data = data, chains = chains, iter = iter,
                            family = family, save_ranef = save_ranef, ...)

      return(list(stan_data = standat, stan_code = stancode))

    } else {

      standat <- brms::make_standata(fin_formula, data = data,
                         chains = chains, iter = iter,
                         family = family, save_ranef = save_ranef, ...)

      stancode <- brms::make_stancode(fin_formula, data = data,
                                     chains = chains, iter = iter,
                                     family = family, save_ranef = save_ranef,
                                     ...)

      return(list(stan_data = standat, stan_code = stancode))

    }

  }

}

#' @export
#'
#'

summary.wbm_stan <- function(object, ...) {

  summary(object$model)

}


# summary.wb_stan <- function(x, ...) {
#
#   cat("MODEL INFO:\n")
#   cat("Entities:", x$model$dims$ngrps[x$id], "\n")
#   cat("Time periods:", length(unique(x$data[,x$wave])), "\n")
#   cat("Dependent variable:", x$dv, "\n")
#   cat("Model type: Linear mixed effects\n")
#
#   # Name the estimator
#   est_name <- x$estimator
#   if (x$estimator == "w-b") {est_name <- "within-between"}
#   if (x$estimator == "stability") {
#     est_name <- "within-between with between-entity time trends"
#   }
#   cat("Estimator: ", est_name, "\n\n", sep = "")
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
#
#   if (length(x$varying) > 0) {
#
#     cat("WITHIN EFFECTS:\n")
#     print(coefs[rownames(coefs) %in% x$varying,])
#     cat("\n")
#
#     coefs <- coefs[!(rownames(coefs) %in% x$varying),]
#     if (length(x$stab_terms) > 0) {
#
#       stabs <- coefs[rownames(coefs) %in% x$stab_terms,]
#       coefs <- coefs[!(rownames(coefs) %in% x$stab_terms),]
#
#     }
#
#   }
#
#   cat("BETWEEN EFFECTS:\n")
#   print(coefs)
#   cat("\n")
#
#   if (x$estimator == "stability") {
#
#     cat("BETWEEN-ENTITY TIME TRENDS:\n")
#     print(stabs)
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
