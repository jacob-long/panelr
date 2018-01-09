#' @export
#'

wbm <- function(formula, data, id = NULL, wave = NULL,
                estimator = "w-b", use.wave = FALSE, wave.factor = FALSE,
                min.waves = 2, family = gaussian,
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
  # Pass to special model_frame function that respects tibble groupings
  data <- model_frame(as.formula(mf_form), data = data)

  data <- complete_cases(data, min.waves = min.waves)
  num_distinct <- length(unique(data$id))
  maxwave <- max(data["wave"])
  minwave <- min(data["wave"])
  if (!is.null(weights)) {
    weights <- data[[weights]]
  }

  e <- wb_estimator(estimator, pf, dv, data)

  data <- e$data

  if (use.wave == TRUE) {
    e$fin_formula <- paste(e$fin_formula, "+", "wave")
  }

  if (wave.factor == TRUE) {
    data[,wave] <- as.factor(data[,wave])
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

    model <- lme4::lmer(fin_formula, data = data,
                        weights = weights, ...)

  } else if (as.character(substitute(family)) == "negbinomial") {

    model <- lme4::glmer.nb(fin_formula, data = data,
                            weights = weights, ...)

  } else {

    model <- lme4::glmer(fin_formula, data = data, family = family,
                         weights = weights, ...)

  }

  # Getting jtools summary info so it isn't re-run every time summary()
  # is called
  j <- jtools::j_summ(model, pvals = pvals, r.squared = pR2)
  j2 <- attributes(j)

  out <- list(model = model, data = data, fin_formula = fin_formula)

  out <- structure(out, dv = dv, id = id, wave = wave,
              num_distinct = num_distinct,
              varying = pf$varying, estimator = estimator,
              stab_terms = e$stab_terms,
              max_wave = maxwave, min_wave = minwave, ints = ints,
              pvals = pvals, pR2 = pR2, jsumm = j, jatts = j2)

  class(out) <- "wbm"

  return(out)

}

#' @export
#'

summary.wbm <- function(x, digits = getOption("jtools-digits", 2), ...) {

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

  # Name the estimator
  est_name <- x2$estimator
  if (x2$estimator == "w-b") {est_name <- "within-between"}
  if (est_name == "random") {est_name <- "between"}

  est_info <- paste("Estimator: ", est_name, "\n\n", sep = "")

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

  if (is.null(nrow(coefs))) { # Can't have single row table

    if (dim(coefs)[1] > 0) {
      vec <- coefs
      vec <- t(as.matrix(vec))
      rownames(vec) <- rows

      between_table <- as.table(vec)
    } else {
      between_table <- NULL
    }

  } else {

    between_table <- as.table(coefs)

  }

  if (x2$estimator == "stability") {

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
              estimator = x2$estimator, est_name = est_name,
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

  if (x$estimator == "stability") {

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
#   estimator <- "within" # it's just always within now
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
#   e <- wb_estimator(estimator, pf, dv, data)
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
#                    varying = pf$varying, estimator = estimator,
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
#   # Name the estimator
#   est_name <- x2$estimator
#
#   est_info <- paste("Estimator: ", est_name, "\n\n", sep = "")
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
#               estimator = x2$estimator, est_name = est_name,
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




# wb_lme <- function(formula, data, id = NULL, wave = NULL, estimator = "w-b",
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
#   e <- wb_estimator(estimator, pf, dv, data)
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
#               varying = pf$varying, estimator = estimator,
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
#   if (x$estimator == "stability") {
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

