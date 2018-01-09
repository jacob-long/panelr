
model_builder <- function(mf, dv, endogs, exogs, constants, id, wave,
                          equal_err_var, print = FALSE) {

  d <- mf$data

  # Endogenous predictor lag structure
  ## Saving info about where in the respective lists the endogenous variables
  ## and their lag numbers are
  if (!is.null(endogs)) {

    if (length(mf$og_terms) > 0) {
      indices <- which(names(mf$new_names) %in% endogs)
    } else {
      indices <- NULL
    }

    ## Now going through and saving the number of lags to list for use later
    endogs_lags <- rep(0, length(endogs))
    names(endogs_lags) <- endogs
    if (length(indices) > 0) {
      endogs <- mf$new_names[indices]
      endogs_lags <- mf$vars_lags[indices]
      names(endogs_lags) <- endogs
      endogs <- unname(endogs)
    }
  } else {

    endogs <- NULL
    endogs_lags <- NULL

  }

  if (!is.null(exogs)) {
    # Exogenous predictor lag structure
    ## Saving info about where in the respective lists the exogenous variables
    ## and their lag numbers are

    if (length(mf$og_terms) > 0) {
      indices <- which(names(mf$new_names) %in% exogs)
    } else {
      indices <- NULL
    }

    ## Now going through and saving the number of lags to list for use later
    exogs_lags <- rep(0, length(exogs))
    names(exogs_lags) <- exogs
    if (length(indices) > 0) {
      exogs <- mf$new_names[indices]
      exogs_lags <- mf$vars_lags[indices]
      names(exogs_lags) <- exogs
      exogs <- unname(exogs)
    }

  } else {

    exogs <- NULL
    exogs_lags <- NULL

  }

  # Saving a vector of all variables that are time varying
  varying <- c(endogs, dv, exogs)

  # if (!is.null(exogs)) { # if exogenous vars, save those too
  #   varying <- c(varying, exogs)
  # }

  # Now I need to have the data in wide format, passing the list of time-varying
  # vars to the internal widen_panel function
  wframe <- widen_panel(d, varying = varying)

  # Save list of waves
  waves <- unique(d[,wave])

  # Creating list of time-varying variables in a list for constructing lavaan
  # model string
  vbywave <- list()
  for (w in waves) {
    varnames <- sapply(varying, paste, "_", w, sep = "")
    vbywave[[w]] <- varnames
  }

  # start variable is used to determine which wave to begin with
  start <- max(mf$vars_lags) # Depends on how many lags we have
  if (start == 0) {
    start <- 1
  }

  ##### Alpha equation #####
  alpha_eq <- paste("alpha =~ ", "1*", vbywave[[min(waves) + start]][dv],
                    sep = "")
  if (length(waves) > 2) {
    for (w in waves[3:length(waves)]) {
      alpha_eq <- paste(alpha_eq, " + 1*", vbywave[[w]][dv], sep = "")
    }
  }

  ##### Main dv equations #####

  main_eqs <- list()
  # iterating over each wave for which we will predict the value of DV
  for (w in waves[(1 + start):length(waves)]) {

    # Reg object has beginning of equation, then will be added onto
    ## Starts with first endogenous variable
    if (!is.null(endogs)) {
      reg <- paste(vbywave[[w]][dv], " ~ ", "en1*", vbywave[[w]][endogs[1]],
                   sep = "")
      ## If more endogenous variables, loop through them
      if (length(endogs) > 1) {
        for (var in endogs[2:length(endogs)]) {
          index <- which(endogs == var)
          reg <- paste(reg, " + ", "en", index, "*", vbywave[[w]][var],
                       sep = "")
        }
      }
    } else {

      reg <- paste(vbywave[[w]][dv], "~")

    }

    ## If there are exogenous time varying vars, loop through those
    if (!is.null(exogs)) {
      for (var in exogs) {

        index <- which(exogs == var)
        reg <- paste(reg, " + ", "ex", index, "*", vbywave[[w]][var],
                     sep = "")

      }
    }

    ## If there are constants, loop through them
    if (!is.null(constants)) {
      for (var in constants) {

        index <- which(constants == var)
        reg <- paste(reg, " + ", "c", index, "*", constants[index],
                     sep = "")

      }
    }

    ## Lastly, add prior wave of DV
    reg <- paste(reg, " + ", "p*", vbywave[[w-1]][dv], sep = "")

    ## Save finished equation to list
    main_eqs[[w-start]] <- reg

  }


  ##### Endogenous IV equations #####

  endogs_eqs <- rep(list(), times = length(endogs))

  # The part of the equation with time-varying exogenous predictors is the same
  # no matter what, so making it here
  if (!is.null(exogs)) {
    for (var in exogs) {
      # Making sure no non-existent values are used (e.g., t - 1 when t = 1)
      if ((start + exogs_lags[var]) < length(waves)) {
        if (which(exogs == var) == 1) {
          # This helps me know if it's the first time through the loop
          exogsreg <- paste("+", vbywave[[start + exogs_lags[var]]][var])
        } else {
          # otherwise prepend with +
          exogsreg <- paste(exogsreg, "+",
                            vbywave[[start + exogs_lags[var]]][var])
        }
        for (w in (start+1+exogs_lags[var]):length(waves)) {
          # Now adding the rest of the waves
          index <- which(exogs == var)
          exogsreg <- paste(exogsreg, " + ", vbywave[[w]][var],
                            sep = "")

        }
      } else if ((start + exogs_lags[var]) == length(waves)) {

        if (which(exogs == var) == 1) {
          exogsreg <- paste("+", vbywave[[start + exogs_lags[var]]][var])
        } else {
          exogsreg <- paste(exogsreg, "+",
                            vbywave[[start + exogs_lags[var]]][var])
        }

      }
    }
  }

  if (!is.null(constants)) { # Now create the constants part of the equations
    for (var in constants) {

      index <- which(constants == var)
      if (index == 1) {
        creg <- paste("+ ", constants[index],
                      sep = "")
      } else {
        creg <- paste(creg, " + ", constants[index],
                      sep = "")
      }

    }
  }

  # Creating the first endogenous IV equation
  if (!is.null(endogs)) {

    for (w in waves[(1 + start):length(waves)]) {
      # Create beginning of equation, including prior wave of its DV
      if (w-1 > endogs_lags[1]) {
        reg <- paste(vbywave[[w]][endogs[1]], " ~~ ", vbywave[[w-1]][endogs[1]],
                     sep = "")
        w2 <- w - 1
        while (w2 > start + endogs_lags[1]) { # Add rest of prior waves
          reg <- paste(reg, " + ", vbywave[[w2-1]][endogs[1]],
                       sep = "")
          w2 <- w2 - 1
        }
      } else {

        reg <- paste(vbywave[[w]][endogs[1]], "~~")

      }

      if (length(endogs) > 1) { # Add other endogenous IVs if they exist

        for (var in endogs[2:length(endogs)]) {
          index <- which(endogs == var)

          if (w - 1 >= start + endogs_lags[var]) {
            reg <- paste(reg, " + ", vbywave[[w-1]][var],
                         sep = "")
          }

          w2 <- w
          while (w2 > start + endogs_lags[var]) {
            reg <- paste(reg, " + ", vbywave[[w2-1]][var],
                         sep = "")
            w2 <- w2 - 1
          }

        }
      }

      if (!is.null(exogs)) { # Add the exogenous portion if needed
        reg <- paste(reg, exogsreg)
      }

      if (!is.null(constants)) { # Add the constants if needed
        reg <- paste(reg, creg)
      }

      # Add the only wave of DV not predicted in main_eqs
      reg <- paste(reg, " + ", vbywave[[start]][dv], sep = "")

      # Add finished equation to list
      endogs_eqs[[w-start]] <- reg

    }
  }

  # If there are more endogenous IVs, we need to make equations for them too
  if (length(endogs) > 1) {

    for (var in endogs[2:length(endogs)]) {

      for (w in waves[(1 + start):length(waves)]) {

        index2 <- which(endogs == var)

        if (w - 1 > endogs_lags[index2]) {
          reg <- paste(vbywave[[w]][endogs[index2]], " ~~ ",
                       vbywave[[w-1]][endogs[index2]],
                       sep = "")
          w2 <- w - 1
          while (w2 > start + endogs_lags[index2]) {
            reg <- paste(reg, " + ", vbywave[[w2-1]][endogs[index2]],
                         sep = "")
            w2 <- w2 - 1
          }
        } else {

          reg <- paste(vbywave[[w]][endogs[index2]], "~~")

        }

        if (length(endogs) > 1) {
          for (varn in endogs[!(endogs == var)]) {

            if (w - 1 > start + endogs_lags[which(endogs == varn)]) {
              reg <- paste(reg, " + ", vbywave[[w-1]][varn],
                           sep = "")
            }

          }

          w2 <- w
          while (w2 > start + endogs_lags[which(endogs == varn)]) {
            reg <- paste(reg, " + ", vbywave[[w2-1]][varn],
                         sep = "")
            w2 <- w2 - 1
          }
        }

        if (!is.null(exogs)) {
          reg <- paste(reg, exogsreg)
        }

        if (!is.null(constants)) {
          reg <- paste(reg, creg)
        }

        reg <- paste(reg, " + ", vbywave[[start]][dv], sep = "")

        endogs_eqs[[w-start]][index2] <- reg

      }
    }
  }

  #### Exogenous tv covariances ####

  exogs_eqs <- rep(list(), times = length(exogs))
  if (!is.null(exogs)) {

    for (w in waves[(1 + start):length(waves)]) {
      # Create beginning of equation, including prior wave of its DV
      if (w - 1 > exogs_lags[1]) {
        reg <- paste(vbywave[[w]][exogs[1]], " ~~ ", vbywave[[w-1]][exogs[1]],
                     sep = "")
        w2 <- w - 1
        while (w2 > start + exogs_lags[1]) { # Add rest of prior waves
          reg <- paste(reg, " + ", vbywave[[w2-1]][exogs[1]],
                       sep = "")
          w2 <- w2 - 1
        }
      } else {

        reg <- paste(vbywave[[w]][exogs[1]], " ~~ ")

      }

      if (length(exogs) > 1) { # Add other exogenous IVs if they exist

        for (var in exogs[2:length(exogs)]) {
          index <- which(exogs == var)

          if (w - 1 > start + exogs_lags[var]) {
            reg <- paste(reg, " + ", vbywave[[w-1]][var],
                         sep = "")
          }

          w2 <- w
          while (w2 > start + exogs_lags[var]) {
            reg <- paste(reg, " + ", vbywave[[w2-1]][var],
                         sep = "")
            w2 <- w2 - 1
          }

        }
      }

      # if (!is.null(exogs)) { # Add the exogenous portion if needed
      #   reg <- paste(reg, exogsreg)
      # }

      if (!is.null(constants)) { # Add the constants if needed
        reg <- paste(reg, creg)
      }

      # Add the only wave of DV not predicted in main_eqs
      reg <- paste(reg, " + ", vbywave[[start]][dv], sep = "")

      # Add finished equation to list
      exogs_eqs[[w-start]] <- reg

    }

    # If there are more endogenous IVs, we need to make equations for them too
    if (length(exogs) > 1) {

      for (var in exogs[2:length(exogs)]) {

        for (w in waves[(1 + start):length(waves)]) {

          index2 <- which(exogs == var)

          if (w - 1 > exogs_lags[index2]) {

            reg <- paste(vbywave[[w]][exogs[index2]], " ~~ ",
                         vbywave[[w-1]][exogs[index2]],
                         sep = "")
            w2 <- w - 1
            while (w2 > start + exogs_lags[index2]) {
              reg <- paste(reg, " + ", vbywave[[w2-1]][exogs[index2]],
                           sep = "")
              w2 <- w2 - 1
            }

          } else {

            reg <- paste(vbywave[[w]][exogs[index2]], "~~")

          }

          if (length(exogs) > 1) {
            for (varn in exogs[!(exogs == var)]) {

              if (w - 1 > start + exogs_lags[which(exogs == varn)]) {
                reg <- paste(reg, " + ", vbywave[[w-1]][varn],
                             sep = "")
              }

            }

            w2 <- w
            while (w2 > start + exogs_lags[which(exogs == varn)]) {
              reg <- paste(reg, " + ", vbywave[[w2-1]][varn],
                           sep = "")
              w2 <- w2 - 1
            }
          }

          # if (!is.null(exogs)) {
          #   reg <- paste(reg, exogsreg)
          # }

          if (!is.null(constants)) {
            reg <- paste(reg, creg)
          }

          reg <- paste(reg, " + ", vbywave[[start]][dv], sep = "")

          exogs_eqs[[w-start]][index2] <- reg

        }
      }
    }
  }

  ##### Constants covariances #####
  constants_covs <- rep(list(), times = length(constants))
  if (!is.null(constants)) {

    for (c in constants) {

      reg <- paste(c, "~~", vbywave[[start]][dv])

      if (which(constants == c) < length(constants)) {

        ocs <- constants[(which(constants == c)+1):length(constants)]
        for (oc in ocs) {

          reg <- paste(reg, "+", oc)

        }

      }

      constants_covs[[which(constants == c)]] <- reg

    }

  }

  #### Simultaneous endogenous correlations ####
  sim_end_covs <- c()
  if (length(endogs) > 1) {

      for (v in endogs[1:length(endogs)-1]) {

        w2 <- start + endogs_lags[v]

        while (w2 <= length(waves)) {

          ovs <- endogs[(which(endogs == v)+1):length(endogs)]
          reg <- paste(vbywave[[w2]][v], "~~")

          for (o in ovs) {

            if (which(ovs == o) == 1) {

              reg <- paste(reg, vbywave[[w2]][o])

            } else {

              reg <- paste(reg, "+", vbywave[[w2]][o])

            }

          }

          sim_end_covs <- c(sim_end_covs, reg)
          w2 <- w2 + 1
        }

    }

  }

  #### Simultaneous exogenous correlations ####
  sim_ex_covs <- c()
  if (length(exogs) > 1) {

    for (v in exogs[1:length(exogs)-1]) {

      w2 <- start + exogs_lags[v]

      while (w2 <= length(waves)) {

        ovs <- exogs[(which(exogs == v)+1):length(exogs)]
        reg <- paste(vbywave[[w2]][v], "~~")

        for (o in ovs) {

          if (which(ovs == o) == 1) {

            reg <- paste(reg, vbywave[[w2]][o])

          } else {

            reg <- paste(reg, "+", vbywave[[w2]][o])

          }

        }

        sim_ex_covs <- c(sim_ex_covs, reg)
        w2 <- w2 + 1
      }

    }

  }

  ##### Correlated future errors equations #####
  endogs_errs <- list()
  reg <- NULL
  if (!is.null(endogs)) {

    for (w in (2 + start):length(waves)) {

      if ((w + endogs_lags[1]) <= length(waves)) {

        for (w2 in (w + endogs_lags[1]):length(waves)) {

          if (w2 == (w + endogs_lags[1])) {
            reg <- paste(vbywave[[w-1]][dv], "~~", vbywave[[w2]][endogs[1]])
          } else {
            reg <- paste(reg, "+", vbywave[[w2]][endogs[1]])
          }

        }

      }

      if (length(endogs) > 1) {

        for (var in endogs[2:length(endogs)]) {

          index <- which(endogs == var)

          if ((w + endogs_lags[index]) <= length(waves)) {

            for (w2 in (w + endogs_lags[index]):length(waves)) {

              reg <- paste(reg, " + ", vbywave[[w2]][var],
                           sep = "")

            }
          }

        }

      }
    }

    endogs_errs[[w-start-1]] <- reg
    reg <- NULL

  }

  ##### alpha covariances #####

  alpha_reg <- "alpha ~~" # Beginning of equation
  varying_vars <- c(endogs, exogs)
  varying_lags <- c(endogs_lags, exogs_lags)

  for (w in waves[start:length(waves)]) {

    if (w != start && ((w + varying_lags[1]) <= length(waves))) {

      alpha_reg <- paste(alpha_reg, "+",
                         vbywave[[w + varying_lags[1]]][varying_vars[1]])

    } else if ((w + varying_lags[1]) <= length(waves)) {
      # Making sure the first time through isn't prepended with +
      alpha_reg <- paste(alpha_reg,
                         vbywave[[w + varying_lags[1]]][varying_vars[1]])

    }

    if (length(varying_vars) > 1) { # Add on other endogenous IVs

      for (var in varying_vars[2:length(varying_vars)]) {

        index <- which(varying_vars == var)

        if ((varying_lags[index] + w) <= length(waves)) {
          alpha_reg <- paste(alpha_reg, " + ",
                             vbywave[[w + varying_lags[index]]][var],
                             sep = "")
        }

      }

    }

    # if (!is.null(exogs)) { # Loop through exogenous IVs
    #
    #   for (var in exogs) {
    #
    #     index <- which(exogs == var)
    #
    #     if ((exogs_lags[index] + w) <= length(waves)) {
    #
    #       alpha_reg <- paste(alpha_reg, " + ",
    #                          vbywave[[w + exogs_lags[index]]][var], sep = "")
    #
    #     }
    #
    #   }
    #
    # }

  }

  for (w in 1:start) { # Add waves of DV prior to first time it is predicted

    alpha_reg <- paste(alpha_reg, " + ", vbywave[[w]][dv], sep = "")

  }

  ## Defining annotations to be used in final model string
  alpha_eq_ann <- "## Alpha latent variable (fixed effects)"
  alpha_reg_ann <- "## Alpha regression (fixed effects)"
  main_eqs_ann <- "## Main regressions"
  endogs_eqs_ann <- "## Covarying endogenous predictors with exogenous predictors"
  exogs_eqs_ann <- "## Covarying exogenous predictors with other exogenous predictors"
  endogs_errs_ann <- "## Correlating DV errors with future values of endogenous predictors"
  err_var_eqs_ann <- "## Holding error variance constant for each wave (optional)"

  ##### Error variance equations (optional) #####
  # If user wants DV error variances for each wave to be held constant, do it
  if (equal_err_var == TRUE) {

    err_var_eqs <- list()
    for (w in (start+1):length(waves)) {

      eq <- paste(vbywave[[w]][dv], " ~~ ", "v*", vbywave[[w]][dv], sep = "")
      err_var_eqs[[w-start]] <- eq

    }

    # Now save all parts of the lavaan model to one object
    all_eqs <- list(main_eqs_ann, main_eqs, alpha_eq_ann, alpha_eq,
                    alpha_reg_ann, alpha_reg, endogs_eqs_ann, endogs_eqs,
                    sim_end_covs,
                    exogs_eqs_ann, exogs_eqs, sim_ex_covs, constants_covs,
                    endogs_errs_ann, endogs_errs, err_var_eqs_ann, err_var_eqs)

  } else {
    # Now save all parts of the lavaan model to one object
    all_eqs <- list(main_eqs_ann, main_eqs, alpha_eq_ann, alpha_eq,
                    alpha_reg_ann, alpha_reg, endogs_eqs_ann, endogs_eqs,
                    sim_end_covs,
                    exogs_eqs_ann, exogs_eqs, sim_ex_covs, constants_covs,
                    endogs_errs_ann, endogs_errs)

  }

  # Adding line breaks between the equations
  out <- lapply(all_eqs, concat)

  # Adding extra line breaks between each set of equations
  out <- concat2(out)

  if (print == TRUE) {
    cat(out, "\n")
  }

  ret_obj <- list(model = out, data = wframe, endogs_lags = endogs_lags,
                  endogs = endogs)
  if (!is.null(exogs)) {
    ret_obj$exogs_lags <- exogs_lags
    ret_obj$exogs <- exogs
  }
  return(ret_obj)

}
