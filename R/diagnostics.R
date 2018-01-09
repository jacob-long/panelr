bptestmod <- function (formula, varformula = NULL, studentize = TRUE, data = list())
{
  dname <- paste(deparse(substitute(formula)))
  if (!inherits(formula, "formula")) {
    X <- model.matrix(terms(formula), model.frame(formula))
    y <- if (is.vector(formula@resp$y))
      formula@resp$y
    else model.response(model.frame(formula))
    Z <- if (is.null(varformula))
      X
    else model.matrix(varformula, data = data)
  }
  else {
    mf <- model.frame(formula, data = data)
    y <- model.response(mf)
    X <- model.matrix(formula, data = data)
    Z <- if (is.null(varformula))
      X
    else model.matrix(varformula, data = data)
  }
  if (!(all(c(row.names(X) %in% row.names(Z), row.names(Z) %in%
              row.names(X))))) {
    allnames <- row.names(X)[row.names(X) %in% row.names(Z)]
    X <- X[allnames, ]
    Z <- Z[allnames, ]
    y <- y[allnames]
  }
  if (ncol(Z) < 2)
    stop("the auxiliary variance regression requires at least an intercept and a regressor")
  k <- ncol(X)
  n <- nrow(X)
  resi <- lm.fit(X, y)$residuals
  sigma2 <- sum(resi^2)/n
  if (studentize) {
    w <- resi^2 - sigma2
    aux <- lm.fit(Z, w)
    bp <- n * sum(aux$fitted.values^2)/sum(w^2)
    method <- "studentized Breusch-Pagan test"
  }
  else {
    f <- resi^2/sigma2 - 1
    aux <- lm.fit(Z, f)
    bp <- 0.5 * sum(aux$fitted.values^2)
    method <- "Breusch-Pagan test"
  }
  names(bp) <- "BP"
  df <- c(df = aux$rank - 1)
  RVAL <- list(statistic = bp, parameter = df, method = method,
               p.value = pchisq(bp, df, lower.tail = FALSE), data.name = dname)
  class(RVAL) <- "htest"
  return(RVAL)
}


dwtestmod <- function (formula, order.by = NULL, alternative = c("greater",
                                                                 "two.sided", "less"), iterations = 15, exact = NULL, tol = 1e-10,
                       data = list())
{
  dname <- paste(deparse(substitute(formula)))
  alternative <- match.arg(alternative)
  if (!inherits(formula, "formula")) {
    if (!is.null(w <- weights(formula))) {
      if (!isTRUE(all.equal(as.vector(w), rep(1L, length(w)))))
        stop("weighted regressions are not supported")
    }
    X <- model.matrix(terms(formula), model.frame(formula))
    y <- if (is.vector(formula@resp$y))
      formula@resp$y
    else model.response(model.frame(formula))
  }
  else {
    mf <- model.frame(formula, data = data)
    y <- model.response(mf)
    X <- model.matrix(formula, data = data)
  }
  if (!is.null(order.by)) {
    if (inherits(order.by, "formula")) {
      z <- model.matrix(order.by, data = data)
      z <- as.vector(z[, ncol(z)])
    }
    else {
      z <- order.by
    }
    X <- as.matrix(X[order(z), ])
    y <- y[order(z)]
  }
  n <- nrow(X)
  if (is.null(exact))
    exact <- (n < 100)
  k <- ncol(X)
  res <- lm.fit(X, y)$residuals
  dw <- sum(diff(res)^2)/sum(res^2)
  Q1 <- chol2inv(qr.R(qr(X)))
  if (n < 3) {
    warning("not enough observations for computing a p value, set to 1")
    pval <- 1
  }
  else {
    if (exact) {
      A <- diag(c(1, rep(2, n - 2), 1))
      A[abs(row(A) - col(A)) == 1] <- -1
      MA <- diag(rep(1, n)) - X %*% Q1 %*% t(X)
      MA <- MA %*% A
      ev <- eigen(MA)$values[1:(n - k)]
      if (any(Im(ev) > tol))
        warning("imaginary parts of eigenvalues discarded")
      ev <- Re(ev)
      ev <- ev[ev > tol]
      pdw <- function(dw) .Fortran("pan", as.double(c(dw,
                                                      ev)), as.integer(length(ev)), as.double(0), as.integer(iterations),
                                   x = double(1), PACKAGE = "lmtest")$x
      pval <- switch(alternative, two.sided = (2 * min(pdw(dw),
                                                       1 - pdw(dw))), less = (1 - pdw(dw)), greater = pdw(dw))
      if (is.na(pval) || ((pval > 1) | (pval < 0))) {
        warning("exact p value cannot be computed (not in [0,1]), approximate p value will be used")
        exact <- FALSE
      }
    }
    if (!exact) {
      if (n < max(5, k)) {
        warning("not enough observations for computing an approximate p value, set to 1")
        pval <- 1
      }
      else {
        AX <- matrix(as.vector(filter(X, c(-1, 2, -1))),
                     ncol = k)
        AX[1, ] <- X[1, ] - X[2, ]
        AX[n, ] <- X[n, ] - X[(n - 1), ]
        XAXQ <- t(X) %*% AX %*% Q1
        P <- 2 * (n - 1) - sum(diag(XAXQ))
        Q <- 2 * (3 * n - 4) - 2 * sum(diag(crossprod(AX) %*%
                                              Q1)) + sum(diag(XAXQ %*% XAXQ))
        dmean <- P/(n - k)
        dvar <- 2/((n - k) * (n - k + 2)) * (Q - P *
                                               dmean)
        pval <- switch(alternative, two.sided = (2 *
                                                   pnorm(abs(dw - dmean), sd = sqrt(dvar), lower.tail = FALSE)),
                       less = pnorm(dw, mean = dmean, sd = sqrt(dvar),
                                    lower.tail = FALSE), greater = pnorm(dw,
                                                                         mean = dmean, sd = sqrt(dvar)))
      }
    }
  }
  alternative <- switch(alternative, two.sided = "true autocorrelation is not 0",
                        less = "true autocorrelation is less than 0", greater = "true autocorrelation is greater than 0")
  names(dw) <- "DW"
  RVAL <- list(statistic = dw, method = "Durbin-Watson test",
               alternative = alternative, p.value = pval, data.name = dname)
  class(RVAL) <- "htest"
  return(RVAL)
}


actest <- function(model, id, wave) {

  ids <- model@frame[,id]
  ids <- sort(ids)

  lframe <- matrix(data = 0L, ncol = 4,
                   nrow = length(ids))
  colnames(lframe) <- c("id", "wave", "resid", "preresid")
  lframe <- as.data.frame(lframe)
  lframe$preresid <- NA

  lframe$id <- ids

  model@frame$resid <- residuals(model)

  for (u in unique(ids)) {

    num <- length(which(ids == u))

    indices <- which(lframe$id == u)

    waves <- unique(model@frame[which(model@frame[,id] == u), wave])
    waves <- sort(waves)

    for (t in 1:length(waves)) {

      lframe$wave[indices[t]] <- waves[t]

      lframe$resid[lframe$id == u & lframe$wave == waves[t]] <-
        model@frame$resid[model@frame[,id] == u &
                            model@frame[,wave] == waves[t]]

      if (t > 1) {
        lframe$preresid[lframe$id == u & lframe$wave == waves[t]] <-
          lframe$resid[lframe$id == u & lframe$wave == waves[t-1]]
      }

    }

  }

  lframes <- jtools::gscale(x = c("resid","preresid"), data = lframe)
  fit <- lm(resid ~ preresid, data = lframes)
  return(round(coefficients(fit), 3))

}

#' @export

endog_test <- function(dv, iv, data, id, wave, lag = TRUE) {

  if (class(data)[1] == "panel_data") {
    id <- attr(data, "id")
    wave <- attr(data, "wave")
  }

  wave <- as.character(substitute(wave))
  id <- as.character(substitute(id))

  data <- panel_data_in(data, id = id, wave = wave)

  dv <- as.character(substitute(dv))
  dv_o <- dv
  iv <- as.character(substitute(iv))

  if (lag == TRUE) {
    dv <- paste("lag(", dv, ")", sep = "")
    mf <- panel_model_frame(c(dv,iv), data)

    data <- mf$data

    if (length(mf$og_terms) > 0) {
      dv <- mf$lags_vars_names
    }
  }

  aggformula <- as.formula(paste(dv, "~", id))
  agg <- aggregate(aggformula, data = data, FUN = "mean")

  aggname <- paste(dv, ".mean", sep = "")

  data[,aggname] <- NA

  for (i in agg[,as.character(id)]) {
    indices <- which(data[,as.character(id)] == i)
    data[,aggname][indices] <- agg[,dv][agg[,as.character(id)] == i]
  }

  data[,dv] <- data[,dv] - data[,aggname]

  # if (center == TRUE) {
  #
  #   aggformula <- as.formula(paste(iv, "~", id))
  #   agg <- aggregate(aggformula, data = data, FUN = "mean")
  #
  #   aggname <- paste(iv, ".mean", sep = "")
  #
  #   data[,aggname] <- NA
  #
  #   for (i in agg[,as.character(id)]) {
  #     indices <- which(data[,as.character(id)] == i)
  #     data[,aggname][indices] <- agg[,iv][agg[,as.character(id)] == i]
  #   }
  #
  #   data[,iv] <- data[,iv] - data[,aggname]
  #
  # }

  if (is.numeric(data[,wave])) {
    maxwave <- max(unique(data[,wave]))
  } else {
    maxwave <- max(unique(as.numeric(data[,wave])))
    maxwave <- as.character(maxwave)
  }

  data <- data[data[,wave] == maxwave,]

  form <- as.formula(paste(iv, " ~ ", dv, " + ", dv, ".mean", sep = ""))

  out <- lm(form, data = as.data.frame(data))

  class(out) <- c("endog_test","lm")

  attr(out, "dv") <- dv_o
  attr(out, "iv") <- iv
  attr(out, "lag") <- lag

  return(out)

}

#' @export
#'

print.endog_test <- function(x, ...) {

  sum <- summary(x)

  pval <- sum$coefficients[2,4]
  est <- sum$coefficients[2,1]

  dv <- attr(x, "dv")
  iv <- attr(x, "iv")
  lag <- attr(x, "lag")

  if (lag == FALSE) {
    change <- "change in"
  } else {
    change <- "prior change in"
  }

  cat("ENDOGENEITY TEST\n")
  cat("Testing whether", change, dv, "causes", iv, "\n")

  cat("\nEsimated effect = ", round(est,3), ", p = ", round(pval, 3), "\n\n",
      sep = "")

}
