#' @importFrom stats na.action residuals
vcov_CR <- function(obj, cluster, type, target = NULL, inverse_var = TRUE, 
                    form = "sandwich", ignore_FE = FALSE, data = NULL) {
  cluster <- droplevels(as.factor(cluster))
  alias <- is.na(coef(obj))
  X <- model.matrix(formula(obj), data = data)
  if (any(alias)) {
    X <- X[, !alias, drop = FALSE]
  }
  p <- NCOL(X)
  N <- NROW(X)
  if (length(cluster) != N) {
    if (inherits(na.action(obj), "omit")) {
      cluster <- droplevels(cluster[-na.action(obj)])
    }
    else {
      stop("Clustering variable must have length equal to nrow(model_matrix(obj)).")
    }
  }
  if (any(is.na(cluster))) 
    stop("Clustering variable cannot have missing values.")
  J <- nlevels(cluster)
  X_list <- matrix_list(X, cluster, "row")
  W_list <- weightMatrix(obj, cluster)
  XW_list <- Map(function(x, w) as.matrix(t(x) %*% w), x = X_list, 
                 w = W_list)
  if (is.null(target)) {
    if (inverse_var) {
      Theta_list <- lapply(W_list, function(w) chol2inv(chol(w)))
    }
    else {
      Theta_list <- targetVariance(obj, cluster)
    }
  }
  else {
    if (!is.list(target)) {
      Theta_list <- matrix_list(target, cluster, "both")
    }
    else {
      Theta_list <- target
    }
  }
  if (type %in% c("CR2", "CR4")) {
    U_list <- X_list
    UW_list <- XW_list
    UWU_list <- Map(function(uw, u) uw %*% u, uw = UW_list, 
                    u = U_list)
    M_U <- matrix_power(Reduce("+", UWU_list), p = -1)
  }
  adjustments <- do.call(type, args = mget(names(formals(type))))
  E_list <- adjust_est_mats(type = type, est_mats = XW_list, 
                            adjustments = adjustments)
  resid <- residuals(obj)
  res_list <- split(resid, cluster)
  components <- do.call(cbind, Map(function(e, r) e %*% r, 
                                   e = E_list, r = res_list))
  v_scale <- nobs(obj)
  w_scale <- attr(W_list, "w_scale")
  if (is.null(w_scale)) 
    w_scale <- 1L
  meat <- tcrossprod(components) * w_scale^2/v_scale
  if (form == "sandwich") {
    if (!requireNamespace("sandwich")) need_package("sandwich")
    bread <- sandwich::bread(obj)
  }
  else if (form == "meat") {
    bread <- NULL
  }
  else if (is.matrix(form)) {
    bread <- form
    form <- "sandwich"
  }
  vcov <- switch(form, sandwich = bread %*% meat %*% bread/v_scale, 
                 meat = meat)
  rownames(vcov) <- colnames(vcov) <- colnames(X)
  attr(vcov, "type") <- type
  attr(vcov, "cluster") <- cluster
  attr(vcov, "bread") <- bread
  attr(vcov, "v_scale") <- v_scale
  attr(vcov, "est_mats") <- XW_list
  attr(vcov, "adjustments") <- adjustments
  attr(vcov, "target") <- Theta_list
  attr(vcov, "inverse_var") <- inverse_var
  attr(vcov, "ignore_FE") <- ignore_FE
  class(vcov) <- c("vcovCR", "clubSandwich")
  return(vcov)
}

matrix_list <- function(x, fac, dim) {
  if (is.vector(x)) {
    if (dim != "both") 
      stop(paste0("Object must be a matrix in order to subset by ", 
                  dim, "."))
    x_list <- split(x, fac)
    lapply(x_list, function(x) diag(x, nrow = length(x)))
  }
  else {
    lapply(levels(fac), sub_f(x, fac, dim))
  }
}

weightMatrix <- function(obj, cluster = nlme::getGroups(obj)) {
  V_list <- targetVariance(obj, cluster)
  lapply(V_list, function(v) chol2inv(chol(v)))
}

targetVariance <- function (obj, cluster = nlme::getGroups(obj)) {
  groups <- nlme::getGroups(obj)
  if (is.null(groups)) 
    groups <- cluster
  if (is.null(obj$modelStruct$corStruct)) {
    if (is.null(obj$modelStruct$varStruct)) {
      V_list <- matrix_list(rep(1, length(cluster)), cluster, 
                            "both")
    }
    else {
      wts <- nlme::varWeights(obj$modelStruct$varStruct)
      V_list <- matrix_list(1/wts^2, cluster, "both")
    }
  }
  else {
    R_list <- nlme::corMatrix(obj$modelStruct$corStruct)
    if (is.null(obj$modelStruct$varStruct)) {
      V_list <- R_list
    }
    else {
      sd_vec <- 1/nlme::varWeights(obj$modelStruct$varStruct)[order(order(groups))]
      sd_list <- split(sd_vec, groups)
      V_list <- Map(function(R, s) tcrossprod(s) * R, R = R_list, 
                    s = sd_list)
    }
  }
  tb_groups <- table(groups)
  tb_cluster <- table(cluster)
  if (length(tb_groups) < length(tb_cluster) | any(as.vector(tb_groups) != 
                                                   rep(as.vector(tb_cluster), length.out = length(tb_groups))) | 
      any(names(tb_groups) != rep(names(tb_cluster), length.out = length(tb_groups)))) {
    tb_cross <- table(groups, cluster)
    nested <- apply(tb_cross, 1, function(x) sum(x > 0) == 
                      1)
    if (!all(nested)) 
      stop("Random effects are not nested within clustering variable.")
    crosswalk <- data.frame(groups, cluster)
    V_list <- add_bdiag(small_mats = V_list, big_mats = matrix_list(rep(0, 
                                                                        length(cluster)), cluster, dim = "both"), crosswalk = crosswalk)
  }
  V_list
}

add_bdiag <- function(small_mats, big_mats, crosswalk) {
  small_indices <- lapply(split(crosswalk[[1]], crosswalk[[2]]), 
                          droplevels)
  big_indices <- unique(crosswalk)
  big_indices <- big_indices[[2]][order(big_indices[[1]])]
  small_mats <- split(small_mats, big_indices)
  Map(add_submatrices, indices = small_indices, small_mat = small_mats, 
      big_mat = big_mats)
}

adjust_est_mats <- function (type, est_mats, adjustments) {
  switch(type, CR0 = est_mats, 
         CR1 = lapply(est_mats, function(e) e * adjustments),
         CR1p = lapply(est_mats, function(e) e * adjustments),
         CR1S = lapply(est_mats, function(e) e * adjustments), 
         CR2 = Map(function(e, a) e %*% a, e = est_mats, a = adjustments), 
         CR3 = Map(function(e, a) e %*% a, e = est_mats, a = adjustments),
         CR4 = Map(function(e, a) a %*% e, e = est_mats, a = adjustments))
}

add_submatrices <- function(indices, small_mat, big_mat) {
  levs <- levels(indices)
  for (i in 1:length(levs)) {
    ind <- levs[i] == indices
    big_mat[ind, ind] <- big_mat[ind, ind] + small_mat[[i]]
  }
  big_mat
}

CR1S <- function (J, N, p) {
  sqrt(J * (N - 1)/((J - 1) * (N - p)))
}

CR2 <- function(M_U, U_list, UW_list, Theta_list, inverse_var = FALSE) {
  Theta_chol <- lapply(Theta_list, chol)
  if (inverse_var) {
    IH_jj <- IH_jj_list(M_U, U_list, UW_list)
    G_list <- Map(function(a, b, ih) 
      as.matrix(a %*% ih %*% b %*% t(a)), a = Theta_chol, 
      b = Theta_list, ih = IH_jj)
  }
  else {
    H_jj <- Map(function(u, uw) u %*% M_U %*% uw, u = U_list, 
                uw = UW_list)
    uwTwu <- Map(function(uw, th) uw %*% th %*% t(uw), uw = UW_list, 
                 th = Theta_list)
    MUWTWUM <- M_U %*% Reduce("+", uwTwu) %*% M_U
    G_list <- Map(function(thet, h, u, v) 
      as.matrix(v %*% (thet - h %*% thet - thet %*% t(h) +
                         u %*% MUWTWUM %*% t(u)) %*% t(v)), 
      thet = Theta_list, h = H_jj, u = U_list, v = Theta_chol)
  }
  Map(function(v, g) as.matrix(t(v) %*% matrix_power(g, -1/2) %*% 
                                 v), v = Theta_chol, g = G_list)
}

sub_f <- function (x, fac, dim) {
  function(f) switch(dim, row = x[fac == f, , drop = FALSE], 
                     col = x[, fac == f, drop = FALSE], both = x[fac == f, 
                                                                 fac == f,
                                                                 drop = FALSE])
}

matrix_power <- function (x, p, symmetric = TRUE, tol = -12) {
  eig <- eigen(x, symmetric = symmetric)
  val_p <- with(eig, ifelse(values > 10^tol, values^p, 0))
  with(eig, vectors %*% (val_p * t(vectors)))
}

IH_jj_list <- function (M, X_list, XW_list) {
  Map(function(x, xw) diag(nrow = nrow(x)) - x %*% M %*% xw, 
      x = X_list, xw = XW_list)
}