# Estimation layer: pp_estimate() runs the estimator on the inputs prepared by
# pp_prepare() and returns the result object built by new_ppeo(). No rows are
# removed or re-inserted: persons without answers and extreme scores are
# handled per person and marked in the status.

pp_status <- c("ok", "extreme", "boundary", "no_response", "not_converged")

# prep: result of pp_prepare(); call: the call of the wrapper; cls: "fourpl",
# "gpcm" or "gpcm4pl"; label: model description for the verbose message.
pp_estimate <- function(prep, call, cls, label) {
  X <- prep$X
  N <- nrow(X)
  type <- prep$type
  if (prep$verbose) message("Estimating: ", label, ", type = ", type)
  if (type == "robust" && prep$family != "4pl") {
    warning("Robust estimation for GPCM is still very experimental! \n")
  }

  ## duplicated response patterns with the same prior and start value are
  ## estimated once; pos maps every person to its pattern
  if (prep$ctrl$killdupli) {
    key <- do.call(paste, c(as.data.frame(X), list(prep$mu, prep$sigma2, prep$start, sep = "\r")))
    first <- !duplicated(key)
    pos <- match(key, key[first])
  } else {
    first <- rep(TRUE, N)
    pos <- seq_len(N)
  }
  Xu <- X[first, , drop = FALSE]

  if (type == "eap") {
    res <- eap_old(prep, Xu, first)
    res$status <- ifelse(rowSums(!is.na(Xu)) == 0, 4L, 1L)
  } else {
    res <- pp_estimate_cpp(prep$bank, Xu, type, prep$mu[first], prep$sigma2[first], prep$H,
                           prep$start[first], prep$range, prep$exac, prep$maxsteps)
  }
  res <- lapply(res, function(v) v[pos])
  status <- factor(pp_status[res$status], levels = pp_status)

  nb <- sum(status == "boundary")
  nn <- sum(status == "not_converged")
  if (nb > 0) {
    warning(nb, " person(s) without a solution inside 'range' = c(", prep$range[1], ", ",
            prep$range[2], "): the estimate is set to the bound (status \"boundary\" in ",
            "$estimates).", call. = FALSE)
  }
  if (nn > 0) {
    warning(nn, " person(s) not converged within maxsteps = ", prep$maxsteps,
            " (status \"not_converged\" in $estimates).", call. = FALSE)
  }

  new_ppeo(prep, res$theta, res$se, res$nsteps, status, call, cls)
}

# EAP with the quadrature code of 0.6.4 (R/eap.R), until block 5 replaces it.
# Persons without answers get the prior mean and SD.
eap_old <- function(prep, Xu, first) {
  mu <- prep$mu[first]
  sigma2 <- prep$sigma2[first]
  r <- switch(prep$family,
    "4pl" = eap_4pl(Xu, prep$thres, prep$slopes, lowerA = prep$lowerA, upperA = prep$upperA,
                    mu = mu, sigma2 = sigma2),
    gpcm  = eap_gpcm(Xu, prep$thres, prep$slopes, mu = mu, sigma2 = sigma2),
    mixed = eap_mixed(Xu, prep$thres, prep$slopes, lowerA = prep$lowerA, upperA = prep$upperA,
                      mu = mu, sigma2 = sigma2, model2est = prep$model2est))
  r <- matrix(r, ncol = 2)
  list(theta = r[, 1], se = r[, 2], nsteps = integer(nrow(Xu)))
}

# The result object (class c(cls, "ppeo")):
#   estimates  data.frame estimate, SE, nsteps, status; row names of respm
#   type, call
#   model      the prepared inputs (pp_prepare())
#   resPP      list(resPP = n x 2 matrix estimate/SE, nsteps = max(nsteps)),
#              the result layout of 0.6.4, kept as an alias
#   ipar       the inputs in the layout of 0.6.4 for JKpp, PV and Pfit until
#              they are rebuilt; respm holds the persons with an SE (JKpp
#              selects the same rows)
new_ppeo <- function(prep, theta, se, nsteps, status, call, cls) {
  rn <- rownames(prep$X)
  estimates <- data.frame(estimate = theta, SE = se, nsteps = as.integer(nsteps),
                          status = status, row.names = rn)
  respp <- cbind(estimate = theta, SE = se)
  rownames(respp) <- rn

  ipar <- list(respm = prep$X[!is.na(se), , drop = FALSE], thres = prep$thres,
               slopes = prep$slopes, lowerA = prep$lowerA, upperA = prep$upperA,
               theta_start = prep$start, mu = prep$mu, sigma2 = prep$sigma2,
               cont = prep$ctrl, model2est = prep$model2est, H = prep$H)
  if (cls == "gpcm") ipar$lowerA <- ipar$upperA <- NULL
  if (cls != "gpcm4pl") ipar$model2est <- NULL

  structure(list(estimates = estimates, type = prep$type, model = prep, call = call,
                 resPP = list(resPP = respp, nsteps = max(0L, nsteps)), ipar = ipar),
            class = c(cls, "ppeo"))
}
