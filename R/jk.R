#' Run a jackknife
#'
#' This function uses a jackknife approach to compute person parameters. The jackknife ability measure is based on primarily estimated models (\code{PP_4pl()}, \code{PP_gpcm()} or \code{PPall()}) - so the function is applied on the estimation objects, and jackknifed ability measures are returned.
#'
#' For each person, every answered item is left out once and the ability is estimated again from the remaining items, with the same estimation type, prior and \code{range} as in \code{estobj} (MLE, WLE, MAP and robust estimates start at the full estimate; EAP estimates use the posterior grid of the full estimate). Items a person did not answer are not left out: with \eqn{L} answered items and the leave-one-out estimates \eqn{\hat\theta_{(-j)}}, the pseudovalues are \eqn{L\hat\theta - (L-1)\hat\theta_{(-j)}}. The jackknife estimate is their mean, median or AMT (\code{cmeth}), the standard error is \eqn{\sqrt{\sum_j (ps_j - \bar{ps})^2 / (L(L-1))}} (Wainer & Wright, 1980).
#'
#' A leave-one-out sample may have no finite estimate, e.g. with \code{type = "mle"} when the only wrong answer is left out. Such samples (and samples at the bound of \code{range} or not converged) are left out of the person's jackknife, counted in the column \code{excluded}, and a warning is given; WLE avoids this in most cases. A person whose full estimate has no solution (status \code{"extreme"}, \code{"no_response"}, \code{"boundary"} or \code{"not_converged"} in \code{estobj$estimates}) or who has fewer than two usable leave-one-out samples (status \code{"too_few_items"}) gets \code{NA}.
#'
#' Up to version 0.6.4 the standard error mixed the leave-one-out estimates with the formula for the pseudovalues and was too small by a factor of about \eqn{L-1}, all items counted as answered, and leave-one-out samples without a finite estimate were dropped silently.
#'
#' \bold{AMT-robustified jackknife:} When choosing \code{cmeth = "AMT"}, the pseudovalues are combined as in scheme 4 of Wainer & Wright (1980): they are split into the pseudovalues of the right and of the wrong answers (polytomous items: answers at or above the expected score at \eqn{\hat\theta} and below it), each group is summarised by Andrews' sine M-estimator (AMT), one of the winners in the Princeton Robustness Study (Andrews et al., 1972), and the two values are averaged with the group sizes as weights. Within a group, \eqn{T} solves \eqn{\sum_j \psi((ps_j - T)/s) = 0} with \eqn{\psi(x) = \sin(x/2.1)} for \eqn{|x| \le 2.1\pi} and 0 otherwise, where \eqn{s} is the median absolute deviation of the group's pseudovalues from their median; pseudovalues further than \eqn{2.1 \pi s} from \eqn{T} get no weight. Up to version 0.6.4 the AMT had a sign error, no scale and maximised the sum of the sines.
#'
#' @param estobj An object which originates from using \code{PP_gpcm()}, \code{PP_4pl()} or \code{PPall()}.
#'
#' @param ...  More input.
#' @export
#' @rdname Jkpp
#' @references
#' Wainer, H., & Wright, B. D. (1980). Robust estimation of ability in the Rasch model. Psychometrika, 45(3), 373-391.
#'
#' Andrews, D. F., Bickel, P. J., Hampel, F. R., Huber, P. J., Rogers, W. H., & Tukey, J. W. (1972). Robust estimates of location: Survey and advances. Princeton University Press.
#'
#' @seealso \link{PP_gpcm}, \link{PP_4pl}, \link{PPall}
#'
#'@example ./R/.examples_JK.R
#'@author Manuel Reif
JKpp <- function(estobj, ...) UseMethod("JKpp")


#' @rdname Jkpp
#' @param cmeth Choose the centering method, to summarize the pseudovalues of a person to one single ability estimate. There are three valid entries: "mean", "median" and "AMT" (see Details for further description).
#' @param maxsteps The maximum number of Newton steps per leave-one-out sample. If \code{NULL} (default), the value of \code{estobj} is used.
#' @param exac How accurate are the leave-one-out estimates supposed to be? If \code{NULL} (default), the value of \code{estobj} is used.
#' @param fullmat Default = FALSE. If TRUE, the function returns the whole jackknife matrix of leave-one-out estimates, which is the basis for the jackknife estimator.
#' @param ctrl Not used any more.
#'
#' @return An object of class \code{jk}, a list with
#' \item{\code{estimates}}{A data.frame with one row per person (in the order of \code{respm}): \code{estimate} and \code{SE} of the jackknife, \code{L} the number of answered items, \code{excluded} the number of leave-one-out samples without a finite estimate and \code{status} (\code{"ok"}, \code{"too_few_items"} or the status of the full estimate).}
#' \item{\code{resjk}}{The matrix of \code{estimate} and \code{SE}, as in earlier versions.}
#' \item{\code{type}, \code{cmeth}, \code{call}}{The estimation type of \code{estobj}, the centering method and the call.}
#' \item{\code{jk_mat}}{Only with \code{fullmat = TRUE}: the persons x items matrix of leave-one-out estimates, \code{NA} for items not answered and excluded samples.}
#'
#' @method JKpp ppeo
#' @export
JKpp.ppeo <- function(estobj, cmeth = "mean", maxsteps = NULL, exac = NULL,
                      fullmat = FALSE, ctrl = list(), ...)
{
  call <- match.call()
  attr(call, "date") <- date()
  attr(call, "version") <- packageVersion("PP")

  cmeth <- match.arg(cmeth, c("mean", "median", "AMT"))
  m <- estobj$model
  if (is.null(maxsteps)) maxsteps <- m$maxsteps
  if (is.null(exac)) exac <- m$exac
  check_scalar(maxsteps, "maxsteps", whole = TRUE)
  check_scalar(exac, "exac")

  X <- m$X
  type <- estobj$type
  theta <- estobj$estimates$estimate
  full <- estobj$estimates$status
  ## the jackknife runs for persons with a solution and L >= 2 answered items
  L <- rowSums(!is.na(X))
  usable <- full == "ok"
  run <- usable & L >= 2

  r <- jk_loo(m, type, theta, run, maxsteps, exac)
  loo <- r$loo
  excluded <- rowSums(r$bad)

  ## pseudovalues ps = L theta - (L - 1) theta_(-j)
  ps <- L * theta - (L - 1) * loo
  nps <- rowSums(!is.na(ps))
  status <- ifelse(usable & nps < 2, "too_few_items", as.character(full))
  est <- switch(cmeth,
                mean = rowMeans(ps, na.rm = TRUE),
                median = apply(ps, 1, stats::median, na.rm = TRUE),
                AMT = {
                  right <- X >= pp_moments_cpp(m$bank, X, ifelse(run, theta, 0))$E
                  vapply(seq_len(nrow(ps)), function(i) jk_amt_ww(ps[i, ], right[i, ]), 0)
                })
  se <- sqrt(rowSums((ps - rowMeans(ps, na.rm = TRUE))^2, na.rm = TRUE) / (nps * (nps - 1)))
  est[status != "ok"] <- NA
  se[status != "ok"] <- NA

  nx <- sum(excluded > 0)
  if (nx > 0) {
    warning(nx, " person(s) with leave-one-out samples without a finite estimate (extreme score, ",
            "bound of 'range' or not converged); these samples are left out of the jackknife ",
            "(column 'excluded' in $estimates).",
            if (type == "mle") " WLE avoids this in most cases.", call. = FALSE)
  }

  estimates <- data.frame(estimate = unname(est), SE = unname(se), L = as.integer(L),
                          excluded = as.integer(excluded),
                          status = factor(status, levels = jk_status))
  resjk <- cbind(estimate = unname(est), SE = unname(se))
  res <- list(estimates = estimates, resjk = resjk, type = type, cmeth = cmeth, call = call)
  if (fullmat) res$jk_mat <- loo
  structure(res, class = "jk")
}

jk_status <- c(pp_status, "too_few_items")

# Leave-one-out estimates (persons x items) of the persons in run, for the
# model m (estobj$model) and the full estimates theta: NA for items not
# answered, persons not in run and samples without a finite estimate; bad
# marks those samples. Used by JKpp() and Pfit(SE = TRUE).
jk_loo <- function(m, type, theta, run, maxsteps, exac) {
  X <- m$X
  if (type == "eap") {
    loo <- pp_posterior_cpp(m$bank, X, m$mu, m$sigma2, jk = TRUE)$loo
    loo[!run, ] <- NA
    bad <- matrix(FALSE, nrow(X), ncol(X))
  } else {
    r <- pp_jackknife_cpp(m$bank, X, type, m$mu, m$sigma2, m$H, ifelse(run, theta, NA),
                          m$range, exac, as.integer(maxsteps))
    loo <- r$loo
    bad <- !is.na(r$status) & r$status != 1L
    loo[bad] <- NA
  }
  dimnames(loo) <- dimnames(X)
  list(loo = loo, bad = bad)
}

# AMT-robustified jackknife of Wainer & Wright (1980, scheme 4): the AMT of
# the pseudovalues of the right answers (right: response >= expected score at
# the estimate) and of the wrong ones, averaged with the group sizes as weights.
jk_amt_ww <- function(ps, right) {
  ok <- !is.na(ps)
  if (!any(ok)) return(NA_real_)
  ps <- ps[ok]
  up <- right[ok]
  n <- c(sum(up), sum(!up))
  t <- c(if (n[1]) jk_amt(ps[up]) else 0, if (n[2]) jk_amt(ps[!up]) else 0)
  sum(n * t) / sum(n)
}

# Andrews' sine M-estimate (AMT) of location: T solves
# sum(psi((x - T) / (c * s))) = 0 with psi(u) = sin(u) for |u| <= pi, else 0,
# and s the median absolute deviation from the median. IRLS with the weights
# psi(u) / u, started at the median.
jk_amt <- function(x, c = 2.1, tol = 1e-10, maxit = 100) {
  if (!length(x)) return(NA_real_)
  tt <- stats::median(x)
  s <- stats::median(abs(x - tt))
  if (s == 0) return(tt)
  for (it in seq_len(maxit)) {
    u <- (x - tt) / (c * s)
    w <- ifelse(abs(u) > pi, 0, ifelse(u == 0, 1, sin(u) / u))
    tn <- sum(w * x) / sum(w)
    done <- abs(tn - tt) < tol
    tt <- tn
    if (done) break
  }
  tt
}
