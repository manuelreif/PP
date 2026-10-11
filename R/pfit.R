#' Person-Fit statistics
#'
#' Compute the person fit statistics lz, lz*, infit and outfit for the 1-PL, 2-PL, 3-PL, 4-PL model, the GPCM and mixed tests, at the person estimates of \code{PP_4pl()}, \code{PP_gpcm()} or \code{PPall()}.
#'
#' All statistics are computed at the estimate \eqn{\hat\theta} of each person, over the items the person answered (missing responses are left out of every sum). With the observed response \eqn{x_i}, the expected score \eqn{E_i}, its variance \eqn{V_i} and fourth central moment \eqn{C_i} of item \eqn{i}, the residual is \eqn{Y_i = x_i - E_i}, and for the \eqn{n} answered items:
#'
#' \itemize{
#' \item \bold{infit} (weighted mean square, Wright & Masters, 1982): \eqn{\sum Y_i^2 / \sum V_i}, with variance \eqn{q^2 = \sum (C_i - V_i^2) / (\sum V_i)^2}.
#' \item \bold{outfit} (unweighted mean square): \eqn{\sum (Y_i^2 / V_i) / n}, with variance \eqn{q^2 = \sum (C_i / V_i^2) / n^2 - 1/n}.
#' \item Both mean squares \eqn{MS} are standardised by the Wilson-Hilferty transformation \eqn{t = (MS^{1/3} - 1) (3/q) + q/3}. \code{chisq} is \eqn{\sum Y_i^2 / V_i} with \code{df} \eqn{= n - 1} and the p-value of the \eqn{\chi^2} distribution.
#' \item \bold{lz} (Drasgow, Levine & Williams, 1985): the log-likelihood \eqn{l_0 = \sum \log P_i(x_i)}, standardised by its expectation and variance under the model, \eqn{(l_0 - \sum E \log P_i) / \sqrt{\sum Var \log P_i}}. \code{lz_unst} is \eqn{l_0 / n}.
#' \item \bold{lz*} (Snijders, 2001) corrects lz for the estimated \eqn{\theta}: with the item information \eqn{I}, \eqn{c_n = \sum Cov(\log P_i, s_i) / I} (\eqn{s_i} the score of item \eqn{i}) and the term \eqn{r_0} of the estimating equation (mle: 0, wle: \eqn{J / (2I)}, map: \eqn{(\mu - \theta) / \sigma^2}), lz* \eqn{= (l_0 - \sum E \log P_i + c_n r_0) / \sqrt{\sum Var \log P_i - c_n^2 I}}. For dichotomous items this is Snijders' lz*, for polytomous items and mixed tests the generalisation of Sinharay (2016).
#' }
#'
#' Person fit needs an estimate of type \code{"mle"}, \code{"wle"} or \code{"map"}. Persons without an estimate (status other than \code{"ok"} in \code{pp$estimates}, e.g. an extreme score with mle) get \code{NA}. With a single answered dichotomous item lz* is not defined (the corrected variance is 0) and the \eqn{\chi^2} p-value has no degree of freedom; both are \code{NA}.
#'
#' With \code{SE = TRUE} every statistic gets a jackknife standard error: for each answered item \eqn{j}, \eqn{\theta} is estimated without item \eqn{j} (the leave-one-out estimates of \code{JKpp()}) and the statistic \eqn{f_{(-j)}} is computed on the remaining items. With \eqn{L} answered items and \eqn{m} leave-one-out samples with a finite estimate, \eqn{SE = (L - 1) \sqrt{\sum_j (f_{(-j)} - \bar f)^2 / (m (m - 1))}}, which is \eqn{\sqrt{(L-1)/L \sum_j (f_{(-j)} - \bar f)^2}} when no sample is left out (Efron & Stein, 1981).
#'
#' Up to version 0.6.4, lz* summed over all items also for persons with missing responses, the GPCM infit and outfit used the observed instead of the possible categories and ignored the slopes, mixed tests were not supported, the jackknife standard error of lz used the prior SD as variance, and all results were rounded.
#'
#'@param respm	      Not needed: the responses are taken from \code{pp}. If given, it must be the response matrix of \code{pp}.
#'@param pp 		      An object of class \code{ppeo} from \code{PP_4pl()}, \code{PP_gpcm()} or \code{PPall()} with type \code{"mle"}, \code{"wle"} or \code{"map"}.
#'@param fitindices		Character vector of the desired person fit statistics, any of \code{c("lz", "lzstar", "infit", "outfit")}.
#'@param SE		        Logical: if \code{TRUE}, jackknife standard errors are computed.
#'
#' @return An object of class \code{PPfit}: a list with one matrix per statistic (persons in rows, in the order of the response matrix):
#'
#' \itemize{
#' \item \code{lz}: \code{lz}, \code{lz_unst}
#' \item \code{lzstar}: \code{lzstar}
#' \item \code{infit}: \code{infit} (mean square), \code{in_t}, \code{in_chisq}, \code{in_df}, \code{in_pv}
#' \item \code{outfit}: \code{outfit} (mean square), \code{ou_t}, \code{ou_chisq}, \code{ou_df}, \code{ou_pv}
#' \item with \code{SE = TRUE} also \code{lz_se}, \code{lzs_se}, \code{in_se}, \code{ou_se}.
#' }
#' \code{summary()} gives, per statistic, the number of persons, mean, SD, range and the number of persons flagged at level \code{alpha}.
#'
#' @rdname pfit
#' @seealso \link{PPall}, \link{PP_4pl}, \link{PP_gpcm}, \link{PPass}, \link{JKpp}
#'
#'@author Jan Steinfeld, Manuel Reif
#'@references
#'
#' Armstrong, R. D., Stoumbos, Z. G., Kung, M. T. & Shi, M. (2007). On the performance of the lz person-fit statistic.  \emph{Practical Assessment, Research & Evaluation}, \bold{12(16)}.
#'
#' De La Torre, J., & Deng, W. (2008). Improving Person-Fit Assessment by Correcting the Ability Estimate and Its Reference Distribution. Journal of Educational Measurement, \bold{45(2)}, 159-177.
#'
#' Drasgow, F., Levine, M. V. & Williams, E. A. (1985) Appropriateness measurement with polychotomous item response models and standardized indices. \emph{British Journal of Mathematical and Statistical Psychology}, \bold{38(1)}, 67--86.
#'
#' Efron, B., & Stein, C. (1981). The jackknife estimate of variance. \emph{The Annals of Statistics}, \bold{9(3)}, 586-596.
#'
#' Karabatsos, G. (2003) Comparing the Aberrant Response Detection Performance of Thirty-Six Person-Fit Statistics. \emph{Applied Measurement In Education}, \bold{16(4)}, 277--298.
#'
#' Magis, D., Raiche, G. & Beland, S. (2012) A didactic presentation of Snijders's l[sub]z[/sub] index of person fit with emphasis on response model selection and ability estimation. \emph{Journal of Educational and Behavioral Statistics}, \bold{37(1)}, 57--81.
#'
#' Meijer, R. R. & Sijtsma, K. (2001) Methodology review: Evaluating person fit. \emph{Applied Psychological Measurement}, \bold{25(2)}, 107--135.
#'
#' Molenaar, I. W. & Hoijtink, H. (1990) The many null distributions of person fit indices. \emph{Psychometrika}, \bold{55(1)}, 75--106.
#'
#' Reise, S. P. (1990). A comparison of item-and person-fit methods of assessing model-data fit in IRT.  \emph{Applied Psychological Measurement}, \bold{14(2)}, 127-137.
#'
#' Sinharay, S. (2016). Asymptotically correct standardization of person-fit statistics beyond dichotomous items. \emph{Psychometrika}, \bold{81(4)}, 992--1013.
#'
#' Snijders, T. B. (2001) Asymptotic null distribution of person fit statistics with estimated person parameter. \emph{Psychometrika}, \bold{66(3)}, 331--342.
#'
#' Wright, B. D. & Masters, G. N. (1990). Computation of OUTFIT and INFIT Statistics.  \emph{Rasch Measurement Transactions}, 3:4, 84-85.
#'
#' Wright, B. D., & Masters, G. N. (1982). \emph{Rating Scale Analysis. Rasch Measurement.} MESA Press, 5835 S. Kimbark Avenue, Chicago, IL 60637.
#'
#'@examples
#' ## dichotomous items
#'
#' set.seed(1337)
#' diffpar <- seq(-3, 3, length = 15)
#' sl      <- round(runif(15, 0.5, 1.5), 2)
#' la      <- round(runif(15, 0, 0.25), 2)
#' awm     <- sim_4pl(beta = diffpar, alpha = sl, lowerA = la, upperA = rep(1, 15),
#'                    theta = rnorm(100))
#'
#' # 3PL, WLE
#' res3plwle <- PP_4pl(respm = awm, thres = diffpar, slopes = sl, lowerA = la, type = "wle")
#'
#' # the responses are taken from the estimation object
#' fit3pl <- Pfit(pp = res3plwle, fitindices = c("lz", "lzstar", "infit", "outfit"))
#' head(fit3pl$lzstar)
#' summary(fit3pl)
#'
#' # with missing responses and jackknife standard errors
#' awm_na <- awm
#' awm_na[sample(length(awm), 300)] <- NA
#' res3plna <- PP_4pl(respm = awm_na, thres = diffpar, slopes = sl, lowerA = la, type = "wle")
#' head(Pfit(pp = res3plna, fitindices = c("lz", "infit"), SE = TRUE)$lz)
#'
#' ## GPCM
#'
#' thres <- rbind(0, c(-1, -0.5, 0), c(0.5, 0.8, 1), c(1.5, 2, 2.5))
#' slg   <- c(0.8, 1.2, 1)
#' respg <- sim_gpcm(thres = thres, alpha = slg, theta = rnorm(50))
#' resgwle <- PP_gpcm(respm = respg, thres = thres, slopes = slg, type = "wle")
#' summary(Pfit(pp = resgwle))
#'
#' ## GPCM and 4PL mixed
#'
#' THRESx <- matrix(c(-2, -1.23, 1.11, 3.48, 1, 2, -1, -0.2, 0.5, 1.3, -0.8, 1.5), nrow = 2)
#' THRESx[2, 1:3] <- NA
#' slm <- c(0.5, 1, 1.5, 1.1, 1, 0.98)
#' lam <- c(0.02, 0.1, 0, 0, 0, 0)
#' uam <- c(0.97, 0.91, 1, 1, 1, 1)
#' awmatrix <- matrix(c(1, 0, 1, 0, 1, 1, 1, 0, 0, 1,
#'                      2, 0, 0, 0, 0, 0, 0, 0, 0, 1,
#'                      1, 2, 2, 1, 1, 1, 1, 0, 0, 1), byrow = TRUE, nrow = 5)
#' resmixed <- PPall(respm = awmatrix, thres = THRESx, slopes = slm, lowerA = lam,
#'                   upperA = uam, type = "wle", model2est = findmodel(THRESx))
#' Pfit(pp = resmixed, fitindices = c("lz", "lzstar"))
#' @keywords Person fit LZ-Index Infit-Outfit
#' @export
Pfit <- function(respm, pp, fitindices, SE = FALSE) UseMethod("Pfit", pp)


#' @rdname pfit
#' @method Pfit ppeo
#' @export
Pfit.ppeo <- function(respm, pp, fitindices = c("lz", "lzstar", "infit", "outfit"), SE = FALSE) {
  call <- match.call()
  if (!pp$type %in% c("mle", "wle", "map")) {
    stop("Person fit needs an estimate of type 'mle', 'wle' or 'map' (here: '", pp$type, "').",
         call. = FALSE)
  }
  fitindices <- match.arg(fitindices, several.ok = TRUE)
  if (!is.logical(SE) || length(SE) != 1 || is.na(SE)) stop("'SE' must be TRUE or FALSE.", call. = FALSE)
  m <- pp$model
  X <- m$X
  if (!missing(respm)) check_same_resp(respm, X)
  which <- intersect(c("lz", "lzstar", "infit", "outfit"), fitindices)

  ok <- pp$estimates$status == "ok"
  theta <- ifelse(ok, pp$estimates$estimate, NA)
  out <- fit_stats(m$bank, X, theta, pp$type, m$mu, m$sigma2, which)
  if (SE) {
    se <- fit_se(m, X, theta, ok, pp$type, which)
    for (ix in which) out[[ix]] <- cbind(out[[ix]], se[[ix]])
  }
  structure(out, class = "PPfit", type = pp$type, call = call)
}

# respm given to Pfit must be the response matrix of the estimate
check_same_resp <- function(respm, X) {
  if (is.data.frame(respm)) respm <- as.matrix(respm)
  obs <- !is.na(X)
  same <- is.matrix(respm) && identical(dim(respm), dim(X)) &&
    identical(unname(is.na(respm)), unname(!obs)) && all(respm[obs] == X[obs])
  if (!same) {
    stop("'respm' is not the response matrix of 'pp'. Pfit takes the responses from 'pp', ",
         "'respm' can be left out.", call. = FALSE)
  }
}

# Wilson-Hilferty transformation of a mean square with variance q2
wilson_hilferty <- function(ms, q2) (ms^(1 / 3) - 1) * (3 / sqrt(q2)) + sqrt(q2) / 3

# The statistics in which (lz, lzstar, infit, outfit) at theta (NA = no
# value), from the item moments of pp_moments_cpp(), summed over the answered
# items. Returns a list of matrices, one row per person.
fit_stats <- function(bank, X, theta, type, mu, sigma2, which) {
  mo <- pp_moments_cpp(bank, X, theta)
  rs <- function(a) rowSums(a, na.rm = TRUE)
  n <- rowSums(!is.na(mo$E))
  none <- n == 0
  mat <- function(...) {
    r <- cbind(...)
    r[none, ] <- NA
    r
  }
  out <- list()
  if (any(c("lz", "lzstar") %in% which)) {
    W <- rs(mo$ll) - rs(mo$Ell)
    vll <- rs(mo$Vll)
  }
  if ("lz" %in% which) out$lz <- mat(lz = W / sqrt(vll), lz_unst = rs(mo$ll) / n)
  if ("lzstar" %in% which) {
    I <- rs(mo$info)
    cn <- rs(mo$Cs) / I
    r0 <- switch(type,
                 mle = 0,
                 wle = rs(mo$J) / (2 * I),
                 map = (mu - theta) / sigma2)
    # tau2 = 0 (up to rounding) when log P is a linear function of the score,
    # e.g. with one dichotomous item: lz* is not defined
    tau2 <- vll - cn^2 * I
    ok <- tau2 > sqrt(.Machine$double.eps) * vll
    out$lzstar <- mat(lzstar = ifelse(ok, (W + cn * r0) / sqrt(pmax(tau2, 0)), NA))
  }
  if (any(c("infit", "outfit") %in% which)) {
    Y2 <- (X - mo$E)^2
    chisq <- rs(Y2 / mo$V)
    df <- n - 1
    # no p-value without a degree of freedom (one answered item)
    pv <- rep(NA_real_, length(n))
    pv[df >= 1] <- stats::pchisq(chisq[df >= 1], df[df >= 1], lower.tail = FALSE)
  }
  if ("infit" %in% which) {
    sv <- rs(mo$V)
    ms <- rs(Y2) / sv
    out$infit <- mat(infit = ms, in_t = wilson_hilferty(ms, rs(mo$C - mo$V^2) / sv^2),
                     in_chisq = chisq, in_df = df, in_pv = pv)
  }
  if ("outfit" %in% which) {
    ms <- chisq / n
    out$outfit <- mat(outfit = ms, ou_t = wilson_hilferty(ms, rs(mo$C / mo$V^2) / n^2 - 1 / n),
                      ou_chisq = chisq, ou_df = df, ou_pv = pv)
  }
  out
}

# Jackknife SE of the statistics in which: each statistic recomputed at the
# leave-one-out estimate without item j, on the remaining items. Returns a list
# of one-column matrices (lz_se, lzs_se, in_se, ou_se).
fit_se <- function(m, X, theta, ok, type, which) {
  L <- rowSums(!is.na(X))
  run <- ok & L >= 2
  r <- jk_loo(m, type, theta, run, m$maxsteps, m$exac)
  loo <- r$loo
  vals <- lapply(stats::setNames(which, which), function(ix) matrix(NA_real_, nrow(X), ncol(X)))
  for (j in seq_len(ncol(X))) {
    rows <- which(!is.na(loo[, j]))
    if (!length(rows)) next
    Xj <- X[rows, , drop = FALSE]
    Xj[, j] <- NA
    f <- fit_stats(m$bank, Xj, loo[rows, j], type, m$mu[rows], m$sigma2[rows], which)
    for (ix in which) vals[[ix]][rows, j] <- f[[ix]][, 1]
  }
  nx <- sum(rowSums(r$bad) > 0)
  if (nx > 0) {
    warning(nx, " person(s) with leave-one-out samples without a finite estimate (extreme score, ",
            "bound of 'range' or not converged); these samples are left out of the jackknife SE.",
            if (type == "mle") " WLE avoids this in most cases.", call. = FALSE)
  }
  nm <- c(lz = "lz_se", lzstar = "lzs_se", infit = "in_se", outfit = "ou_se")
  lapply(stats::setNames(which, which), function(ix) {
    f <- vals[[ix]]
    k <- rowSums(!is.na(f))
    se <- (L - 1) * sqrt(rowSums((f - rowMeans(f, na.rm = TRUE))^2, na.rm = TRUE) / (k * (k - 1)))
    se[!run | k < 2] <- NA
    matrix(se, ncol = 1, dimnames = list(NULL, nm[[ix]]))
  })
}


#' @rdname pfit
#' @param x,object An object of class \code{PPfit}.
#' @param digits Number of significant digits to print.
#' @param ... Further arguments, passed to \code{print()}.
#' @export
print.PPfit <- function(x, digits = 4, ...) {
  print(do.call(cbind, unclass(x)), digits = digits, ...)
  invisible(x)
}

#' @rdname pfit
#' @param alpha Significance level: persons with \eqn{|z| > z_{1-\alpha/2}} (lz, lz*, \code{in_t}, \code{ou_t}) are counted as flagged.
#' @export
summary.PPfit <- function(object, alpha = 0.05, digits = 4, ...) {
  check_alpha(alpha)
  z <- stats::qnorm(1 - alpha / 2)
  cols <- list(lz = "lz", lzstar = "lzstar", infit = c("infit", "in_t"), outfit = c("outfit", "ou_t"))
  vals <- unlist(lapply(names(object), function(ix) {
    lapply(stats::setNames(nm = cols[[ix]]), function(sp) object[[ix]][, sp])
  }), recursive = FALSE)
  res <- fit_table(vals, z)
  cat("Person fit at the", attr(object, "type"), "estimates,", nrow(object[[1]]), "persons\n")
  cat("flagged: |z| >", format(z, digits = 3), "(alpha =", alpha, ")\n\n")
  print(res, digits = digits, row.names = FALSE, ...)
  invisible(res)
}

check_alpha <- function(alpha) {
  check_scalar(alpha, "alpha")
  if (!(alpha > 0 && alpha < 1)) stop("'alpha' must be between 0 and 1.", call. = FALSE)
}

# Summary table of fit statistics (a named list of vectors); the z statistics
# lz, lzstar, in_t and ou_t count the persons with |z| above the critical value z
fit_table <- function(vals, z) {
  do.call(rbind, lapply(names(vals), function(sp) {
    v <- vals[[sp]]
    v <- v[is.finite(v)]
    data.frame(statistic = sp, n = length(v), mean = mean(v), sd = stats::sd(v),
               min = if (length(v)) min(v) else NA, max = if (length(v)) max(v) else NA,
               flagged = if (sp %in% c("lz", "lzstar", "in_t", "ou_t")) sum(abs(v) > z) else NA_integer_)
  }))
}
