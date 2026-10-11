#' Draw Plausible values
#'
#' This function draws \code{npv} plausible values for each person from their posterior density.
#'
#' Plausible values need a prior, so \code{estobj} must come from an estimation with \code{type = "eap"} or \code{type = "map"}. The posterior of each person combines the likelihood of the answered items with the normal prior \code{N(mu, sigma2)} of the person, as given to \code{PP_4pl()}, \code{PP_gpcm()} or \code{PPall()} (default \code{N(0, 1)}). It is computed on a grid of ability values, the same as for \code{type = "eap"}: one grid for all persons, and a finer grid around the posterior mode for persons whose posterior is very narrow or lies outside the range of the prior. Both types give the same plausible values, since they share the posterior; the point estimate itself is not used. MLE, WLE and robust estimates have no prior, and \code{PV()} stops with an error for them. Missing responses are skipped; a person without any response gets draws from the prior.
#'
#' With \code{approx = FALSE} each value is drawn from the posterior on the grid: a grid point is chosen with its posterior probability, then the value is drawn uniformly within its cell. The draws are independent. With \code{approx = TRUE} (default) the draws come from the normal distribution with the posterior mean and the posterior SD of the person (the EAP estimate and its SE).
#'
#' Up to version 0.6.4 \code{PV()} accepted any estimation type, \code{approx = FALSE} used a Metropolis-Hastings algorithm and \code{approx = TRUE} drew from the normal distribution around the estimate of \code{estobj} with its SE. For an MLE or WLE estimate this is not a posterior and the plausible values were too widely spread. The arguments \code{thinning}, \code{burnin} and \code{mult} of the sampler are no longer used.
#'
#' @param estobj An object which originates from using \code{PP_4pl()}, \code{PP_gpcm()} or \code{PPall()} with \code{type = "eap"} or \code{type = "map"}.
#' @param npv The number of plausible values per person - default is 10.
#' @param approx If \code{TRUE} (default), the plausible values are drawn from a normal approximation of the posterior. If \code{FALSE}, they are drawn from the posterior itself.
#' @param ... Not used. \code{thinning}, \code{burnin} and \code{mult} of version 0.6.4 give a warning and are ignored.
#'
#' @return An object of class \code{pv}, a list with \code{pvdraws}, a matrix with one row per person (in the order of \code{respm}) and \code{npv} columns, and \code{call}.
#'
#'@references Mislevy, R. J. (1991). Randomization-based inference about latent variables from complex samples. Psychometrika, 56(2), 177-196.
#'
#'Von Davier, M., Gonzalez, E., & Mislevy, R. (2009). What are plausible values and why are they useful. IERI monograph series, 2, 9-36.
#'
#' @seealso \link{PP_gpcm}, \link{PP_4pl}, \link{PPall}, \link{JKpp}
#'
#' @rdname PV
#'@examples
#' ## 2PL model
#'
#' set.seed(1522)
#' # difficulties and slopes
#' diffpar <- seq(-3, 3, length = 12)
#' sl      <- round(runif(12, 0.5, 1.5), 2)
#'
#' # response matrix
#' awm <- sim_4pl(beta = diffpar, alpha = sl, lowerA = rep(0, 12), upperA = rep(1, 12),
#'                theta = rnorm(10))
#'
#' # plausible values need a prior: EAP (or MAP) estimation, here with the default N(0, 1)
#' res2pl <- PP_4pl(respm = awm, thres = diffpar, slopes = sl, type = "eap")
#'
#' # 10 plausible values per person, normal approximation of the posterior
#' res_pv <- PV(res2pl)
#' summary(res_pv)
#'
#' # 200 draws from the posterior itself; their means approximate the EAP
#' res_pv2 <- PV(res2pl, npv = 200, approx = FALSE)
#' cbind(pv_mean = rowMeans(res_pv2$pvdraws),
#'       eap = res2pl$estimates$estimate)
#'
#'
#' ## GPCM model with person-specific priors
#'
#' THRES  <- matrix(c(-2,-1.23,1.11,3.48,1
#'                    ,2,-1,-0.2,0.5,1.3,-0.8,1.5),nrow=2)
#' sl     <- c(0.5,1,1.5,1.1,1,0.98)
#' awmatrix <- matrix(c(1,0,2,0,1,1,1,0,0,1
#'                      ,2,0,0,0,0,0,0,0,0,1,1,2,2,1,1,1,1,0,0,1),byrow=TRUE,nrow=5)
#'
#' resgpcm <- PP_gpcm(respm = awmatrix, thres = THRES, slopes = sl, type = "eap",
#'                    mu = c(0, 0, 1, 1, 1), sigma2 = 0.5)
#' PV(resgpcm, npv = 5, approx = FALSE)
#'
#'
#' ## GPCM and 4PL items mixed
#'
#' THRESx <- THRES
#' THRESx[2,1:3] <- NA
#' la     <- c(0.02,0.1,0,0,0,0)
#' ua     <- c(0.97,0.91,1,1,1,1)
#' awmatrix <- matrix(c(1,0,1,0,1,1,1,0,0,1
#'                      ,2,0,0,0,0,0,0,0,0,1
#'                      ,1,2,2,1,1,1,1,0,0,1),byrow=TRUE,nrow=5)
#'
#' respcm <- PPall(respm = awmatrix, thres = THRESx, slopes = sl, lowerA = la, upperA = ua,
#'                 type = "eap", model2est = findmodel(THRESx))
#' res_mixedpv <- PV(respcm, npv = 200, approx = FALSE)
#'
#' # quantiles of the draws per person
#' apply(res_mixedpv$pvdraws, 1, quantile)
#' @export
#' @author Manuel Reif
PV <- function(estobj, ...) UseMethod("PV")


#' @rdname PV
#' @export
PV.ppeo <- function(estobj, npv = 10, approx = TRUE, ...) {
  call <- match.call()
  attr(call, "date") <- date()
  attr(call, "version") <- packageVersion("PP")

  old <- intersect(names(list(...)), c("thinning", "burnin", "mult"))
  if (length(old)) {
    warning("'", paste(old, collapse = "', '"), "' no longer used: plausible values are drawn ",
            "from the posterior on a grid, without a Metropolis-Hastings sampler.", call. = FALSE)
  }
  if (!estobj$type %in% c("eap", "map")) {
    stop("Plausible values are draws from the posterior and need a prior: estimate with ",
         "type = \"eap\" (or \"map\") and the prior in 'mu'/'sigma2', not with type = \"",
         estobj$type, "\".", call. = FALSE)
  }
  if (!is.numeric(npv) || length(npv) != 1 || is.na(npv) || npv < 1 || npv != round(npv)) {
    stop("'npv' must be a positive whole number.", call. = FALSE)
  }
  if (!is.logical(approx) || length(approx) != 1 || is.na(approx)) {
    stop("'approx' must be TRUE or FALSE.", call. = FALSE)
  }
  npv <- as.integer(npv)

  m <- estobj$model
  N <- nrow(m$X)
  if (approx) {
    post <- pp_posterior_cpp(m$bank, m$X, m$mu, m$sigma2)
    draws <- matrix(stats::rnorm(N * npv, rep(post$eap, each = npv), rep(post$psd, each = npv)),
                    N, npv, byrow = TRUE)
  } else {
    draws <- pp_posterior_cpp(m$bank, m$X, m$mu, m$sigma2, npv)$draws
  }
  colnames(draws) <- paste0("PV", seq_len(npv))
  structure(list(pvdraws = draws, call = call), class = "pv")
}
