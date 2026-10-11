# Simulators. The item parameters are checked by the same code as in the
# estimation functions (normalize_thres(), prepare_items()); the responses are
# drawn in C++ (pp_sim_cpp) from the category probabilities of the item kernel.


#' Simulate data for 1/2/3/4-pl model
#'
#' This function returns a dichotomous matrix of simulated responses under given item and person parameters.
#'
#' \code{alpha}, \code{lowerA} and \code{upperA} may also be single numbers,
#' which are then used for every item. A person with \code{NA} as person
#' parameter gets a row of \code{NA}.
#'
#'@param beta A numeric vector which contains the difficulty parameters for each item.
#'@param alpha A numeric vector, which contains the slope parameters for each item.
#'@param lowerA A numeric vector, which contains the lower asymptote parameters (kind of guessing parameter) for each item.
#'@param upperA numeric vector, which contains the upper asymptote parameters for each item.
#'@param theta A numeric vector which contains person parameters.
#'
#'@return An integer matrix with one row per person and one column per item.
#'
#'@seealso \link{sim_gpcm}, \link{sim_mixed}, \link{PP_4pl}
#'@examples
#' set.seed(1700)
#' theta <- rnorm(200)
#' b <- seq(-2, 2, length.out = 7)
#'
#' # 1PL (Rasch) model: slopes 1, no asymptotes
#' x1 <- sim_4pl(beta = b, alpha = 1, lowerA = 0, upperA = 1, theta = theta)
#' colMeans(x1)
#'
#' # 4PL model
#' x4 <- sim_4pl(beta = b, alpha = c(1, 1.1, 0.9, 0.9, 1.5, 1.1, 1),
#'               lowerA = c(0, 0, 0.2, 0.15, 0.04, 0, 0.21),
#'               upperA = c(0.9, 0.98, 0.97, 1, 1, 1, 0.97), theta = theta)
#' head(x4)
#'
#'@author Manuel Reif
#'
#'@export
sim_4pl <- function(beta, alpha, lowerA, upperA, theta) {
  if (!is.numeric(beta) || !is.null(dim(beta)) || length(beta) == 0 || any(!is.finite(beta))) {
    stop("'beta' must be a numeric vector of finite difficulties.", call. = FALSE)
  }
  items <- prepare_items(normalize_thres(beta), alpha, lowerA, upperA, NULL, "4pl",
                         slope_name = "alpha")
  pp_sim_cpp(items$bank, check_theta(theta))
}


#' Simulate data for the gpcm model
#'
#' This function returns an integer matrix of simulated responses under given item and person parameters.
#'
#' \code{alpha} may also be a single number, which is then used for every item.
#' A person with \code{NA} as person parameter gets a row of \code{NA}.
#'
#'@param thres A numeric matrix which contains threshold parameters for each item. The first row may contain zeroes only (as in \code{\link{PP_gpcm}}); otherwise a row of zeroes is added.
#'@param alpha A numeric vector, which contains the slope parameters - one parameter per item is expected.
#'@param theta A numeric vector which contains person parameters.
#'
#'@return An integer matrix with one row per person and one column per item.
#'
#'@examples
#' set.seed(1750)
#' # thresholds of six items with three categories (the row of zeroes is optional)
#' thres <- rbind(0, matrix(c(-2, -1.23, 1.11, 3.48, 1, 2, -1, -0.2, 0.5, 1.3, -0.8, 1.5),
#'                          nrow = 2))
#' x <- sim_gpcm(thres = thres, alpha = c(0.5, 1, 1.5, 1.1, 1, 0.98), theta = rnorm(100))
#' head(x)
#'
#' # the last two items get a fourth category
#' thres4 <- rbind(thres, c(NA, NA, NA, NA, 1.7, 1))
#' apply(sim_gpcm(thres = thres4, alpha = 1, theta = rnorm(100)), 2, max)
#'
#'@seealso \link{sim_4pl}, \link{sim_mixed}, \link{PP_gpcm}
#'
#'@export
sim_gpcm <- function(thres, alpha, theta) {
  items <- prepare_items(normalize_thres(thres), alpha, NULL, NULL, NULL, "gpcm",
                         slope_name = "alpha")
  pp_sim_cpp(items$bank, check_theta(theta))
}


#' Simulate data for mixed tests (4PL and GPCM items)
#'
#' This function returns an integer matrix of simulated responses for a test with dichotomous (1/2/3/4-PL) and polytomous (GPCM) items, with the item parameters in the layout of \code{\link{PPall}}.
#'
#' \code{alpha}, \code{lowerA} and \code{upperA} may also be single numbers, which are then used for every item. \code{lowerA} and \code{upperA} only apply to the 4PL items. A person with \code{NA} as person parameter gets a row of \code{NA}. The data of a pure 4PL or GPCM test are the same as those of \code{\link{sim_4pl}} or \code{\link{sim_gpcm}} with the same seed.
#'
#'@param thres A numeric matrix with the thresholds of each item in its columns, as in \code{\link{PPall}}: for a 4PL item the difficulty, for a GPCM item its thresholds, \code{NA} below the last one. The first row may contain zeroes only; otherwise a row of zeroes is added.
#'@param alpha A numeric vector of slope parameters. If \code{NULL} (default), all slopes are 1.
#'@param lowerA A numeric vector of lower asymptotes (4PL items). If \code{NULL} (default), 0.
#'@param upperA A numeric vector of upper asymptotes (4PL items). If \code{NULL} (default), 1.
#'@param model2est A character vector with \code{"4PL"} or \code{"GPCM"} for each item. If \code{NULL} (default), \code{\link{findmodel}(thres)}: items with one threshold are 4PL items.
#'@param theta A numeric vector which contains person parameters.
#'
#'@return An integer matrix with one row per person and one column per item.
#'
#'@examples
#' set.seed(1800)
#' # four 3PL items and two GPCM items with three categories
#' thres <- rbind(0, c(-1, -0.3, 0.4, 1.2, -0.5, 0.2), c(NA, NA, NA, NA, 0.8, 1.4))
#' x <- sim_mixed(thres = thres, alpha = c(1, 1.2, 0.8, 1.1, 1, 0.9),
#'                lowerA = c(0.2, 0.1, 0.15, 0.2, 0, 0), theta = rnorm(300))
#' head(x)
#'
#' # and back: estimate the persons with the same parameters
#' res <- PPall(x, thres = thres, slopes = c(1, 1.2, 0.8, 1.1, 1, 0.9),
#'              lowerA = c(0.2, 0.1, 0.15, 0.2, 0, 0))
#' summary(res$estimates$estimate)
#'
#'@seealso \link{sim_4pl}, \link{sim_gpcm}, \link{PPall}
#'
#'@export
sim_mixed <- function(thres, alpha = NULL, lowerA = NULL, upperA = NULL, model2est = NULL,
                      theta) {
  items <- prepare_items(normalize_thres(thres), alpha, lowerA, upperA, model2est, "mixed",
                         slope_name = "alpha")
  pp_sim_cpp(items$bank, check_theta(theta))
}


# Person parameters for the simulators: numeric, finite or NA
check_theta <- function(theta) {
  if (!is.numeric(theta) && !(is.logical(theta) && all(is.na(theta)))) {
    stop("'theta' must be numeric.", call. = FALSE)
  }
  if (any(is.infinite(theta))) stop("'theta' must be finite (or NA).", call. = FALSE)
  as.numeric(theta)
}
