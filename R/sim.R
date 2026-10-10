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
#'@seealso \link{sim_gpcm}, \link{PP_4pl}
#'@example ./R/.examples_sim.R
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
#'@example ./R/.examples_simgpcm.R
#'
#'@seealso \link{sim_4pl}, \link{PP_gpcm}
#'
#'@export
sim_gpcm <- function(thres, alpha, theta) {
  items <- prepare_items(normalize_thres(thres), alpha, NULL, NULL, NULL, "gpcm",
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
