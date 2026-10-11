# Input layer: every estimation function checks and normalises its arguments
# here, in one place. pp_prepare() returns the normalised inputs ("prep") and
# the canonical item bank, which the C++ item kernel will read.


# --- threshold matrix ------------------------------------------------------

# Bring thres into the internal form: a matrix with one column per item, a
# first row of zeros, the thresholds of item j in rows 2 .. maxsc[j] + 1 and NA
# below. A vector holds one threshold (difficulty) per item. A matrix already
# has the zero row if it has at least two rows and its first row is all 0;
# otherwise a zero row is added on top. Returns list(thres, maxsc).
normalize_thres <- function(thres) {
  if (is.data.frame(thres)) thres <- as.matrix(thres)
  if (!is.numeric(thres) && !all(is.na(thres))) stop("'thres' must be numeric.", call. = FALSE)
  if (!is.matrix(thres)) thres <- matrix(thres, nrow = 1, dimnames = list(NULL, names(thres)))
  storage.mode(thres) <- "double"
  if (ncol(thres) == 0) stop("'thres' has no items.", call. = FALSE)

  has_zero_row <- nrow(thres) >= 2 && isTRUE(all(thres[1, ] == 0))
  if (!has_zero_row) {
    thres <- rbind(0, thres, deparse.level = 0)
  }
  dimnames(thres) <- if (!is.null(colnames(thres))) list(NULL, colnames(thres))

  obs <- !is.na(thres[-1, , drop = FALSE])
  maxsc <- colSums(obs)
  if (any(maxsc == 0)) {
    stop("'thres': no threshold for item(s) ",
         paste(which(maxsc == 0), collapse = ", "), ".", call. = FALSE)
  }
  # NA is only allowed below the last threshold of an item
  if (any(!obs[1, ])) {
    stop("'thres': the first threshold of item(s) ",
         paste(which(!obs[1, ]), collapse = ", "), " is NA.", call. = FALSE)
  }
  gap <-vapply(seq_len(ncol(obs)), function(j) any(!obs[seq_len(maxsc[j]), j]), TRUE)
  if (any(gap)) {
    stop("'thres': NA between the thresholds of item(s) ",
         paste(which(gap), collapse = ", "),
         " (NA is only allowed below the last threshold).", call. = FALSE)
  }
  if (any(is.infinite(thres))) stop("'thres' must not contain infinite values.", call. = FALSE)

  list(thres = thres, maxsc = unname(maxsc))
}


#' Create a model-type vector template
#'
#' This is a small helper function which creates a vector template  quick and easily for the \code{PPall()} function. Modify this template as you like.
#'
#' This function tries to guess the model which was applied to each item by using the matrix of threshold parameters. It only discriminates between GPCM and 4-PL model, and returns a character vector of length equal to the number of items, that contains \code{"GPCM"} or \code{"4PL"} entries depending on the structure of the thres matrix.
#'
#'@param thres A numeric matrix which contains the threshold parameter for each item. NA is allowed - in fact expected!
#'
#' @seealso \link{PPall}
#'
#'@export
#'
#'@author Manuel Reif
#'
#'@example ./R/.examples.R
#'@keywords Person Parameters
findmodel <- function(thres) {
  maxsc <- normalize_thres(thres)$maxsc
  ifelse(maxsc > 1, "GPCM", "4PL")
}


# --- small checks ----------------------------------------------------------

# A numeric vector of length 1 (recycled to n) or n
recycle_arg <- function(x, n, name, what) {
  if (!is.numeric(x) && !(is.logical(x) && all(is.na(x)))) {
    stop("'", name, "' must be numeric.", call. = FALSE)
  }
  if (length(x) == 1) {
    x <- rep_len(as.numeric(x), n)
  } else if (length(x) != n) {
    stop("'", name, "' must have length 1 or ", n, " (the number of ", what,
         "), not ", length(x), ".", call. = FALSE)
  }
  x
}

check_scalar <- function(x, name, whole = FALSE) {
  ok <- is.numeric(x) && length(x) == 1 && is.finite(x) && x > 0
  if (ok && whole) ok <- x == round(x)
  if (!ok) {
    stop("'", name, "' must be a single positive ",
         if (whole) "whole number" else "number", ".", call. = FALSE)
  }
}


# --- item bank --------------------------------------------------------------

# The canonical item bank as an R list (one entry per field of the C++ struct
# ItemBank). model: 0 = 4PL, 1 = GPCM; ncat: number of categories; off:
# 0-based offset of the item's thresholds in thr; a, c, d: slope, lower and
# upper asymptote (GPCM: c = 0, d = 1); thr: 4PL b, GPCM delta_1 .. delta_{K-1};
# hub_loc: location of the Huber residual (4PL: b, GPCM: mean(delta)).
make_bank_r <- function(thres, maxsc, slopes, lowerA, upperA, is4pl) {
  thr_list <- lapply(seq_along(maxsc), function(j) thres[1 + seq_len(maxsc[j]), j])
  ncat <- as.integer(maxsc + 1L)
  list(n = length(maxsc),
       model = ifelse(is4pl, 0L, 1L),
       ncat = ncat,
       off = as.integer(c(0L, cumsum(ncat - 1L))[seq_along(ncat)]),
       a = as.numeric(slopes),
       c = ifelse(is4pl, as.numeric(lowerA), 0),
       d = ifelse(is4pl, as.numeric(upperA), 1),
       thr = unlist(thr_list, use.names = FALSE),
       hub_loc = vapply(thr_list, mean, 0))
}


# --- the input layer --------------------------------------------------------

pp_types <- c("mle", "wle", "map", "eap", "robust")

# Check and normalise the inputs of PP_4pl (family "4pl"), PP_gpcm ("gpcm")
# and PPall ("mixed"). Argument checks, recycling and length checks always
# run; ctrl$skipcheck only skips the checks of the response values, which need
# a pass over the whole data matrix.
pp_prepare <- function(respm, thres, slopes = NULL, lowerA = NULL, upperA = NULL,
                       model2est = NULL, mu = NULL, sigma2 = NULL, theta_start = NULL,
                       type = "wle", maxsteps = 100, exac = 0.001, H = 1, ctrl = list(),
                       range = c(-6, 6), verbose = FALSE,
                       family = c("4pl", "gpcm", "mixed")) {
  family <- match.arg(family)

  ## controls
  cont <- list(killdupli = FALSE, skipcheck = FALSE)
  user_ctrlI <- match(names(ctrl), names(cont))
  if (any(is.na(user_ctrlI))) {
    notex <- names(ctrl)[is.na(user_ctrlI)]
    warning("The following options in ctrl do not exist: ", paste(notex, collapse = ", "))
    ctrl <- ctrl[!is.na(user_ctrlI)]
    user_ctrlI <- user_ctrlI[!is.na(user_ctrlI)]
  }
  cont[user_ctrlI] <- ctrl

  ## type (always checked, also with skipcheck)
  if (!is.character(type) || length(type) != 1 || !type %in% pp_types) {
    stop("'type' must be one of ", paste0("\"", pp_types, "\"", collapse = ", "), ".",
         call. = FALSE)
  }
  check_scalar(maxsteps, "maxsteps", whole = TRUE)
  check_scalar(exac, "exac")
  check_scalar(H, "H")
  if (!is.numeric(range) || length(range) != 2 || any(!is.finite(range)) || range[1] >= range[2]) {
    stop("'range' must be c(lower, upper) with finite lower < upper.", call. = FALSE)
  }
  if (!isTRUE(verbose) && !isFALSE(verbose)) stop("'verbose' must be TRUE or FALSE.", call. = FALSE)

  ## responses
  if (is.data.frame(respm)) respm <- as.matrix(respm)
  if (!is.matrix(respm)) stop("'respm' must be a matrix or a data.frame.", call. = FALSE)
  if (!is.numeric(respm) && !is.logical(respm)) {
    stop("'respm' must contain numeric responses.", call. = FALSE)
  }
  N <- nrow(respm)
  nitem <- ncol(respm)
  if (N == 0 || nitem == 0) stop("'respm' has no persons or no items.", call. = FALSE)

  ## thresholds
  nt <- normalize_thres(thres)
  thres <- nt$thres
  maxsc <- nt$maxsc
  if (ncol(thres) != nitem) {
    stop("'thres' has ", ncol(thres), " items, 'respm' has ", nitem, ".", call. = FALSE)
  }

  if (!cont$skipcheck) {
    obs <- respm[!is.na(respm)]
    if (any(obs != round(obs))) stop("'respm' must contain whole numbers (or NA).", call. = FALSE)
    if (any(obs < 0)) stop("'respm' must not contain negative responses.", call. = FALSE)
    toohigh <- colSums(respm > rep(maxsc, each = N), na.rm = TRUE) > 0
    if (any(toohigh)) {
      stop("Responses in categories which do not exist according to 'thres' (item(s) ",
           paste(which(toohigh), collapse = ", "), ").", call. = FALSE)
    }
  }
  storage.mode(respm) <- "integer"

  items <- prepare_items(nt, slopes, lowerA, upperA, model2est, family)

  ## person vectors. Only map and eap have a prior (default N(0, 1)); for
  ## mle, wle and robust it is NA. A prior given there is checked like any
  ## person vector and then ignored with a warning; NA (as in $model of such an
  ## object) is accepted silently.
  if (type %in% c("map", "eap")) {
    if (is.null(mu)) mu <- 0
    if (is.null(sigma2)) sigma2 <- 1
    mu <- recycle_arg(mu, N, "mu", "persons")
    sigma2 <- recycle_arg(sigma2, N, "sigma2", "persons")
    if (anyNA(mu) || any(!is.finite(mu))) stop("'mu' must be finite.", call. = FALSE)
    if (anyNA(sigma2) || any(!is.finite(sigma2)) || any(sigma2 <= 0)) {
      stop("'sigma2' must be positive and finite.", call. = FALSE)
    }
  } else {
    given <- c(mu = !is.null(mu) && !all(is.na(recycle_arg(mu, N, "mu", "persons"))),
               sigma2 = !is.null(sigma2) && !all(is.na(recycle_arg(sigma2, N, "sigma2", "persons"))))
    if (any(given)) {
      warning(paste0("'", names(given)[given], "'", collapse = " and "),
              " (the prior of type = \"map\" and \"eap\") ignored for type = \"", type, "\".",
              call. = FALSE)
    }
    mu <- sigma2 <- rep(NA_real_, N)
  }

  ## starting values; NA lets the estimator start at the logit of the
  ## relative score
  if (is.null(theta_start)) {
    theta_start <- rep(NA_real_, N)
  } else {
    theta_start <- recycle_arg(theta_start, N, "theta_start", "persons")
    if (anyNA(theta_start) || any(!is.finite(theta_start))) {
      stop("'theta_start' must be finite.", call. = FALSE)
    }
  }

  list(X = respm, thres = thres, maxsc = maxsc,
       slopes = items$slopes, lowerA = items$lowerA, upperA = items$upperA,
       model2est = items$model2est,
       mu = mu, sigma2 = sigma2, start = theta_start,
       type = type, maxsteps = maxsteps, exac = exac, H = H, range = as.numeric(range),
       verbose = verbose, ctrl = cont, family = family, bank = items$bank)
}


# Check and normalise the item parameters. nt is the result of
# normalize_thres(); family as in pp_prepare(). Shared by pp_prepare() and the
# simulators; slope_name is the name of the slope argument in error messages.
# Returns the recycled slopes/lowerA/upperA (lowerA/upperA stay as passed for
# "gpcm"), model2est (only for "mixed"), is4pl and the item bank.
prepare_items <- function(nt, slopes, lowerA, upperA, model2est, family,
                          slope_name = "slopes") {
  thres <- nt$thres
  maxsc <- nt$maxsc
  nitem <- ncol(thres)

  ## model per item
  if (family == "mixed") {
    if (is.null(model2est)) {
      model2est <- findmodel(thres)
    }
    model2est <- unname(as.character(model2est))
    if (length(model2est) != nitem) {
      stop("'model2est' must have one entry per item (", nitem, "), not ",
           length(model2est), ".", call. = FALSE)
    }
    bad <- !model2est %in% c("4PL", "GPCM")
    if (any(bad)) {
      stop("'model2est' must contain only \"4PL\" or \"GPCM\" (invalid: ",
           paste0("\"", unique(model2est[bad]), "\"", collapse = ", "), ").", call. = FALSE)
    }
    is4pl <- model2est == "4PL"
  } else {
    is4pl <- rep(family == "4pl", nitem)
  }
  if (any(is4pl & maxsc != 1)) {
    stop("Item(s) ", paste(which(is4pl & maxsc != 1), collapse = ", "),
         if (family == "4pl") " have more than one threshold in 'thres'; use PP_gpcm() or PPall() for polytomous items."
         else " are 4PL items in 'model2est' but have more than two categories in 'thres'.",
         call. = FALSE)
  }

  ## item parameters
  if (is.null(slopes)) slopes <- 1
  slopes <- recycle_arg(slopes, nitem, slope_name, "items")
  if (anyNA(slopes) || any(!is.finite(slopes)) || any(slopes <= 0)) {
    stop("'", slope_name, "' must be positive and finite.", call. = FALSE)
  }
  if (family != "gpcm") {
    if (is.null(lowerA)) lowerA <- 0
    if (is.null(upperA)) upperA <- 1
    lowerA <- recycle_arg(lowerA, nitem, "lowerA", "items")
    upperA <- recycle_arg(upperA, nitem, "upperA", "items")
    if (anyNA(lowerA) || anyNA(upperA)) stop("'lowerA' and 'upperA' must not be NA.", call. = FALSE)
    bad <- is4pl & !(lowerA >= 0 & lowerA < upperA & upperA <= 1)
    if (any(bad)) {
      stop("0 <= lowerA < upperA <= 1 is violated for item(s) ",
           paste(which(bad), collapse = ", "), ".", call. = FALSE)
    }
    if (family == "mixed") {
      if (any(lowerA[!is4pl] != 0)) warning("At least for one item with #categories > 1, there is an lower asymptote != 0 submitted. This will be ignored.\n")
      if (any(upperA[!is4pl] != 1)) warning("At least for one item with #categories > 1, there is an upper asymptote != 1 submitted. This will be ignored.\n")
    }
  }

  list(slopes = slopes, lowerA = lowerA, upperA = upperA,
       model2est = if (family == "mixed") model2est, is4pl = is4pl,
       bank = make_bank_r(thres, maxsc, slopes,
                          if (family == "gpcm") 0 else lowerA,
                          if (family == "gpcm") 1 else upperA, is4pl))
}
