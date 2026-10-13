#' Person assessment: estimates and person fit in one step
#'
#' \code{PPass} (\bold{P}erson \bold{P}arameter \bold{ass}essment) estimates the person parameters and computes the person fit in one call, and returns both in one table, next to the other person variables. It takes a data.frame or matrix with item responses and known item parameters, or a model fitted with the \code{eRm} package.
#'
#' \bold{Data with item parameters} (\code{PPass.default}): the columns \code{items} of \code{respdf} are the responses, the other columns are kept as person variables. The item parameters are passed in \code{...} (\code{thres}, \code{slopes}, \code{lowerA}, \code{upperA}, \code{model2est}), and the model follows from them: \code{thres} alone gives the 1PL model, with \code{slopes} the 2PL, with \code{lowerA} the 3PL and with \code{upperA} the 4PL model, estimated with \code{PP_4pl()}. A threshold matrix with more than one threshold for some item gives the partial credit model (all slopes 1) or the generalized partial credit model, estimated with \code{PP_gpcm()}. With \code{model2est}, or with such polytomous items and \code{lowerA} or \code{upperA}, the test is a mixed test, estimated with \code{PPall()}. The argument \code{mod} of earlier versions is no longer needed; it is ignored with a warning.
#'
#' \bold{eRm models} (\code{PPass.eRm}): all models of the \code{eRm} package are supported, the Rasch model (\code{RM}), the linear logistic test model (\code{LLTM}), the rating scale model (\code{RSM}), the partial credit model (\code{PCM}) and the linear models \code{LRSM} and \code{LPCM}, also with several time points (\code{mpoints}) and groups (\code{groupvec}). The responses and item parameters come from the fitted object. eRm stores easiness parameters per category, \eqn{\beta_{ih} = -\sum_{k \le h} \delta_{ik}}; they are turned into the thresholds \eqn{\delta_{ih} = \beta_{i,h-1} - \beta_{ih}}, so for the dichotomous models the difficulty is \eqn{-\beta_i}. With time points or groups every combination of item, time point and group is a virtual item with its own parameters; a person answers the virtual items of their group, and gets one ability over all time points. The person parameters are estimated with \code{PP_4pl()} (RM, LLTM) or \code{PP_gpcm()} with all slopes 1. The maximum likelihood estimates (\code{type = "mle"}) equal those of \code{eRm::person.parameter()}; unlike \code{eRm}, \code{PPass} also gives estimates for models with group contrasts, and WLE, MAP, EAP and robust estimates, person fit, jackknife and plausible values. The \code{eRm} package is not needed to run \code{PPass} on such an object.
#'
#' Person fit is computed by \code{Pfit()} and needs an estimate of type \code{"mle"}, \code{"wle"} or \code{"map"}. For \code{type = "eap"} and \code{"robust"} \code{PPass} returns the estimates only, with a message.
#'
#' @param \ldots Arguments for the estimation: the item parameters (\code{PPass.default} only) and \code{type}, \code{mu}, \code{sigma2}, \code{theta_start}, \code{range}, \code{maxsteps}, \code{exac}, \code{H} and \code{verbose}, see \code{PP_4pl()}, \code{PP_gpcm()} and \code{PPall()}. The default estimate is WLE.
#'
#' @return A data.frame of class \code{ppass} with one row per person: the person variables (the columns of \code{respdf} that are not items; for eRm models with groups the column \code{group}), \code{estimate}, \code{SE} and \code{status} of the estimate (see \code{PP_4pl()}) and the columns of the chosen person fit indices (see \code{Pfit()}). It can be used like any data.frame. The estimation object is kept as attribute, so \code{JKpp()} and \code{PV()} can be applied to the result directly; \code{print()} and \code{summary()} show an overview with the persons flagged by the person fit, and \code{as.data.frame()} returns a plain data.frame.
#'
#' @author Manuel Reif, Jan Steinfeld
#' @seealso \link{PP_4pl}, \link{PP_gpcm}, \link{PPall}, \link{Pfit}, \link{JKpp}, \link{PV}
#' @rdname PPass
#'@examples
#' ## data with item parameters
#'
#' set.seed(1337)
#' # difficulties and slopes of 15 items
#' diffpar <- seq(-3, 3, length = 15)
#' sl <- round(runif(15, 0.5, 1.5), 2)
#' # responses of 200 persons, plus an id and a group variable
#' awm <- sim_4pl(beta = diffpar, alpha = sl, lowerA = rep(0, 15), upperA = rep(1, 15),
#'                theta = rnorm(200))
#' dat <- data.frame(id = 1:200, group = rep(c("a", "b"), 100), awm)
#'
#' # 2PL, WLE estimates and all person fit indices; id and group are kept
#' res <- PPass(dat, items = 3:17, thres = diffpar, slopes = sl)
#' res
#' summary(res)
#'
#' # a plain data.frame for further use
#' head(as.data.frame(res))
#'
#' # jackknife directly from the result
#' jk <- JKpp(res)
#'
#'
#' ## real data (adaptive test, 1PL)
#'
#' data(pp_amt)
#' d <- pp_amt$daten_amt
#' rd_res <- PPass(d, items = 8:ncol(d), thres = pp_amt$betas[, 2], fitindices = "lz")
#' head(rd_res)
#'
#'
#' ## models fitted with eRm
#'
#' if (requireNamespace("eRm", quietly = TRUE)) {
#'
#'   # Rasch model
#'   my_rm <- eRm::RM(eRm::raschdat1)
#'   res_rm <- PPass(my_rm)
#'   summary(res_rm)
#'
#'   # the maximum likelihood estimates equal eRm::person.parameter()
#'   res_mle <- PPass(my_rm, type = "mle", fitindices = "lzstar")
#'
#'   # partial credit model
#'   set.seed(2751)
#'   thres <- rbind(0, matrix(c(-2, -1, 0.5, -1.2, 0.3, NA, -0.5, 1, NA, 0, 1.5, 2), 3))
#'   simdat <- sim_gpcm(thres = thres, alpha = rep(1, 4), theta = rnorm(300))
#'   res_pcm <- PPass(eRm::PCM(simdat))
#'
#'   # linear partial credit model with two time points and two groups
#'   simdat2 <- sim_gpcm(thres = thres, alpha = rep(1, 4), theta = rnorm(300, 0.5))
#'   my_lpcm <- eRm::LPCM(cbind(simdat, simdat2), mpoints = 2, groupvec = rep(1:2, 150))
#'   res_lpcm <- PPass(my_lpcm)
#'   head(res_lpcm)
#'
#'   # plausible values need an EAP or MAP estimate
#'   pv <- PV(PPass(my_rm, type = "map"))
#' }
#' @export
PPass <- function(...) UseMethod("PPass")


#' @param respdf A data.frame or matrix with one row per person, which contains the item responses and possibly other person variables.
#' @param items The item columns of \code{respdf}: \code{"all"} (default), their positions or their names.
#' @param fitindices The person fit indices to compute, any of \code{"lz"}, \code{"lzstar"}, \code{"infit"} and \code{"outfit"} (default all).
#' @param SE If \code{TRUE}, jackknife standard errors of the fit indices are added (see \code{Pfit()}).
#'
#' @rdname PPass
#' @method PPass default
#' @export
PPass.default <- function(respdf, items = "all", fitindices = c("lz", "lzstar", "infit", "outfit"),
                          SE = FALSE, ...) {
  call <- match.call()
  fitindices <- match.arg(fitindices, several.ok = TRUE)
  if (!is.data.frame(respdf) && !is.matrix(respdf)) {
    stop("'respdf' must be a data.frame or a matrix.", call. = FALSE)
  }
  cols <- item_columns(respdf, items)
  X <- as.matrix(respdf[, cols, drop = FALSE])
  rest <- as.data.frame(respdf[, -cols, drop = FALSE])
  args <- list(...)
  if ("mod" %in% names(args)) {
    warning("'mod' is no longer needed and is ignored; the model follows from the item parameters.",
            call. = FALSE)
    args$mod <- NULL
  }
  est <- do.call(ppass_fun(args), c(list(respm = X), args))
  ppass_build(est, rest, fitindices, SE, model = ppass_label(est$model), call = call)
}


#' @param RMobj A model fitted with the \code{eRm} package (\code{RM()}, \code{LLTM()}, \code{RSM()}, \code{PCM()}, \code{LRSM()} or \code{LPCM()}).
#'
#' @rdname PPass
#' @method PPass eRm
#' @export
PPass.eRm <- function(RMobj, fitindices = c("lz", "lzstar", "infit", "outfit"), SE = FALSE, ...) {
  call <- match.call()
  fitindices <- match.arg(fitindices, several.ok = TRUE)
  args <- list(...)
  given <- intersect(names(args), c("respm", "thres", "slopes", "lowerA", "upperA", "model2est", "mod"))
  if (length(given)) {
    stop("The responses and item parameters come from the eRm object, leave out ",
         paste0("'", given, "'", collapse = ", "), ".", call. = FALSE)
  }
  it <- erm_items(RMobj)
  est <- if (it$dichotomous) {
    do.call(PP_4pl, c(list(respm = it$X, thres = it$thres[2, ]), args))
  } else {
    do.call(PP_gpcm, c(list(respm = it$X, thres = it$thres, slopes = rep(1, ncol(it$X))), args))
  }
  rest <- if (is.null(it$group)) NULL else data.frame(group = it$group)
  ppass_build(est, rest, fitindices, SE, model = it$label, call = call, items = it$items)
}


# The estimation function for the item parameters in args: polytomous items go
# to PP_gpcm, with lowerA/upperA or model2est to PPall, dichotomous ones to PP_4pl.
ppass_fun <- function(args) {
  poly <- !is.null(args$thres) && any(normalize_thres(args$thres)$maxsc > 1)
  if (!is.null(args$model2est) || (poly && (!is.null(args$lowerA) || !is.null(args$upperA)))) {
    PPall
  } else if (poly) {
    PP_gpcm
  } else {
    PP_4pl
  }
}

# Model name for print/summary, from the prepared item parameters
ppass_label <- function(prep) {
  differs <- function(x, v) any(x != v, na.rm = TRUE)
  switch(prep$family,
    "4pl" = if (differs(prep$upperA, 1)) "4PL model"
            else if (differs(prep$lowerA, 0)) "3PL model"
            else if (differs(prep$slopes, 1)) "2PL model"
            else "1PL model",
    "gpcm" = if (differs(prep$slopes, 1)) "generalized partial credit model"
             else "partial credit model",
    "mixed 4PL/GPCM model")
}

# Column indices of the items in respdf
item_columns <- function(respdf, items) {
  nc <- ncol(respdf)
  if (identical(items, "all")) return(seq_len(nc))
  if (is.character(items)) {
    miss <- setdiff(items, colnames(respdf))
    if (length(miss)) stop("'items' names columns that 'respdf' does not have: ",
                           paste(miss, collapse = ", "), ".", call. = FALSE)
    return(match(items, colnames(respdf)))
  }
  if (!is.numeric(items) || !length(items) || anyNA(items) || any(items != round(items)) ||
      any(items < 1 | items > nc) || anyDuplicated(items)) {
    stop("'items' must be \"all\", column names or distinct column positions of 'respdf'.",
         call. = FALSE)
  }
  as.integer(items)
}

# Responses and thresholds of an eRm model. With time points or groups the
# parameters in betapar are ordered by time point, group, item and category,
# one block per virtual item; a person answers the virtual items of their group.
erm_items <- function(obj) {
  models <- c(RM = "Rasch model", LLTM = "linear logistic test model", RSM = "rating scale model",
              PCM = "partial credit model", LRSM = "linear rating scale model",
              LPCM = "linear partial credit model")
  if (is.null(obj$model) || !obj$model %in% names(models)) {
    stop("PPass knows the eRm models ", paste(names(models), collapse = ", "), ".", call. = FALSE)
  }
  X <- as.matrix(obj$X)
  mp <- if (is.null(obj$mpoints)) 1L else as.integer(obj$mpoints)
  ng <- if (is.null(obj$ngroups)) 1L else as.integer(obj$ngroups)
  gv <- if (length(obj$groupvec) == nrow(X)) as.integer(obj$groupvec) else rep(1L, nrow(X))
  k <- ncol(X) %/% mp
  dich <- obj$model %in% c("RM", "LLTM")

  ti <- rep(seq_len(mp), each = ng * k)
  gi <- rep(rep(seq_len(ng), each = k), mp)
  col <- (ti - 1L) * k + rep(seq_len(k), mp * ng)
  ncat <- if (dich) rep(1L, length(col)) else apply(X, 2, max, na.rm = TRUE)[col]
  beta <- obj$betapar
  if (sum(ncat) != length(beta)) {
    stop("The parameters of this eRm object do not match its responses.", call. = FALSE)
  }

  thres <- matrix(NA_real_, max(ncat) + 1L, length(col))
  thres[1, ] <- 0
  last <- cumsum(ncat)
  Xv <- matrix(NA_integer_, nrow(X), length(col))
  for (v in seq_along(col)) {
    b <- beta[last[v] - ncat[v] + seq_len(ncat[v])]
    thres[1 + seq_len(ncat[v]), v] <- -diff(c(0, b))
    Xv[gv == gi[v], v] <- X[gv == gi[v], col[v]]
  }
  cn <- if (is.null(colnames(X))) paste0("I", seq_len(ncol(X))) else colnames(X)
  colnames(Xv) <- paste0(cn[col], if (ng > 1) paste0(" g", gi))

  label <- sprintf("%s (eRm::%s)", models[[obj$model]], obj$model)
  if (mp > 1) label <- paste0(label, ", ", mp, " time points")
  if (ng > 1) label <- paste0(label, ", ", ng, " groups")
  items <- if (mp > 1 || ng > 1) paste(length(col), "virtual items") else paste(length(col), "items")
  list(X = Xv, thres = thres, dichotomous = dich, label = label, items = items,
       group = if (ng > 1) gv)
}

# The ppass result: person variables, estimates and person fit
ppass_build <- function(est, rest, fitindices, SE, model, call,
                        items = paste(ncol(est$model$X), "items")) {
  call[[1]] <- as.name("PPass")
  attributes(call) <- attributes(est$call)
  est$call <- call
  e <- est$estimates
  out <- data.frame(estimate = e$estimate, SE = e$SE, status = e$status)
  if (est$type %in% c("mle", "wle", "map")) {
    fit <- Pfit(pp = est, fitindices = fitindices, SE = SE)
    out <- cbind(out, do.call(cbind, unclass(fit)))
  } else {
    message("Person fit needs an estimate of type 'mle', 'wle' or 'map'; ",
            "PPass returns the ", est$type, " estimates only.")
    fitindices <- character(0)
  }
  if (!is.null(rest) && ncol(rest)) out <- cbind(rest, out)
  rownames(out) <- NULL
  structure(out, class = c("ppass", "data.frame"), ppeo = est,
            info = list(model = model, type = est$type, items = items,
                        fitindices = fitindices))
}

# TRUE if x still has all rows of the estimation in their order
ppass_intact <- function(x) {
  est <- attr(x, "ppeo")
  !is.null(est) && !is.null(attr(x, "info")) && "estimate" %in% names(x) &&
    identical(x$estimate, est$estimates$estimate)
}

# The column used to flag persons and its critical value
ppass_flag <- function(x, alpha) {
  sp <- intersect(c("lzstar", "lz", "in_t", "ou_t"), names(x))[1]
  z <- stats::qnorm(1 - alpha / 2)
  if (is.na(sp)) return(NULL)
  list(column = sp, z = z, flagged = which(is.finite(x[[sp]]) & abs(x[[sp]]) > z))
}

ppass_header <- function(x, alpha) {
  info <- attr(x, "info")
  cat("Person assessment:", nrow(x), "persons,", info$items, "\n")
  cat("Model:", info$model, "|", toupper(info$type), "estimates\n")
  st <- table(x$status)
  st <- st[st > 0]
  cat("Status:", paste(st, names(st), collapse = ", "), "\n")
  if (length(info$fitindices)) cat("Person fit:", paste(info$fitindices, collapse = ", "), "\n")
  fl <- ppass_flag(x, alpha)
  if (!is.null(fl)) {
    cat(sprintf("Flagged (|%s| > %.2f): %d persons\n", fl$column, fl$z, length(fl$flagged)))
  }
}

#' @param x,object A result of \code{PPass()}.
#' @param n Number of persons to print.
#' @param digits Number of significant digits to print.
#' @rdname PPass
#' @export
print.ppass <- function(x, n = 10, digits = 3, ...) {
  if (!ppass_intact(x)) {
    print(as.data.frame(x), digits = digits, ...)
    return(invisible(x))
  }
  ppass_header(x, 0.05)
  cat("\n")
  # the person variables (at most 2), the estimate and the main fit statistics
  first <- match("estimate", names(x))
  main <- c("estimate", "SE", "status", "lz", "lzstar", "infit", "in_t", "outfit", "ou_t",
            "lz_se", "lzs_se", "in_se", "ou_se")
  show <- c(names(x)[seq_len(min(2, first - 1))], intersect(main, names(x)))
  print(utils::head(as.data.frame(x)[, show, drop = FALSE], n), digits = digits, ...)
  if (nrow(x) > n) cat("...", nrow(x) - n, "more persons\n")
  hidden <- setdiff(names(x), show)
  if (length(hidden)) cat("Not shown:", paste(hidden, collapse = ", "), "\n")
  invisible(x)
}

#' @param alpha Significance level for flagging persons: \eqn{|z| > z_{1-\alpha/2}} for lz*, lz, \code{in_t} or \code{ou_t} (the first one computed).
#' @rdname PPass
#' @export
summary.ppass <- function(object, alpha = 0.05, digits = 3, ...) {
  check_alpha(alpha)
  if (!ppass_intact(object)) {
    stop("'object' is not a complete result of PPass (rows were removed or reordered).",
         call. = FALSE)
  }
  ppass_header(object, alpha)
  ok <- object$status == "ok"
  cat("\nEstimates (status ok):\n")
  est <- summary(object$estimate[ok])
  print(est, digits = digits)
  res <- list(estimates = est, status = table(object$status))

  vals <- lapply(stats::setNames(nm = intersect(c("lz", "lzstar", "infit", "in_t", "outfit", "ou_t"),
                                                names(object))), function(sp) object[[sp]])
  if (length(vals)) {
    res$fit <- fit_table(vals, stats::qnorm(1 - alpha / 2))
    cat("\nPerson fit:\n")
    print(res$fit, digits = digits, row.names = FALSE, ...)
    fl <- ppass_flag(object, alpha)
    pv <- utils::head(seq_len(match("estimate", names(object)) - 1L), 2)
    res$flagged <- data.frame(person = fl$flagged,
                              as.data.frame(object)[fl$flagged, pv, drop = FALSE],
                              estimate = object$estimate[fl$flagged],
                              value = object[[fl$column]][fl$flagged])
    names(res$flagged)[ncol(res$flagged)] <- fl$column
    res$flagged <- res$flagged[order(res$flagged[[fl$column]]), , drop = FALSE]
    rownames(res$flagged) <- NULL
    if (nrow(res$flagged)) {
      cat("\nFlagged persons (", fl$column, "):\n", sep = "")
      print(utils::head(res$flagged, 20), digits = digits, row.names = FALSE, ...)
      if (nrow(res$flagged) > 20) cat("...", nrow(res$flagged) - 20, "more\n")
    }
  }
  invisible(res)
}

#' @param row.names,optional Ignored.
#' @rdname PPass
#' @export
as.data.frame.ppass <- function(x, row.names = NULL, optional = FALSE, ...) {
  attr(x, "ppeo") <- NULL
  attr(x, "info") <- NULL
  class(x) <- "data.frame"
  x
}

# The estimation object of a complete PPass result
ppass_estimate <- function(x) {
  if (!ppass_intact(x)) {
    stop("'estobj' is not a complete result of PPass (rows were removed or reordered).",
         call. = FALSE)
  }
  attr(x, "ppeo")
}

#' @rdname Jkpp
#' @method JKpp ppass
#' @export
JKpp.ppass <- function(estobj, ...) JKpp(ppass_estimate(estobj), ...)

#' @rdname PV
#' @method PV ppass
#' @export
PV.ppass <- function(estobj, ...) PV(ppass_estimate(estobj), ...)
