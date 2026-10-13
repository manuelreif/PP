#' Plots of person estimates and person fit
#'
#' Three plots for the results of \code{PPass()}, \code{PP_4pl()}, \code{PP_gpcm()} and \code{PPall()}: a person fit map, the measurement precision of the test, and the response profiles of single persons.
#'
#' \bold{Person fit map} (\code{which = "fit"}): the person fit statistic \code{index} against the estimate, one point per person with status \code{"ok"}. The dashed lines are the critical values \eqn{\pm z_{1-\alpha/2}}; persons beyond them are flagged (the same rule as in \code{summary()} of a \code{PPass} result), and the at most 10 most extreme ones are labelled.
#'
#' \bold{Measurement precision} (\code{which = "precision"}): the upper panel shows the distribution of the estimates, the lower panel the standard error of each person against the estimate. Because the item parameters are known, the standard error of the whole test follows from the test information \eqn{I(\theta)}, the sum of the item informations: \eqn{1/\sqrt{I(\theta)}}, and \eqn{1/\sqrt{I(\theta) + 1/\sigma^2}} for MAP and EAP estimates with the prior variance \eqn{\sigma^2}. This curve is drawn when every person answered every item (\code{curve = NULL}); in incomplete or adaptive designs each person has an own set of items, and the curve of all items holds for nobody. The ticks at the bottom mark the location of each item, the ability at which it gives the most information (for the 1PL and 2PL its difficulty). The plot shows where the test measures accurately and whether the persons are located there. For robust estimates the curve is the model standard error without the Huber weights; for EAP estimates the standard error is the posterior SD.
#'
#' \bold{Person profile} (\code{which = "profile"}): one panel per person in \code{person}. Each answered item is a point: the expected score at the estimate (x axis, relative to the maximum score of the item, easy items on the left) against the observed score (y axis, relative to the maximum score). Responses in line with the model lie close to the diagonal. Items with a standardised residual \eqn{|z| = |x - E|/\sqrt{V}} above \eqn{z_{1-\alpha/2}} are marked as unexpected and labelled with the item name: a wrong answer to an easy item at the bottom left, a right answer to a hard item at the top right. The profile shows which responses make a person conspicuous. The panel title gives the estimate, its standard error and the fit statistic \code{index}.
#'
#' The plots are drawn with the \code{tinyplot} package, so a theme set with \code{tinyplot::tinytheme()} applies to them.
#'
#' @param x A result of \code{PPass()} (class \code{ppass}) or of \code{PP_4pl()}, \code{PP_gpcm()} or \code{PPall()} (class \code{ppeo}).
#' @param which The plot: \code{"fit"} (person fit map), \code{"precision"} (measurement precision) or \code{"profile"} (person profiles). The default is \code{"fit"} for \code{PPass} results with person fit and \code{"precision"} otherwise.
#' @param person For \code{which = "profile"}: the persons to show, as row positions. The default are the (at most 4) flagged persons with the strongest misfit: the lowest values of lz and lz*, the highest of the infit and outfit t.
#' @param index The person fit statistic used to flag persons: one of \code{"lzstar"}, \code{"lz"}, \code{"in_t"} and \code{"ou_t"}. For \code{PPass} results the default is the first of these that was computed, for estimation results \code{"lzstar"}; for estimation results it is computed with \code{Pfit()}, which needs an estimate of type \code{"mle"}, \code{"wle"} or \code{"map"}.
#' @param alpha Significance level: persons with \eqn{|index| > z_{1-\alpha/2}} are flagged, and responses with \eqn{|z| > z_{1-\alpha/2}} are unexpected.
#' @param label For \code{PPass} results: the name of a column of \code{x} (e.g. an id) whose values label the persons. By default persons are labelled with their row position.
#' @param curve For \code{which = "precision"}: draw the standard error curve of all items? \code{NULL} (default) draws it when every person answered every item (and, for MAP and EAP, all persons have the same prior variance).
#' @param \ldots Further arguments for \code{tinyplot::tinyplot()}, e.g. \code{main}, \code{xlab}, \code{ylab} or \code{palette}.
#'
#' @return The plotted data, invisibly: for \code{"fit"} a data.frame with one row per plotted person (\code{person}, \code{label}, \code{estimate}, \code{value} of \code{index}, \code{flagged}); for \code{"precision"} a list with the data.frames \code{persons} (\code{person}, \code{estimate}, \code{SE}), \code{curve} (\code{theta}, \code{info}, \code{SE}; \code{NULL} without curve) and \code{items} (\code{item}, \code{location}); for \code{"profile"} a data.frame with one row per answered item and person (\code{person}, \code{item}, \code{response}, \code{max}, \code{expected}, \code{z}, \code{unexpected}).
#'
#' @author Manuel Reif
#' @seealso \link{PPass}, \link{Pfit}, \link{PP_4pl}, \link{PP_gpcm}, \link{PPall}
#' @name pp-plots
#' @examples
#' set.seed(1337)
#' diffpar <- seq(-3, 3, length = 20)
#' sl <- round(runif(20, 0.5, 1.5), 2)
#' awm <- sim_4pl(beta = diffpar, alpha = sl, lowerA = rep(0, 20), upperA = rep(1, 20),
#'                theta = rnorm(300))
#' # 10 persons answer at random
#' awm[1:10, ] <- matrix(rbinom(200, 1, 0.5), 10)
#' dat <- data.frame(id = sprintf("P%03d", 1:300), awm)
#'
#' res <- PPass(dat, items = 2:21, thres = diffpar, slopes = sl)
#'
#' # who does not fit?
#' plot(res, label = "id")
#'
#' # where does the test measure accurately, and are the persons there?
#' plot(res, which = "precision")
#'
#' # which responses make the flagged persons conspicuous?
#' plot(res, which = "profile", label = "id")
#'
#' # the same plots for an estimation result
#' est <- PP_4pl(respm = awm, thres = diffpar, slopes = sl, type = "map")
#' plot(est)
#' plot(est, which = "profile", person = 1:2)
NULL


# Colours: the persons/items, flagged persons and unexpected responses,
# the persons not flagged, and lines and text
pp_col <- c(base = "#2a78d6", flag = "#eb6834", context = "#a3a29d", ink = "#3d3c39")

fit_names <- c(lzstar = "lz*", lz = "lz", in_t = "infit t", ou_t = "outfit t")

#' @rdname pp-plots
#' @export
plot.ppass <- function(x, which = c("fit", "precision", "profile"), person = NULL, index = NULL,
                       alpha = 0.05, label = NULL, curve = NULL, ...) {
  if (!ppass_intact(x)) {
    stop("'x' is not a complete result of PPass (rows were removed or reordered).", call. = FALSE)
  }
  avail <- intersect(names(fit_names), names(x))
  which <- if (missing(which) && !length(avail)) "precision" else match.arg(which)
  if (is.null(label)) {
    labels <- as.character(seq_len(nrow(x)))
  } else {
    if (!is.character(label) || length(label) != 1 || !label %in% names(x)) {
      stop("'label' must be the name of a column of 'x'.", call. = FALSE)
    }
    labels <- as.character(x[[label]])
  }
  fit <- function(index) {
    if (!length(avail)) {
      stop("This PPass result has no person fit (type '", attr(x, "info")$type,
           "'); person fit needs an estimate of type 'mle', 'wle' or 'map'.", call. = FALSE)
    }
    if (is.null(index)) index <- avail[1]
    check_index(index, avail)
    list(index = index, value = x[[index]])
  }
  pp_plot(attr(x, "ppeo"), which, person, index, alpha, labels, curve, fit, length(avail) > 0, ...)
}

#' @rdname pp-plots
#' @export
plot.ppeo <- function(x, which = c("precision", "fit", "profile"), person = NULL, index = NULL,
                      alpha = 0.05, curve = NULL, ...) {
  which <- match.arg(which)
  fit <- function(index) {
    if (is.null(index)) index <- "lzstar"
    check_index(index, names(fit_names))
    if (!x$type %in% c("mle", "wle", "map")) {
      stop("Person fit needs an estimate of type 'mle', 'wle' or 'map', not '", x$type, "'.",
           call. = FALSE)
    }
    fi <- c(lzstar = "lzstar", lz = "lz", in_t = "infit", ou_t = "outfit")[[index]]
    list(index = index, value = Pfit(pp = x, fitindices = fi)[[fi]][, index])
  }
  pp_plot(x, which, person, index, alpha, as.character(seq_len(nrow(x$estimates))), curve, fit,
          x$type %in% c("mle", "wle", "map"), ...)
}

check_index <- function(index, avail) {
  if (!is.character(index) || length(index) != 1 || !index %in% avail) {
    stop("'index' must be one of ", paste0("'", avail, "'", collapse = ", "), ".", call. = FALSE)
  }
}

# fit(index) returns the fit statistic index of every person (list(index, value));
# has_fit: fit() works without an error (used for the titles of the profiles)
pp_plot <- function(est, which, person, index, alpha, labels, curve, fit, has_fit, ...) {
  check_alpha(alpha)
  if (which != "precision" && !is.null(curve)) {
    stop("'curve' is only used with which = \"precision\".", call. = FALSE)
  }
  if (which != "profile" && !is.null(person)) {
    stop("'person' is only used with which = \"profile\".", call. = FALSE)
  }
  z <- stats::qnorm(1 - alpha / 2)
  switch(which,
    fit = plot_fit(est, fit(index), z, labels, ...),
    precision = plot_precision(est, curve, ...),
    profile = {
      f <- if (has_fit || is.null(person) || !is.null(index)) fit(index) else NULL
      plot_profile(est, person, f, z, labels, ...)
    })
}

# tinyplot() with evaluated arguments: tinyplot evaluates some arguments (e.g.
# palette) by name, which fails for local variables of the calling function
tp <- function(...) do.call(tinyplot::tinyplot, list(...))

# tinyplot() with the defaults of PP, overridden by the arguments in dots
pp_tinyplot <- function(defaults, dots) do.call(tinyplot::tinyplot, utils::modifyList(defaults, dots))

estimate_label <- function(est) paste0("Estimate (", toupper(est$type), ")")


# --- person fit map ----------------------------------------------------------

plot_fit <- function(est, f, z, labels, ...) {
  e <- est$estimates
  show <- e$status == "ok" & is.finite(f$value)
  if (!any(show)) {
    stop("No person has an estimate with status 'ok' and a value of ", f$index, ".", call. = FALSE)
  }
  d <- data.frame(person = which(show), label = labels[show], estimate = e$estimate[show],
                  value = f$value[show])
  d$flagged <- abs(d$value) > z
  grp <- factor(ifelse(d$flagged, "flagged", "not flagged"), levels = c("not flagged", "flagged"))
  top <- utils::head(order(-abs(d$value) * d$flagged), min(10, sum(d$flagged)))
  hidden <- sum(!show)
  # room for the labels above and below the points
  yr <- range(c(d$value, -z, z))
  pp_tinyplot(list(x = d$estimate, y = d$value, by = grp, palette = unname(pp_col[c("context", "flag")]),
                   pch = 19, ylim = yr + c(-0.08, 0.08) * diff(yr), xlab = estimate_label(est),
                   ylab = fit_names[[f$index]], main = "Person fit",
                   sub = if (hidden) paste(hidden, "persons without estimate or fit value not shown"),
                   legend = list("bottom!", title = NULL)),
              list(...))
  graphics::abline(h = c(-z, z), lty = 2, col = pp_col[["ink"]])
  if (length(top)) {
    graphics::text(d$estimate[top], d$value[top], d$label[top], pos = ifelse(d$value[top] < 0, 1, 3),
                   cex = 0.75, col = pp_col[["ink"]], xpd = TRUE)
  }
  rownames(d) <- NULL
  invisible(d)
}


# --- measurement precision ---------------------------------------------------

# Fisher information of each item at theta (length(theta) x items). It does not
# depend on the response, so the kernel is evaluated at response 0.
test_info <- function(bank, theta) {
  pp_item_eval_cpp(bank, theta, matrix(0L, length(theta), bank$n))$info
}

plot_precision <- function(est, curve, ...) {
  m <- est$model
  e <- est$estimates
  ok <- e$status == "ok" & is.finite(e$estimate) & is.finite(e$SE)
  if (!any(ok)) stop("No person has an estimate with status 'ok'.", call. = FALSE)
  prior <- est$type %in% c("map", "eap")
  same_prior <- !prior || length(unique(m$sigma2)) == 1
  if (is.null(curve)) {
    curve <- !anyNA(m$X) && same_prior
  } else if (!is.logical(curve) || length(curve) != 1 || is.na(curve)) {
    stop("'curve' must be NULL, TRUE or FALSE.", call. = FALSE)
  }

  theta <- seq(m$range[1], m$range[2], length.out = 1201)
  info <- test_info(m$bank, theta)
  items <- data.frame(item = item_names(m$X), location = theta[apply(info, 2, which.max)])
  persons <- data.frame(person = which(ok), estimate = e$estimate[ok], SE = e$SE[ok])
  xlim <- range(persons$estimate, items$location)
  xlim <- pmin(pmax(xlim + c(-0.05, 0.05) * diff(xlim) + c(-0.1, 0.1), m$range[1]), m$range[2])

  cv <- NULL
  if (curve) {
    inside <- theta >= xlim[1] & theta <= xlim[2]
    I <- rowSums(info[inside, , drop = FALSE])
    pv <- if (prior && same_prior) 1 / m$sigma2[1] else 0
    cv <- data.frame(theta = theta[inside], info = I, SE = 1 / sqrt(I + pv))
  }
  ymax <- max(persons$SE)
  if (!is.null(cv)) ymax <- min(max(ymax, cv$SE), 1.5 * ymax)

  dots <- list(...)
  main <- if (is.null(dots$main)) "Measurement precision" else dots$main
  dots$main <- NULL
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op))
  graphics::layout(matrix(1:2), heights = c(1, 2.2))
  graphics::par(mar = c(0.3, 4.1, 3.1, 1.1))
  tp(persons$estimate, type = "histogram", xlim = xlim, xaxt = "n", xlab = "", ylab = "Persons",
     main = main)
  hidden <- sum(!ok)
  if (hidden) {
    graphics::mtext(paste(hidden, "persons with status other than 'ok' not shown"), side = 3,
                    line = 0.2, cex = 0.8, col = pp_col[["ink"]])
  }
  graphics::par(mar = c(4.1, 4.1, 0.3, 1.1))
  pp_tinyplot(list(x = persons$estimate, y = persons$SE, xlim = xlim, ylim = c(0, 1.15 * ymax),
                   pch = 19, col = pp_col[["base"]], alpha = 0.5, xlab = estimate_label(est),
                   ylab = "Standard error"),
              dots)
  if (!is.null(cv)) graphics::lines(cv$theta, cv$SE, lwd = 2, col = pp_col[["ink"]])
  graphics::rug(items$location, ticksize = 0.04, lwd = 1.5, col = pp_col[["ink"]])
  leg <- c("persons", if (!is.null(cv)) paste("all", nrow(items), "items"), "item locations")
  graphics::legend("top", legend = leg, horiz = TRUE, bty = "n", cex = 0.85,
                   col = c(pp_col[["base"]], if (!is.null(cv)) pp_col[["ink"]], pp_col[["ink"]]),
                   pch = c(19, if (!is.null(cv)) NA, 124), lty = c(NA, if (!is.null(cv)) 1, NA),
                   lwd = c(NA, if (!is.null(cv)) 2, NA))
  invisible(list(persons = persons, curve = cv, items = items))
}

item_names <- function(X) {
  if (is.null(colnames(X))) paste0("I", seq_len(ncol(X))) else colnames(X)
}


# --- person profile ----------------------------------------------------------

# Expected score and standardised residual of every answered item of the persons
profile_data <- function(est, person) {
  m <- est$model
  X <- m$X[person, , drop = FALSE]
  mo <- pp_moments_cpp(m$bank, X, est$estimates$estimate[person])
  cell <- which(!is.na(X), arr.ind = TRUE)
  cell <- cell[order(cell[, 1], cell[, 2]), , drop = FALSE]
  mx <- (m$bank$ncat - 1L)[cell[, 2]]
  x <- X[cell]
  E <- mo$E[cell]
  data.frame(person = person[cell[, 1]], item = item_names(m$X)[cell[, 2]], response = x, max = mx,
             expected = E, z = (x - E) / sqrt(mo$V[cell]))
}

plot_profile <- function(est, person, f, z, labels, ...) {
  e <- est$estimates
  N <- nrow(e)
  if (is.null(person)) {
    v <- f$value
    fl <- which(e$status == "ok" & is.finite(v) & abs(v) > z)
    if (!length(fl)) {
      stop("No person is flagged by ", f$index, "; choose the persons with 'person'.", call. = FALSE)
    }
    # strongest misfit first: low lz and lz*, high infit and outfit t
    misfit <- if (f$index %in% c("lz", "lzstar")) -v[fl] else v[fl]
    person <- utils::head(fl[order(-misfit)], 4)
  } else if (!is.numeric(person) || !length(person) || anyNA(person) || any(person != round(person)) ||
             any(person < 1 | person > N) || anyDuplicated(person)) {
    stop("'person' must be distinct row positions between 1 and ", N, ".", call. = FALSE)
  }
  person <- as.integer(person)
  bad <- person[e$status[person] != "ok"]
  if (length(bad)) {
    stop("A profile needs an estimate with status 'ok'; person ",
         paste0(bad, " (", e$status[bad], ")", collapse = ", "), ".", call. = FALSE)
  }

  d <- profile_data(est, person)
  d$unexpected <- abs(d$z) > z
  title <- sprintf("Person %s: %.2f (SE %.2f)", labels[person], e$estimate[person], e$SE[person])
  if (!is.null(f)) {
    title <- paste0(title, sprintf(", %s = %.2f", fit_names[[f$index]], f$value[person]))
  }
  panel <- factor(title[match(d$person, person)], levels = title)
  lev <- c("as expected", sprintf("unexpected (|z| > %.2f)", z))
  grp <- factor(lev[d$unexpected + 1L], levels = lev)
  ex <- d$expected / d$max
  ob <- d$response / d$max
  pal <- unname(pp_col[c("base", "flag")])

  pp_tinyplot(list(x = ex, y = ob, by = grp, facet = panel, palette = pal, pch = 19,
                   xlim = c(1, 0), ylim = c(0, 1), xlab = "Expected score at the estimate (relative)",
                   ylab = "Observed score (relative)", legend = list("bottom!", title = NULL),
                   facet.args = list(axes = "outer", fmar = rep(0.75, 4))),
              list(...))
  # residuals and the expected score (the diagonal)
  tp(x = ex, y = ob, xmin = ex, xmax = ex, ymin = ex, ymax = ob, by = grp, facet = panel,
     palette = pal, type = "segments", add = TRUE)
  tp(x = ex, y = ob, facet = panel, type = tinyplot::type_abline(a = 0, b = 1),
     col = pp_col[["ink"]], add = TRUE)
  # names of the unexpected items, written from the point into the panel, beside
  # the residual; items with almost the same expected score share one label
  lab <- rep("", nrow(d))
  lx <- ex
  u <- which(d$unexpected)
  u <- u[order(d$person[u], ob[u], ex[u])]
  if (length(u)) {
    key <- paste(d$person[u], ob[u])
    grp_u <- cumsum(c(TRUE, key[-1] != key[-length(u)] | diff(ex[u]) > 0.03))
    for (g in unique(grp_u)) {
      i <- u[grp_u == g]
      lab[i[1]] <- paste(d$item[i], collapse = ", ")
      lx[i[1]] <- mean(ex[i])
    }
  }
  for (low in c(TRUE, FALSE)) {
    l <- ifelse((ob < 0.5) == low, lab, "")
    if (any(nzchar(l))) {
      tp(x = lx, y = ob, facet = panel, add = TRUE, col = pp_col[["ink"]], cex = 0.7,
         type = tinyplot::type_text(labels = l, srt = 90, adj = c(if (low) -0.1 else 1.1, -0.4),
                                    xpd = TRUE))
    }
  }
  invisible(d)
}
