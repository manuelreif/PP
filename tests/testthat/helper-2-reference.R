# Reference implementations.
#
# Plain R, deliberately naive and never calling PP. Derivatives are numerical
# (central differences), so the references do not share the analytic formulas
# of PP's core. They are slow, which is fine for small test data.
#
# An item is a list:
#   list(model = "4PL",  a = , b = , c = , d = )   P(X = 1) = c + (d - c) plogis(a (theta - b))
#   list(model = "GPCM", a = , delta = c(...))      P(X = k) ~ exp(sum_{m <= k} a (theta - delta_m))
# A test is a list of items. Responses are integer vectors (one person) with NA
# for items that were not answered; NA items are left out of every sum.

# --- item level ----------------------------------------------------------

ref_ncat <- function(item) if (item$model == "4PL") 2L else length(item$delta) + 1L

# Category probabilities, a length(theta) x K matrix (column k + 1 = category k)
ref_catprob <- function(item, theta) {
  if (item$model == "4PL") {
    z <- item$a * (theta - item$b)
    s <- item$d - item$c
    # Q is computed directly, not as 1 - P
    cbind((1 - item$d) + s * plogis(-z), item$c + s * plogis(z))
  } else {
    K <- ref_ncat(item)
    # eta_k = sum_{m <= k} a (theta - delta_m) = a k theta - a sum_{m <= k} delta_m
    eta <- item$a * (outer(theta, 0:(K - 1)) -
                       rep(c(0, cumsum(item$delta)), each = length(theta)))
    m <- eta[, 1]
    for (k in seq_len(K)[-1]) m <- pmax(m, eta[, k])
    p <- exp(eta - m)
    p / rowSums(p)
  }
}

num_d1 <- function(f, theta, h = 1e-5) (f(theta + h) - f(theta - h)) / (2 * h)
num_d2 <- function(f, theta, h = 1e-4) (f(theta + h) - 2 * f(theta) + f(theta - h)) / h^2

# Fisher information and Warm's J of one item, both from numerical P'_k, P''_k
ref_item_info <- function(item, theta) {
  p  <- ref_catprob(item, theta)
  d1 <- num_d1(function(t) ref_catprob(item, t), theta)
  rowSums(ifelse(p > 0, d1^2 / p, 0))
}

ref_item_J <- function(item, theta) {
  p  <- ref_catprob(item, theta)
  d1 <- num_d1(function(t) ref_catprob(item, t), theta)
  d2 <- num_d2(function(t) ref_catprob(item, t), theta)
  rowSums(ifelse(p > 0, d1 * d2 / p, 0))
}

# d/dtheta log P(X = x)
ref_item_score <- function(item, x, theta) {
  num_d1(function(t) log(ref_catprob(item, t)[, x + 1]), theta)
}

# Huber weight of the residual a (theta - location), location = b (4PL) or
# mean(delta) (GPCM)
ref_huber <- function(item, theta, H) {
  loc <- if (item$model == "4PL") item$b else mean(item$delta)
  r <- abs(item$a * (theta - loc))
  ifelse(r <= H, 1, H / r)
}

# --- person level (sums over answered items), vectorised over theta --------

ref_sum <- function(x, items, f) {
  obs <- which(!is.na(x))
  Reduce(`+`, lapply(obs, function(j) f(items[[j]], x[j])), 0)
}

ref_loglik <- function(theta, x, items) {
  ref_sum(x, items, function(it, xj) log(ref_catprob(it, theta)[, xj + 1]))
}
ref_score <- function(theta, x, items) {
  ref_sum(x, items, function(it, xj) ref_item_score(it, xj, theta))
}
ref_info <- function(theta, x, items) {
  ref_sum(x, items, function(it, xj) ref_item_info(it, theta))
}
ref_J <- function(theta, x, items) {
  ref_sum(x, items, function(it, xj) ref_item_J(it, theta))
}

# Estimating equation f(theta) = 0 of each method
ref_esteq <- function(theta, x, items, type, mu = 0, s2 = 1, H = 1) {
  switch(type,
    mle    = ref_score(theta, x, items),
    wle    = ref_score(theta, x, items) +
               ref_J(theta, x, items) / (2 * ref_info(theta, x, items)),
    map    = ref_score(theta, x, items) - (theta - mu) / s2,
    robust = ref_sum(x, items, function(it, xj)
               ref_huber(it, theta, H) * ref_item_score(it, xj, theta)),
    stop("unknown type ", type))
}

ref_is_extreme <- function(x, items) {
  obs <- which(!is.na(x))
  top <- vapply(items[obs], ref_ncat, 1L) - 1L
  if (all(x[obs] == 0)) return(-1)
  if (all(x[obs] == top)) return(1)
  0
}

# Root of the estimating equation on range. No answered item: NA. mle/robust
# with an extreme score: -Inf/Inf. No sign change on range: the bound the
# equation points to. More than one sign change: error, the test data are
# ambiguous and must be changed.
ref_estimate <- function(x, items, type, mu = 0, s2 = 1, H = 1, range = c(-10, 10)) {
  if (all(is.na(x))) return(NA_real_)
  if (type %in% c("mle", "robust") && ref_is_extreme(x, items) != 0) {
    return(ref_is_extreme(x, items) * Inf)
  }
  f <- function(t) ref_esteq(t, x, items, type, mu, s2, H)
  g <- seq(range[1], range[2], length.out = 401)
  fg <- f(g)
  if (anyNA(fg)) stop("ref_estimate: estimating equation not finite on the range")
  ch <- which(diff(sign(fg)) != 0)
  if (length(ch) == 0) return(if (fg[1] > 0) range[2] else range[1])
  if (length(ch) > 1 || fg[ch] < 0) {
    stop("ref_estimate: estimating equation has several roots (", type, ", x = ",
         paste(x, collapse = ""), ")")
  }
  # tol 1e-8: the numerical derivatives limit the accuracy to about 1e-9 anyway
  stats::uniroot(f, g[c(ch, ch + 1)], f.lower = fg[ch], f.upper = fg[ch + 1],
                 tol = 1e-8)$root
}

# Standard error at theta
ref_se <- function(theta, x, items, type, mu = 0, s2 = 1, H = 1) {
  if (!is.finite(theta)) return(NA_real_)
  switch(type,
    mle = , wle = 1 / sqrt(ref_info(theta, x, items)),
    map = 1 / sqrt(ref_info(theta, x, items) + 1 / s2),
    robust = {
      obs <- which(!is.na(x))
      w <- vapply(items[obs], ref_huber, 0, theta = theta, H = H)
      I <- vapply(items[obs], ref_item_info, 0, theta = theta)
      sqrt(sum(w^2 * I)) / sum(w * I)   # sandwich
    },
    stop("unknown type ", type))
}

# Estimates and SEs for a response matrix (n x 2, like PP's resPP)
ref_estimates <- function(X, items, type, mu = 0, s2 = 1, H = 1, range = c(-10, 10)) {
  n <- nrow(X)
  mu <- rep_len(mu, n)
  s2 <- rep_len(s2, n)
  t(vapply(seq_len(n), function(i) {
    th <- ref_estimate(X[i, ], items, type, mu[i], s2[i], H, range)
    c(estimate = th, SE = ref_se(th, X[i, ], items, type, mu[i], s2[i], H))
  }, c(estimate = 0, SE = 0)))
}

# --- posterior on a fine grid --------------------------------------------

ref_posterior <- function(x, items, mu = 0, s2 = 1, n = 4001) {
  g <- mu + sqrt(s2) * seq(-10, 10, length.out = n)
  lp <- ref_loglik(g, x, items) + stats::dnorm(g, mu, sqrt(s2), log = TRUE)
  w <- exp(lp - max(lp))
  w <- w / sum(w)
  m <- sum(w * g)
  list(mean = m, sd = sqrt(sum(w * (g - m)^2)), grid = g, w = w)
}

# EAP and posterior SD for a response matrix (n x 2)
ref_eap <- function(X, items, mu = 0, s2 = 1) {
  n <- nrow(X)
  mu <- rep_len(mu, n)
  s2 <- rep_len(s2, n)
  t(vapply(seq_len(n), function(i) {
    p <- ref_posterior(X[i, ], items, mu[i], s2[i])
    c(estimate = p$mean, SE = p$sd)
  }, c(estimate = 0, SE = 0)))
}

# --- jackknife -----------------------------------------------------------

# Textbook jackknife over the L answered items of one person: leave-one-out
# estimates theta_(-j), pseudovalues ps_j = L theta - (L - 1) theta_(-j),
# estimate = mean, median or AMT of ps, SE = sqrt(sum (ps - mean ps)^2 / (m (m - 1)))
# over the m pseudovalues. A leave-one-out estimate that is not finite or not
# inside range has no solution there and is left out (excluded); estimate and
# SE are NA if theta itself has none or fewer than 2 pseudovalues remain (no
# jackknife runs, nothing is excluded, if theta has none or L < 2). loo
# has one entry per item (the raw values, also the excluded ones), NA for items
# the person did not answer.
ref_jackknife <- function(x, items, type, mu = 0, s2 = 1, H = 1, cmeth = "mean",
                          range = c(-6, 6)) {
  fit <- function(xx) {
    if (type == "eap") ref_posterior(xx, items, mu, s2)$mean
    else ref_estimate(xx, items, type, mu, s2, H)
  }
  inside <- function(t) is.finite(t) & t > range[1] & t < range[2]
  obs <- which(!is.na(x))
  L <- length(obs)
  theta <- fit(x)
  loo <- rep(NA_real_, length(x))
  loo[obs] <- vapply(obs, function(j) { xx <- x; xx[j] <- NA; fit(xx) }, 0)
  ok <- obs[inside(loo[obs])]
  ps <- L * theta - (L - 1) * loo[ok]
  m <- length(ps)
  est <- se <- NA_real_
  if (inside(theta) && m >= 2) {
    est <- switch(cmeth, mean = mean(ps), median = stats::median(ps), AMT = ref_amt(ps))
    se <- sqrt(sum((ps - mean(ps))^2) / (m * (m - 1)))
  }
  list(estimate = est, se = se, theta = theta, loo = loo,
       excluded = if (inside(theta) && L >= 2) L - m else 0)
}

# Andrews' sine M-estimate (AMT) as the minimum of sum(rho((x - T) / (c s)))
# with rho(u) = 1 - cos(u) for |u| <= pi, else 2 (its derivative is the sine
# psi), s the median absolute deviation from the median. The minimum is
# searched within c s pi of the median.
ref_amt <- function(x, c = 2.1) {
  m <- stats::median(x)
  s <- stats::median(abs(x - m))
  if (s == 0) return(m)
  rho <- function(t) {
    u <- (x - t) / (c * s)
    sum(ifelse(abs(u) <= pi, 1 - cos(u), 2))
  }
  stats::optimize(rho, m + c(-1, 1) * c * s * pi, tol = 1e-12)$minimum
}

# --- person fit (theta is given, usually PP's own estimate) ----------------

# Mean, variance and 4th central moment of the item score at theta
ref_moments <- function(item, theta) {
  p <- ref_catprob(item, theta)[1, ]
  k <- seq_along(p) - 1
  E <- sum(k * p)
  c(E = E, V = sum((k - E)^2 * p), C = sum((k - E)^4 * p))
}

# Wilson-Hilferty transformation of a mean square with variance q2
ref_wh <- function(ms, q2) (ms^(1 / 3) - 1) * (3 / sqrt(q2)) + sqrt(q2) / 3

ref_fit_parts <- function(theta, x, items) {
  obs <- which(!is.na(x))
  m <- vapply(obs, function(j) ref_moments(items[[j]], theta), c(E = 0, V = 0, C = 0))
  list(Y = x[obs] - m["E", ], V = m["V", ], C = m["C", ], n = length(obs))
}

ref_infit <- function(theta, x, items) {
  p <- ref_fit_parts(theta, x, items)
  ms <- sum(p$Y^2) / sum(p$V)
  q2 <- sum(p$C - p$V^2) / sum(p$V)^2
  c(infit = ms, in_t = ref_wh(ms, q2))
}

ref_outfit <- function(theta, x, items) {
  p <- ref_fit_parts(theta, x, items)
  ms <- sum(p$Y^2 / p$V) / p$n
  q2 <- sum(p$C / p$V^2) / p$n^2 - 1 / p$n
  c(outfit = ms, ou_t = ref_wh(ms, q2))
}

# lz (Drasgow, Levine & Williams, 1985), dichotomous and polytomous
ref_lz <- function(theta, x, items) {
  obs <- which(!is.na(x))
  l0 <- E <- V <- 0
  for (j in obs) {
    lp <- log(ref_catprob(items[[j]], theta)[1, ])
    p <- exp(lp)
    l0 <- l0 + lp[x[j] + 1]
    E <- E + sum(p * lp)
    V <- V + sum(p * lp^2) - sum(p * lp)^2
  }
  (l0 - E) / sqrt(V)
}

# lz* (Snijders, 2001) for dichotomous items. The estimating equation is
# sum r_i (x_i - P_i) + r0(theta) = 0 with r_i = P'_i / (P_i Q_i) and
# r0 = 0 (mle), J / (2 I) (wle), (mu - theta) / s2 (map).
ref_lzstar <- function(theta, x, items, type, mu = 0, s2 = 1) {
  obs <- which(!is.na(x))
  its <- items[obs]
  P  <- vapply(its, function(it) ref_catprob(it, theta)[1, 2], 0)
  Q  <- vapply(its, function(it) ref_catprob(it, theta)[1, 1], 0)
  dP <- vapply(its, function(it) num_d1(function(t) ref_catprob(it, t)[, 2], theta), 0)
  r <- dP / (P * Q)
  w <- log(P / Q)
  n <- length(obs)
  W <- sum((x[obs] - P) * w)
  cn <- sum(dP * w) / sum(dP * r)
  tau2 <- sum((w - cn * r)^2 * P * Q) / n
  r0 <- switch(type,
    mle = 0,
    wle = ref_J(theta, x, items) / (2 * ref_info(theta, x, items)),
    map = (mu - theta) / s2)
  (W + cn * r0) / sqrt(n * tau2)
}

# Jackknife SE of lz over the answered items: theta and lz are recomputed
# without item j, SE = sqrt((L - 1) / L * sum (lz_(-j) - mean)^2)
ref_lz_se <- function(x, items, type, mu = 0, s2 = 1) {
  obs <- which(!is.na(x))
  L <- length(obs)
  lzj <- vapply(obs, function(j) {
    xx <- x
    xx[j] <- NA
    ref_lz(ref_estimate(xx, items, type, mu, s2), xx, items)
  }, 0)
  sqrt((L - 1) / L * sum((lzj - mean(lzj))^2))
}
