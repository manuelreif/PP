# The references themselves are checked against cases with a known answer:
# closed forms, independent optimisation/integration and textbook identities.
# If one of these fails, the safety net is broken, not PP.

theta_grid <- seq(-4, 4, by = 0.5)

test_that("category probabilities are positive and sum to 1", {
  for (it in c(set_4pl$items, set_gpcm$items, set_mixed$items, set_gpcm12$items)) {
    p <- ref_catprob(it, c(-30, theta_grid, 30))
    expect_true(all(p >= 0))
    expect_close(rowSums(p), rep(1, nrow(p)), 1e-12)
  }
})

test_that("a GPCM item with two categories is a 2PL item", {
  expect_close(ref_catprob(item_gpcm(1.3, 0.4), theta_grid),
               ref_catprob(item_4pl(1.3, 0.4), theta_grid), 1e-12)
})

test_that("numerical score, information and J match the 2PL closed forms", {
  items <- set_2pl$items
  x <- set_2pl$X[1, ]
  a <- vapply(items, `[[`, 0, "a")
  b <- vapply(items, `[[`, 0, "b")
  for (th in theta_grid) {
    P <- plogis(a * (th - b))
    expect_close(ref_score(th, x, items), sum(a * (x - P)), 1e-6)
    expect_close(ref_info(th, x, items), sum(a^2 * P * (1 - P)), 1e-6)
    expect_close(ref_J(th, x, items), sum(a^3 * P * (1 - P) * (1 - 2 * P)), 1e-5)
  }
})

test_that("numerical information matches the 4PL closed form", {
  items <- set_4pl$items
  x <- set_4pl$X[1, ]
  for (th in theta_grid) {
    I <- sum(vapply(items, function(it) {
      L <- plogis(it$a * (th - it$b))
      P <- it$c + (it$d - it$c) * L
      dP <- (it$d - it$c) * it$a * L * (1 - L)
      dP^2 / (P * (1 - P))
    }, 0))
    expect_close(ref_info(th, x, items), I, 1e-6)
  }
})

test_that("GPCM information is a^2 Var(X) and J is a^3 times the third central moment", {
  for (it in set_gpcm$items) {
    for (th in theta_grid) {
      p <- ref_catprob(it, th)[1, ]
      k <- seq_along(p) - 1
      E <- sum(k * p)
      expect_close(ref_item_info(it, th), it$a^2 * sum((k - E)^2 * p), 1e-6)
      expect_close(ref_item_J(it, th), it$a^3 * sum((k - E)^3 * p), 1e-5)
    }
  }
})

test_that("1PL MLE: the raw score equals the expected score", {
  items <- set_1pl$items
  for (i in seq_len(nrow(set_1pl$X))) {
    x <- set_1pl$X[i, ]
    th <- ref_estimate(x, items, "mle")
    if (!is.finite(th)) next
    expect_close(sum(x), sum(vapply(items, function(it) ref_catprob(it, th)[1, 2], 0)), 1e-6)
  }
})

test_that("mle, wle (2PL) and map maximise their objective functions", {
  items <- set_2pl$items
  for (i in c(1, 2, 3, 10)) {
    x <- set_2pl$X[i, ]
    obj <- list(
      mle = function(t) ref_loglik(t, x, items),
      # for the 2PL, J = I', so Warm's equation is the derivative of l + log(I) / 2
      wle = function(t) ref_loglik(t, x, items) + log(ref_info(t, x, items)) / 2,
      map = function(t) ref_loglik(t, x, items) + dnorm(t, 0.5, sqrt(2), log = TRUE))
    for (ty in names(obj)) {
      opt <- optimize(obj[[ty]], c(-8, 8), maximum = TRUE, tol = 1e-10)$maximum
      expect_close(ref_estimate(x, items, ty, mu = 0.5, s2 = 2), opt, 1e-5)
    }
  }
})

test_that("mle of an extreme score is infinite, no answers give NA", {
  items <- set_2pl$items
  expect_identical(ref_estimate(rep(0L, 12), items, "mle"), -Inf)
  expect_identical(ref_estimate(rep(1L, 12), items, "robust"), Inf)
  expect_true(is.finite(ref_estimate(rep(1L, 12), items, "wle")))
  expect_identical(ref_estimate(rep(NA_integer_, 12), items, "wle"), NA_real_)
})

test_that("the posterior without answers is the prior", {
  p <- ref_posterior(rep(NA_integer_, 12), set_2pl$items, mu = 1, s2 = 0.5)
  expect_close(c(p$mean, p$sd), c(1, sqrt(0.5)), 1e-8)
})

test_that("the posterior mean matches integrate()", {
  items <- set_gpcm$items
  x <- set_gpcm$X[3, ]
  f <- function(t) exp(ref_loglik(t, x, items)) * dnorm(t, -0.5, 1.2)
  m <- integrate(function(t) t * f(t), -Inf, Inf)$value / integrate(f, -Inf, Inf)$value
  expect_close(ref_posterior(x, items, -0.5, 1.44)$mean, m, 1e-6)
})

test_that("both jackknife SE formulas agree (pseudovalues vs. leave-one-out)", {
  items <- set_2pl$items
  x <- set_2pl$X[2, ]
  jk <- ref_jackknife(x, items, "wle")
  L <- length(x)
  expect_close(jk$se, sqrt((L - 1) / L * sum((jk$loo - mean(jk$loo))^2)), 1e-10)
  expect_close(jk$estimate, L * jk$theta - (L - 1) * mean(jk$loo), 1e-10)
})

test_that("leaving out an unanswered item changes nothing in the jackknife", {
  items <- set_2pl$items
  x <- set_2pl$X[2, ]
  xna <- x
  xna[c(3, 7)] <- NA
  jk <- ref_jackknife(xna, items, "wle")
  jk_short <- ref_jackknife(x[-c(3, 7)], items[-c(3, 7)], "wle")
  expect_close(c(jk$estimate, jk$se), c(jk_short$estimate, jk_short$se), 1e-10)
  expect_true(all(is.na(jk$loo[c(3, 7)])))
})

test_that("AMT is the median for symmetric data and ignores a far outlier", {
  x <- c(-2.1, -1, -0.4, 0.3, 0.9, 1.6, 2.7)
  xs <- c(x, 2 * stats::median(x) - x)
  expect_close(ref_amt(xs), stats::median(xs), 1e-6)
  # the outlier is beyond 2.1 * pi * MAD of the median: no weight
  y <- c(-1.2, -0.5, -0.3, 0.1, 0.2, 0.6, 1.1)
  s <- stats::median(abs(y - stats::median(y)))
  out <- stats::median(y) + 2.1 * pi * s * 3
  expect_close(ref_amt(c(y, out)), ref_amt(c(y, out + 50)), 1e-6)
  expect_close(ref_amt(c(1, 1, 1, 5)), 1, 0)
})

test_that("fit statistics match a hand calculation for two 2PL items", {
  items <- items_4pl(c(1, 2), c(0, 1))
  x <- c(1L, 0L)
  th <- 0.3
  P <- plogis(c(1, 2) * (th - c(0, 1)))
  V <- P * (1 - P)
  C <- P * (1 - P)^4 + (1 - P) * P^4
  Y <- x - P
  ms_in <- sum(Y^2) / sum(V)
  ms_out <- mean(Y^2 / V)
  q_in <- sqrt(sum(C - V^2) / sum(V)^2)
  q_out <- sqrt(sum(C / V^2) / 4 - 1 / 2)
  expect_close(ref_infit(th, x, items),
               c(ms_in, (ms_in^(1 / 3) - 1) * 3 / q_in + q_in / 3), 1e-10)
  expect_close(ref_outfit(th, x, items),
               c(ms_out, (ms_out^(1 / 3) - 1) * 3 / q_out + q_out / 3), 1e-10)
  lz <- (sum(x * log(P) + (1 - x) * log(1 - P)) - sum(P * log(P) + (1 - P) * log(1 - P))) /
    sqrt(sum(P * (1 - P) * log(P / (1 - P))^2))
  expect_close(ref_lz(th, x, items), lz, 1e-10)
})

test_that("lz* matches the 2PL closed form at the mle, wle and map estimate", {
  # At the estimate of each method, W + c_n r0 = sum (x - P) w~ with
  # w~ = w - c_n r. For the 2PL r = a and w = log(P / Q) = a (theta - b).
  items <- set_2pl$items
  a <- vapply(items, `[[`, 0, "a")
  b <- vapply(items, `[[`, 0, "b")
  for (i in c(1, 2, 3, 10)) {
    x <- set_2pl$X[i, ]
    for (ty in c("mle", "wle", "map")) {
      th <- ref_estimate(x, items, ty, mu = 0.5, s2 = 2)
      P <- plogis(a * (th - b))
      cn <- sum(a^2 * P * (1 - P) * (th - b)) / sum(a^2 * P * (1 - P))
      wt <- a * (th - b) - cn * a
      expect_close(ref_lzstar(th, x, items, ty, mu = 0.5, s2 = 2),
                   sum((x - P) * wt) / sqrt(sum(wt^2 * P * (1 - P))), 1e-6)
    }
  }
})
