# The posterior grid pp_posterior_cpp() (src/core.cpp) directly: EAP and
# posterior SD against the posterior on a fine grid (helper-2-reference.R) in
# both stages, and the plausible-value draws.

core_post <- function(set, X = set$X, mu = 0, s2 = 1, npv = 0L, ...) {
  n <- nrow(X)
  PP:::pp_posterior_cpp(kernel_bank(set$items), X, rep_len(mu, n), rep_len(s2, n), npv, ...)
}

expect_post <- function(r, ref, tol = 1e-6) {
  expect_close(r$eap, ref[, 1], tol)
  expect_close(r$psd, ref[, 2], tol)
}

test_that("EAP and PSD of all sets match the posterior", {
  for (nm in names(sets_all)) {
    s <- sets_all[[nm]]
    r <- core_post(s)
    expect_post(r, ref_eap(s$X, s$items))
    expect_true(all(r$stage %in% 1:2))
    expect_identical(dim(r$draws), c(nrow(s$X), 0L))
  }
})

test_that("EAP and PSD with missing responses and person-specific priors match the posterior", {
  for (nm in c("2PL", "3PL", "GPCM", "mixed")) {
    s <- sets_all[[nm]]
    X <- make_na(s$X, 7)
    n <- nrow(X)
    expect_post(core_post(s, X, prior_mu(n), prior_s2(n)), ref_eap(X, s$items, prior_mu(n), prior_s2(n)))
  }
})

test_that("a person without answers gets the prior", {
  s <- set_gpcm
  X <- s$X[1:3, ]
  X[2, ] <- NA
  r <- core_post(s, X, mu = c(0, 1.5, 0), s2 = c(1, 0.5, 1))
  expect_close(r$eap[2], 1.5, 1e-6)
  expect_close(r$psd[2], sqrt(0.5), 1e-6)
})

test_that("sharp posteriors of a long test go to stage 2 and match the posterior", {
  r <- core_post(set_long)
  expect_true(all(r$stage == 2L))
  ref <- ref_eap(set_long$X, set_long$items)
  expect_post(r, ref)
  # without the stage-2 table: the windows are evaluated with the kernel
  expect_post(core_post(set_long, max_table = 1), ref)
})

test_that("a very small prior variance goes to stage 2", {
  s <- set_2pl
  n <- nrow(s$X)
  s2 <- c(1e-4, rep(1, n - 1))
  r <- core_post(s, s2 = s2)
  expect_identical(r$stage[1], 2L)
  expect_post(r, ref_eap(s$X, s$items, 0, s2))
})

test_that("a posterior beyond the grid of the prior is found", {
  # 3000 Rasch items at b = 9: the posterior lies near 9, the stage-1 grid
  # (prior N(0, 1)) ends at 6
  items <- items_4pl(1, rep(9, 3000))
  s <- list(items = items, X = sim_resp(c(9, 8.7), items, 5))
  r <- core_post(s)
  expect_identical(r$stage, c(2L, 2L))
  expect_post(r, ref_eap(s$X, items))
  expect_gt(min(r$eap), 8.5)
})

test_that("the draws are reproducible and follow the posterior", {
  s <- set_2pl
  set.seed(3)
  a <- core_post(s, npv = 4L)
  set.seed(3)
  b <- core_post(s, npv = 4L)
  expect_identical(dim(a$draws), c(nrow(s$X), 4L))
  expect_identical(a$draws, b$draws)
  expect_identical(a$eap, core_post(s)$eap)
  # one person replicated, in stage 1 (12 items) and stage 2 (1100 items);
  # tolerance 4 Monte Carlo SEs of independent draws
  for (set in list(set_2pl, set_long)) {
    X <- matrix(set$X[3, ], 2000, ncol(set$X), byrow = TRUE)
    set.seed(4)
    r <- core_post(set, X, npv = 20L)
    post <- ref_posterior(set$X[3, ], set$items)
    m <- length(r$draws)
    expect_close(mean(r$draws), post$mean, 4 * post$sd / sqrt(m))
    expect_close(sd(as.vector(r$draws)), post$sd, 4 * post$sd / sqrt(2 * m))
  }
})

# --- jackknife (jk = TRUE): posterior mean without item j ------------------

# Reference: the posterior mean of X[i, ] with item j set to NA
ref_loo_eap <- function(X, items, mu = 0, s2 = 1, cols = seq_len(ncol(X))) {
  n <- nrow(X)
  mu <- rep_len(mu, n)
  s2 <- rep_len(s2, n)
  out <- matrix(NA_real_, n, length(cols))
  for (i in seq_len(n)) for (k in seq_along(cols)) {
    if (is.na(X[i, cols[k]])) next
    x <- X[i, ]
    x[cols[k]] <- NA
    out[i, k] <- ref_posterior(x, items, mu[i], s2[i])$mean
  }
  out
}

test_that("the EAP jackknife matches the posterior without the item", {
  for (nm in c("2PL", "3PL", "GPCM", "mixed")) {
    s <- sets_all[[nm]]
    X <- make_na(s$X[1:12, ], 7)
    n <- nrow(X)
    r <- core_post(s, X, prior_mu(n), prior_s2(n), jk = TRUE)
    expect_close(r$loo, ref_loo_eap(X, s$items, prior_mu(n), prior_s2(n)), 1e-6)
    # EAP and PSD do not change with jk
    expect_identical(r[c("eap", "psd", "stage")],
                     core_post(s, X, prior_mu(n), prior_s2(n))[c("eap", "psd", "stage")])
  }
  expect_null(core_post(set_2pl)$loo)
})

test_that("the EAP jackknife works in stage 2, also without the table", {
  cols <- c(1, 550, 1100)
  ref <- ref_loo_eap(set_long$X, set_long$items, cols = cols)
  expect_close(core_post(set_long, jk = TRUE)$loo[, cols], ref, 1e-6)
  expect_close(core_post(set_long, jk = TRUE, max_table = 1)$loo[, cols], ref, 1e-6)
  s <- set_2pl
  s2 <- c(1e-4, rep(1, 4))
  r <- core_post(s, s$X[1:5, ], s2 = s2, jk = TRUE)
  expect_identical(r$stage[1], 2L)
  expect_close(r$loo, ref_loo_eap(s$X[1:5, ], s$items, 0, s2), 1e-6)
})

test_that("a leave-one-out posterior beyond the nodes of the full posterior is found", {
  # the steep item 41 (answered 0) holds the posterior below 4; without it the
  # 40 easy answers move it to about 8.5, beyond the stage-2 window. Prior mean
  # 2: the grid of ref_posterior (mu +- 10 sigma) must cover the posterior.
  items <- c(items_4pl(1, rep(7, 40)), list(item_4pl(20, 4)))
  s <- list(items = items)
  X <- matrix(c(rep(1L, 40), 0L), 1)
  r <- core_post(s, X, mu = 2, jk = TRUE)
  expect_identical(r$stage, 2L)
  ref <- ref_loo_eap(X, items, mu = 2)
  expect_gt(ref[41], 6)
  expect_close(r$loo, ref, 1e-6)
})

test_that("invalid input is rejected", {
  s <- set_2pl
  expect_error(core_post(s, s2 = 0), "sigma2")
  expect_error(core_post(s, mu = NA), "mu")
  expect_error(core_post(s, X = s$X[, -1]), "one column per item")
  X <- s$X
  X[1, 1] <- 2L
  expect_error(core_post(s, X), "not a category")
})
