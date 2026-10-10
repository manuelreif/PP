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

test_that("invalid input is rejected", {
  s <- set_2pl
  expect_error(core_post(s, s2 = 0), "sigma2")
  expect_error(core_post(s, mu = NA), "mu")
  expect_error(core_post(s, X = s$X[, -1]), "one column per item")
  X <- s$X
  X[1, 1] <- 2L
  expect_error(core_post(s, X), "not a category")
})
