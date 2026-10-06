# Plausible values are draws from the posterior. One response pattern is
# replicated n times, so the pooled draws can be compared with the posterior on
# the fine grid (helper-2-reference.R). Tolerance: 4 Monte Carlo SEs of
# independent draws.

replicate_person <- function(set, i, n) {
  matrix(set$X[i, ], n, ncol(set$X), byrow = TRUE)
}

expect_draws_match <- function(draws, post) {
  m <- length(draws)
  expect_close(mean(draws), post$mean, 4 * post$sd / sqrt(m))
  expect_close(sd(as.vector(draws)), post$sd, 4 * post$sd / sqrt(2 * m))
}

test_that("PV returns npv draws per person and is reproducible", {
  for (s in list(set_2pl, set_gpcm, set_mixed)) {
    e <- pp_fit(s, "eap")
    for (approx in c(TRUE, FALSE)) {
      set.seed(11)
      a <- PV(e, npv = 4, approx = approx)$pvdraws
      set.seed(11)
      b <- PV(e, npv = 4, approx = approx)$pvdraws
      expect_identical(dim(a), c(nrow(s$X), 4L))
      expect_identical(a, b)
    }
  }
})

test_that("approx = TRUE on an EAP object draws from N(EAP, PSD^2)", {
  for (s in list(set_2pl, set_gpcm)) {
    e <- pp_fit(s, "eap", X = replicate_person(s, 3, 1000))
    set.seed(12)
    expect_draws_match(PV(e, npv = 20, approx = TRUE)$pvdraws,
                       ref_posterior(s$X[3, ], s$items))
  }
})

# --- known bugs -----------------------------------------------------------

test_that("A4: PV draws follow the posterior (no overdispersion)", {
  skip_known("A4", 5)
  for (s in list(set_2pl, set_gpcm)) {
    e <- pp_fit(s, "eap", X = replicate_person(s, 3, 2000))
    set.seed(13)
    expect_draws_match(PV(e, npv = 20, approx = FALSE)$pvdraws,
                       ref_posterior(s$X[3, ], s$items))
  }
})

test_that("A4: PV draws use the person-specific prior", {
  skip_known("A4", 5)
  s <- set_2pl
  e <- pp_fit(s, "eap", X = replicate_person(s, 3, 300), mu = rep(2, 300), sigma2 = rep(1, 300))
  set.seed(14)
  expect_draws_match(PV(e, npv = 20, approx = FALSE)$pvdraws,
                     ref_posterior(s$X[3, ], s$items, mu = 2, s2 = 1))
})

test_that("A4: PV works with missing responses", {
  skip_known("A4", 5)
  e <- pp_fit(set_2pl, "eap", X = make_na(set_2pl$X, 9))
  set.seed(15)
  draws <- PV(e, npv = 3, approx = FALSE)$pvdraws
  expect_false(anyNA(draws))
})

test_that("A4: PV of an mle object with extreme scores keeps the rows of the persons", {
  skip_known("A4", 5)
  s <- set_2pl
  m <- pp_fit(s, "mle")
  set.seed(16)
  draws <- PV(m, npv = 200, approx = FALSE)$pvdraws
  expect_identical(nrow(draws), nrow(s$X))
  post <- ref_eap(s$X, s$items)
  # person means of 200 draws: 5 MC-SE
  expect_close(rowMeans(draws), post[, 1], 5 * max(post[, 2]) / sqrt(200))
})

test_that("B4: approx = TRUE on a WLE object draws from the posterior", {
  skip_known("B4", 5)
  s <- set_2pl
  w <- pp_fit(s, "wle", X = replicate_person(s, 3, 1000))
  set.seed(17)
  expect_draws_match(PV(w, npv = 20, approx = TRUE)$pvdraws,
                     ref_posterior(s$X[3, ], s$items))
})
