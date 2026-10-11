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
      expect_false(anyNA(a))
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

test_that("PV of a long test (narrow posterior) follows the posterior", {
  s <- set_long
  e <- pp_fit(s, "eap", X = replicate_person(s, 3, 1000))
  set.seed(18)
  expect_draws_match(PV(e, npv = 20, approx = FALSE)$pvdraws, ref_posterior(s$X[3, ], s$items))
})

test_that("PV names the columns, rows are the persons in order", {
  X <- set_2pl$X[1:3, ]
  rownames(X) <- c("a", "b", "c")
  pv <- PV(pp_fit(set_2pl, "eap", X = X), npv = 2)
  expect_s3_class(pv, "pv")
  expect_identical(dimnames(pv$pvdraws), list(NULL, c("PV1", "PV2")))
  # one person, one value
  one <- PV(pp_fit(set_2pl, "eap", X = X[1, , drop = FALSE]), npv = 1, approx = FALSE)$pvdraws
  expect_identical(dim(one), c(1L, 1L))
})

test_that("duplicated persons get independent draws, also with killdupli", {
  X <- replicate_person(set_gpcm, 2, 3)
  e <- pp_fit(set_gpcm, "eap", X = X, ctrl = list(killdupli = TRUE))
  for (approx in c(TRUE, FALSE)) {
    d <- PV(e, npv = 5, approx = approx)$pvdraws
    expect_false(any(d[1, ] == d[2, ]))
  }
})

test_that("the arguments of the old sampler give a warning and are ignored", {
  e <- pp_fit(set_2pl, "eap")
  set.seed(19)
  expect_warning(a <- PV(e, npv = 2, approx = FALSE, thinning = 3, burnin = 5)$pvdraws,
                 "'thinning', 'burnin' no longer used")
  set.seed(19)
  expect_identical(a, PV(e, npv = 2, approx = FALSE)$pvdraws)
  expect_error(PV(e, npv = 0), "npv")
  expect_error(PV(e, npv = 2.5), "npv")
  expect_error(PV(e, approx = NA), "approx")
})

# --- known bugs -----------------------------------------------------------

test_that("A4: PV draws follow the posterior (no overdispersion)", {
  for (s in list(set_2pl, set_gpcm)) {
    e <- pp_fit(s, "eap", X = replicate_person(s, 3, 2000))
    set.seed(13)
    expect_draws_match(PV(e, npv = 20, approx = FALSE)$pvdraws,
                       ref_posterior(s$X[3, ], s$items))
  }
})

test_that("A4: PV draws use the person-specific prior", {
  s <- set_2pl
  e <- pp_fit(s, "eap", X = replicate_person(s, 3, 300), mu = rep(2, 300), sigma2 = rep(1, 300))
  set.seed(14)
  expect_draws_match(PV(e, npv = 20, approx = FALSE)$pvdraws,
                     ref_posterior(s$X[3, ], s$items, mu = 2, s2 = 1))
})

test_that("A4: PV works with missing responses", {
  e <- pp_fit(set_2pl, "eap", X = make_na(set_2pl$X, 9))
  set.seed(15)
  draws <- PV(e, npv = 3, approx = FALSE)$pvdraws
  expect_false(anyNA(draws))
})

test_that("A4: PV keeps the rows of persons with extreme scores", {
  s <- set_2pl
  m <- pp_fit(s, "eap")
  set.seed(16)
  draws <- PV(m, npv = 200, approx = FALSE)$pvdraws
  expect_identical(nrow(draws), nrow(s$X))
  post <- ref_eap(s$X, s$items)
  # person means of 200 draws: 5 MC-SE
  expect_close(rowMeans(draws), post[, 1], 5 * max(post[, 2]) / sqrt(200))
})

test_that("B4: PV on a WLE object is refused instead of drawing from N(WLE, SE^2)", {
  w <- pp_fit(set_2pl, "wle")
  expect_error(PV(w, npv = 20, approx = TRUE), "prior")
})

test_that("B6: PV needs an object with a prior (type eap or map)", {
  for (s in list(set_2pl, set_gpcm, set_mixed)) {
    for (ty in c("mle", "wle", "robust")) {
      expect_error(PV(pp_fit(s, ty), npv = 2), "prior")
    }
  }
})

test_that("B6: PV of a MAP object draws from the posterior with its prior", {
  s <- set_2pl
  m <- pp_fit(s, "map", X = replicate_person(s, 3, 300), mu = rep(2, 300), sigma2 = rep(1, 300))
  set.seed(19)
  expect_draws_match(PV(m, npv = 20, approx = FALSE)$pvdraws,
                     ref_posterior(s$X[3, ], s$items, mu = 2, s2 = 1))
})
