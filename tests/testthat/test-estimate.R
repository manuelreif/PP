# Person estimates (mle, wle, map, robust) of PP_4pl, PP_gpcm and PPall
# against the roots of the estimating equations (helper-2-reference.R), on the
# default range c(-6, 6) of the wrappers.

types <- c("mle", "wle", "map", "robust")
rng <- c(-6, 6)

# PP call; robust converges only linearly, so its fixed point is checked with
# a tight convergence criterion
fit_tight <- function(set, type, ...) {
  if (type == "robust") pp_fit(set, type, exac = 1e-6, maxsteps = 500, ...)
  else pp_fit(set, type, ...)
}

for (nm in names(sets_all)) {
  test_that(paste(nm, "estimates and mle/wle SEs match the reference"), {
    s <- sets_all[[nm]]
    for (ty in types) {
      ref <- ref_estimates(s$X, s$items, ty, range = rng)
      res <- est(fit_tight(s, ty))
      expect_close(res[, 1], ref[, 1], 1e-3)
      if (ty %in% c("mle", "wle")) expect_close(res[, 2], ref[, 2], 1e-3)
    }
  })
}

test_that("estimates with an adaptive NA pattern match the reference", {
  for (nm in c("1PL", "2PL", "PCM", "GPCM", "mixed")) {
    s <- sets_all[[nm]]
    Xna <- make_na(s$X, 7)
    for (ty in if (nm == "mixed") c("wle", "map") else types) {
      ref <- ref_estimates(Xna, s$items, ty, range = rng)
      res <- est(fit_tight(s, ty, X = Xna))
      expect_close(res[, 1], ref[, 1], 1e-3)
      if (ty %in% c("mle", "wle")) expect_close(res[, 2], ref[, 2], 1e-3)
    }
  }
})

test_that("map estimates use person-specific priors", {
  for (s in list(set_2pl, set_gpcm, set_mixed)) {
    n <- nrow(s$X)
    ref <- ref_estimates(s$X, s$items, "map", prior_mu(n), prior_s2(n), range = rng)
    res <- est(pp_fit(s, "map", mu = prior_mu(n), sigma2 = prior_s2(n)))
    expect_close(res[, 1], ref[, 1], 1e-3)
  }
})

test_that("mle of an extreme score is +-Inf with SE NA", {
  for (s in list(set_2pl, set_gpcm, set_mixed)) {
    ext <- apply(s$X, 1, ref_is_extreme, items = s$items)
    res <- est(pp_fit(s, "mle"))
    expect_identical(unname(res[ext != 0, 1]), ext[ext != 0] * Inf)
    expect_true(all(is.na(res[ext != 0, 2])))
  }
})

test_that("the ctrl options of 0.6.4 give a warning and change nothing", {
  for (s in list(set_2pl, set_gpcm, set_mixed)) {
    X <- rbind(s$X[1, ], s$X, s$X[c(1, 3, 3), ])
    for (ty in c("mle", "wle", "map", "eap")) {
      for (opt in c("killdupli", "skipcheck")) {
        expect_warning(a <- est(pp_fit(s, ty, X = X, ctrl = setNames(list(TRUE), opt))),
                       paste0("'", opt, "' no longer needed and ignored"))
        expect_identical(a, est(pp_fit(s, ty, X = X)))
      }
    }
  }
})

# --- known bugs -----------------------------------------------------------

test_that("A7: the MAP SE includes the prior precision", {
  for (s in list(set_2pl, set_gpcm, set_mixed)) {
    n <- nrow(s$X)
    ref <- ref_estimates(s$X, s$items, "map", 0, 0.5, range = rng)
    res <- est(pp_fit(s, "map", mu = rep(0, n), sigma2 = rep(0.5, n)))
    expect_close(res[, 2], ref[, 2], 1e-3)
  }
})

test_that("B2: the robust SE is the sandwich with Huber weights", {
  for (s in list(set_2pl, set_gpcm)) {
    ref <- ref_estimates(s$X, s$items, "robust", range = rng)
    res <- est(fit_tight(s, "robust"))
    expect_close(res[, 2], ref[, 2], 1e-3)
  }
})

test_that("B2: robust estimates are accurate to exac with the default settings", {
  for (s in list(set_2pl, set_gpcm)) {
    ref <- ref_estimates(s$X, s$items, "robust", range = rng)
    expect_close(est(pp_fit(s, "robust"))[, 1], ref[, 1], 1e-3)
  }
})

test_that("A8: 3PL/4PL WLE of extreme scores does not depend on maxsteps", {
  # the extreme scores of the 3PL/4PL sets, where the old Newton oscillated
  wle_oscillates <- list("3PL" = 4, "4PL" = c(4, 9))
  for (nm in names(wle_oscillates)) {
    s <- sets_all[[nm]]
    X <- s$X[wle_oscillates[[nm]], , drop = FALSE]
    ref <- ref_estimates(X, s$items, "wle", range = rng)
    expect_close(est(pp_fit(s, "wle", X = X))[, 1], ref[, 1], 1e-3)
    expect_close(est(pp_fit(s, "wle", X = X, maxsteps = 41))[, 1], ref[, 1], 1e-3)
  }
})

test_that("A8: 3PL mle of the oscillating benchmark persons does not depend on maxsteps", {
  r40 <- est(pp_fit(osc3pl, "mle", maxsteps = 40))[, 1]
  r41 <- est(pp_fit(osc3pl, "mle", maxsteps = 41))[, 1]
  expect_false(anyNA(r40))
  expect_close(r40, r41, 1e-3)
})

test_that("A8: persons without a finite mle do not get NaN", {
  # 4PL: one wrong answer on an item with upper asymptote < 1, the likelihood
  # keeps rising; same in the mixed test
  for (s in list(set_4pl, set_mixed)) {
    ref <- ref_estimates(s$X, s$items, "mle", range = rng)
    bound <- abs(ref[, 1]) %in% 6
    expect_true(any(bound))
    expect_false(anyNA(est(pp_fit(s, "mle"))[bound, 1]))
  }
})

test_that("A5: a person without answers gets NA and does not affect the others", {
  X <- set_2pl$X
  X[2, ] <- NA
  for (ty in c("mle", "wle")) {
    res <- est(pp_fit(set_2pl, ty, X = X))
    expect_true(is.na(res[2, 1]))
    expect_close(res[-2, ], est(pp_fit(set_2pl, ty, X = X[-2, ])), 1e-10)
  }
})

test_that("A6: a scalar prior is recycled to all persons", {
  s <- set_2pl
  n <- nrow(s$X)
  for (ty in c("map", "eap")) {
    expect_close(est(pp_fit(s, ty, mu = 0.5, sigma2 = 2)),
                 est(pp_fit(s, ty, mu = rep(0.5, n), sigma2 = rep(2, n))), 1e-10)
  }
})

test_that("C1: an extreme item (b = +-40, slope 20) does not produce NaN", {
  s <- set_extreme
  for (ty in c("mle", "wle", "map")) {
    expect_false(anyNA(est(pp_fit(s, ty))[, 1]))
  }
  # person 1 has an interior mle; the map exists for both persons
  expect_close(est(pp_fit(s, "mle"))[1, 1], ref_estimate(s$X[1, ], s$items, "mle"), 1e-3)
  expect_close(est(pp_fit(s, "map"))[, 1], ref_estimates(s$X, s$items, "map", range = rng)[, 1], 1e-3)
})

test_that("G2: estimation with the default prior is silent", {
  p <- items_to_pp(set_2pl$items)
  expect_silent(PP_4pl(set_2pl$X, thres = p$b, slopes = p$slopes, type = "map"))
})

test_that("F7: the persons are numbered in the order of the response matrix", {
  X <- set_2pl$X
  rownames(X) <- paste0("person", seq_len(nrow(X)))
  o <- pp_fit(set_2pl, "wle", X = X)
  expect_identical(rownames(o$estimates), as.character(seq_len(nrow(X))))
  expect_identical(est(o), est(pp_fit(set_2pl, "wle")))
})
