# Person fit (lz, lz*, infit, outfit) against the references. The references
# are evaluated at PP's own estimate, so these tests check the fit formulas
# only, not the estimation.

# Reference fit for all persons with a finite estimate; f(theta, x) returns a
# vector, the result has one row per person
ref_fit <- function(theta, X, f) {
  m <- vapply(seq_len(nrow(X)), function(i) {
    if (is.finite(theta[i])) f(theta[i], X[i, ]) else NA * f(0, X[i, ])
  }, f(0, X[1, ]))
  if (is.matrix(m)) t(m) else matrix(m, ncol = 1)
}

fit_quiet <- function(X, obj, ...) pp_quiet(Pfit(X, obj, ...))

check_fit_4pl <- function(s, X, type, tol = 1e-5) {
  obj <- pp_fit(s, type, X = X)
  th <- est(obj)[, 1]
  ok <- is.finite(th)
  pf <- fit_quiet(X, obj, fitindices = c("lz", "lzstar", "infit", "outfit"))
  it <- s$items
  expect_close(pf$lz[ok, "lz"], ref_fit(th, X, function(t, x) ref_lz(t, x, it))[ok, ], tol)
  expect_close(pf$lzstar[ok, "lzstar"],
               ref_fit(th, X, function(t, x) ref_lzstar(t, x, it, type))[ok, ], tol)
  expect_close(pf$infit[ok, c("infit", "in_t")],
               ref_fit(th, X, function(t, x) ref_infit(t, x, it))[ok, ], tol)
  expect_close(pf$outfit[ok, c("outfit", "ou_t")],
               ref_fit(th, X, function(t, x) ref_outfit(t, x, it))[ok, ], tol)
}

check_fit_poly <- function(s, X, type, tol = 1e-5) {
  obj <- pp_fit(s, type, X = X)
  th <- est(obj)[, 1]
  ok <- is.finite(th)
  pf <- fit_quiet(X, obj, fitindices = c("infit", "outfit"))
  it <- s$items
  expect_close(pf$infit[ok, c("infit", "in_t")],
               ref_fit(th, X, function(t, x) ref_infit(t, x, it))[ok, ], tol)
  expect_close(pf$outfit[ok, c("outfit", "ou_t")],
               ref_fit(th, X, function(t, x) ref_outfit(t, x, it))[ok, ], tol)
}

for (nm in names(sets_dich)) {
  test_that(paste(nm, "lz, lz*, infit and outfit match the references"), {
    for (ty in c("mle", "wle", "map")) check_fit_4pl(sets_dich[[nm]], sets_dich[[nm]]$X, ty)
  })
}

test_that("lz, infit and outfit with an adaptive NA pattern match the references", {
  s <- set_2pl
  X <- make_na(s$X, 11)
  obj <- pp_fit(s, "wle", X = X)
  th <- est(obj)[, 1]
  pf <- fit_quiet(X, obj, fitindices = c("lz", "infit", "outfit"))
  expect_close(pf$lz[, "lz"], ref_fit(th, X, function(t, x) ref_lz(t, x, s$items)), 1e-5)
  expect_close(pf$infit[, c("infit", "in_t")],
               ref_fit(th, X, function(t, x) ref_infit(t, x, s$items)), 1e-5)
  expect_close(pf$outfit[, c("outfit", "ou_t")],
               ref_fit(th, X, function(t, x) ref_outfit(t, x, s$items)), 1e-5)
})

test_that("PCM infit and outfit match the references", {
  for (ty in c("mle", "wle")) check_fit_poly(set_pcm, set_pcm$X, ty)
})

test_that("the jackknife SE of lz (WLE) matches the reference", {
  s <- set_2pl
  X <- s$X[1:8, ]
  pf <- fit_quiet(X, pp_fit(s, "wle", X = X), fitindices = "lz", SE = TRUE)
  ref <- vapply(seq_len(nrow(X)), function(i) ref_lz_se(X[i, ], s$items, "wle"), 0)
  expect_close(pf$lz[, "lz_se"], ref, 1e-3)
})

# --- known bugs -----------------------------------------------------------

test_that("A9: lz* with missing responses matches the reference", {
  skip_known("A9", 7)
  s <- set_2pl
  X <- make_na(s$X, 11)
  for (ty in c("mle", "wle", "map")) {
    obj <- pp_fit(s, ty, X = X)
    th <- est(obj)[, 1]
    ok <- is.finite(th)
    pf <- fit_quiet(X, obj, fitindices = "lzstar")
    expect_close(pf$lzstar[ok, "lzstar"],
                 ref_fit(th, X, function(t, x) ref_lzstar(t, x, s$items, ty))[ok, ], 1e-5)
  }
})

test_that("A10: PCM fit with a category nobody reached", {
  skip_known("A10", 7)
  check_fit_poly(set_pcm_unobs, set_pcm_unobs$X, "wle")
})

test_that("A10: GPCM fit uses the slopes", {
  skip_known("A10", 7)
  for (ty in c("mle", "wle")) check_fit_poly(set_gpcm, set_gpcm$X, ty)
})

test_that("A6: the jackknife SE of lz uses person-specific priors", {
  skip_known("A6", 7)
  s <- set_2pl
  X <- s$X[1:8, ]
  mu <- prior_mu(8)
  s2 <- prior_s2(8)
  pf <- fit_quiet(X, pp_fit(s, "map", X = X, mu = mu, sigma2 = s2), fitindices = "lz", SE = TRUE)
  ref <- vapply(seq_len(nrow(X)), function(i) ref_lz_se(X[i, ], s$items, "map", mu[i], s2[i]), 0)
  expect_close(pf$lz[, "lz_se"], ref, 1e-3)
})

test_that("G5: fit statistics are not rounded", {
  skip_known("G5", 7)
  s <- set_2pl
  obj <- pp_fit(s, "wle")
  th <- est(obj)[, 1]
  pf <- fit_quiet(s$X, obj, fitindices = c("lz", "infit"))
  expect_close(pf$lz[, "lz"], ref_fit(th, s$X, function(t, x) ref_lz(t, x, s$items)), 1e-10)
  expect_close(pf$infit[, "infit"],
               ref_fit(th, s$X, function(t, x) ref_infit(t, x, s$items)["infit"]), 1e-10)
})

test_that("G6: Pfit works for mixed tests", {
  skip_known("G6", 7)
  check_fit_poly(set_mixed, set_mixed$X, "wle")
})

test_that("G6: Pfit takes the responses from the estimation object", {
  skip_known("G6", 7)
  obj <- pp_fit(set_2pl, "wle")
  expect_identical(pp_quiet(Pfit(pp = obj, fitindices = "infit"))$infit,
                   fit_quiet(set_2pl$X, obj, fitindices = "infit")$infit)
})
