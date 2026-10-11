# Person fit (lz, lz*, infit, outfit and their jackknife SEs) against the
# references. The references are evaluated at PP's own estimate, so these tests
# check the fit formulas only, not the estimation.

# Reference fit for all persons with a finite theta; f(theta, x, i) returns a
# vector, the result has one row per person (NA rows for theta NA)
ref_fit <- function(theta, X, f) {
  na <- NA * f(0, X[1, ], 1)
  m <- vapply(seq_len(nrow(X)), function(i) {
    if (is.finite(theta[i])) f(theta[i], X[i, ], i) else na
  }, na)
  if (is.matrix(m)) t(m) else matrix(m, ncol = 1)
}

# chisq and df of infit/outfit
ref_chisq <- function(theta, x, items) {
  p <- ref_fit_parts(theta, x, items)
  c(chisq = sum(p$Y^2 / p$V), df = p$n - 1)
}

# theta of the persons with status ok, NA for the others
theta_ok <- function(obj) ifelse(obj$estimates$status == "ok", obj$estimates$estimate, NA)

# All four statistics of a set against the references; mu and s2 are the
# priors of map (one per person)
check_fit <- function(s, X, type, mu = 0, s2 = 1, tol = 1e-5) {
  n <- nrow(X)
  mu <- rep_len(mu, n)
  s2 <- rep_len(s2, n)
  obj <- if (type == "map") pp_fit(s, type, X = X, mu = mu, sigma2 = s2) else pp_fit(s, type, X = X)
  th <- theta_ok(obj)
  it <- s$items
  pf <- pp_quiet(Pfit(pp = obj))
  expect_s3_class(pf, "PPfit")
  expect_named(pf, c("lz", "lzstar", "infit", "outfit"))
  expect_close(pf$lz[, "lz"], ref_fit(th, X, function(t, x, i) ref_lz(t, x, it)), tol)
  expect_close(pf$lzstar[, "lzstar"],
               ref_fit(th, X, function(t, x, i) ref_lzstar(t, x, it, type, mu[i], s2[i])), tol)
  expect_close(pf$infit[, c("infit", "in_t")],
               ref_fit(th, X, function(t, x, i) ref_infit(t, x, it)), tol)
  expect_close(pf$outfit[, c("outfit", "ou_t")],
               ref_fit(th, X, function(t, x, i) ref_outfit(t, x, it)), tol)
  cs <- ref_fit(th, X, function(t, x, i) ref_chisq(t, x, it))
  expect_close(pf$infit[, c("in_chisq", "in_df")], cs, 1e-5 * max(1, cs[, 1], na.rm = TRUE))
  expect_close(pf$outfit[, c("ou_chisq", "ou_df")], cs, 1e-5 * max(1, cs[, 1], na.rm = TRUE))
  expect_close(pf$infit[, "in_pv"], stats::pchisq(cs[, 1], cs[, 2], lower.tail = FALSE), 1e-5)
  # persons without an estimate have no fit
  bad <- obj$estimates$status != "ok"
  for (ix in names(pf)) expect_true(all(is.na(pf[[ix]][bad, ])))
  invisible(pf)
}

for (nm in names(sets_all)) {
  test_that(paste(nm, "lz, lz*, infit and outfit match the references"), {
    for (ty in c("mle", "wle", "map")) check_fit(sets_all[[nm]], sets_all[[nm]]$X, ty)
  })
}

test_that("A9: all fit statistics with an adaptive NA pattern match the references", {
  for (s in list(set_2pl, set_3pl, set_gpcm, set_mixed)) {
    X <- make_na(s$X, 11)
    for (ty in c("mle", "wle", "map")) check_fit(s, X, ty)
  }
})

test_that("A10: PCM fit with a category nobody reached", {
  for (ty in c("mle", "wle")) check_fit(set_pcm_unobs, set_pcm_unobs$X, ty)
})

test_that("A10: GPCM fit uses the slopes", {
  for (ty in c("mle", "wle")) check_fit(set_gpcm, set_gpcm$X, ty)
})

test_that("lz* of map uses the person-specific priors", {
  n <- nrow(set_mixed$X)
  check_fit(set_mixed, set_mixed$X, "map", mu = prior_mu(n), s2 = prior_s2(n))
  check_fit(set_3pl, set_3pl$X, "map", mu = prior_mu(20), s2 = prior_s2(20))
})

test_that("persons without an estimate get NA", {
  obj <- pp_fit(set_2pl, "mle")
  ex <- obj$estimates$status == "extreme"
  expect_true(all(ex[c(4, 9)]))
  pf <- Pfit(pp = obj)
  for (ix in names(pf)) {
    expect_true(all(is.na(pf[[ix]][ex, ])))
    expect_true(all(is.finite(pf[[ix]][!ex, 1])))
  }
  # boundary: the estimate is the bound of range, not a solution
  obj <- suppressWarnings(pp_fit(set_2pl, "map", range = c(-0.5, 0.5)))
  bd <- obj$estimates$status == "boundary"
  expect_true(any(bd))
  pf <- Pfit(pp = obj, fitindices = "lz")
  expect_true(all(is.na(pf$lz[bd, ])) && all(is.finite(pf$lz[!bd, ])))
})

test_that("a person with one answered item has lz but no lz* and no p-value", {
  X <- set_2pl$X[1:3, ]
  X[2, -5] <- NA
  pf <- Pfit(pp = pp_fit(set_2pl, "wle", X = X))
  expect_true(is.finite(pf$lz[2, "lz"]))
  expect_true(is.na(pf$lzstar[2, "lzstar"]))
  expect_identical(unname(pf$infit[2, "in_df"]), 0)
  expect_true(is.na(pf$infit[2, "in_pv"]) && is.na(pf$outfit[2, "ou_pv"]))
  expect_true(all(is.finite(pf$lzstar[-2, ])))
})

# --- jackknife SE ---------------------------------------------------------

ref_all4 <- function(items, type, mu = 0, s2 = 1) {
  function(t, x) c(ref_lz(t, x, items), ref_lzstar(t, x, items, type, mu, s2),
                   ref_infit(t, x, items)[1], ref_outfit(t, x, items)[1])
}
se_cols <- list(lz = "lz_se", lzstar = "lzs_se", infit = "in_se", outfit = "ou_se")

check_fit_se <- function(s, X, type, mu = 0, s2 = 1, tol = 1e-3) {
  n <- nrow(X)
  mu <- rep_len(mu, n)
  s2 <- rep_len(s2, n)
  obj <- if (type == "map") pp_fit(s, type, X = X, mu = mu, sigma2 = s2) else pp_fit(s, type, X = X)
  pf <- pp_quiet(Pfit(pp = obj, SE = TRUE))
  ok <- obj$estimates$status == "ok"
  ref <- t(vapply(seq_len(n), function(i) {
    if (!ok[i]) return(rep(NA_real_, 4))
    ref_fit_se(X[i, ], s$items, type, ref_all4(s$items, type, mu[i], s2[i]), mu[i], s2[i])
  }, numeric(4)))
  got <- vapply(names(se_cols), function(ix) pf[[ix]][, se_cols[[ix]]], numeric(n))
  expect_close(got, ref, tol)
  invisible(pf)
}

test_that("the jackknife SE of all statistics (WLE) matches the reference", {
  check_fit_se(set_2pl, set_2pl$X[1:8, ], "wle")
  check_fit_se(set_mixed, set_mixed$X[c(1, 5, 12, 20), ], "wle")
  check_fit_se(set_gpcm, make_na(set_gpcm$X, 3)[2:6, ], "wle")
})

test_that("A6: the jackknife SE of lz uses person-specific priors", {
  s <- set_2pl
  X <- s$X[1:8, ]
  mu <- prior_mu(8)
  s2 <- prior_s2(8)
  pf <- pp_quiet(Pfit(pp = pp_fit(s, "map", X = X, mu = mu, sigma2 = s2), fitindices = "lz", SE = TRUE))
  ref <- vapply(seq_len(nrow(X)), function(i) ref_lz_se(X[i, ], s$items, "map", mu[i], s2[i]), 0)
  expect_close(pf$lz[, "lz_se"], ref, 1e-3)
  check_fit_se(s, X, "map", mu, s2)
})

test_that("mle leave-one-out samples without a finite estimate are left out of the SE", {
  s <- set_2pl
  rows <- which(vapply(seq_len(nrow(s$X)), function(i) near_extreme(s$X[i, ], s$items), TRUE))
  rows <- setdiff(rows, c(4, 9))[1:3]
  obj <- pp_fit(s, "mle", X = s$X[rows, ])
  expect_warning(Pfit(pp = obj, fitindices = "lz", SE = TRUE), "left out of the jackknife SE")
  suppressWarnings(check_fit_se(s, s$X[rows, ], "mle"))
})

# --- interface ------------------------------------------------------------

test_that("G5: fit statistics are not rounded", {
  s <- set_2pl
  obj <- pp_fit(s, "wle")
  th <- est(obj)[, 1]
  pf <- pp_quiet(Pfit(s$X, obj, fitindices = c("lz", "infit")))
  expect_close(pf$lz[, "lz"], ref_fit(th, s$X, function(t, x, i) ref_lz(t, x, s$items)), 1e-10)
  expect_close(pf$infit[, "infit"],
               ref_fit(th, s$X, function(t, x, i) ref_infit(t, x, s$items)["infit"]), 1e-10)
})

test_that("G6: Pfit works for mixed tests", {
  check_fit(set_mixed, set_mixed$X, "wle")
})

test_that("G6: Pfit takes the responses from the estimation object", {
  obj <- pp_fit(set_2pl, "wle")
  expect_identical(Pfit(pp = obj, fitindices = "infit")$infit,
                   Pfit(set_2pl$X, obj, fitindices = "infit")$infit)
  expect_identical(Pfit(pp = obj)$lz, Pfit(as.data.frame(set_2pl$X), obj)$lz)
  X <- set_2pl$X
  X[3, 2] <- 1L - X[3, 2]
  expect_error(Pfit(X, obj), "not the response matrix")
  expect_error(Pfit(set_2pl$X[-1, ], obj), "not the response matrix")
  X <- set_2pl$X
  X[1, 1] <- NA
  expect_error(Pfit(X, obj), "not the response matrix")
})

test_that("Pfit needs an mle, wle or map estimate", {
  for (ty in c("eap", "robust")) {
    expect_error(Pfit(pp = pp_fit(set_2pl, ty)), "mle', 'wle' or 'map'")
  }
  expect_error(Pfit(pp = pp_fit(set_2pl, "wle"), fitindices = "chi"))
})

test_that("the result keeps the layout of earlier versions", {
  X <- set_2pl$X
  rownames(X) <- paste0("p", seq_len(nrow(X)))
  pf <- Pfit(pp = pp_fit(set_2pl, "wle", X = X), fitindices = c("outfit", "lz"), SE = TRUE)
  expect_named(pf, c("lz", "outfit"))
  expect_identical(colnames(pf$lz), c("lz", "lz_unst", "lz_se"))
  expect_identical(colnames(pf$outfit), c("outfit", "ou_t", "ou_chisq", "ou_df", "ou_pv", "ou_se"))
  expect_null(rownames(pf$lz))
  # PPass binds the statistics with do.call(cbind, .)
  expect_identical(dim(do.call(cbind, pf)), c(nrow(X), 9L))
  pf <- Pfit(pp = pp_fit(set_2pl, "wle"))
  expect_identical(colnames(pf$infit), c("infit", "in_t", "in_chisq", "in_df", "in_pv"))
  expect_identical(colnames(pf$lzstar), "lzstar")
})

test_that("summary() counts the flagged persons", {
  pf <- Pfit(pp = pp_fit(set_mixed, "wle"))
  out <- utils::capture.output(sm <- summary(pf, alpha = 0.1))
  expect_true(any(grepl("persons", out)))
  expect_identical(sm$statistic, c("lz", "lzstar", "infit", "in_t", "outfit", "ou_t"))
  expect_identical(sm$flagged[1], sum(abs(pf$lz[, "lz"]) > stats::qnorm(0.95)))
  expect_true(is.na(sm$flagged[3]))
  expect_identical(sm$n[2], sum(is.finite(pf$lzstar[, 1])))
  expect_error(summary(pf, alpha = 2))
  expect_output(print(pf), "lzstar")
})

test_that("the person fit functions of 0.6.4-1 are removed", {
  ns <- asNamespace("PP")
  for (f in c("lzstar_mixed", "Pfit_lzstarmix", "print_personfit_summary", "lz", "lzstar",
              "Infit", "Outfit", "Infitpoly", "Outfitpoly", "jackknife")) {
    expect_false(exists(f, envir = ns, inherits = FALSE), info = f)
  }
  expect_null(pp_fit(set_2pl, "wle")$ipar)
})
