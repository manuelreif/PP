# The C++ person estimator pp_estimate_cpp() (src/core.cpp) directly, without
# the R wrappers: estimates, SEs and status against the plain-R references
# (helper-2-reference.R), the safeguarded Newton and the item mask.

status_levels <- c("ok", "extreme", "boundary", "no_response", "not_converged")

# Estimator call for a set; the default range is the one of the wrappers
core_fit <- function(set, type, X = set$X, mu = 0, s2 = 1, H = 1, start = NA,
                     range = c(-6, 6), exac = 1e-6, maxsteps = 100, skip = 0L) {
  n <- nrow(X)
  r <- PP:::pp_estimate_cpp(kernel_bank(set$items), X, type, rep_len(mu, n), rep_len(s2, n),
                            H, rep_len(as.numeric(start), n), range, exac, maxsteps, skip)
  r$status <- status_levels[r$status]
  r
}

types <- c("mle", "wle", "map", "robust")

test_that("estimates, SEs and status of all sets match the reference", {
  for (nm in names(sets_all)) {
    s <- sets_all[[nm]]
    for (ty in types) {
      r <- core_fit(s, ty)
      ref <- ref_estimates(s$X, s$items, ty, range = c(-6, 6))
      expect_close(r$theta, ref[, 1], 1e-4)
      expect_close(r$se, ref[, 2], 1e-4)
      ext <- apply(s$X, 1, ref_is_extreme, items = s$items) != 0
      expected <- ifelse(abs(ref[, 1]) %in% 6, "boundary",
                         ifelse(ty %in% c("mle", "robust") & ext, "extreme", "ok"))
      expect_identical(r$status, expected, info = paste(nm, ty))
    }
  }
})

test_that("estimates with missing responses and person-specific priors match the reference", {
  for (nm in c("2PL", "3PL", "GPCM", "mixed")) {
    s <- sets_all[[nm]]
    X <- make_na(s$X, 7)
    n <- nrow(X)
    # robust 3PL has several roots for some of these persons
    for (ty in if (nm == "3PL") c("mle", "wle", "map") else types) {
      r <- core_fit(s, ty, X = X)
      expect_close(r$theta, ref_estimates(X, s$items, ty, range = c(-6, 6))[, 1], 1e-4)
    }
    r <- core_fit(s, "map", mu = prior_mu(n), s2 = prior_s2(n))
    ref <- ref_estimates(s$X, s$items, "map", prior_mu(n), prior_s2(n), range = c(-6, 6))
    expect_close(r$theta, ref[, 1], 1e-4)
    expect_close(r$se, ref[, 2], 1e-4)
  }
})

test_that("the MAP SE includes the prior and the robust SE is the sandwich", {
  for (s in list(set_2pl, set_3pl, set_gpcm)) {
    ref <- ref_estimates(s$X, s$items, "map", 0, 0.5, range = c(-6, 6))
    expect_close(core_fit(s, "map", s2 = 0.5)$se, ref[, 2], 1e-4)
    # with H < 1 the robust equation of the 2PL/3PL sets has several roots for
    # some persons; the reference needs a unique root
    if (identical(s, set_3pl)) next
    for (H in c(1, 2, 3)) {
      ref <- ref_estimates(s$X, s$items, "robust", H = H, range = c(-6, 6))
      r <- core_fit(s, "robust", H = H)
      expect_close(r$theta, ref[, 1], 1e-4)
      expect_close(r$se, ref[, 2], 1e-4)
    }
  }
})

test_that("Huber weights match the reference", {
  items <- c(set_mixed$items, set_gpcm12$items)
  theta <- seq(-4, 4, by = 0.25)
  for (H in c(0.5, 1, 3)) {
    w <- PP:::pp_huber_cpp(kernel_bank(items), theta, H)
    ref <- vapply(items, ref_huber, numeric(length(theta)), theta = theta, H = H)
    expect_close(w, ref, 1e-14)
  }
})

test_that("the oscillating 3PL persons converge independently of maxsteps", {
  for (ty in c("mle", "wle")) {
    r <- lapply(c(40, 41, 100), function(m) core_fit(osc3pl, ty, exac = 0.001, maxsteps = m))
    expect_true(all(r[[3]]$status %in% c("ok", "boundary")))
    expect_identical(r[[1]]$theta, r[[3]]$theta)
    expect_identical(r[[2]]$theta, r[[3]]$theta)
    ref <- ref_estimates(osc3pl$X, osc3pl$items, ty, range = c(-6, 6))
    expect_close(r[[3]]$theta, ref[, 1], 2e-3)
  }
})

test_that("the number of iterations stays below the bound of the safeguard", {
  # rtsafe halves the step at least every second iteration
  for (exac in c(1e-3, 1e-8)) {
    bound <- 2 * log2(12 / exac) + 5
    for (s in c(sets_all, list(osc3pl))) for (ty in types) {
      expect_lte(max(core_fit(s, ty, exac = exac)$nsteps), bound)
    }
  }
})

test_that("not converged within maxsteps is reported", {
  r <- core_fit(set_3pl, "wle", exac = 1e-12, maxsteps = 2)
  expect_true(any(r$status == "not_converged"))
  expect_true(all(r$nsteps <= 2))
})

test_that("persons without answers get NA, extreme scores +-Inf or a finite value", {
  for (s in list(set_2pl, set_gpcm, set_mixed)) {
    X <- s$X
    X[3, ] <- NA
    ext <- apply(X, 1, function(x) if (all(is.na(x))) 0 else ref_is_extreme(x, s$items))
    for (ty in types) {
      r <- core_fit(s, ty, X = X)
      expect_true(is.na(r$theta[3]) && is.na(r$se[3]))
      expect_identical(r$status[3], "no_response")
      if (ty %in% c("mle", "robust")) {
        expect_identical(r$theta[ext != 0], ext[ext != 0] * Inf)
        expect_true(all(is.na(r$se[ext != 0])))
      } else {
        expect_true(all(is.finite(r$theta[-3])))
      }
    }
  }
})

test_that("an extreme item (b = +-40, slope 20) gives no NaN", {
  for (ty in types) {
    r <- core_fit(set_extreme, ty)
    expect_false(anyNA(r$theta))
    expect_false(anyNA(r$se))
  }
  ref <- ref_estimates(set_extreme$X, set_extreme$items, "map", range = c(-6, 6))
  expect_close(core_fit(set_extreme, "map")$theta, ref[, 1], 1e-4)
})

test_that("skip_item gives the estimate without that item", {
  for (s in list(set_3pl, set_gpcm, set_mixed)) {
    for (j in c(1, ncol(s$X))) {
      for (ty in c("wle", "map")) {
        a <- core_fit(s, ty, skip = j)
        b <- core_fit(list(items = s$items[-j]), ty, X = s$X[, -j, drop = FALSE])
        expect_identical(a, b)
      }
    }
  }
})

test_that("the start value changes the result only within exac", {
  for (s in list(set_3pl, set_gpcm)) {
    for (ty in types) {
      a <- core_fit(s, ty, exac = 1e-4)
      for (st in c(-5, 0, 5)) {
        expect_close(core_fit(s, ty, exac = 1e-4, start = st)$theta, a$theta, 1e-3)
      }
    }
  }
})

test_that("invalid input is rejected", {
  bank <- kernel_bank(set_2pl$items)
  X <- set_2pl$X
  n <- nrow(X)
  call <- function(...) {
    a <- modifyList(list(bank = bank, X = X, method = "wle", mu = rep(0, n), sigma2 = rep(1, n),
                         H = 1, start = rep(NA_real_, n), range = c(-6, 6), exac = 1e-3,
                         maxsteps = 100L), list(...))
    do.call(PP:::pp_estimate_cpp, a)
  }
  expect_error(call(method = "eap"), "unknown method")
  expect_error(call(X = X[, -1]), "one column per item")
  expect_error(call(mu = 0), "one entry per person")
  expect_error(call(range = c(1, -1)), "lo < hi")
  X2 <- X
  X2[1, 1] <- 2L
  expect_error(call(X = X2), "not a category")
})
