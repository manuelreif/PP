# JKpp against the textbook jackknife over the answered items
# (ref_jackknife in helper-2-reference.R). Estimates and jackknife run with
# exac = 1e-8, so the tolerances can be tight.

# Reference for a response matrix: JK estimate and SE (n x 2), the
# leave-one-out estimates PP should report (n x items: NA for excluded samples
# and for persons without a jackknife) and the number of excluded samples
ref_jk <- function(items, X, type, cmeth = "mean", ...) {
  r <- lapply(seq_len(nrow(X)), function(i) ref_jackknife(X[i, ], items, type, cmeth = cmeth, ...))
  est <- cbind(vapply(r, `[[`, 0, "estimate"), vapply(r, `[[`, 0, "se"))
  loo <- t(vapply(r, `[[`, numeric(ncol(X)), "loo"))
  loo[!(is.finite(loo) & abs(loo) < 6)] <- NA
  theta <- vapply(r, `[[`, 0, "theta")
  loo[!(is.finite(theta) & abs(theta) < 6), ] <- NA
  list(est = est, loo = loo, excluded = vapply(r, `[[`, 0, "excluded"))
}

fit8 <- function(set, type, X, ...) pp_fit(set, type, X = X, exac = 1e-8, ...)
jk_quiet <- function(obj, ...) pp_quiet(suppressWarnings(JKpp(obj, fullmat = TRUE, ...)))

jk_cases <- list(
  list(set = set_2pl, types = c("mle", "wle", "map", "eap", "robust")),
  list(set = set_gpcm, types = c("mle", "wle", "map", "eap")),
  list(set = set_mixed, types = c("mle", "wle", "map", "eap")))

test_that("leave-one-out estimates, JK estimate and SE match the reference", {
  for (cs in jk_cases) {
    s <- cs$set
    for (ty in cs$types) {
      X <- s$X[1:10, ]
      ref <- ref_jk(s$items, X, ty)
      jk <- jk_quiet(fit8(s, ty, X))
      expect_close(unname(jk$jk_mat), ref$loo, 1e-6)
      # pseudovalues amplify the error of a leave-one-out estimate by L - 1
      expect_close(unname(jk$resjk), ref$est, 1e-4)
      expect_identical(jk$estimates$excluded, as.integer(ref$excluded))
      expect_identical(jk$estimates$L, rep(ncol(X), nrow(X)))
    }
  }
})

test_that("the 3PL persons with an oscillating Newton get the reference jackknife", {
  s <- osc3pl
  X <- s$X[1:4, ]
  jk <- jk_quiet(fit8(s, "wle", X))
  ref <- ref_jk(s$items, X, "wle")
  expect_close(unname(jk$jk_mat), ref$loo, 1e-6)
  expect_close(unname(jk$resjk), ref$est, 1e-4)
})

test_that("cmeth = 'median' and 'AMT' match the reference", {
  for (s in list(set_2pl, set_gpcm, set_mixed)) {
    X <- s$X[1:8, ]
    o <- fit8(s, "wle", X)
    for (cm in c("median", "AMT")) {
      jk <- jk_quiet(o, cmeth = cm)
      expect_identical(jk$cmeth, cm)
      expect_close(unname(jk$resjk), ref_jk(s$items, X, "wle", cmeth = cm)$est, 1e-4)
    }
  }
  expect_error(JKpp(o, cmeth = "mode"), "should be one of")
})

test_that("persons without a jackknife get NA and a status", {
  s <- set_2pl
  X <- rbind(s$X[1:2, ], 0L, NA, c(1L, rep(NA, ncol(s$X) - 1)))
  rownames(X) <- paste0("p", 1:5)
  jk <- jk_quiet(fit8(s, "mle", X))
  expect_identical(as.character(jk$estimates$status),
                   c("ok", "ok", "extreme", "no_response", "extreme"))
  expect_identical(rownames(jk$estimates), as.character(1:5))
  expect_null(rownames(jk$resjk))
  expect_true(all(is.na(jk$resjk[3:5, ])))
  # wle: one answered item has an estimate but no jackknife
  jw <- jk_quiet(fit8(s, "wle", X))
  expect_identical(as.character(jw$estimates$status),
                   c("ok", "ok", "ok", "no_response", "too_few_items"))
  expect_identical(jw$estimates$L, c(12L, 12L, 12L, 0L, 1L))
  expect_true(all(is.na(jw$resjk[4:5, ])))
  expect_false(anyNA(jw$resjk[1:3, ]))
  expect_s3_class(jw$estimates$status, "factor")
})

test_that("summary and print of a jk object run", {
  s <- set_2pl
  jk <- jk_quiet(fit8(s, "wle", s$X[1:5, ]))
  out <- capture.output(summary(jk))
  expect_true(any(grepl("centering method: mean", out)))
  expect_output(print(jk))
  expect_null(JKpp(pp_fit(s, "wle", X = s$X[1:5, ]))$jk_mat)
})

test_that("maxsteps and exac come from the estimate unless given", {
  s <- set_2pl
  o <- fit8(s, "wle", s$X[1:5, ])
  a <- JKpp(o, fullmat = TRUE)
  b <- JKpp(o, fullmat = TRUE, exac = 1e-8, maxsteps = 100)
  expect_identical(a$jk_mat, b$jk_mat)
  expect_error(JKpp(o, maxsteps = 0), "maxsteps")
  expect_error(JKpp(o, exac = -1), "exac")
})

test_that("B3: AMT combines right and wrong answers separately (Wainer & Wright, scheme 4)", {
  s <- set_2pl
  X <- s$X[1:8, ]
  o <- fit8(s, "wle", X)
  jk <- jk_quiet(o, cmeth = "AMT")
  theta <- o$estimates$estimate
  L <- rowSums(!is.na(X))
  for (i in which(jk$estimates$status == "ok")) {
    obs <- !is.na(X[i, ])
    ps <- (L[i] * theta[i] - (L[i] - 1) * jk$jk_mat[i, ])[obs]
    right <- X[i, obs] == 1
    part <- function(p) if (length(p)) length(p) * ref_amt(p) else 0
    expected <- (part(ps[right]) + part(ps[!right])) / L[i]
    expect_close(jk$estimates$estimate[i], expected, 1e-6)
  }
  # the pooled AMT of 0.7.0-dev differs
  pooled <- vapply(1:8, function(i) {
    PP:::jk_amt(stats::na.omit(L[i] * theta[i] - (L[i] - 1) * jk$jk_mat[i, ]))
  }, 0)
  expect_gt(max(abs(pooled - jk$estimates$estimate)), 1e-3)
})

test_that("jk_amt: median for symmetric data, robust against an outlier", {
  amt <- PP:::jk_amt
  x <- c(-2.1, -1, -0.4, 0.3, 0.9, 1.6, 2.7)
  xs <- c(x, 2 * stats::median(x) - x)
  expect_close(amt(xs), stats::median(xs), 1e-10)
  y <- c(-1.2, -0.5, -0.3, 0.1, 0.2, 0.6, 1.1, 1.4)
  expect_close(amt(y), ref_amt(y), 1e-6)
  expect_close(amt(c(y, 40)), amt(c(y, 400)), 1e-12)
  expect_close(amt(c(y, 40)), ref_amt(c(y, 40)), 1e-6)
  expect_identical(amt(c(1, 1, 1, 5)), 1)
  expect_identical(amt(numeric(0)), NA_real_)
})

# --- known bugs -----------------------------------------------------------

test_that("A1: the JK standard error matches the textbook formula", {
  for (cs in jk_cases) {
    s <- cs$set
    X <- s$X[1:8, ]
    jk <- jk_quiet(pp_fit(s, "wle", X = X))
    expect_close(jk$resjk[, 2], ref_jk(s$items, X, "wle")$est[, 2], 2e-2)
  }
})

test_that("A2: killdupli with a duplicate in front gives the same jackknife", {
  s <- set_2pl
  X <- rbind(s$X[1, ], s$X[1:8, ])
  a <- jk_quiet(pp_fit(s, "wle", X = X, ctrl = list(killdupli = TRUE)))
  b <- jk_quiet(pp_fit(s, "wle", X = X))
  expect_close(a$resjk, b$resjk, 1e-10)
})

test_that("A3: the jackknife of a mixed EAP object works", {
  s <- set_mixed
  X <- s$X[1:6, ]
  jk <- jk_quiet(pp_fit(s, "eap", X = X))
  ref <- ref_jk(s$items, X, "eap")
  expect_close(jk$jk_mat, ref$loo, 1e-3)
  expect_close(jk$resjk[, 1], ref$est[, 1], 2e-2)
})

test_that("A11: an extreme leave-one-out sample with mle is not dropped silently", {
  # person 1 has one wrong answer: without that item the mle does not exist
  s <- set_2pl
  X <- rbind(c(rep(1L, 11), 0L), s$X[1, ])
  o <- pp_fit(s, "mle", X = X)
  expect_warning(jk <- pp_quiet(JKpp(o, fullmat = TRUE)), "left out of the jackknife")
  expect_identical(jk$estimates$excluded, c(1L, 0L))
  expect_true(is.na(jk$jk_mat[1, 12]))
  L <- 12
  ps <- L * o$estimates$estimate[1] - (L - 1) * jk$jk_mat[1, 1:11]
  expect_close(jk$resjk[1, ], c(estimate = mean(ps), SE = sqrt(sum((ps - mean(ps))^2) / (11 * 10))),
               1e-12)
})

test_that("A13: the jackknife runs over the answered items only", {
  s <- set_2pl
  X <- make_na(s$X[1:8, ], 10)
  jk <- jk_quiet(pp_fit(s, "wle", X = X))
  ref <- ref_jk(s$items, X, "wle")
  expect_close(jk$jk_mat, ref$loo, 1e-3)
  expect_close(jk$resjk[, 1], ref$est[, 1], 2e-2)
})
