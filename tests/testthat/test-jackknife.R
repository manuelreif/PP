# JKpp against the textbook jackknife over the answered items
# (ref_jackknife in helper-2-reference.R).

# Reference for a response matrix: JK estimate and SE (n x 2) and the
# leave-one-out estimates (n x items)
ref_jk <- function(items, X, type, cmeth = "mean") {
  r <- lapply(seq_len(nrow(X)), function(i) ref_jackknife(X[i, ], items, type, cmeth = cmeth))
  list(est = cbind(vapply(r, `[[`, 0, "estimate"), vapply(r, `[[`, 0, "se")),
       loo = t(vapply(r, `[[`, numeric(ncol(X)), "loo")))
}

jk_quiet <- function(obj, ...) pp_quiet(JKpp(obj, fullmat = TRUE, ...))

jk_cases <- list(
  list(set = set_2pl, types = c("mle", "wle", "map", "eap")),
  list(set = set_gpcm, types = c("mle", "wle", "map", "eap")),
  list(set = set_mixed, types = c("mle", "wle", "map")))

test_that("leave-one-out estimates and the JK estimate match the reference", {
  for (cs in jk_cases) {
    s <- cs$set
    for (ty in cs$types) {
      X <- s$X[1:10, ]
      ref <- ref_jk(s$items, X, ty)
      # Left out: persons with an infinite or boundary leave-one-out estimate
      # (extreme score, A11; no finite mle, A8). PP is then called for the
      # remaining persons only, because JKpp drops extreme persons itself (A5).
      keep <- apply(abs(ref$loo) < 10, 1, all)
      X <- X[keep, ]
      ref <- lapply(ref, function(r) r[keep, , drop = FALSE])
      jk <- jk_quiet(pp_fit(s, ty, X = X))
      expect_close(jk$jk_mat, ref$loo, 1e-3)
      # pseudovalues amplify the estimation error by about the number of items
      expect_close(jk$resjk[, 1], ref$est[, 1], 2e-2)
    }
  }
})

test_that("cmeth = 'median' uses the median of the pseudovalues", {
  s <- set_2pl
  X <- s$X[1:8, ]
  jk <- jk_quiet(pp_fit(s, "wle", X = X), cmeth = "median")
  expect_close(jk$resjk[, 1], ref_jk(s$items, X, "wle", cmeth = "median")$est[, 1], 2e-2)
})

# --- known bugs -----------------------------------------------------------

test_that("A1: the JK standard error matches the textbook formula", {
  skip_known("A1", 6)
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
  skip_known("A3", 6)
  s <- set_mixed
  X <- s$X[1:6, ]
  jk <- jk_quiet(pp_fit(s, "eap", X = X))
  ref <- ref_jk(s$items, X, "eap")
  expect_close(jk$jk_mat, ref$loo, 1e-3)
  expect_close(jk$resjk[, 1], ref$est[, 1], 2e-2)
})

test_that("A11: an extreme leave-one-out sample with mle is not dropped silently", {
  skip_known("A11", 6)
  # person 1 has one wrong answer: without that item the mle does not exist
  s <- set_2pl
  X <- rbind(c(rep(1L, 11), 0L), s$X[1, ])
  expect_warning(jk_quiet(pp_fit(s, "mle", X = X)))
})

test_that("A13: the jackknife runs over the answered items only", {
  skip_known("A13", 6)
  s <- set_2pl
  X <- make_na(s$X[1:8, ], 10)
  jk <- jk_quiet(pp_fit(s, "wle", X = X))
  ref <- ref_jk(s$items, X, "wle")
  expect_close(jk$jk_mat, ref$loo, 1e-3)
  expect_close(jk$resjk[, 1], ref$est[, 1], 2e-2)
})
