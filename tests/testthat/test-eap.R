# EAP and posterior SD against the posterior on a fine grid
# (helper-2-reference.R, 4001 nodes). test-posterior.R tests the grid itself.

for (nm in names(sets_all)) {
  test_that(paste(nm, "EAP and PSD match the posterior"), {
    s <- sets_all[[nm]]
    expect_close(est(pp_fit(s, "eap")), ref_eap(s$X, s$items), 1e-6)
  })
}

test_that("EAP with an adaptive NA pattern matches the posterior", {
  for (s in list(set_2pl, set_4pl, set_gpcm, set_mixed)) {
    Xna <- make_na(s$X, 8)
    expect_close(est(pp_fit(s, "eap", X = Xna)), ref_eap(Xna, s$items), 1e-6)
  }
})

test_that("EAP uses person-specific priors", {
  for (s in list(set_3pl, set_pcm, set_mixed)) {
    n <- nrow(s$X)
    expect_close(est(pp_fit(s, "eap", mu = prior_mu(n), sigma2 = prior_s2(n))),
                 ref_eap(s$X, s$items, prior_mu(n), prior_s2(n)), 1e-6)
  }
})

test_that("a person without answers gets the prior mean and SD", {
  s <- set_2pl
  X <- s$X[1:4, ]
  X[3, ] <- NA
  e <- pp_fit(s, "eap", X = X, mu = 0.5, sigma2 = 2)$estimates
  expect_close(c(e$estimate[3], e$SE[3]), c(0.5, sqrt(2)), 1e-6)
  expect_identical(as.character(e$status), c("ok", "ok", "no_response", "ok"))
})

test_that("EAP of an extreme item matches the posterior", {
  expect_close(est(pp_fit(set_extreme, "eap")), ref_eap(set_extreme$X, set_extreme$items), 1e-6)
})

# --- known bugs -----------------------------------------------------------

test_that("C3: EAP of a 1100-item test is finite and matches the posterior", {
  res <- est(pp_fit(set_long, "eap"))
  expect_false(anyNA(res))
  expect_close(res, ref_eap(set_long$X, set_long$items), 1e-6)
})
