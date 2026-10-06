# Input checks and defaults. Today most of these inputs are accepted silently
# or fail with an unrelated message; the tests describe the behaviour after
# block 2 (one central input layer).

p2 <- items_to_pp(set_2pl$items)
pm <- items_to_pp(set_mixed$items)
X2 <- set_2pl$X

test_that("misspelled type and wrong lengths are errors", {
  expect_error(pp_quiet(PP_4pl(X2, p2$b, slopes = p2$slopes, type = "wlee")))
  expect_error(pp_quiet(PP_4pl(X2, p2$b[-1], slopes = p2$slopes)))
  expect_error(pp_quiet(PP_gpcm(set_gpcm$X, items_to_pp(set_gpcm$items)$thres, slopes = 1:3)))
})

# --- known bugs -----------------------------------------------------------

test_that("F2: non-integer responses are an error", {
  skip_known("F2", 2)
  expect_error(pp_quiet(PP_4pl(X2 * 0.6, p2$b, slopes = p2$slopes)))
})

test_that("F2: negative responses are an error", {
  skip_known("F2", 2)
  X <- X2
  X[1, 1] <- -1L
  expect_error(pp_quiet(PP_4pl(X, p2$b, slopes = p2$slopes)))
})

test_that("F2: invalid item parameters are an error", {
  skip_known("F2", 2)
  expect_error(pp_quiet(PP_4pl(X2, p2$b, slopes = replace(p2$slopes, 1, -1))))
  expect_error(pp_quiet(PP_4pl(X2, p2$b, slopes = replace(p2$slopes, 2, NA))))
  expect_error(pp_quiet(PP_4pl(X2, p2$b, slopes = p2$slopes,
                               lowerA = rep(0.3, 12), upperA = rep(0.3, 12))))
})

test_that("F2: a non-positive prior variance is an error", {
  skip_known("F2", 2)
  n <- nrow(X2)
  expect_error(pp_quiet(PP_4pl(X2, p2$b, slopes = p2$slopes, type = "map",
                               mu = rep(0, n), sigma2 = rep(-1, n))))
})

test_that("F2: a data.frame of responses is accepted", {
  skip_known("F2", 2)
  expect_identical(est(pp_quiet(PP_4pl(as.data.frame(X2), p2$b, slopes = p2$slopes))),
                   est(pp_quiet(PP_4pl(X2, p2$b, slopes = p2$slopes))))
})

test_that("F3: a misspelled type is reported even with skipcheck", {
  skip_known("F3", 2)
  expect_error(pp_quiet(PP_4pl(X2, p2$b, slopes = p2$slopes, type = "wlee",
                               ctrl = list(skipcheck = TRUE))), "type")
})

test_that("F4: a one-row thres matrix of zeros holds difficulties", {
  skip_known("F4", 2)
  expect_identical(est(pp_quiet(PP_4pl(X2[, 1:3], matrix(0, 1, 3)))),
                   est(pp_quiet(PP_4pl(X2[, 1:3], c(0, 0, 0)))))
})

test_that("F4: NA in the first row of thres gives an error about thres", {
  skip_known("F4", 2)
  expect_error(pp_quiet(PP_4pl(X2, rbind(NA, p2$b), slopes = p2$slopes)), "thres")
})

test_that("F4: PP_gpcm accepts a vector of thresholds", {
  skip_known("F4", 2)
  expect_identical(est(pp_quiet(PP_gpcm(X2, p2$b, slopes = p2$slopes))),
                   est(pp_quiet(PP_gpcm(X2, rbind(0, p2$b), slopes = p2$slopes))))
})

test_that("F1: PPall works when only one person is left after removing extremes", {
  skip_known("F1", 2)
  X <- rbind(rep(0L, 12), set_mixed$X[1, ], c(rep(1L, 8), 2L, 3L, 3L, 4L))
  res <- est(pp_quiet(PPall(X, pm$thres, pm$slopes, pm$lowerA, pm$upperA,
                            model2est = pm$model2est, type = "mle")))
  expect_close(res[2, 1], ref_estimate(X[2, ], set_mixed$items, "mle"), 1e-3)
})

test_that("F1: PPall accepts thres as a vector", {
  skip_known("F1", 2)
  m2e <- rep("4PL", 12)
  expect_identical(
    est(pp_quiet(PPall(X2, p2$b, p2$slopes, rep(0, 12), rep(1, 12), model2est = m2e))),
    est(pp_quiet(PPall(X2, rbind(0, p2$b), p2$slopes, rep(0, 12), rep(1, 12), model2est = m2e))))
})

test_that("F1: model2est is checked against thres", {
  skip_known("F1", 2)
  # a "4PL" item with 5 categories
  expect_error(pp_quiet(PPall(set_mixed$X, pm$thres, pm$slopes, pm$lowerA, pm$upperA,
                              model2est = rep("4PL", 12))))
  # one entry too many, and invalid
  expect_error(pp_quiet(PPall(set_mixed$X, pm$thres, pm$slopes, pm$lowerA, pm$upperA,
                              model2est = c(pm$model2est, "gpcm"))))
})

test_that("G3: PP_gpcm without slopes is the PCM", {
  skip_known("G3", 2)
  thr <- items_to_pp(set_pcm$items)$thres
  expect_identical(est(pp_quiet(PP_gpcm(set_pcm$X, thr))),
                   est(pp_quiet(PP_gpcm(set_pcm$X, thr, slopes = rep(1, 6)))))
})

test_that("G3: PPall has defaults for lowerA, upperA and model2est", {
  skip_known("G3", 2)
  full <- est(pp_quiet(PPall(set_mixed$X, pm$thres, pm$slopes, rep(0, 12), rep(1, 12),
                             model2est = pm$model2est)))
  expect_identical(est(pp_quiet(PPall(set_mixed$X, pm$thres, pm$slopes,
                                      model2est = pm$model2est))), full)
  expect_identical(est(pp_quiet(PPall(set_mixed$X, pm$thres, pm$slopes,
                                      rep(0, 12), rep(1, 12)))), full)
})

test_that("G3: PP_4pl with lowerA but without slopes uses slopes = 1", {
  skip_known("G3", 2)
  expect_identical(est(pp_quiet(PP_4pl(X2, p2$b, lowerA = rep(0.1, 12)))),
                   est(pp_quiet(PP_4pl(X2, p2$b, slopes = rep(1, 12), lowerA = rep(0.1, 12)))))
})
