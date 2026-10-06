# Simulators: frequencies against the model probabilities (ref_catprob) and
# findmodel().

test_that("sim_4pl reproduces the 4PL probabilities", {
  it <- set_4pl$items[c(1, 4, 7, 10)]
  p <- items_to_pp(it)
  theta <- rep(c(-1, 0, 1.5), length.out = 6000)
  set.seed(21)
  X <- sim_4pl(p$b, p$slopes, p$lowerA, p$upperA, theta)
  expect_identical(dim(X), c(6000L, 4L))
  expect_true(all(X %in% 0:1))
  for (j in seq_along(it)) {
    P <- ref_catprob(it[[j]], theta)[, 2]
    z <- (sum(X[, j]) - sum(P)) / sqrt(sum(P * (1 - P)))
    expect_lt(abs(z), 4)
  }
})

test_that("sim_gpcm reproduces the GPCM probabilities", {
  it <- set_gpcm$items[c(1, 4, 6)]
  p <- items_to_pp(it)
  theta <- rep(c(-1, 0.5), length.out = 3000)
  set.seed(22)
  X <- sim_gpcm(p$thres, p$slopes, theta)
  expect_identical(dim(X), c(3000L, 3L))
  for (j in seq_along(it)) {
    P <- ref_catprob(it[[j]], theta)
    expect_true(all(X[, j] %in% (seq_len(ncol(P)) - 1)))
    expected <- colSums(P)
    observed <- tabulate(X[, j] + 1, ncol(P))
    expect_lt(max(abs(observed - expected) / sqrt(expected)), 4)
  }
})

test_that("findmodel tells 4PL from GPCM items by the number of thresholds", {
  expect_identical(findmodel(items_to_pp(set_mixed$items)$thres),
                   items_to_pp(set_mixed$items)$model2est)
})

# --- known bugs -----------------------------------------------------------

test_that("A6: sim_4pl recycles scalar item parameters", {
  skip_known("A6", 3)
  X <- suppressWarnings(sim_4pl(beta = rep(0, 6), alpha = rep(1, 6), lowerA = 0, upperA = 1,
                                theta = rep(30, 3)))
  expect_identical(dim(X), c(3L, 6L))
  expect_true(all(X == 1))
})

test_that("C2: sim_gpcm works for extreme abilities", {
  skip_known("C2", 3)
  X <- sim_gpcm(matrix(c(0, -1, 0, 1, 2), ncol = 1), 3, rep(100, 5))
  expect_true(all(X == 4))
})

test_that("F6: sim_gpcm with one item returns persons x 1", {
  skip_known("F6", 3)
  set.seed(23)
  expect_identical(dim(sim_gpcm(matrix(c(0, -1, 0, 1), ncol = 1), 1, rnorm(5))), c(5L, 1L))
})
