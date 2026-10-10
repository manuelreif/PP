# Simulators: frequencies against the model probabilities (ref_catprob), shape,
# recycling, extreme abilities and input checks; findmodel().

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

# Chi-square goodness of fit per item, within groups of equal theta; cells
# with an expected count below 5 are pooled within their group
sim_chisq <- function(x, item, theta) {
  stat <- 0
  df <- 0
  for (th in unique(theta)) {
    sel <- theta == th
    e <- sum(sel) * ref_catprob(item, th)[1, ]
    o <- tabulate(x[sel] + 1, length(e))
    small <- e < 5
    if (any(small)) {
      e <- c(e[!small], sum(e[small]))
      o <- c(o[!small], sum(o[small]))
    }
    stat <- stat + sum((o - e)^2 / e)
    df <- df + length(e) - 1
  }
  stats::pchisq(stat, df, lower.tail = FALSE)
}

test_that("the simulator fits the category probabilities of every model", {
  items <- c(set_4pl$items[c(2, 9)], set_3pl$items[5], set_1pl$items[3],
             set_mixed$items[9:12], set_gpcm12$items)
  p <- items_to_pp(items)
  bank <- PP:::prepare_items(PP:::normalize_thres(p$thres), p$slopes, p$lowerA, p$upperA,
                             p$model2est, "mixed")$bank
  theta <- rep(c(-2.5, -1, 0, 0.8, 2), each = 4000)
  set.seed(24)
  X <- PP:::pp_sim_cpp(bank, theta)
  expect_identical(dim(X), c(length(theta), length(items)))
  for (j in seq_along(items)) {
    expect_true(all(X[, j] %in% (seq_len(ref_ncat(items[[j]])) - 1)))
    expect_gt(sim_chisq(X[, j], items[[j]], theta), 1e-4)
  }
})

test_that("responses are independent given theta", {
  p <- items_to_pp(set_mixed$items)
  set.seed(25)
  X <- PP:::pp_sim_cpp(
    PP:::prepare_items(PP:::normalize_thres(p$thres), p$slopes, p$lowerA, p$upperA,
                       p$model2est, "mixed")$bank,
    rep(0.3, 20000))
  r <- cor(X)
  expect_lt(max(abs(r[upper.tri(r)])), 4.5 / sqrt(nrow(X)))
})

test_that("the simulators return persons x items for one item or one person", {
  set.seed(26)
  expect_identical(dim(sim_4pl(0, 1, 0, 1, rnorm(5))), c(5L, 1L))
  expect_identical(dim(sim_4pl(c(-1, 0, 1, 2), 1, 0, 1, 0.1)), c(1L, 4L))
  thres <- items_to_pp(set_gpcm$items[1:3])$thres
  expect_identical(dim(sim_gpcm(thres, 1, 0.2)), c(1L, 3L))
})

test_that("scalar item parameters give the same data as vectors", {
  b <- c(-1, 0, 0.5, 2)
  th <- rnorm(50)
  set.seed(27)
  s <- sim_4pl(b, 1.2, 0.1, 0.95, th)
  set.seed(27)
  v <- sim_4pl(b, rep(1.2, 4), rep(0.1, 4), rep(0.95, 4), th)
  expect_identical(s, v)
  thres <- items_to_pp(set_gpcm$items)$thres
  set.seed(28)
  s <- sim_gpcm(thres, 0.8, th)
  set.seed(28)
  v <- sim_gpcm(thres, rep(0.8, ncol(thres)), th)
  expect_identical(s, v)
})

test_that("the zero row of thres is optional in sim_gpcm", {
  thres <- items_to_pp(set_gpcm$items)$thres
  th <- seq(-2, 2, length.out = 30)
  set.seed(29)
  a <- sim_gpcm(thres, 1, th)
  set.seed(29)
  b <- sim_gpcm(thres[-1, ], 1, th)
  expect_identical(a, b)
})

test_that("the simulation is reproducible with set.seed", {
  p <- items_to_pp(set_4pl$items)
  set.seed(30)
  a <- sim_4pl(p$b, p$slopes, p$lowerA, p$upperA, rep(0, 40))
  set.seed(30)
  b <- sim_4pl(p$b, p$slopes, p$lowerA, p$upperA, rep(0, 40))
  expect_identical(a, b)
})

test_that("NA in theta gives an NA row and uses no random numbers", {
  thres <- items_to_pp(set_gpcm$items)$thres
  set.seed(31)
  a <- sim_gpcm(thres, 1, c(0, NA, 1))
  set.seed(31)
  b <- sim_gpcm(thres, 1, c(0, 1))
  expect_true(all(is.na(a[2, ])))
  expect_identical(a[-2, ], b)
})

test_that("invalid simulator arguments are rejected", {
  expect_error(sim_4pl(rep(0, 3), c(1, 1), 0, 1, 0), "'alpha' must have length 1 or 3")
  expect_error(sim_4pl(rep(0, 3), -1, 0, 1, 0), "'alpha' must be positive")
  expect_error(sim_4pl(rep(0, 3), 1, 0.5, 0.4, 0), "lowerA < upperA")
  expect_error(sim_4pl(c(0, NA), 1, 0, 1, 0), "'beta'")
  expect_error(sim_4pl(0, 1, 0, 1, Inf), "'theta' must be finite")
  expect_error(sim_gpcm(matrix(c(0, -1, 0, 1), ncol = 1), 0, 1), "'alpha' must be positive")
  expect_error(sim_gpcm(matrix(c(0, -1, 0, 1), ncol = 1), 1, "a"), "'theta' must be numeric")
})

test_that("findmodel tells 4PL from GPCM items by the number of thresholds", {
  expect_identical(findmodel(items_to_pp(set_mixed$items)$thres),
                   items_to_pp(set_mixed$items)$model2est)
})

# --- known bugs (fixed in block 3) ------------------------------------------

test_that("A6: sim_4pl recycles scalar item parameters", {
  X <- suppressWarnings(sim_4pl(beta = rep(0, 6), alpha = rep(1, 6), lowerA = 0, upperA = 1,
                                theta = rep(30, 3)))
  expect_identical(dim(X), c(3L, 6L))
  expect_true(all(X == 1))
})

test_that("C2: sim_gpcm works for extreme abilities", {
  X <- sim_gpcm(matrix(c(0, -1, 0, 1, 2), ncol = 1), 3, rep(100, 5))
  expect_true(all(X == 4))
  expect_true(all(sim_4pl(c(-1, 0, 1), 2, 0, 1, rep(-1000, 4)) == 0))
})

test_that("F6: sim_gpcm with one item returns persons x 1", {
  set.seed(23)
  expect_identical(dim(sim_gpcm(matrix(c(0, -1, 0, 1), ncol = 1), 1, rnorm(5))), c(5L, 1L))
})
