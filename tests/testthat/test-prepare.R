# The input layer: normalize_thres(), pp_prepare() and the item bank.

prep <- function(...) pp_quiet(PP:::pp_prepare(...))
nthr <- PP:::normalize_thres

# --- thres ------------------------------------------------------------------

test_that("normalize_thres: vector, with and without zero row", {
  b <- c(-1, 0, 1)
  expect_identical(nthr(b)$thres, rbind(0, b, deparse.level = 0))
  expect_identical(nthr(b)$maxsc, c(1, 1, 1))
  expect_identical(nthr(rbind(0, b))$thres, nthr(b)$thres)
  expect_identical(nthr(matrix(b, 1))$thres, nthr(b)$thres)
  # one row of zeros = three difficulties 0, not the zero row
  expect_identical(nthr(matrix(0, 1, 3))$thres, matrix(0, 2, 3))
})

test_that("normalize_thres: NA below the last threshold gives maxsc", {
  thr <- rbind(0, c(-1, -2, 0.5), c(1, NA, 1), c(2, NA, NA))
  out <- nthr(thr)
  expect_identical(out$thres, thr)
  expect_identical(out$maxsc, c(3, 1, 2))
  # without the zero row the same
  expect_identical(nthr(thr[-1, ])$thres, thr)
})

test_that("normalize_thres: invalid thres are errors that name thres", {
  expect_error(nthr(rbind(NA, c(-1, 1))), "thres")                 # NA in row 1
  expect_error(nthr(rbind(0, c(-1, NA), c(1, 2))), "thres")        # gap
  expect_error(nthr(rbind(0, c(-1, NA))), "thres")                 # item without threshold
  expect_error(nthr(c(0, Inf)), "thres")
  expect_error(nthr(c("a", "b")), "thres")
})

test_that("findmodel uses the same rule", {
  thr <- rbind(c(-1, -2, 0.5), c(1, NA, 1))
  expect_identical(findmodel(thr), c("GPCM", "4PL", "GPCM"))
  expect_identical(findmodel(c(1, 2)), c("4PL", "4PL"))
})

# --- recycling and defaults -------------------------------------------------

X2 <- set_2pl$X
p2 <- items_to_pp(set_2pl$items)
n <- nrow(X2)

test_that("scalars are recycled to items and persons", {
  a <- prep(X2, p2$b, slopes = 1.5, lowerA = 0.1, upperA = 0.9, mu = 0.5, sigma2 = 2,
            theta_start = 0.2, type = "map", family = "4pl")
  b <- prep(X2, p2$b, slopes = rep(1.5, 12), lowerA = rep(0.1, 12), upperA = rep(0.9, 12),
            mu = rep(0.5, n), sigma2 = rep(2, n), theta_start = rep(0.2, n), type = "map",
            family = "4pl")
  expect_identical(a, b)
})

test_that("defaults: slopes 1, asymptotes 0/1, prior N(0, 1), model2est from thres", {
  a <- prep(X2, p2$b, type = "map", family = "4pl")
  expect_identical(a$slopes, rep(1, 12))
  expect_identical(a$lowerA, rep(0, 12))
  expect_identical(a$upperA, rep(1, 12))
  expect_identical(a$mu, rep(0, n))
  expect_identical(a$sigma2, rep(1, n))
  pm <- items_to_pp(set_mixed$items)
  m <- prep(set_mixed$X, pm$thres, slopes = pm$slopes, family = "mixed")
  expect_identical(m$model2est, pm$model2est)
})

test_that("wrong lengths are errors", {
  expect_error(prep(X2, p2$b, slopes = rep(1, 11), family = "4pl"), "slopes")
  expect_error(prep(X2, p2$b, lowerA = rep(0, 2), family = "4pl"), "lowerA")
  expect_error(prep(X2, p2$b, mu = rep(0, n - 1), family = "4pl"), "mu")
  expect_error(prep(X2, p2$b, sigma2 = rep(1, n + 1), family = "4pl"), "sigma2")
  expect_error(prep(X2, p2$b, theta_start = c(0, 1), family = "4pl"), "theta_start")
  expect_error(prep(X2, p2$b[-1], family = "4pl"), "thres")
})

test_that("invalid scalar arguments are errors", {
  expect_error(prep(X2, p2$b, type = c("mle", "wle"), family = "4pl"), "type")
  expect_error(prep(X2, p2$b, maxsteps = 2.5, family = "4pl"), "maxsteps")
  expect_error(prep(X2, p2$b, exac = 0, family = "4pl"), "exac")
  expect_error(prep(X2, p2$b, H = -1, family = "4pl"), "H")
  expect_error(prep(X2, p2$b, theta_start = NA, family = "4pl"), "theta_start")
})

test_that("PP_4pl rejects polytomous thresholds", {
  thr <- items_to_pp(set_pcm$items)$thres
  expect_error(prep(set_pcm$X, thr, family = "4pl"), "PP_gpcm")
})

test_that("responses: categories above thres are an error, NA columns are fine", {
  X <- X2
  X[1, 1] <- 2L
  expect_error(prep(X, p2$b, family = "4pl"), "categories")
  X <- X2
  X[, 3] <- NA
  expect_silent(PP:::pp_prepare(X, p2$b, family = "4pl"))
})

test_that("skipcheck skips only the checks of the response values", {
  ctrl <- list(skipcheck = TRUE)
  expect_error(prep(X2, p2$b, type = "wlee", ctrl = ctrl, family = "4pl"), "type")
  expect_error(prep(X2, p2$b, mu = rep(0, 2), ctrl = ctrl, family = "4pl"), "mu")
  X <- X2
  X[1, 1] <- 2L
  expect_identical(prep(X, p2$b, ctrl = ctrl, family = "4pl")$X[1, 1], 2L)
})

test_that("the response matrix becomes an integer matrix with its dimnames", {
  X <- X2
  storage.mode(X) <- "double"
  dimnames(X) <- list(paste0("p", seq_len(n)), paste0("i", 1:12))
  a <- prep(X, p2$b, family = "4pl")
  expect_identical(typeof(a$X), "integer")
  expect_identical(dimnames(a$X), dimnames(X))
  expect_identical(prep(as.data.frame(X), p2$b, family = "4pl")$X, a$X)
})

# --- item bank --------------------------------------------------------------

test_that("item bank of a mixed test", {
  pm <- items_to_pp(set_mixed$items)
  bank <- prep(set_mixed$X, pm$thres, slopes = pm$slopes, lowerA = pm$lowerA,
               upperA = pm$upperA, model2est = pm$model2est, family = "mixed")$bank
  it <- set_mixed$items
  is4 <- pm$model2est == "4PL"
  thr <- lapply(it, function(i) if (i$model == "4PL") i$b else i$delta)
  ncat <- vapply(it, ref_ncat, 1L)
  expect_identical(bank$n, 12L)
  expect_identical(bank$model, ifelse(is4, 0L, 1L))
  expect_identical(bank$ncat, ncat)
  expect_identical(bank$off, as.integer(cumsum(c(0, ncat - 1))[1:12]))
  expect_equal(bank$thr, unlist(thr))
  expect_equal(bank$a, pm$slopes)
  expect_equal(bank$c, ifelse(is4, pm$lowerA, 0))
  expect_equal(bank$d, ifelse(is4, pm$upperA, 1))
  expect_equal(bank$hub_loc, vapply(thr, mean, 0))
  # the thresholds of item j sit at off[j] + 1 .. off[j] + ncat[j] - 1
  for (j in 1:12) {
    expect_equal(bank$thr[bank$off[j] + seq_len(bank$ncat[j] - 1)], thr[[j]])
  }
})

test_that("item bank: PP_gpcm items are GPCM items, also with two categories", {
  bank <- prep(X2, p2$b, slopes = p2$slopes, family = "gpcm")$bank
  expect_identical(bank$model, rep(1L, 12))
  expect_identical(bank$c, rep(0, 12))
  expect_identical(bank$d, rep(1, 12))
})

# --- known bugs -------------------------------------------------------------

test_that("B6: mle, wle and robust have no prior (NA), map and eap N(0, 1)", {
  for (ty in c("mle", "wle", "robust")) {
    a <- prep(X2, p2$b, type = ty, family = "4pl")
    expect_identical(a$mu, rep(NA_real_, n))
    expect_identical(a$sigma2, rep(NA_real_, n))
  }
  for (ty in c("map", "eap")) {
    a <- prep(X2, p2$b, type = ty, family = "4pl")
    expect_identical(a$mu, rep(0, n))
    expect_identical(a$sigma2, rep(1, n))
  }
})

test_that("B6: a prior with mle, wle or robust is ignored with a warning", {
  pr <- function(...) PP:::pp_prepare(X2, p2$b, family = "4pl", ...)
  for (ty in c("mle", "wle", "robust")) {
    expect_warning(a <- pr(type = ty, mu = 0.5), "ignored")
    expect_identical(a$mu, rep(NA_real_, n))
    expect_warning(a <- pr(type = ty, sigma2 = rep(2, n)), "ignored")
    expect_identical(a$sigma2, rep(NA_real_, n))
    # NA, as in $model of such an object, is accepted silently
    expect_silent(a <- pr(type = ty, mu = rep(NA_real_, n), sigma2 = NA))
    expect_identical(a$mu, rep(NA_real_, n))
    # wrong lengths stay errors
    expect_error(pr(type = ty, mu = rep(0, n - 1)), "mu")
  }
})
