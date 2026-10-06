# Test data, generated in plain R with fixed seeds (PP's own simulators are
# only used in test-sim.R). Items are written out explicitly in the format of
# helper-2-reference.R and converted into PP's arguments by items_to_pp(), so the
# test data do not depend on how PP normalises its inputs.

item_4pl  <- function(a, b, c = 0, d = 1) list(model = "4PL", a = a, b = b, c = c, d = d)
item_gpcm <- function(a, delta) list(model = "GPCM", a = a, delta = delta)
items_4pl <- function(a, b, c = 0, d = 1) {
  n <- length(b)
  Map(item_4pl, rep_len(a, n), b, rep_len(c, n), rep_len(d, n))
}

# PP's arguments: thres (first row 0, NA below the last threshold), slopes,
# lowerA, upperA, model2est; b = difficulties of the 4PL items
items_to_pp <- function(items) {
  K <- max(vapply(items, ref_ncat, 1L))
  thres <- matrix(NA_real_, K, length(items))
  thres[1, ] <- 0
  for (j in seq_along(items)) {
    it <- items[[j]]
    thr <- if (it$model == "4PL") it$b else it$delta
    thres[1 + seq_along(thr), j] <- thr
  }
  is4 <- vapply(items, function(it) it$model == "4PL", TRUE)
  list(thres = thres,
       b = thres[2, ],
       slopes = vapply(items, function(it) it$a, 0),
       lowerA = ifelse(is4, vapply(items, function(it) if (is.null(it[["c"]])) 0 else it[["c"]], 0), 0),
       upperA = ifelse(is4, vapply(items, function(it) if (is.null(it[["d"]])) 1 else it[["d"]], 0), 1),
       model2est = ifelse(is4, "4PL", "GPCM"))
}

# Responses by inverse CDF from ref_catprob(), persons x items
sim_resp <- function(theta, items, seed) {
  set.seed(seed)
  X <- vapply(items, function(it) {
    cum <- t(apply(ref_catprob(it, theta), 1, cumsum))
    as.integer(rowSums(stats::runif(length(theta)) > cum))
  }, integer(length(theta)))
  matrix(X, length(theta))
}

# A test set: items, responses and which PP function estimates it
make_set <- function(items, theta, seed, kind, extreme_rows = integer(0)) {
  X <- sim_resp(theta, items, seed)
  top <- vapply(items, ref_ncat, 1L) - 1L
  if (length(extreme_rows)) {
    X[extreme_rows[1], ] <- 0L
    X[extreme_rows[2], ] <- top
  }
  list(items = items, X = X, kind = kind)
}

# Call PP_4pl / PP_gpcm / PPall for a set. Default item parameters are left
# out for PP_4pl, as a user would do (1PL: only thres, 2PL: + slopes, ...).
pp_fit <- function(set, type, X = set$X, ...) {
  p <- items_to_pp(set$items)
  pp_quiet(switch(set$kind,
    "4pl" = PP_4pl(X, thres = p$b,
                   slopes = if (any(p$slopes != 1)) p$slopes,
                   lowerA = if (any(p$lowerA != 0)) p$lowerA,
                   upperA = if (any(p$upperA != 1)) p$upperA,
                   type = type, ...),
    gpcm  = PP_gpcm(X, thres = p$thres, slopes = p$slopes, type = type, ...),
    mixed = PPall(X, thres = p$thres, slopes = p$slopes, lowerA = p$lowerA,
                  upperA = p$upperA, model2est = p$model2est, type = type, ...)))
}

# Responses with an adaptive-test-like NA pattern: each person answers a random
# half to all but one of the items
make_na <- function(X, seed) {
  set.seed(seed)
  k <- ncol(X)
  for (i in seq_len(nrow(X))) {
    keep <- sample(k, sample(ceiling(k / 2):(k - 1), 1))
    X[i, -keep] <- NA
  }
  X
}

# --- dichotomous sets: 12 items, same difficulties ------------------------

b12 <- seq(-2.5, 2.5, length.out = 12)
a12 <- c(0.6, 1.2, 0.8, 1.4, 1.0, 0.7, 1.3, 0.9, 1.5, 1.1, 0.75, 1.25)
c12 <- c(0.10, 0.20, 0.05, 0.15, 0.25, 0.00, 0.10, 0.20, 0.05, 0.15, 0.10, 0.20)
d12 <- c(0.95, 0.90, 1.00, 0.85, 0.95, 0.90, 1.00, 0.95, 0.90, 0.85, 1.00, 0.95)
theta20 <- seq(-2, 2.5, length.out = 20)

set_1pl <- make_set(items_4pl(1, b12), theta20, 101, "4pl", c(4, 9))
set_2pl <- make_set(items_4pl(a12, b12), theta20, 102, "4pl", c(4, 9))
set_3pl <- make_set(items_4pl(a12, b12, c12), theta20, 103, "4pl", c(4, 9))
set_4pl <- make_set(items_4pl(a12, b12, c12, d12), theta20, 113, "4pl", c(4, 9))
sets_dich <- list("1PL" = set_1pl, "2PL" = set_2pl, "3PL" = set_3pl, "4PL" = set_4pl)

# --- polytomous sets: 6 items with 3 to 5 categories ----------------------

delta6 <- list(c(-1, 0.5), c(-1.5, -0.2, 1), c(-0.5, 0.3, 1.2),
               c(-2, -0.8, 0.4, 1.5), c(0, 1), c(-1.2, 0, 0.8))
a6 <- c(0.7, 1.0, 1.3, 0.9, 1.2, 0.8)
theta25 <- seq(-2, 2, length.out = 25)

set_pcm  <- make_set(lapply(delta6, item_gpcm, a = 1), theta25, 201, "gpcm", c(5, 18))
set_gpcm <- make_set(Map(item_gpcm, a6, delta6), theta25, 202, "gpcm", c(5, 18))
sets_poly <- list(PCM = set_pcm, GPCM = set_gpcm)

# --- mixed: 8 x 4PL + 4 x GPCM --------------------------------------------

set_mixed <- make_set(c(items_4pl(a12[1:8], seq(-2.5, 2.5, length.out = 8),
                                  c12[1:8], d12[1:8]),
                        Map(item_gpcm, a6[1:4], delta6[1:4])),
                      theta25, 301, "mixed", c(7, 15))

sets_all <- c(sets_dich, sets_poly, list(mixed = set_mixed))

# --- special cases --------------------------------------------------------

# Person-specific priors
prior_mu <- function(n) seq(-2, 2, length.out = n)
prior_s2 <- function(n) rep_len(c(0.5, 1, 2), n)

# 3PL persons from the benchmark data (bench/scenarios.R, seed 1, 5000 x 40):
# rows where Newton oscillates (result depends on maxsteps) or ends in NaN
# (A8). The simulation is copied from bench/scenarios.R.
osc3pl <- local({
  sim_3pl_r <- function(theta, a, b, c) {
    eta <- sweep(outer(theta, b, "-"), 2, a, "*")
    P <- sweep(sweep(plogis(eta), 2, 1 - c, "*"), 2, c, "+")
    matrix(as.integer(runif(length(P)) < P), nrow(P))
  }
  set.seed(1)
  b <- rnorm(40); a <- runif(40, 0.7, 1.8); c <- runif(40, 0, 0.2)
  th <- rnorm(5000)
  X <- sim_3pl_r(th, a, b, c)
  rows <- c(2913, 3600, 4035, 4139, 152, 197, 279, 1826, 2052, 2287, 3477)
  list(items = items_4pl(a, b, c), X = X[rows, ], kind = "4pl", rows = rows)
})

# One extreme item (C1): b = +-40 with slope 20
set_extreme <- list(items = items_4pl(20, c(-40, 0, 0.5, 1, 40)),
                    X = matrix(c(1, 1, 0, 1, 0,
                                 1, 0, 0, 0, 0), 2, byrow = TRUE),
                    kind = "4pl")

# Long test (C3): 1100 Rasch items close to the persons, so the likelihood
# (about 0.5^1100) underflows when computed as a product
set_long <- local({
  items <- items_4pl(1, seq(-0.5, 0.5, length.out = 1100))
  list(items = items, X = sim_resp(c(-1, -0.3, 0, 0.4, 1), items, 401), kind = "4pl")
})

# Two GPCM items with 12 categories (A12): patterns (1, 11) and (11, 1) give
# the same key without a separator
set_gpcm12 <- list(items = Map(item_gpcm, c(0.6, 1.4),
                               list(seq(-3, 3, length.out = 11), seq(-2.5, 3.5, length.out = 11))),
                   X = matrix(c(1, 11, 11, 1, 5, 6, 3, 2), 4, byrow = TRUE),
                   kind = "gpcm")

# PCM where nobody reaches the top category of items 1 and 3 (A10)
set_pcm_unobs <- local({
  items <- lapply(list(c(-1, 0.5, 6), c(-1.5, -1, -0.5), c(0, 1, 6)), item_gpcm, a = 1)
  X <- sim_resp(seq(-2, 0.5, length.out = 40), items, 501)
  stopifnot(max(X[, 1]) < 3, max(X[, 3]) < 3, max(X[, 2]) == 3)
  list(items = items, X = X, kind = "gpcm")
})

# Persons whose MLE is finite but who get an extreme score when one item is
# left out (A11)
near_extreme <- function(x, items) {
  obs <- which(!is.na(x))
  any(vapply(obs, function(j) { xx <- x; xx[j] <- NA; ref_is_extreme(xx, items) != 0 }, TRUE))
}
