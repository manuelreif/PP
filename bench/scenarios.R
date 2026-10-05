# Benchmark scenarios for PP.
#
# The response data are simulated in plain R, independent of PP's own
# simulators, so every PP version is timed on exactly the same matrices.
# Only the public API is called, with arguments that survive the refactoring.
# Targets (ziel) are single-threaded goals in seconds after the refactoring.
#
# Used by bench/bench.R; run it from the package root.

# --- data ----------------------------------------------------------------

# 3PL: P = c + (1 - c) * plogis(a * (theta - b))
sim_3pl_r <- function(theta, a, b, c) {
  eta <- sweep(outer(theta, b, "-"), 2, a, "*")
  P <- sweep(sweep(plogis(eta), 2, 1 - c, "*"), 2, c, "+")
  matrix(as.integer(runif(length(P)) < P), nrow(P))
}

# GPCM: P(X = k) proportional to exp(sum_{m <= k} a * (theta - delta_m)),
# delta is a (K - 1) x I matrix, categories 0, ..., K - 1
sim_gpcm_r <- function(theta, a, delta) {
  K <- nrow(delta) + 1
  N <- length(theta)
  U <- upper.tri(diag(K), diag = TRUE)
  sapply(seq_len(ncol(delta)), function(j) {
    eta <- a[j] * (outer(theta, 0:(K - 1)) - rep(c(0, cumsum(delta[, j])), each = N))
    P <- exp(eta - apply(eta, 1, max))
    P <- P / rowSums(P)
    as.integer(rowSums(runif(N) > P %*% U))   # inverse CDF
  })
}

bench_data <- function() {
  set.seed(1)
  N <- 5000; I <- 40
  b <- rnorm(I); a <- runif(I, 0.7, 1.8); c <- runif(I, 0, 0.2)
  X3 <- sim_3pl_r(rnorm(N), a, b, c)

  set.seed(2)
  Ng <- 2000; Ig <- 20; K <- 5
  delta <- apply(matrix(rnorm((K - 1) * Ig), K - 1), 2, sort)
  ag <- runif(Ig, 0.5, 1.5)
  thg <- rnorm(Ng)
  Xg <- sim_gpcm_r(thg, ag, delta)

  list(X3 = X3, b = b, a = a, c = c,
       Xg = Xg, thres_g = rbind(0, delta), ag = ag, thg = thg)
}

# --- scenarios -----------------------------------------------------------

fit_3pl <- function(d, type, rows = seq_len(nrow(d$X3))) {
  PP_4pl(d$X3[rows, , drop = FALSE], d$b, slopes = d$a, lowerA = d$c, type = type)
}

fit_gpcm <- function(d, type) {
  PP_gpcm(d$Xg, d$thres_g, slopes = d$ag, type = type)
}

# The metadata fields are written to every row of the durchlaeufe table.
# setup() runs once and is not timed; run(d, s) gets its result as s.
scenario <- function(funktion, methode, modell, n_personen, n_items, ziel, run, setup = NULL) {
  list(funktion = funktion, methode = methode, modell = modell,
       n_personen = n_personen, n_items = n_items, ziel = ziel,
       run = run, setup = setup)
}

bench_scenarios <- list(
  "3pl_mle"  = scenario("PP_4pl", "MLE", "3PL", 5000, 40, 0.05,
                        function(d, s) fit_3pl(d, "mle")),
  "3pl_wle"  = scenario("PP_4pl", "WLE", "3PL", 5000, 40, 0.05,
                        function(d, s) fit_3pl(d, "wle")),
  "3pl_map"  = scenario("PP_4pl", "MAP", "3PL", 5000, 40, 0.05,
                        function(d, s) fit_3pl(d, "map")),
  "3pl_eap"  = scenario("PP_4pl", "EAP", "3PL", 5000, 40, 0.03,
                        function(d, s) fit_3pl(d, "eap")),
  "gpcm_mle" = scenario("PP_gpcm", "MLE", "GPCM", 2000, 20, 0.02,
                        function(d, s) fit_gpcm(d, "mle")),
  "gpcm_wle" = scenario("PP_gpcm", "WLE", "GPCM", 2000, 20, 0.02,
                        function(d, s) fit_gpcm(d, "wle")),
  "gpcm_eap" = scenario("PP_gpcm", "EAP", "GPCM", 2000, 20, 0.02,
                        function(d, s) fit_gpcm(d, "eap")),
  "jk_wle"   = scenario("JKpp", "WLE", "3PL", 500, 40, 0.1,
                        setup = function(d) fit_3pl(d, "wle", rows = 1:500),
                        run = function(d, s) JKpp(s)),
  "pv"       = scenario("PV", "EAP", "3PL", 300, 40, 0.01,
                        setup = function(d) fit_3pl(d, "eap", rows = 1:300),
                        run = function(d, s) { set.seed(42); PV(s, npv = 10, approx = FALSE) }),
  "sim_gpcm" = scenario("sim_gpcm", NA_character_, "GPCM", 2000, 20, 0.01,
                        function(d, s) { set.seed(42); sim_gpcm(d$thres_g, d$ag, d$thg) })
)
