# Benchmark scenarios for PP.
#
# The response data are simulated in plain R, independent of PP's own
# simulators, so every PP version is timed on exactly the same matrices.
# Only the public API is called, with arguments that survive the refactoring.
# Targets are single-threaded goals in seconds.
#
# Used by bench/bench.R and bench/compare.R; run those from the package root.

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

# setup() runs once and is not timed; run(d, s) gets its result as s.
bench_scenarios <- list(
  "3pl_mle"  = list(size = "N = 5000, I = 40", target = 0.05,
                    run = function(d, s) fit_3pl(d, "mle")),
  "3pl_wle"  = list(size = "N = 5000, I = 40", target = 0.05,
                    run = function(d, s) fit_3pl(d, "wle")),
  "3pl_map"  = list(size = "N = 5000, I = 40", target = 0.05,
                    run = function(d, s) fit_3pl(d, "map")),
  "3pl_eap"  = list(size = "N = 5000, I = 40", target = 0.03,
                    run = function(d, s) fit_3pl(d, "eap")),
  "gpcm_mle" = list(size = "N = 2000, I = 20, K = 5", target = 0.02,
                    run = function(d, s) fit_gpcm(d, "mle")),
  "gpcm_wle" = list(size = "N = 2000, I = 20, K = 5", target = 0.02,
                    run = function(d, s) fit_gpcm(d, "wle")),
  "gpcm_eap" = list(size = "N = 2000, I = 20, K = 5", target = 0.02,
                    run = function(d, s) fit_gpcm(d, "eap")),
  "jk_wle"   = list(size = "N = 500, I = 40", target = 0.1,
                    setup = function(d) fit_3pl(d, "wle", rows = 1:500),
                    run = function(d, s) JKpp(s)),
  "pv"       = list(size = "N = 300, I = 40, npv = 10", target = 0.01,
                    setup = function(d) fit_3pl(d, "eap", rows = 1:300),
                    run = function(d, s) { set.seed(42); PV(s, npv = 10, approx = FALSE) }),
  "sim_gpcm" = list(size = "N = 2000, I = 20, K = 5", target = 0.01,
                    run = function(d, s) { set.seed(42); sim_gpcm(d$thres_g, d$ag, d$thg) })
)

# --- timing --------------------------------------------------------------

# Swallow the cat() output and warnings of the old code.
quietly <- function(expr) {
  out <- NULL
  utils::capture.output(out <- suppressMessages(suppressWarnings(expr)))
  invisible(out)
}

# A run slower than `slow` seconds is timed once: its warm-up run is the
# measurement, warm-up effects do not matter at that scale.
time_scenario <- function(sc, d, reps, slow = 10) {
  s <- if (is.null(sc$setup)) NULL else quietly(sc$setup(d))
  run_once <- function() system.time(quietly(sc$run(d, s)), gcFirst = TRUE)[["elapsed"]]
  t <- run_once()
  if (t <= slow) t <- vapply(seq_len(reps), function(i) run_once(), numeric(1))
  c(median = stats::median(t), min = min(t), reps = length(t))
}

bench_meta <- function() {
  git <- function(...) {
    tryCatch(suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = FALSE)),
             error = function(e) character(0))
  }
  sha <- git("rev-parse", "--short", "HEAD")
  dirty <- length(git("status", "--porcelain", "--", "R", "src", "DESCRIPTION")) > 0
  cpu <- tryCatch(grep("^model name", readLines("/proc/cpuinfo"), value = TRUE)[1],
                  error = function(e) NA_character_, warning = function(w) NA_character_)
  list(pp_version = as.character(utils::packageVersion("PP")),
       git = if (length(sha)) paste0(sha, if (dirty) "-dirty") else NA_character_,
       r = R.version.string,
       cpu = sub("^model name\\s*:\\s*", "", cpu),
       date = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
}

run_bench <- function(only = NULL, reps = 5) {
  suppressPackageStartupMessages(library(PP))
  d <- bench_data()
  ids <- names(bench_scenarios)
  if (!is.null(only)) ids <- grep(only, ids, value = TRUE)
  rows <- lapply(ids, function(id) {
    sc <- bench_scenarios[[id]]
    message(sprintf("%-9s ...", id))
    tm <- time_scenario(sc, d, reps)
    data.frame(scenario = id, size = sc$size, median = tm[["median"]],
               min = tm[["min"]], reps = tm[["reps"]], target = sc$target)
  })
  res <- do.call(rbind, rows)
  attr(res, "meta") <- bench_meta()
  res
}

# --- command line --------------------------------------------------------

# Parses --key=value and --flag arguments.
bench_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  get <- function(key, default = NULL) {
    hit <- grep(paste0("^--", key, "(=|$)"), args, value = TRUE)
    if (!length(hit)) return(default)
    val <- sub(paste0("^--", key, "=?"), "", hit[1])
    if (nzchar(val)) val else TRUE
  }
  list(reps = as.integer(get("reps", 5)), only = get("only"), out = get("out"),
       baseline = get("baseline", "bench/baseline.rds"), current = get("current"),
       md = isTRUE(get("md", FALSE)))
}

print_meta <- function(res, label) {
  m <- attr(res, "meta")
  cat(sprintf("%-9s PP %s @ %s, %s, %s, %s\n", label, m$pp_version, m$git, m$r, m$cpu, m$date))
}
