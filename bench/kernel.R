# Microbenchmark of the C++ item kernel (src/items.h): nanoseconds per item
# evaluation (ll, score, info and J of one person-item pair).
#
#   Rscript bench/kernel.R [--evals=1e7] [--rounds=5]
#
# Run from the package root after `R CMD INSTALL .` (without the -O0 objects
# that roxygenize leaves in src/). Uses the internal hook pp_kernel_time_cpp(),
# which loops over all pairs `reps` times inside C++, so R overhead is excluded.
# Prints the median over the rounds.

suppressPackageStartupMessages(library(PP))

args <- commandArgs(TRUE)
arg <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) as.numeric(sub(".*=", "", hit[1])) else default
}
n_evals <- arg("evals", 1e7)
rounds <- arg("rounds", 5)

# Item banks and responses, simulated with PP itself (only the timing matters)
set.seed(1)
bank_of <- function(thres, slopes, lowerA = NULL, upperA = NULL, family) {
  PP:::prepare_items(PP:::normalize_thres(thres), slopes, lowerA, upperA, NULL, family)$bank
}
gpcm_case <- function(N, I, K) {
  thres <- rbind(0, apply(matrix(rnorm(I * (K - 1)), K - 1), 2, sort))
  a <- runif(I, 0.7, 1.8)
  th <- rnorm(N)
  list(bank = bank_of(thres, a, family = "gpcm"), theta = th, X = sim_gpcm(thres, a, th))
}
b <- rnorm(40); a <- runif(40, 0.7, 1.8); c <- runif(40, 0, 0.2)
th <- rnorm(5000)
cases <- list(
  "3PL, 5000 x 40" = list(bank = bank_of(b, a, c, 1, "4pl"), theta = th,
                          X = sim_4pl(b, a, c, 1, th)),
  "1PL, 5000 x 40" = list(bank = bank_of(b, 1, 0, 1, "4pl"), theta = th,
                          X = sim_4pl(b, 1, 0, 1, th)),
  "GPCM K = 5, 2000 x 20" = gpcm_case(2000, 20, 5),
  "GPCM K = 12, 2000 x 20" = gpcm_case(2000, 20, 12)
)

cat(sprintf("%-24s %10s %12s\n", "Fall", "ns/Ausw.", "Auswertungen"))
for (nm in names(cases)) {
  cs <- cases[[nm]]
  pairs <- length(cs$X)
  reps <- max(1L, as.integer(round(n_evals / pairs)))
  PP:::pp_kernel_time_cpp(cs$bank, cs$theta, cs$X, 1L)             # warm-up
  t <- replicate(rounds, system.time(PP:::pp_kernel_time_cpp(cs$bank, cs$theta, cs$X, reps))[["elapsed"]])
  cat(sprintf("%-24s %10.1f %12.0f\n", nm, 1e9 * median(t) / (reps * pairs), reps * pairs))
}
