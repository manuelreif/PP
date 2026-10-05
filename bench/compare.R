# Compare the installed PP (or a saved result) against the baseline.
#
#   Rscript bench/compare.R [--reps=5] [--only=<regex>] [--md]
#                           [--baseline=bench/baseline.rds] [--current=<file.rds>]
#
# Without --current the scenarios are timed now. --md prints a Markdown table.

Sys.setenv(OMP_NUM_THREADS = "1")
options(PP.threads = 1, width = 120)
source("bench/scenarios.R")

args <- bench_args()
base <- readRDS(args$baseline)
cur <- if (is.null(args$current)) {
  run_bench(only = args$only, reps = args$reps)
} else {
  readRDS(args$current)
}

ids <- intersect(names(bench_scenarios), intersect(base$scenario, cur$scenario))
b <- base[match(ids, base$scenario), ]
k <- cur[match(ids, cur$scenario), ]
tab <- data.frame(
  scenario = ids,
  size = vapply(bench_scenarios[ids], `[[`, "", "size"),
  baseline_s = b$median,
  current_s = k$median,
  speedup = b$median / k$median,
  target_s = vapply(bench_scenarios[ids], `[[`, 0, "target"),
  row.names = NULL
)
tab$target_met <- tab$current_s <= tab$target_s

print_meta(base, "baseline")
print_meta(cur, "current")
cat("\n")
if (args$md) {
  print(knitr::kable(tab, digits = c(0, 0, 3, 3, 1, 3, 0)))
} else {
  print(tab, row.names = FALSE, digits = 3)
}
