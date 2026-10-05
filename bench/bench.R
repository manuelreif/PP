# Time all benchmark scenarios against the installed PP.
#
#   Rscript bench/bench.R [--reps=5] [--only=<regex>] [--out=<file.rds>]
#
# Run from the package root after `R CMD INSTALL .`. The baseline was made with
#   Rscript bench/bench.R --out=bench/baseline.rds

Sys.setenv(OMP_NUM_THREADS = "1")
options(PP.threads = 1, width = 120)
source("bench/scenarios.R")

args <- bench_args()
res <- run_bench(only = args$only, reps = args$reps)

print_meta(res, "PP")
print(res, row.names = FALSE, digits = 3)

if (!is.null(args$out)) {
  saveRDS(res, args$out)
  message("saved to ", args$out)
}
