# Data for the pictures of the safeguarded Newton (person estimator).
#
#   Rscript blog/newton_daten.R [--label=block4]
#
# Writes blog/output/newton_konvergenz/newton_<label>.rds, a list with
#   pendel     the oscillating 3PL person of fehler_newton (score 0, WLE): the
#              iterates of PP 0.6.4-1 (from fehlerbilder_block1.rds) and of the
#              new estimator with their bracket, the curve f(theta) and the root
#   schritte   number of steps of every person in the benchmark data, plus the
#              steps PP 0.6.4-1 gave every person (baseline benchmark)
#   konvergenz |theta_k - theta*| after k steps for 300 persons
#   stopp      the same persons with the default exac: number of steps and the
#              distance of the returned estimate to theta*
#
# The estimator is deterministic, so pp_estimate_cpp() with maxsteps = k
# returns its k-th iterate.

source("blog/gemeinsam.R")
lade_test_helfer()
source("bench/scenarios.R")
source("bench/read_bench.R")
args <- blog_args()

# Estimator call on a response matrix with prior N(0, 1) and the default start
iterate <- function(X, items, type, k, exac = 0.001) {
  n <- nrow(X)
  PP:::pp_estimate_cpp(kernel_bank(items), X, type, rep(0, n), rep(1, n), 1, rep(NA_real_, n),
                       c(-6, 6), exac, k)
}

# --- the oscillating person ----------------------------------------------

alt_file <- file.path(blog_out, "fehlerbilder_block1.rds")
if (!file.exists(alt_file)) {
  stop(alt_file, " is missing: it holds the iterates of PP 0.6.4-1 ",
       "(blog/fehlerbilder_daten.R --label=block1 on the old code)")
}
alt <- readRDS(alt_file)$a8

s <- set_3pl
x <- s$X[4, , drop = FALSE]   # score 0, as in fehler_newton
stopifnot(sum(x) == 0)
f <- function(t) ref_esteq(t, x[1, ], s$items, "wle")

fertig <- iterate(x, s$items, "wle", 100)
n_neu <- fertig$nsteps
neu <- data.table(k = 0:n_neu, theta = vapply(0:n_neu, function(k) iterate(x, s$items, "wle", k)$theta, 0))
stopifnot(neu[k == n_neu, theta] == fertig$theta)

# bracket in which iterate k was chosen: [-6, 6], then lo = theta_j where
# f(theta_j) > 0 and hi = theta_j where f(theta_j) < 0 for all j < k
neu[, f := f(theta)]
lo <- -6; hi <- 6
for (i in seq_len(nrow(neu))) {
  set(neu, i, c("lo", "hi"), list(lo, hi))
  if (neu$f[i] > 0) lo <- neu$theta[i] else hi <- neu$theta[i]
}
neu[, schritt := fifelse(k == 0, "Start",
                         fifelse(abs(theta - (lo + hi) / 2) < 1e-12, "Halbierung", "Newton"))]

# PP 0.6.4-1 started at the mean difficulty of the answered items
start_alt <- mean(vapply(s$items, function(it) it$b, 0))
alt_pfad <- rbind(data.table(k = 0L, theta = start_alt),
                  data.table(k = alt$pfad$maxsteps, theta = alt$pfad$theta))

wurzel <- ref_estimate(x[1, ], s$items, "wle")
g <- seq(-6, 6, by = 0.01)
pendel <- list(neu = neu, alt = alt_pfad, kurve = data.table(theta = g, f = f(g)),
               wurzel = wurzel, n_neu = n_neu,
               beschreibung = "3PL, 12 Items, Person mit Score 0, WLE")

# --- steps per person in the benchmark data ---------------------------------

d <- bench_data()
it1 <- items_4pl(1, d$b)
it2 <- items_4pl(d$a, d$b)
it3 <- items_4pl(d$a, d$b, d$c)
itg <- Map(item_gpcm, d$ag, split(d$thres_g[-1, ], col(d$thres_g[-1, ])))
faelle <- list(list("1PL", d$X1, it1), list("2PL", d$X2, it2), list("3PL", d$X3, it3),
               list("GPCM", d$Xg, itg))
schritte <- rbindlist(lapply(faelle, function(fa) {
  rbindlist(lapply(c("mle", "wle", "map"), function(ty) {
    r <- iterate(fa[[2]], fa[[3]], ty, 100)
    data.table(modell = fa[[1]], methode = toupper(ty), nsteps = r$nsteps,
               status = c("ok", "extreme", "boundary", "no_response", "not_converged")[r$status])
  }))
}))

# PP 0.6.4-1 iterated all persons together, so every person got the steps of
# the whole run (40 = maxsteps of PP_4pl: not converged)
base <- bench_read(label = "baseline")
alt_schritte <- unique(base[grepl("^(1pl|2pl|3pl|gpcm)_(mle|wle|map)$", szenario),
                            .(szenario, iterationen)])
alt_schritte[, `:=`(modell = toupper(sub("_.*", "", szenario)),
                    methode = toupper(sub(".*_", "", szenario)))]
alt_schritte[, szenario := NULL]

# --- error per step ------------------------------------------------------

set.seed(7)
rows <- sample(nrow(d$X2), 300)
konv_faelle <- list(list("2PL", d$X2, it2), list("3PL", d$X3, it3))
konv <- lapply(konv_faelle, function(fa) {
  X <- fa[[2]][rows, ]
  lapply(c("mle", "wle"), function(ty) {
    star <- iterate(X, fa[[3]], ty, 200, exac = 1e-12)
    ok <- star$status == 1
    pfad <- rbindlist(lapply(0:12, function(k) {
      th <- iterate(X, fa[[3]], ty, k, exac = 1e-12)$theta
      data.table(modell = fa[[1]], methode = toupper(ty), person = which(ok), k = k,
                 fehler = abs(th - star$theta)[ok])
    }))
    # where the run with the default exac stops and how close its result is
    def <- iterate(X, fa[[3]], ty, 100)
    stopp <- data.table(modell = fa[[1]], methode = toupper(ty), person = which(ok),
                        k = def$nsteps[ok], fehler = abs(def$theta - star$theta)[ok])
    list(pfad = pfad, stopp = stopp)
  })
})
konv <- unlist(konv, recursive = FALSE)
konvergenz <- rbindlist(lapply(konv, `[[`, "pfad"))
stopp <- rbindlist(lapply(konv, `[[`, "stopp"))

ordner <- file.path(blog_out, "newton_konvergenz")
dir.create(ordner, showWarnings = FALSE, recursive = TRUE)
out <- file.path(ordner, sprintf("newton_%s.rds", args$label))
saveRDS(list(pendel = pendel, schritte = schritte, alt_schritte = alt_schritte,
             konvergenz = konvergenz, stopp = stopp, exac = 0.001, code = code_stand(), zeitpunkt = Sys.time()),
        out)
message("gespeichert: ", out)

cat("Pendel-Person:", n_neu, "Schritte, Ende", fertig$theta, "Referenz", wurzel,
    "Differenz", abs(fertig$theta - wurzel), "\n")
print(neu)
stopifnot(all(neu$lo < neu$hi), all(diff(neu$hi - neu$lo) <= 0),
          all(neu$theta >= neu$lo & neu$theta <= neu$hi),
          all(wurzel >= neu$lo & wurzel <= neu$hi))
print(dcast(schritte[status == "ok", .(median = median(nsteps)), by = .(modell, methode)],
            modell ~ methode, value.var = "median"))
print(schritte[, .N, by = .(modell, methode, status)][status != "ok"])
print(konvergenz[, .(median = median(fehler)), by = .(modell, methode, k)][k <= 5] |>
        dcast(modell + methode ~ k, value.var = "median"))
print(stopp[, .(schritte = as.numeric(median(k)), median = median(fehler), max = max(fehler),
                ueber_exac = sum(fehler > 0.001)), by = .(modell, methode)])
