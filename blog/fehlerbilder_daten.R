# Data for three pictures of bugs in PP 0.6.4-1 (installed PP vs. references).
#
#   Rscript blog/fehlerbilder_daten.R [--label=block1]
#
# Writes blog/output/fehlerbilder_<label>.rds, a list with
#   a8  WLE of a 3PL person with score 0 for maxsteps = 1..41, and the Warm
#       estimating equation f(theta) with its root (oscillation, A8)
#   a1  JK standard errors from JKpp vs. the textbook jackknife (A1)
#   a4  pooled PV draws of one replicated response pattern vs. the exact
#       posterior, without and with a prior mu = 2 (A4)

source("blog/gemeinsam.R")
lade_test_helfer()
args <- blog_args()

# --- A8: oscillating Newton --------------------------------------------------

s <- set_3pl
x <- s$X[4, , drop = FALSE]   # score 0
stopifnot(sum(x) == 0)
pfad <- rbindlist(lapply(1:41, function(k) {
  data.table(maxsteps = k, theta = est(pp_fit(s, "wle", X = x, maxsteps = k))[1, 1])
}))
wurzel <- ref_estimate(x[1, ], s$items, "wle")
g <- seq(-7, 0, by = 0.01)
kurve <- data.table(theta = g, f = ref_esteq(g, x[1, ], s$items, "wle"))
a8 <- list(pfad = pfad, kurve = kurve, wurzel = wurzel,
           beschreibung = "3PL, 12 Items, Person mit Score 0, WLE")

# --- A1: jackknife SE ------------------------------------------------------

jk_vergleich <- function(set, name) {
  X <- set$X
  jk <- pp_quiet(JKpp(pp_fit(set, "wle", X = X)))
  ref <- vapply(seq_len(nrow(X)), function(i) ref_jackknife(X[i, ], set$items, "wle")$se, 0)
  data.table(testsatz = name, L = ncol(X), se_pp = jk$resjk[, 2], se_korrekt = ref,
             extrem = apply(X, 1, ref_is_extreme, items = set$items) != 0)
}
a1 <- rbind(jk_vergleich(set_2pl, "2PL, 12 Items"), jk_vergleich(set_gpcm, "GPCM, 6 Items"))

# --- A4: plausible values -------------------------------------------------

s <- set_2pl
x3 <- s$X[3, ]
Xr <- matrix(x3, 2000, ncol(s$X), byrow = TRUE)
set.seed(41)
draws0 <- as.vector(PV(pp_fit(s, "eap", X = Xr), npv = 20, approx = FALSE)$pvdraws)
set.seed(42)
draws2 <- as.vector(PV(pp_fit(s, "eap", X = Xr[1:300, ], mu = rep(2, 300), sigma2 = rep(1, 300)),
                       npv = 20, approx = FALSE)$pvdraws)
dichte <- function(mu) {
  p <- ref_posterior(x3, s$items, mu, 1)
  data.table(theta = p$grid, dichte = p$w / diff(p$grid[1:2]), mean = p$mean, sd = p$sd)
}
a4 <- list(
  draws = rbind(data.table(fall = "Prior N(0, 1)", theta = draws0),
                data.table(fall = "Prior N(2, 1)", theta = draws2)),
  posterior = rbind(cbind(fall = "Prior N(0, 1)", prior = "N(0, 1)", dichte(0)),
                    cbind(fall = "Prior N(2, 1)", prior = "N(2, 1)", dichte(2)),
                    cbind(fall = "Prior N(2, 1)", prior = "N(0, 1)", dichte(0))),
  beschreibung = "2PL, 12 Items, eine Antwortfolge repliziert, 20 PV je Person"
)

out <- file.path(blog_out, sprintf("fehlerbilder_%s.rds", args$label))
dir.create(blog_out, showWarnings = FALSE, recursive = TRUE)
saveRDS(list(a8 = a8, a1 = a1, a4 = a4, code = code_stand(), zeitpunkt = Sys.time()), out)
message("gespeichert: ", out)
print(a8$pfad[maxsteps >= 36])
cat("Wurzel:", wurzel, "\n")
print(a1[, .(faktor = median(se_korrekt / se_pp)), by = .(testsatz, L)])
print(a4$draws[, .(mean = mean(theta), sd = sd(theta), n = .N), by = fall])
print(unique(a4$posterior[, .(fall, prior, mean, sd)]))
