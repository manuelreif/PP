# Data for the pictures of the posterior grid (EAP and plausible values).
#
#   Rscript blog/posterior/posterior_daten.R [--label=block5]
#
# Writes blog/posterior/output/posterior_<label>.rds, a list with
#   pv       draws of one replicated response pattern (2PL, 12 items) in three
#            cases, from PP 0.6.4-1 and from the installed PP, plus the exact
#            posterior. The old draws come from R/PV.R of the CRAN version
#            (commit 6bdbde5, read with git show); it reads $ipar and $resPP,
#            which the new object still provides.
#   gitter   three persons: their posterior on the nodes of stage 1 (one grid
#            for all persons) and, where PP uses it, of stage 2 (a fine window
#            around the mode), and the exact posterior curve
#   eap      the error of the EAP over the test length (Rasch items): the
#            60-node quadrature of PP 0.6.4-1 (R/eap.R, still in the package
#            for the old jackknife) and the grid, against the exact posterior
#
# Run from the package root against the installed PP.

source("blog/gemeinsam.R")
lade_test_helfer()
args <- blog_args()
ordner <- "blog/posterior/output"

# --- plausible values -------------------------------------------------------

alt <- new.env()
eval(parse(text = system("git show 6bdbde5:R/PV.R", intern = TRUE)), envir = alt)

s <- set_2pl
x3 <- s$X[3, ]
Xr <- matrix(x3, 2000, ncol(s$X), byrow = TRUE)
e0 <- pp_fit(s, "eap", X = Xr)
e2 <- pp_fit(s, "eap", X = Xr[1:300, ], mu = rep(2, 300), sigma2 = rep(1, 300))
w0 <- pp_fit(s, "wle", X = Xr)

ziehe <- function(obj, approx, seed) {
  set.seed(seed)
  a <- as.vector(alt$PV.fourpl(obj, npv = 20, approx = approx)$pvdraws)
  set.seed(seed)
  n <- as.vector(PV(obj, npv = 20, approx = approx)$pvdraws)
  rbind(data.table(version = "PP 0.6.4-1", theta = a), data.table(version = "neu", theta = n))
}
faelle_pv <- c("Ziehung, Prior N(0, 1)", "Ziehung, Prior N(2, 1)", "Normal-Approximation, WLE-Objekt")
pv_draws <- rbind(
  cbind(fall = faelle_pv[1], ziehe(e0, FALSE, 41)),
  cbind(fall = faelle_pv[2], ziehe(e2, FALSE, 42)),
  cbind(fall = faelle_pv[3], ziehe(w0, TRUE, 43))
)
dichte <- function(fall, mu) {
  p <- ref_posterior(x3, s$items, mu, 1)
  data.table(fall = fall, theta = p$grid, dichte = p$w / diff(p$grid[1:2]), mean = p$mean, sd = p$sd)
}
pv_post <- rbind(dichte(faelle_pv[1], 0), dichte(faelle_pv[2], 2), dichte(faelle_pv[3], 0))
pv <- list(draws = pv_draws, posterior = pv_post, faelle = faelle_pv,
           wle = w0$estimates[1, c("estimate", "SE")],
           beschreibung = "2PL, 12 Items, eine Antwortfolge repliziert, 20 PV je Person")

# --- the two stages of the grid ----------------------------------------------

# Posterior weights on the nodes g (prior N(0, 1))
gewichte <- function(x, items, g) {
  lp <- ref_loglik(g, x, items) + dnorm(g, log = TRUE)
  w <- exp(lp - max(lp))
  w / sum(w)
}
h1 <- 0.05
g1 <- seq(-6, 6, by = h1)   # stage 1 of PP for prior N(0, 1)

gitter_fall <- function(name, x, items) {
  pp <- PP:::pp_posterior_cpp(kernel_bank(items), matrix(as.integer(x), 1), 0, 1)
  lp <- function(t) ref_loglik(t, x, items) + dnorm(t, log = TRUE)
  modus <- optimize(lp, c(-15, 15), maximum = TRUE, tol = 1e-10)$maximum
  skala <- 1 / sqrt(ref_info(modus, x, items) + 1)
  w1 <- gewichte(x, items, g1)
  st1 <- data.table(fall = name, stufe = 1L, theta = g1, gewicht = w1, h = h1)
  # stage 2 as in pp_posterior_cpp: mode +- 10 Laplace SDs, 201 nodes
  if (pp$stage == 2L) {
    g2 <- seq(modus - 10 * skala, modus + 10 * skala, length.out = 201)
    w2 <- gewichte(x, items, g2)
    st2 <- data.table(fall = name, stufe = 2L, theta = g2, gewicht = w2, h = diff(g2[1:2]))
    eap_kontrolle <- sum(w2 * g2)
  } else {
    st2 <- NULL
    eap_kontrolle <- sum(w1 * g1)
  }
  kurve_g <- seq(modus - 8 * skala, modus + 8 * skala, length.out = 1500)
  kw <- exp(lp(kurve_g) - lp(modus))
  kurve <- data.table(fall = name, theta = kurve_g, dichte = kw / sum(kw) / diff(kurve_g[1:2]))
  info <- data.table(fall = name, stufe = pp$stage, eap = pp$eap, psd = pp$psd, modus = modus,
                     skala = skala, knoten_mit_gewicht = sum(w1 > 0.01 * max(w1)),
                     eap_kontrolle = eap_kontrolle)
  list(knoten = rbind(st1, st2), kurve = kurve, info = info)
}
items_aussen <- items_4pl(1, rep(9, 3000))
x_aussen <- sim_resp(9, items_aussen, 5)[1, ]
faelle_g <- list(
  gitter_fall("Kurzer Test: 12 Items", x3, s$items),
  gitter_fall("Langer Test: 1100 Items", set_long$X[3, ], set_long$items),
  gitter_fall("Jenseits des Gitters: 3000 Items bei b = 9", x_aussen, items_aussen)
)
gitter <- list(knoten = rbindlist(lapply(faelle_g, `[[`, "knoten")),
               kurve = rbindlist(lapply(faelle_g, `[[`, "kurve")),
               info = rbindlist(lapply(faelle_g, `[[`, "info")),
               h1 = h1)

# --- EAP error over the test length -------------------------------------------

# Exact posterior of Rasch items: the likelihood depends on the responses only
# through the raw score r, log L = r theta - sum_j x_j b_j - sum_j log(1 + exp(theta - b_j));
# the term sum_j x_j b_j cancels in the posterior. Grid distance 0.001.
exakt_rasch <- function(X, b) {
  g <- seq(-8, 8, by = 0.001)
  c_g <- numeric(length(g))
  for (blk in split(b, ceiling(seq_along(b) / 200))) {
    c_g <- c_g + rowSums(log1p(exp(outer(g, blk, "-"))))
  }
  t(vapply(rowSums(X), function(r) {
    lp <- r * g - c_g + dnorm(g, log = TRUE)
    w <- exp(lp - max(lp))
    w <- w / sum(w)
    m <- sum(w * g)
    c(m, sqrt(sum(w * (g - m)^2)))
  }, numeric(2)))
}

laengen <- c(10, 20, 50, 100, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 2000)
set.seed(5)
theta_p <- rnorm(25)
eap <- rbindlist(lapply(laengen, function(L) {
  b <- seq(-2, 2, length.out = L)
  X <- sim_resp(theta_p, items_4pl(1, b), 1000 + L)
  ex <- exakt_rasch(X, b)
  n <- nrow(X)
  alt_r <- PP:::eap_4pl(X, rbind(0, b), rep(1, L), lowerA = rep(0, L), upperA = rep(1, L),
                        mu = rep(0, n), sigma2 = rep(1, n))
  neu_r <- as.matrix(PP_4pl(X, b, type = "eap")$estimates[, c("estimate", "SE")])
  zeile <- function(version, r) {
    ok <- is.finite(r[, 1])
    data.table(items = L, version = version, personen = n, nan = sum(!ok),
               fehler_eap = if (any(ok)) max(abs(r[ok, 1] - ex[ok, 1])) else NA_real_,
               fehler_psd = if (any(ok)) max(abs(r[ok, 2] - ex[ok, 2])) else NA_real_,
               psd_median = median(ex[, 2]))
  }
  rbind(zeile("PP 0.6.4-1", alt_r), zeile("neu", neu_r))
}))

dir.create(ordner, showWarnings = FALSE, recursive = TRUE)
out <- file.path(ordner, sprintf("posterior_%s.rds", args$label))
saveRDS(list(pv = pv, gitter = gitter, eap = eap, code = code_stand(), zeitpunkt = Sys.time()), out)
message("gespeichert: ", out)

print(pv_draws[, .(mean = mean(theta), sd = sd(theta), n = .N), by = .(fall, version)])
print(unique(pv_post[, .(fall, mean, sd)]))
print(gitter$info)
stopifnot(gitter$info[, all(abs(eap - eap_kontrolle) < 1e-6)])
print(eap)
