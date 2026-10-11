# Measures the status matrix "What works?": every cell compares the installed
# PP with the references from tests/testthat/helper-2-reference.R.
#
#   Rscript blog/status_daten.R [--label=block1]
#
# Writes blog/output/status_<label>.rds (one row per cell). The status is
# measured, not set:
#   ok         all persons (or statistics) within the tolerance
#   teilweise  some persons wrong or NaN (at most 20 %, at least 5 compared)
#   falsch     more than that
#   abbruch    the call fails with an error
#   fehlt      not offered by this PP version (not run)
# Known causes of deviating cells are in `ids_bekannt`. The tables show the
# cause in words; a deviating cell without a known cause is reported on the
# console (ids = "neu?").

source("blog/gemeinsam.R")
lade_test_helfer()
args <- blog_args()

# --- comparison ----------------------------------------------------------

# Compares PP values with reference values. A reference at the bound of its
# range (|theta| = 10: no finite estimate exists) only requires a non-missing
# value from PP.
vergleich <- function(ist, soll, tol, bound = rep(FALSE, length(soll))) {
  ist <- as.numeric(ist)
  soll <- as.numeric(soll)
  schlecht <- ifelse(bound, is.na(ist),
              ifelse(is.na(soll), !is.na(ist),
              ifelse(is.infinite(soll), is.na(ist) | ist != soll,
                     is.na(ist) | abs(ist - soll) > tol)))
  fin <- !bound & is.finite(ist) & is.finite(soll)
  list(n = length(soll), n_schlecht = sum(schlecht), n_nan = sum(schlecht & is.na(ist)),
       abw = if (any(fin)) max(abs(ist[fin] - soll[fin])) else NA_real_)
}

zellen <- list()

# Registers one cell. `pruefung` returns a vergleich() result, or NULL if the
# function is not offered.
zelle <- function(tabelle, zeile, spalte, mass, pruefung) {
  t0 <- Sys.time()
  r <- tryCatch(suppressWarnings(pp_quiet(pruefung())), error = function(e) e)
  status <- if (inherits(r, "error")) "abbruch" else if (is.null(r)) "fehlt" else
    if (r$n_schlecht == 0) "ok" else
    if (r$n >= 5 && r$n_schlecht <= 0.2 * r$n) "teilweise" else "falsch"
  zellen[[length(zellen) + 1]] <<- data.table(
    tabelle = tabelle, zeile = zeile, spalte = spalte, mass = mass, status = status,
    n = if (is.list(r) && !inherits(r, "error")) r$n else NA_integer_,
    n_schlecht = if (is.list(r) && !inherits(r, "error")) r$n_schlecht else NA_integer_,
    n_nan = if (is.list(r) && !inherits(r, "error") && !is.null(r$n_nan)) r$n_nan else 0L,
    abw = if (is.list(r) && !inherits(r, "error")) r$abw else NA_real_,
    meldung = if (inherits(r, "error")) conditionMessage(r) else NA_character_,
    sekunden = as.numeric(Sys.time() - t0, units = "secs"))
  message(sprintf("%-14s %-38s %-10s %-6s %s", tabelle, zeile, spalte, mass, status))
}

tol_theta <- 0.01   # practically relevant on the theta scale
tol_se <- 0.01
tol_eap <- 0.01
tol_jk <- 0.02      # pseudovalues amplify estimation errors by about L
tol_fit <- 1e-4

modelle <- c("1PL" = "1PL", "2PL" = "2PL", "3PL" = "3PL", "4PL" = "4PL",
             "PCM" = "PCM", "GPCM" = "GPCM", "gemischt" = "mixed")
methoden <- c(MLE = "mle", WLE = "wle", MAP = "map", EAP = "eap", Robust = "robust")

# --- table 1: estimators (model x method, theta and SE) ---------------------

for (mo in names(modelle)) {
  s <- sets_all[[modelle[[mo]]]]
  for (me in names(methoden)) {
    ty <- methoden[[me]]
    ref <- if (ty == "eap") ref_eap(s$X, s$items) else ref_estimates(s$X, s$items, ty)
    bound <- abs(ref[, 1]) %in% 10
    res <- NULL
    tol <- if (ty == "eap") tol_eap else tol_theta
    zelle("schaetzer", mo, me, "theta", function() {
      res <<- est(pp_fit(s, ty))
      vergleich(res[, 1], ref[, 1], tol, bound)
    })
    zelle("schaetzer", mo, me, "se", function() {
      if (is.null(res)) stop("no estimate")
      ok <- !bound
      vergleich(res[ok, 2], ref[ok, 2], if (ty == "eap") tol_eap else tol_se)
    })
  }
}

# --- table 2: downstream functions (function x model) ----------------------

spalten2 <- c("2PL" = "2PL", "PCM" = "PCM", "GPCM" = "GPCM", "gemischt" = "mixed")

ref_jk <- function(items, X, type) {
  r <- lapply(seq_len(nrow(X)), function(i) ref_jackknife(X[i, ], items, type))
  list(est = cbind(vapply(r, `[[`, 0, "estimate"), vapply(r, `[[`, 0, "se")),
       loo = t(vapply(r, `[[`, numeric(ncol(X)), "loo")))
}

for (sp in names(spalten2)) {
  s <- sets_all[[spalten2[[sp]]]]
  X <- s$X[1:8, ]
  jk_ref <- ref_jk(s$items, X, "wle")
  jk <- NULL
  zelle("folge", "JKpp: Leave-one-out-Schätzer (WLE)", sp, "loo", function() {
    jk <<- JKpp(pp_fit(s, "wle", X = X), fullmat = TRUE)
    bad <- rowSums(abs(jk$jk_mat - jk_ref$loo) > tol_theta | is.na(jk$jk_mat))
    list(n = nrow(X), n_schlecht = sum(bad > 0), abw = max(abs(jk$jk_mat - jk_ref$loo), na.rm = TRUE))
  })
  zelle("folge", "JKpp: JK-Schätzer (WLE)", sp, "theta", function() {
    vergleich(jk$resjk[, 1], jk_ref$est[, 1], tol_jk)
  })
  zelle("folge", "JKpp: JK-Standardfehler (WLE)", sp, "se", function() {
    vergleich(jk$resjk[, 2], jk_ref$est[, 2], tol_jk)
  })
  zelle("folge", "JKpp: JK-Schätzer (EAP)", sp, "theta", function() {
    Xe <- s$X[1:6, ]
    r <- ref_jk(s$items, Xe, "eap")
    vergleich(JKpp(pp_fit(s, "eap", X = Xe))$resjk[, 1], r$est[, 1], tol_jk)
  })

  # PV: one response pattern replicated, pooled draws vs. the posterior,
  # tolerance 4 Monte Carlo SEs; compared are mean and SD
  post <- ref_posterior(s$X[3, ], s$items)
  pv_vergleich <- function(draws) {
    m <- length(draws)
    a <- vergleich(mean(draws), post$mean, 4 * post$sd / sqrt(m))
    b <- vergleich(sd(as.vector(draws)), post$sd, 4 * post$sd / sqrt(2 * m))
    list(n = 2L, n_schlecht = a$n_schlecht + b$n_schlecht, abw = max(a$abw, b$abw))
  }
  Xr <- matrix(s$X[3, ], 2000, ncol(s$X), byrow = TRUE)
  zelle("folge", "PV: Ziehung aus dem Posterior", sp, "pv", function() {
    set.seed(31)
    pv_vergleich(PV(pp_fit(s, "eap", X = Xr), npv = 20, approx = FALSE)$pvdraws)
  })
  zelle("folge", "PV: Normal-Approximation (EAP-Objekt)", sp, "pv", function() {
    set.seed(32)
    pv_vergleich(PV(pp_fit(s, "eap", X = Xr[1:1000, ]), npv = 20, approx = TRUE)$pvdraws)
  })

  # person fit at PP's own WLE
  obj <- pp_fit(s, "wle")
  th <- est(obj)[, 1]
  ref_fit <- function(f) vapply(seq_len(nrow(s$X)), function(i) f(th[i], s$X[i, ]), 0)
  dich <- s$kind == "4pl"
  zelle("folge", "Pfit: lz", sp, "fit", function() {
    if (s$kind == "gpcm") return(NULL)   # PP offers only infit/outfit for the GPCM
    vergleich(Pfit(s$X, obj, fitindices = "lz")$lz[, "lz"],
              ref_fit(function(t, x) ref_lz(t, x, s$items)), tol_fit)
  })
  zelle("folge", "Pfit: lz*", sp, "fit", function() {
    if (s$kind == "gpcm") return(NULL)
    vergleich(Pfit(s$X, obj, fitindices = "lzstar")$lzstar[, "lzstar"],
              ref_fit(function(t, x) ref_lzstar(t, x, s$items, "wle")), tol_fit)
  })
  zelle("folge", "Pfit: Infit/Outfit", sp, "fit", function() {
    pf <- Pfit(s$X, obj, fitindices = c("infit", "outfit"))
    a <- vergleich(pf$infit[, "infit"], ref_fit(function(t, x) ref_infit(t, x, s$items)[1]), tol_fit)
    b <- vergleich(pf$outfit[, "outfit"], ref_fit(function(t, x) ref_outfit(t, x, s$items)[1]), tol_fit)
    list(n = a$n, n_schlecht = max(a$n_schlecht, b$n_schlecht), abw = max(a$abw, b$abw))
  })

  # simulation: frequencies vs. model probabilities, |z| < 4 per item and category
  zelle("folge", "Simulation", sp, "sim", function() {
    if (s$kind == "mixed") return(NULL)
    p <- items_to_pp(s$items)
    theta <- rep(c(-1, 0, 1), length.out = 3000)
    set.seed(33)
    Xs <- if (dich) sim_4pl(p$b, p$slopes, p$lowerA, p$upperA, theta) else
      sim_gpcm(p$thres, p$slopes, theta)
    z <- unlist(lapply(seq_along(s$items), function(j) {
      P <- ref_catprob(s$items[[j]], theta)
      e <- colSums(P)
      (tabulate(Xs[, j] + 1, ncol(P)) - e) / sqrt(e)
    }))
    list(n = length(z), n_schlecht = sum(abs(z) > 4), abw = max(abs(z)))
  })
}

# --- table 3: special cases (case x function) ------------------------------

s <- set_2pl
n <- nrow(s$X)

# missing responses (adaptive test)
Xna <- make_na(s$X, 7)
fall <- "Fehlende Antworten (adaptiv)"
zelle("sonder", fall, "Schätzer", "theta", function() {
  vergleich(est(pp_fit(s, "wle", X = Xna))[, 1], ref_estimates(Xna, s$items, "wle")[, 1], tol_theta)
})
zelle("sonder", fall, "EAP", "theta", function() {
  vergleich(est(pp_fit(s, "eap", X = Xna)), ref_eap(Xna, s$items), tol_eap)
})
zelle("sonder", fall, "JKpp", "theta", function() {
  Xj <- Xna[1:8, ]
  vergleich(JKpp(pp_fit(s, "wle", X = Xj))$resjk[, 1], ref_jk(s$items, Xj, "wle")$est[, 1], tol_jk)
})
zelle("sonder", fall, "PV", "pv", function() {
  set.seed(34)
  d <- PV(pp_fit(s, "eap", X = Xna), npv = 3, approx = FALSE)$pvdraws
  list(n = nrow(Xna), n_schlecht = sum(!is.finite(rowSums(d))), n_nan = sum(!is.finite(rowSums(d))),
       abw = NA_real_)
})
zelle("sonder", fall, "Pfit", "fit", function() {
  obj <- pp_fit(s, "wle", X = Xna)
  th <- est(obj)[, 1]
  vergleich(Pfit(Xna, obj, fitindices = "lzstar")$lzstar[, "lzstar"],
            vapply(seq_len(n), function(i) ref_lzstar(th[i], Xna[i, ], s$items, "wle"), 0), tol_fit)
})

# a person without any answer (row 2)
X0 <- s$X
X0[2, ] <- NA
fall <- "Person ohne Antworten"
zelle("sonder", fall, "Schätzer", "theta", function() {
  vergleich(est(pp_fit(s, "mle", X = X0))[, 1], ref_estimates(X0, s$items, "mle")[, 1], tol_theta)
})
zelle("sonder", fall, "EAP", "theta", function() {
  vergleich(est(pp_fit(s, "eap", X = X0)), ref_eap(X0, s$items), tol_eap)
})

# scalar prior
fall <- "Ein Prior für alle (μ = 0,5, σ² = 2)"
zelle("sonder", fall, "Schätzer", "theta", function() {
  vergleich(est(pp_fit(s, "map", mu = 0.5, sigma2 = 2))[, 1],
            ref_estimates(s$X, s$items, "map", 0.5, 2)[, 1], tol_theta)
})
zelle("sonder", fall, "EAP", "theta", function() {
  vergleich(est(pp_fit(s, "eap", mu = 0.5, sigma2 = 2))[, 1], ref_eap(s$X, s$items, 0.5, 2)[, 1], tol_eap)
})

# person-specific priors
fall <- "Personenspezifische Priors"
mu <- prior_mu(n)
s2 <- prior_s2(n)
zelle("sonder", fall, "Schätzer", "theta", function() {
  vergleich(est(pp_fit(s, "map", mu = mu, sigma2 = s2))[, 1],
            ref_estimates(s$X, s$items, "map", mu, s2)[, 1], tol_theta)
})
zelle("sonder", fall, "EAP", "theta", function() {
  vergleich(est(pp_fit(s, "eap", mu = mu, sigma2 = s2)), ref_eap(s$X, s$items, mu, s2), tol_eap)
})
zelle("sonder", fall, "PV", "pv", function() {
  post <- ref_posterior(s$X[3, ], s$items, 2, 1)
  Xr <- matrix(s$X[3, ], 300, 12, byrow = TRUE)
  set.seed(35)
  d <- PV(pp_fit(s, "eap", X = Xr, mu = rep(2, 300), sigma2 = rep(1, 300)), npv = 20, approx = FALSE)$pvdraws
  vergleich(mean(d), post$mean, 4 * post$sd / sqrt(length(d)))
})
zelle("sonder", fall, "Pfit", "fit", function() {
  Xp <- s$X[1:8, ]
  pf <- Pfit(Xp, pp_fit(s, "map", X = Xp, mu = mu[1:8], sigma2 = s2[1:8]), fitindices = "lz", SE = TRUE)
  vergleich(pf$lz[, "lz_se"],
            vapply(1:8, function(i) ref_lz_se(Xp[i, ], s$items, "map", mu[i], s2[i]), 0), 1e-3)
})

# duplicate in front with killdupli
fall <- "Duplikat vorne mit killdupli"
Xd <- rbind(s$X[1, ], s$X)
zelle("sonder", fall, "Schätzer", "theta", function() {
  m <- nrow(Xd)
  vergleich(est(pp_fit(s, "map", X = Xd, mu = prior_mu(m), sigma2 = prior_s2(m),
                       ctrl = list(killdupli = TRUE)))[, 1],
            ref_estimates(Xd, s$items, "map", prior_mu(m), prior_s2(m))[, 1], tol_theta)
})
zelle("sonder", fall, "JKpp", "theta", function() {
  Xj <- Xd[1:9, ]
  vergleich(JKpp(pp_fit(s, "wle", X = Xj, ctrl = list(killdupli = TRUE)))$resjk[, 1],
            ref_jk(s$items, Xj, "wle")$est[, 1], tol_jk)
})

# extreme item
fall <- "Extremes Item (b = ±40, Slope 20)"
zelle("sonder", fall, "Schätzer", "theta", function() {
  vergleich(est(pp_fit(set_extreme, "map"))[, 1],
            ref_estimates(set_extreme$X, set_extreme$items, "map")[, 1], tol_theta)
})
zelle("sonder", fall, "EAP", "theta", function() {
  vergleich(est(pp_fit(set_extreme, "eap")), ref_eap(set_extreme$X, set_extreme$items), tol_eap)
})

# 3PL persons at the guessing level (benchmark data)
fall <- "3PL am Rateniveau (Bench-Daten)"
zelle("sonder", fall, "Schätzer", "theta", function() {
  r40 <- est(pp_fit(osc3pl, "mle", maxsteps = 40))[, 1]
  r41 <- est(pp_fit(osc3pl, "mle", maxsteps = 41))[, 1]
  schlecht <- is.na(r40) | is.na(r41) | abs(r40 - r41) > tol_theta
  list(n = length(r40), n_schlecht = sum(schlecht), n_nan = sum(is.na(r40) | is.na(r41)),
       abw = max(abs(r40 - r41), na.rm = TRUE))
})

# very long test
fall <- "Sehr langer Test (1100 Items)"
zelle("sonder", fall, "EAP", "theta", function() {
  vergleich(est(pp_fit(set_long, "eap")), ref_eap(set_long$X, set_long$items), tol_eap)
})

# categories >= 10 with killdupli
fall <- "Kategorien ≥ 10 mit killdupli"
zelle("sonder", fall, "Schätzer", "theta", function() {
  vergleich(est(pp_fit(set_gpcm12, "wle", ctrl = list(killdupli = TRUE)))[, 1],
            ref_estimates(set_gpcm12$X, set_gpcm12$items, "wle")[, 1], tol_theta)
})

# --- known causes per cell --------------------------------------------------

# Known causes of deviating cells. `ids` (problem IDs) stay internal; the
# tables show `grund` as a footnote at the column label ("spalte"), the row
# label ("zeile") or the cell itself ("zelle").
ids_bekannt <- rbindlist(list(
  data.table(tabelle = "folge", zeile = "Pfit: Infit/Outfit", spalte = "GPCM", mass = "*", ids = "A10",
             ort = "zelle", grund = "Rechnet mit Slope 1 statt mit den Slopes der Items."),
  data.table(tabelle = "folge", zeile = c("Pfit: lz", "Pfit: lz*", "Pfit: Infit/Outfit"),
             spalte = "gemischt", mass = "*", ids = "G6", ort = "zelle",
             grund = "Für gemischte Tests gibt es keine Fit-Funktion."),
  data.table(tabelle = "sonder", zeile = "Fehlende Antworten (adaptiv)", spalte = "Pfit",
             mass = "*", ids = "A9", ort = "zeile",
             grund = "lz* ist bei fehlenden Antworten falsch."),
  data.table(tabelle = "sonder", zeile = "Personenspezifische Priors", spalte = "Pfit", mass = "*",
             ids = "A6", ort = "zeile",
             grund = "Die Jackknife-SE von lz rechnet mit der Standardabweichung des Priors statt mit seiner Varianz.")
))

st <- rbindlist(zellen)
st[, c("ids", "ort", "grund") := NA_character_]
for (i in seq_len(nrow(ids_bekannt))) {
  k <- ids_bekannt[i]
  hit <- st$tabelle == k$tabelle & (k$zeile == "*" | st$zeile == k$zeile) &
    (k$spalte == "*" | st$spalte == k$spalte) & (k$mass == "*" | st$mass == k$mass)
  st[hit, c("ids", "ort", "grund") := .(k$ids, k$ort, k$grund)]
}
st[status == "ok", c("ids", "ort", "grund") := NA_character_]
st[status %in% c("teilweise", "falsch", "abbruch") & is.na(ids), ids := "neu?"]
st[, `:=`(label = args$label, code = code_stand(), zeitpunkt = Sys.time())]

dir.create(blog_out, showWarnings = FALSE, recursive = TRUE)
out <- file.path(blog_out, sprintf("status_%s.rds", args$label))
saveRDS(st, out)
message("gespeichert: ", out)
print(st[, .N, by = .(tabelle, status)])
if (any(st$ids == "neu?", na.rm = TRUE)) {
  message("Zellen mit Abweichung ohne bekannte ID:")
  print(st[ids == "neu?", .(tabelle, zeile, spalte, mass, status, n_schlecht, abw, meldung)])
}
