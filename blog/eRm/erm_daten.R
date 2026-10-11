# Data for the table "PP und eRm": PPass on every eRm model against
# eRm::person.parameter() (maximum likelihood), plus a model with group
# contrasts, for which eRm gives no person parameters.
#
#   Rscript blog/eRm/erm_daten.R [--label=block8]
#
# Writes blog/eRm/output/erm_<label>.rds (table: blog/eRm/erm.R).

source("blog/gemeinsam.R")
suppressPackageStartupMessages({
  library(PP)
  library(eRm)
})
args <- blog_args()
ordner <- "blog/eRm/output"
dir.create(ordner, showWarnings = FALSE, recursive = TRUE)

set.seed(4711)
thres <- rbind(0, matrix(c(-1.5, 0, 1, -1, 0.5, NA, -0.5, 1, NA, 0, 0.8, NA, -2, -0.2, NA,
                           0.4, 1.2, NA), 3))
poly <- function(n, shift = 0) sim_gpcm(thres = thres, alpha = rep(1, 6), theta = rnorm(n, shift))
rsm <- function(n) {
  th <- rbind(0, outer(c(-0.8, 0.8), seq(-1, 1, length.out = 6), "+"))
  sim_gpcm(thres = th, alpha = rep(1, 6), theta = rnorm(n))
}
dich <- function(n, shift = 0) sim_4pl(beta = seq(-1.5, 1.5, length.out = 8), alpha = rep(1, 8),
                                      lowerA = rep(0, 8), upperA = rep(1, 8), theta = rnorm(n, shift))
n <- 500
d <- dich(n)
d[1:100, 7:8] <- NA                          # 100 persons without the last two items
d2 <- cbind(dich(n), dich(n, 0.5))           # two time points
p <- poly(n)
p2 <- cbind(poly(n), poly(n, 0.5))
r <- rsm(n)
# with two time points eRm::LRSM fails on data simulated from a rating scale
# model; uniform random responses work
r2 <- matrix(sample(0:2, n * 12, replace = TRUE), n)

still <- function(expr) suppressWarnings(suppressMessages(expr))
modelle <- list(
  list(kurz = "RM", art = "dichotom", design = "mit fehlenden Antworten", fit = function() RM(d)),
  list(kurz = "LLTM", art = "dichotom", design = "",
       fit = function() LLTM(d, W = cbind(seq(-1, 1, length.out = 8), rep(0:1, 4)))),
  list(kurz = "RSM", art = "polytom", design = "", fit = function() RSM(r)),
  list(kurz = "PCM", art = "polytom", design = "", fit = function() PCM(p)),
  list(kurz = "LRSM", art = "polytom", design = "", fit = function() LRSM(r)),
  list(kurz = "LPCM", art = "polytom", design = "", fit = function() LPCM(p)),
  list(kurz = "LLTM", art = "dichotom", design = "2 Zeitpunkte", fit = function() LLTM(d2, mpoints = 2)),
  list(kurz = "LRSM", art = "polytom", design = "2 Zeitpunkte", fit = function() LRSM(r2, mpoints = 2)),
  list(kurz = "LPCM", art = "polytom", design = "2 Zeitpunkte", fit = function() LPCM(p2, mpoints = 2)),
  list(kurz = "LPCM", art = "polytom", design = "2 Zeitpunkte, 2 Gruppen",
       fit = function() LPCM(p2, mpoints = 2, groupvec = rep(1:2, n / 2)))
)

zeilen <- rbindlist(lapply(modelle, function(m) {
  o <- still(m$fit())
  res <- still(PPass(o, type = "mle", fitindices = "lzstar"))
  utils::capture.output(pt <- tryCatch(still(person.parameter(o))$theta.table,
                                       error = function(e) NULL))
  wle <- still(PPass(o))
  zeile <- data.table(modell = m$kurz, art = m$art, design = m$design, personen = nrow(o$X),
                      items = ncol(o$X), virtuell = ncol(attr(res, "ppeo")$model$X),
                      erm_moeglich = !is.null(pt), verglichen = NA_integer_,
                      max_theta = NA_real_, max_se = NA_real_,
                      pp_geschaetzt = sum(res$status == "ok"),
                      wle_geschaetzt = sum(wle$status == "ok"),
                      auffaellig = sum(abs(wle$lzstar) > qnorm(0.975), na.rm = TRUE))
  if (!is.null(pt)) {
    i <- match(rownames(o$X), rownames(pt))
    sel <- res$status == "ok" & !pt[i, "Interpolated"]
    zeile[, `:=`(verglichen = sum(sel), max_theta = max(abs(res$estimate - pt[i, 1])[sel]),
                 max_se = max(abs(res$SE - pt[i, 2])[sel]))]
  }
  zeile
}))
zeilen[, `:=`(label = args$label, code = code_stand(), zeitpunkt = Sys.time(),
              erm_version = as.character(packageVersion("eRm")))]
print(zeilen[, .(modell, design, verglichen, max_theta, max_se, pp_geschaetzt, wle_geschaetzt)])

out <- file.path(ordner, sprintf("erm_%s.rds", args$label))
saveRDS(zeilen, out)
message("gespeichert: ", out)
