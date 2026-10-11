# Data for the pictures of the plausible values on the level of a population.
#
#   Rscript blog/PV/pv_daten.R [--label=block5]
#
# Writes blog/PV/output/pv_<label>.rds, a list with
#   population  3000 persons from N(0, 1), 2PL with 12 items: true theta, WLE,
#               EAP and one PV per person: PP 0.6.4-1 on the WLE object
#               (default approx = TRUE: N(WLE, SE^2)), new on the EAP object
#               (PV() needs a prior)
#   gruppen     two groups with different means and person-specific priors
#               (background model), 2PL with 8 items: true theta and PVs from
#               the EAP object with approx = FALSE (PP 0.6.4-1: MH sampler,
#               new: grid posterior)
#
# The old code is R/PV.R of the CRAN version (commit 6bdbde5, read with git
# show); it reads $ipar (added by alt_objekt() from blog/gemeinsam.R) and $resPP.
# Run from the package root against the installed PP.

source("blog/gemeinsam.R")
lade_test_helfer()
args <- blog_args()
ordner <- "blog/PV/output"

alt <- new.env()
eval(parse(text = system("git show 6bdbde5:R/PV.R", intern = TRUE)), envir = alt)

# One draw per person from both versions with the same seed (obj_alt for the
# old code, obj_neu for the new). The old PV returns a transposed matrix for
# npv = 1, so both draw two and keep the first.
ein_pv <- function(obj_alt, obj_neu, approx, seed) {
  set.seed(seed)
  a <- alt$PV.fourpl(alt_objekt(obj_alt), npv = 2, approx = approx)$pvdraws[, 1]
  set.seed(seed)
  n <- PP::PV(obj_neu, npv = 2, approx = approx)$pvdraws[, 1]
  list(alt = a, neu = n)
}

# --- population: point estimates and PVs against the true distribution --------

set.seed(51)
n_pop <- 3000
theta <- rnorm(n_pop)
items <- items_4pl(c(0.9, 1.3, 1.1, 1.6, 0.8, 1.2, 1.4, 1, 1.5, 0.9, 1.2, 1.1),
                   round(seq(-2, 2, length.out = 12), 2))
X <- sim_resp(theta, items, 52)
satz <- list(items = items, kind = "4pl")
wle <- pp_fit(satz, "wle", X = X)
eap <- pp_fit(satz, "eap", X = X)
pv <- ein_pv(wle, eap, TRUE, 53)
population <- rbind(
  data.table(groesse = "wahr", theta = theta),
  data.table(groesse = "WLE", theta = wle$estimates$estimate),
  data.table(groesse = "EAP", theta = eap$estimates$estimate),
  data.table(groesse = "PV alt", theta = pv$alt),
  data.table(groesse = "PV neu", theta = pv$neu)
)

# --- two groups with a background model ----------------------------------------

set.seed(54)
n_gr <- 1500
gruppen_def <- data.table(gruppe = c("Gruppe A", "Gruppe B"), mu = c(-0.8, 0.8), sd = 0.6)
g <- rep(gruppen_def$gruppe, each = n_gr)
mu_p <- rep(gruppen_def$mu, each = n_gr)
s2_p <- rep(gruppen_def$sd^2, each = n_gr)
theta_g <- rnorm(2 * n_gr, mu_p, sqrt(s2_p))
items_g <- items_4pl(c(1.2, 0.9, 1.4, 1, 1.3, 0.8, 1.1, 1.5), round(seq(-1.5, 1.5, length.out = 8), 2))
Xg <- sim_resp(theta_g, items_g, 55)
eap_g <- pp_fit(list(items = items_g, kind = "4pl"), "eap", X = Xg, mu = mu_p, sigma2 = s2_p)
pv_g <- ein_pv(eap_g, eap_g, FALSE, 56)
gruppen <- list(
  personen = data.table(gruppe = g, theta = theta_g, pv_alt = pv_g$alt, pv_neu = pv_g$neu),
  def = gruppen_def,
  beschreibung = sprintf("2PL, 8 Items, je %d Personen, Prior je Person: Mittel und Streuung der eigenen Gruppe",
                         n_gr)
)

dir.create(ordner, showWarnings = FALSE, recursive = TRUE)
out <- file.path(ordner, sprintf("pv_%s.rds", args$label))
saveRDS(list(population = population, gruppen = gruppen,
             beschreibung_pop = sprintf("2PL, 12 Items, %d Personen aus N(0, 1), ein PV je Person", n_pop),
             code = code_stand(), zeitpunkt = Sys.time()), out)
message("gespeichert: ", out)

print(population[, .(mean = mean(theta), sd = sd(theta)), by = groesse])
print(gruppen$personen[, .(wahr = mean(theta), alt = mean(pv_alt), neu = mean(pv_neu),
                           sd_wahr = sd(theta), sd_alt = sd(pv_alt), sd_neu = sd(pv_neu)), by = gruppe])
