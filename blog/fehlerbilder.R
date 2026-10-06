# Three pictures of bugs in PP 0.6.4-1: oscillating Newton (A8), jackknife
# SE (A1) and plausible values (A4). The problem IDs are not shown.
#
#   Rscript blog/fehlerbilder.R [--label=block1]
#
# Reads blog/output/fehlerbilder_<label>.rds (blog/fehlerbilder_daten.R) and
# writes fehler_newton, fehler_jackknife and fehler_pv (_<label>.png/.svg).

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})
source("blog/gemeinsam.R")
args <- blog_args()
d <- readRDS(file.path(blog_out, sprintf("fehlerbilder_%s.rds", args$label)))
dunkel <- "#1F2328"
blau <- "#0969DA"
stand <- sprintf("Stand: %s.", gsub("`", "", d$code))

# --- A8: oscillating Newton --------------------------------------------------

a8 <- d$a8
w <- a8$wurzel
zyklus <- unique(round(a8$pfad[maxsteps > 30, theta], 6))
f_an <- function(t) approx(a8$kurve$theta, a8$kurve$f, t)$y

p_pfad <- ggplot(a8$pfad, aes(maxsteps, theta)) +
  geom_hline(yintercept = w, colour = dunkel, linetype = "22") +
  geom_line(colour = akzent, linewidth = 0.4) +
  geom_point(colour = akzent, size = 1.6) +
  annotate("label", x = 41, y = w, label = sprintf("Lösung %s", zahl_de(w)), hjust = 1,
           size = 3.3, colour = dunkel, fill = "white", border.colour = "white", family = "Open Sans") +
  scale_x_continuous(breaks = c(1, 10, 20, 30, 40)) +
  labs(x = "maxsteps", y = "Ergebnis von PP (θ̂)", title = "Ergebnis je nach maxsteps") +
  theme_blog() +
  theme(plot.title = element_text(size = rel(1)))

p_kurve <- ggplot(a8$kurve, aes(theta, f)) +
  geom_hline(yintercept = 0, colour = grau, linewidth = 0.3) +
  geom_line(colour = dunkel, linewidth = 0.6) +
  geom_point(data = data.table(theta = w, f = 0), colour = dunkel, size = 2.6) +
  geom_point(data = data.table(theta = zyklus, f = f_an(zyklus)), colour = akzent, size = 2.6) +
  annotate("text", x = w, y = 0, label = "Nullstelle", vjust = 1.8, size = 3.3, colour = dunkel,
           family = "Open Sans") +
  labs(x = "θ", y = "Warm-Gleichung f(θ)", title = "Schätzgleichung mit den Werten aus PP") +
  theme_blog() +
  theme(plot.title = element_text(size = rel(1)))

p8 <- (p_pfad | p_kurve) +
  plot_annotation(
    title = "Das Newton-Verfahren pendelt",
    subtitle = sprintf("%s · PP springt zwischen %s und kommt nie an der Lösung an",
                       a8$beschreibung, paste(zahl_de(sort(zyklus)), collapse = ", ")),
    caption = paste("Orange: Ergebnisse von PP 0.6.4-1, schwarz: Referenz (Nullstelle der Warm-Gleichung per uniroot).", stand),
    theme = theme_blog()
  )
speichere_plot(p8, sprintf("fehler_newton_%s", args$label), width = 10, height = 4.4)

# --- A1: jackknife SE --------------------------------------------------------

a1 <- d$a1
linien <- unique(a1[, .(testsatz, L)])
farben <- setNames(c(akzent, blau), linien$testsatz)
lim <- c(0, max(a1$se_korrekt) * 1.05)

a1[, person := fifelse(extrem, "Extremscore", "übrige Personen")]
p1 <- ggplot(a1, aes(se_korrekt, se_pp, colour = testsatz)) +
  geom_abline(slope = 1, intercept = 0, colour = grau, linetype = "22") +
  geom_abline(data = linien, aes(slope = 1 / (L - 1), intercept = 0, colour = testsatz),
              linetype = "13", linewidth = 0.5) +
  annotate("text", x = lim[2] * 0.62, y = lim[2] * 0.62, label = "korrekt: y = x", angle = 0,
           hjust = -0.1, vjust = 1.4, size = 3.3, colour = grau, family = "Open Sans") +
  geom_point(aes(shape = person), size = 2.4, alpha = 0.85) +
  geom_text(data = linien, aes(x = lim[2], y = lim[2] / (L - 1), label = sprintf("y = x / %d", L - 1)),
            hjust = 1, vjust = -0.7, size = 3.3, family = "Open Sans", show.legend = FALSE) +
  scale_colour_manual(values = farben, name = NULL) +
  scale_shape_manual(values = c("übrige Personen" = 16, "Extremscore" = 2), name = NULL) +
  coord_cartesian(xlim = lim, ylim = lim) +
  labs(
    title = "Jackknife-Standardfehler um den Faktor L − 1 zu klein",
    subtitle = "JK-SE von PP gegen den Lehrbuch-Jackknife, WLE, eine Person je Punkt",
    x = "JK-SE korrekt (Referenz)", y = "JK-SE von PP 0.6.4-1",
    caption = paste("L = Zahl der Items. PP kombiniert die Leave-one-out-Schätzer mit der Formel für Pseudowerte.",
                    "Bei Extremscores (Dreiecke) liegen JK-Schätzer und Mittel der Leave-one-out-Schätzer weit auseinander,",
                    "dort weicht der Fehler anders ab.", stand, sep = "\n")
  ) +
  theme_blog()
speichere_plot(p1, sprintf("fehler_jackknife_%s", args$label), width = 8, height = 6.4)

# --- A4: plausible values ------------------------------------------------------

a4 <- d$a4
kenn <- a4$draws[, .(mean = mean(theta), sd = sd(theta)), by = fall]
post <- a4$posterior
post[, linie := fifelse(fall == "Prior N(2, 1)" & prior == "N(0, 1)",
                        "Posterior mit N(0, 1), den PP stattdessen nimmt", "exakter Posterior")]
korrekt <- unique(post[linie == "exakter Posterior", .(fall, mean, sd)])
facette <- setNames(
  sprintf("%s · PV: Mittel %s, SD %s · korrekt: %s, %s", kenn$fall, zahl_de(kenn$mean), zahl_de(kenn$sd),
          zahl_de(korrekt[match(kenn$fall, fall), mean]), zahl_de(korrekt[match(kenn$fall, fall), sd])),
  kenn$fall)

p4 <- ggplot(a4$draws, aes(theta)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.1, fill = akzent, alpha = 0.45,
                 colour = NA, boundary = 0) +
  geom_line(data = post[theta > -4 & theta < 2.5], aes(theta, dichte, linetype = linie),
            colour = dunkel, linewidth = 0.6) +
  scale_linetype_manual(values = c("exakter Posterior" = "solid",
                                   "Posterior mit N(0, 1), den PP stattdessen nimmt" = "22"),
                        name = NULL) +
  facet_wrap(~ fall, ncol = 1, labeller = as_labeller(facette)) +
  coord_cartesian(xlim = c(-4, 2.5)) +
  labs(
    title = "Plausible Values: zu breit und ohne Prior",
    subtitle = sprintf("%s · Histogramm: PV von PP 0.6.4-1", a4$beschreibung),
    x = "θ", y = "Dichte",
    caption = paste("Oben 40.000 Draws: der MH-Sampler speichert nur angenommene Sprünge und streut zu breit.",
                    "Unten 6.000 Draws mit Prior N(2, 1): PP zieht trotzdem aus dem Posterior mit N(0, 1).",
                    stand, sep = "\n")
  ) +
  theme_blog()
speichere_plot(p4, sprintf("fehler_pv_%s", args$label), width = 8, height = 7)
