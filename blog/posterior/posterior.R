# Pictures of the posterior grid (EAP and plausible values):
#   pv_vorher_nachher  draws of one replicated response pattern against the
#                      exact posterior, PP 0.6.4-1 and new
#   gitter_zwei_stufen the posterior on the nodes of the two stages of the grid
#   eap_genauigkeit    error of EAP and PSD over the test length
#
#   Rscript blog/posterior/posterior.R [--label=block5]
#
# Reads and writes in blog/posterior/output/ (data: blog/posterior/posterior_daten.R).

suppressPackageStartupMessages(library(ggplot2))
source("blog/gemeinsam.R")
args <- blog_args()
ordner <- "blog/posterior/output"
d <- readRDS(file.path(ordner, sprintf("posterior_%s.rds", args$label)))
dunkel <- "#1F2328"
blau <- "#0969DA"
stand <- sprintf("Stand: %s.", gsub("`", "", d$code))
versionen <- c("PP 0.6.4-1", "neu")
farben <- c("PP 0.6.4-1" = akzent, "neu" = blau)

# 10^-k with superscript digits
hoch <- function(e) chartr("0123456789-", "⁰¹²³⁴⁵⁶⁷⁸⁹⁻", as.character(e))
hoch10 <- function(v) {
  e <- round(log10(v))
  ifelse(e == 0, "1", paste0("10", hoch(e)))
}

# --- pv_vorher_nachher -------------------------------------------------------

pv <- d$pv
dr <- copy(pv$draws)
dr[, `:=`(fall = factor(fall, levels = pv$faelle), version = factor(version, levels = versionen))]
post <- copy(pv$posterior)[theta > -4.5 & theta < 3.5]
post[, fall := factor(fall, levels = pv$faelle)]
post <- rbindlist(lapply(versionen, function(v) cbind(post, version = factor(v, levels = versionen))))
kenn <- dr[, .(mean = mean(theta), sd = sd(theta)), by = .(fall, version)]
exakt <- unique(pv$posterior[, .(fall, m_ex = mean, sd_ex = sd)])
kenn <- merge(kenn, exakt, by = "fall")
kenn[, fall := factor(fall, levels = pv$faelle)]
kenn[, text := sprintf("Draws: Mittel %s, SD %s\nexakt:  Mittel %s, SD %s", zahl_de(mean), zahl_de(sd),
                       zahl_de(m_ex), zahl_de(sd_ex))]

p_pv <- ggplot(dr, aes(theta)) +
  geom_histogram(aes(y = after_stat(density), fill = version), binwidth = 0.1, boundary = 0,
                 alpha = 0.5, colour = NA) +
  geom_line(data = post, aes(theta, dichte), colour = dunkel, linewidth = 0.6) +
  geom_text(data = kenn, aes(x = -4.4, y = Inf, label = text, colour = version), hjust = 0, vjust = 1.3,
            size = 3, family = "Open Sans", lineheight = 0.95) +
  scale_fill_manual(values = farben, guide = "none") +
  scale_colour_manual(values = farben, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
  facet_grid(version ~ fall) +
  coord_cartesian(xlim = c(-4.5, 3.5)) +
  labs(
    title = "Plausible Values kommen jetzt aus dem Posterior",
    subtitle = sprintf("%s · Balken: Draws, Linie: exakter Posterior der Person", pv$beschreibung),
    x = "θ", y = "Dichte",
    caption = stand
  ) +
  theme_blog() +
  theme(strip.text.y = element_text(angle = 0, hjust = 0), panel.spacing = unit(1, "lines"))
speichere_plot(p_pv, sprintf("pv_vorher_nachher_%s", args$label), width = 12, height = 6.5, dir = ordner)

# --- gitter_zwei_stufen ------------------------------------------------------

gi <- d$gitter
faelle_g <- gi$info$fall
info <- copy(gi$info)
# visible range per panel: the whole short test; around the peak of the long
# test; from the end of the stage-1 grid to the posterior far outside
bereich <- data.table(
  fall = faelle_g,
  lo = c(-4, info$eap[2] - 0.35, 5.6),
  hi = c(2.5, info$eap[2] + 0.35, info$eap[3] + 0.35)
)
kn <- merge(gi$knoten, bereich, by = "fall")[theta >= lo & theta <= hi]
kn[, `:=`(dichte = gewicht / h, stufe = factor(sprintf("Stufe %d", stufe), levels = c("Stufe 1", "Stufe 2")))]
ku <- merge(gi$kurve, bereich, by = "fall")[theta >= lo & theta <= hi]
rand <- merge(gi$knoten[stufe == 1L], bereich, by = "fall")[theta >= lo & theta <= hi]
info[, text := c(
  sprintf("Posterior-SD %s: %d Knoten tragen Gewicht.\nStufe 1 reicht.", zahl_de(psd[1]), knoten_mit_gewicht[1]),
  sprintf("Posterior-SD %s: nur %d Knoten unter der Spitze.\nStufe 2 legt 201 Knoten darüber.",
          zahl_de(psd[2]), knoten_mit_gewicht[2]),
  sprintf("Das Gitter für den Prior endet bei 6.\nStufe 2 sucht den Modus (%s)\nund legt das Fenster dorthin.",
          zahl_de(modus[3]))
)]
info <- merge(info, bereich, by = "fall")
for (dt in list(kn, ku, rand, info)) dt[, fall := factor(fall, levels = faelle_g)]

p_g <- ggplot() +
  geom_segment(data = kn, aes(x = theta, xend = theta, y = 0, yend = dichte, colour = stufe),
               linewidth = 0.5) +
  geom_point(data = kn[stufe == "Stufe 1"], aes(theta, dichte, colour = stufe), size = 1.2) +
  geom_line(data = ku, aes(theta, dichte), colour = dunkel, linewidth = 0.5) +
  geom_rug(data = rand, aes(x = theta), colour = grau, length = unit(0.02, "npc"), linewidth = 0.3) +
  geom_text(data = info, aes(x = lo, y = Inf, label = text), hjust = 0, vjust = 1.3, size = 3,
            colour = dunkel, family = "Open Sans", lineheight = 0.95) +
  scale_colour_manual(values = c("Stufe 1" = grau, "Stufe 2" = blau), name = NULL,
                      labels = c("Stufe 1: ein Gitter für alle Personen, Abstand 0,05",
                                 "Stufe 2: Fenster um den Modus, 201 Knoten")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.45))) +
  facet_wrap(~ fall, scales = "free", ncol = 3) +
  labs(
    title = "Ein Gitter für alle, ein feines Fenster wo nötig",
    subtitle = "Posterior einer Person auf den Knoten (Höhe = Gewicht / Knotenabstand), Linie: exakter Posterior, Striche unten: Knoten von Stufe 1",
    x = "θ", y = "Posterior-Dichte",
    caption = stand
  ) +
  theme_blog() +
  theme(panel.spacing = unit(1.2, "lines"))
speichere_plot(p_g, sprintf("gitter_zwei_stufen_%s", args$label), width = 13, height = 5.2, dir = ordner)

# --- eap_genauigkeit ---------------------------------------------------------

boden <- 1e-16
ea <- melt(d$eap, id.vars = c("items", "version", "personen", "nan", "psd_median"),
           measure.vars = c("fehler_eap", "fehler_psd"), variable.name = "groesse", value.name = "fehler")
ea[, groesse := factor(fifelse(groesse == "fehler_eap", "EAP (Posterior-Mittel)", "SE (Posterior-SD)"),
                       levels = c("EAP (Posterior-Mittel)", "SE (Posterior-SD)"))]
ea[, `:=`(version = factor(version, levels = versionen), abstand = pmax(fehler, boden))]
nan <- ea[nan > 0]
aufzaehlung <- function(v) sub(", ([^,]*)$", " und \\1", paste(v, collapse = ", "))
nan_n <- unique(nan[, .(items, nan)])
nan_text <- sprintf("×: PP 0.6.4-1 gibt NaN\nbei %s Items\nfür %s von %d Personen",
                    aufzaehlung(nan_n$items), aufzaehlung(nan_n$nan), nan$personen[1])
top <- 1

p_e <- ggplot(ea[!is.na(fehler)], aes(items, abstand, colour = version)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  geom_point(data = nan, aes(y = top), shape = 4, size = 2.6, stroke = 1) +
  annotate("text", x = 2000, y = 1e-6, label = nan_text, hjust = 1, size = 3, colour = akzent,
           family = "Open Sans", lineheight = 0.95) +
  annotate("text", x = 10, y = boden, label = "Rechengenauigkeit", hjust = 0, vjust = -0.6, size = 3,
           colour = grau, family = "Open Sans") +
  scale_colour_manual(values = farben, name = NULL,
                      labels = c("PP 0.6.4-1: 60 Knoten, Produkt der Wahrscheinlichkeiten",
                                 "neu: Gitter mit Log-Wahrscheinlichkeiten")) +
  scale_x_log10(breaks = c(10, 20, 50, 100, 200, 500, 1000, 2000)) +
  scale_y_log10(breaks = 10^c(0, -3, -6, -9, -12, -15), labels = hoch10) +
  coord_cartesian(ylim = c(boden / 5, top * 3)) +
  facet_wrap(~ groesse, nrow = 1) +
  labs(
    title = "Auch bei langen Tests bleibt der EAP exakt",
    subtitle = sprintf("Größte Abweichung vom exakten Posterior bei %d Personen, Rasch-Items von −2 bis 2",
                       d$eap$personen[1]),
    x = "Zahl der Items", y = "größte Abweichung",
    caption = stand
  ) +
  theme_blog()
speichere_plot(p_e, sprintf("eap_genauigkeit_%s", args$label), width = 10, height = 5.4, dir = ordner)
