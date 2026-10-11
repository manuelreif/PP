# Pictures of the plausible values on the level of a population:
#   pv_population  WLE, EAP and PVs (PP 0.6.4-1 on the WLE object, new on the
#                  EAP object) against the true distribution of a population
#   pv_gruppen     two groups with person-specific priors: PVs of the MH
#                  sampler of PP 0.6.4-1 and of the grid posterior
#
#   Rscript blog/PV/pv.R [--label=block5]
#
# Reads and writes in blog/PV/output/ (data: blog/PV/pv_daten.R).

suppressPackageStartupMessages(library(ggplot2))
source("blog/gemeinsam.R")
args <- blog_args()
ordner <- "blog/PV/output"
d <- readRDS(file.path(ordner, sprintf("pv_%s.rds", args$label)))
dunkel <- "#1F2328"
blau <- "#0969DA"
schrift <- "Open Sans"
stand <- sprintf("Stand: %s.", gsub("`", "", d$code))

# --- pv_population ------------------------------------------------------------

po <- d$population
sd_wahr <- po[groesse == "wahr", sd(theta)]
panels <- c("WLE" = "WLE (Punktschätzer)", "EAP" = "EAP (Punktschätzer)",
            "PV alt" = "PP 0.6.4-1: PV() auf dem WLE-Objekt = N(WLE, SE²)",
            "PV neu" = "neu: PV() auf dem EAP-Objekt, Posterior mit Prior N(0, 1)")
fuell <- setNames(c(grau, grau, akzent, blau), panels)
ph <- po[groesse != "wahr"][, panel := factor(panels[groesse], levels = panels)]
kenn <- ph[, .(sd = sd(theta)), by = panel]
kenn[, text := sprintf("SD %s\nwahr %s", zahl_de(sd), zahl_de(sd_wahr))]
kurve <- data.table(theta = seq(-4.5, 4.5, by = 0.02))[, dichte := dnorm(theta)]
kurve <- rbindlist(lapply(panels, function(p) cbind(kurve, panel = factor(p, levels = panels))))

p_po <- ggplot(ph, aes(theta)) +
  geom_histogram(aes(y = after_stat(density), fill = panel), binwidth = 0.2, boundary = 0, alpha = 0.55,
                 colour = "white", linewidth = 0.2) +
  geom_line(data = kurve, aes(theta, dichte), colour = dunkel, linewidth = 0.7) +
  geom_text(data = kenn, aes(x = -4.4, y = Inf, label = text, colour = panel), hjust = 0, vjust = 1.4,
            size = 3.5, family = schrift, fontface = "bold", lineheight = 0.95) +
  scale_fill_manual(values = fuell, guide = "none") +
  scale_colour_manual(values = c(setNames(c(dunkel, dunkel), panels[1:2]), fuell[3:4]), guide = "none") +
  scale_x_continuous(labels = function(v) zahl_de(v, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  facet_wrap(~ panel, ncol = 2) +
  coord_cartesian(xlim = c(-4.5, 4.5)) +
  labs(
    title = "Plausible Values treffen die Verteilung der Population",
    subtitle = sprintf("%s · Linie: wahre Verteilung N(0, 1)", d$beschreibung_pop),
    x = "θ", y = "Dichte",
    caption = stand
  ) +
  theme_blog() +
  theme(panel.spacing = unit(1.4, "lines"))
speichere_plot(p_po, sprintf("pv_population_%s", args$label), width = 10, height = 7, dir = ordner)

# --- pv_gruppen ---------------------------------------------------------------

gr <- d$gruppen
gdef <- gr$def
versionen <- c("pv_alt" = "PP 0.6.4-1: MH-Sampler (approx = FALSE)", "pv_neu" = "neu: Ziehung aus dem Posterior-Gitter")
gf <- c("Gruppe A" = "#8250DF", "Gruppe B" = "#1A7F37")
pg <- melt(gr$personen, id.vars = c("gruppe", "theta"), measure.vars = names(versionen),
           variable.name = "version", value.name = "pv")
pg[, version := factor(versionen[as.character(version)], levels = versionen)]
mittel <- pg[, .(pv = mean(pv), sd = sd(pv), wahr = mean(theta)), by = .(version, gruppe)]
abstand <- mittel[, .(lo = pv[gruppe == "Gruppe A"], hi = pv[gruppe == "Gruppe B"],
                      sd_in = mean(sd)), by = version]
wahr_abstand <- diff(gdef$mu)
abstand[, text := sprintf("Abstand der Gruppen %s (wahr %s)\nSD in der Gruppe %s (wahr %s)",
                          zahl_de(hi - lo), zahl_de(wahr_abstand), zahl_de(sd_in), zahl_de(gdef$sd[1]))]
kurve_g <- rbindlist(lapply(seq_len(nrow(gdef)), function(i) {
  data.table(gruppe = gdef$gruppe[i], theta = seq(-3.5, 3.5, by = 0.02))[
    , dichte := dnorm(theta, gdef$mu[i], gdef$sd[i])]
}))
kurve_g <- rbindlist(lapply(versionen, function(v) cbind(kurve_g, version = factor(v, levels = versionen))))
y_pfeil <- max(kurve_g$dichte) * 1.42
pfeil <- arrow(length = unit(0.16, "cm"), ends = "both", type = "closed")

p_gr <- ggplot(pg, aes(pv)) +
  geom_histogram(aes(y = after_stat(density), fill = gruppe), binwidth = 0.15, boundary = 0, alpha = 0.4,
                 position = "identity", colour = NA) +
  geom_line(data = kurve_g, aes(theta, dichte, colour = gruppe), linewidth = 0.8) +
  geom_vline(data = mittel, aes(xintercept = pv, colour = gruppe), linewidth = 0.5, linetype = "22") +
  geom_segment(data = abstand, aes(x = lo, xend = hi, y = y_pfeil, yend = y_pfeil), colour = dunkel,
               linewidth = 0.5, arrow = pfeil) +
  geom_text(data = abstand, aes(x = 3.4, y = y_pfeil, label = text), hjust = 1, vjust = 0.5,
            size = 3.3, family = schrift, colour = dunkel, lineheight = 0.95) +
  scale_fill_manual(values = gf, name = NULL) +
  scale_colour_manual(values = gf, name = NULL) +
  scale_x_continuous(labels = function(v) zahl_de(v, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  facet_wrap(~ version, ncol = 1) +
  coord_cartesian(xlim = c(-3.5, 3.5), ylim = c(0, y_pfeil * 1.12)) +
  labs(
    title = "Mit Hintergrundmodell: Der alte Sampler vergisst den Prior",
    subtitle = paste0(gr$beschreibung, "\nBalken: ein PV je Person, Linie: wahre Verteilung der Gruppe, ",
                      "gestrichelt: Mittel der PV"),
    x = "θ", y = "Dichte",
    caption = stand
  ) +
  theme_blog() +
  theme(panel.spacing = unit(1.4, "lines"))
speichere_plot(p_gr, sprintf("pv_gruppen_%s", args$label), width = 10, height = 7, dir = ordner)
