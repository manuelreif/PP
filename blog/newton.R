# Pictures of the safeguarded Newton (person estimator):
#   newton_gesichert   the oscillating person of fehler_newton, old vs. new, as
#                      GIF (one frame per step) and as a still of the last frame
#   newton_schritte    number of steps per person in the benchmark data
#   newton_konvergenz  distance to the solution after k steps
#
#   Rscript blog/newton.R [--label=block4]
#
# Reads blog/output/newton_<label>.rds (blog/newton_daten.R).

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})
source("blog/gemeinsam.R")
args <- blog_args()
d <- readRDS(file.path(blog_out, sprintf("newton_%s.rds", args$label)))
dunkel <- "#1F2328"
blau <- "#0969DA"
stand <- sprintf("Stand: %s.", gsub("`", "", d$code))
exac_de <- zahl_de(d$exac, 3)

# --- newton_gesichert ----------------------------------------------------

pe <- d$pendel
w <- pe$wurzel
n_neu <- pe$n_neu
K <- n_neu + 3
f_an <- function(t) approx(pe$kurve$theta, pe$kurve$f, t)$y
xlim <- c(-6, 0.3)
kurve <- pe$kurve[theta >= xlim[1] & theta <= xlim[2]]
ylim <- range(kurve$f)

alt <- pe$alt[k <= K]
alt[, f := f_an(theta)]
neu <- pe$neu
# after convergence the new estimator stays at its last value
neu_k <- function(kk) neu[k == min(kk, n_neu)]

grund <- function(titel) {
  ggplot(kurve, aes(theta, f)) +
    geom_hline(yintercept = 0, colour = grau, linewidth = 0.3) +
    geom_line(colour = dunkel, linewidth = 0.6) +
    geom_point(data = data.table(theta = w, f = 0), colour = dunkel, size = 2.4) +
    # stretched around 0, so the steps close to the root stay visible
    scale_y_continuous(transform = scales::pseudo_log_trans(sigma = 0.01, base = 10),
                       breaks = c(-5, -1, -0.1, 0, 0.1),
                       labels = function(v) vapply(v, function(x) if (is.na(x) || x == 0) "0" else
                         zahl_de(x, max(0, -floor(log10(abs(x))))), "")) +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    labs(x = "θ", y = "Warm-Gleichung f(θ)", title = titel) +
    theme_blog() +
    theme(plot.title = element_text(size = rel(1)))
}

wert_label <- function(p, farbe, text) {
  p + annotate("label", x = xlim[1] + 0.1, y = ylim[1], label = text, hjust = 0, vjust = 0,
               size = 3.3, colour = farbe, fill = "white", border.colour = "white",
               family = "Open Sans")
}

# frame kk (kk is not called k: inside data.table k is the column)
bild <- function(kk) {
  # left: PP 0.6.4-1
  spur_a <- alt[k < kk]
  jetzt_a <- alt[k == kk]
  p_alt <- grund(sprintf("PP 0.6.4-1 · Schritt %d", kk)) +
    geom_point(data = spur_a, colour = akzent, size = 1.6, alpha = 0.35) +
    geom_point(data = jetzt_a, colour = akzent, size = 3.4)
  p_alt <- wert_label(p_alt, akzent, sprintf("θ = %s", zahl_de(jetzt_a$theta, 3)))

  # middle: safeguarded Newton with its bracket
  jetzt_n <- neu_k(kk)
  spur_n <- neu[k < jetzt_n$k]
  fertig <- kk >= n_neu
  titel_n <- if (fertig) sprintf("neu · fertig nach %d Schritten", n_neu) else
    sprintf("neu · Schritt %d", kk)
  p_neu <- grund(titel_n) +
    annotate("rect", xmin = jetzt_n$lo, xmax = jetzt_n$hi, ymin = -Inf, ymax = Inf,
             fill = blau, alpha = 0.12) +
    geom_point(data = spur_n, aes(shape = schritt), colour = blau, size = 1.6, alpha = 0.35) +
    geom_point(data = jetzt_n, aes(shape = schritt), colour = blau, size = 3.4) +
    scale_shape_manual(values = c(Start = 15, Newton = 16, Halbierung = 17), drop = FALSE,
                       limits = c("Start", "Newton", "Halbierung"), name = NULL) +
    guides(shape = guide_legend(override.aes = list(size = 2.4, alpha = 1))) +
    theme(axis.title.y = element_blank(), legend.position = "inside", legend.position.inside = c(0.98, 0.98),
          legend.justification = c(1, 1), legend.background = element_rect(fill = "white", colour = NA))
  breite <- jetzt_n$hi - jetzt_n$lo
  p_neu <- wert_label(p_neu, blau,
                      sprintf("θ = %s · %s\nIntervallbreite %s", zahl_de(jetzt_n$theta, 3),
                              c(Start = "Startwert", Newton = "Newton-Schritt", Halbierung = "Halbierung")[[jetzt_n$schritt]], formatC(breite, digits = 2, format = "g",
                                                                decimal.mark = ",")))

  # right: distance to the solution
  abst <- rbind(
    alt[k <= kk, .(k, abstand = abs(theta - w), verfahren = "PP 0.6.4-1")],
    neu[k <= min(kk, n_neu), .(k, abstand = abs(theta - w), verfahren = "neu")]
  )
  p_abst <- ggplot(abst, aes(k, abstand, colour = verfahren)) +
    geom_hline(yintercept = d$exac, colour = grau, linetype = "22", linewidth = 0.3) +
    annotate("text", x = K, y = d$exac, label = sprintf("exac = %s", exac_de), hjust = 1,
             vjust = -0.5, size = 3, colour = grau, family = "Open Sans") +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1.6) +
    scale_colour_manual(values = c("PP 0.6.4-1" = akzent, "neu" = blau), name = NULL,
                        limits = c("PP 0.6.4-1", "neu")) +
    scale_y_log10(labels = function(v) formatC(v, format = "g", decimal.mark = ",")) +
    scale_x_continuous(breaks = seq(0, K, by = 2)) +
    coord_cartesian(xlim = c(0, K), ylim = c(1e-5, 10)) +
    labs(x = "Schritt", y = "Abstand zur Lösung |θ − θ*|", title = "Abstand zur Lösung") +
    theme_blog() +
    theme(plot.title = element_text(size = rel(1)))

  (p_alt | p_neu | p_abst) +
    plot_layout(widths = c(1, 1, 0.8)) +
    plot_annotation(
      title = "Das gesicherte Newton-Verfahren kommt an",
      subtitle = sprintf("%s · links das alte Verfahren, das zwischen drei Werten pendelt, in der Mitte das neue mit seinem Suchintervall (blau)",
                         pe$beschreibung),
      caption = paste(
        "Ein Newton-Schritt wird nur genommen, wenn er im Intervall bleibt und höchstens halb so lang ist wie der vorletzte, sonst wird das Intervall halbiert.",
        sprintf("Jeder Schritt verkleinert das Intervall. Schluss ist, wenn sich θ um weniger als exac = %s ändert. Schwarzer Punkt: Lösung (Referenz per uniroot). Die y-Achse ist um null gestreckt.", exac_de),
        stand, sep = "\n"),
      theme = theme_blog()
    )
}

breite_in <- 13
hoehe_in <- 5
frames <- file.path(tempdir(), sprintf("newton_%02d.png", 0:K))
for (k in 0:K) {
  # frame 0 has one point per line: ggplot's message about it is expected
  suppressMessages(ggsave(frames[k + 1], bild(k), width = breite_in, height = hoehe_in, dpi = 110,
                          bg = "white"))
}
# 0.9 s per step, the start and the last frame stand longer
anim <- magick::image_read(frames[c(1, 1, seq_along(frames), rep(length(frames), 4))])
anim <- magick::image_animate(anim, delay = 90, optimize = TRUE)
gif <- file.path(blog_out, sprintf("newton_gesichert_%s.gif", args$label))
magick::image_write(anim, gif)
message("gespeichert: ", gif, " (", round(file.size(gif) / 1e6, 2), " MB, ", length(frames), " Schritte)")
speichere_plot(bild(K), sprintf("newton_gesichert_%s", args$label), width = breite_in, height = hoehe_in)

# --- newton_schritte -------------------------------------------------------

st <- d$schritte
anteile <- st[status == "ok", .N, by = .(modell, methode, nsteps)]
anteile[, anteil := N / sum(N), by = .(modell, methode)]
xmax <- max(st$nsteps) + 1
alt_s <- copy(d$alt_schritte)
alt_s[, `:=`(x = pmin(iterationen, xmax),
             text = fifelse(iterationen > xmax, sprintf("früher: %d →", iterationen),
                            sprintf("früher: %d", iterationen)))]
median_s <- st[status == "ok", .(median = median(nsteps), max = max(nsteps)), by = .(modell, methode)]
methoden <- c("MLE", "WLE", "MAP")
for (dt in list(anteile, alt_s, median_s)) dt[, methode := factor(methode, levels = methoden)]

ohne <- st[, mean(status != "ok")]
p_s <- ggplot(anteile, aes(nsteps, anteil)) +
  geom_col(width = 0.8, fill = blau) +
  # the longest persons are too few for a visible bar
  geom_point(data = median_s, aes(x = max, y = 0), inherit.aes = FALSE, colour = blau,
             shape = 17, size = 1.8) +
  geom_text(data = median_s, aes(x = xmax - 0.5, y = 0.6, label = sprintf("längste: %d", max)),
            inherit.aes = FALSE, hjust = 1, size = 3.1, colour = blau, family = "Open Sans") +
  geom_vline(data = alt_s, aes(xintercept = x), colour = akzent, linetype = "22", linewidth = 0.5) +
  geom_text(data = alt_s, aes(x = x, y = 0.95, label = text), inherit.aes = FALSE, hjust = 1.05,
            size = 3.1, colour = akzent, family = "Open Sans") +
  scale_y_continuous(labels = function(v) paste0(round(100 * v), " %"), breaks = c(0, 0.5, 1)) +
  scale_x_continuous(breaks = seq(0, xmax, by = 2)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(-0.5, xmax + 0.5)) +
  facet_grid(modell ~ methode) +
  labs(
    title = "Jede Person bekommt ihren eigenen Newton",
    subtitle = sprintf("Anteil der Personen je Zahl der Schritte · die meisten sind nach %s Schritten fertig",
                       paste(sort(unique(median_s$median)), collapse = " bis ")),
    x = "Schritte bis zur Konvergenz", y = "Anteil der Personen",
    caption = paste(
      "1PL bis 3PL: 5000 Personen × 40 Items, GPCM: 2000 Personen × 20 Items (simuliert), exac = 0,001.",
      "Blaues Dreieck: die Person mit den meisten Schritten.",
      sprintf("Personen mit Extremscore oder Lösung am Rand (insgesamt %s %%) brauchen keinen Schritt und fehlen hier.",
              zahl_de(100 * ohne, 1)),
      "Orange: PP 0.6.4-1 iterierte alle Personen gemeinsam, jede Person bekam so viele Schritte wie die langsamste.",
      "Bei 3PL MLE/WLE brach es nach 40 Schritten (maxsteps) ab, ohne zu konvergieren. GPCM MAP wurde damals nicht gemessen.",
      stand, sep = "\n")
  ) +
  theme_blog() +
  theme(panel.spacing = unit(0.8, "lines"))
speichere_plot(p_s, sprintf("newton_schritte_%s", args$label), width = 9, height = 8)

# --- newton_konvergenz -----------------------------------------------------

# 10^-k with superscript digits
hoch10 <- function(v) {
  e <- round(log10(v))
  hoch <- chartr("0123456789-", "⁰¹²³⁴⁵⁶⁷⁸⁹⁻", as.character(e))
  ifelse(e == 0, "1", paste0("10", hoch))
}
ko <- d$konvergenz
boden <- 1e-16
ko[, abstand := pmax(fehler, boden)]
ko[, fall := sprintf("%s %s", modell, methode)]
med <- ko[, .(abstand = median(abstand)), by = .(fall, k)]

p_k <- ggplot(ko, aes(k, abstand)) +
  geom_line(aes(group = person), colour = blau, alpha = 0.08, linewidth = 0.3) +
  geom_line(data = med, colour = dunkel, linewidth = 0.9) +
  geom_point(data = med, colour = dunkel, size = 1.6) +
  geom_hline(yintercept = d$exac, colour = akzent, linetype = "22", linewidth = 0.5) +
  annotate("text", x = 12, y = d$exac, label = sprintf("exac = %s", exac_de), hjust = 1,
           vjust = -0.5, size = 3.1, colour = akzent, family = "Open Sans") +
  annotate("text", x = 12, y = boden, label = "Rechengenauigkeit erreicht", hjust = 1,
           vjust = 1.8, size = 3.1, colour = grau, family = "Open Sans") +
  scale_y_log10(breaks = 10^c(0, -3, -6, -9, -12, -15), labels = hoch10) +
  coord_cartesian(ylim = c(boden / 20, 3)) +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  facet_wrap(~ fall, nrow = 1) +
  labs(
    title = "Wie schnell der Abstand zur Lösung schrumpft",
    subtitle = "Je Person eine Linie (300 Personen), schwarz: Median · bei 2PL MLE verdoppelt sich die Zahl richtiger Stellen pro Schritt",
    x = "Schritt k", y = "|θₖ − θ*|",
    caption = paste(
      "Der Schritt teilt durch die Testinformation I(θ), nicht durch die exakte Ableitung der Schätzgleichung (Fisher-Scoring).",
      "Nur bei MLE im 1PL/2PL sind beide gleich, dann ist es das echte Newton-Verfahren mit quadratischer Konvergenz.",
      "Sonst gewinnt es etwa anderthalb Stellen pro Schritt. θ* = Ergebnis mit exac = 10⁻¹², Personen mit Extremscore oder Lösung am Rand sind weggelassen.",
      stand, sep = "\n")
  ) +
  theme_blog()
speichere_plot(p_k, sprintf("newton_konvergenz_%s", args$label), width = 10, height = 4.8)
