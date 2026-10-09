# Runs the test suite against the installed PP and stores one row per test.
#
#   Rscript blog/testbilanz_daten.R [--label=block1]
#
# Writes blog/output/testbilanz_<label>.rds with the test file, the area, the
# number of passed expectations and, for skipped known-bug tests, the problem
# ID and the block that fixes it.

source("blog/gemeinsam.R")
args <- blog_args()
suppressPackageStartupMessages(library(PP))

Sys.setenv(PP_KNOWN_BUGS = "")
res <- testthat::test_dir("tests/testthat", package = "PP", load_package = "installed",
                          reporter = "silent", stop_on_failure = FALSE)

skip_grund <- function(t) {
  sk <- Filter(function(e) inherits(e, "expectation_skip"), t$results)
  if (length(sk)) sub("^Reason: ", "", conditionMessage(sk[[1]])) else NA_character_
}
tests <- rbindlist(lapply(res, function(t) data.table(
  datei = t$file,
  test = t$test,
  bestanden = sum(vapply(t$results, function(e) inherits(e, "expectation_success"), TRUE)),
  fehlgeschlagen = sum(vapply(t$results, function(e) inherits(e, c("expectation_failure", "expectation_error")), TRUE)),
  skip = skip_grund(t)
)))
tests[, id := sub(" - fixed in block .*$", "", skip)]
tests[, block := as.integer(sub("^.* - fixed in block ", "", skip))]

bereiche <- data.table(
  datei = c("test-reference.R", "test-estimate.R", "test-eap.R", "test-pv.R", "test-jackknife.R",
            "test-pfit.R", "test-input.R", "test-prepare.R", "test-sim.R", "test-ppass.R",
            "tests4pl.R", "testsgpcm.R", "testsgpcm4pl.R", "testsjk.R"),
  bereich = c("Referenzen (Selbsttest)", "Schätzer: mle, wle, map, robust", "EAP",
              "Plausible Values", "Jackknife", "Personenfit", "Eingaben und Defaults", "Eingaben und Defaults",
              "Simulation", "PPass", rep("Alte Tests", 4)),
  ord = c(1:7, 7:9, rep(10L, 4))
)
tests <- merge(tests, bereiche, by = "datei", all.x = TRUE, sort = FALSE)
tests[, `:=`(label = args$label, code = code_stand(), zeitpunkt = Sys.time())]

dir.create(blog_out, showWarnings = FALSE, recursive = TRUE)
out <- file.path(blog_out, sprintf("testbilanz_%s.rds", args$label))
saveRDS(tests, out)
message("gespeichert: ", out)
print(tests[, .(tests = .N, bestanden = sum(bestanden), fehlgeschlagen = sum(fehlgeschlagen),
                skips = sum(!is.na(skip))), keyby = .(ord, bereich)])
