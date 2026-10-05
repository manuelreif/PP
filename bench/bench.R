# Time the benchmark scenarios against the installed PP and store every single
# run in the DuckDB database.
#
#   Rscript bench/bench.R --label=baseline [--runs=30] [--only=<regex>]
#                         [--notiz="..."] [--db=bench/bench.duckdb]
#
# Run from the package root after `R CMD INSTALL .`. After one untimed warm-up
# round, every round runs all scenarios once in a random order, so drift
# (temperature, clock speed, background load) hits all scenarios alike. The
# rows of a round are written as soon as the round is done.

Sys.setenv(OMP_NUM_THREADS = "1")
options(PP.threads = 1, width = 120)
source("bench/read_bench.R")
source("bench/scenarios.R")

bench_schema <- c("
  CREATE TABLE IF NOT EXISTS messungen (
    messung_id    INTEGER PRIMARY KEY,
    label         VARCHAR NOT NULL,
    zeitpunkt     TIMESTAMP NOT NULL,
    pp_version    VARCHAR,
    git_sha       VARCHAR,
    git_dirty     BOOLEAN,
    r_version     VARCHAR,
    plattform     VARCHAR,
    cpu           VARCHAR,
    blas          VARCHAR,
    threads       INTEGER,
    n_durchlaeufe INTEGER,
    session_info  VARCHAR,
    notiz         VARCHAR
  )", "
  CREATE TABLE IF NOT EXISTS durchlaeufe (
    messung_id    INTEGER NOT NULL REFERENCES messungen (messung_id),
    szenario      VARCHAR NOT NULL,
    funktion      VARCHAR NOT NULL,
    methode       VARCHAR,
    modell        VARCHAR,
    n_personen    INTEGER,
    n_items       INTEGER,
    durchlauf     INTEGER NOT NULL,
    position      INTEGER NOT NULL,
    sekunden      DOUBLE NOT NULL,
    cpu_sekunden  DOUBLE,
    ziel_sekunden DOUBLE,
    zeitpunkt     TIMESTAMP,
    nr_aufrufe    INTEGER,
    iterationen   INTEGER,
    iter_max      INTEGER,
    PRIMARY KEY (messung_id, szenario, durchlauf)
  )",
  # databases created before the iteration columns existed
  "ALTER TABLE durchlaeufe ADD COLUMN IF NOT EXISTS nr_aufrufe INTEGER",
  "ALTER TABLE durchlaeufe ADD COLUMN IF NOT EXISTS iterationen INTEGER",
  "ALTER TABLE durchlaeufe ADD COLUMN IF NOT EXISTS iter_max INTEGER")

# Swallow the cat() output and warnings of the old code.
quietly <- function(expr) {
  out <- NULL
  utils::capture.output(out <- suppressMessages(suppressWarnings(expr)))
  invisible(out)
}

# Newton-Raphson iterations. PP_4pl, PP_gpcm and JKpp call the NR drivers from
# R, so a trace on the drivers sees every call, including the 40 inside JKpp,
# which drops nsteps. NR_GPCM reports one sweep less than it runs
# (howlong = newr instead of newr + 1), so the log adds the offset back and
# holds the sweeps actually done.
nr_drivers <- c(NR_4PL = 0L, NR_GPCM = 1L, NR_mixed = 0L)
nr_log <- new.env()
nr_log$steps <- integer(0)

nr_trace <- function() {
  ns <- asNamespace("PP")
  for (f in intersect(names(nr_drivers), ls(ns))) {
    exit <- bquote(assign("steps", c(get("steps", envir = .(nr_log)),
                                     as.integer(returnValue()$nsteps) + .(nr_drivers[[f]])),
                          envir = .(nr_log)))
    suppressMessages(trace(f, exit = exit, where = ns, print = FALSE))
  }
}

time_run <- function(sc, d, s) {
  nr_log$steps <- integer(0)
  t <- system.time(quietly(sc$run(d, s)), gcFirst = TRUE)
  st <- nr_log$steps
  list(sekunden = t[["elapsed"]], cpu_sekunden = t[["user.self"]] + t[["sys.self"]],
       nr_aufrufe = length(st),
       iterationen = if (length(st)) sum(st) else NA_integer_,
       iter_max = if (length(st)) max(st) else NA_integer_)
}

git <- function(...) {
  tryCatch(suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = FALSE)),
           error = function(e) character(0))
}

messung_meta <- function(label, runs, notiz) {
  si <- sessionInfo()
  sha <- git("rev-parse", "--short", "HEAD")
  cpu <- tryCatch(grep("^model name", readLines("/proc/cpuinfo"), value = TRUE)[1],
                  error = function(e) NA_character_, warning = function(w) NA_character_)
  data.table(
    label = label,
    zeitpunkt = Sys.time(),
    pp_version = as.character(utils::packageVersion("PP")),
    git_sha = if (length(sha)) sha else NA_character_,
    git_dirty = length(git("status", "--porcelain", "--", "R", "src", "DESCRIPTION")) > 0,
    r_version = R.version.string,
    plattform = if (is.null(si$running)) NA_character_ else si$running,
    cpu = sub("^model name\\s*:\\s*", "", cpu),
    blas = if (is.null(si$BLAS)) NA_character_ else si$BLAS,
    threads = 1L,
    n_durchlaeufe = as.integer(runs),
    session_info = paste(utils::capture.output(print(si)), collapse = "\n"),
    notiz = if (is.null(notiz)) NA_character_ else notiz
  )
}

db_new_messung <- function(db, meta) {
  con <- bench_connect(db)
  on.exit(bench_disconnect(con))
  for (sql in bench_schema) DBI::dbExecute(con, sql)
  id <- DBI::dbGetQuery(con, "SELECT coalesce(max(messung_id), 0) + 1 AS id FROM messungen")$id
  DBI::dbAppendTable(con, "messungen", cbind(data.table(messung_id = as.integer(id)), meta))
  as.integer(id)
}

db_append_runs <- function(db, rows) {
  con <- bench_connect(db)
  on.exit(bench_disconnect(con))
  DBI::dbWithTransaction(con, DBI::dbAppendTable(con, "durchlaeufe", rows))
}

# --- main ----------------------------------------------------------------

args <- bench_args()
if (is.null(args$label)) stop("please name the measurement, e.g. --label=baseline")

ids <- names(bench_scenarios)
if (!is.null(args$only)) ids <- grep(args$only, ids, value = TRUE)
if (!length(ids)) stop("no scenario matches --only=", args$only)

# Draw all round orders first, before any scenario touches the RNG.
set.seed(20261005)
orders <- lapply(seq_len(args$runs), function(r) sample(ids))

suppressPackageStartupMessages(library(PP))
nr_trace()
d <- bench_data()
setups <- lapply(bench_scenarios[ids], function(sc) if (is.null(sc$setup)) NULL else quietly(sc$setup(d)))

messung_id <- db_new_messung(args$db, messung_meta(args$label, args$runs, args$notiz))
message(sprintf("Messung %d '%s': %d Szenarien x %d Durchlaeufe -> %s",
                messung_id, args$label, length(ids), args$runs, args$db))

t0 <- Sys.time()
for (id in ids) time_run(bench_scenarios[[id]], d, setups[[id]])
message(sprintf("Aufwaermrunde fertig (%.0f s)", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

t_start <- Sys.time()
for (r in seq_len(args$runs)) {
  t_round <- Sys.time()
  rows <- rbindlist(lapply(seq_along(orders[[r]]), function(pos) {
    id <- orders[[r]][pos]
    sc <- bench_scenarios[[id]]
    tm <- time_run(sc, d, setups[[id]])
    data.table(messung_id = messung_id, szenario = id, funktion = sc$funktion, methode = sc$methode,
               modell = sc$modell, n_personen = as.integer(sc$n_personen), n_items = as.integer(sc$n_items),
               durchlauf = r, position = pos, sekunden = tm$sekunden,
               cpu_sekunden = tm$cpu_sekunden, ziel_sekunden = sc$ziel, zeitpunkt = Sys.time(),
               nr_aufrufe = tm$nr_aufrufe, iterationen = tm$iterationen, iter_max = tm$iter_max)
  }))
  db_append_runs(args$db, rows)
  elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
  message(sprintf("Runde %d/%d fertig (%.0f s, Rest ca. %.0f min)", r, args$runs,
                  as.numeric(difftime(Sys.time(), t_round, units = "secs")),
                  elapsed / r * (args$runs - r) / 60))
}

message(sprintf("Fertig: %d Durchlaeufe in %.1f min gespeichert (messung_id %d).",
                args$runs * length(ids), as.numeric(difftime(Sys.time(), t0, units = "mins")), messung_id))
