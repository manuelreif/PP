# Read benchmark runs from the DuckDB database into data.tables.
#
#   Rscript bench/read_bench.R [--label=baseline] [--db=bench/bench.duckdb]
#
# prints the summary of the chosen measurement (default: the latest one).
# bench/bench.R and bench/table_gt.R source this file for the shared helpers.
#
# Tables (created by bench/bench.R):
#   messungen    one row per measurement session (label, git sha, machine, ...)
#   durchlaeufe  one row per timed run (szenario, funktion, durchlauf, sekunden, ...)

suppressPackageStartupMessages(library(data.table))

bench_db_default <- "bench/bench.duckdb"

# Parses --key=value and --flag arguments.
bench_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  get <- function(key, default = NULL) {
    hit <- grep(paste0("^--", key, "(=|$)"), args, value = TRUE)
    if (!length(hit)) return(default)
    val <- sub(paste0("^--", key, "=?"), "", hit[1])
    if (nzchar(val)) val else TRUE
  }
  list(db = get("db", bench_db_default), label = get("label"), runs = as.integer(get("runs", 30)),
       only = get("only"), notiz = get("notiz"), out = get("out"))
}

# DuckDB lets only one process open the file at a time, so connections are kept
# short and opening is retried while another process holds the lock.
bench_connect <- function(db = bench_db_default, read_only = FALSE, tries = 10) {
  if (read_only && !file.exists(db)) stop("database not found: ", db)
  for (i in seq_len(tries)) {
    con <- tryCatch(
      DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = db, read_only = read_only),
      error = function(e) e
    )
    if (!inherits(con, "error")) return(con)
    if (i < tries) Sys.sleep(1)
  }
  stop("could not open ", db, ": ", conditionMessage(con))
}

bench_disconnect <- function(con) DBI::dbDisconnect(con, shutdown = TRUE)

# All runs joined with their measurement metadata. With a label, only the
# latest measurement carrying that label is returned.
bench_read <- function(db = bench_db_default, label = NULL) {
  con <- bench_connect(db, read_only = TRUE)
  on.exit(bench_disconnect(con))
  sql <- "
    SELECT m.label, m.zeitpunkt AS messung_zeitpunkt, m.pp_version, m.git_sha, m.git_dirty,
           m.r_version, m.plattform, m.cpu, m.threads, d.*
    FROM durchlaeufe d JOIN messungen m USING (messung_id)"
  dt <- if (is.null(label)) {
    DBI::dbGetQuery(con, sql)
  } else {
    DBI::dbGetQuery(con, paste(sql, "WHERE d.messung_id = (SELECT max(messung_id) FROM messungen WHERE label = ?)"),
                    params = list(label))
  }
  setDT(dt)
  if (!nrow(dt)) stop("no runs found", if (!is.null(label)) paste0(" for label '", label, "'"))
  setkey(dt, messung_id, szenario, durchlauf)
  dt[]
}

# Per measurement and scenario: location, spread, target factor, and the
# single run times in run order (as text, for gt nanoplots).
bench_summary <- function(dt) {
  dt[order(messung_id, durchlauf), .(
    n           = .N,
    mittel      = mean(sekunden),
    sd          = sd(sekunden),
    median      = median(sekunden),
    q25         = quantile(sekunden, 0.25, names = FALSE),
    q75         = quantile(sekunden, 0.75, names = FALSE),
    iqr         = IQR(sekunden),
    min         = min(sekunden),
    max         = max(sekunden),
    vk          = sd(sekunden) / mean(sekunden),
    cpu_mittel  = mean(cpu_sekunden),
    ziel        = ziel_sekunden[1],
    faktor_ziel = mean(sekunden) / ziel_sekunden[1],
    werte       = paste(format(sekunden, digits = 6, scientific = FALSE, trim = TRUE), collapse = " ")
  ), by = .(messung_id, label, szenario, funktion, methode, modell, n_personen, n_items)]
}

if (sys.nframe() == 0L) {
  args <- bench_args()
  dt <- bench_read(args$db, args$label)
  dt <- dt[messung_id == max(messung_id)]
  m <- dt[1]
  cat(sprintf("Messung %d '%s': PP %s @ %s%s, %s, %s, %s (UTC)\n",
              m$messung_id, m$label, m$pp_version, m$git_sha, if (isTRUE(m$git_dirty)) " (dirty)" else "",
              m$r_version, m$cpu, format(m$messung_zeitpunkt, "%Y-%m-%d %H:%M")))
  s <- bench_summary(dt)
  print(s[, !c("messung_id", "label", "werte")], digits = 3, class = FALSE, row.names = FALSE)
}
