# ==============================================================================
# daily_update.R
#
# Finds the latest available SNODAS file, computes HUC8 zonal statistics,
# and appends the result to data/snodas_huc8_history.csv.
#
# Designed to run nightly via GitHub Actions after backfill is complete.
# Skips gracefully if today's date is already in the CSV.
#
# Usage:
#   Rscript R/daily_update.R
# ==============================================================================

source("R/snodas_functions.R")

# ------------------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------------------

work_dir    <- "tmp_snodas"
cache_dir   <- "cache"
data_dir    <- "data"
history_csv <- file.path(data_dir,  "snodas_huc8_history.csv")
huc8_cache  <- file.path(cache_dir, "HUC8_WA_WGS84.gpkg")

for (d in c(work_dir, cache_dir, data_dir))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)

# ------------------------------------------------------------------------------
# FIND LATEST AVAILABLE SNODAS DATE
# ------------------------------------------------------------------------------

snodas_info <- find_latest_snodas()
date_str    <- format(snodas_info$date, "%Y-%m-%d")
message(sprintf("Target date: %s", date_str))

# ------------------------------------------------------------------------------
# SKIP IF ALREADY IN CSV
# ------------------------------------------------------------------------------

if (file.exists(history_csv)) {
  existing <- tryCatch(read.csv(history_csv, stringsAsFactors = FALSE),
                       error = function(e) NULL)
  if (!is.null(existing) && date_str %in% existing$swe_date) {
    message(sprintf("Date %s already in CSV -- nothing to do.", date_str))
    quit(status = 0)
  }
}

# ------------------------------------------------------------------------------
# PROCESS
# ------------------------------------------------------------------------------

huc8   <- get_huc8_wa(huc8_cache)
result <- process_one_date(snodas_info$date, huc8, work_dir, cleanup = TRUE)

if (is.null(result)) {
  message(sprintf("Processing failed for %s -- CSV unchanged.", date_str))
  quit(status = 1)
}

# ------------------------------------------------------------------------------
# APPEND TO CSV
# ------------------------------------------------------------------------------

if (file.exists(history_csv)) {
  history <- read.csv(history_csv, stringsAsFactors = FALSE)
} else {
  history <- data.frame()
}

history <- dplyr::bind_rows(history, result)

# Keep sorted by date then HUC8
history <- history[order(history$swe_date, history$HUC8), ]

write.csv(history, history_csv, row.names = FALSE)

message(sprintf("\nAppended %d rows for %s", nrow(result), date_str))
message(sprintf("CSV now contains %d total rows (%d unique dates)",
                nrow(history),
                length(unique(history$swe_date))))
message(sprintf("Total WA SWE: %.1f KAF",
                sum(result$swe_volume_kaf, na.rm = TRUE)))
