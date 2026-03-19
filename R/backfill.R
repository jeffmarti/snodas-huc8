# ==============================================================================
# backfill.R
#
# Downloads and processes SNODAS SWE for every day
# from September 2003 to the present day. Designed to run nightly on GitHub
# Actions, resuming from a checkpoint file each night until complete.
#
# Progress is saved after every date so a partial run is never wasted.
# The accumulated results are written to data/snodas_huc8_history.csv.
#
# Usage:
#   Rscript R/backfill.R
#
# Environment variables (set in GHA workflow):
#   MAX_RUNTIME_MIN  : stop processing this many minutes before GHA limit
#                      (default: 330, leaving 30 min buffer before 6hr limit)
# ==============================================================================

source("R/snodas_functions.R")

# ------------------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------------------

work_dir       <- "tmp_snodas"          # scratch space, gitignored
cache_dir      <- "cache"
data_dir       <- "data"
history_csv    <- file.path(data_dir,  "snodas_huc8_history.csv")
progress_file  <- file.path(data_dir,  "backfill_progress.txt")
huc8_cache     <- file.path(cache_dir, "HUC8_WA_WGS84.gpkg")

# SNODAS period of record starts 2003-09-30
backfill_start <- as.Date("2003-10-01")
backfill_end   <- Sys.Date() - 1        # yesterday

# Runtime guard: stop before GitHub Actions 6-hour wall
max_runtime_min <- as.numeric(Sys.getenv("MAX_RUNTIME_MIN", unset = "330"))
start_time      <- proc.time()["elapsed"]

# ------------------------------------------------------------------------------
# BUILD TARGET DATE LIST (1st and 15th of each month)
# ------------------------------------------------------------------------------

build_target_dates <- function(start_date, end_date) {
  seq(start_date, end_date, by = "day")
}

target_dates <- build_target_dates(backfill_start, backfill_end)
message(sprintf("Total target dates: %d  (%s to %s)",
                length(target_dates),
                format(min(target_dates), "%Y-%m-%d"),
                format(max(target_dates), "%Y-%m-%d")))

# ------------------------------------------------------------------------------
# DETERMINE WHICH DATES STILL NEED PROCESSING
# ------------------------------------------------------------------------------

# Dates already in the CSV
completed_dates <- as.Date(character(0))
if (file.exists(history_csv)) {
  existing <- tryCatch(read.csv(history_csv, stringsAsFactors = FALSE),
                       error = function(e) NULL)
  if (!is.null(existing) && "swe_date" %in% names(existing)) {
    completed_dates <- as.Date(unique(existing$swe_date))
  }
}

# Resume checkpoint (last successfully processed date in this backfill run)
checkpoint_date <- as.Date(NA)
if (file.exists(progress_file)) {
  cp <- tryCatch(trimws(readLines(progress_file, warn = FALSE)[1]),
                 error = function(e) "")
  if (nchar(cp) == 10) checkpoint_date <- as.Date(cp)
}

# Dates still needed
remaining_dates <- target_dates[!target_dates %in% completed_dates]

message(sprintf("Already completed : %d dates", length(completed_dates)))
message(sprintf("Remaining         : %d dates", length(remaining_dates)))

if (length(remaining_dates) == 0) {
  message("\nBackfill complete -- all target dates are in the CSV.")
  message("You can disable the backfill workflow in GitHub Actions.")
  quit(status = 0)
}

# ------------------------------------------------------------------------------
# SET UP OUTPUT
# ------------------------------------------------------------------------------

for (d in c(work_dir, cache_dir, data_dir))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)

# Load or initialise HUC8 boundaries
huc8 <- get_huc8_wa(huc8_cache)

# Load existing history or initialise empty data frame
# Load existing history or initialise empty data frame
if (file.exists(history_csv)) {
  history <- read.csv(history_csv, stringsAsFactors = FALSE,
                      colClasses = c(
                        HUC8           = "character",
                        Name           = "character",
                        States         = "character",
                        AreaSqKm       = "numeric",
                        AreaAcres      = "numeric",
                        swe_date       = "character",
                        swe_mean_mm    = "numeric",
                        swe_mean_in    = "numeric",
                        swe_min_mm     = "numeric",
                        swe_max_mm     = "numeric",
                        swe_volume_af  = "numeric",
                        swe_volume_kaf = "numeric"
                      ))
  message(sprintf("Loaded existing history: %d rows", nrow(history)))
} else {
  history <- data.frame()
  message("No existing history -- starting fresh.")
}

# ------------------------------------------------------------------------------
# MAIN BACKFILL LOOP
# ------------------------------------------------------------------------------

n_processed  <- 0L
n_skipped    <- 0L
n_errors     <- 0L

message(sprintf("\nStarting backfill loop -- max runtime: %g minutes\n",
                max_runtime_min))

for (date in remaining_dates) {

  # Check elapsed time -- stop gracefully before GHA wall
  elapsed_min <- (proc.time()["elapsed"] - start_time) / 60
  if (elapsed_min >= max_runtime_min) {
    message(sprintf("\nRuntime limit reached (%.1f min) -- stopping gracefully.",
                    elapsed_min))
    message("Resume will pick up from here on the next run.")
    break
  }

  date <- as.Date(date, origin = "1970-01-01")
  date_str <- format(date, "%Y-%m-%d")
  message(sprintf("[%s]  elapsed: %.1f min  remaining: %d",
                  date_str, elapsed_min,
                  length(remaining_dates) - n_processed - n_skipped - n_errors))

  result <- process_one_date(date, huc8, work_dir, cleanup = TRUE)

  if (is.null(result)) {
    n_skipped <- n_skipped + 1L
    # Still update checkpoint so we don't retry unavailable dates endlessly
    writeLines(date_str, progress_file)
    next
  }

  # Append to history and write CSV immediately (crash-safe)
  history <- bind_rows(history, result)
  write.csv(history, history_csv, row.names = FALSE)

  # Update checkpoint
  writeLines(date_str, progress_file)

  n_processed <- n_processed + 1L
  message(sprintf("  OK  total SWE: %.1f KAF",
                  sum(result$swe_volume_kaf, na.rm = TRUE)))
}

# ------------------------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------------------------

elapsed_total <- (proc.time()["elapsed"] - start_time) / 60

message(sprintf("\n=== Backfill Session Complete ==="))
message(sprintf("  Processed  : %d dates", n_processed))
message(sprintf("  Skipped    : %d dates (not available on NSIDC)", n_skipped))
message(sprintf("  Errors     : %d dates", n_errors))
message(sprintf("  Elapsed    : %.1f minutes", elapsed_total))
message(sprintf("  CSV rows   : %d", nrow(history)))
message(sprintf("  Remaining  : %d dates",
                length(remaining_dates) - n_processed - n_skipped - n_errors))

# Check if fully complete
all_done <- all(target_dates %in% as.Date(unique(history$swe_date)))
if (all_done) {
  message("\n*** BACKFILL COMPLETE *** All target dates processed.")
  message("You can now disable the backfill GHA workflow.")
  writeLines("COMPLETE", progress_file)
}
