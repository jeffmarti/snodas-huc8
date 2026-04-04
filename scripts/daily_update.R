# ==============================================================================
# daily_update.R
#
# Finds the latest available SNODAS file, computes HUC8 zonal statistics,
# and appends the result to data/snodas_huc8_history.csv.
#
# Also fetches WA statewide monthly climate data from NCEI once per month
# (after the 10th) and writes to data/ncei_climate_monthly_wa.csv.
#
# Designed to run nightly via GitHub Actions after backfill is complete.
# Skips gracefully if today's date is already in the CSV.
#
# Usage:
#   Rscript R/daily_update.R
# ==============================================================================

source("scripts/snodas_functions.R")

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

history$HUC8 <- as.character(history$HUC8)
result$HUC8  <- as.character(result$HUC8)

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

# ------------------------------------------------------------------------------
# NCEI WA STATEWIDE MONTHLY CLIMATE (runs at most once per month, after the 10th)
# ------------------------------------------------------------------------------

message("\nChecking NCEI WA statewide climate update...")

fetch_ncei_statewide_wa <- function(variable, max_retries = 3, timeout_sec = 90) {
  url <- sprintf(
    paste0("https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/",
           "statewide/time-series/45/%s/1/0/2000-2026/data.csv",
           "?base_prd=true&begbaseyear=1991&endbaseyear=2020"),
    variable
  )
  
  resp <- NULL
  for (attempt in seq_len(max_retries)) {
    message(sprintf("  NCEI WA %s: attempt %d/%d...", variable, attempt, max_retries))
    resp <- tryCatch(
      httr::GET(url, httr::timeout(timeout_sec)),
      error = function(e) {
        message(sprintf("  Attempt %d failed: %s", attempt, e$message))
        NULL
      }
    )
    if (!is.null(resp) && httr::status_code(resp) == 200L) break
    if (attempt < max_retries) {
      wait <- attempt * 10L
      message(sprintf("  Waiting %ds before retry...", wait))
      Sys.sleep(wait)
    }
  }
  
  if (is.null(resp) || httr::status_code(resp) != 200L) {
    warning(sprintf("NCEI WA fetch failed after %d attempts: %s",
                    max_retries, variable))
    return(NULL)
  }
  
  text  <- httr::content(resp, as = "text", encoding = "UTF-8")
  lines <- strsplit(text, "\n")[[1]]
  data_lines <- lines[!startsWith(trimws(lines), "#") & nchar(trimws(lines)) > 0]
  clean_text <- paste(data_lines, collapse = "\n")
  
  df <- tryCatch(
    read.csv(text = clean_text, stringsAsFactors = FALSE),
    error = function(e) { warning("CSV parse failed: ", e$message); NULL }
  )
  if (is.null(df) || nrow(df) == 0L) return(NULL)
  
  df %>%
    dplyr::rename(yyyymm = Date, value = Value, anomaly = Anomaly) %>%
    dplyr::mutate(
      yyyymm   = sprintf("%06d", as.integer(yyyymm)),
      variable = variable,
      year     = as.integer(substr(yyyymm, 1L, 4L)),
      month    = as.integer(substr(yyyymm, 5L, 6L))
    ) %>%
    dplyr::filter(!is.na(value), value > -99)
}

ncei_wa_file         <- file.path(data_dir, "ncei_climate_monthly_wa.csv")
ncei_wa_needs_update <- FALSE
today                <- Sys.Date()
current_month        <- format(today, "%Y-%m")
day_of_month         <- as.integer(format(today, "%d"))

if (day_of_month >= 10L) {
  if (!file.exists(ncei_wa_file)) {
    message("  NCEI WA CSV missing -- will fetch")
    ncei_wa_needs_update <- TRUE
  } else {
    existing_wa <- tryCatch(
      read.csv(ncei_wa_file, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    if (is.null(existing_wa) || nrow(existing_wa) == 0L) {
      message("  NCEI WA CSV empty or unreadable -- will fetch")
      ncei_wa_needs_update <- TRUE
    } else {
      last_modified_wa   <- file.info(ncei_wa_file)$mtime
      updated_this_month <- format(last_modified_wa, "%Y-%m") == current_month
      if (updated_this_month) {
        message(sprintf("  NCEI WA already updated this month (%s) -- skipping",
                        current_month))
      } else {
        message(sprintf("  NCEI WA not yet updated for %s -- will fetch",
                        current_month))
        ncei_wa_needs_update <- TRUE
      }
    }
  }
} else {
  message(sprintf("  NCEI update not expected until ~10th (today is %s) -- skipping",
                  format(today, "%b %d")))
}

if (ncei_wa_needs_update) {
  ncei_wa_raw <- tryCatch({
    tavg_wa <- fetch_ncei_statewide_wa("tavg")
    Sys.sleep(0.5)
    pcp_wa  <- fetch_ncei_statewide_wa("pcp")
    dplyr::bind_rows(tavg_wa, pcp_wa)
  }, error = function(e) {
    message("  ERROR fetching NCEI WA: ", conditionMessage(e))
    NULL
  })
  
  if (!is.null(ncei_wa_raw) && nrow(ncei_wa_raw) > 0L) {
    write.csv(ncei_wa_raw, ncei_wa_file, row.names = FALSE)
    message(sprintf("  Saved %d rows to %s  (tavg + pcp, 2000-%d)",
                    nrow(ncei_wa_raw), ncei_wa_file, max(ncei_wa_raw$year)))
  } else {
    if (file.exists(ncei_wa_file)) {
      message("  WARNING: fetch failed -- retaining existing CSV")
    } else {
      message("  WARNING: fetch failed and no existing CSV -- NCEI WA data unavailable")
    }
  }
}