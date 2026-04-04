# ==============================================================================
# NCEI STATEWIDE WA FETCH — add to the snodas-huc8 update pipeline script
#
# This fetches WA statewide monthly temperature and precipitation anomalies
# from NOAA NCEI and writes them to data/ncei_climate_monthly_wa.csv.
#
# The fetch is skipped on days 1-9 of the month (NCEI publishes around the
# 8th-10th) and also skipped if the file was already updated this month.
#
# URLs use:
#   /statewide/time-series/45/   (WA = state code 45)
#   base period: 1991-2021
# This is DIFFERENT from the Yakima app, which uses:
#   /divisional/time-series/4506/ (WA East Cascades climate division)
# ==============================================================================

# -- Paste this helper near the top of your pipeline script ------------------
# (or add it inline below the existing library() calls)

fetch_ncei_statewide_wa <- function(variable, max_retries = 3, timeout_sec = 90) {
  url <- sprintf(
    paste0("https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/",
           "statewide/time-series/45/%s/1/0/2000-2026/data.csv",
           "?base_prd=true&begbaseyear=1991&endbaseyear=2020"),
    variable
  )
  
  resp <- NULL
  for (attempt in seq_len(max_retries)) {
    cat(sprintf("  NCEI WA statewide %s: attempt %d/%d...\n",
                variable, attempt, max_retries))
    resp <- tryCatch(
      httr::GET(url, httr::timeout(timeout_sec)),
      error = function(e) {
        cat(sprintf("  Attempt %d failed: %s\n", attempt, e$message))
        NULL
      }
    )
    if (!is.null(resp) && httr::status_code(resp) == 200L) break
    if (attempt < max_retries) {
      wait <- attempt * 10L
      cat(sprintf("  Waiting %ds before retry...\n", wait))
      Sys.sleep(wait)
    }
  }
  
  if (is.null(resp) || httr::status_code(resp) != 200L) {
    warning(sprintf("NCEI WA statewide fetch failed after %d attempts: %s",
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
    rename(yyyymm = Date, value = Value, anomaly = Anomaly) %>%
    mutate(
      yyyymm   = sprintf("%06d", as.integer(yyyymm)),
      variable = variable,
      year     = as.integer(substr(yyyymm, 1L, 4L)),
      month    = as.integer(substr(yyyymm, 5L, 6L))
    ) %>%
    filter(!is.na(value), value > -99)
}


# -- Paste this block into the body of your pipeline script ------------------
# (after the existing SNODAS download step; mirrors Yakima's NCEI step)

cat("\nStep N: Fetching NCEI WA statewide climate...\n")

ncei_wa_file         <- file.path(data_dir, "ncei_climate_monthly_wa.csv")
ncei_wa_needs_update <- FALSE
today                <- Sys.Date()
current_month        <- format(today, "%Y-%m")
day_of_month         <- as.integer(format(today, "%d"))

if (day_of_month >= 10L) {
  if (!file.exists(ncei_wa_file)) {
    cat("  NCEI WA CSV missing -- will fetch\n")
    ncei_wa_needs_update <- TRUE
  } else {
    existing_wa <- tryCatch(
      read.csv(ncei_wa_file, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    if (is.null(existing_wa) || nrow(existing_wa) == 0L) {
      cat("  NCEI WA CSV empty or unreadable -- will fetch\n")
      ncei_wa_needs_update <- TRUE
    } else {
      last_modified_wa    <- file.info(ncei_wa_file)$mtime
      updated_this_month  <- format(last_modified_wa, "%Y-%m") == current_month
      if (updated_this_month) {
        cat(sprintf("  NCEI WA already updated this month (%s) -- skipping\n",
                    current_month))
      } else {
        cat(sprintf("  NCEI WA not yet updated for %s -- will fetch\n",
                    current_month))
        ncei_wa_needs_update <- TRUE
      }
    }
  }
} else {
  cat(sprintf("  NCEI update not expected until ~10th (today is %s) -- skipping\n",
              format(today, "%b %d")))
}

if (ncei_wa_needs_update) {
  ncei_wa_raw <- tryCatch({
    tavg_wa <- fetch_ncei_statewide_wa("tavg")
    Sys.sleep(0.5)
    pcp_wa  <- fetch_ncei_statewide_wa("pcp")
    bind_rows(tavg_wa, pcp_wa)
  }, error = function(e) {
    cat("  ERROR fetching NCEI WA statewide:", conditionMessage(e), "\n")
    NULL
  })
  
  if (!is.null(ncei_wa_raw) && nrow(ncei_wa_raw) > 0L) {
    write_csv(ncei_wa_raw, ncei_wa_file)
    cat(sprintf("  Saved %d rows  (tavg + pcp, 2000-%d)\n",
                nrow(ncei_wa_raw), max(ncei_wa_raw$year)))
  } else {
    if (file.exists(ncei_wa_file)) {
      cat("  WARNING: fetch failed -- retaining existing CSV\n")
    } else {
      cat("  WARNING: fetch failed and no existing CSV -- NCEI WA data unavailable\n")
    }
  }
}


# ==============================================================================
# GITHUB ACTIONS YML — changes needed
#
# 1. In the "Install R packages" step, add 'httr' to the pkgs vector if it
#    isn't already there (it should be, since you use it for SNODAS fetching).
#
# 2. In the "Deploy to shinyapps.io" step, add this line to the appFiles vector:
#       'data/ncei_climate_monthly_wa.csv',
#
# 3. In the "Commit data files" step, no changes needed -- the existing
#    `git add data/*.csv` will pick up the new ncei_climate_monthly_wa.csv.
#
# Also: you'll need to run the pipeline locally once first to create the
# initial ncei_climate_monthly_wa.csv before the next GHA run, since the
# file won't exist in the repo yet. The easiest way:
#
#   source("your_pipeline_script.R")   # from RStudio with day_of_month forced:
#   # or just run the fetch block manually:
#   ncei_wa_needs_update <- TRUE        # force it
#   # ... then run the if(ncei_wa_needs_update) block
#
# Then commit data/ncei_climate_monthly_wa.csv to the repo.
# ==============================================================================