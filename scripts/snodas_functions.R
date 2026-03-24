# ==============================================================================
# snodas_functions.R
#
# Shared functions for SNODAS SWE download, extraction, raster building,
# and HUC8 zonal statistics. Sourced by both backfill.R and daily_update.R.
#
# Data source: https://noaadata.apps.nsidc.org/NOAA/G02158/masked/
# ==============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(exactextractr)
  library(httr)
  library(dplyr)
})

BASE_URL <- "https://noaadata.apps.nsidc.org/NOAA/G02158/masked"

# ------------------------------------------------------------------------------
# get_huc8_wa()
# Load HUC8 boundaries from cache, or download via nhdplusTools.
# The cache file should be committed to the repo under cache/.
# ------------------------------------------------------------------------------

get_huc8_wa <- function(cache_path) {
  if (file.exists(cache_path)) {
    message("Loading HUC8 from cache...")
    return(sf::st_read(cache_path, quiet = TRUE))
  }

  message("Downloading HUC8 via nhdplusTools...")

  wa <- tigris::states(cb = TRUE, progress_bar = FALSE) |>
    dplyr::filter(STUSPS == "WA") |>
    sf::st_transform(4326)

  huc8 <- nhdplusTools::get_huc(AOI = wa, t_srs = 4326, type = "huc08")

  # Standardise column names -- skip geometry column
  geom_col <- attr(huc8, "sf_column")
  non_geom  <- names(huc8)[names(huc8) != geom_col]
  names(huc8)[names(huc8) != geom_col] <- tools::toTitleCase(tolower(non_geom))

  name_map <- c(
    "Huc8"      = "HUC8",
    "Name"      = "Name",
    "States"    = "States",
    "Areasqkm"  = "AreaSqKm",
    "Areaacres" = "AreaAcres"
  )
  for (old in names(name_map)) {
    if (old %in% names(huc8))
      names(huc8)[names(huc8) == old] <- name_map[[old]]
  }

  message(sprintf("  Downloaded: %d HUC8 polygons", nrow(huc8)))

  if (!dir.exists(dirname(cache_path)))
    dir.create(dirname(cache_path), recursive = TRUE)
  sf::st_write(huc8, cache_path, quiet = TRUE)
  message(sprintf("  Cached to: %s", basename(cache_path)))

  huc8
}

# ------------------------------------------------------------------------------
# check_snodas_url()
# Returns TRUE if the SNODAS tar file exists for a given date.
# ------------------------------------------------------------------------------

check_snodas_url <- function(date, base_url = BASE_URL) {
  yr       <- format(date, "%Y")
  mon_num  <- format(date, "%m")
  mon_name <- format(date, "%b")
  dy       <- format(date, "%d")
  filename <- sprintf("SNODAS_%s%s%s.tar", yr, mon_num, dy)
  file_url <- sprintf("%s/%s/%s_%s/%s", base_url, yr, mon_num, mon_name, filename)

  resp <- tryCatch(HEAD(file_url, timeout(20)), error = function(e) NULL)
  if (!is.null(resp) && status_code(resp) == 200)
    return(list(available = TRUE,  url = file_url, filename = filename))
  else
    return(list(available = FALSE, url = file_url, filename = filename))
}

# ------------------------------------------------------------------------------
# find_latest_snodas()
# Scan back up to 4 days to find the most recent available SNODAS file.
# ------------------------------------------------------------------------------

find_latest_snodas <- function(base_url = BASE_URL) {
  message("Searching for latest SNODAS file on NSIDC...")
  for (lag in 0:4) {
    d      <- Sys.Date() - lag
    result <- check_snodas_url(d, base_url)
    if (result$available) {
      message(sprintf("  Found: %s", result$filename))
      return(list(date = d, url = result$url, filename = result$filename))
    }
    message(sprintf("  %s not available, trying previous day...",
                    format(d, "%Y-%m-%d")))
  }
  stop("Could not find a recent SNODAS file after checking 4 days.")
}

# ------------------------------------------------------------------------------
# download_and_extract_snodas()
# Downloads the SNODAS tar for a given date info list, extracts SWE product.
# Returns list with paths needed by read_snodas_swe().
# Cleans up tar and extracted files if cleanup = TRUE.
# ------------------------------------------------------------------------------

download_and_extract_snodas <- function(snodas_info, work_dir, cleanup = FALSE) {

  if (!dir.exists(work_dir)) dir.create(work_dir, recursive = TRUE)

  tar_path <- file.path(work_dir, snodas_info$filename)

  if (!file.exists(tar_path)) {
    resp <- GET(snodas_info$url,
                write_disk(tar_path, overwrite = TRUE),
                timeout(300))
    if (http_error(resp)) stop("Download failed: HTTP ", status_code(resp))
  }

  extract_dir <- file.path(work_dir, "extracted",
                           format(snodas_info$date, "%Y%m%d"))
  if (!dir.exists(extract_dir)) dir.create(extract_dir, recursive = TRUE)

  untar(tar_path, exdir = extract_dir)

  inner_tars <- list.files(extract_dir, pattern = "\\.tar$",
                           full.names = TRUE, recursive = TRUE)
  swe_tar    <- inner_tars[grepl("1034", inner_tars)]
  if (length(swe_tar) > 0)
    untar(swe_tar[1], exdir = extract_dir)

  all_files <- list.files(extract_dir, recursive = TRUE, full.names = TRUE)
  gz_file   <- all_files[grepl("1034", all_files) & grepl("\\.dat\\.gz$", all_files)]
  dat_file  <- all_files[grepl("1034", all_files) & grepl("\\.dat$", all_files) &
                           !grepl("\\.dat\\.gz$", all_files)]

  if (length(dat_file) > 0) {
    result <- list(extract_dir = extract_dir,
                   dat_file    = dat_file[1],
                   gz_file     = if (length(gz_file) > 0) gz_file[1] else NULL,
                   compressed  = FALSE,
                   tar_path    = tar_path,
                   cleanup     = cleanup)
  } else if (length(gz_file) > 0) {
    result <- list(extract_dir = extract_dir,
                   dat_file    = NULL,
                   gz_file     = gz_file[1],
                   compressed  = TRUE,
                   tar_path    = tar_path,
                   cleanup     = cleanup)
  } else {
    stop("SWE data file (product 1034) not found.")
  }

  result
}

# ------------------------------------------------------------------------------
# read_snodas_swe()
# Reads binary SWE raster from extracted SNODAS files.
# Returns a terra SpatRaster in WGS84 with values in mm.
# ------------------------------------------------------------------------------

read_snodas_swe <- function(extracted) {

  if (extracted$compressed) {
    dat_file <- sub("\\.gz$", "", extracted$gz_file)
    if (!file.exists(dat_file)) {
      gz_con  <- gzfile(extracted$gz_file, "rb")
      raw_dat <- readBin(gz_con, "raw", n = 4096L * 8192L * 2L)
      close(gz_con)
      writeBin(raw_dat, dat_file)
    }
  } else {
    dat_file <- extracted$dat_file
  }

  txt_gz <- paste0(tools::file_path_sans_ext(dat_file), ".txt.gz")
  if (!file.exists(txt_gz) && !is.null(extracted$gz_file))
    txt_gz <- sub("\\.dat\\.gz$", ".txt.gz", extracted$gz_file)

  scale <- NA_real_

  if (file.exists(txt_gz)) {
    con <- gzcon(file(txt_gz, "rb"))
    hdr <- readLines(con)
    close(con)

    get_hdr <- function(key) {
      line <- hdr[grepl(key, hdr, ignore.case = TRUE)]
      if (length(line) == 0) return(NA_real_)
      as.numeric(trimws(gsub(".*:", "", line[1])))
    }

    nrows <- as.integer(get_hdr("Number of rows"))
    ncols <- as.integer(get_hdr("Number of columns"))
    xmin  <- get_hdr("Minimum x-axis coordinate")
    xmax  <- get_hdr("Maximum x-axis coordinate")
    ymin  <- get_hdr("Minimum y-axis coordinate")
    ymax  <- get_hdr("Maximum y-axis coordinate")
    scale <- get_hdr("Scaling factor")

    if (any(is.na(c(nrows, ncols, xmin, xmax, ymin, ymax))))
      stop("Could not parse all grid parameters from header.")

  } else {
    message("  Header not found -- using national grid constants")
    nrows <- 4096L; ncols <- 8192L
    xmin  <- -130.516666666667; xmax <- -62.25
    ymin  <-   24.0999999999998; ymax <- 58.2333333333333
  }

  nodata <- -9999L
  con  <- file(dat_file, "rb")
  vals <- readBin(con, integer(), n = nrows * ncols,
                  size = 2L, signed = TRUE, endian = "big")
  close(con)

  r <- rast(nrows = nrows, ncols = ncols,
            xmin = xmin, xmax = xmax,
            ymin = ymin, ymax = ymax,
            crs  = "EPSG:4326")
  values(r) <- matrix(vals, nrow = nrows, ncol = ncols, byrow = TRUE)

  r[r == nodata] <- NA

  if (!is.na(scale) && scale != 0) {
    r <- r * scale
  }

  names(r) <- "swe_mm"
  r
}

# ------------------------------------------------------------------------------
# compute_huc8_swe()
# Zonal statistics: mean/min/max SWE per HUC8, plus acre-feet volume.
# Returns a plain data frame (no geometry).
# ------------------------------------------------------------------------------

compute_huc8_swe <- function(swe_raster, huc8, swe_date) {

  if (!isTRUE(st_crs(huc8) == st_crs("EPSG:4326")))
    huc8 <- st_transform(huc8, 4326)

  swe_crop <- crop(swe_raster, ext(vect(huc8)))

  swe_mean <- exact_extract(swe_crop, huc8, "mean", progress = FALSE)
  swe_min  <- exact_extract(swe_crop, huc8, "min",  progress = FALSE)
  swe_max  <- exact_extract(swe_crop, huc8, "max",  progress = FALSE)

 result <- huc8 %>%
    st_drop_geometry() %>%
    rename(
      HUC8     = huc8,
      Name     = name,
      States   = states,
      AreaSqKm = area_km2_clipped
    ) %>%
    mutate(AreaAcres = AreaSqKm * 247.105) %>%
    select(HUC8, Name, States, AreaSqKm, AreaAcres) %>%
    select(HUC8, Name, States, AreaSqKm, AreaAcres) %>%
    cbind(swe_mean_mm = round(swe_mean, 2)) %>%
    cbind(swe_min_mm  = round(swe_min,  2)) %>%
    cbind(swe_max_mm  = round(swe_max,  2)) %>%
    mutate(
      swe_date = format(as.Date(swe_date), "%Y-%m-%d"),
      swe_mean_in    = round(swe_mean_mm / 25.4, 3),
      swe_volume_af  = round(((swe_mean_mm / 25.4) / 12) * AreaAcres, 0),
      swe_volume_kaf = round(swe_volume_af / 1000, 2)
    ) %>%
    select(HUC8, Name, States, AreaSqKm, AreaAcres,
           swe_date, swe_mean_mm, swe_mean_in,
           swe_min_mm, swe_max_mm,
           swe_volume_af, swe_volume_kaf)

  result
}

# ------------------------------------------------------------------------------
# cleanup_snodas()
# Remove downloaded tar and extracted directory to free disk space.
# Called after successful zonal stats computation.
# ------------------------------------------------------------------------------

cleanup_snodas <- function(extracted) {
  if (isTRUE(extracted$cleanup)) {
    if (!is.null(extracted$tar_path) && file.exists(extracted$tar_path))
      file.remove(extracted$tar_path)
    if (!is.null(extracted$extract_dir) && dir.exists(extracted$extract_dir))
      unlink(extracted$extract_dir, recursive = TRUE)
  }
}

# ------------------------------------------------------------------------------
# process_one_date()
# Full pipeline for a single date: download → extract → raster → zonal stats
# → cleanup. Returns a one-row-per-HUC8 data frame, or NULL on failure.
# ------------------------------------------------------------------------------

process_one_date <- function(date, huc8, work_dir, cleanup = TRUE) {

  date_str <- format(date, "%Y-%m-%d")

  snodas_info <- tryCatch({
    result <- check_snodas_url(date)
    if (!result$available) {
      message(sprintf("  [SKIP] %s -- not available on NSIDC", date_str))
      return(NULL)
    }
    list(date = date, url = result$url, filename = result$filename)
  }, error = function(e) {
    message(sprintf("  [ERROR] %s -- URL check failed: %s", date_str, e$message))
    return(NULL)
  })

  if (is.null(snodas_info)) return(NULL)

  extracted <- tryCatch(
    download_and_extract_snodas(snodas_info, work_dir, cleanup = cleanup),
    error = function(e) {
      message(sprintf("  [ERROR] %s -- download/extract failed: %s",
                      date_str, e$message))
      NULL
    }
  )

  if (is.null(extracted)) return(NULL)

  swe_raster <- tryCatch(
    read_snodas_swe(extracted),
    error = function(e) {
      message(sprintf("  [ERROR] %s -- raster read failed: %s",
                      date_str, e$message))
      cleanup_snodas(extracted)
      NULL
    }
  )

  if (is.null(swe_raster)) return(NULL)

  result <- tryCatch(
    compute_huc8_swe(swe_raster, huc8, date),
    error = function(e) {
      message(sprintf("  [ERROR] %s -- zonal stats failed: %s",
                      date_str, e$message))
      NULL
    }
  )

  cleanup_snodas(extracted)
  result
}
