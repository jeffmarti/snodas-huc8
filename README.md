# snodas-huc8

Automated daily SNODAS Snow Water Equivalent (SWE) analysis by HUC8 watershed, 
with a complete backfill to the SNODAS period of record (September 2003).

## What this repo does

Every night, a GitHub Actions workflow:
1. Downloads the latest [SNODAS](https://nsidc.org/data/g02158) SWE raster from NSIDC
2. Computes mean SWE and snow water **volume in acre-feet** for each HUC8 watershed
3. Appends the result to `data/snodas_huc8_history.csv` and commits it back to this repo

The `data/` folder is a growing community dataset — the CSV is directly readable 
from R, Python, or any tool that handles CSV files.

## Data

**`data/snodas_huc8_history.csv`** — one row per HUC8 per date

| Column | Description |
|---|---|
| `HUC8` | 8-digit HUC code |
| `Name` | Watershed name |
| `States` | States intersected |
| `AreaSqKm` | Watershed area (km²) |
| `AreaAcres` | Watershed area (acres) |
| `swe_date` | Date (YYYY-MM-DD) |
| `swe_mean_mm` | Mean SWE depth (mm) |
| `swe_mean_in` | Mean SWE depth (inches) |
| `swe_min_mm` | Minimum SWE cell value (mm) |
| `swe_max_mm` | Maximum SWE cell value (mm) |
| `swe_volume_af` | Snow water volume (acre-feet) |
| `swe_volume_kaf` | Snow water volume (thousand acre-feet) |

**Backfill schedule:** The 1st and 15th of every month from October 2003 to 
present are being loaded nightly. Once complete, the daily job takes over.

## Volume formula

```
volume_af = ((swe_mean_mm / 25.4) / 12) * AreaAcres
```

## Read the data in R

```r
url <- "https://raw.githubusercontent.com/YOUR_USERNAME/snodas-huc8/main/data/snodas_huc8_history.csv"
history <- read.csv(url)
```

## Adapt for your own region

This repo is designed to be easily adapted for any HUC8 region in the 
continental US. To change the study area:

1. Edit `get_huc8_wa()` in `R/snodas_functions.R` — replace the Washington 
   State filter with your region of interest
2. Delete `cache/HUC8_WA_WGS84.gpkg` and let it regenerate on first run
3. Update the README

SNODAS covers the contiguous US. The zonal stats pipeline works for any 
HUC8 polygon set within that extent.

## Data sources

- **SNODAS SWE:** [NSIDC G02158](https://nsidc.org/data/g02158) — National Snow 
  and Ice Data Center, NOAA National Weather Service
- **HUC8 boundaries:** [USGS Watershed Boundary Dataset](https://www.usgs.gov/national-hydrography/watershed-boundary-dataset) 
  via `nhdplusTools`

## Requirements

```r
install.packages(c("terra", "sf", "exactextractr", "httr", 
                   "dplyr", "nhdplusTools", "tigris"))
```

## License

Data: Public domain (SNODAS and WBD are US federal government products)  
Code: MIT
