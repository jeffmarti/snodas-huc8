# snodas-huc8

Automated daily SNODAS Snow Water Equivalent (SWE) analysis by HUC8 watershed,
with a complete backfill to the SNODAS period of record (October 2003).

A companion Shiny app provides interactive visualization of current conditions,
historical climatology, and annual snowpack metrics for 71 HUC8 watersheds in
Washington State.

🔗 **Live app:** [waterwater.shinyapps.io/snodas-huc8](https://waterwater.shinyapps.io/snodas-huc8)

---

## What this repo does

Every night, a GitHub Actions workflow:

1. Downloads the latest [SNODAS](https://nsidc.org/data/g02158) SWE raster from NSIDC
2. Computes mean SWE and snow water **volume in acre-feet** for each of 71 HUC8
   watersheds falling within Washington State
3. Appends the result to `data/snodas_huc8_history.csv` and commits it back to this repo

The `data/` folder is a growing community dataset — the CSV is directly readable
from R, Python, or any tool that handles CSV files.

---

## Shiny App

The app has three tabs:

### 🗓️ Date Explorer
An interactive Leaflet choropleth map showing SWE volume by watershed for any
date in the record. Key features:
- Select any two dates and toggle between Current, Comparison, and Difference map views
- Summary bar showing total SWE change across all 71 watersheds between the two dates
- Clickable watershed popups with SWE depth, volume, and area
- Buttons to allow quick comparison dates
- Sortable data table with CSV download

### 📈 Climatology
Two charts for understanding how a selected watershed's current snowpack compares
to the historical record:

**SWE Climatology ribbon chart** — daily SWE volume for the current water year
plotted against historical percentile ribbons (min/max, 10th–90th, 25th–75th).
A % of Normal badge summarizes current conditions relative to the historical median.

**Monthly SWE Change chart** — shows the snowpack as a natural reservoir that
fills and drains across the water year. Each bar represents the median monthly
gain or loss in SWE volume (Oct→Nov through Aug→Sep transitions). Blue bars are
gaining months; red bars are losing months. A Pareto line on the right axis shows
the cumulative percentage of total seasonal accumulation by month. Lollipop dots
show how the current water year's monthly changes compare to the historical median —
blue dots indicate stronger-than-median gains (or shallower losses), red dots
indicate weaker performance.

### ☃️ Peak & Melt-Out
Two panels showing year-to-year variability in the two most operationally
significant snowpack events:
- **Peak SWE panel** — the highest daily SWE volume recorded in each water year,
  with hover showing the exact date the peak occurred
- **Melt-Out panel** — the last date in each water year when SWE was greater than
  zero (the true snow-free date), expressed as a water-year day-of-year with
  calendar date shown on hover

CSV download available for the full annual metrics table.

---

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

**`cache/huc8_base.rds`** — pre-processed HUC8 boundary polygons (simplified at
dTolerance = 1.0, ~9.5 MB) in WGS84, ready for use with Leaflet. Generated once
from the USGS WBD and committed to the repo to avoid repeated processing at app startup.

**`cache/wa_border.rds`** — pre-processed Washington State boundary in WGS84,
used for the map overlay.

**`cache/huc8_centroids.csv`** — pre-computed centroid coordinates for all 71
HUC8s, used for map popup placement.

---

## Volume formula

```
volume_af = ((swe_mean_mm / 25.4) / 12) * AreaAcres
```

---

## Read the data in R

```r
url <- "https://raw.githubusercontent.com/jeffmarti/snodas-huc8/main/data/snodas_huc8_history.csv"
history <- read.csv(url)
```

---

## Repo structure

```
snodas-huc8/
├── app.R                        # Shiny app (3 tabs)
├── scripts/                     # Pipeline scripts (not auto-sourced by Shiny)
│   └── snodas_functions.R       # Core SNODAS download and zonal stats functions
├── data/
│   └── snodas_huc8_history.csv  # Growing daily record, Oct 2003–present
├── cache/
│   ├── huc8_base.rds            # Pre-processed HUC8 boundaries
│   ├── wa_border.rds            # Pre-processed WA state boundary
│   └── huc8_centroids.csv       # Pre-computed centroids
└── .github/workflows/           # GitHub Actions pipeline
```

> **Note:** The pipeline scripts live in `scripts/` rather than `R/` to prevent
> Shiny from auto-sourcing them at app startup.

---

## Adapt for your own region

This repo is designed to be easily adapted for any HUC8 region in the
continental US. To change the study area:

1. Edit `get_huc8_wa()` in `scripts/snodas_functions.R` — replace the Washington
   State filter with your region of interest
2. Delete the cached `.rds` files and let them regenerate on first run
3. Update the README

SNODAS covers the contiguous US. The zonal stats pipeline works for any
HUC8 polygon set within that extent.

---

## App requirements

```r
install.packages(c("shiny", "leaflet", "sf", "dplyr",
                   "DT", "plotly"))
```

## Pipeline requirements

```r
install.packages(c("terra", "sf", "exactextractr", "httr",
                   "dplyr", "nhdplusTools", "tigris"))
```

---

## Data sources

- **SNODAS SWE:** [NSIDC G02158](https://nsidc.org/data/g02158) — National Snow
  and Ice Data Center, NOAA National Weather Service
- **HUC8 boundaries:** [USGS Watershed Boundary Dataset](https://www.usgs.gov/national-hydrography/watershed-boundary-dataset)
  via `nhdplusTools`

---

## Coverage notes

SNODAS coverage is incomplete for some transboundary basins along the
Canadian border. Known gaps in the Washington dataset:

| Basin | Approximate SNODAS Coverage |
|---|---|
| Sumas River | ~79% |
| Nooksack | ~92% |
| Upper Skagit | ~99% |

SWE volumes for these basins are underestimates relative to their true area.
---

## Data History

### April 14, 2026: GEE Backfill Upgrade
The SNODAS HUC8 history file (`data/snodas_huc8_history.csv`) was replaced 
with a Google Earth Engine (GEE)-derived dataset covering the full period 
of record (2003-10-01 to present).

**Key improvements:**
- Applied NOHRSC repair mask for 2014-10-09 to 2019-10-10 period, 
  correcting a known SNODAS land/water mask error
- Removed San Juan Islands HUC (17110019) which had no valid SNODAS 
  coverage (previously retained as NA placeholder rows)
- Identified and corrected a significant data artifact in Upper Skagit 
  (2019-01-05: 1.99M AF → 417k AF)

**Validation:**
- Correlation with original dataset: 1.000
- Average difference: 117 AF
- Weighted mean difference: 0.6%

**Archive files** retained in Google Drive (`snodas_gee_exports` folder).

Daily updates continue via GitHub Actions pipeline pulling directly 
from NOHRSC.
---

## License

Data: Public domain (SNODAS and WBD are US federal government products)
Code: MIT
