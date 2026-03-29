# ==============================================================================
# app.R
# SNODAS HUC8 SWE Comparison Shiny App
#
# Tab 1: Date Explorer -- leaflet map with SWE choropleth
# Tab 2: Climatology  -- SWE percentile ribbon chart with hover box + CSV download
# Tab 3: Peak & Melt-Out -- annual peak SWE and melt-out date by water year
#
# Data source: https://github.com/jeffmarti/snodas-huc8
# Deploy to  : waterwater.shinyapps.io
#
# CHANGES:
#   - HUC8 boundaries and WA border now loaded from pre-processed .rds files
#     (huc8_base.rds, wa_border.rds) instead of raw .gpkg/.geojson files.
#     This skips st_read() + st_transform() at startup, reducing memory usage.
#     huc8_base.rds is simplified at dTolerance=1.0 (~9.5MB vs original 26MB).
#   - Added Tab 3: Peak & Melt-Out showing annual peak SWE volume and last
#     snow-free date (melt-out) per watershed per water year, with CSV download.
# ==============================================================================


library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
library(plotly)

# ------------------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------------------

HISTORY_URL   <- "https://raw.githubusercontent.com/jeffmarti/snodas-huc8/main/data/snodas_huc8_history.csv"
CENTROIDS_URL <- "https://raw.githubusercontent.com/jeffmarti/snodas-huc8/main/cache/huc8_centroids.csv"

INIT_LNG  <- -120.5
INIT_LAT  <-   47.5
INIT_ZOOM <-    6

# Water year: Oct 1 = day 1
current_wy <- if (as.integer(format(Sys.Date(), "%m")) >= 10) {
  as.integer(format(Sys.Date(), "%Y")) + 1
} else {
  as.integer(format(Sys.Date(), "%Y"))
}

# ------------------------------------------------------------------------------
# LOAD DATA (once at startup)
# ------------------------------------------------------------------------------

message("Loading SNODAS history CSV...")
history <- read.csv(HISTORY_URL, stringsAsFactors = FALSE) %>%
  select(HUC8, swe_date, swe_mean_in, swe_mean_mm, swe_volume_af, swe_volume_kaf)
history$HUC8     <- as.character(history$HUC8)
history$swe_date <- as.Date(history$swe_date)

message("Loading HUC8 boundaries (pre-processed rds)...")
huc8_base <- readRDS("cache/huc8_base.rds")   # simplified + transformed, ready to use

message("Loading WA state boundary (pre-processed rds)...")
wa_border <- readRDS("cache/wa_border.rds")   # transformed, ready to use

# Load pre-computed centroids for popups
huc8_centroids <- read.csv(CENTROIDS_URL, stringsAsFactors = FALSE)

gc()

# Available dates (ascending for calendar)
available_dates <- sort(unique(history$swe_date), decreasing = FALSE)

# Compute missing dates so the calendar can grey them out
full_range    <- seq(min(available_dates), max(available_dates), by = "day")
missing_dates <- as.character(full_range[!full_range %in% available_dates])

message(sprintf("Ready: %d dates, %d HUC8s", length(available_dates), nrow(huc8_base)))

# ------------------------------------------------------------------------------
# CLIMATOLOGY: pre-compute water year day
# ------------------------------------------------------------------------------

history <- history %>%
  mutate(
    month  = as.integer(format(swe_date, "%m")),
    year   = as.integer(format(swe_date, "%Y")),
    wy     = ifelse(month >= 10, year + 1, year),
    wy_doy = as.integer(swe_date - as.Date(ifelse(month >= 10,
                                                  paste0(year,     "-10-01"),
                                                  paste0(year - 1, "-10-01")))) + 1
  )

# Basin lookup for grouped dropdown
huc4_labels <- c(
  "1711" = "Puget Sound",
  "1710" = "Coastal",
  "1708" = "Lower Columbia",
  "1707" = "Middle Columbia",
  "1706" = "Lower Snake",
  "1703" = "Yakima",
  "1702" = "Upper Columbia",
  "1701" = "Spokane / Pend Oreille"
)

basin_lookup <- history %>%
  select(HUC8) %>%
  distinct() %>%
  left_join(huc8_base %>% st_drop_geometry() %>% select(HUC8, Name, AreaAcres),
            by = "HUC8") %>%
  mutate(
    huc4  = substr(HUC8, 1, 4),
    group = huc4_labels[huc4]
  ) %>%
  arrange(group, Name)

grouped_choices <- split(
  setNames(basin_lookup$HUC8, basin_lookup$Name),
  basin_lookup$group
)

# ------------------------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------------------------

get_map_data <- function(date_val) {
  swe <- history %>%
    filter(swe_date == as.Date(date_val)) %>%
    select(HUC8, swe_mean_in, swe_mean_mm, swe_volume_af, swe_volume_kaf)
  huc8_base %>% left_join(swe, by = "HUC8")
}

get_table_data <- function(cur_date, cmp_date) {
  cur <- history %>%
    filter(swe_date == as.Date(cur_date)) %>%
    select(HUC8, cur_af = swe_volume_af, cur_in = swe_mean_in)
  cmp <- history %>%
    filter(swe_date == as.Date(cmp_date)) %>%
    select(HUC8, cmp_af = swe_volume_af, cmp_in = swe_mean_in)
  huc8_base %>%
    st_drop_geometry() %>%
    select(HUC8, Name, AreaAcres) %>%
    left_join(cur, by = "HUC8") %>%
    left_join(cmp, by = "HUC8") %>%
    mutate(
      cur_af   = ifelse(is.na(cur_af), 0, cur_af),
      cmp_af   = ifelse(is.na(cmp_af), 0, cmp_af),
      diff_af  = cur_af - cmp_af,
      diff_pct = ifelse(cmp_af > 0, (diff_af / cmp_af) * 100, NA)
    ) %>%
    arrange(desc(cur_af))
}

swe_popup <- function(d, date_label) {
  sprintf(
    paste0("<b>%s</b><br/><small>%s</small><br/>HUC8: %s<br/>",
           "<hr style='margin:4px 0'>",
           "SWE Mean: %.1f in | %.0f mm<br/>Volume: %s AF<br/>Area: %s acres"),
    d$Name, date_label, d$HUC8,
    ifelse(is.na(d$swe_mean_in), 0, d$swe_mean_in),
    ifelse(is.na(d$swe_mean_mm), 0, d$swe_mean_mm),
    formatC(ifelse(is.na(d$swe_volume_af), 0, d$swe_volume_af),
            format = "f", digits = 0, big.mark = ","),
    formatC(d$AreaAcres, format = "f", digits = 0, big.mark = ",")
  )
}

diff_popup <- function(d, cur_label, cmp_label) {
  diff_af <- ifelse(is.na(d$diff_af), 0, d$diff_af)
  diff_in <- ifelse(is.na(d$diff_in), 0, d$diff_in)
  diff_mm <- ifelse(is.na(d$diff_mm), 0, d$diff_mm)
  sprintf(
    paste0("<b>%s</b><br/><small>%s vs %s</small><br/>HUC8: %s<br/>",
           "<hr style='margin:4px 0'>",
           "Volume change: <b>%s AF</b><br/>",
           "SWE change: %+.1f in | %+.0f mm<br/>Area: %s acres"),
    d$Name, cur_label, cmp_label, d$HUC8,
    formatC(diff_af, format = "f", digits = 0, big.mark = ",", flag = "+"),
    diff_in, diff_mm,
    formatC(d$AreaAcres, format = "f", digits = 0, big.mark = ",")
  )
}

get_clim_data <- function(huc_id) {
  df <- history %>%
    filter(HUC8 == huc_id, !is.na(swe_volume_af)) %>%
    select(swe_date, wy, wy_doy, swe_volume_af, swe_volume_kaf)
  
  latest_wy  <- max(df$wy)
  display_wy <- if (any(df$wy == current_wy)) current_wy else latest_wy
  
  hist_df <- df %>% filter(wy != display_wy)
  
  ribbons <- hist_df %>%
    group_by(wy_doy) %>%
    summarise(
      p00  = min(swe_volume_af,            na.rm = TRUE),
      p10  = quantile(swe_volume_af, 0.10, na.rm = TRUE),
      p25  = quantile(swe_volume_af, 0.25, na.rm = TRUE),
      p50  = quantile(swe_volume_af, 0.50, na.rm = TRUE),
      p75  = quantile(swe_volume_af, 0.75, na.rm = TRUE),
      p90  = quantile(swe_volume_af, 0.90, na.rm = TRUE),
      p100 = max(swe_volume_af,            na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(wy_doy)
  
  current_df <- df %>% filter(wy == display_wy) %>% arrange(wy_doy)
  
  list(ribbons = ribbons, current = current_df,
       hist = hist_df, display_wy = display_wy)
}
get_monthly_profile <- function(huc_id) {
  
  month_labels <- c("10"="Oct","11"="Nov","12"="Dec",
                    "1"="Jan", "2"="Feb", "3"="Mar",
                    "4"="Apr", "5"="May", "6"="Jun",
                    "7"="Jul", "8"="Aug", "9"="Sep")
  
  # Month pairs for 11 deltas: Oct->Nov through Aug->Sep
  from_months <- c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8)
  to_months   <- c(11, 12,  1, 2, 3, 4, 5, 6, 7, 8, 9)
  
  # 1st-of-month values, historical WYs only
  base_df <- history %>%
    filter(HUC8 == huc_id,
           wy < current_wy,
           !is.na(swe_volume_af),
           as.integer(format(swe_date, "%d")) == 1) %>%
    select(wy, month_num = month, swe_volume_af)
  
  # Compute delta for each WY x month pair
  delta_rows <- lapply(unique(base_df$wy), function(yr) {
    yr_df <- base_df %>% filter(wy == yr)
    deltas <- mapply(function(f, t) {
      fv <- yr_df$swe_volume_af[yr_df$month_num == f]
      tv <- yr_df$swe_volume_af[yr_df$month_num == t]
      if (length(fv) == 0 || length(tv) == 0) NA_real_ else tv - fv
    }, from_months, to_months)
    data.frame(wy = yr, month_num = from_months, delta_af = deltas,
               stringsAsFactors = FALSE)
  })
  
  delta_df <- bind_rows(delta_rows)
  
  # Median delta per month
  med_deltas <- delta_df %>%
    group_by(month_num) %>%
    summarise(median_delta = median(delta_af, na.rm = TRUE), .groups = "drop")
  
  # Build ordered profile + Pareto
  profile <- data.frame(month_num = from_months, stringsAsFactors = FALSE) %>%
    left_join(med_deltas, by = "month_num") %>%
    mutate(
      month_lbl    = factor(month_labels[as.character(month_num)],
                            levels = month_labels[as.character(from_months)]),
      pos_contrib  = pmax(median_delta, 0),
      total_pos    = sum(pos_contrib, na.rm = TRUE),
      cum_pct      = cumsum(pos_contrib) / total_pos * 100
    )
  
  # Current WY deltas for completed month pairs
  cur_df <- history %>%
    filter(HUC8 == huc_id,
           wy == current_wy,
           !is.na(swe_volume_af),
           as.integer(format(swe_date, "%d")) == 1) %>%
    select(month_num = month, cur_af = swe_volume_af)
  
  cur_deltas <- mapply(function(f, t) {
    fv <- cur_df$cur_af[cur_df$month_num == f]
    tv <- cur_df$cur_af[cur_df$month_num == t]
    if (length(fv) == 0 || length(tv) == 0) NA_real_ else tv - fv
  }, from_months, to_months)
  
  cur_delta_df <- data.frame(month_num    = from_months,
                             current_delta = cur_deltas,
                             stringsAsFactors = FALSE)
  
  profile %>%
    left_join(cur_delta_df, by = "month_num") %>%
    mutate(above_median = !is.na(current_delta) & current_delta >= median_delta)
}
get_annual_metrics <- function(huc_id) {
  df <- history %>%
    filter(HUC8 == huc_id, !is.na(swe_volume_af), wy < current_wy)
  
  # Peak SWE per water year
  peak_df <- df %>%
    group_by(wy) %>%
    slice_max(swe_volume_af, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(wy,
           peak_af     = swe_volume_af,
           peak_date   = swe_date,
           peak_wy_doy = wy_doy)
  
  # Melt-out: last date in the water year when SWE > 0
  meltout_df <- df %>%
    filter(swe_volume_af > 0) %>%
    group_by(wy) %>%
    slice_max(swe_date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(wy,
           meltout_date   = swe_date,
           meltout_wy_doy = wy_doy)
  
  peak_df %>%
    left_join(meltout_df, by = "wy") %>%
    arrange(wy)
}

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; }
      .title-bar {
        background: #1a5276; color: white;
        padding: 12px 20px; margin-bottom: 15px; border-radius: 4px;
      }
      .title-bar h3 { margin: 0; font-size: 20px; }
      .title-bar p  { margin: 4px 0 0 0; font-size: 13px; opacity: 0.85; }
      .summary-bar {
        background: #f0f4f8; border: 1px solid #d0dae4;
        border-radius: 4px; padding: 10px 20px; margin-bottom: 15px;
      }
      .map-title { font-size: 13px; font-weight: bold; color: #444;
                   margin-bottom: 4px; padding-left: 4px; }
      .leaflet-container { border-radius: 4px; }
      .nav-tabs { margin-bottom: 15px; }
      .clim-hover-box {
        position: absolute;
        top: 10px;
        right: 10px;
        background: rgba(255,255,255,0.92);
        border: 1px solid #ccc;
        border-radius: 6px;
        padding: 8px 12px;
        font-size: 12px;
        line-height: 1.7;
        pointer-events: none;
        z-index: 9999;
        min-width: 180px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.12);
      }
      /* --- Info sidebar panel --- */
      .info-panel {
        display: none;
        position: fixed;
        top: 0; right: 0;
        width: 340px; height: 100%;
        background: #fff;
        border-left: 2px solid #1a5276;
        box-shadow: -4px 0 12px rgba(0,0,0,0.15);
        z-index: 99999;
        overflow-y: auto;
        padding: 20px 20px 40px 20px;
        font-size: 13px;
        line-height: 1.6;
      }
      .info-panel h4 {
        color: #1a5276;
        border-bottom: 1px solid #d0dae4;
        padding-bottom: 6px;
        margin-top: 20px;
        margin-bottom: 8px;
        font-size: 14px;
      }
      .info-panel h4:first-of-type { margin-top: 0; }
      .info-panel p, .info-panel li { color: #333; }
      .info-panel ul { padding-left: 16px; margin: 4px 0; }
      .info-panel li { margin-bottom: 4px; }
      .info-overlay {
        display: none;
        position: fixed;
        top: 0; left: 0;
        width: 100%; height: 100%;
        background: rgba(0,0,0,0.25);
        z-index: 99998;
      }
      .info-close-btn {
        float: right;
        background: none;
        border: none;
        font-size: 20px;
        cursor: pointer;
        color: #666;
        line-height: 1;
        margin-top: -4px;
      }
      .info-toggle-btn {
        background: rgba(255,255,255,0.18);
        border: 1px solid rgba(255,255,255,0.5);
        color: white;
        border-radius: 4px;
        padding: 4px 10px;
        font-size: 13px;
        cursor: pointer;
        float: right;
        margin-top: 2px;
      }
      .info-toggle-btn:hover { background: rgba(255,255,255,0.30); }

    ")),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        var $dates = $('#current_date, #compare_date');
        $dates.prop('readonly', true).css('caret-color', 'transparent');
        $(document).on('keydown keypress keyup', '#current_date, #compare_date',
          function(e) { e.preventDefault(); e.stopPropagation(); return false; });
        $(document).on('shiny:value', function() {
          $('#current_date, #compare_date').prop('readonly', true);
        });
      });
      // Info panel toggle
      $(document).on('click', '#info_toggle_btn', function() {
        $('#info_panel, #info_overlay').fadeIn(200);
      });
      $(document).on('click', '#info_close_btn, #info_overlay', function() {
        $('#info_panel, #info_overlay').fadeOut(200);
      });
    "))
  ),
  
  div(class = "title-bar",
      tags$button(id = "info_toggle_btn", class = "info-toggle-btn",
                  "\u2139\ufe0f About & Help"),
      tags$h3("\U0001f3d4\ufe0f Watershed level snow water volumes in Washington State"),
      tags$p("Daily snow water equivalent by HUC8 watershed using SNODAS \u00b7 NOAA - NOHRSC \u00b7 Oct 2003\u2013present")
  ),
  
  # Dim overlay (behind the panel)
  tags$div(id = "info_overlay", class = "info-overlay"),
  
  # Collapsible info panel
  tags$div(id = "info_panel", class = "info-panel",
           
           tags$button(id = "info_close_btn", class = "info-close-btn", HTML("&times;")),
           
           tags$h4("\u2139\ufe0f About This App"),
           tags$p("This application displays daily gridded snow water equivalent (SWE)
            estimates for 71 HUC8 watersheds falling within Washington State, derived from the
            NOAA SNODAS model. The estimates are aggregated to provide an estimate of the volume
            of water stored in the snowpack for each watershed on any given day. The app allows
            users to explore current and historical SWE conditions, compare different dates,
            and examine how current conditions relate to the historical record."),
           
           tags$h4("\U0001f4e1 About SNODAS"),
           tags$p("SNODAS (Snow Data Assimilation System) is produced daily by NOAA's
            National Snow and Ice Data Center (NSIDC). It provides ~1 km gridded
            estimates of snow depth and SWE across the contiguous United States
            by assimilating satellite, airborne, and ground-based snow observations
            into an energy- and mass-balance snowpack model."),
           tags$ul(
             tags$li("Temporal coverage: October 2003 \u2013 present"),
             tags$li("Updated daily, typically by mid-morning Pacific Time"),
             tags$li(tags$a(href="https://nsidc.org/data/g02158", target="_blank",
                            "NSIDC SNODAS product page (G02158)"))
           ),
           
           tags$h4("\U0001f5d3\ufe0f How to Use \u2014 Date Explorer"),
           tags$ul(
             tags$li(tags$b("Current / Primary Date:"), " The main date displayed on the map and table."),
             tags$li(tags$b("Comparison Date:"), " A second date for side-by-side or difference analysis.
                     Use the ", tags$b("Jump to Peak Season"), " dropdown to quickly navigate
                     to April 1st of any year."),
             tags$li(tags$b("Map View:"), " Toggle between Current, Comparison, or Difference
                     (Current minus Comparison) choropleth views."),
             tags$li(tags$b("Swap Dates:"), " Reverses the current and comparison dates instantly."),
             tags$li(tags$b("Clicking a watershed"), " on the map opens a popup with SWE detail.
                     Use ", tags$b("Clear Popup"), " to dismiss it."),
             tags$li(tags$b("Download CSV"), " in the table exports all 71 watersheds for the
                     selected date pair.")
           ),
           
           tags$h4("\U0001f4c8 How to Use \u2014 Climatology"),
           tags$ul(
             tags$li("Select any of the 71 HUC8 watersheds from the dropdown."),
             tags$li("The chart shows historical SWE percentile ribbons (min/max,
               10th\u201390th, 25th\u201375th) computed from all available water years
               except the current one."),
             tags$li("The red line is the current (or most recent) water year."),
             tags$li("Hover over the chart to see exact values in the info box."),
             tags$li("The ", tags$b("% of Normal"), " badge compares the most recent
               SWE value to the historical median for that day of the water year."),
             tags$li("Use ", tags$b("Download Climatology Data (CSV)"),
                     " to export the full percentile table.")
           ),
           tags$h4("\U0001f4ca How to Use \u2014 Monthly SWE Change"),
           tags$p("The lower chart on the Climatology tab shows the snowpack as a
             natural reservoir that fills and drains over the course of the water year.
             Rather than showing SWE levels, it shows how much SWE is ", tags$em("added
             or lost"), " each month."),
           tags$ul(
             tags$li(tags$b("Blue bars (gaining months):"), " The snowpack is typically
               growing during this month. The taller the bar, the more water is being
               stored. For most Washington basins, November through March are the primary
               accumulation months."),
             tags$li(tags$b("Red bars (losing months):"), " The snowpack is typically
               melting faster than it is accumulating during these months. The transition
               from gaining to losing months varies considerably by basin \u2014 lower
               elevation and coastal basins lose snowpack earlier in the season than
               high-elevation interior basins."),
             tags$li(tags$b("Cumulative % line (right axis):"), " Shows what percentage
               of the season\u2019s total accumulation has occurred by the end of each
               month. When the line flattens and plateaus, the accumulation season is
               effectively over and melt dominates."),
             tags$li(tags$b("Lollipop dots (current water year):"), " Each dot shows
               the actual monthly SWE change for the current water year. ",
                     tags$b(style="color:#1a5276;", "Blue dots"), " indicate a stronger
               gain (or shallower loss) than the historical median. ",
                     tags$b(style="color:#922b21;", "Red dots"), " indicate a weaker gain
               or deeper loss than normal. The stem connects the dot to the median bar
               so you can see the magnitude of the departure at a glance."),
             tags$li("Only months with a completed 1st-to-1st transition are shown
               for the current water year \u2014 future months show only the historical
               median bar.")
           ),
           tags$h4("\u2603\ufe0f How to Use \u2014 Peak & Melt-Out"),
           tags$ul(
             tags$li("Select any of the 71 HUC8 watersheds from the dropdown."),
             tags$li(tags$b("Peak SWE panel:"), " The highest daily SWE volume recorded
               in each water year. Hover for the exact date and value."),
             tags$li(tags$b("Melt-Out panel:"), " The last date in each water year when
               SWE was greater than zero \u2014 the true snow-free date."),
             tags$li("Gaps in the melt-out panel indicate years where SWE remained
               above zero through the end of the water year, or the current water year
               is still in progress."),
             tags$li("Use ", tags$b("Download Annual Metrics (CSV)"),
                     " to export the full table of peak and melt-out values.")
           ),
           
           tags$h4("\U0001f4e7 Contact"),
           tags$p(
             "Jeff Marti", tags$br(),
             tags$a(href="mailto:jeffjmarti@gmail.com", "jeffjmarti@gmail.com"), tags$br(),
             tags$a(href="https://github.com/jeffmarti/snodas-huc8", target="_blank",
                    "github.com/jeffmarti/snodas-huc8")
           ),
           
           tags$p(style="color:#aaa; font-size:11px; margin-top:20px;",
                  "App built in R Shiny \u00b7 Data: NOAA NSIDC, USGS WBD")
  ),
  
  tabsetPanel(
    id = "main_tabs",
    
    # ==========================================================================
    # TAB 1: Date Explorer
    # ==========================================================================
    tabPanel("\U0001f5d3\ufe0f Date Explorer",
             
             fluidRow(
               column(3, dateInput("current_date", label = "Current / Primary Date",
                                   value = max(available_dates),
                                   min = min(available_dates), max = max(available_dates),
                                   datesdisabled = missing_dates, format = "MM d, yyyy")),
               column(3, dateInput("compare_date", label = "Comparison Date",
                                   value = available_dates[tail(which(
                                     available_dates <= max(available_dates) - 365), 1)],
                                   min = min(available_dates), max = max(available_dates),
                                   datesdisabled = missing_dates, format = "MM d, yyyy")),
               column(3, selectInput("season_jump", label = "Jump to Peak Season Date",
                                     choices = c("-- select --", setNames(
                                       paste0(2004:as.integer(format(max(available_dates), "%Y")), "-04-01"),
                                       paste0("April 1, ", 2004:as.integer(format(max(available_dates), "%Y")))
                                     )), selected = "-- select --")),
               column(3, selectInput("map_view", label = "Map View",
                                     choices = c("Current", "Comparison", "Difference"),
                                     selected = "Current"))
             ),
             
             fluidRow(
               column(12, tags$div(style = "margin-bottom: 10px;",
                                   actionButton("swap_dates", "\u21c4 Swap Dates", class = "btn btn-default btn-sm"),
                                   tags$span(style = "margin-left: 8px;"),
                                   actionButton("clear_popup", "\u2715 Clear Popup", class = "btn btn-default btn-sm")
               ))
             ),
             
             uiOutput("summary_bar"),
             div(class = "map-title", textOutput("map_title")),
             leafletOutput("main_map", height = "500px"),
             tags$div(style = "height: 20px;"),
             
             fluidRow(
               column(12,
                      tags$div(style = "font-size: 13px; font-weight: bold; color: #444; margin-bottom: 6px;",
                               textOutput("table_title")),
                      DTOutput("swe_table")
               )
             ),
             
             tags$div(style = "height: 20px;"),
             tags$p(style = "color: #999; font-size: 11px; text-align: center; padding-bottom: 10px;",
                    "Data: NOAA NSIDC SNODAS G02158 \u00b7 HUC8: USGS WBD \u00b7 ",
                    tags$a(href = "https://github.com/jeffmarti/snodas-huc8", target = "_blank", "GitHub"))
    ),  # end TAB 1
    
    # ==========================================================================
    # TAB 2: Climatology
    # ==========================================================================
    tabPanel("\U0001f4c8 Climatology",
             
             tags$div(style = "height: 15px;"),
             
             fluidRow(
               column(4, selectInput("clim_huc", label = "Select Watershed",
                                     choices = grouped_choices, selected = "17110005")),
               column(4, radioButtons("clim_units", label = "Units",
                                      choices = c("Acre-Feet" = "af", "KAF (thousands)" = "kaf"),
                                      selected = "af", inline = TRUE)),
               column(4, tags$div(style = "margin-top: 25px;", uiOutput("pct_normal_badge")))
             ),
             
             tags$div(
               style = "position: relative;",
               plotlyOutput("clim_plot", height = "520px"),
               uiOutput("clim_hover_box")
             ),
             
             tags$div(style = "height: 10px;"),
             downloadButton("clim_download_csv", "Download Climatology Data (CSV)"),
             tags$p(style = "color: #999; font-size: 11px; text-align: center; padding-bottom: 10px;",
                    "Climatology: SNODAS Oct 2003\u2013present \u00b7 ",
                    "Ribbons: min/max, 10th\u201390th, 25th\u201375th percentile by day of water year"),
             
             tags$hr(style = "border-top: 1px solid #d0dae4; margin: 10px 0 20px 0;"),
             
             plotlyOutput("monthly_profile_plot", height = "400px"),
             
             tags$p(style = "color: #999; font-size: 11px; text-align: center; padding-bottom: 10px;",
                    "Monthly SWE change: median gain/loss per month (Oct\u2192Nov through Aug\u2192Sep) \u00b7 ",
                    "Pareto line = cumulative % of total seasonal accumulation \u00b7 ",
                    "Dots = current WY \u00b7 Blue = stronger than median, Red = weaker than median")
    ),  # end TAB 2
    # ==========================================================================
    # TAB 3: Peak SWE & Melt-Out
    # ==========================================================================
    tabPanel("\u2603\ufe0f Peak & Melt-Out",
             
             tags$div(style = "height: 15px;"),
             
             fluidRow(
               column(4, selectInput("metrics_huc", label = "Select Watershed",
                                     choices = grouped_choices, selected = "17110005")),
               column(4, radioButtons("metrics_units", label = "Peak SWE Units",
                                      choices = c("Acre-Feet" = "af", "KAF (thousands)" = "kaf"),
                                      selected = "af", inline = TRUE)),
               column(4, tags$div(style = "margin-top: 25px;",
                                  downloadButton("metrics_download_csv",
                                                 "Download Annual Metrics (CSV)")))
             ),
             
             plotlyOutput("metrics_plot", height = "620px"),
             
             tags$p(style = "color: #999; font-size: 11px; text-align: center; padding-bottom: 10px;",
                    "Peak SWE: highest daily SWE volume recorded in each water year \u00b7 ",
                    "Melt-out: last date SWE > 0 in each water year \u00b7 ",
                    "Gaps indicate years with no recorded snow-free date (SWE > 0 all year, or current WY in progress)")
             
    )  # end TAB 3
    
  )  # end tabsetPanel
)  # end fluidPage

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  .popup_lock <- FALSE
  
  observe({ invalidateLater(30000); gc() })
  
  observeEvent(input$swap_dates, {
    cur <- input$current_date; cmp <- input$compare_date
    updateDateInput(session, "current_date", value = cmp)
    updateDateInput(session, "compare_date", value = cur)
  })
  
  observeEvent(input$clear_popup, {
    leafletProxy("main_map") %>% clearPopups()
  })
  
  observeEvent(input$season_jump, {
    req(input$season_jump != "-- select --")
    d <- as.Date(input$season_jump)
    nearest <- available_dates[which.min(abs(available_dates - d))]
    updateDateInput(session, "compare_date", value = nearest)
    updateSelectInput(session, "season_jump", selected = "-- select --")
  })
  
  valid_current <- reactive({
    d <- input$current_date
    if (is.null(d) || !d %in% available_dates) return(max(available_dates))
    d
  })
  
  valid_compare <- reactive({
    d <- input$compare_date
    if (is.null(d) || !d %in% available_dates)
      return(available_dates[which(available_dates <= max(available_dates) - 365)[1]])
    d
  })
  
  current_data <- reactive({ req(valid_current()); get_map_data(valid_current()) })
  compare_data <- reactive({ req(valid_compare()); get_map_data(valid_compare()) })
  
  diff_data <- reactive({
    cur <- current_data() %>% st_drop_geometry() %>%
      select(HUC8, swe_volume_af, swe_volume_kaf, swe_mean_in, swe_mean_mm)
    cmp <- compare_data() %>% st_drop_geometry() %>%
      select(HUC8, cmp_af = swe_volume_af, cmp_kaf = swe_volume_kaf,
             cmp_in = swe_mean_in, cmp_mm = swe_mean_mm)
    huc8_base %>%
      left_join(cur, by = "HUC8") %>%
      left_join(cmp, by = "HUC8") %>%
      mutate(diff_af  = swe_volume_af - cmp_af,
             diff_kaf = swe_volume_kaf - cmp_kaf,
             diff_in  = swe_mean_in - cmp_in,
             diff_mm  = swe_mean_mm - cmp_mm)
  })
  
  table_data <- reactive({ get_table_data(valid_current(), valid_compare()) })
  
  output$summary_bar <- renderUI({
    cur_total <- sum(current_data()$swe_volume_af, na.rm = TRUE)
    cmp_total <- sum(compare_data()$swe_volume_af, na.rm = TRUE)
    delta_af  <- cur_total - cmp_total
    delta_pct <- if (cmp_total > 0) (delta_af / cmp_total) * 100 else NA
    direction <- if (delta_af >= 0) "greater" else "less"
    color     <- if (delta_af >= 0) "#1a5276" else "#922b21"

    # Real-world context helper
    # Sources:
    #   Households: WA avg 111 gpd/person x 2.5 people = 101,288 gal/household/yr
    #               (NEEF/USGS Water Use in Washington, 2015)
    #   Irrigated acres: Columbia Basin crops ~3.25 AF/acre/season
    #               (WSU Extension, Washington Water Rights for Agricultural Producers)
    af_context <- function(af) {
      households   <- af * 3.22
      irr_acres    <- af / 3.25
      sprintf(
        "\u2248 %s WA households supplied for a year \u00b7 %s acres of Eastern WA cropland irrigated",
        formatC(round(households, -2), format = "f", digits = 0, big.mark = ","),
        formatC(round(irr_acres,    -1), format = "f", digits = 0, big.mark = ",")
      )
    }

    div(class = "summary-bar",

        # Line 1: directional comparison statement
        div(style = "font-size: 15px; color: #333;",
            tagList(
              tags$b(format(valid_current(), "%B %d, %Y")), " SWE was ",
              tags$b(style = sprintf("color:%s;", color), direction), " than ",
              tags$b(format(valid_compare(), "%B %d, %Y")),
              sprintf(" by %s AF (%s)",
                      formatC(abs(delta_af), format="f", digits=0, big.mark=","),
                      if (!is.na(delta_pct)) sprintf("%.1f%%", abs(delta_pct)) else "N/A")
            )
        ),

        # Line 2: Current total + context
        div(style = "font-size: 13px; color: #444; margin-top: 6px;",
            tags$b(sprintf("Current (%s):", format(valid_current(), "%b %d, %Y"))),
            sprintf(" %s AF  \u2014  ", formatC(cur_total, format="f", digits=0, big.mark=",")),
            tags$span(style = "color: #666; font-style: italic;", af_context(cur_total))
        ),

        # Line 3: Comparison total + context
        div(style = "font-size: 13px; color: #444; margin-top: 4px;",
            tags$b(sprintf("Comparison (%s):", format(valid_compare(), "%b %d, %Y"))),
            sprintf(" %s AF  \u2014  ", formatC(cmp_total, format="f", digits=0, big.mark=",")),
            tags$span(style = "color: #666; font-style: italic;", af_context(cmp_total))
        )
    )
  })
  output$map_title <- renderText({
    view <- input$map_view
    if (view == "Current")    return(paste("Current:",    format(valid_current(), "%B %d, %Y")))
    if (view == "Comparison") return(paste("Comparison:", format(valid_compare(), "%B %d, %Y")))
    paste("Difference: Current (", format(valid_current(), "%b %d, %Y"),
          ") minus Comparison (", format(valid_compare(), "%b %d, %Y"), ")")
  })
  
  output$table_title <- renderText({
    sprintf("HUC8 Summary: %s vs %s",
            format(valid_current(), "%B %d, %Y"), format(valid_compare(), "%B %d, %Y"))
  })
  
  output$main_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 7)) %>%
      addProviderTiles(providers$Esri.WorldShadedRelief) %>%
      setView(lng = INIT_LNG, lat = INIT_LAT, zoom = INIT_ZOOM) %>%
      setMaxBounds(lng1 = -125.5, lat1 = 45.5, lng2 = -116.5, lat2 = 49.5)
  })
  
  observe({
    view <- input$map_view
    if (view == "Current") {
      d   <- current_data()
      pal <- colorNumeric("YlGnBu", domain = d$swe_volume_af, na.color = "#cccccc")
      lbl <- sprintf("%s \u2014 %s AF", d$Name,
                     formatC(ifelse(is.na(d$swe_volume_af),0,d$swe_volume_af),
                             format="f", digits=0, big.mark=","))
      leafletProxy("main_map", data = d) %>% clearShapes() %>% clearControls() %>%
        addPolygons(fillColor=~pal(swe_volume_af), fillOpacity=0.75,
                    color="white", weight=1, opacity=1,
                    highlightOptions=highlightOptions(weight=2.5,color="#333",
                                                      fillOpacity=0.9,bringToFront=TRUE),
                    label=lbl, layerId=~HUC8) %>%
        addLegend(pal=pal, values=~swe_volume_af,
                  title=paste0("SWE Volume (AF)<br/><small>",
                               format(valid_current(),"%b %d, %Y"),"</small>"),
                  position="bottomright", opacity=0.85, na.label="No data") %>%
        addPolylines(data=wa_border, color="#333333", weight=1.5, opacity=0.8)
      
    } else if (view == "Comparison") {
      d   <- compare_data()
      pal <- colorNumeric("YlGnBu", domain = d$swe_volume_af, na.color = "#cccccc")
      lbl <- sprintf("%s \u2014 %s AF", d$Name,
                     formatC(ifelse(is.na(d$swe_volume_af),0,d$swe_volume_af),
                             format="f", digits=0, big.mark=","))
      leafletProxy("main_map", data = d) %>% clearShapes() %>% clearControls() %>%
        addPolygons(fillColor=~pal(swe_volume_af), fillOpacity=0.75,
                    color="white", weight=1, opacity=1,
                    highlightOptions=highlightOptions(weight=2.5,color="#333",
                                                      fillOpacity=0.9,bringToFront=TRUE),
                    label=lbl, layerId=~HUC8) %>%
        addLegend(pal=pal, values=~swe_volume_af,
                  title=paste0("SWE Volume (AF)<br/><small>",
                               format(valid_compare(),"%b %d, %Y"),"</small>"),
                  position="bottomright", opacity=0.85, na.label="No data") %>%
        addPolylines(data=wa_border, color="#333333", weight=1.5, opacity=0.8)
      
    } else {
      d       <- diff_data()
      max_abs <- max(abs(d$diff_af), na.rm = TRUE)
      if (is.infinite(max_abs) || max_abs == 0) max_abs <- 1
      pal <- colorNumeric(
        palette = c("#922b21","#e74c3c","#fadbd8","#ffffff","#d6eaf8","#2e86c1","#1a5276"),
        domain = c(-max_abs, max_abs), na.color = "#cccccc")
      lbl <- sprintf("%s \u2014 %s AF", d$Name,
                     formatC(ifelse(is.na(d$diff_af),0,d$diff_af),
                             format="f", digits=0, big.mark=",", flag="+"))
      leafletProxy("main_map", data = d) %>% clearShapes() %>% clearControls() %>%
        addPolygons(fillColor=~pal(diff_af), fillOpacity=0.75,
                    color="white", weight=1, opacity=1,
                    highlightOptions=highlightOptions(weight=2.5,color="#333",
                                                      fillOpacity=0.9,bringToFront=TRUE),
                    label=lbl, layerId=~HUC8) %>%
        addLegend(pal=pal, values=c(-max_abs,max_abs), title="Volume Change (AF)",
                  position="bottomright", opacity=0.85,
                  labFormat=labelFormat(transform=function(x) round(x,0)),
                  na.label="No data") %>%
        addPolylines(data=wa_border, color="#333333", weight=1.5, opacity=0.8)
    }
  })
  
  observeEvent(input$main_map_shape_click, ignoreInit = TRUE, {
    req(input$main_map_shape_click$id)
    huc  <- input$main_map_shape_click$id
    view <- input$map_view
    centroid <- huc8_centroids[huc8_centroids$HUC8 == huc, ]
    popup_html <- if (view == "Current") {
      swe_popup(current_data() %>% filter(HUC8==huc), format(valid_current(),"%B %d, %Y"))
    } else if (view == "Comparison") {
      swe_popup(compare_data() %>% filter(HUC8==huc), format(valid_compare(),"%B %d, %Y"))
    } else {
      diff_popup(diff_data() %>% filter(HUC8==huc),
                 format(valid_current(),"%b %d, %Y"), format(valid_compare(),"%b %d, %Y"))
    }
    leafletProxy("main_map") %>% clearPopups() %>%
      addPopups(lng=centroid$X, lat=centroid$Y, popup=popup_html)
  })
  
  output$swe_table <- renderDT({
    td <- table_data()
    datatable(
      td %>% transmute(
        `HUC8`            = HUC8, `Watershed` = Name,
        `Area (acres)`    = formatC(AreaAcres, format="f", digits=0, big.mark=","),
        `Current (AF)`    = formatC(cur_af,    format="f", digits=0, big.mark=","),
        `Comparison (AF)` = formatC(cmp_af,    format="f", digits=0, big.mark=","),
        `Difference (AF)` = formatC(diff_af,   format="f", digits=0, big.mark=",", flag="+"),
        `Diff %`          = ifelse(is.na(diff_pct),"N/A",sprintf("%+.1f%%",diff_pct)),
        .diff_af_num      = diff_af
      ),
      options = list(pageLength=15, scrollX=FALSE, dom="Bfrtip",
                     buttons=list(list(extend="csv", text="\u2193 Download CSV",
                                       filename="snodas_huc8_comparison",
                                       exportOptions=list(
                                         modifier=list(page="all"),
                                         columns=":visible"
                                       ))),
                     columnDefs=list(list(visible=FALSE, targets=7)),
                     order=list(list(3,"desc"))),
      extensions="Buttons", rownames=FALSE, class="stripe hover compact"
    ) %>%
      formatStyle("Difference (AF)", valueColumns=".diff_af_num",
                  color=styleInterval(0, c("#922b21","#1a5276")), fontWeight="bold")
  }, server = FALSE)
  
  # ============================================================================
  # CLIMATOLOGY TAB SERVER
  # ============================================================================
  
  clim_data <- reactive({ req(input$clim_huc); get_clim_data(input$clim_huc) })
  
  output$pct_normal_badge <- renderUI({
    cd <- clim_data()
    req(nrow(cd$current) > 0)
    today_doy <- cd$current$wy_doy[which.max(cd$current$wy_doy)]
    today_af  <- cd$current$swe_volume_af[which.max(cd$current$wy_doy)]
    med_row   <- cd$ribbons %>% filter(wy_doy == today_doy)
    if (nrow(med_row) == 0 || med_row$p50 == 0) return(NULL)
    pct   <- round((today_af / med_row$p50) * 100)
    color <- if (pct >= 90) "#1a5276" else if (pct >= 70) "#d4ac0d" else "#922b21"
    label <- if (pct >= 110) "Above Normal" else if (pct >= 90) "Near Normal" else
      if (pct >= 70) "Below Normal" else "Well Below Normal"
    tags$div(
      style = sprintf("background:%s; color:white; padding:8px 16px; border-radius:6px;
                       display:inline-block; font-size:15px; font-weight:bold;", color),
      sprintf("%d%% of Normal \u2014 %s", pct, label)
    )
  })
  
  # Hover box — driven by customdata (wy_doy) to avoid fragile date parsing
  output$clim_hover_box <- renderUI({
    hover <- plotly::event_data("plotly_hover", source = "clim_plot", session = session)
    if (is.null(hover) || is.null(hover$customdata)) return(NULL)
    
    cd         <- clim_data()
    units      <- input$clim_units
    unit_lbl   <- if (units == "kaf") "KAF" else "AF"
    fmt_digits <- if (units == "kaf") 1 else 0
    
    rb <- cd$ribbons
    cy <- cd$current
    
    if (units == "kaf") {
      rb <- rb %>% mutate(across(c(p00,p10,p25,p50,p75,p90,p100), ~ ./1000))
      cy <- cy %>% mutate(swe_volume_af = swe_volume_af / 1000)
    }
    
    doy <- as.integer(hover$customdata[1])
    
    match_row <- rb %>% filter(wy_doy == doy)
    if (nrow(match_row) == 0) return(NULL)
    
    cy_row <- cy %>% filter(wy_doy == doy)
    cy_val <- if (nrow(cy_row) > 0) {
      formatC(cy_row$swe_volume_af[1], format="f", digits=fmt_digits, big.mark=",")
    } else "\u2014"
    
    fmt <- function(x) formatC(x, format="f", digits=fmt_digits, big.mark=",")
    
    hov_date <- as.Date("1999-10-01") + doy - 1
    date_lbl <- format(hov_date, "%b %d")
    
    tags$div(
      class = "clim-hover-box",
      tags$div(style = "font-weight:bold; margin-bottom:4px;
                        color:#333; border-bottom:1px solid #ddd;
                        padding-bottom:3px;",
               date_lbl),
      tags$div(style = "color:#e74c3c; margin-bottom:4px;",
               paste0("WY ", cd$display_wy, ": ", cy_val, " ", unit_lbl)),
      tags$div(style = "border-top:1px solid #eee; margin-bottom:4px;"),
      tags$div(style = "color:#555;",
               tags$div(paste0("90th:   ", fmt(match_row$p90[1]), " ", unit_lbl)),
               tags$div(paste0("75th:   ", fmt(match_row$p75[1]), " ", unit_lbl)),
               tags$div(paste0("Median: ", fmt(match_row$p50[1]), " ", unit_lbl)),
               tags$div(paste0("25th:   ", fmt(match_row$p25[1]), " ", unit_lbl)),
               tags$div(paste0("10th:   ", fmt(match_row$p10[1]), " ", unit_lbl))
      )
    )
  })
  
  output$clim_plot <- renderPlotly({
    cd       <- clim_data()
    units    <- input$clim_units
    huc_name <- basin_lookup$Name[basin_lookup$HUC8 == input$clim_huc][1]
    disp_wy  <- cd$display_wy
    wy_label <- if (disp_wy == current_wy) "Current" else "Most Recent"
    
    rb <- cd$ribbons
    cy <- cd$current
    
    if (units == "kaf") {
      rb <- rb %>% mutate(across(c(p00,p10,p25,p50,p75,p90,p100), ~ ./1000))
      cy <- cy %>% mutate(swe_volume_af = swe_volume_af / 1000)
      y_label    <- "Snow Water Volume (KAF)"
      fmt_digits <- 1
    } else {
      y_label    <- "Snow Water Volume (Acre-Feet)"
      fmt_digits <- 0
    }
    
    wy_start     <- as.Date("1999-10-01")
    rb$x_date    <- wy_start + rb$wy_doy - 1
    cy$x_date    <- wy_start + cy$wy_doy - 1
    month_ticks  <- seq(as.Date("1999-10-01"), as.Date("2000-09-30"), by = "month")
    month_labels <- format(month_ticks, "%b")
    
    p <- plot_ly(source = "clim_plot") %>%
      
      add_ribbons(data=rb, x=~x_date, ymin=~p00, ymax=~p100,
                  customdata=~wy_doy,
                  fillcolor="rgba(180,180,180,0.25)", line=list(color="transparent"),
                  name="Min/Max Range", hoverinfo="none",
                  showlegend=TRUE) %>%
      
      add_ribbons(data=rb, x=~x_date, ymin=~p10, ymax=~p90,
                  customdata=~wy_doy,
                  fillcolor="rgba(130,130,130,0.30)", line=list(color="transparent"),
                  name="10th\u201390th Percentile", hoverinfo="none",
                  showlegend=TRUE) %>%
      
      add_ribbons(data=rb, x=~x_date, ymin=~p25, ymax=~p75,
                  customdata=~wy_doy,
                  fillcolor="rgba(80,80,80,0.35)", line=list(color="transparent"),
                  name="25th\u201375th Percentile", hoverinfo="none",
                  showlegend=TRUE) %>%
      
      add_lines(data=rb, x=~x_date, y=~p90,
                customdata=~wy_doy,
                line=list(color="transparent",width=0), name="90th Pct",
                showlegend=FALSE, hoverinfo="none") %>%
      
      add_lines(data=rb, x=~x_date, y=~p75,
                customdata=~wy_doy,
                line=list(color="transparent",width=0), name="75th Pct",
                showlegend=FALSE, hoverinfo="none") %>%
      
      add_lines(data=rb, x=~x_date, y=~p50,
                customdata=~wy_doy,
                line=list(color="rgba(60,60,60,0.85)",width=2,dash="dot"),
                name="Median", showlegend=TRUE, hoverinfo="none") %>%
      
      add_lines(data=rb, x=~x_date, y=~p25,
                customdata=~wy_doy,
                line=list(color="transparent",width=0), name="25th Pct",
                showlegend=FALSE, hoverinfo="none") %>%
      
      add_lines(data=rb, x=~x_date, y=~p10,
                customdata=~wy_doy,
                line=list(color="transparent",width=0), name="10th Pct",
                showlegend=FALSE, hoverinfo="none") %>%
      
      add_lines(data=cy, x=~x_date, y=~swe_volume_af,
                customdata=~wy_doy,
                line=list(color="#e74c3c", width=4),
                name=paste0("WY ", disp_wy, " (", wy_label, ")"),
                showlegend=TRUE, hoverinfo="none") %>%
      
      event_register("plotly_hover") %>%
      event_register("plotly_unhover") %>%
      
      layout(
        title = list(
          text = sprintf("<b>%s</b>  \u2014  SWE Climatology  |  WY %d highlighted",
                         huc_name, disp_wy),
          font = list(size = 15)
        ),
        xaxis = list(
          title       = "Month",
          tickvals    = as.numeric(month_ticks) * 86400 * 1000,
          ticktext    = month_labels,
          hoverformat = "%b %d",
          showgrid    = TRUE,
          gridcolor   = "rgba(200,200,200,0.5)"
        ),
        yaxis = list(
          title     = y_label,
          rangemode = "tozero",
          showgrid  = TRUE,
          gridcolor = "rgba(200,200,200,0.5)"
        ),
        legend        = list(orientation="h", x=0, y=-0.15),
        hovermode     = "x",
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        margin        = list(t=60, b=80)
      )
    
    p
  })
  
  output$clim_download_csv <- downloadHandler(
    filename = function() {
      huc_name <- basin_lookup$Name[basin_lookup$HUC8 == input$clim_huc][1]
      paste0("SNODAS_Climatology_", gsub(" ", "_", huc_name), ".csv")
    },
    content = function(file) {
      cd       <- clim_data()
      units    <- input$clim_units
      unit_lbl <- if (units == "kaf") "KAF" else "AF"
      
      rb <- cd$ribbons
      cy <- cd$current
      
      if (units == "kaf") {
        rb <- rb %>% mutate(across(c(p00,p10,p25,p50,p75,p90,p100), ~ ./1000))
        cy <- cy %>% mutate(swe_volume_af = swe_volume_af / 1000)
      }
      
      wy_start  <- as.Date("1999-10-01")
      rb$x_date <- wy_start + rb$wy_doy - 1
      
      cy_lookup <- cy %>%
        mutate(x_date = wy_start + wy_doy - 1) %>%
        select(x_date, current_wy_val = swe_volume_af)
      
      out <- rb %>%
        left_join(cy_lookup, by = "x_date") %>%
        mutate(date = format(x_date, "%b %d")) %>%
        select(date, current_wy_val, p10, p25, p50, p75, p90)
      
      names(out) <- c(
        "Date",
        paste0("WY_", cd$display_wy, "_", unit_lbl),
        paste0("10th_", unit_lbl),
        paste0("25th_", unit_lbl),
        paste0("Median_", unit_lbl),
        paste0("75th_", unit_lbl),
        paste0("90th_", unit_lbl)
      )
      
      write.csv(out, file, row.names = FALSE)
    }
  )
  output$monthly_profile_plot <- renderPlotly({
    
    prof     <- get_monthly_profile(input$clim_huc)
    units    <- input$clim_units
    huc_name <- basin_lookup$Name[basin_lookup$HUC8 == input$clim_huc][1]
    
    if (units == "kaf") {
      prof <- prof %>% mutate(median_delta  = median_delta  / 1000,
                              current_delta = current_delta / 1000)
      unit_lbl <- "KAF"
    } else {
      unit_lbl <- "AF"
    }
    
    # Pre-format hover labels in R for reliable comma display
    fmt_digits <- if (units == "kaf") 2 else 0
    prof <- prof %>%
      mutate(
        bar_hover_lbl = formatC(median_delta, format = "f",
                                digits = fmt_digits, big.mark = ",", flag = "+"),
        lol_hover_lbl = ifelse(!is.na(current_delta),
                               formatC(current_delta, format = "f",
                                       digits = fmt_digits, big.mark = ",", flag = "+"),
                               NA_character_)
      )
    
    month_order <- levels(prof$month_lbl)
    
    # Bar colors: gaining months steel blue, losing months muted red
    bar_colors <- ifelse(prof$median_delta >= 0,
                         "rgba(70,130,180,0.65)",
                         "rgba(192,57,43,0.55)")
    
    prof_above <- prof %>% filter(!is.na(current_delta),  above_median)
    prof_below <- prof %>% filter(!is.na(current_delta), !above_median)
    
    plot_ly() %>%
      
      # ---- Median delta bars ----
    add_bars(
      data          = prof,
      x             = ~month_lbl,
      y             = ~median_delta,
      customdata    = ~bar_hover_lbl,
      name          = "Historical Median \u0394SWE",
      marker        = list(color = bar_colors,
                           line  = list(color = "rgba(60,60,60,0.3)", width = 0.5)),
      hovertemplate = paste0("<b>%{x}</b><br>Median \u0394SWE: %{customdata} ",
                             unit_lbl, "<extra></extra>")
    ) %>%
      
      
      # ---- Zero reference line ----
    add_segments(
      x         = month_order[1],
      xend      = month_order[length(month_order)],
      y         = 0,
      yend      = 0,
      line      = list(color = "rgba(0,0,0,0.4)", width = 1, dash = "dot"),
      showlegend = FALSE,
      hoverinfo  = "none"
    ) %>%
      
      # ---- Pareto cumulative % line (right axis) ----
    add_trace(
      data          = prof,
      x             = ~month_lbl,
      y             = ~cum_pct,
      type          = "scatter",
      mode          = "lines+markers",
      name          = "Cumulative % of Accumulation",
      yaxis         = "y2",
      line          = list(color = "rgba(60,60,60,0.8)", width = 2, dash = "dot"),
      marker        = list(color = "rgba(60,60,60,0.8)", size = 5),
      hovertemplate = "<b>%{x}</b><br>Cumulative: %{y:.1f}%<extra></extra>"
    ) %>%
      
      # ---- Lollipop stems: above median (blue) ----
    add_segments(
      data       = prof_above,
      x          = ~month_lbl, xend = ~month_lbl,
      y          = ~median_delta, yend = ~current_delta,
      line       = list(color = "rgba(26,82,118,0.85)", width = 2.5),
      name       = paste0("WY ", current_wy, " \u2014 Stronger than Median"),
      showlegend = nrow(prof_above) > 0,
      hoverinfo  = "none"
    ) %>%
      add_markers(
        data          = prof_above,
        x             = ~month_lbl,
        y             = ~current_delta,
        customdata    = ~lol_hover_lbl,
        marker        = list(color = "rgba(26,82,118,0.95)", size = 9),
        showlegend    = FALSE,
        hovertemplate = paste0("<b>%{x}</b><br>WY ", current_wy,
                               " \u0394SWE: %{customdata} ", unit_lbl, "<extra></extra>")
      ) %>%
      
      # ---- Lollipop stems: below median (red) ----
    add_segments(
      data       = prof_below,
      x          = ~month_lbl, xend = ~month_lbl,
      y          = ~median_delta, yend = ~current_delta,
      line       = list(color = "rgba(192,57,43,0.85)", width = 2.5),
      name       = paste0("WY ", current_wy, " \u2014 Weaker than Median"),
      showlegend = nrow(prof_below) > 0,
      hoverinfo  = "none"
    ) %>%
      add_markers(
        data          = prof_below,
        x             = ~month_lbl,
        y             = ~current_delta,
        customdata    = ~lol_hover_lbl,
        marker        = list(color = "rgba(192,57,43,0.95)", size = 9),
        showlegend    = FALSE,
        hovertemplate = paste0("<b>%{x}</b><br>WY ", current_wy,
                               " \u0394SWE: %{customdata} ", unit_lbl, "<extra></extra>")
      ) %>%
      
      layout(
        title = list(
          text = sprintf(
            "<b>%s</b>  \u2014  Monthly SWE Change  |  Median vs WY %d",
            huc_name, current_wy),
          font = list(size = 14)
        ),
        xaxis = list(
          title         = "Month (Oct 1 \u2192 Sep 1 transitions)",
          categoryorder = "array",
          categoryarray = month_order,
          showgrid      = FALSE,
          tickfont      = list(size = 11)
        ),
        yaxis = list(
          title     = paste0("SWE Change (", unit_lbl, ")"),
          showgrid  = TRUE,
          gridcolor = "rgba(200,200,200,0.5)",
          tickfont  = list(size = 10),
          zeroline  = FALSE
        ),
        yaxis2 = list(
          title      = "Cumulative % of Total Accumulation",
          overlaying = "y",
          side       = "right",
          range      = c(0, 105),
          ticksuffix = "%",
          showgrid   = FALSE,
          tickfont   = list(size = 10)
        ),
        legend        = list(orientation = "h", x = 0, y = -0.22),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        margin        = list(t = 60, b = 70, r = 70)
      )
  })
  # ============================================================================
  # PEAK & MELT-OUT TAB SERVER
  # ============================================================================
  
  metrics_data <- reactive({
    req(input$metrics_huc)
    get_annual_metrics(input$metrics_huc)
  })
  
  output$metrics_plot <- renderPlotly({
    
    md       <- metrics_data()
    units    <- input$metrics_units
    huc_name <- basin_lookup$Name[basin_lookup$HUC8 == input$metrics_huc][1]
    
    # Unit conversion for peak SWE panel
    if (units == "kaf") {
      md       <- md %>% mutate(peak_af = peak_af / 1000)
      unit_lbl <- "KAF"
      peak_fmt <- "<b>WY %{x}</b><br>Peak: %{y:,.1f} KAF<br>Date: %{customdata}<extra></extra>"
    } else {
      unit_lbl <- "AF"
      peak_fmt <- "<b>WY %{x}</b><br>Peak: %{y:,.0f} AF<br>Date: %{customdata}<extra></extra>"
    }
    
    # Y-axis tick marks for melt-out panel (1st of each month in wy_doy)
    wy_start    <- as.Date("1999-10-01")
    tick_dates  <- seq(as.Date("1999-11-01"), as.Date("2000-09-01"), by = "month")
    tick_doys   <- as.integer(tick_dates - wy_start) + 1
    tick_labels <- format(tick_dates, "%b 1")
    
    # ---- Panel 1: Peak SWE Volume ----
    p1 <- plot_ly() %>%
      add_trace(
        data          = md,
        x             = ~wy,
        y             = ~peak_af,
        customdata    = ~format(peak_date, "%b %d"),
        type          = "scatter",
        mode          = "lines+markers",
        line          = list(color = "rgba(41,128,185,0.55)", width = 1.5),
        marker        = list(color = "rgba(41,128,185,0.9)",  size  = 6),
        hovertemplate = peak_fmt,
        showlegend    = FALSE
      ) %>%
      layout(
        xaxis = list(
          title     = "",
          showgrid  = TRUE,
          gridcolor = "rgba(200,200,200,0.5)",
          tickfont  = list(size = 10),
          dtick     = 1
        ),
        yaxis = list(
          title     = paste0("Peak SWE (", unit_lbl, ")"),
          rangemode = "tozero",
          showgrid  = TRUE,
          gridcolor = "rgba(200,200,200,0.5)",
          tickfont  = list(size = 10)
        ),
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
    
    # ---- Panel 2: Melt-Out Date ----
    p2 <- plot_ly() %>%
      add_trace(
        data          = md %>% filter(!is.na(meltout_wy_doy)),
        x             = ~wy,
        y             = ~meltout_wy_doy,
        customdata    = ~format(meltout_date, "%b %d, %Y"),
        type          = "scatter",
        mode          = "lines+markers",
        line          = list(color = "rgba(192,57,43,0.55)",  width = 1.5),
        marker        = list(color = "rgba(192,57,43,0.9)",   size  = 6),
        hovertemplate = "<b>WY %{x}</b><br>Melt-out: %{customdata}<extra></extra>",
        showlegend    = FALSE
      ) %>%
      layout(
        xaxis = list(
          title     = "Water Year",
          showgrid  = TRUE,
          gridcolor = "rgba(200,200,200,0.5)",
          tickfont  = list(size = 10),
          dtick     = 1
        ),
        yaxis = list(
          title     = "Melt-Out Date",
          tickvals  = tick_doys,
          ticktext  = tick_labels,
          showgrid  = TRUE,
          gridcolor = "rgba(200,200,200,0.5)",
          tickfont  = list(size = 10)
        ),
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
    
    # ---- Assemble 2-panel subplot ----
    subplot(p1, p2,
            nrows   = 2,
            shareX  = FALSE,
            shareY  = FALSE,
            margin  = 0.08) %>%
      layout(
        title = list(
          text = sprintf("<b>%s</b>  \u2014  Annual Peak SWE &amp; Melt-Out Date by Water Year",
                         huc_name),
          font = list(size = 15)
        ),
        annotations = list(
          list(text="<b>Peak SWE Volume</b>", x=0.5, y=1.0,
               xref="paper", yref="paper", xanchor="center", yanchor="bottom",
               showarrow=FALSE, font=list(size=12, color="#1a5276")),
          list(text="<b>Snow Melt-Out Date</b>", x=0.5, y=0.44,
               xref="paper", yref="paper", xanchor="center", yanchor="bottom",
               showarrow=FALSE, font=list(size=12, color="#1a5276"))
        ),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        margin        = list(t = 70, b = 50)
      )
  })
  
  output$metrics_download_csv <- downloadHandler(
    filename = function() {
      huc_name <- basin_lookup$Name[basin_lookup$HUC8 == input$metrics_huc][1]
      paste0("SNODAS_AnnualMetrics_", gsub(" ", "_", huc_name), ".csv")
    },
    content = function(file) {
      md <- metrics_data()
      out <- md %>%
        mutate(
          peak_date    = format(peak_date,    "%Y-%m-%d"),
          meltout_date = format(meltout_date, "%Y-%m-%d")
        ) %>%
        select(
          Water_Year     = wy,
          Peak_SWE_AF    = peak_af,
          Peak_Date      = peak_date,
          Peak_WY_DOY    = peak_wy_doy,
          Meltout_Date   = meltout_date,
          Meltout_WY_DOY = meltout_wy_doy
        )
      write.csv(out, file, row.names = FALSE)
    }
  )
  
}  # end server

# ------------------------------------------------------------------------------
shinyApp(ui, server)