# ==============================================================================
# app.R
# SNODAS HUC8 SWE Comparison Shiny App
#
# Tab 1: Date Explorer -- leaflet map with SWE choropleth
# Tab 2: Climatology  -- SWE percentile ribbon chart with hover box + CSV download
#
# Data source: https://github.com/jeffmarti/snodas-huc8
# Deploy to  : waterwater.shinyapps.io
#
# CHANGES:
#   - HUC8 boundaries and WA border now loaded from pre-processed .rds files
#     (huc8_base.rds, wa_border.rds) instead of raw .gpkg/.geojson files.
#     This skips st_read() + st_transform() at startup, reducing memory usage.
#     huc8_base.rds is simplified at dTolerance=1.0 (~9.5MB vs original 26MB).
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
           
           tags$h4("\U0001f4e7 Contact"),
           tags$p(
             # ---- Fill in your details below ----
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
                    "Ribbons: min/max, 10th\u201390th, 25th\u201375th percentile by day of water year")
             
    )   # end TAB 2
    
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
    div(class = "summary-bar",
        div(style = "font-size: 15px; color: #333;",
            tagList(tags$b(format(valid_current(), "%B %d, %Y")), " SWE was ",
                    tags$b(style = sprintf("color:%s;", color), direction), " than ",
                    tags$b(format(valid_compare(), "%B %d, %Y")),
                    sprintf(" by %s AF (%s)",
                            formatC(abs(delta_af), format="f", digits=0, big.mark=","),
                            if (!is.na(delta_pct)) sprintf("%.1f%%", abs(delta_pct)) else "N/A"))),
        div(style = "font-size: 13px; color: #666; margin-top: 4px;",
            sprintf("Current: %s AF  \u2022  Comparison: %s AF",
                    formatC(cur_total, format="f", digits=0, big.mark=","),
                    formatC(cmp_total, format="f", digits=0, big.mark=",")))
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
  }, server = FALSE)  # <-- add this here, outside the datatable() call
  
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
  
}  # end server

# ------------------------------------------------------------------------------
shinyApp(ui, server)
