# ==============================================================================
# app.R
# SNODAS HUC8 SWE Comparison Shiny App
#
# Displays daily SNODAS snow water equivalent by HUC8 watershed for
# Washington State (and cross-border HUC8s). Single map with view selector
# (Current / Comparison / Difference) and data table showing all three
# values per HUC8 for the selected date pair.
#
# Architecture:
#   - Single renderLeaflet() basemap, updated via leafletProxy on view change
#   - DT datatable below map shows Current, Comparison, Difference per HUC8
#   - clicked_source tracks map clicks for popup placement
#   - Plain R variables (.popup_lock) prevent circular triggers
#
# Memory optimizations:
#   - Single map eliminates two-thirds of polygon render memory vs three-map layout
#   - WA state boundary loaded from lightweight repo GeoJSON (no tigris)
#   - history CSV columns trimmed to only what the app needs at load time
#   - HUC8 centroids loaded from pre-computed static CSV (no geometry ops)
#   - rm() + gc() called after startup to free transient objects
#
# Data source: https://github.com/jeffmarti/snodas-huc8
# Deploy to  : waterwater.shinyapps.io
# ==============================================================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(DT)

# ------------------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------------------

HISTORY_URL   <- "https://raw.githubusercontent.com/jeffmarti/snodas-huc8/main/data/snodas_huc8_history.csv"
HUC8_URL      <- "https://github.com/jeffmarti/snodas-huc8/raw/main/cache/HUC8_WA_WGS84.gpkg"
WA_URL        <- "https://github.com/jeffmarti/snodas-huc8/raw/main/cache/WA_State_Boundary.geojson"
CENTROIDS_URL <- "https://raw.githubusercontent.com/jeffmarti/snodas-huc8/main/cache/huc8_centroids.csv"

INIT_LNG  <- -120.5
INIT_LAT  <-   47.5
INIT_ZOOM <-    6

# ------------------------------------------------------------------------------
# LOAD DATA (once at startup)
# ------------------------------------------------------------------------------

message("Loading SNODAS history CSV...")
history <- read.csv(HISTORY_URL, stringsAsFactors = FALSE) %>%
  select(HUC8, swe_date, swe_mean_in, swe_mean_mm, swe_volume_af, swe_volume_kaf)
history$HUC8     <- as.character(history$HUC8)
history$swe_date <- as.Date(history$swe_date)

message("Loading HUC8 boundaries...")
huc8_tmp <- tempfile(fileext = ".gpkg")
download.file(HUC8_URL, huc8_tmp, mode = "wb", quiet = TRUE)
huc8 <- sf::st_read(huc8_tmp, quiet = TRUE) %>%
  sf::st_transform(4326)
unlink(huc8_tmp)

message("Loading WA state boundary...")
wa_tmp <- tempfile(fileext = ".geojson")
download.file(WA_URL, wa_tmp, mode = "wb", quiet = TRUE)
wa_border <- sf::st_read(wa_tmp, quiet = TRUE) %>%
  sf::st_transform(4326)
unlink(wa_tmp)

# Available dates (ascending for calendar)
available_dates <- sort(unique(history$swe_date), decreasing = FALSE)

# Compute missing dates so the calendar can grey them out
full_range    <- seq(min(available_dates), max(available_dates), by = "day")
missing_dates <- as.character(full_range[!full_range %in% available_dates])

# Pre-join spatial + attribute data for fast filtering at runtime
huc8_base <- huc8 %>%
  select(HUC8, Name, AreaSqKm, AreaAcres) %>%
  mutate(HUC8 = as.character(HUC8))

# Load pre-computed centroids for popups (no geometry operation at runtime)
huc8_centroids <- read.csv(CENTROIDS_URL, stringsAsFactors = FALSE)

# Free the full huc8 object -- huc8_base is all we need from here on
rm(huc8)
gc()

message(sprintf("Ready: %d dates, %d HUC8s", length(available_dates), nrow(huc8_base)))

# ------------------------------------------------------------------------------
# HELPER: build map data for a single date
# ------------------------------------------------------------------------------

get_map_data <- function(date_val) {
  swe <- history %>%
    filter(swe_date == as.Date(date_val)) %>%
    select(HUC8, swe_mean_in, swe_mean_mm, swe_volume_af, swe_volume_kaf)
  
  huc8_base %>%
    left_join(swe, by = "HUC8")
}

# ------------------------------------------------------------------------------
# HELPER: build combined table data for both dates
# ------------------------------------------------------------------------------

get_table_data <- function(cur_date, cmp_date) {
  cur <- history %>%
    filter(swe_date == as.Date(cur_date)) %>%
    select(HUC8,
           cur_af  = swe_volume_af,
           cur_in  = swe_mean_in)
  
  cmp <- history %>%
    filter(swe_date == as.Date(cmp_date)) %>%
    select(HUC8,
           cmp_af  = swe_volume_af,
           cmp_in  = swe_mean_in)
  
  huc8_base %>%
    st_drop_geometry() %>%
    select(HUC8, Name, AreaAcres) %>%
    left_join(cur, by = "HUC8") %>%
    left_join(cmp, by = "HUC8") %>%
    mutate(
      cur_af  = ifelse(is.na(cur_af), 0, cur_af),
      cmp_af  = ifelse(is.na(cmp_af), 0, cmp_af),
      diff_af = cur_af - cmp_af,
      diff_pct = ifelse(cmp_af > 0, (diff_af / cmp_af) * 100, NA)
    ) %>%
    arrange(desc(cur_af))
}

# ------------------------------------------------------------------------------
# HELPER: popup HTML for current/comparison views
# ------------------------------------------------------------------------------

swe_popup <- function(d, date_label) {
  sprintf(
    paste0(
      "<b>%s</b><br/>",
      "<small>%s</small><br/>",
      "HUC8: %s<br/>",
      "<hr style='margin:4px 0'>",
      "SWE Mean: %.1f in | %.0f mm<br/>",
      "Volume: %s AF<br/>",
      "Area: %s acres"
    ),
    d$Name,
    date_label,
    d$HUC8,
    ifelse(is.na(d$swe_mean_in), 0, d$swe_mean_in),
    ifelse(is.na(d$swe_mean_mm), 0, d$swe_mean_mm),
    formatC(ifelse(is.na(d$swe_volume_af), 0, d$swe_volume_af),
            format = "f", digits = 0, big.mark = ","),
    formatC(d$AreaAcres, format = "f", digits = 0, big.mark = ",")
  )
}

# ------------------------------------------------------------------------------
# HELPER: popup HTML for difference view
# ------------------------------------------------------------------------------

diff_popup <- function(d, cur_label, cmp_label) {
  diff_af  <- ifelse(is.na(d$diff_af),  0, d$diff_af)
  diff_in  <- ifelse(is.na(d$diff_in),  0, d$diff_in)
  diff_mm  <- ifelse(is.na(d$diff_mm),  0, d$diff_mm)
  sprintf(
    paste0(
      "<b>%s</b><br/>",
      "<small>%s vs %s</small><br/>",
      "HUC8: %s<br/>",
      "<hr style='margin:4px 0'>",
      "Volume change: <b>%s AF</b><br/>",
      "SWE change: %+.1f in | %+.0f mm<br/>",
      "Area: %s acres"
    ),
    d$Name,
    cur_label,
    cmp_label,
    d$HUC8,
    formatC(diff_af, format = "f", digits = 0, big.mark = ",", flag = "+"),
    diff_in,
    diff_mm,
    formatC(d$AreaAcres, format = "f", digits = 0, big.mark = ",")
  )
}

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; }
      .title-bar {
        background: #1a5276;
        color: white;
        padding: 12px 20px;
        margin-bottom: 15px;
        border-radius: 4px;
      }
      .title-bar h3 { margin: 0; font-size: 20px; }
      .title-bar p  { margin: 4px 0 0 0; font-size: 13px; opacity: 0.85; }
      .summary-bar {
        background: #f0f4f8;
        border: 1px solid #d0dae4;
        border-radius: 4px;
        padding: 10px 20px;
        margin-bottom: 15px;
      }
      .map-title {
        font-size: 13px; font-weight: bold; color: #444;
        margin-bottom: 4px; padding-left: 4px;
      }
      .leaflet-container { border-radius: 4px; }
      .diff-pos { color: #1a5276; font-weight: bold; }
      .diff-neg { color: #922b21; font-weight: bold; }
    ")),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {

        var $dates = $('#current_date, #compare_date');

        $dates.prop('readonly', true)
              .css('caret-color', 'transparent');

        $(document).on('keydown keypress keyup',
                       '#current_date, #compare_date',
                       function(e) {
                         e.preventDefault();
                         e.stopPropagation();
                         return false;
                       });

        $(document).on('shiny:value', function() {
          $('#current_date, #compare_date').prop('readonly', true);
        });

      });
    "))
  ),
  
  # Title bar
  div(class = "title-bar",
      tags$h3("\U0001f3d4\ufe0f Washington State SNODAS SWE Explorer"),
      tags$p("Daily snow water equivalent by HUC8 watershed \u00b7 NOAA SNODAS \u00b7 Oct 2003\u2013present")
  ),
  
  # Controls row
  fluidRow(
    column(3,
           dateInput("current_date",
                     label         = "Current / Primary Date",
                     value         = max(available_dates),
                     min           = min(available_dates),
                     max           = max(available_dates),
                     datesdisabled = missing_dates,
                     format        = "MM d, yyyy")
    ),
    column(3,
           dateInput("compare_date",
                     label         = "Comparison Date",
                     value         = available_dates[tail(which(
                       available_dates <= max(available_dates) - 365
                     ), 1)],
                     min           = min(available_dates),
                     max           = max(available_dates),
                     datesdisabled = missing_dates,
                     format        = "MM d, yyyy")
    ),
    column(3,
           selectInput("season_jump",
                       label    = "Jump to Peak Season Date",
                       choices  = c("-- select --",
                                    setNames(
                                      paste0(2004:as.integer(format(max(available_dates), "%Y")), "-04-01"),
                                      paste0("April 1, ", 2004:as.integer(format(max(available_dates), "%Y")))
                                    )),
                       selected = "-- select --")
    ),
    column(3,
           selectInput("map_view",
                       label    = "Map View",
                       choices  = c("Current", "Comparison", "Difference"),
                       selected = "Current")
    )
  ),
  
  # Second controls row
  fluidRow(
    column(12,
           tags$div(style = "margin-bottom: 10px;",
                    actionButton("swap_dates", "\u21c4 Swap Dates",
                                 class = "btn btn-default btn-sm"),
                    tags$span(style = "margin-left: 8px;"),
                    actionButton("clear_popup", "\u2715 Clear Popup",
                                 class = "btn btn-default btn-sm")
           )
    )
  ),
  
  # Summary bar
  uiOutput("summary_bar"),
  
  # Single map -- full width, taller
  div(class = "map-title", textOutput("map_title")),
  leafletOutput("main_map", height = "500px"),
  
  tags$div(style = "height: 20px;"),
  
  # Data table
  fluidRow(
    column(12,
           tags$div(style = "font-size: 13px; font-weight: bold; color: #444;
                             margin-bottom: 6px;",
                    textOutput("table_title")),
           DTOutput("swe_table")
    )
  ),
  
  tags$div(style = "height: 20px;"),
  
  # Footer
  tags$p(
    style = "color: #999; font-size: 11px; text-align: center; padding-bottom: 10px;",
    "Data: NOAA National Snow and Ice Data Center (NSIDC) SNODAS G02158 \u00b7 ",
    "HUC8 boundaries: USGS Watershed Boundary Dataset \u00b7 ",
    tags$a(href = "https://github.com/jeffmarti/snodas-huc8",
           target = "_blank", "GitHub")
  )
)

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Plain R variable -- outside reactive graph to avoid circular triggers
  .popup_lock <- FALSE
  
  # Periodic garbage collection
  observe({
    invalidateLater(30000)
    gc()
  })
  
  # -- Swap dates button -------------------------------------------------------
  observeEvent(input$swap_dates, {
    cur <- input$current_date
    cmp <- input$compare_date
    updateDateInput(session, "current_date", value = cmp)
    updateDateInput(session, "compare_date", value = cur)
  })
  
  # -- Clear popup button ------------------------------------------------------
  observeEvent(input$clear_popup, {
    leafletProxy("main_map") %>% clearPopups()
  })
  
  # -- Season jump selector ----------------------------------------------------
  observeEvent(input$season_jump, {
    req(input$season_jump != "-- select --")
    d <- as.Date(input$season_jump)
    nearest <- available_dates[which.min(abs(available_dates - d))]
    updateDateInput(session, "compare_date", value = nearest)  # <- changed
    updateSelectInput(session, "season_jump", selected = "-- select --")
  })
  
  # -- Validate date selections ------------------------------------------------
  valid_current <- reactive({
    d <- input$current_date
    if (is.null(d) || !d %in% available_dates) return(max(available_dates))
    d
  })
  
  valid_compare <- reactive({
    d <- input$compare_date
    if (is.null(d) || !d %in% available_dates)
      return(available_dates[which(
        available_dates <= max(available_dates) - 365)[1]])
    d
  })
  
  # -- Reactive: map data -----------------------------------------------------
  current_data <- reactive({
    req(valid_current())
    get_map_data(valid_current())
  })
  
  compare_data <- reactive({
    req(valid_compare())
    get_map_data(valid_compare())
  })
  
  diff_data <- reactive({
    cur <- current_data()
    cmp <- compare_data()
    
    cur_vals <- cur %>%
      st_drop_geometry() %>%
      select(HUC8, swe_volume_af, swe_volume_kaf, swe_mean_in, swe_mean_mm)
    
    cmp_vals <- cmp %>%
      st_drop_geometry() %>%
      select(HUC8,
             cmp_af  = swe_volume_af,
             cmp_kaf = swe_volume_kaf,
             cmp_in  = swe_mean_in,
             cmp_mm  = swe_mean_mm)
    
    huc8_base %>%
      left_join(cur_vals, by = "HUC8") %>%
      left_join(cmp_vals, by = "HUC8") %>%
      mutate(
        diff_af  = swe_volume_af - cmp_af,
        diff_kaf = swe_volume_kaf - cmp_kaf,
        diff_in  = swe_mean_in  - cmp_in,
        diff_mm  = swe_mean_mm  - cmp_mm
      )
  })
  
  # -- Reactive: table data ---------------------------------------------------
  table_data <- reactive({
    get_table_data(valid_current(), valid_compare())
  })
  
  # -- Summary bar ------------------------------------------------------------
  output$summary_bar <- renderUI({
    cur_total <- sum(current_data()$swe_volume_af, na.rm = TRUE)
    cmp_total <- sum(compare_data()$swe_volume_af, na.rm = TRUE)
    delta_af  <- cur_total - cmp_total
    delta_pct <- if (cmp_total > 0) (delta_af / cmp_total) * 100 else NA
    
    cur_date_str  <- format(valid_current(), "%B %d, %Y")
    cmp_date_str  <- format(valid_compare(), "%B %d, %Y")
    direction     <- if (delta_af >= 0) "greater" else "less"
    color         <- if (delta_af >= 0) "#1a5276" else "#922b21"
    abs_delta_str <- formatC(abs(delta_af), format = "f", digits = 0, big.mark = ",")
    pct_str       <- if (!is.na(delta_pct)) sprintf("%.1f%%", abs(delta_pct)) else "N/A"
    
    sentence <- tagList(
      tags$b(cur_date_str),
      " SWE was ",
      tags$b(style = sprintf("color: %s;", color), direction),
      " than ",
      tags$b(cmp_date_str),
      sprintf(" by %s AF (%s)", abs_delta_str, pct_str)
    )
    
    div(class = "summary-bar",
        div(style = "font-size: 15px; color: #333;", sentence),
        div(style = "font-size: 13px; color: #666; margin-top: 4px;",
            sprintf("Current: %s AF  \u2022  Comparison: %s AF",
                    formatC(cur_total, format = "f", digits = 0, big.mark = ","),
                    formatC(cmp_total, format = "f", digits = 0, big.mark = ",")))
    )
  })
  
  # -- Map title --------------------------------------------------------------
  output$map_title <- renderText({
    view <- input$map_view
    if (view == "Current")    return(paste("Current:",    format(valid_current(), "%B %d, %Y")))
    if (view == "Comparison") return(paste("Comparison:", format(valid_compare(), "%B %d, %Y")))
    paste("Difference: Current (", format(valid_current(), "%b %d, %Y"),
          ") minus Comparison (", format(valid_compare(), "%b %d, %Y"), ")")
  })
  
  # -- Table title ------------------------------------------------------------
  output$table_title <- renderText({
    sprintf("HUC8 Summary: %s vs %s",
            format(valid_current(), "%B %d, %Y"),
            format(valid_compare(), "%B %d, %Y"))
  })
  
  # -- Base map: created ONCE -------------------------------------------------
  output$main_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 7)) %>%
      addProviderTiles(providers$Esri.WorldShadedRelief) %>%
      setView(lng = INIT_LNG, lat = INIT_LAT, zoom = INIT_ZOOM) %>%
      setMaxBounds(lng1 = -125.5, lat1 = 45.5, lng2 = -116.5, lat2 = 49.5)
  })
  
  # -- Update map polygons when data or view changes --------------------------
  observe({
    view <- input$map_view
    
    if (view == "Current") {
      d   <- current_data()
      pal <- colorNumeric("YlGnBu", domain = d$swe_volume_af, na.color = "#cccccc")
      labels <- sprintf("%s \u2014 %s AF", d$Name,
                        formatC(ifelse(is.na(d$swe_volume_af), 0, d$swe_volume_af),
                                format = "f", digits = 0, big.mark = ","))
      leafletProxy("main_map", data = d) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          fillColor        = ~pal(swe_volume_af),
          fillOpacity      = 0.75,
          color            = "white",
          weight           = 1,
          opacity          = 1,
          highlightOptions = highlightOptions(
            weight = 2.5, color = "#333", fillOpacity = 0.9, bringToFront = TRUE
          ),
          label   = labels,
          layerId = ~HUC8
        ) %>%
        addLegend(pal      = pal,
                  values   = ~swe_volume_af,
                  title    = paste0("SWE Volume (AF)<br/><small>",
                                    format(valid_current(), "%b %d, %Y"), "</small>"),
                  position = "bottomright",
                  opacity  = 0.85,
                  na.label = "No data") %>%
        addPolylines(data = wa_border, color = "#333333", weight = 1.5, opacity = 0.8)
      
    } else if (view == "Comparison") {
      d   <- compare_data()
      pal <- colorNumeric("YlGnBu", domain = d$swe_volume_af, na.color = "#cccccc")
      labels <- sprintf("%s \u2014 %s AF", d$Name,
                        formatC(ifelse(is.na(d$swe_volume_af), 0, d$swe_volume_af),
                                format = "f", digits = 0, big.mark = ","))
      leafletProxy("main_map", data = d) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          fillColor        = ~pal(swe_volume_af),
          fillOpacity      = 0.75,
          color            = "white",
          weight           = 1,
          opacity          = 1,
          highlightOptions = highlightOptions(
            weight = 2.5, color = "#333", fillOpacity = 0.9, bringToFront = TRUE
          ),
          label   = labels,
          layerId = ~HUC8
        ) %>%
        addLegend(pal      = pal,
                  values   = ~swe_volume_af,
                  title    = paste0("SWE Volume (AF)<br/><small>",
                                    format(valid_compare(), "%b %d, %Y"), "</small>"),
                  position = "bottomright",
                  opacity  = 0.85,
                  na.label = "No data") %>%
        addPolylines(data = wa_border, color = "#333333", weight = 1.5, opacity = 0.8)
      
    } else {
      # Difference view
      d       <- diff_data()
      max_abs <- max(abs(d$diff_af), na.rm = TRUE)
      if (is.infinite(max_abs) || max_abs == 0) max_abs <- 1
      pal <- colorNumeric(
        palette  = c("#922b21", "#e74c3c", "#fadbd8", "#ffffff",
                     "#d6eaf8", "#2e86c1", "#1a5276"),
        domain   = c(-max_abs, max_abs),
        na.color = "#cccccc"
      )
      labels <- sprintf("%s \u2014 %s AF", d$Name,
                        formatC(ifelse(is.na(d$diff_af), 0, d$diff_af),
                                format = "f", digits = 0, big.mark = ",", flag = "+"))
      leafletProxy("main_map", data = d) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          fillColor        = ~pal(diff_af),
          fillOpacity      = 0.75,
          color            = "white",
          weight           = 1,
          opacity          = 1,
          highlightOptions = highlightOptions(
            weight = 2.5, color = "#333", fillOpacity = 0.9, bringToFront = TRUE
          ),
          label   = labels,
          layerId = ~HUC8
        ) %>%
        addLegend(pal       = pal,
                  values    = c(-max_abs, max_abs),
                  title     = "Volume Change (AF)",
                  position  = "bottomright",
                  opacity   = 0.85,
                  labFormat = labelFormat(transform = function(x) round(x, 0))) %>%
        addPolylines(data = wa_border, color = "#333333", weight = 1.5, opacity = 0.8)
    }
  })
  
  # -- Map click popup --------------------------------------------------------
  observeEvent(input$main_map_shape_click, ignoreInit = TRUE, {
    req(input$main_map_shape_click$id)
    huc  <- input$main_map_shape_click$id
    view <- input$map_view
    
    centroid <- huc8_centroids[huc8_centroids$HUC8 == huc, ]
    lng <- centroid$X
    lat <- centroid$Y
    
    popup_html <- if (view == "Current") {
      row <- current_data() %>% filter(HUC8 == huc)
      swe_popup(row, format(valid_current(), "%B %d, %Y"))
    } else if (view == "Comparison") {
      row <- compare_data() %>% filter(HUC8 == huc)
      swe_popup(row, format(valid_compare(), "%B %d, %Y"))
    } else {
      row <- diff_data() %>% filter(HUC8 == huc)
      diff_popup(row,
                 format(valid_current(), "%b %d, %Y"),
                 format(valid_compare(), "%b %d, %Y"))
    }
    
    leafletProxy("main_map") %>%
      clearPopups() %>%
      addPopups(lng = lng, lat = lat, popup = popup_html)
  })
  
  # -- Data table -------------------------------------------------------------
  output$swe_table <- renderDT({
    td <- table_data()
    
    datatable(
      td %>%
        transmute(
          `HUC8`           = HUC8,
          `Watershed`      = Name,
          `Area (acres)`   = formatC(AreaAcres, format = "f", digits = 0, big.mark = ","),
          `Current (AF)`   = formatC(cur_af,    format = "f", digits = 0, big.mark = ","),
          `Comparison (AF)`= formatC(cmp_af,    format = "f", digits = 0, big.mark = ","),
          `Difference (AF)`= formatC(diff_af,   format = "f", digits = 0, big.mark = ",",
                                     flag = "+"),
          `Diff %`         = ifelse(is.na(diff_pct), "N/A",
                                    sprintf("%+.1f%%", diff_pct)),
          # Hidden numeric columns for color coding
          .diff_af_num     = diff_af
        ),
      options = list(
        pageLength  = 15,
        scrollX     = FALSE,
        dom         = "Bfrtip",
        buttons     = list(list(extend = "csv", text = "\u2193 Download CSV",
                                filename = "snodas_huc8_comparison")),
        columnDefs  = list(
          list(visible = FALSE, targets = 7)  # hide .diff_af_num
        ),
        order       = list(list(3, "desc"))   # sort by Current AF descending
      ),
      extensions = "Buttons",
      rownames   = FALSE,
      class      = "stripe hover compact"
    ) %>%
      formatStyle(
        "Difference (AF)",
        valueColumns = ".diff_af_num",
        color = styleInterval(0, c("#922b21", "#1a5276")),
        fontWeight = "bold"
      )
  })
  
}

# ------------------------------------------------------------------------------
shinyApp(ui, server)
