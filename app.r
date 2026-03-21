# ==============================================================================
# app.R
# SNODAS HUC8 SWE Comparison Shiny App
#
# Displays daily SNODAS snow water equivalent by HUC8 watershed for
# Washington State (and cross-border HUC8s). Allows comparison of any
# two dates in the historical record with a difference map.
#
# Architecture:
#   - renderLeaflet() creates basemap ONCE (tiles + initial view only)
#   - observe() blocks update polygons/legend via leafletProxy when data changes
#   - State border added LAST in each proxy block so it renders on top
#   - clicked_source tracks which map was clicked to avoid duplicate popups
#   - Plain R variables (.syncing, .popup_lock) prevent circular triggers
#
# Memory optimizations:
#   - WA state boundary loaded from lightweight repo GeoJSON (no tigris)
#   - history CSV columns trimmed to only what the app needs at load time
#   - HUC8 centroids pre-computed once at startup (plain data frame lookup)
#   - rm() + gc() called after startup to free transient objects
#
# Data source: https://github.com/jeffmarti/snodas-huc8
# Deploy to  : waterwater.shinyapps.io
# ==============================================================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# ------------------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------------------

HISTORY_URL <- "https://raw.githubusercontent.com/jeffmarti/snodas-huc8/main/data/snodas_huc8_history.csv"
HUC8_URL    <- "https://github.com/jeffmarti/snodas-huc8/raw/main/cache/HUC8_WA_WGS84.gpkg"
WA_URL      <- "https://github.com/jeffmarti/snodas-huc8/raw/main/cache/WA_State_Boundary.geojson"

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

# Pre-compute centroids once at startup -- plain data frame for fast lookup
# Suppressing the benign "attributes constant over geometries" warning
huc8_centroids <- suppressWarnings(
  huc8_base %>%
    sf::st_centroid() %>%
    sf::st_coordinates() %>%
    as.data.frame() %>%
    mutate(HUC8 = huc8_base$HUC8)
)

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
# HELPER: popup HTML for current/comparison maps
# ------------------------------------------------------------------------------

swe_popup <- function(d) {
  sprintf(
    paste0(
      "<b>%s</b><br/>",
      "HUC8: %s<br/>",
      "<hr style='margin:4px 0'>",
      "SWE Mean: %.1f in | %.0f mm<br/>",
      "Volume: %s AF | %s ML<br/>",
      "Area: %s acres | %s km\u00b2"
    ),
    d$Name,
    d$HUC8,
    ifelse(is.na(d$swe_mean_in),  0, d$swe_mean_in),
    ifelse(is.na(d$swe_mean_mm),  0, d$swe_mean_mm),
    formatC(ifelse(is.na(d$swe_volume_af), 0, d$swe_volume_af),
            format = "f", digits = 0, big.mark = ","),
    formatC(ifelse(is.na(d$swe_volume_af), 0, d$swe_volume_af) * 1.23348,
            format = "f", digits = 0, big.mark = ","),
    formatC(d$AreaAcres, format = "f", digits = 0, big.mark = ","),
    formatC(d$AreaSqKm,  format = "f", digits = 0, big.mark = ",")
  )
}

# ------------------------------------------------------------------------------
# HELPER: popup HTML for difference map
# ------------------------------------------------------------------------------

diff_popup <- function(d) {
  sprintf(
    paste0(
      "<b>%s</b><br/>",
      "HUC8: %s<br/>",
      "<hr style='margin:4px 0'>",
      "Volume change: <b>%s AF</b><br/>",
      "SWE change: %+.1f in | %+.0f mm<br/>",
      "Area: %s acres | %s km\u00b2"
    ),
    d$Name,
    d$HUC8,
    formatC(ifelse(is.na(d$diff_af), 0, d$diff_af),
            format = "f", digits = 0, big.mark = ",", flag = "+"),
    ifelse(is.na(d$diff_in), 0, d$diff_in),
    ifelse(is.na(d$diff_mm), 0, d$diff_mm),
    formatC(d$AreaAcres, format = "f", digits = 0, big.mark = ","),
    formatC(d$AreaSqKm,  format = "f", digits = 0, big.mark = ",")
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

  # Date controls row
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
           tags$div(style = "margin-top: 25px;",
                    actionButton("swap_dates", "\u21c4 Swap Dates",
                                 class = "btn btn-default btn-sm"),
                    tags$span(style = "margin-left: 8px;"),
                    actionButton("clear_popups", "\u2715 Clear Popups",
                                 class = "btn btn-default btn-sm")
           )
    )
  ),

  # Summary bar
  uiOutput("summary_bar"),

  # Row 1: Current date map (full width)
  div(class = "map-title", textOutput("current_map_title")),
  leafletOutput("current_map", height = "400px"),

  tags$div(style = "height: 15px;"),

  # Row 2: Comparison map + Difference map
  fluidRow(
    column(6,
           div(class = "map-title", textOutput("compare_map_title")),
           leafletOutput("compare_map", height = "380px")
    ),
    column(6,
           div(class = "map-title", textOutput("diff_map_title")),
           leafletOutput("diff_map", height = "380px")
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

  # Plain R variables -- outside reactive graph to avoid circular triggers
  .syncing    <- FALSE
  .popup_lock <- FALSE

  # -- Reactive values --------------------------------------------------------
  clicked_huc    <- reactiveVal(NULL)
  clicked_source <- reactiveVal(NULL)

  # -- Swap dates button -------------------------------------------------------
  observeEvent(input$swap_dates, {
    cur <- input$current_date
    cmp <- input$compare_date
    updateDateInput(session, "current_date", value = cmp)
    updateDateInput(session, "compare_date", value = cur)
  })

  # -- Clear popups button -----------------------------------------------------
  observeEvent(input$clear_popups, {
    leafletProxy("current_map") %>% clearPopups()
    leafletProxy("compare_map") %>% clearPopups()
    leafletProxy("diff_map")    %>% clearPopups()
    clicked_huc(NULL)
    clicked_source(NULL)
  })

  # -- Season jump selector ----------------------------------------------------
  observeEvent(input$season_jump, {
    req(input$season_jump != "-- select --")
    d <- as.Date(input$season_jump)
    nearest <- available_dates[which.min(abs(available_dates - d))]
    updateDateInput(session, "current_date", value = nearest)
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
        diff_af  = cmp_af  - swe_volume_af,
        diff_kaf = cmp_kaf - swe_volume_kaf,
        diff_in  = cmp_in  - swe_mean_in,
        diff_mm  = cmp_mm  - swe_mean_mm
      )
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

  # -- Map titles -------------------------------------------------------------
  output$current_map_title <- renderText({
    paste("Current:", format(valid_current(), "%B %d, %Y"))
  })
  output$compare_map_title <- renderText({
    paste("Comparison:", format(valid_compare(), "%B %d, %Y"))
  })
  output$diff_map_title <- renderText({
    "Difference (Comparison minus Current)"
  })

  # -- Base maps: created ONCE, tiles + initial view only ---------------------

  output$current_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 7)) %>%
      addProviderTiles(providers$Esri.WorldShadedRelief) %>%
      setView(lng = INIT_LNG, lat = INIT_LAT, zoom = INIT_ZOOM) %>%
      setMaxBounds(lng1 = -125.5, lat1 = 45.5, lng2 = -116.5, lat2 = 49.5)
  })

  output$compare_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 6)) %>%
      addProviderTiles(providers$Esri.WorldShadedRelief) %>%
      setView(lng = INIT_LNG, lat = INIT_LAT, zoom = INIT_ZOOM) %>%
      setMaxBounds(lng1 = -125.5, lat1 = 45.5, lng2 = -116.5, lat2 = 49.5)
  })

  output$diff_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 6)) %>%
      addProviderTiles(providers$Esri.WorldShadedRelief) %>%
      setView(lng = INIT_LNG, lat = INIT_LAT, zoom = INIT_ZOOM) %>%
      setMaxBounds(lng1 = -125.5, lat1 = 45.5, lng2 = -116.5, lat2 = 49.5)
  })

  # -- Update current map polygons when data changes --------------------------
  observe({
    d   <- current_data()
    pal <- colorNumeric("YlGnBu", domain = d$swe_volume_af, na.color = "#cccccc")
    labels <- sprintf("%s \u2014 %s AF", d$Name,
                      formatC(ifelse(is.na(d$swe_volume_af), 0, d$swe_volume_af),
                              format = "f", digits = 0, big.mark = ","))

    leafletProxy("current_map", data = d) %>%
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
                title    = "SWE Volume (AF)",
                position = "bottomright",
                opacity  = 0.85,
                na.label = "No data") %>%
      addPolylines(data    = wa_border,
                   color   = "#333333",
                   weight  = 1.5,
                   opacity = 0.8)
  })

  # -- Update comparison map polygons when data changes -----------------------
  observe({
    d   <- compare_data()
    pal <- colorNumeric("YlGnBu", domain = d$swe_volume_af, na.color = "#cccccc")
    labels <- sprintf("%s \u2014 %s AF", d$Name,
                      formatC(ifelse(is.na(d$swe_volume_af), 0, d$swe_volume_af),
                              format = "f", digits = 0, big.mark = ","))

    leafletProxy("compare_map", data = d) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor        = ~pal(swe_volume_af),
        fillOpacity      = 0.75,
        color            = "white",
        weight           = 1,
        highlightOptions = highlightOptions(
          weight = 2.5, color = "#333", fillOpacity = 0.9, bringToFront = TRUE
        ),
        label   = labels,
        layerId = ~HUC8
      ) %>%
      addLegend(pal      = pal,
                values   = ~swe_volume_af,
                title    = "SWE Volume (AF)",
                position = "bottomright",
                opacity  = 0.85,
                na.label = "No data") %>%
      addPolylines(data    = wa_border,
                   color   = "#333333",
                   weight  = 1.5,
                   opacity = 0.8)
  })

  # -- Update difference map polygons when data changes -----------------------
  observe({
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
                              format = "f", digits = 0, big.mark = ",",
                              flag = "+"))

    leafletProxy("diff_map", data = d) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor        = ~pal(diff_af),
        fillOpacity      = 0.75,
        color            = "white",
        weight           = 1,
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
      addPolylines(data    = wa_border,
                   color   = "#333333",
                   weight  = 1.5,
                   opacity = 0.8)
  })

  # -- Sync clicks across all three maps --------------------------------------
  observeEvent(input$current_map_shape_click, ignoreInit = TRUE, {
    if (.popup_lock) return()
    req(input$current_map_shape_click$id)
    clicked_source("current_map")
    clicked_huc(input$current_map_shape_click$id)
  })

  observeEvent(input$compare_map_shape_click, ignoreInit = TRUE, {
    if (.popup_lock) return()
    req(input$compare_map_shape_click$id)
    clicked_source("compare_map")
    clicked_huc(input$compare_map_shape_click$id)
  })

  observeEvent(input$diff_map_shape_click, ignoreInit = TRUE, {
    if (.popup_lock) return()
    req(input$diff_map_shape_click$id)
    clicked_source("diff_map")
    clicked_huc(input$diff_map_shape_click$id)
  })

  observeEvent(clicked_huc(), ignoreNULL = TRUE, {
    huc         <- clicked_huc()
    source_map  <- clicked_source()
    .popup_lock <<- TRUE

    # Look up pre-computed centroid -- no geometry operation at runtime
    centroid <- huc8_centroids[huc8_centroids$HUC8 == huc, ]
    lng <- centroid$X
    lat <- centroid$Y

    cur_row <- current_data() %>% filter(HUC8 == huc)
    cmp_row <- compare_data() %>% filter(HUC8 == huc)
    dif_row <- diff_data()    %>% filter(HUC8 == huc)

    leafletProxy("current_map") %>%
      clearPopups() %>%
      addPopups(lng = lng, lat = lat, popup = swe_popup(cur_row))

    leafletProxy("compare_map") %>%
      clearPopups() %>%
      addPopups(lng = lng, lat = lat, popup = swe_popup(cmp_row))

    leafletProxy("diff_map") %>%
      clearPopups() %>%
      addPopups(lng = lng, lat = lat, popup = diff_popup(dif_row))

    clicked_huc(NULL)
    .popup_lock <<- FALSE
  })

  # -- Sync zoom and pan across all three maps --------------------------------
  observe({
    if (.syncing) return()
    z <- input$current_map_zoom
    c <- input$current_map_center
    req(z, c)
    .syncing <<- TRUE
    leafletProxy("compare_map") %>% setView(c$lng, c$lat, z)
    leafletProxy("diff_map")    %>% setView(c$lng, c$lat, z)
    .syncing <<- FALSE
  })

  observe({
    if (.syncing) return()
    z <- input$compare_map_zoom
    c <- input$compare_map_center
    req(z, c)
    .syncing <<- TRUE
    leafletProxy("current_map") %>% setView(c$lng, c$lat, z)
    leafletProxy("diff_map")    %>% setView(c$lng, c$lat, z)
    .syncing <<- FALSE
  })

  observe({
    if (.syncing) return()
    z <- input$diff_map_zoom
    c <- input$diff_map_center
    req(z, c)
    .syncing <<- TRUE
    leafletProxy("current_map") %>% setView(c$lng, c$lat, z)
    leafletProxy("compare_map") %>% setView(c$lng, c$lat, z)
    .syncing <<- FALSE
  })

}

# ------------------------------------------------------------------------------
shinyApp(ui, server)
