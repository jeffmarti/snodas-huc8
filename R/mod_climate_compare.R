# ==============================================================================
# R/mod_climate_compare.R
#
# Shiny module: SWE & Climate Comparison
# Three stacked panels on a shared time x-axis with rangeslider:
#   1. Monthly Statewide SWE Change    (bars, blue/red gain/loss)
#   2. Monthly Temperature Anomaly     (bars, red/blue warm/cool)
#   3. Monthly Precipitation Anomaly   (bars, green/tan wet/dry)
#
# Global objects consumed (computed in app.r at startup):
#   swe_monthly_statewide  — data frame: bar_date, wy, month_num, delta_af
#   ncei_temp              — data frame: bar_date, year, month, value, anomaly
#   ncei_precip            — data frame: bar_date, year, month, value, anomaly
#
# The module gracefully handles a missing / empty ncei file by showing
# a placeholder message in the climate panels.
# ==============================================================================

# -- Color constants (self-contained so the module is portable) ----------------

.cc_swe_gain  <- "#1a5276"   # blue  — snowpack gaining
.cc_swe_loss  <- "#922b21"   # red   — snowpack losing
.cc_warm      <- "#e74c3c"   # red   — above-normal temperature
.cc_cool      <- "#2e86c1"   # blue  — below-normal temperature
.cc_wet       <- "#27ae60"   # green — above-normal precip
.cc_dry       <- "#d4ac0d"   # tan   — below-normal precip
.cc_grid      <- "#eeeeee"

# -- UI ------------------------------------------------------------------------

mod_climate_compare_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(style = "height: 15px;"),
    fluidRow(
      column(12,
        plotlyOutput(ns("climate_plot"), height = "700px", width = "100%")
      )
    ),
    tags$div(
      style = "margin-top: 8px; display: flex; justify-content: space-between;
               align-items: center; padding: 0 4px;",
      tags$div(
        style = "font-size: 10px; color: #888;",
        tags$span(
          "SWE: NOAA NSIDC SNODAS (71 WA HUC8 watersheds summed)  |  ",
          "Climate: NOAA NCEI, Washington State (1991\u20132020 normals)"
        )
      ),
      tags$div(
        downloadButton(ns("download_climate"), "Download Data (CSV)",
                       style = "font-size: 11px; padding: 4px 10px;")
      )
    )
  )
}

# -- Server --------------------------------------------------------------------

mod_climate_compare_server <- function(id, swe_data, temp_data, precip_data) {
  moduleServer(id, function(input, output, session) {

    output$climate_plot <- renderPlotly({
      withProgress(message = "Building comparison chart...", {
        build_climate_compare_plot(swe_data, temp_data, precip_data)
      })
    })

    output$download_climate <- downloadHandler(
      filename = function() {
        paste0("snodas_climate_compare_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(build_climate_compare_download(swe_data, temp_data, precip_data),
                  file, row.names = FALSE, na = "")
      }
    )

  })
}

# -- Plot builder --------------------------------------------------------------

build_climate_compare_plot <- function(df_swe, df_temp, df_precip) {

  fmt_af  <- function(x) formatC(round(x), format = "d", big.mark = ",")
  fmt_kaf <- function(x) formatC(round(x / 1000, 1), format = "f",
                                  digits = 1, big.mark = ",")

  # -- Panel layout: 3 equal panels, shared x ---------------------------------
  gap <- 0.03
  ph  <- (1 - 2 * gap) / 3

  yd <- list(
    p1 = c(2 * ph + 2 * gap,  1),
    p2 = c(ph + gap,          2 * ph + gap),
    p3 = c(0,                 ph)
  )

  panel_titles <- c(
    "Monthly Statewide SWE Change (acre-feet, sum of 71 WA HUC8 watersheds)",
    "Monthly Temperature Anomaly \u00b0F vs 1991\u20132020 normal (WA Statewide, NCEI)",
    "Monthly Precipitation Anomaly (in vs 1991\u20132020 normal, WA Statewide, NCEI)"
  )

  bar_width_ms <- 30.44 * 24 * 3600 * 1000 * 0.75

  fig  <- plot_ly()
  axes <- list()

  # Shared x-axis base (no tick labels except bottom panel)
  x_base <- list(
    type           = "date",
    showticklabels = FALSE,
    showgrid       = TRUE,
    gridcolor      = .cc_grid,
    tickfont       = list(size = 9),
    title          = ""
  )

  # -- Panel 1: Statewide SWE monthly change -----------------------------------

  swe_colors <- ifelse(df_swe$delta_af >= 0, .cc_swe_gain, .cc_swe_loss)

  fig <- fig %>%
    add_bars(
      data          = df_swe,
      x             = ~bar_date,
      y             = ~delta_af,
      xaxis         = "x",
      yaxis         = "y",
      name          = "SWE Change",
      showlegend    = FALSE,
      width         = bar_width_ms,
      marker        = list(color = swe_colors, line = list(width = 0)),
      customdata    = ~bar_date,
      text          = ~paste0(
        format(bar_date, "%b %Y"), " (WY", wy, ")<br>",
        "SWE change: ",
        ifelse(delta_af >= 0, "+", ""),
        fmt_af(delta_af), " AF<br>",
        "(",
        fmt_kaf(delta_af), " KAF)<br>",
        ifelse(delta_af >= 0,
               "\u25b2 Snowpack gaining",
               "\u25bc Snowpack losing")
      ),
      hovertemplate = "%{text}<extra></extra>",
      textposition  = "none"
    )

  axes[["xaxis"]] <- modifyList(x_base, list(anchor = "y"))
  axes[["yaxis"]] <- list(
    domain      = yd$p1,
    anchor      = "x",
    title       = "",
    zeroline    = TRUE,
    zerolinecolor = "#999999",
    zerolinewidth = 1.2,
    showgrid    = TRUE,
    gridcolor   = .cc_grid,
    tickformat  = ",.0f",
    tickfont    = list(size = 8)
  )

  # -- Panel 2: Temperature anomaly --------------------------------------------

  if (nrow(df_temp) > 0) {
    temp_colors <- ifelse(df_temp$anomaly >= 0, .cc_warm, .cc_cool)

    fig <- fig %>%
      add_bars(
        data          = df_temp,
        x             = ~bar_date,
        y             = ~anomaly,
        xaxis         = "x2",
        yaxis         = "y2",
        name          = "Temp Anomaly",
        showlegend    = FALSE,
        width         = bar_width_ms,
        marker        = list(color = temp_colors, line = list(width = 0)),
        text          = ~paste0(
          format(bar_date, "%b %Y"), "<br>",
          "Temp anomaly: ", sprintf("%+.1f", anomaly), "\u00b0F<br>",
          "Avg temp: ", sprintf("%.1f", value), "\u00b0F"
        ),
        hovertemplate = "%{text}<extra></extra>",
        textposition  = "none"
      )
  } else {
    # Placeholder if NCEI data is not yet available
    fig <- fig %>%
      add_annotations(
        text      = "Temperature data not yet available \u2014 run update pipeline",
        x = 0.5, xref = "x2 domain",
        y = 0.5, yref = "y2 domain",
        showarrow = FALSE,
        font      = list(size = 12, color = "#aaa")
      )
  }

  axes[["xaxis2"]] <- modifyList(x_base, list(anchor = "y2", matches = "x"))
  axes[["yaxis2"]] <- list(
    domain      = yd$p2,
    anchor      = "x2",
    title       = "",
    zeroline    = TRUE,
    zerolinecolor = "#999999",
    zerolinewidth = 1.2,
    showgrid    = TRUE,
    gridcolor   = .cc_grid,
    ticksuffix  = "\u00b0F",
    tickfont    = list(size = 8)
  )

  # -- Panel 3: Precipitation anomaly (with rangeslider) ----------------------

  if (nrow(df_precip) > 0) {
    precip_colors <- ifelse(df_precip$anomaly >= 0, .cc_wet, .cc_dry)

    fig <- fig %>%
      add_bars(
        data          = df_precip,
        x             = ~bar_date,
        y             = ~anomaly,
        xaxis         = "x3",
        yaxis         = "y3",
        name          = "Precip Anomaly",
        showlegend    = FALSE,
        width         = bar_width_ms,
        marker        = list(color = precip_colors, line = list(width = 0)),
        text          = ~paste0(
          format(bar_date, "%b %Y"), "<br>",
          "Precip anomaly: ", sprintf("%+.2f", anomaly), " in<br>",
          "Monthly precip: ", sprintf("%.2f", value), " in"
        ),
        hovertemplate = "%{text}<extra></extra>",
        textposition  = "none"
      )
  } else {
    fig <- fig %>%
      add_annotations(
        text      = "Precipitation data not yet available \u2014 run update pipeline",
        x = 0.5, xref = "x3 domain",
        y = 0.5, yref = "y3 domain",
        showarrow = FALSE,
        font      = list(size = 12, color = "#aaa")
      )
  }

  # Default range: last ~5 water years
  # Most recent bar_date available drives the right end
  all_dates <- c(
    if (nrow(df_swe) > 0)    max(df_swe$bar_date)    else NULL,
    if (nrow(df_temp) > 0)   max(df_temp$bar_date)   else NULL,
    if (nrow(df_precip) > 0) max(df_precip$bar_date) else NULL
  )
  range_end   <- if (length(all_dates) > 0) max(all_dates) + 60 else Sys.Date()
  range_start <- range_end - 365 * 5

  axes[["xaxis3"]] <- modifyList(x_base, list(
    anchor         = "y3",
    matches        = "x",
    showticklabels = TRUE,
    tickformat     = "%Y",
    dtick          = "M12",
    range          = list(
      format(range_start, "%Y-%m-%d"),
      format(range_end,   "%Y-%m-%d")
    ),
    rangeslider = list(
      visible     = TRUE,
      thickness   = 0.04,
      bgcolor     = "#f0f4f8",
      bordercolor = "#cccccc",
      borderwidth = 1
    )
  ))

  axes[["yaxis3"]] <- list(
    domain      = yd$p3,
    anchor      = "x3",
    title       = "",
    zeroline    = TRUE,
    zerolinecolor = "#999999",
    zerolinewidth = 1.2,
    showgrid    = TRUE,
    gridcolor   = .cc_grid,
    ticksuffix  = " in",
    tickfont    = list(size = 8)
  )

  # -- Panel title annotations -------------------------------------------------
  ann <- lapply(seq_along(panel_titles), function(i) {
    pd <- yd[[paste0("p", i)]]
    list(
      text      = paste0("<b>", panel_titles[[i]], "</b>"),
      x = 0, xref = "paper",
      y = pd[2], yref = "paper",
      xanchor   = "left",
      yanchor   = "bottom",
      showarrow = FALSE,
      font      = list(size = 10, color = "#444")
    )
  })

  final_layout <- c(
    axes,
    list(
      annotations = ann,
      title = list(
        text = paste0(
          "<b>Washington State \u2014 Snowpack & Climate Comparison</b><br>",
          "<sup>Drag slider to navigate  |  ",
          "Scroll/pinch to zoom  |  ",
          "Hover for values  |  ",
          "SNODAS Oct 2003\u2013present</sup>"
        ),
        x = 0.5,
        font = list(size = 13)
      ),
      margin        = list(t = 70, b = 10, l = 75, r = 20),
      hovermode     = "x",
      paper_bgcolor = "white",
      plot_bgcolor  = "white",
      bargap        = 0.1
    )
  )

  do.call(plotly::layout, c(list(fig), final_layout)) %>%
    plotly::config(
      responsive             = TRUE,
      displayModeBar         = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d")
    )
}

# -- Download builder ----------------------------------------------------------

build_climate_compare_download <- function(df_swe, df_temp, df_precip) {

  swe_dl <- df_swe %>%
    select(
      Date                  = bar_date,
      Water_Year            = wy,
      Month_Num             = month_num,
      SWE_Change_AF         = delta_af
    )

  temp_dl <- if (nrow(df_temp) > 0) {
    df_temp %>%
      select(
        Date              = bar_date,
        Temp_Avg_degF     = value,
        Temp_Anomaly_degF = anomaly
      )
  } else {
    data.frame(Date = as.Date(character()),
               Temp_Avg_degF = numeric(),
               Temp_Anomaly_degF = numeric())
  }

  precip_dl <- if (nrow(df_precip) > 0) {
    df_precip %>%
      select(
        Date               = bar_date,
        Precip_in          = value,
        Precip_Anomaly_in  = anomaly
      )
  } else {
    data.frame(Date = as.Date(character()),
               Precip_in = numeric(),
               Precip_Anomaly_in = numeric())
  }

  # Join monthly series on Date (all are mid-month: the 15th)
  swe_dl %>%
    full_join(temp_dl,   by = "Date") %>%
    full_join(precip_dl, by = "Date") %>%
    arrange(Date)
}