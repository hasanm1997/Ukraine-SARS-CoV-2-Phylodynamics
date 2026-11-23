# ============================================================
# PANEL B ONLY
#
# Effective Population Size Dynamics of SARS-CoV-2
# in Kharkiv Region
#
# Requested version:
# - Panel B only
# - No panel label
# - width = 7 inches
# - height = 6 inches
# - starts at July 2020
# - all observations before July 2020 removed
# ============================================================


rm(list = ls())
graphics.off()


# ============================================================
# 1. LOAD BSP DATA
# ============================================================

file_med <-
  "C:/Users/mehed/OneDrive/RESEARCH WORKS/Ukraine Data Set/DATA/Final Date sets/Resampled_100/OUTPUT/Log files/SKYLINE_TABLES_Median/BSP_percentiles_by_calendar_year.csv"


dt <- read.csv(
  file_med,
  check.names = FALSE
)


dt <- dt[
  order(dt$calendar_year),
]


# ============================================================
# 2. HELPER FUNCTIONS
# ============================================================

decimal_year_to_date <- function(y) {
  
  yr <- floor(y)
  
  frac <- y - yr
  
  as.Date(
    paste0(
      yr,
      "-01-01"
    )
  ) +
    round(
      frac * 365.25
    )
}


month_start <- function(d) {
  
  as.Date(
    format(
      d,
      "%Y-%m-01"
    )
  )
}


# ------------------------------------------------------------
# X-axis function
# ------------------------------------------------------------

draw_time_axis <- function(
    x_ticks_labeled,
    x_labels,
    srt = 45,
    cex = 0.80,
    tick_tcl = -0.25,
    y_npc = -0.085) {
  
  axis(
    1,
    at = x_ticks_labeled,
    labels = FALSE,
    tcl = tick_tcl
  )
  
  y <-
    grconvertY(
      y_npc,
      from = "npc",
      to = "user"
    )
  
  par(
    xpd = NA
  )
  
  text(
    x = x_ticks_labeled,
    y = y,
    labels = x_labels,
    srt = srt,
    adj = 1,
    cex = cex
  )
  
  par(
    xpd = FALSE
  )
}


# ============================================================
# 3. PREPARE BSP DATA
# ============================================================

stopifnot(
  all(
    c(
      "2.5%",
      "50%",
      "97.5%",
      "calendar_year"
    ) %in%
      names(dt)
  )
)


dt$date <-
  month_start(
    decimal_year_to_date(
      dt$calendar_year
    )
  )


# ============================================================
# 4. WAR START
# ============================================================

war_start <-
  as.Date(
    "2022-02-24"
  )


# ============================================================
# 5. X-AXIS RANGE
# Start at July 2020
# ============================================================

x_start <-
  as.Date(
    "2020-07-01"
  )


x_end <-
  as.Date(
    "2024-12-31"
  )


# ------------------------------------------------------------
# Complete monthly sequence
# ------------------------------------------------------------

x_ticks_all <-
  seq(
    from = x_start,
    to = x_end,
    by = "1 month"
  )


x_labs_all <-
  paste0(
    format(
      x_ticks_all,
      "%Y"
    ),
    "-",
    as.integer(
      format(
        x_ticks_all,
        "%m"
      )
    )
  )


# ------------------------------------------------------------
# Label every 3 months
# ------------------------------------------------------------

every_n <- 3


idx_lab <-
  seq(
    1,
    length(
      x_ticks_all
    ),
    by = every_n
  )


x_ticks <-
  x_ticks_all[
    idx_lab
  ]


x_labs <-
  x_labs_all[
    idx_lab
  ]


# ============================================================
# 6. COLORS
# ============================================================

war_color <-
  "red"


bsp_color <-
  "#A93226"


bsp_ci_color <-
  adjustcolor(
    bsp_color,
    alpha.f = 0.30
  )


# ============================================================
# 7. OUTPUT FILE
# ============================================================

out_pdf <-
  "C:/Users/mehed/OneDrive/RESEARCH WORKS/Ukraine Data Set/COVID incident data/Panel_B_only_7x6_no_label.pdf"


if (
  capabilities(
    "cairo"
  )
) {
  
  cairo_pdf(
    filename = out_pdf,
    width = 7,
    height = 6,
    bg = "white"
  )
  
} else {
  
  pdf(
    file = out_pdf,
    width = 7,
    height = 6
  )
}


# ============================================================
# 8. FIGURE LAYOUT
# ============================================================

par(
  
  mfrow =
    c(
      1,
      1
    ),
  
  mar =
    c(
      5.5,
      5.1,
      5.1,
      2.1
    ),
  
  mgp =
    c(
      3.5,
      0.8,
      0
    ),
  
  xaxs =
    "i"
)


# ============================================================
# PANEL B ONLY
# EFFECTIVE POPULATION SIZE
# ============================================================


# ------------------------------------------------------------
# Keep valid BSP values
# and remove observations before July 2020
# ------------------------------------------------------------

keep <-
  is.finite(
    dt$date
  ) &
  is.finite(
    dt[["50%"]]
  ) &
  is.finite(
    dt[["2.5%"]]
  ) &
  is.finite(
    dt[["97.5%"]]
  ) &
  dt[["50%"]] > 0 &
  dt[["2.5%"]] > 0 &
  dt[["97.5%"]] > 0 &
  dt$date >= x_start


x2 <-
  dt$date[
    keep
  ]


y_med <-
  dt[["50%"]][
    keep
  ]


y_lo <-
  dt[["2.5%"]][
    keep
  ]


y_hi <-
  dt[["97.5%"]][
    keep
  ]


# ------------------------------------------------------------
# Keep plotting clipped inside panel
# ------------------------------------------------------------

par(
  xpd = FALSE
)


plot(
  
  x2,
  y_med,
  
  type =
    "n",
  
  log =
    "y",
  
  xlim =
    c(
      x_start,
      x_end
    ),
  
  ylim =
    range(
      c(
        y_lo,
        y_hi
      ),
      na.rm = TRUE
    ),
  
  main =
    "",
  
  xlab =
    "",
  
  ylab =
    "",
  
  xaxt =
    "n",
  
  yaxt =
    "n",
  
  cex.axis =
    1.0,
  
  cex.lab =
    1.25
)


# ------------------------------------------------------------
# Y-AXIS TITLE
# ------------------------------------------------------------

mtext(
  expression(
    N[e]
  ),
  side = 2,
  line = 3.4,
  cex = 1.25
)


# ------------------------------------------------------------
# TITLE
# ------------------------------------------------------------

mtext(
  "Effective Population Size Dynamics of SARS-CoV-2",
  side = 3,
  line = 2.3,
  cex = 1.22,
  font = 2
)


mtext(
  "in Kharkiv Region",
  side = 3,
  line = 1.0,
  cex = 1.22,
  font = 2
)


# ============================================================
# IMPORTANT:
# NO "B" PANEL LABEL IN THIS VERSION
# ============================================================


# ------------------------------------------------------------
# 95% CREDIBLE INTERVAL
# ------------------------------------------------------------

polygon(
  
  c(
    x2,
    rev(
      x2
    )
  ),
  
  c(
    y_lo,
    rev(
      y_hi
    )
  ),
  
  col =
    bsp_ci_color,
  
  border =
    NA
)


# ------------------------------------------------------------
# MEDIAN ESTIMATE
# ------------------------------------------------------------

fit <-
  smooth.spline(
    
    as.numeric(
      x2
    ),
    
    log10(
      y_med
    ),
    
    spar =
      0.10
  )


y_smooth <-
  10^(
    predict(
      fit,
      as.numeric(
        x2
      )
    )$y
  )


lines(
  x2,
  y_smooth,
  col = "black",
  lwd = 2.2
)


# ------------------------------------------------------------
# WAR START
# ------------------------------------------------------------

abline(
  v = war_start,
  col = war_color,
  lwd = 1.7,
  lty = 2
)


# ------------------------------------------------------------
# Y-AXIS
# ------------------------------------------------------------

axis(
  2,
  las = 1,
  cex.axis = 1.0
)


# ------------------------------------------------------------
# X-AXIS
# ------------------------------------------------------------

draw_time_axis(
  x_ticks_labeled = x_ticks,
  x_labels = x_labs,
  srt = 45,
  cex = 0.78,
  y_npc = -0.085
)


# ------------------------------------------------------------
# LEGEND
# ------------------------------------------------------------

legend(
  
  "topleft",
  
  inset =
    c(
      0.015,
      0.015
    ),
  
  legend =
    c(
      expression(
        paste(
          "Median Estimate (",
          N[e],
          ")"
        )
      ),
      
      "95% Credible Interval",
      
      "War Start"
    ),
  
  col =
    c(
      "black",
      bsp_ci_color,
      war_color
    ),
  
  lty =
    c(
      1,
      1,
      2
    ),
  
  lwd =
    c(
      2.2,
      7,
      1.7
    ),
  
  cex =
    0.82,
  
  bty =
    "n"
)


box(
  lwd = 1
)


# ============================================================
# 9. CLOSE PDF
# ============================================================

dev.off()


cat(
  "\nSaved successfully:\n",
  out_pdf,
  "\n"
)