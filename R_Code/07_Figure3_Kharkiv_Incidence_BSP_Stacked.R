# ============================================================
# REVISED TWO-PANEL FIGURE
#
# Panel A:
# Monthly COVID-19 incidence in Kharkiv Region
#
# Panel B:
# Effective Population Size Dynamics of SARS-CoV-2
#
# Final revisions:
# - "War Start"
# - "Kharkiv Region"
# - both panels start at July 2020
# - all observations before July 2020 removed
# - larger A and B panel labels
# - consistent A/B label positions
# - War Start lines clipped inside each panel
# - 2 rows x 1 column
# - width = 7, height = 12
# ============================================================


rm(list = ls())
graphics.off()

library(dplyr)


# ============================================================
# 1. PATHS
# ============================================================

setwd(
  "C:/Users/mehed/OneDrive/RESEARCH WORKS/Ukraine Data Set/COVID incident data"
)


# ============================================================
# 2. LOAD INCIDENCE DATA
# ============================================================

load(
  "C:/Users/mehed/OneDrive/RESEARCH WORKS/Ukraine Data Set/COVID incident data/part01_07_covid19_annual_data_monthly_aggregated.RData"
)

df <- part01_07_covid19_annual_data_monthly_aggregated


load(
  "C:/Users/mehed/OneDrive/RESEARCH WORKS/Ukraine Data Set/COVID incident data/part01_08_covid19_annual_data_monthly_aggregated.RData"
)

df1 <- part01_08_covid19_annual_data_monthly_aggregated


# ============================================================
# 3. LOAD BSP DATA
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
# 4. HELPER FUNCTIONS
# ============================================================

make_region_std <- function(dat) {

  if ("region_new" %in% names(dat)) {

    dat <- dat %>%
      mutate(
        region_std = region_new
      )

  } else if ("region" %in% names(dat)) {

    dat <- dat %>%
      mutate(
        region_std = region
      )

  } else {

    stop(
      "No region column found (expected 'region' or 'region_new')."
    )
  }

  dat
}


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
# Shared x-axis function
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
# 5. PREPARE INCIDENCE DATA
# ============================================================

df <- make_region_std(
  df
)

df1 <- make_region_std(
  df1
)


all_data <-
  bind_rows(
    df,
    df1
  ) %>%

  mutate(
    date =
      as.Date(
        period_last_date
      )
  ) %>%

  filter(
    region_std ==
      "Oblast"
  )


oblast_monthly <-
  all_data %>%

  group_by(
    date
  ) %>%

  arrange(
    desc(
      total_cases
    )
  ) %>%

  slice(
    1
  ) %>%

  ungroup() %>%

  arrange(
    date
  )


# ============================================================
# 6. PREPARE BSP DATA
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
# 7. WAR START
# ============================================================

war_start <-
  as.Date(
    "2022-02-24"
  )


# ============================================================
# 8. SHARED X-AXIS RANGE
#
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
# 9. COLORS
# ============================================================

prewar_color <-
  "#238443"


wartime_color <-
  "#d9f0a3"


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
# 10. OUTPUT FILE
# ============================================================

out_pdf <-
  "C:/Users/mehed/OneDrive/RESEARCH WORKS/Ukraine Data Set/COVID incident data/For new article_revised_final_July2020_AB_fixed.pdf"


if (
  capabilities(
    "cairo"
  )
) {

  cairo_pdf(
    filename = out_pdf,
    width = 7,
    height = 12,
    bg = "white"
  )

} else {

  pdf(
    file = out_pdf,
    width = 7,
    height = 12
  )
}


# ============================================================
# 11. FIGURE LAYOUT
# ============================================================

par(

  mfrow =
    c(
      2,
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
# PANEL A
# MONTHLY COVID-19 INCIDENCE
# ============================================================

x1 <-
  oblast_monthly$date


y1 <-
  oblast_monthly$total_cases


# ------------------------------------------------------------
# Remove observations before July 2020
# ------------------------------------------------------------

keep1 <-
  x1 >= x_start


x1 <-
  x1[
    keep1
  ]


y1 <-
  y1[
    keep1
  ]


# ------------------------------------------------------------
# Make sure plotting is clipped inside panel
# ------------------------------------------------------------

par(
  xpd = FALSE
)


plot(

  x1,
  y1,

  type =
    "n",

  xlim =
    c(
      x_start,
      x_end
    ),

  ylim =
    c(
      0,
      max(
        y1,
        na.rm = TRUE
      ) *
        1.04
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
# PANEL A Y-AXIS TITLE
# ------------------------------------------------------------

mtext(
  "Monthly Incidence",
  side = 2,
  line = 3.6,
  cex = 1.25
)


# ------------------------------------------------------------
# PANEL A TITLE
# ------------------------------------------------------------

mtext(
  "Temporal Trends in Monthly ARIs Incidence",
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


# ------------------------------------------------------------
# PANEL A LABEL
#
# Larger than previous version
# Same relative position used for Panel B
# ------------------------------------------------------------

mtext(
  "A",
  side = 3,
  line = 0.5,
  adj = -0.075,
  cex = 2.5,
  font = 1
)


# ------------------------------------------------------------
# FULL INCIDENCE SERIES FIRST
#
# Draw complete series in dark green.
# This maintains the connecting line from the final
# pre-war observation to the first wartime observation.
# ------------------------------------------------------------

lines(
  x = x1,
  y = y1,
  col = prewar_color,
  lwd = 5,
  type = "l"
)


lines(
  x = x1,
  y = y1,
  col = prewar_color,
  lwd = 10,
  pch = 19,
  type = "p"
)


# ------------------------------------------------------------
# WARTIME DATA OVERLAY
# ------------------------------------------------------------

wartime <-
  x1 >=
  war_start


lines(
  x = x1[
    wartime
  ],

  y = y1[
    wartime
  ],

  col = wartime_color,
  lwd = 5,
  type = "l"
)


lines(
  x = x1[
    wartime
  ],

  y = y1[
    wartime
  ],

  col = wartime_color,
  lwd = 10,
  pch = 19,
  type = "p"
)


# ------------------------------------------------------------
# WAR START
#
# Using abline keeps it inside Panel A
# ------------------------------------------------------------

abline(
  v = war_start,
  col = war_color,
  lwd = 2.5,
  lty = 2
)


# ------------------------------------------------------------
# PANEL A Y-AXIS
# ------------------------------------------------------------

y1_ticks <-
  pretty(
    c(
      0,
      y1
    ),
    n = 5
  )


axis(
  2,

  at =
    y1_ticks,

  labels =
    format(
      y1_ticks,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),

  las =
    1,

  cex.axis =
    1.0
)


# ------------------------------------------------------------
# PANEL A X-AXIS
# ------------------------------------------------------------

draw_time_axis(
  x_ticks_labeled = x_ticks,
  x_labels = x_labs,
  srt = 45,
  cex = 0.78,
  y_npc = -0.085
)


# ------------------------------------------------------------
# PANEL A LEGEND
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
      "Pre-War Data",
      "Wartime Data",
      "War Start"
    ),

  col =
    c(
      prewar_color,
      wartime_color,
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
      2.5,
      2.5,
      2.5
    ),

  pch =
    c(
      19,
      19,
      NA
    ),

  pt.cex =
    c(
      1.15,
      1.15,
      NA
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
# PANEL B
# BSP EFFECTIVE POPULATION SIZE
# ============================================================


# ------------------------------------------------------------
# Keep valid BSP values
# AND remove observations before July 2020
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
# Make sure plotting is clipped inside Panel B
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
# PANEL B Y-AXIS TITLE
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
# PANEL B TITLE
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


# ------------------------------------------------------------
# PANEL B LABEL
#
# Same size and relative position as Panel A
# ------------------------------------------------------------

mtext(
  "B",
  side = 3,
  line = 0.5,
  adj = -0.075,
  cex = 2.5,
  font = 1
)


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
#
# This remains inside Panel B
# ------------------------------------------------------------

abline(
  v = war_start,
  col = war_color,
  lwd = 1.7,
  lty = 2
)


# ------------------------------------------------------------
# PANEL B Y-AXIS
# ------------------------------------------------------------

axis(
  2,
  las = 1,
  cex.axis = 1.0
)


# ------------------------------------------------------------
# PANEL B X-AXIS
# ------------------------------------------------------------

draw_time_axis(
  x_ticks_labeled = x_ticks,
  x_labels = x_labs,
  srt = 45,
  cex = 0.78,
  y_npc = -0.085
)


# ------------------------------------------------------------
# PANEL B LEGEND
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
# 12. CLOSE PDF
# ============================================================

dev.off()


cat(
  "\nSaved successfully:\n",
  out_pdf,
  "\n"
)