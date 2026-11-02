# =============================================================================
# Figure 3: Monthly COVID-19 Incidence and Bayesian Skyline Plot, Kharkiv Oblast
# Project: Ukraine-SARS-CoV-2-Phylodynamics
#
# Purpose:
#   Create the two-panel figure used in the research manuscript:
#     Panel A - Monthly COVID-19 incidence in Kharkiv Oblast
#     Panel B - Effective population size (Ne) from BSP percentile summaries
#
# Required inputs:
#   part01_07_covid19_annual_data_monthly_aggregated.RData
#   part01_08_covid19_annual_data_monthly_aggregated.RData
#   BSP_percentiles_by_calendar_year.csv
#
# Output:
#   Figure3_Kharkiv_Incidence_and_BSP.pdf
#
# Note:
#   Original plotting code and settings are retained. Local Windows input/output
#   paths must be edited when running on a different computer.
# =============================================================================

rm(list = ls())
graphics.off()

library(dplyr)

# =========================
# Paths
# =========================
setwd("C:/Users/mehed/OneDrive/RESEARCH WORKS/Ukraine Data Set/COVID incident data")

load("C:/Users/mehed/OneDrive/RESEARCH WORKS/Ukraine Data Set/COVID incident data/part01_07_covid19_annual_data_monthly_aggregated.RData")
df  <- part01_07_covid19_annual_data_monthly_aggregated

load("C:/Users/mehed/OneDrive/RESEARCH WORKS/Ukraine Data Set/COVID incident data/part01_08_covid19_annual_data_monthly_aggregated.RData")
df1 <- part01_08_covid19_annual_data_monthly_aggregated

file_med <- "C:/Users/mehed/OneDrive/RESEARCH WORKS/Ukraine Data Set/DATA/Final Date sets/Resampled_100/OUTPUT/Log files/SKYLINE_TABLES_Median/BSP_percentiles_by_calendar_year.csv"
dt <- read.csv(file_med, check.names = FALSE)
dt <- dt[order(dt$calendar_year), ]

# =========================
# Helpers
# =========================
make_region_std <- function(dat) {
  if ("region_new" %in% names(dat)) {
    dat <- dat %>% mutate(region_std = region_new)
  } else if ("region" %in% names(dat)) {
    dat <- dat %>% mutate(region_std = region)
  } else {
    stop("No region column found (expected 'region' or 'region_new').")
  }
  dat
}

decimal_year_to_date <- function(y) {
  yr <- floor(y)
  frac <- y - yr
  as.Date(paste0(yr, "-01-01")) + round(frac * 365.25)
}

month_start <- function(d) as.Date(format(d, "%Y-%m-01"))

# Angled x-axis labels AND ticks that match labels (1:1)
draw_time_axis <- function(x_ticks_labeled, x_labels,
                           srt = 35, cex = 0.85,
                           tick_tcl = -0.30,
                           y_npc = -0.10) {
  # ticks ONLY where we label
  axis(1, at = x_ticks_labeled, labels = FALSE, tcl = tick_tcl)
  
  # stable y placement (works for log plots too)
  y <- grconvertY(y_npc, from = "npc", to = "user")
  
  par(xpd = NA)
  text(x = x_ticks_labeled, y = y, labels = x_labels,
       srt = srt, adj = 1, cex = cex)
  par(xpd = FALSE)
}

# Consistent A/B tag in the top-left corner of each panel
panel_tag_topleft <- function(tag = "A", cex = 1.6) {
  par(xpd = NA)
  
  # NPC coords: (0,0)=bottom-left of current panel; (1,1)=top-right
  x <- grconvertX(-0.13, from = "npc", to = "user")  # a bit inside from left edge
  y <- grconvertY(1.05,  from = "npc", to = "user")  # near the top
  
  text(x, y, tag, font = 2, cex = cex)
  par(xpd = FALSE)
}

# =========================
# Plot 1 data
# =========================
df  <- make_region_std(df)
df1 <- make_region_std(df1)

all_data <- bind_rows(df, df1) %>%
  mutate(date = as.Date(period_last_date)) %>%
  filter(region_std == "Oblast")

oblast_monthly <- all_data %>%
  group_by(date) %>%
  arrange(desc(total_cases)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(date)

# =========================
# Plot 2 data -> Date
# =========================
stopifnot(all(c("2.5%","50%","97.5%","calendar_year") %in% names(dt)))
dt$date <- month_start(decimal_year_to_date(dt$calendar_year))

# =========================
# Shared X axis (FROM plot 2)
# =========================
# =========================
# Shared X axis (FROM plot 2)
# =========================
war_start <- as.Date("2022-02-24")

x_start <- as.Date("2020-01-01")
x_end   <- as.Date("2024-12-31")

# Full monthly sequence
x_ticks_all <- seq(from = x_start, to = x_end, by = "1 month")
x_labs_all  <- paste0(format(x_ticks_all, "%Y"), "-", as.integer(format(x_ticks_all, "%m")))

# Label every 3 months (poster clean) AND only keep ticks that have labels
every_n <- 3
idx_lab <- seq(1, length(x_ticks_all), by = every_n)
x_ticks <- x_ticks_all[idx_lab]
x_labs  <- x_labs_all[idx_lab]


# =========================
# Output
# =========================
out_pdf <- "C:/Users/mehed/OneDrive/RESEARCH WORKS/Ukraine Data Set/COVID incident data/Figure3_Kharkiv_Incidence_and_BSP.pdf"

if (capabilities("cairo")) {
  cairo_pdf(filename = out_pdf, width = 10.2, height = 7.6, bg = "white")
} else {
  pdf(file = out_pdf, width = 10.2, height = 7.6)
}

# Layout: 2 rows, 1 col
# Reduce left outer margin (was too large)
par(mfrow = c(2, 1), xaxs = "i", oma = c(0.2, 1.4, 0.1, 0.2))

# =========================
# PANEL A (Top): Incidence
# =========================
#par(mar = c(4.2, 6.2, 3.2, 2.0), mgp = c(3.8, 1.0, 0))
par(mar = c(4.2, 6.2, 3.2, 2.5), mgp = c(3.8, 1.0, 0))

x1 <- oblast_monthly$date
y1 <- oblast_monthly$total_cases

plot(x1, y1,
     type = "n",
     xlim = c(x_start, x_end),
     ylim = range(y1, na.rm = TRUE),
     main = "Temporal Trends in Monthly COVID-19 Incidence in Kharkiv Oblast",
     xlab = "", ylab = "Monthly Incidence",
     xaxt = "n", yaxt = "n"
)

lines(x1, y1, col = "navy", lwd = 2)
points(x1, y1, col = "navy", pch = 19, cex = 0.85)

abline(v = war_start, col = "red", lwd = 2, lty = 2)

# y axis (comma)
y1_ticks <- pretty(y1, n = 5)
axis(2, at = y1_ticks,
     labels = format(y1_ticks, big.mark = ",", scientific = FALSE),
     las = 1)

# x axis (ticks match labels)
draw_time_axis(x_ticks, x_labs, srt = 35, cex = 0.85, y_npc = -0.10)
#mtext("Time", side = 1, line = 3.2, cex = 1.0)

# Legend: smaller and moved so it doesn't touch the data
legend("topleft",
       inset = c(0.03, 0.04),
       legend = c("Monthly Aggregated Data", "Russian Invasion"),
       fill = c("navy", "red"),
       border = NA,
       bty = "o",
       bg = "white",
       cex = 0.72)

#panel_tag_topleft("A", cex = 1.4)
panel_tag_topleft("A", cex = 1.3)
box(lwd = 1)

# =========================
# PANEL B (Bottom): BSP
# =========================
#par(mar = c(4.7, 6.2, 3.2, 2.0), mgp = c(3.8, 1.0, 0))
par(mar = c(4.7, 6.2, 3.2, 2.5), mgp = c(3.8, 1.0, 0))

keep <- is.finite(dt$date) & is.finite(dt[["50%"]]) & is.finite(dt[["2.5%"]]) & is.finite(dt[["97.5%"]]) &
  (dt[["50%"]] > 0) & (dt[["2.5%"]] > 0) & (dt[["97.5%"]] > 0)

x2 <- dt$date[keep]
y_med <- dt[["50%"]][keep]
y_lo  <- dt[["2.5%"]][keep]
y_hi  <- dt[["97.5%"]][keep]

plot(x2, y_med,
     type = "n",
     log = "y",
     xlim = c(x_start, x_end),
     ylim = range(c(y_lo, y_hi), na.rm = TRUE),
     main = "Effective Population Size Dynamics of SARS-CoV-2 in Kharkiv Oblast (Log Scale)",
     xlab = "", ylab = expression(N[e]),
     xaxt = "n", yaxt = "n"
)

# ribbon (95% CI)
polygon(c(x2, rev(x2)), c(y_lo, rev(y_hi)),
        col = adjustcolor("#A93226", alpha.f = 0.30),
        border = NA)

# median line (light smoothing)
fit <- smooth.spline(as.numeric(x2), log10(y_med), spar = 0.10)
y_smooth <- 10^(predict(fit, as.numeric(x2))$y)
lines(x2, y_smooth, col = "black", lwd = 2)

# war start line
abline(v = war_start, col = "red", lwd = 2, lty = 2)

# y axis
axis(2, las = 1)

# x axis (ticks match labels)
draw_time_axis(x_ticks, x_labs, srt = 35, cex = 0.85, y_npc = -0.10)
#mtext("Time", side = 1, line = 3.2, cex = 1.0)

# Legend: explain CI + median, include CI color
legend("topleft",
       inset = c(0.03, 0.04),
       legend = c("Median Estimate (Ne)",
                  "95% Credible Interval",
                  "Russian Invasion"),
       fill = c("black",
                adjustcolor("#A93226", alpha.f = 0.30),
                "red"),
       border = NA,
       bty = "o",
       bg = "white",
       cex = 0.72)

#panel_tag_topleft("B", cex = 1.4)
panel_tag_topleft("B", cex = 1.3)
box(lwd = 1)

dev.off()
cat("Saved:", out_pdf, "\n")
