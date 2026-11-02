# =============================================================================
# Kharkiv Oblast: Monthly COVID-19 Incidence Plot
# Project: Ukraine SARS-CoV-2 Phylogenetic and Phylodynamic Analysis
# Purpose: Combine monthly incidence datasets and plot Kharkiv Oblast COVID-19
#          incidence with the Russian invasion date (February 24, 2022).
# Inputs:  part01_07_covid19_annual_data_monthly_aggregated.RData
#          part01_08_covid19_annual_data_monthly_aggregated.RData
# Outputs: kharkiv_oblast_monthly_Incidence.pdf
#          kharkiv_oblast_monthly_Incidence.png
# =============================================================================

rm(list = ls())
graphics.off()

library(dplyr)
library(lubridate)
library(ggplot2)

setwd("C:/Users/mehed/Documents/Ukraine Data Set/COVID incident data")

load("C:/Users/mehed/Documents/Ukraine Data Set/COVID incident data/part01_07_covid19_annual_data_monthly_aggregated.RData")
df  <- part01_07_covid19_annual_data_monthly_aggregated

load("C:/Users/mehed/Documents/Ukraine Data Set/COVID incident data/part01_08_covid19_annual_data_monthly_aggregated.RData")
df1 <- part01_08_covid19_annual_data_monthly_aggregated

# ----------------------------
# 1) Standardize region column
# ----------------------------
# Some files use "region", others "region_new"
# Create a single column name called region_std.
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

df  <- make_region_std(df)
df1 <- make_region_std(df1)

# ----------------------------
# 2) Stack + keep only Oblast
# ----------------------------
all_data <- bind_rows(df, df1) %>%
  mutate(date = as.Date(period_last_date)) %>%   # end-of-month date
  filter(region_std == "Oblast")

# ----------------------------
# 3) Remove duplicate months (overlap 2021-2023)
#    Keep the row with larger total_cases (safer if one file is corrected)
# ----------------------------
oblast_monthly <- all_data %>%
  group_by(date) %>%
  arrange(desc(total_cases)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(date)

# ----------------------------
# 4) Line plot with year ticks like your screenshot
# ----------------------------
library(ggplot2)
library(scales)

lab_month <- "Monthly aggregated data"
lab_inv   <- "Russian Invasion"

# 1) Add a real legend column to the main data
oblast_monthly$grp <- lab_month

# 2) Separate vline data with same legend column name
vlines <- data.frame(
  xintercept = as.Date("2022-02-24"),
  grp = lab_inv
)

p_final <- ggplot(oblast_monthly, aes(x = date, y = total_cases)) +
  
  # Monthly series: map color to a REAL column
  geom_line(aes(color = grp), linewidth = 1.5) +
  geom_point(aes(color = grp), size = 3) +
  
  # Invasion line: map color to grp in its own data
  geom_vline(
    data = vlines,
    aes(xintercept = xintercept, color = grp),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  
  # Manual colors (now guaranteed to match)
  scale_color_manual(
    name = NULL,
    values = c(
      "Monthly aggregated data" = "navy",
      "Russian Invasion"        = "red"
    )
  ) +
  
  # Force legend to be square boxes
  guides(
    color = guide_legend(
      override.aes = list(shape = 15, size = 4, linewidth = 0)
    )
  ) +
  
  scale_x_date(
    limits = as.Date(c("2020-01-01", "2024-12-31")),
    breaks = as.Date(c("2020-01-01","2022-01-01","2024-01-01")),
    labels = c("2020", "2022", "2024"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(labels = comma) +
  
  labs(
    title = "Temporal Trends in Monthly COVID-19 Incidence in Kharkiv Oblast",
    x = "Calendar year",
    y = "Monthly Incidence"
  ) +
  
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", linewidth = 1),
    
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(fill = "white"),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.y = unit(0.15, "cm"),
    legend.text = element_text(size = 10),
    legend.box.margin = margin(2, 4, 2, 4),
    
    axis.text = element_text(color = "black")
  )

print(p_final)

ggsave("kharkiv_oblast_monthly_Incidence.pdf", p_final,
       device = cairo_pdf, width = 10.2, height = 4.2, units = "in", bg = "white")

ggsave("kharkiv_oblast_monthly_Incidence.png", p_final,
       width = 10, height = 6, units = "in", dpi = 600, bg = "white")
