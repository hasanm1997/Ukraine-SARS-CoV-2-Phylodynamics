rm(list=ls())
graphics.off()
library(data.table)

# ======================================================
# SETTINGS  (edit paths only)
# ======================================================
in_dir <- "C:/Users/mehed/Documents/Ukraine Data Set/DATA/Final Date sets/Resampled_100/OUTPUT/Log files/SKYLINE_TABLES_COMMONGRID"

out_tsv <- file.path(in_dir, "BSP_percentiles_by_calendar_year_mean.tsv")
out_csv <- file.path(in_dir, "BSP_percentiles_by_calendar_year_mean.csv")

value_col  <- "mean"   # use  mean_Ne from each run (recommended)
round_year <- 6           # stabilize floating calendar years
# ======================================================

# ------------------------------------------------------
# Read all skyline tables
# ------------------------------------------------------
files <- list.files(in_dir,
                    pattern = "_skyline_table\\.tsv$",
                    full.names = TRUE)

stopifnot(length(files) > 0)

all_dt <- rbindlist(
  lapply(files, function(f) {
    dt <- fread(f)
    
    if (!("calendar_year" %in% names(dt)))
      stop("calendar_year column missing in ", basename(f))
    
    if (!(value_col %in% names(dt)))
      stop(value_col, " column missing in ", basename(f))
    
    dt <- dt[, .(
      calendar_year = round(calendar_year, round_year),
      value = get(value_col)
    )]
    
    dt[, run := basename(f)]
    dt
  }),
  use.names = TRUE,
  fill = TRUE
)

# ------------------------------------------------------
# Compute percentiles across runs at each time point
# ------------------------------------------------------
pct_dt <- all_dt[!is.na(value),
                 .(
                   `0%`    = as.numeric(quantile(value, 0.00, na.rm = TRUE)),
                   `2.5%`  = as.numeric(quantile(value, 0.025, na.rm = TRUE)),
                   `25%`   = as.numeric(quantile(value, 0.25, na.rm = TRUE)),
                   `50%`   = as.numeric(quantile(value, 0.50, na.rm = TRUE)),
                   `75%`   = as.numeric(quantile(value, 0.75, na.rm = TRUE)),
                   `97.5%` = as.numeric(quantile(value, 0.975, na.rm = TRUE)),
                   `100%`  = as.numeric(quantile(value, 1.00, na.rm = TRUE))
                 ),
                 by = calendar_year]

# Sort like Tracer (most recent year first)
setorder(pct_dt, -calendar_year)

# ------------------------------------------------------
# Save outputs
# ------------------------------------------------------
fwrite(pct_dt, out_tsv, sep = "\t")
fwrite(pct_dt, out_csv)

cat("Saved BSP robustness percentile tables:\n")
cat("TSV:", out_tsv, "\n")
cat("CSV:", out_csv, "\n\n")

# Quick sanity check
print(head(pct_dt, 10))
