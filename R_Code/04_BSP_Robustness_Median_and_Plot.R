rm(list=ls())
graphics.off()
library(data.table)

# ======================================================
# SETTINGS  (edit paths only)
# ======================================================
in_dir <- "C:/Users/mehed/Documents/Ukraine Data Set/DATA/Final Date sets/Resampled_100/OUTPUT/Log files/SKYLINE_TABLES_Median"

out_tsv <- file.path(in_dir, "BSP_percentiles_by_calendar_year.tsv")
out_csv <- file.path(in_dir, "BSP_percentiles_by_calendar_year.csv")

value_col  <- "median"   # use median Ne from each run (recommended)
round_year <- 6           # stabilize floating calendar years
# ======================================================

# ------------------------------------------------------
# Read all skyline tables
# ------------------------------------------------------
in_dir <- trimws(in_dir)   # remove trailing/leading spaces
in_dir <- normalizePath(in_dir, winslash = "/", mustWork = TRUE)

files <- list.files(in_dir,
                    pattern = "_skyline_table\\.tsv$",
                    full.names = TRUE)

files <- normalizePath(trimws(files), winslash = "/", mustWork = TRUE)

stopifnot(length(files) > 0)



all_dt <- rbindlist(
  lapply(files, function(f) {
    dt <- fread(f, sep = "\t")   # IMPORTANT
    
    if (!("calendar_year" %in% names(dt)))
      stop("calendar_year column missing in ", basename(f),
           "\nColumns found: ", paste(names(dt), collapse = ", "))
    
    if (!(value_col %in% names(dt)))
      stop(value_col, " column missing in ", basename(f),
           "\nColumns found: ", paste(names(dt), collapse = ", "))
    
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





# ===== Robust BSP plot from percentile table (ggplot2) =====
# install.packages("ggplot2")  # if needed

library(ggplot2)

file_med <- "C:/Users/mehed/Documents/Ukraine Data Set/DATA/Final Date sets/Resampled_100/OUTPUT/Log files/SKYLINE_TABLES_Median/BSP_percentiles_by_calendar_year.csv"
dt <- read.csv(file_med, check.names = FALSE)
dt <- dt[order(dt$calendar_year), ]



p <- ggplot(dt, aes(x = calendar_year)) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.25) +
  geom_line(aes(y = `50%`), linewidth = 1.1) +
  scale_y_log10() +   # remove this line if you want linear y-axis
  labs(
    x = "Calendar year",
    y = "Effective population size (Ne) using ",
    title = "Robust Bayesian Skyline (100)(Tree_height_median_on ALL 100 LOGS"
  ) +
  theme_bw()

print(p)

# Save
pdf_out <- sub("\\.csv$", "_robust_bsp.pdf", file_med)

ggsave(
  filename = pdf_out,
  plot = p,
  width = 8,
  height = 4.5,
  device = "pdf"
)

cat("Saved PDF to:", pdf_out, "\n")
