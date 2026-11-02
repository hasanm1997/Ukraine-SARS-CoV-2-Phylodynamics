rm(list=ls())
graphics.off()

library(data.table)

# ---------- SETTINGS ----------


log_dir <- "C:/Users/mehed/Documents/Ukraine Data Set/DATA/Final Date sets/Resampled_100/OUTPUT/Log files"
#log_dir <- "C:/Users/mehed/Documents/Ukraine Data Set/DATA/Final Date sets/Resampled_100/OUTPUT/Log files"
burnin_frac <- 0.10
root_col <- "Tree.height"
# ----------------------------

log_files <- list.files(log_dir, pattern = "\\.log$", full.names = TRUE)
stopifnot(length(log_files) > 0)

# Read BEAST log: drop leading lines starting with "#"
read_beast_log <- function(path) {
  lines <- readLines(path, warn = FALSE)
  
  # keep only non-comment lines
  keep <- !grepl("^\\s*#", lines)
  lines2 <- lines[keep]
  
  # fread can read from a text string via 'text='
  fread(text = paste(lines2, collapse = "\n"))
}

per_run <- vector("list", length(log_files))

for (i in seq_along(log_files)) {
  f <- log_files[i]
  
  dt <- read_beast_log(f)
  
  if (!(root_col %in% names(dt))) {
    warning(sprintf("Missing %s in %s", root_col, basename(f)))
    next
  }
  
  burnin_n <- floor(nrow(dt) * burnin_frac)
  if (burnin_n >= nrow(dt)) {
    warning(sprintf("Burn-in too large in %s", basename(f)))
    next
  }
  
  dt2 <- dt[(burnin_n + 1):.N]
  rh <- dt2[[root_col]]
  rh <- rh[is.finite(rh)]
  
  if (length(rh) == 0) {
    warning(sprintf("No finite root heights after burn-in in %s", basename(f)))
    next
  }
  
  per_run[[i]] <- data.table(
    file      = basename(f),
    n_post    = length(rh),
    rh_mean   = mean(rh),
    rh_median = median(rh),
    rh_q025   = as.numeric(quantile(rh, 0.025)),
    rh_q975   = as.numeric(quantile(rh, 0.975)),
    rh_min    = min(rh),
    rh_max    = max(rh)
  )
}

per_run_dt <- rbindlist(per_run, fill = TRUE)
stopifnot(nrow(per_run_dt) > 0)

# Global Tmax choices
Tmax_maxAcrossRuns <- max(per_run_dt$rh_q975, na.rm = TRUE)
Tmin_minAcrossRuns <- min(per_run_dt$rh_q975, na.rm = TRUE)
Tmax_q95AcrossRuns <- as.numeric(quantile(per_run_dt$rh_q975, 0.95, na.rm = TRUE))
Tmax_medianAcrossRuns <- median(per_run_dt$rh_q975, na.rm = TRUE)

cat("\n================ GLOBAL Tmax ================\n")
cat("Runs processed:", nrow(per_run_dt), "out of", length(log_files), "\n\n")
cat("Using per-run root height 97.5% quantile (rh_q975):\n")
cat("  Tmax (max across runs)          =", Tmax_maxAcrossRuns, "\n")
cat("  Tmax (95% quantile across runs) =", Tmax_q95AcrossRuns, "\n")
cat("  Tmax (median across runs)       =", Tmax_medianAcrossRuns, "\n")
cat("=============================================\n\n")

out_file <- file.path(log_dir, "rootHeight_summary_per_run.tsv")
fwrite(per_run_dt, out_file, sep = "\t")
cat("Saved per-run summary to:\n", out_file, "\n")
max(per_run_dt$rh_q975)


out_file_csv <- file.path(log_dir, "rootHeight_summary_per_run.csv")
fwrite(per_run_dt, out_file_csv)
