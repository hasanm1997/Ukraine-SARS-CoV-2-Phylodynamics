rm(list=ls())
graphics.off()

library(data.table)

# =========================
# USER SETTINGS (edit these)
# =========================
#log_dir <- "C:/Users/mehed/Documents/Ukraine Data Set/DATA/Final Date sets/Resampled_100/OUTPUT/Log files"
log_dir <- "C:/Users/mehed/Documents/Ukraine Data Set/DATA/Final Date sets/Resampled_100/OUTPUT/Log files"
burnin_frac <- 0.10
n_bins <- 300

# This is the "Age of youngest tip" you used in Tracer (enter the same value)
youngest_tip <- 2025.63

# Confirmed column names in your .log
root_col <- "Tree.height"
pop_prefix <- "bPopSizes."
grp_prefix <- "bGroupSizes."
# =========================

# ---------- Load per-run root height summary and compute global Tmax ----------
summary_file <- file.path(log_dir, "rootHeight_summary_per_run.tsv")
stopifnot(file.exists(summary_file))

rh_sum <- fread(summary_file)

x <- rh_sum$rh_q975
x <- x[is.finite(x)]   # drop NA/Inf if any
stopifnot(length(x) > 0)

# ---- Global Tmax choices ----
Tmax_max    <- max(x)
Tmax_minimum   <- min(x)
Tmax_median <- median(x)
Tmax_q95    <- as.numeric(quantile(x, 0.95))



# Define the common time grid (time measured backwards from youngest_tip)
t_grid <- seq(0, Tmax_median, length.out = n_bins)

# ---------- Helper to read BEAST log (skip initial # comment/XML lines) ----------
find_data_start <- function(path) {
  con <- file(path, open = "r")
  on.exit(close(con), add = TRUE)
  
  k <- 0L
  repeat {
    line <- readLines(con, n = 1L, warn = FALSE)
    if (length(line) == 0L) break
    k <- k + 1L
    # first real data header line starts with "Sample"
    if (!grepl("^\\s*#", line) && grepl("^Sample\\s", line)) {
      return(k - 1L)  # fread(skip=) uses number of lines to skip
    }
  }
  stop("Could not find data header line starting with 'Sample' in: ", basename(path))
}

read_beast_log <- function(path) {
  skip_n <- find_data_start(path)
  fread(path, skip = skip_n)
}

# ---------- Skyline table builder for one run ----------
# We map each posterior sample's piecewise-constant Ne to the common grid,
# then summarize across posterior samples at each grid time.
make_skyline_table_one <- function(log_path) {
  dt <- read_beast_log(log_path)
  
  # burn-in drop
  burnin_n <- floor(nrow(dt) * burnin_frac)
  if (burnin_n >= nrow(dt)) stop("Burn-in too large for: ", basename(log_path))
  dt <- dt[(burnin_n + 1):.N]
  
  # identify pop/group columns
  pop_cols <- grep(paste0("^", gsub("\\.", "\\\\.", pop_prefix)), names(dt), value = TRUE)
  grp_cols <- grep(paste0("^", gsub("\\.", "\\\\.", grp_prefix)), names(dt), value = TRUE)
  
  if (!(root_col %in% names(dt))) stop("Missing Tree.height in: ", basename(log_path))
  if (length(pop_cols) == 0) stop("No bPopSizes.* in: ", basename(log_path))
  if (length(grp_cols) == 0) stop("No bGroupSizes.* in: ", basename(log_path))
  if (length(pop_cols) != length(grp_cols)) stop("Pop/group dimension mismatch in: ", basename(log_path))
  
  pop_mat <- as.matrix(dt[, ..pop_cols])
  grp_mat <- as.matrix(dt[, ..grp_cols])
  root_h  <- dt[[root_col]]
  
  n_samp <- nrow(pop_mat)
  n_int  <- ncol(pop_mat)
  
  # map one posterior sample i to Ne on common t_grid
  map_one <- function(i) {
    ps <- pop_mat[i, ]
    gs <- grp_mat[i, ]
    rh <- root_h[i]
    
    # interval breakpoints implied by group sizes
    breaks <- c(0, cumsum(gs))
    # scale breaks to match that sample's root height (stabilizes small rounding issues)
    if (breaks[length(breaks)] > 0) breaks <- breaks * (rh / breaks[length(breaks)])
    
    # For times beyond this sample's root height: NA (run doesn't inform that old)
    out <- rep(NA_real_, length(t_grid))
    ok  <- t_grid <= rh
    tg  <- t_grid[ok]
    
    idx <- findInterval(tg, breaks, rightmost.closed = TRUE)
    idx[idx == 0] <- 1
    idx[idx > n_int] <- n_int
    
    out[ok] <- ps[idx]
    out
  }
  
  # build matrix: rows=samples, cols=bins
  ne_mat <- vapply(seq_len(n_samp), map_one, FUN.VALUE = numeric(length(t_grid)))
  ne_mat <- t(ne_mat)
  
  # summarize across posterior samples at each time bin (ignore NAs)
  mean_v   <- apply(ne_mat, 2, function(x) mean(x, na.rm = TRUE))
  median_v <- apply(ne_mat, 2, function(x) median(x, na.rm = TRUE))
  lower_v  <- apply(ne_mat, 2, function(x) as.numeric(quantile(x, 0.025, na.rm = TRUE)))
  upper_v  <- apply(ne_mat, 2, function(x) as.numeric(quantile(x, 0.975, na.rm = TRUE)))
  
  out <- data.table(
    time   = t_grid,
    mean   = mean_v,
    median = median_v,
    lower  = lower_v,
    upper  = upper_v
  )
  
  # calendar mapping (approx like Tracer export)
  out[, calendar_year := youngest_tip - time]
  
  # optional date column (approx conversion of decimal year -> Date)
  year_floor <- floor(out$calendar_year)
  frac <- out$calendar_year - year_floor
  out[, date := as.Date(paste0(year_floor, "-01-01")) + round(frac * 365.25)]
  
  out
}

# ---------- Batch over all logs ----------
log_files <- list.files(log_dir, pattern = "\\.log$", full.names = TRUE)
stopifnot(length(log_files) > 0)

out_dir <- file.path(log_dir, "SKYLINE_TABLES_Median")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (f in log_files) {
  cat("Processing:", basename(f), "\n")
  tab <- make_skyline_table_one(f)
  
  out_name <- paste0(tools::file_path_sans_ext(basename(f)), "_skyline_table.tsv")
  out_path <- file.path(out_dir, out_name)
  
  fwrite(tab, out_path, sep = "\t")
}

cat("\nDone. Saved skyline tables to:\n", out_dir, "\n")
