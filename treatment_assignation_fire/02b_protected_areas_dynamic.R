# 02b_protected_areas_dynamic.R
# ---------------------------------------------------------------------------
# Build treatment variables from the dynamic WDPA (data/dynamic_wdpa.rds),
# which captures temporal changes in PA designation status.
#
# This script replaces the static WDPA section of 02a by using the dynamic
# temporal states. For each year from 2000 to 2024 and each grid cell, it
# identifies which PA (if any) covers the cell AND was active that year.
#
# The output follows the same long-format parquet convention as 02a:
#   cell_id, x, y, date, variable, year, value
#
# Variables produced (for intersection levels: touch, include):
#   WDPAID_{level}_dyn      : WDPAID of the active PA
#   IUCN_{level}_dyn        : IUCN code of the active PA
#   desig_type_{level}_dyn  : 1 = permanent, 2 = temporary protection
#   year_created_{level}_dyn: Year of the PA state's valid_from
#
# Requires: sf, tidyverse, arrow, and the MODIS reference grid (via S3 or
# local path). Designed to run on SSP Cloud.
# ---------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(arrow)

# ---------------------------------------------------------------------------
# 0. Configuration
# ---------------------------------------------------------------------------

# -- Paths
project_root <- here::here()
dynamic_wdpa_path <- file.path(project_root, "data", "dynamic_wdpa.rds")

# S3 output path (same convention as 02a)
s3_output <- "PA_impacts_fires/data/output/fire-db.parquet"

# If running on SSP Cloud, the grid is already local:
grid_path <- "./data/source/modis_ref_grid.rds"

# ---------------------------------------------------------------------------
# 1. Load data
# ---------------------------------------------------------------------------

cat("Loading dynamic WDPA...\n")
dw <- readRDS(dynamic_wdpa_path)

# Keep only external boundaries (one geometry per PA-state)
dw_ext <- dw |>
  filter(zone_type == "external_boundary") |>
  mutate(
    WDPAID_int = as.integer(WDPAID),
    STATUS_YR_int = as.integer(STATUS_YR),
    IUCN = case_match(
      IUCN_CAT,
      "Ia" ~ 1L, "Ib" ~ 2L, "II" ~ 3L,
      "III" ~ 4L, "IV" ~ 5L, "V" ~ 6L, "VI" ~ 7L,
      .default = NA_integer_
    ),
    is_temporary = as.integer(DESIG == "Protection Temporaire"),
    year_from = as.integer(format(valid_from, "%Y")),
    year_to = if_else(
      is.na(valid_to),
      2025L,
      as.integer(format(valid_to, "%Y"))
    )
  ) |>
  st_make_valid()

cat("  ", n_distinct(dw_ext$WDPAID), "PAs,", nrow(dw_ext), "temporal states\n")

# ---------------------------------------------------------------------------
# 2. Load reference grid
# ---------------------------------------------------------------------------
cat("Loading reference grid...\n")

if (file.exists(grid_path)) {
  grid_sf <- readRDS(grid_path) |> st_as_sf()
} else {
  stop(
    "Reference grid not found at ", grid_path, "\n",
    "Please ensure the MODIS reference grid is available."
  )
}

cat("  Grid:", nrow(grid_sf), "cells\n")

# ---------------------------------------------------------------------------
# 3. Compute spatial intersections (once, then filter temporally)
# ---------------------------------------------------------------------------
# Instead of computing intersections per year, we compute them once against the
# union of all states, then filter by year.

cat("Computing spatial intersections...\n")

# For each PA-state, we need: WDPAID, IUCN, is_temporary, year_from, year_to
# We intersect grid cells with all PA-state geometries.

# touch = st_intersects, include = st_within
intersects_idx <- st_intersects(grid_sf, dw_ext)
within_idx <- st_within(grid_sf, dw_ext)

cat("  Intersections computed.\n")

# ---------------------------------------------------------------------------
# 4. Build treatment variables year by year
# ---------------------------------------------------------------------------

# For a given year, a PA-state is "active" if year_from <= year < year_to
# Among active PAs covering a cell, keep the one with the earliest year_from
# (oldest PA first, as in the static approach).

build_year <- function(year, intersects_idx, within_idx, dw_ext, grid_sf) {

  # Which PA-states are active this year?
  active <- which(dw_ext$year_from <= year & dw_ext$year_to > year)
  if (length(active) == 0) return(NULL)

  # Pre-sort active states by year_from for "first_or_na" logic
  active_sorted <- active[order(dw_ext$year_from[active])]

  # Helper: for each cell, find the first active PA-state index
  first_active <- function(idx_list) {
    vapply(idx_list, function(hits) {
      matched <- intersect(hits, active_sorted)
      if (length(matched) == 0L) return(NA_integer_)
      # Return the one that appears first in active_sorted
      active_sorted[which(active_sorted %in% matched)[1]]
    }, integer(1))
  }

  touch_idx <- first_active(intersects_idx)
  include_idx <- first_active(within_idx)

  tibble(
    cell_id = grid_sf$cell_id,
    x = grid_sf$x,
    y = grid_sf$y,
    year = as.integer(year),
    date = as.Date(paste0(year, "-01-01")),
    # touch-level
    WDPAID_touch_dyn = dw_ext$WDPAID_int[touch_idx],
    IUCN_touch_dyn = dw_ext$IUCN[touch_idx],
    desig_type_touch_dyn = if_else(
      dw_ext$is_temporary[touch_idx] == 1L, 2L, 1L
    ),
    year_created_touch_dyn = dw_ext$year_from[touch_idx],
    # include-level
    WDPAID_include_dyn = dw_ext$WDPAID_int[include_idx],
    IUCN_include_dyn = dw_ext$IUCN[include_idx],
    desig_type_include_dyn = if_else(
      dw_ext$is_temporary[include_idx] == 1L, 2L, 1L
    ),
    year_created_include_dyn = dw_ext$year_from[include_idx]
  )
}

cat("Building treatment variables for 2000-2024...\n")

treatment_all <- map(
  2000:2024,
  function(yr) {
    cat("  Year", yr, "...")
    res <- build_year(yr, intersects_idx, within_idx, dw_ext, grid_sf)
    cat(" done\n")
    res
  },
  .progress = FALSE
) |>
  bind_rows()

# ---------------------------------------------------------------------------
# 5. Pivot to long format and export
# ---------------------------------------------------------------------------
cat("Pivoting to long format...\n")

treatment_long <- treatment_all |>
  pivot_longer(
    cols = c(
      starts_with("WDPAID_"), starts_with("IUCN_"),
      starts_with("desig_type_"), starts_with("year_created_")
    ),
    names_to = "variable",
    values_to = "value"
  ) |>
  select(cell_id, x, y, date, variable, year, value)

cat("  Total rows:", nrow(treatment_long), "\n")

# Export to parquet (partitioned by variable and year)
output_path <- paste0("s3://projet-betsaka/", s3_output)
cat("Writing to", output_path, "...\n")

arrow::write_dataset(
  treatment_long,
  path = output_path,
  format = "parquet",
  partitioning = c("variable", "year"),
  existing_data_behavior = "overwrite"
)

cat("Done.\n")
