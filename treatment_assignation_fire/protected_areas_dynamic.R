# =============================================================================
# protected_areas_dynamic.R
#
# Replacement for the "## Protected areas" section of
# 02a_indicator_computation_cell.qmd.
#
# Uses data/dynamic_wdpa.rds (temporal states with per-period geometries) to
# compute pixel-level treatment variables that reflect the ACTUAL date of
# temporary and permanent protection start for each grid cell.
#
# Key improvements over the original approach:
#   - Distinguishes temporary vs permanent protection at the cell level
#   - Per-state geometries: boundary modifications are honoured, so cells in
#     an expansion area get the expansion date, not the PA's founding date
#   - IUCN category taken from the PA's current (latest) state
#
# Variables produced (for each distance: touch, include, 5km, 10km, 20km):
#   WDPAID_{d}       : WDPAID of the earliest PA covering the cell
#   IUCN_{d}         : IUCN category (integer code) of that PA (current state)
#   year_temp_{d}    : year the cell first received temporary protection
#   year_perm_{d}    : year the cell first received permanent protection
#   year_any_{d}     : year the cell first received any form of protection
#
# Input:  data/dynamic_wdpa.rds, ./data/source/modis_ref_grid.rds
# Output: written to parquet dataset on S3 (same layout as other variables)
# =============================================================================

library(tidyverse)
library(sf)
library(terra)
library(arrow)
library(pbapply)
library(lubridate)

s3_output <- "PA_impacts_fires/data/output/fire-db.parquet"

# --- 1. Load reference grid ---
grid_sf <- readRDS("./data/source/modis_ref_grid.rds") |>
  st_as_sf()

# --- 2. Load dynamic WDPA and prepare PA states ---
dw <- readRDS("data/dynamic_wdpa.rds")

# Keep external boundaries, classify as temporary/permanent, compute IUCN code
pa_states <- dw |>
  filter(zone_type == "external_boundary") |>
  mutate(
    WDPAID = as.integer(WDPAID),
    is_temporary = (DESIG == "Protection Temporaire"),
    protection_year = year(valid_from),
    IUCN = recode(
      IUCN_CAT,
      "Ia" = 1L,
      "Ib" = 2L,
      "II" = 3L,
      "III" = 4L,
      "IV" = 5L,
      "V" = 6L,
      "VI" = 7L,
      "Not Reported" = NA_integer_,
      "Not Applicable" = NA_integer_,
      .default = NA_integer_
    )
  ) |>
  st_make_valid() |>
  arrange(valid_from) # sort by date so earliest states come first

# Current IUCN for each PA (from latest state)
pa_current_iucn <- pa_states |>
  st_drop_geometry() |>
  group_by(WDPAID) |>
  filter(valid_from == max(valid_from)) |>
  slice(1) |>
  ungroup() |>
  select(WDPAID, IUCN_current = IUCN)

# --- 3. Helper: build cell-level treatment from spatial intersection results ---
#
# Given an st_intersects result (grid vs pa_states), produce a tibble with
# one row per cell containing the earliest treatment dates and PA identity.

build_treatment <- function(spatial_result, pa_states_df, pa_iucn_df, suffix) {
  # Expand sparse matrix to long format: cell_idx -> state_idx
  cell_state <- tibble(
    cell_idx = rep(seq_along(spatial_result), lengths(spatial_result)),
    state_idx = unlist(spatial_result)
  )

  if (nrow(cell_state) == 0) {
    # No intersections at all (unlikely but defensive)
    return(tibble(
      cell_id = integer(),
      !!paste0("WDPAID_", suffix) := integer(),
      !!paste0("IUCN_", suffix) := integer(),
      !!paste0("year_temp_", suffix) := integer(),
      !!paste0("year_perm_", suffix) := integer(),
      !!paste0("year_any_", suffix) := integer()
    ))
  }

  cell_state <- cell_state |>
    mutate(
      cell_id = grid_sf$cell_id[cell_idx],
      WDPAID = pa_states_df$WDPAID[state_idx],
      is_temporary = pa_states_df$is_temporary[state_idx],
      protection_year = pa_states_df$protection_year[state_idx],
      valid_from = pa_states_df$valid_from[state_idx]
    )

  # For each cell, compute first PA (by date) and first temp/perm years
  cell_summary <- cell_state |>
    group_by(cell_id) |>
    arrange(valid_from, .by_group = TRUE) |>
    summarise(
      WDPAID_first = first(WDPAID),
      year_temp = {
        yrs <- protection_year[is_temporary]
        if (length(yrs) > 0) min(yrs) else NA_integer_
      },
      year_perm = {
        yrs <- protection_year[!is_temporary]
        if (length(yrs) > 0) min(yrs) else NA_integer_
      },
      .groups = "drop"
    ) |>
    mutate(year_any = pmin(year_temp, year_perm, na.rm = TRUE)) |>
    # Join current IUCN (from the first PA that touches the cell)
    left_join(pa_iucn_df, by = c("WDPAID_first" = "WDPAID"))

  # Rename columns with the distance suffix
  cell_summary |>
    transmute(
      cell_id,
      !!paste0("WDPAID_", suffix) := WDPAID_first,
      !!paste0("IUCN_", suffix) := IUCN_current,
      !!paste0("year_temp_", suffix) := as.integer(year_temp),
      !!paste0("year_perm_", suffix) := as.integer(year_perm),
      !!paste0("year_any_", suffix) := as.integer(year_any)
    )
}

# --- 4. Spatial intersections using per-state geometries ---
# This ensures that cells in expansion areas get the correct (later) date.

cat("Computing spatial intersections (touch)...\n")
ints_touch <- st_intersects(grid_sf, pa_states)
treat_touch <- build_treatment(ints_touch, pa_states, pa_current_iucn, "touch")

cat("Computing spatial intersections (include)...\n")
ints_include <- st_within(grid_sf, pa_states)
treat_include <- build_treatment(
  ints_include,
  pa_states,
  pa_current_iucn,
  "include"
)

# For buffer distances, use the latest geometry per PA to avoid buffering
# every temporal state (more efficient, boundary refinements are minor at
# 5-20 km scale).
pa_latest <- pa_states |>
  group_by(WDPAID) |>
  filter(valid_from == max(valid_from)) |>
  slice(1) |>
  ungroup()

# Build a PA-level summary for buffer-based variables
pa_level <- pa_latest |>
  st_drop_geometry() |>
  group_by(WDPAID) |>
  summarise(
    is_temporary = first(is_temporary),
    protection_year = first(protection_year),
    valid_from = first(valid_from),
    IUCN = first(IUCN),
    .groups = "drop"
  )

# Re-join geometry from pa_latest
pa_for_buffer <- pa_latest |>
  select(WDPAID) |>
  left_join(pa_level, by = "WDPAID") |>
  arrange(valid_from)

# Also compute PA-level earliest temp/perm years (for buffer approach)
pa_years <- pa_states |>
  st_drop_geometry() |>
  group_by(WDPAID) |>
  summarise(
    year_temp_pa = {
      yrs <- protection_year[is_temporary]
      if (length(yrs) > 0) min(yrs) else NA_integer_
    },
    year_perm_pa = {
      yrs <- protection_year[!is_temporary]
      if (length(yrs) > 0) min(yrs) else NA_integer_
    },
    .groups = "drop"
  ) |>
  mutate(year_any_pa = pmin(year_temp_pa, year_perm_pa, na.rm = TRUE))

# Helper for buffer-based treatment (PA-level approach)
build_treatment_buffer <- function(
  spatial_result,
  pa_df,
  pa_years_df,
  pa_iucn_df,
  suffix
) {
  cell_pa <- tibble(
    cell_idx = rep(seq_along(spatial_result), lengths(spatial_result)),
    pa_idx = unlist(spatial_result)
  )

  if (nrow(cell_pa) == 0) {
    return(tibble(
      cell_id = integer(),
      !!paste0("WDPAID_", suffix) := integer(),
      !!paste0("IUCN_", suffix) := integer(),
      !!paste0("year_temp_", suffix) := integer(),
      !!paste0("year_perm_", suffix) := integer(),
      !!paste0("year_any_", suffix) := integer()
    ))
  }

  cell_pa <- cell_pa |>
    mutate(
      cell_id = grid_sf$cell_id[cell_idx],
      WDPAID = pa_df$WDPAID[pa_idx],
      valid_from = pa_df$valid_from[pa_idx]
    ) |>
    left_join(pa_years_df, by = "WDPAID")

  cell_pa |>
    group_by(cell_id) |>
    arrange(valid_from, .by_group = TRUE) |>
    summarise(
      WDPAID_first = first(WDPAID),
      year_temp = min(year_temp_pa, na.rm = TRUE),
      year_perm = min(year_perm_pa, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      year_temp = if_else(
        is.infinite(year_temp),
        NA_integer_,
        as.integer(year_temp)
      ),
      year_perm = if_else(
        is.infinite(year_perm),
        NA_integer_,
        as.integer(year_perm)
      ),
      year_any = pmin(year_temp, year_perm, na.rm = TRUE)
    ) |>
    left_join(pa_iucn_df, by = c("WDPAID_first" = "WDPAID")) |>
    transmute(
      cell_id,
      !!paste0("WDPAID_", suffix) := WDPAID_first,
      !!paste0("IUCN_", suffix) := IUCN_current,
      !!paste0("year_temp_", suffix) := year_temp,
      !!paste0("year_perm_", suffix) := year_perm,
      !!paste0("year_any_", suffix) := year_any
    )
}

for (buf_dist in c(5000, 10000, 20000)) {
  label <- paste0(buf_dist / 1000, "km")
  cat("Computing spatial intersections (", label, ")...\n")
  pa_buffered <- st_buffer(pa_for_buffer, buf_dist)
  ints_buf <- st_intersects(grid_sf, pa_buffered)
  assign(
    paste0("treat_", label),
    build_treatment_buffer(
      ints_buf,
      pa_for_buffer,
      pa_years,
      pa_current_iucn,
      label
    )
  )
  rm(pa_buffered, ints_buf)
}

# --- 5. Assemble final treatment table ---
grid_base <- grid_sf |>
  st_drop_geometry() |>
  select(cell_id, x, y)

treatment_status <- grid_base |>
  left_join(treat_touch, by = "cell_id") |>
  left_join(treat_include, by = "cell_id") |>
  left_join(treat_5km, by = "cell_id") |>
  left_join(treat_10km, by = "cell_id") |>
  left_join(treat_20km, by = "cell_id")

# --- 6. Pivot to long format and write to parquet ---
grid_treat <- treatment_status |>
  pivot_longer(
    cols = -c(cell_id, x, y),
    names_to = "variable",
    values_to = "value"
  ) |>
  mutate(
    year = 2025L,
    date = as.Date("2025-07-29")
  )

write_dataset(
  grid_treat,
  path = paste0("s3://projet-betsaka/", s3_output),
  format = "parquet",
  partitioning = c("variable", "year"),
  existing_data_behavior = "overwrite"
)

# --- 7. Metadata ---
metadata <- readRDS("data/output/metadata.rds")

distances <- c("touch", "include", "5km", "10km", "20km")
dist_labels <- c(
  "touching the cell",
  "strictly including the cell",
  "within 5 km of the cell",
  "within 10 km of the cell",
  "within 20 km of the cell"
)

for (i in seq_along(distances)) {
  d <- distances[i]
  dl <- dist_labels[i]
  src_geo <- if (d %in% c("touch", "include")) {
    "Dynamic WDPA (per-state geometries)"
  } else {
    "Dynamic WDPA (latest geometry, buffered)"
  }

  metadata[[paste0("WDPAID_", d)]] <- list(
    Description = paste0("WDPAID of the earliest PA ", dl),
    source = src_geo
  )
  metadata[[paste0("IUCN_", d)]] <- list(
    Description = paste0("IUCN category (current) of the earliest PA ", dl),
    source = src_geo,
    codebook = data.frame(
      code = 1:7,
      label = c("Ia", "Ib", "II", "III", "IV", "V", "VI")
    )
  )
  metadata[[paste0("year_temp_", d)]] <- list(
    Description = paste0(
      "Year the cell first received temporary protection (Protection ",
      "Temporaire) from a PA ",
      dl
    ),
    source = src_geo
  )
  metadata[[paste0("year_perm_", d)]] <- list(
    Description = paste0(
      "Year the cell first received permanent protection (non-temporary ",
      "designation) from a PA ",
      dl
    ),
    source = src_geo
  )
  metadata[[paste0("year_any_", d)]] <- list(
    Description = paste0(
      "Year the cell first received any form of protection (temporary ",
      "or permanent) from a PA ",
      dl
    ),
    source = src_geo
  )
}

saveRDS(metadata, "data/output/metadata.rds")
put_to_s3(
  from = "data/output/metadata.rds",
  to = "PA_impacts_fires/data/output/metadata.rds"
)

cat("\nDone. Treatment variables written to parquet.\n")
