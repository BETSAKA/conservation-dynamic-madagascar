# =============================================================================
# extract_pa_decree_years.R
#
# Extract a flat table of protected area decree years from the dynamic WDPA
# dataset. One row per PA with:
#   - WDPAID
#   - NAME
#   - temporary_decree_year: year the PA received temporary protection (NA if none)
#   - permanent_decree_year: year the PA received permanent designation (NA if none)
#
# Input:  data/dynamic_wdpa.rds (from conservation-dynamic-madagascar/09_consolidation_rules.qmd)
# Output: data/pa_decree_years.csv
# =============================================================================

library(tidyverse)
library(sf)

# --- Load dynamic WDPA ---
dw <- readRDS("data/dynamic_wdpa.rds")

# --- Build PA-level summary ---
# Keep only external_boundary states (one per PA per temporal period)
pa_decree_years <- dw |>
  st_drop_geometry() |>
  filter(zone_type == "external_boundary") |>
  mutate(
    WDPAID = as.integer(WDPAID),
    is_temporary = (DESIG == "Protection Temporaire"),
    state_year = year(valid_from)
  ) |>
  group_by(WDPAID) |>
  summarise(
    NAME = first(NAME),
    # Earliest year where DESIG == "Protection Temporaire"
    temporary_decree_year = if (any(is_temporary, na.rm = TRUE)) {
      min(state_year[is_temporary], na.rm = TRUE)
    } else {
      NA_integer_
    },
    # Earliest year where DESIG != "Protection Temporaire"
    permanent_decree_year = if (any(!is_temporary, na.rm = TRUE)) {
      min(state_year[!is_temporary], na.rm = TRUE)
    } else {
      NA_integer_
    },
    .groups = "drop"
  ) |>
  arrange(WDPAID)

# --- Quick check ---
n_total <- nrow(pa_decree_years)
n_temp <- sum(!is.na(pa_decree_years$temporary_decree_year))
n_perm <- sum(!is.na(pa_decree_years$permanent_decree_year))
n_both <- sum(
  !is.na(pa_decree_years$temporary_decree_year) &
    !is.na(pa_decree_years$permanent_decree_year)
)

cat("PA decree years summary:\n")
cat("  Total PAs:                    ", n_total, "\n")
cat("  With temporary decree year:   ", n_temp, "\n")
cat("  With permanent decree year:   ", n_perm, "\n")
cat("  With both:                    ", n_both, "\n")
cat("  Temporary only (still temp):  ", n_temp - n_both, "\n")
cat("  Permanent only (no temp phase):", n_perm - n_both, "\n")

# --- Export ---
write_csv(pa_decree_years, "data/pa_decree_years.csv")
cat("\nSaved to data/pa_decree_years.csv\n")
