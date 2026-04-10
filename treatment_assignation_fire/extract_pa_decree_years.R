# extract_pa_decree_years.R
# ---------------------------------------------------------------------------
# Extract temporary and permanent decree years from the dynamic WDPA dataset
# produced by the consolidation pipeline (data/dynamic_wdpa.rds).
#
# Output: data/wdpaid_decree_years.csv with columns:
#   WDPAID, NAME, IUCN_CAT,
#   temporary_decree_year, permanent_decree_year,
#   first_valid_from, latest_valid_to
# ---------------------------------------------------------------------------

library(tidyverse)
library(sf)

# -- Paths (adjust if running from a different working directory) ------------
project_root <- here::here()
dynamic_wdpa_path <- file.path(project_root, "data", "dynamic_wdpa.rds")
output_path <- file.path(project_root, "data", "wdpaid_decree_years.csv")

# -- Load dynamic WDPA -------------------------------------------------------
dw <- readRDS(dynamic_wdpa_path) |>
  st_drop_geometry()

# -- Keep only external boundaries (one row per PA-state) --------------------
dw_ext <- dw |>
  filter(zone_type == "external_boundary")

# -- Classify each state as temporary or permanent ---------------------------
# "Protection Temporaire" is the only temporary designation type.
dw_ext <- dw_ext |>
  mutate(
    protection_type = if_else(
      DESIG == "Protection Temporaire", "temporary", "permanent",
      missing = "permanent"
    )
  )

# -- Helper: min that returns NA instead of Inf on empty input ---------------
safe_min <- function(x) if (length(x) == 0 || all(is.na(x))) NA_integer_ else min(x, na.rm = TRUE)

# -- Extract decree years per PA ---------------------------------------------
decree_years <- dw_ext |>
  group_by(WDPAID, NAME, IUCN_CAT) |>
  summarise(
    temporary_decree_year = safe_min(
      as.integer(STATUS_YR[protection_type == "temporary"])
    ),
    permanent_decree_year = safe_min(
      as.integer(STATUS_YR[protection_type == "permanent"])
    ),
    first_valid_from = min(valid_from, na.rm = TRUE),
    latest_valid_to = if (all(is.na(valid_to))) NA_Date_ else max(valid_to, na.rm = TRUE),
    .groups = "drop"
  )

# -- Write to CSV -------------------------------------------------------------
write_csv(decree_years, output_path)

cat(
  "Wrote", nrow(decree_years), "PAs to", output_path, "\n",
  "  - with temporary year:", sum(!is.na(decree_years$temporary_decree_year)), "\n",
  "  - with permanent year:", sum(!is.na(decree_years$permanent_decree_year)), "\n",
  "  - with both:          ",
  sum(
    !is.na(decree_years$temporary_decree_year) &
      !is.na(decree_years$permanent_decree_year)
  ), "\n"
)
