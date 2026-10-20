# fig_cumulative_area_comparison.R
#
# Cumulative national protected-area area in Madagascar, 2000-2025:
#   1. WDPA November 2025 baseline projected backward through STATUS_YR
#   2. temporally extended dataset (data/dynamic_wdpa.rds)
#
# Implements the rules in add-cumsum-fig.md with two agreed adjustments:
#   - Series 2 counts each PA once per year, using the state in force on
#     Dec 31. The literal overlap rule (valid_to > Jan 1) double-counts
#     handoff years because every handoff is exact
#     (valid_to == next valid_from), e.g. +41,660 km2 in 2015 alone.
#   - Records with unreported STATUS_YR (= 0) are excluded from Series 1,
#     matching the status_yr_to_date convention of the pipeline and the
#     dynamic series, which can never activate them.
#
# Run from the project root. Generates the figure and the yearly table only:
# no manuscript edits, no commits, no changes to the underlying dataset.

library(tidyverse)
library(sf)
library(lubridate)

years <- 2000:2025

stopifnot(
  "WDPA baseline missing: data/WDPA_WDOECM_Nov2025_Public_MDG.zip" = file.exists(
    "data/WDPA_WDOECM_Nov2025_Public_MDG.zip"
  ),
  "Dynamic dataset missing: data/dynamic_wdpa.rds" = file.exists(
    "data/dynamic_wdpa.rds"
  )
)

# ---- Series 1: WDPA baseline comparator -------------------------------------

wdpa_mdg <- wdpar::wdpa_read("data/WDPA_WDOECM_Nov2025_Public_MDG.zip")

n_wdpa_national <- sum(wdpa_mdg$DESIG_TYPE == "National")

wdpa_records <- wdpa_mdg |>
  filter(DESIG_TYPE == "National") |>
  st_make_valid() |>
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) |>
  st_drop_geometry() |>
  # STATUS_YR = 0 flags an unreported designation year, not the year 0
  filter(STATUS_YR > 0)

n_wdpa_used <- nrow(wdpa_records)

series_wdpa <- tibble(
  year = years,
  area_wdpa_km2 = vapply(
    years,
    function(y) {
      sum(wdpa_records$area_km2[wdpa_records$STATUS_YR <= y])
    },
    numeric(1)
  )
)

# ---- Series 2: temporally extended dataset ----------------------------------

dyn <- readRDS("data/dynamic_wdpa.rds")

dyn_states <- dyn |>
  filter(zone_type == "external_boundary", DESIG_TYPE == "National") |>
  st_make_valid() |>
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) |>
  st_drop_geometry()

series_dyn <- tibble(
  year = years,
  area_dynamic_km2 = vapply(
    years,
    function(y) {
      end_y <- as.Date(paste0(y, "-12-31"))
      active <- dyn_states |>
        filter(valid_from <= end_y, is.na(valid_to) | valid_to > end_y)
      # Non-overlapping intervals imply at most one state per PA per year
      stopifnot(
        "several active states for one WDPAID in year" = !any(duplicated(
          active$WDPAID
        ))
      )
      sum(active$area_km2)
    },
    numeric(1)
  )
)

yearly <- left_join(series_wdpa, series_dyn, by = "year")

# ---- Validation -------------------------------------------------------------

cat("=== Validation ===\n")
cat("WDPA national records in Nov 2025 baseline: ", n_wdpa_national, "\n")
cat(
  "WDPA records used in the comparator:        ",
  n_wdpa_used,
  " (",
  n_wdpa_national - n_wdpa_used,
  " excluded: unreported STATUS_YR = 0 )\n",
  sep = ""
)

dyn_used <- dyn_states |>
  filter(
    valid_from <= as.Date("2025-12-31"),
    is.na(valid_to) | valid_to > as.Date("2000-01-01")
  )
cat(
  "Distinct WDPAIDs used in the dynamic series:",
  n_distinct(dyn_used$WDPAID),
  "\n"
)

missing_years <- years[!years %in% yearly$year]
cat(
  "Missing years: ",
  if (length(missing_years) == 0) {
    "none"
  } else {
    paste(missing_years, collapse = ", ")
  },
  "\n",
  sep = ""
)

diffs <- yearly |>
  mutate(diff_km2 = area_dynamic_km2 - area_wdpa_km2)

cat(
  "\nDynamic differs from WDPA baseline in ",
  sum(diffs$diff_km2 != 0),
  " of ",
  nrow(diffs),
  " years; ",
  "max |difference| = ",
  round(max(abs(diffs$diff_km2)), 1),
  " km2\n",
  sep = ""
)
cat("\nYears with the largest absolute differences:\n")
print(
  diffs |>
    arrange(desc(abs(diff_km2))) |>
    slice_head(n = 5) |>
    select(year, area_wdpa_km2, area_dynamic_km2, diff_km2)
)

cat("\nNotes:\n")
cat(
  "- No STATUS filter applied (Designated, Proposed and Inscribed all kept), ",
  "as specified.\n",
  sep = ""
)
cat(
  "- Dynamic-only WDPAIDs (historical states, absent from the current ",
  "baseline): ",
  paste(
    setdiff(unique(dyn_used$WDPAID), as.character(unique(wdpa_records$WDPAID))),
    collapse = ", "
  ),
  "\n",
  sep = ""
)
cat(
  "- One dynamic state excluded by DESIG_TYPE = NA (WDPAID 5025, ",
  "international designation amended to NA).\n",
  sep = ""
)

# ---- Yearly table -----------------------------------------------------------

cat("\n=== Yearly values (head) ===\n")
print(head(yearly))
cat("\n=== Yearly values (tail) ===\n")
print(tail(yearly))

write_csv(
  yearly,
  "paper/drafts/figures/fig_cumulative_area_comparison_yearly.csv"
)

# ---- Figure -----------------------------------------------------------------

plot_df <- yearly |>
  pivot_longer(
    c(area_wdpa_km2, area_dynamic_km2),
    names_to = "series",
    values_to = "area_km2"
  ) |>
  mutate(
    series = recode(
      series,
      area_wdpa_km2 = "WDPA baseline (Nov 2025, STATUS_YR)",
      area_dynamic_km2 = "Temporally extended dataset"
    )
  )

p <- ggplot(plot_df, aes(x = year, y = area_km2, colour = series)) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  labs(
    title = "Cumulative national protected area in Madagascar, 2000-2025",
    x = "Year",
    y = expression("Cumulative area" ~ (km^2)),
    colour = NULL
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "paper/drafts/figures/fig_cumulative_area_comparison.png",
  p,
  width = 8,
  height = 5,
  dpi = 300
)

print(p)
