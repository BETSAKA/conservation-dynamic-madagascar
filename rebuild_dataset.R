# rebuild_dataset.R
#
# Reproduce the published Madagascar temporal protected-area dataset.
#
# Run from the repository root:
#   Rscript rebuild_dataset.R

wdpa_path <- "data/WDPA_WDOECM_Nov2025_Public_MDG.zip"
qmd_path <- "09_consolidation_rules.qmd"
reference_path <- "data/dynamic_wdpa.rds"

required_files <- c(wdpa_path, qmd_path, reference_path)
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required file(s): ",
    paste(missing_files, collapse = ", ")
  )
}

required_packages <- c(
  "tidyverse",
  "sf",
  "yaml",
  "wdpar",
  "lubridate",
  "arrow"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing R package(s): ",
    paste(missing_packages, collapse = ", ")
  )
}

quarto <- Sys.which("quarto")

if (!nzchar(quarto)) {
  stop("Quarto was not found on PATH.")
}

# Store the published structure before rebuilding
reference <- readRDS(reference_path)

make_keys <- function(x) {
  x |>
    sf::st_drop_geometry() |>
    dplyr::transmute(
      state_id = as.character(state_id),
      WDPAID = as.character(WDPAID),
      valid_from = as.character(valid_from),
      valid_to = as.character(valid_to),
      zone_type = as.character(zone_type),
      zone_name = dplyr::coalesce(as.character(zone_name), "<NA>")
    ) |>
    dplyr::arrange(
      WDPAID,
      state_id,
      zone_type,
      zone_name,
      valid_from,
      valid_to
    )
}

reference_keys <- make_keys(reference)

outputs <- c(
  "data/dynamic_wdpa.rds",
  "data/dynamic_wdpa.gpkg",
  "data/dynamic_wdpa.parquet"
)

# The published RDS has already been loaded into memory as the
# reference dataset. Remove all outputs so successful reproduction
# requires the consolidation workflow to recreate every format.
existing_outputs <- outputs[file.exists(outputs)]

if (length(existing_outputs) > 0) {
  ok <- file.remove(existing_outputs)

  if (!all(ok)) {
    stop(
      "Could not remove existing output file(s): ",
      paste(existing_outputs[!ok], collapse = ", ")
    )
  }
}


status <- system2(
  quarto,
  c("render", qmd_path)
)

if (status != 0) {
  stop("Dataset reconstruction failed.")
}

expected_outputs <- c(
  "data/dynamic_wdpa.rds",
  "data/dynamic_wdpa.gpkg",
  "data/dynamic_wdpa.parquet"
)

missing_outputs <- expected_outputs[!file.exists(expected_outputs)]

if (length(missing_outputs) > 0) {
  stop(
    "Expected output(s) missing: ",
    paste(missing_outputs, collapse = ", ")
  )
}

rebuilt <- readRDS("data/dynamic_wdpa.rds")
rebuilt_keys <- make_keys(rebuilt)

if (!identical(reference_keys, rebuilt_keys)) {
  stop(
    "Rebuilt dataset does not reproduce the published temporal-state structure."
  )
}

message(
  "Reconstruction successful: ",
  nrow(rebuilt),
  " rows, ",
  dplyr::n_distinct(rebuilt$state_id),
  " temporal states, ",
  dplyr::n_distinct(rebuilt$WDPAID),
  " WDPAIDs."
)
