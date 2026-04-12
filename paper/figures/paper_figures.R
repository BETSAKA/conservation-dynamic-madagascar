library(sf)
library(tmap)
library(dplyr)

# Load the temporal dataset produced by the CAR engine (Ch. 09)
dynamic_wdpa <- readRDS("data/dynamic_wdpa.rds")

# Select the three emblematic PAs
example_names <- c("Ankarafantsika", "Ambatovaky", "Kirindy Mite")

examples <- dynamic_wdpa |>
  filter(
    grepl(paste(example_names, collapse = "|"), NAME, ignore.case = TRUE),
    zone_type == "external_boundary"
  )

tmap_mode("plot")

# Create one panel per PA, faceted by valid_from
tm_shape(examples) +
  tm_polygons(
    fill = "valid_from",
    fill.scale = tm_scale_continuous(values = "brewer.blues"),
    fill_alpha = 0.5,
    col = "darkblue",
    lwd = 0.8
  ) +
  tm_facets(by = c("NAME", "valid_from"), free.coords = TRUE, ncol = 3) +
  tm_layout(
    panel.labels = NULL,
    frame = TRUE
  )
