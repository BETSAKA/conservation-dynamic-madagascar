# Figure 2 ---------------------------------------------------------------

library(sf)
library(tmap)
library(dplyr)

# Load the temporal dataset produced by the CAR engine (Ch. 09)
dynamic_wdpa <- readRDS("data/dynamic_wdpa.rds")

# Select the three emblematic PAs
example_names <- c("Ankarafantsika", "Ambatovaky", "Kirindy Mite")

examples <- dynamic_wdpa |>
  filter(
    NAME %in% example_names,
    zone_type == "external_boundary"
  ) |>
  mutate(valid_from_label = as.character(valid_from))

tmap_mode("plot")

# One overlapping map per PA, faceted by NAME
fig_examples <- tm_shape(examples) +
  tm_polygons(
    fill = "valid_from_label",
    fill_alpha = 0.5,
    fill.scale = tm_scale_categorical(values = "brewer.set2"),
    fill.legend = tm_legend(title = "Boundary date"),
    col = "grey30",
    lwd = 0.6
  ) +
  tm_facets("NAME", free.coords = TRUE, ncol = 3) +
  tm_layout(frame = TRUE)

# Save as ../figures/fig_examples.png
tmap_save(fig_examples, "paper/figures/fig_examples.png")

# Figure 3 ---------------------------------------------------------------

library(sf)
library(tmap)
library(dplyr)

dynamic_wdpa <- readRDS("data/dynamic_wdpa.rds")

# Count states per PA (external boundaries only)
state_counts <- dynamic_wdpa |>
  st_drop_geometry() |>
  filter(zone_type == "external_boundary") |>
  count(WDPAID, NAME, name = "n_states")

# Get the most recent external boundary per PA for the map
current_boundaries <- dynamic_wdpa |>
  filter(zone_type == "external_boundary") |>
  group_by(WDPAID) |>
  slice_max(valid_from, n = 1) |>
  ungroup() |>
  left_join(state_counts |> select(WDPAID, n_states), by = "WDPAID")

tmap_mode("plot")

# Load Madagascar national boundary
mdg_border <- geodata::gadm("MDG", level = 0, path = "data/gadm") |> st_as_sf()

fig_map <- tm_shape(mdg_border) +
  tm_borders(col = "grey50", lwd = 0.5) +
  tm_shape(current_boundaries) +
  tm_polygons(
    fill = "n_states",
    fill.scale = tm_scale_intervals(
      breaks = c(1, 2, 3, 5, 10, Inf),
      values = "brewer.yl_or_rd",
      labels = c("1", "2", "3–4", "5–9", "10+")
    ),
    fill.legend = tm_legend(title = "Temporal states"),
    col = "grey40",
    lwd = 0.3
  ) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom")
  )

# save in paper/figures/map_temporal_states.png
tmap_save(fig_map, "paper/figures/fig_map.png")


# Figure 4 ---------------------------------------------------------------

library(tidyverse)
library(yaml)

# Read all YAML amendment files
yaml_files <- list.files(
  "data/amendments",
  pattern = "\\.yml$",
  full.names = TRUE
)

amendment_types <- map_dfr(yaml_files, function(f) {
  a <- read_yaml(f)
  tibble(amendment_type = a$amendment_type)
})

# Readable labels
type_labels <- c(
  boundary_modification = "Boundary\nmodification",
  status_change = "Status\nchange",
  correction = "Correction",
  secondary_zoning = "Secondary\nzoning",
  temporary_protection = "Temporary\nprotection"
)

amendment_summary <- amendment_types |>
  count(amendment_type) |>
  mutate(label = type_labels[amendment_type] %||% amendment_type)

fig_amendments <- ggplot(
  amendment_summary,
  aes(x = reorder(label, -n), y = n)
) +
  geom_col(fill = "#2c7fb8", width = 0.6) +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +
  labs(x = NULL, y = "Number of amendments") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank())

# Save in paper/figures/fig_amendments.png
ggsave(
  "paper/figures/fig_amendments.png",
  fig_amendments,
  width = 7,
  height = 4,
  dpi = 300
)
