library(sf)
library(tmap)
library(dplyr)

# Load the temporal dataset produced by the CAR engine (Ch. 09)
dynamic_wdpa <- readRDS("data/dynamic_wdpa.rds") |>
  filter(
    STATUS == "Designated",
    !DESIG %in%
      c(
        "UNESCO-MAB Biosphere Reserve",
        "Ramsar Site, Wetland of International Importance",
        "World Heritage Site (natural or mixed)"
      )
  )

# Select the three emblematic PAs
example_names <- c("Ankarafantsika", "Ambatovaky", "Kirindy Mite")

examples <- dynamic_wdpa |>
  filter(
    NAME %in% example_names,
    zone_type == "external_boundary"
  ) |>
  mutate(valid_from_label = as.character(valid_from))

tmap_mode("plot")

# One map per PA timeline, arranged in rows
create_pa_map <- function(pa_name) {
  pa_data <- examples |> filter(NAME == pa_name)
  tm_shape(pa_data) +
    tm_polygons(
      fill = "valid_from_label",
      fill.scale = tm_scale_categorical(values = "brewer.set2"),
      fill.legend = tm_legend_hide(),
      col = "grey30",
      lwd = 0.6
    ) +
    tm_facets("valid_from_label", free.coords = FALSE, nrow = 1)
}

create_label_map <- function(pa_name) {
  null_geom <- st_sfc(st_point(c(0, 0))) |> st_sf(name = pa_name)
  tm_shape(null_geom) +
    tm_text("name", angle = 90, size = 1.5, fontface = "bold") +
    tm_layout(frame = FALSE, outer.margins = 0)
}

maps_col <- tmap_arrange(
  create_pa_map("Ankarafantsika"),
  create_pa_map("Ambatovaky"),
  create_pa_map("Kirindy Mite"),
  ncol = 1
)

text_col <- tmap_arrange(
  create_label_map("Ankarafantsika"),
  create_label_map("Ambatovaky"),
  create_label_map("Kirindy Mite"),
  ncol = 1
)

fig_examples <- tmap_arrange(
  c(rbind(text_col, maps_col)),
  ncol = 2,
  widths = c(0.08, 1)
)

tmap_save(
  fig_examples,
  "paper/figures/fig_examples.png",
  width = 8,
  height = 7,
  dpi = 300
)
