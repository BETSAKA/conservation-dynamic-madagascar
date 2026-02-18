# app.R - Application Shiny pour visualisation des aires protégées consolidées

library(shiny)
library(dplyr)
library(sf)
library(ggplot2)
library(tmap)
library(arrow)
library(geoarrow)
library(DT)
library(htmltools)
library(stringr)
library(lubridate)

tmap_mode("view")

# ---- Data Loading ----

# Spatial consolidated dataset
data <- readRDS("data/all_PAs_conso.rds") %>%
  mutate(
    unique_id = ifelse(
      !is.na(WDPAID),
      paste0("WDPAID_", WDPAID),
      paste0(dataset_id, "_", NAME)
    )
  )

data_no_geom <- st_drop_geometry(data)

# Legal dataset (full CNLEGIS content)
legal <- readRDS("data/legal_texts.rds")

# WDPA names for display
wdpa_names <- data_no_geom %>%
  filter(dataset_id == "WDPA_2025") %>%
  distinct(WDPAID, .keep_all = TRUE) %>%
  select(WDPAID, NAME_WDPA = NAME)

# WDPAIDs used in analysis
wdpaid_analysis <- data_no_geom %>%
  filter(dataset_id == "WDPA_2025") %>%
  filter(STATUS == "Designated") %>%
  filter(STATUS_YR > 2000) %>%
  filter(MARINE %in% c("terrestrial", "partial")) %>%
  pull(WDPAID) %>%
  unique()

# Index table for selection
index_table <- data_no_geom %>%
  group_by(unique_id) %>%
  summarise(
    WDPAID = first(WDPAID[!is.na(WDPAID)]),
    NAME = {
      w <- first(WDPAID[!is.na(WDPAID)])
      if (!is.na(w)) {
        wdpa_names$NAME_WDPA[match(w, wdpa_names$WDPAID)]
      } else {
        first(NAME)
      }
    },
    in_WDPA = any(dataset_id == "WDPA_2025"),
    in_others = any(dataset_id != "WDPA_2025"),
    in_analysis = any(WDPAID %in% wdpaid_analysis),
    .groups = "drop"
  ) %>%
  mutate(
    category = case_when(
      in_WDPA & !in_others ~ "WDPA seulement",
      in_WDPA & in_others ~ "WDPA et d'autres",
      !in_WDPA & in_others ~ "Seulement d'autres",
      TRUE ~ "Non classé"
    )
  ) %>%
  arrange(NAME)

# WDPA historical
wdpa_hist <- read_parquet("data/MDG_WDPA_Consolidated.parquet") %>%
  mutate(geometry = st_as_sfc(geometry)) %>%
  st_as_sf() %>%
  mutate(dataset_id = paste0("WDPA_", data_year))

# ---- UI ----

ui <- fluidPage(
  titlePanel("Exploration des aires protégées consolidées"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "filter_category",
        "PA incluses dans:",
        choices = c(
          unique(index_table$category),
          "STATUS_YR influençant l'analyse"
        ),
        selected = "WDPA et d'autres"
      ),
      uiOutput("pa_selector")
    ),
    mainPanel(
      h4("Présence de l'AP dans les jeux de données"),
      plotOutput("presence_plot", height = "150px"),
      br(),
      tabsetPanel(
        tabPanel("Infos consolidées", DTOutput("pa_details_main")),
        tabPanel("CNLEGIS", DTOutput("pa_details_cnlegis")),
        tabPanel("Texte CNLEGIS", DTOutput("cnlegis_full_info")),
        tabPanel(
          "Carte",
          HTML(
            '<p>Pour masquer ou visualiser des couches, cliquer sur 
            <img src="layer_icon.png" height="25" style="vertical-align:middle;">.</p>'
          ),
          tmapOutput("pa_map", height = "500px")
        )
      )
    )
  )
)

# ---- Server ----

server <- function(input, output, session) {
  dataset_order <- c(
    "WDPA_2025",
    "SAPM_2024",
    "SAPM_2017",
    "SAPM_evol_2001-2011",
    "SAPM_2010",
    "MNP_2010",
    "ANGAP_2002",
    "CNLEGIS_2024"
  )

  filtered_table <- reactive({
    req(input$filter_category)
    if (input$filter_category == "STATUS_YR influençant l'analyse") {
      index_table %>% filter(in_analysis)
    } else {
      index_table %>% filter(category == input$filter_category)
    }
  })

  output$pa_selector <- renderUI({
    ft <- filtered_table()
    selectInput(
      "selected_pa",
      "Nom de l'AP",
      choices = setNames(ft$unique_id, ft$NAME)
    )
  })

  output$presence_plot <- renderPlot({
    req(input$selected_pa)

    selected_data <- data %>%
      filter(unique_id == input$selected_pa) %>%
      st_drop_geometry()

    presence_df <- tibble(dataset_id = dataset_order) %>%
      mutate(present = dataset_id %in% selected_data$dataset_id)

    ggplot(presence_df, aes(x = dataset_id, y = 1, color = present)) +
      geom_point(size = 6) +
      scale_color_manual(values = c("TRUE" = "black", "FALSE" = "lightgray")) +
      scale_x_discrete(limits = dataset_order) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })

  output$pa_details_main <- renderDT({
    req(input$selected_pa)
    data %>%
      filter(unique_id == input$selected_pa, dataset_id != "CNLEGIS_2024") %>%
      st_drop_geometry() %>%
      select(
        dataset_id,
        NAME,
        DESIG,
        IUCN_CAT,
        MANG_AUTH,
        STATUS_YR,
        WDPAID,
        WDPA_NAME,
        overlap_WPDA
      ) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE))
  })

  cnlegis_rows_conso <- reactive({
    req(input$selected_pa)
    data %>%
      filter(unique_id == input$selected_pa, dataset_id == "CNLEGIS_2024") %>%
      st_drop_geometry()
  })

  output$pa_details_cnlegis <- renderDT({
    df <- cnlegis_rows_conso()

    if (nrow(df) == 0) {
      return(datatable(
        tibble(message = "Aucune entrée CNLEGIS pour cette aire protégée."),
        options = list(dom = "t")
      ))
    }

    if (!"cnlegis_url" %in% names(df)) {
      df$cnlegis_url <- NA_character_
    }

    df_tbl <- df %>%
      mutate(
        lien = ifelse(
          is.na(cnlegis_url),
          NA_character_,
          as.character(tags$a(href = cnlegis_url, target = "_blank", "ouvrir"))
        )
      ) %>%
      select(
        WDPA_NAME,
        WDPAID,
        date_texte,
        type_texte,
        num_texte,
        type_decision,
        lien
      )

    datatable(
      df_tbl,
      escape = FALSE,
      selection = "single",
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$cnlegis_full_info <- renderDT({
    conso_df <- cnlegis_rows_conso()

    if (nrow(conso_df) == 0) {
      return(datatable(
        tibble(message = "Aucune entrée CNLEGIS pour cette aire protégée."),
        options = list(dom = "t")
      ))
    }

    i <- input$pa_details_cnlegis_rows_selected
    if (is.null(i) || length(i) == 0) {
      return(datatable(
        tibble(
          message = "Sélectionner un texte à afficher dans l'onglet CNLEGIS."
        ),
        options = list(dom = "t")
      ))
    }

    sel_wdpaid <- conso_df$WDPAID[i]
    sel_num <- conso_df$num_texte[i]

    if (is.na(sel_wdpaid) || is.na(sel_num) || sel_num == "") {
      return(datatable(
        tibble(
          message = "Entrée sélectionnée sans WDPAID ou num_texte utilisable."
        ),
        options = list(dom = "t")
      ))
    }

    if (!all(c("WDPAID", "num_texte") %in% names(legal))) {
      return(datatable(
        tibble(message = "legal_texts.rds doit contenir WDPAID et num_texte."),
        options = list(dom = "t")
      ))
    }

    legal_match <- legal %>%
      filter(WDPAID == sel_wdpaid, num_texte == sel_num)

    if (nrow(legal_match) == 0) {
      return(datatable(
        tibble(
          message = "Aucun contenu trouvé dans legal_texts.rds pour ce WDPAID et ce num_texte."
        ),
        options = list(dom = "t")
      ))
    }

    legal_match_tbl <- legal_match %>%
      mutate(across(
        where(~ !is.list(.x)),
        ~ if (inherits(.x, "Date")) as.character(.x) else as.character(.x)
      ))

    kv <- legal_match_tbl %>%
      mutate(row_id = row_number()) %>%
      tidyr::pivot_longer(
        cols = -row_id,
        names_to = "champ",
        values_to = "valeur"
      ) %>%
      mutate(
        valeur = vapply(
          valeur,
          function(v) {
            if (is.null(v) || (length(v) == 1 && is.na(v))) {
              return("")
            }
            if (is.list(v)) {
              return(paste(capture.output(str(v)), collapse = "\n"))
            }
            as.character(v)
          },
          character(1)
        )
      ) %>%
      filter(valeur != "") %>%
      select(champ, valeur)

    datatable(
      kv,
      rownames = FALSE,
      options = list(pageLength = 200, scrollX = TRUE)
    )
  })

  output$pa_map <- renderTmap({
    req(input$selected_pa)

    selected_sf <- data %>%
      filter(unique_id == input$selected_pa, dataset_id != "CNLEGIS_2024") %>%
      st_zm(drop = TRUE, what = "ZM") %>%
      st_make_valid() %>%
      st_cast("MULTIPOLYGON")

    wdpaid <- selected_sf %>% st_drop_geometry() %>% pull(WDPAID) %>% unique()
    wdpaid <- wdpaid[!is.na(wdpaid)]

    if (length(wdpaid) == 1) {
      hist_layers <- wdpa_hist %>%
        filter(WDPAID == wdpaid) %>%
        st_transform(st_crs(selected_sf)) %>%
        st_make_valid() %>%
        st_cast("MULTIPOLYGON")

      if (nrow(hist_layers) > 0) {
        selected_sf <- bind_rows(selected_sf, hist_layers)
      }
    }

    tmap_mode("view")
    map <- NULL

    if ("WDPA_2025" %in% selected_sf$dataset_id) {
      map <- selected_sf %>%
        filter(dataset_id == "WDPA_2025") %>%
        tm_shape() +
        tm_polygons(
          border.col = "red",
          col = "red",
          alpha = 0.2,
          id = "NAME",
          group = "WDPA_2025"
        )
    }

    other_datasets <- unique(selected_sf$dataset_id[
      selected_sf$dataset_id != "WDPA_2025"
    ])

    for (ds in other_datasets) {
      tmp <- selected_sf %>%
        filter(dataset_id == ds) %>%
        tm_shape() +
        tm_polygons(
          col = "blue",
          alpha = 0.2,
          id = "NAME",
          group = as.character(ds)
        )
      map <- if (is.null(map)) tmp else map + tmp
    }

    map
  })
}

shinyApp(ui, server)
