
# On commence par SAPM 2017
pa_names <- sapm_2017 %>%
  st_drop_geometry() %>%
  select(short_name_sapm_2017 = SHORT_NAME, full_name_sapm_2017 = FULL_NAME) %>%
  mutate(short_name_sapm_2017 = str_remove(short_name_sapm_2017, "\\n"),
         full_name_sapm_2017 = str_remove(full_name_sapm_2017, "\\n"))

# Handling duplicates
## There are 2 PAs under the name "Analalava". Need to rename the 2nd
pa_names <- pa_names %>%
  mutate(short_name_sapm_2017 = ifelse(
    full_name_sapm_2017 == "Réserve de Ressources Naturelles d\'Analalava", 
    "RRN Analalava", short_name_sapm_2017))
wdpa_mdg_all <- wdpa_mdg_all %>%
  mutate(NAME = ifelse(WDPAID == 555697889, "RRN Analalava", NAME))

# On rapproche Vahatra
vahatra_names <- vahatra %>%
  st_drop_geometry() %>%
  select(nom_vahatra = nom, full_name_vahatra = full_name,
         nom_wdpa_vahatra = nom_wdpa) %>%
  mutate(full_name_vahatra = str_replace(full_name_vahatra,
                                         "Nouvelle Aire", "Aire"))

pa_names <- pa_names %>%
  left_join(vahatra_names, by = c("full_name_sapm_2017" = "full_name_vahatra"))


# On joint WDPA 2024
wdpa_2024_names <- wdpa_mdg_all %>%
  st_drop_geometry() %>%
  filter(data_year == 2024) %>%
  select(name_wdpa = NAME, orig_name_wdpa = ORIG_NAME, WDPAID)

pa_names <- pa_names %>%
  mutate(name_wdpa = ifelse(is.na(nom_wdpa_vahatra), short_name_sapm_2017,
                            nom_wdpa_vahatra),
         name_wdpa = case_when(
           name_wdpa == "Behara Tranomaro" ~ "Behara-Tranomaro",
           name_wdpa == "Ranobe bay" ~ "Ranobe Bay",
           name_wdpa == "Tampoketsa d'Analamaitso" ~ "Tampoketsa Analamaitso",
           name_wdpa == "Allée des Baobabs" ~ "Allées des Baobabs",
           name_wdpa == "Analabe Betanatanana" ~ "Analabe-Betanatanana",
           name_wdpa == "Ampotaka-Ankorabe" ~ "Ampotaka Ankorabe",
           .default = name_wdpa)
  ) %>%
  select(-nom_wdpa_vahatra)

pa_names <- pa_names %>%
  left_join(wdpa_2024_names, by = c("name_wdpa"), 
            # Duplicate in WDPA with a wrong/incomplete Antrema
            multiple = "first") %>%
  select(-orig_name_wdpa)


pa_names2 <- pa_names %>%
  select(short_name_sapm_2017, name_wdpa, WDPAID) %>%
  mutate(original_pa = str_trim(str_to_lower(short_name_sapm_2017)))

# Load the verified conversion table
conversion_table_verif <- read_xlsx("data/conversion_table_verif.xlsx") %>%
  select(original_pa, closest_match) %>%
  unique() %>%
  clean_pa_names_cols(name_cols = c("original_pa", "closest_match")) %>%
  left_join(pa_names2, by = "original_pa")

sum(is.na(conversion_table_verif$short_name_sapm_2017))


test <- all_PAs_matched %>%
  select(original_name, WDPA_NAME = name_y, WDPAID = id_y) %>%
  st_drop_geometry()

write_csv(test, "data/test.csv")
