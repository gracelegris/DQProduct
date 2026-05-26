### WUENIC 2025 rev: Country-specific charts ###
## func :: translate country names ----
# use translated country names
translate_ctry_names <- function(df) {
  df %>%
    left_join(reg_ref %>% mutate(iso3c = tolower(iso3c)) %>% select(iso3c, country_fr, country_es, country_pt)) %>%
    mutate(country = case_when(language == "fr" & iso3c != "REG" ~ country_fr,
                               language == "es" & iso3c != "REG"  ~ country_es,
                               language == "pt" & iso3c != "REG"  ~ country_pt,
                               TRUE ~ country)) %>%
    select(-c(country_fr, country_es, country_pt))
}
 
## func :: clean region names ----
clean_reg_names <- function(df) {
  df %>%
    mutate(lvl_3 = case_when(lvl_2 %in% c("region_who") ~ toupper(lvl_3), 
                             lvl_2 == "region_unicef_global_old" ~ "Global",
                             grepl("wb", lvl_2) & lvl_3 == "low income" ~ "LICs",
                             grepl("wb", lvl_2) & lvl_3 == "upper middle income" ~ "UMICs",
                             grepl("wb", lvl_2) & lvl_3 == "lower middle income" ~ "LMICs",
                             grepl("wb", lvl_2) & lvl_3 == "high income" ~ "HICs",
                             grepl("wb", lvl_2) & lvl_3 == "middle income" ~ "MICs",
                             grepl("gavi", lvl_2) & lvl_3 == "gavi57" ~ "Gavi 57",
                             grepl("gavi", lvl_2) & lvl_3 == "non_gavi" ~ "Non Gavi",
                             grepl("gavi", lvl_2) & lvl_3 == "gavi" ~ "Gavi",
                             grepl("gavi", lvl_2) & lvl_3 == "gavi_mic" ~ "Gavi MICs",
                             grepl("gavi", lvl_2) & lvl_3 == "Gavi57" ~ "Gavi 57",
                             grepl("gavi", lvl_2) & lvl_3 == "HIC" ~ "HICs",
                             grepl("gavi", lvl_2) & lvl_3 == "NeverGaviMIC" ~ "Never Gavi MICs",
                             grepl("gavi", lvl_2) & lvl_3 == "GaviTransit" ~ "Gavi transition",
                             TRUE ~ lvl_3),
           country = case_when(lvl_2 %in% c("region_who") & lvl_1 == "region" ~ toupper(country),
                               lvl_1 == "region" & lvl_2 == "region_unicef_global_old" ~ "Global",
                               grepl("wb", lvl_2) & country == "low income" ~ "LICs",
                               grepl("wb", lvl_2) & country == "upper middle income" ~ "UMICs",
                               grepl("wb", lvl_2) & country == "lower middle income" ~ "LMICs",
                               grepl("wb", lvl_2) & country == "high income" ~ "HICs",
                               grepl("wb", lvl_2) & country == "middle income" ~ "MICs",
                               grepl("gavi", lvl_2) & country == "gavi57" ~ "Gavi 57",
                               grepl("gavi", lvl_2) & country == "non_gavi" ~ "Non Gavi",
                               grepl("gavi", lvl_2) & country == "gavi" ~ "Gavi",
                               grepl("gavi", lvl_2) & country == "gavi_mic" ~ "Gavi MICs",
                               grepl("gavi", lvl_2) & country == "Gavi57" ~ "Gavi 57",
                               grepl("gavi", lvl_2) & country == "HIC" ~ "HICs",
                               grepl("gavi", lvl_2) & country == "NeverGaviMIC" ~ "Never Gavi MICs",
                               grepl("gavi", lvl_2) & country == "GaviTransit" ~ "Gavi transition",
                               TRUE ~ country))
}
## func :: clean region names ----

## func :: translation tables ----
# translates pre-defined headers and other items in charts
get_text <- function(key, lang, .env = parent.frame()) {
  template <- translations %>%
    filter(key == !!key) %>%
    pull(!!sym(lang)) %>%
    first()
  
  # Replace literal "\\n" with real newline character
  template <- gsub("\\\\n", "\n", template)
  
  # Interpolate variables like {regn}, {rev_yr}
  glue::glue_data(.env, template)
}

# get_text("title", "fr")
## func :: translation tables ----


