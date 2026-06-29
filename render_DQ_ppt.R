# ==========================================================================================================================================
# Script Name:  Render Data Quality PPTs for all countries
# Author: Grace Legris, glegris@unicef.org
# ==========================================================================================================================================

rm(list = ls())

source("main_vars.R")
source("user_profiles.R")

## ── PARAMETERS ───────────────────────────────────────────────────────────────
language <- "en"

# get unique list of countries
wuenic_master <- read.csv(file.path(DummyDataDir, "wuenic-master_2025rev.csv"))
countries <- unique(wuenic_master$Country)

# ── LANGUAGE SETUP ────────────────────────────────────────────────────────────
language_list <- read_excel(str_glue("{utils}/Languages.xlsx")) %>%
  janitor::clean_names() %>%
  mutate(iso3_code = tolower(iso3_code)) %>%
  select(iso3c = iso3_code, language)

iso_codes <- unique(wuenic_master$ISOCountryCode)
list_fr <- language_list %>% filter(language == "fr") %>% filter(iso3c %in% iso_codes) %>% pull(iso3c) # french
list_es <- language_list %>% filter(language == "es") %>% filter(iso3c %in% iso_codes) %>% pull(iso3c) # spanish
list_pt <- language_list %>% filter(language == "pt") %>% filter(iso3c %in% iso_codes) %>% pull(iso3c) # portuguese
list_ar <- language_list %>% filter(language == "ar") %>% filter(iso3c %in% iso_codes) %>% pull(iso3c) # arabic

# run the loop
cat("Starting  DQ ppt generation for", length(countries), "countries...\n")

current_country = "Ethiopia"

for (current_country in countries) {
  
  cat("Processing:", current_country, "\n")
  
  .current_country <- current_country
  ctryn <- current_country
  
  wuenic_master <- read.csv(file.path(DummyDataDir, "wuenic-master_2025rev.csv"))
  .current_iso3c <- wuenic_master %>% filter(Country == current_country) %>% pull(ISOCountryCode) %>% unique()
  x <- tolower(.current_iso3c)
  
  # determine languages to produce (skipping arabic for now)
  if (x %in% list_fr) {
    languages <- c("en", "fr")
  } else if (x %in% list_es) {
    languages <- c("en", "es")
  } else if (x %in% list_pt) {
    languages <- c("en", "pt")
  } else {
    languages <- "en"
  }
  
  tryCatch({
    suppressWarnings(suppressMessages(
      source(ppt_script_path, local = FALSE)
    ))
    cat("✅ Successfully generated PPT for:", current_country, "\n\n")
  }, error = function(e) {
    cat("❌ ERROR processing:", current_country, "\n")
    message(e)
  })
}

cat("All countries processed!")