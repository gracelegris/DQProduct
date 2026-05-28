# ==========================================================================================================================================
# Script Name:  Render Data Quality PPTs for all countries
# Author: Grace Legris, glegris@unicef.org
# ==========================================================================================================================================

type <- "draft"

## ─────────────────────────────────────────────────────────────────────────────
source("user_profiles.R")
#source("01_setup.R") 
source(file.path(PrjDir, "R/label_vals.R"))
source(file.path(PrjDir, "R/funcs.R"))

# data cleaning functions from subnational folder
source(paste0(SubnatFuncDir, "/user_functions_outliers.R"))
source(paste0(SubnatFuncDir, "/data_quality_funcs.R"))

## ── PATHS ───────────────────────────────────────────────────────────────────
RevDir <- file.path("/Users/UNICEF/Library/CloudStorage/OneDrive-SharedLibraries-UNICEF/Health-HIV Data & Analytics - 2025 rev")
directory <- file.path("/Users/UNICEF/Library/CloudStorage/OneDrive-SharedLibraries-UNICEF/Health-HIV Data & Analytics - 2025 rev/unicef-products")
utils      <- str_glue(RevDir, "/unicef-products/{type}/utils")
wrkfolder  <- str_glue(RevDir, "/unicef-products/{type}/country-specific-charts")
wiisefolder <- str_glue(RevDir, "/unicef-products/{type}/wiise-outputs")
dqfolder   <- str_glue(RevDir, "/unicef-products/{type}/data-quality/DQProduct")
SubnatFuncDir <- file.path("/Users/UNICEF/Library/CloudStorage/OneDrive-SharedLibraries-UNICEF/Health-HIV Data & Analytics - Subnational data analysis/utils/R")
DummyDataDir <- str_glue(RevDir, "/wuenic_master/dummy")
ppt_script_path <- file.path(dqfolder, "DQ_ppt_formatted.R")

## ── PARAMETERS ───────────────────────────────────────────────────────────────
pct_threshold <- 0.10
second_pct_threshold <- 0.30
rev_yr        <- 2025
hpv_rev_yr    <- 2024
wpp_rev_yr    <- 2024
min_yr_plots  <- 2000
n_years_comparison_plot <- 5 # years to display in the wuenic vs. official vs. admin coverage heatmap
language <- "en"

## ── COLORS ──────────────────────────────────────────────────────────────────
unicef_colors <- c("#0058AB","#1CABE2","#00833D","#80BD41","#6A1E74",
                   "#961A49","#E2231A","#F26A21","#FFC20E","#FFF09C","#002759")

source_colors <- c("WHO/UNICEF" = "#0083CF", "Admin" = "#6A1E74", "Official (Government Estimate)" = "#80BD41", "Survey" = "#FFC20E")

## ── LOAD DATA ────────────────────────────────────────────────────────────────
source(file.path(PrjDir, "load_data.R"))


# get unique list of countries
wuenic_master <- read.csv(file.path(DummyDataDir, "wuenic-master_2025rev.csv"))
countries <- unique(wuenic_master$Country)

# run the loop
cat("Starting  DQ ppt generation for", length(countries), "countries...\n")

for (current_country in countries) {
  
  cat("Processing:", current_country, "\n")
  
  # global variable that 'DQ_ppt_formatted.R' should use to filter data
  .current_country <- current_country 
  
  # get iso3c for this country
  wuenic_master <- read.csv(file.path(DummyDataDir, "wuenic-master_2025rev.csv"))
  .current_iso3c <- wuenic_master %>% filter(Country == current_country) %>% pull(ISOCountryCode) %>% unique()
  x <- tolower(.current_iso3c)
  
  # execute DQ_ppt_formatted.R
  tryCatch({
    #suppressWarnings(
      #suppressMessages(
        source(ppt_script_path, local = FALSE)
      #)
    #)
    cat("✅ Successfully generated PPT for:", current_country, "\n\n")
  }, error = function(e) {
    cat("ERROR processing:", current_country, "\n")
    message(e)
  })
}

cat("All countries processed!")