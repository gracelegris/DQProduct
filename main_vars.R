####### USER VARIABLES ####### 
library(tidyverse)
library(stringr)

## main variables ----
rev_yr     <- 2025
hpv_rev_yr <- 2025
wpp_rev_yr <- 2024
measlesc   <- "6MARCH2026"   # measles cases data date

## set working directory to Immunization OneDrive ----
USERNAME <- Sys.getenv("USERNAME")
USER <- Sys.getenv("USER")

if (USERNAME %in% c("lfrancis", "laure")) {
  wd <- str_glue("C:/Users/{USERNAME}/OneDrive - UNICEF/Immunization")
  RevDir <- file.path(wd, "WUENIC Revision/2025 rev")
  HPVDir <- file.path(wd, "WUENIC Revision/HPV estimates")
} else if (USER == "UNICEF") {
  RevDir <- file.path("/Users/UNICEF/Library/CloudStorage/OneDrive-SharedLibraries-UNICEF/Health-HIV Data & Analytics - 2025 rev")
  HPVDir <- file.path("/Users/UNICEF/Library/CloudStorage/OneDrive-SharedLibraries-UNICEF/Health-HIV Data & Analytics - HPV estimates")
}

type <- "dummy"  # dummy, draft, final
pct_threshold <- 0.10
min_yr_plots  <- 2010
n_years_comparison_plot <- 5

## ── PATHS ───────────────────────────────────────────────────────────────────
directory <- file.path(RevDir, "unicef-products")
utils      <- str_glue(RevDir, "/unicef-products/{type}/utils")
wrkfolder  <- str_glue(RevDir, "/unicef-products/{type}/country-specific-charts")
wiisefolder <- str_glue(RevDir, "/unicef-products/{type}/wiise-outputs")
dqfolder   <- str_glue(RevDir, "/unicef-products/{type}/data-quality/DQProduct")
SubnatFuncDir <- file.path("/Users/UNICEF/Library/CloudStorage/OneDrive-SharedLibraries-UNICEF/Health-HIV Data & Analytics - Subnational data analysis/utils/R")
DummyDataDir <- str_glue(RevDir, "/wuenic_master/dummy")
ppt_script_path <- file.path(dqfolder, "DQ_ppt_compile_translate.R")


## regions Nicaragua belongs to ----
nic_regions <- c("Global","LACR","AMR")

source(file.path(dqfolder, "R/label_vals.R"))
source(file.path(dqfolder, "R/funcs.R"))

# source functions
source(file.path(dqfolder, "R/label_vals.R"))
source(file.path(dqfolder, "R/funcs.R"))
source(str_glue("{utils}/R/slide_general_funcs.R"))    # func_slide_v, func_slide_bb, etc.
source(str_glue("{utils}/R/slide_production_funcs.R")) # func_slide_v_txt, func_slide_v_tlm, etc.

## ── COLORS ──────────────────────────────────────────────────────────────────
unicef_colors <- c("#0058AB","#1CABE2","#00833D","#80BD41","#6A1E74",
                   "#961A49","#E2231A","#F26A21","#FFC20E","#FFF09C","#002759")

source_colors <- c("WUENIC" = "#0083CF", "Admin" = "#6A1E74", "Official Estimate" = "#80BD41", "Survey" = "#FFC20E")

## ── LOAD DATA ────────────────────────────────────────────────────────────────
source(file.path(dqfolder, "load_data.R"))
