# ==========================================================================================================================================
# Script Name: Produce Formatted Data Quality PPT
# Author: Grace Legris, glegris@unicef.org
# ==========================================================================================================================================

## ── PARAMETERS ─────────────────────────────────────────────────────────────
CountryName   <- "Ethiopia"
Current_ISO3  <- "ETH"
x             <- "eth"
pct_threshold <- 0.10
rev_yr        <- 2025
hpv_rev_yr    <- 2024
wpp_rev_yr    <- 2024
min_yr_plots  <- 2000
n_years_comparison_plot <- 5 # years to display in the wuenic vs. official vs. admin coverage heatmap
type          <- "dummy"
language <- "en"

## ── LIBRARIES ───────────────────────────────────────────────────────────────
library(tidyverse)
library(readxl)
library(openxlsx)
library(janitor)
library(here)
library(ggrepel)
library(ggpattern)
library(patchwork)
library(ggbump)
library(gghighlight)
library(ggpubr)
library(gridGraphics)
library(ggplotify)
library(purrr)
library(flextable)
library(officer)
library(rvg)

options(scipen = 999)

## ── PATHS ───────────────────────────────────────────────────────────────────
# Mirror the same folder conventions your team uses so paths to the shared
# template and utility scripts resolve identically.
RevDir <- file.path("/Users/UNICEF/Library/CloudStorage/OneDrive-SharedLibraries-UNICEF/Health-HIV Data & Analytics - 2025 rev")
utils      <- str_glue(RevDir, "/unicef-products/{type}/utils")
wrkfolder  <- str_glue(RevDir, "/unicef-products/{type}/country-specific-charts")
wiisefolder <- str_glue(RevDir, "/unicef-products/{type}/wiise-outputs")
dqfolder   <- str_glue(RevDir, "/unicef-products/{type}/data-quality/DQProduct")

## ── SOURCE SHARED UTILITY FUNCTIONS ────────────────────────────────────────
source(file.path(dqfolder, "R/label_vals.R"))
source(file.path(dqfolder, "R/funcs.R"))
source(str_glue("{utils}/R/slide_general_funcs.R"))    # func_slide_v, func_slide_bb, etc.
source(str_glue("{utils}/R/slide_production_funcs.R")) # func_slide_v_txt, func_slide_v_tlm, etc.

# Subnational data quality helpers
source(paste0(SubnatFuncDir, "/user_functions_outliers.R"))
source(paste0(SubnatFuncDir, "/data_quality_funcs.R"))

## ── COLORS ──────────────────────────────────────────────────────────────────
unicef_colors <- c("#0058AB","#1CABE2","#00833D","#80BD41","#6A1E74",
                   "#961A49","#E2231A","#F26A21","#FFC20E","#FFF09C","#002759")

source_colors <- c("WHO/UNICEF"                    = "#0083CF",
                   "Admin"                          = "#6A1E74",
                   "Official (Government Estimate)" = "#80BD41",
                   "Survey"                         = "#FFC20E")

## ── DATA LOADING ─────────────────────────────────────────────────────────────

# Master WUENIC dataset
wuenic_master <- read.csv(file.path(DummyDataDir, "wuenic-master_2025rev.csv")) %>%
  mutate(ISOCountryCode = toupper(ISOCountryCode)) %>%
  filter(ISOCountryCode == Current_ISO3,
         Year >= min_yr_plots, Year <= rev_yr) %>%
  mutate(Vaccine = case_when(
    Vaccine == "hepbb" ~ "HepBB", Vaccine == "hepb3" ~ "HepB3",
    Vaccine == "hib3"  ~ "Hib3",  Vaccine == "rotac" ~ "RotaC",
    Vaccine == "menga" ~ "MengA", Vaccine == "hpvc"  ~ "HPVc",
    TRUE ~ toupper(Vaccine))) %>%
  rename(Admin = AdministrativeCoverage,
         Official = GovernmentEstimate,
         Survey = SurveyInformation) %>%
  relocate(Admin, .after = WUENIC) %>%
  relocate(Official, .after = Admin) %>%
  relocate(Survey, .after = Official) %>%
  relocate(ChildrenVaccinated, .after = Survey) %>%
  relocate(ChildrenInTarget, .after = ChildrenVaccinated)

wuenic_master$Vaccine <- factor(
  wuenic_master$Vaccine,
  levels = c("BCG","HepBB","DTP1","DTP3","Hib3","HepB3","PCVC","RotaC",
             "POL3","IPV1","IPVC","MCV1","RCV1","MCV2","YFV","MengA","HPVc"))

# Regional info
regional_info <- read_csv(file.path(DummyUtils, "regional-groups_2026-release.csv")) %>%
  filter(iso3c == Current_ISO3)

# WIISE schedule and introductions
wiise_schedule  <- read_excel(str_glue("{wiisefolder}/output/wiise-schedule-dta_{rev_yr}rev.xlsx"))
wiise_intro     <- read_excel(str_glue("{wiisefolder}/output/wiise-intro-dta_{rev_yr}rev.xlsx"))
wiise_stockouts <- read_excel(str_glue("{wiisefolder}/output/wiise-stock-dta_{rev_yr}rev.xlsx"))

# Latest WUENIC revision (for heatmap / stockout helpers)
wueniclatestrev <- read_rds(here(str_glue(
  file.path(RevDir, "/unicef-products/{type}/01_wuenic_dataset-prep/clean_wuenic_MASTER_{rev_yr}rev.rds")
))) %>%
  mutate(country = case_when(
    iso3c == "bol" ~ "Bolivia", iso3c == "caf" ~ "CAR",   iso3c == "cod" ~ "DRC",
    iso3c == "fsm" ~ "Micronesia", iso3c == "irn" ~ "Iran", iso3c == "png" ~ "PNG",
    iso3c == "prk" ~ "DPRK",   iso3c == "syr" ~ "Syria",  iso3c == "lao" ~ "Laos",
    iso3c == "tza" ~ "Tanzania", iso3c == "ven" ~ "Venezuela", iso3c == "tur" ~ "Turkiye",
    TRUE ~ country)) %>%
  filter(year >= 2000, lvl_2 %in% c("region_unicef_global_old","region_unicef_ops")) %>%
  label_vals_millions(target, "target_lbl") %>%
  label_vals_millions(vaccinated, "vaccinated_lbl") %>%
  label_vals_millions(unvaccinated, "unvaccinated_lbl")

wueniclatestrev <- clean_reg_names(wueniclatestrev)
wvax <- unique(wueniclatestrev$vaccine)

# Stockout data — clean to match WUENIC vaccine names
wuenic_stockouts_clean <- wiise_stockouts %>%
  rename(code = vaccine) %>%
  mutate(vaccine = tolower(code),
         vaccine = case_when(
           vaccine == "opv"       ~ "pol3",
           vaccine == "pcv"       ~ "pcv3",
           vaccine == "ipv"       ~ "ipv1;ipv2",
           vaccine == "hepb"      ~ "hepbb;hepb3",
           vaccine == "hib"       ~ "hib3",
           vaccine == "rotavirus" ~ "rotac",
           vaccine %in% c("measles-rubella (mr)","measles-mumps-rubella (mmr)") ~ "mcv1;mcv2;rcv1",
           vaccine == "dtp-hib-hepb-ipv"     ~ "dtp1;dtp3;hib3;hepb3",
           vaccine == "dtp-containing vaccine"~ "dtp1;dtp3;hib3;hepb3",
           vaccine == "dtp-hepb-ipv"         ~ "dtp1;dtp3;ipv1;hepb3;ipv2",
           vaccine == "dtp-hib-ipv"          ~ "dtp1;dtp3;hib3;ipv1;ipv2",
           vaccine == "dtp-hib-hepb"         ~ "dtp1;dtp3;hib3;hepb3",
           vaccine == "mcv"  ~ "mcv1;mcv2",
           vaccine == "mena" ~ "menga",
           vaccine == "rcv"  ~ "rcv1",
           TRUE ~ vaccine)) %>%
  mutate(v = strsplit(vaccine, ";")) %>%
  unnest_wider(v, names_sep = "_") %>%
  select(iso3c, year, starts_with("v_")) %>%
  pivot_longer(-c(iso3c, year), names_to = "v", values_to = "vaccine") %>%
  select(-v) %>%
  filter(vaccine %in% wvax) %>%
  mutate(any_stockout = 1) %>%
  distinct()

# Add stockout markers to wuenic_master
wuenic_stockouts_master <- wuenic_stockouts_clean %>%
  mutate(iso3c = toupper(iso3c),
         vaccine = case_when(
           vaccine == "hepbb" ~ "HepBB", vaccine == "hepb3" ~ "HepB3",
           vaccine == "hib3"  ~ "Hib3",  vaccine == "rotac" ~ "RotaC",
           vaccine == "menga" ~ "MengA", vaccine == "hpvc"  ~ "HPVc",
           TRUE ~ toupper(vaccine)))

wuenic_master <- wuenic_master %>%
  left_join(wuenic_stockouts_master %>% select(iso3c, year, vaccine, any_stockout),
            by = c("ISOCountryCode" = "iso3c", "Year" = "year", "Vaccine" = "vaccine")) %>%
  mutate(any_stockout = if_else(is.na(any_stockout), 0, any_stockout))

# Vaccine introduction table (for PPT)
tbl_intro_r <- wiise_intro %>%
  mutate(iso3c = tolower(iso3c)) %>%
  filter(iso3c == x) %>%
  select(vaccine_name, contains("year_intro")) %>%
  arrange(vaccine_name) %>%
  rename(Vaccine              = vaccine_name,
         `National introduction` = year_intro_national,
         `Partial introduction`  = year_intro_partial,
         `Risk groups`           = year_intro_risk_groups,
         `Risk areas`            = year_intro_risk_area) %>%
  select(where(~ !all(is.na(.) | . == ""))) %>%
  mutate(across(where(is.numeric), ~ as.character(.)))

no_data <- function(df) nrow(df) == 0

## ── GENERATE ALL DATA QUALITY PLOTS ─────────────────────────────────────────

source(file.path(PrjDir, "R/tbl_schedule.R"))          # → tbl_schedule, tbl_schedule_r

if (!no_data(tbl_intro_r)) {
  source(file.path(PrjDir, "R/tbl_intro.R"))            # → tbl_intro
}

source(file.path(PrjDir, "R/tbl_stock.R"))              # → tbl_stock, tbl_stock_r

source(file.path(PrjDir, "R/all_vax_heatmap.R"))        # → plt_all_vax_heatmap  (WUENIC coverage heatmap)

# ── DQ-specific plots (from your figures script) ─────────────────────────────
source(file.path(PrjDir, "figs_tables_ppt.R"))

# Intro paragraph (plain text — no translation needed)
intro_paragraph <- paste0(
  regional_info$country, " is located in ", str_to_title(regional_info$un_region),
  ", within the UNICEF ", toupper(regional_info$region_unicef_ops), " region.",
  if_else(regional_info$region_ldc    == "ldc",          " It is classified as a least developed country (LDC).", ""),
  if_else(regional_info$region_lmic   == "lmic",         " It is a low- or middle-income country (LMIC).", ""),
  if_else(regional_info$fragility     == "conflict",      " The country is classified as a fragile and conflict-affected state.", ""),
  if_else(regional_info$gavi          == "gavi",          " It is eligible for Gavi support.", ""),
  if_else(regional_info$region_imm_sp_priority == "imm_sp_priority",
          paste0(" ", regional_info$country, " is an immunization special priority country."), ""),
  if_else(regional_info$unaids_highimpact == "hiimpact",  " It is a UNAIDS high-impact country.", ""),
  if_else(regional_info$big_forty     == "big_forty",     " It is one of the Big 40 priority countries for immunization.", "")
)

## ── CONVERT PLOTS TO EDITABLE DML ───────────────────────────────────────────
# Collect every plt_* object and wrap it in dml() for PowerPoint embedding.
# Arabic PNG fallback from the original script is not needed here (English only).
plot_names     <- ls(pattern = "^plt_")
ggplot_objects <- mget(plot_names, envir = .GlobalEnv)

for (name in names(ggplot_objects)) {
  assign(
    paste0("dml_", name),
    rvg::dml(ggobj = ggplot_objects[[name]], editable = TRUE),
    envir = .GlobalEnv
  )
}

## ── BUILD POWERPOINT ─────────────────────────────────────────────────────────
doc <- read_pptx(str_glue("{utils}/region-specific-blank-slides.pptx"))

slide_title <- CountryName
rect <- rectGrob(gp = gpar(fill = "black", col = NA))  # top-line message underline

# ── COVER ────────────────────────────────────────────────────────────────────
doc <- add_slide(doc, layout = "cover_country", master = "Office Theme")
doc <- ph_with(
  x = doc,
  block_list(fpar(
    ftext(CountryName,
          prop = fp_text(font.size = 40, color = "white")),
    fp_p = fp_par(text.align = "center"))),
  location = ph_location("body", left = 1.9, top = 2.86, width = 9.5, height = 2))

# Subtitle: report type
doc <- ph_with(
  x = doc,
  block_list(fpar(
    ftext(paste("Administrative Data Quality Review Report,", rev_yr),
          prop = fp_text(font.size = 22, color = "white")),
    fp_p = fp_par(text.align = "center"))),
  location = ph_location("body", left = 3.34, top = 3.92, width = 7, height = 1.22))

doc <- remove_slide(doc, index = 1)   # remove the blank first slide in the template

# ── INTRO SLIDE ───────────────────────────────────────────────────────────────
doc <- add_slide(doc, layout = "intro_slide", master = "Office Theme")

# Country context paragraph
# doc <- ph_with(
#   x = doc,
#   block_list(fpar(
#     ftext(intro_paragraph,
#           prop = fp_text(font.size = 14, color = "white")),
#     fp_p = fp_par(text.align = "left"))),
#   location = ph_location("body", left = 0.57, top = 1.5, width = 12.2, height = 4, bg = "#203864"))

# ── SECTION DIVIDER: SCHEDULE & STOCKOUTS ────────────────────────────────────
func_slide_bb("Schedule & Stockouts")

# Vaccine schedule table
schedule_year <- wiise_schedule %>% 
  filter(iso3c == x) %>% 
  pull(year) %>% 
  max(na.rm = TRUE)
if (nrow(tbl_schedule_r) > 0 && ncol(tbl_schedule_r) > 0) {
  doc <- add_slide(doc, layout = "data_1", master = "Office Theme")
  doc <- ph_with(doc,
                 value = fpar(ftext("Vaccine Schedule",
                                    prop = fp_text(font.size = 28, color = "black", font.family = "Calibri"))),
                 location = ph_location(left = 0.6, top = 0.6, width = 8, height = 1))
  doc <- ph_with(x = doc, value = tbl_schedule,
                 location = ph_location(left = 0.6, top = 1.5, width = total_width_sched, height = 5))
  func_slide_v_txt(paste0("Vaccine schedule as reported to WIISE. Latest update: ", schedule_year, "."))
}

# Vaccine introductions table
if (!no_data(tbl_intro_r) && nrow(tbl_intro_r) > 0 && ncol(tbl_intro_r) > 0) {
  doc <- add_slide(doc, layout = "data_1", master = "Office Theme")
  doc <- ph_with(doc,
                 value = fpar(ftext("Vaccine Introductions",
                                    prop = fp_text(font.size = 28, color = "black", font.family = "Calibri"))),
                 location = ph_location(left = 0.6, top = 0.6, width = 8, height = 1))
  doc <- ph_with(x = doc, value = tbl_intro,
                 location = ph_location(left = 0.6, top = 1.5, width = 10, height = 5))
  func_slide_v_txt("Year in which each vaccine was introduced nationally, partially, for risk groups, or risk areas.")
}

# Stockout table
if (nrow(tbl_stock_r) > 0 && ncol(tbl_stock_r) > 0) {
  doc <- add_slide(doc, layout = "data_no_logos", master = "Office Theme")
  doc <- ph_with(doc,
                 value = fpar(ftext("Vaccine Stockouts",
                                    prop = fp_text(font.size = 28, color = "black", font.family = "Calibri"))),
                 location = ph_location(left = 0.6, top = 0.6, width = 8, height = 1))
  doc <- ph_with(x = doc, value = tbl_stock,
                 location = ph_location(left = 0.6, top = 1.5, width = 10, height = 5))
  func_slide_v_txt("Years in which a national or subnational vaccine stockout was recorded.")
}

# ── SECTION DIVIDER: COVERAGE OVERVIEW ───────────────────────────────────────
func_slide_bb("Coverage Overview")

# WUENIC coverage heatmap
min_yr <- min(df$year)
max_yr <- max(df$year)
func_slide_v(dml_plt_all_vax_heatmap)
func_slide_v_txt(paste0("WHO/UNICEF coverage estimates across all vaccines, ", min_yr, "–", max_yr, ".",
                        " Blank cells indicate years prior to vaccine introduction."))

# Summary table: coverage by source
func_slide_v(dml_plt_summary_table)
func_slide_v_txt(paste0(
  "Coverage by vaccine and data source (WHO/UNICEF, Admin, Official) for the most recent ",
  n_years_comparison_plot, " years. Colour scale: red (<60%) to green (≥90%). Missing cells shown in grey. Blank cells indicate years prior to vaccine introduction."))

# ── SECTION DIVIDER: COVERAGE TRENDS BY SOURCE ───────────────────────────────
func_slide_bb("Coverage Trends by Source")

# All vaccines — line chart
func_slide_v(dml_plt_all_vax_line)
func_slide_v_txt(paste0(
  "Time-series of WHO/UNICEF estimates, administrative coverage, official government estimates, ",
  "and survey data by vaccine, ", min_yr_plots, "–", rev_yr, "."))

# DTP1, DTP3, MCV1 — line chart
func_slide_v(dml_plt_selected_vax_line)
func_slide_v_txt(paste0(
  "Coverage trends for DTP1, DTP3, and MCV1 by data source, ", min_yr_plots, "–", rev_yr, "."))

# ── SECTION DIVIDER: ADMIN DATA FLAGS ────────────────────────────────────────
func_slide_bb("Admin Coverage Flags")

# All vaccines — flag chart
func_slide_v(dml_plt_coverage_flags)
func_slide_v_txt(paste0(
  "Admin coverage flagged where values exceed 100% (orange) or change by more than ±",
  pct_threshold * 100, " percentage points from the previous year (red), ", min_yr_plots, "–", rev_yr, "."))

# DTP1, DTP3, MCV1 — flag chart
func_slide_v(dml_plt_selected_coverage_flags)
func_slide_v_txt(paste0(
  "Same flag logic applied to DTP1, DTP3, and MCV1 only, ", min_yr_plots, "–", rev_yr, "."))

# ── SECTION DIVIDER: ADMIN VS ESTIMATES ──────────────────────────────────────
func_slide_bb("Admin vs. Estimates")

# Admin vs WHO/UNICEF
func_slide_v(dml_plt_admin_vs_wuenic)
func_slide_v_txt(paste0(
  "Comparison of administrative coverage and WHO/UNICEF estimates by vaccine. ",
  "Red points indicate gaps exceeding ±", pct_threshold * 100, " percentage points."))

# Admin vs Official Government Estimate
func_slide_v(dml_plt_admin_vs_official)
func_slide_v_txt(paste0(
  "Comparison of administrative coverage and official government estimates. ",
  "Red points indicate gaps exceeding ±", pct_threshold * 100, " percentage points."))

# ── SECTION DIVIDER: NUMERATOR CHECKS ────────────────────────────────────────
func_slide_bb("Numerator Checks")

# Year-on-year % change in vaccinated children
func_slide_v(dml_plt_perc_change_line)
func_slide_v_txt(paste0(
  "Year-to-year percentage change in the number of children vaccinated (numerator) by vaccine. ",
  "Red points flag changes exceeding ±", pct_threshold * 100,
  "%. Green line shows raw counts on the secondary axis. ",
  "Orange shading indicates a vaccine stockout in that year."))

# ── SECTION DIVIDER: DENOMINATOR CHECKS ──────────────────────────────────────
func_slide_bb("Denominator Checks")

# Year-on-year % change in target population (all vaccines)
func_slide_v(dml_plt_denom_change)
func_slide_v_txt(paste0(
  "Year-to-year percentage change in the number of children in the target population (denominator) ",
  "by vaccine. Red points flag changes exceeding ±", pct_threshold * 100,
  "%. Green line shows raw counts on the secondary axis."))

# Live births (BCG) vs surviving infants (DTP1)
func_slide_v(dml_plt_births_vs_si)
func_slide_v_txt(paste0(
  "Comparison of live births (BCG denominator) and surviving infants (DTP1 denominator) ",
  "over time, ", min_yr_plots, "–", rev_yr, ". Large or persistent divergence may indicate a denominator issue."))

# % change in BCG and DTP1 denominators
func_slide_v(dml_plt_denom_pct_change)
func_slide_v_txt(paste0(
  "Year-to-year percentage change in BCG (live births) and DTP1 (surviving infants) denominators. ",
  "Red points flag changes exceeding ±", pct_threshold * 100, "%."))

# ── SECTION DIVIDER: DROPOUT & CO-ADMINISTRATION ────────────────────────────
func_slide_bb("Dropout & Co-administration")

# Dropout rates
func_slide_vnologo(dml_plt_dropout_with_rate)
func_slide_v_txt(paste0(
  "Admin coverage for key vaccine pairs (DTP1→DTP3, DTP1→MCV1) with dropout rate shown as bars. ",
  "High dropout may indicate supply, access, or demand barriers."))

# DTP3–PCVC co-administration
func_slide_v(dml_plt_coadmin_dtp_pcv)
func_slide_v_txt(paste0(
  "Admin coverage for DTP3 and PCVC, which are co-administered. ",
  "Divergence between the two series may indicate a reporting inconsistency."))

# 6-week vaccine cluster
func_slide_v(dml_plt_6wk)
func_slide_v_txt("Admin coverage of all vaccines administered at 6 weeks per the national schedule.")

# 14-week vaccine cluster
func_slide_v(dml_plt_14wk)
func_slide_v_txt("Admin coverage of all vaccines administered at 14 weeks per the national schedule.")

# ── SECTION DIVIDER: DATA AVAILABILITY ───────────────────────────────────────
func_slide_bb("Data Availability")

# Missing data heatmap
func_slide_v(dml_plt_missing_heatmap)
func_slide_v_txt(paste0(
  "Heatmap showing whether admin data are present (green), missing (red), or not applicable ",
  "because the vaccine had not yet been introduced (white), by vaccine and year."))

# ── SAVE ─────────────────────────────────────────────────────────────────────
folder_path <- str_glue("{dqfolder}/outputs")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

print(doc, target = file.path(folder_path, paste0(x, "_en.pptx")))

message("✓ Data quality report saved to: ", folder_path)

## END -------------------------------------------------------------------------