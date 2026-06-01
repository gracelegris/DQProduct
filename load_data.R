# ==========================================================================================================================================
# Script Name: Load data
# Author: Grace Legris, glegris@unicef.org
# ==========================================================================================================================================

# source functions
source(file.path(dqfolder, "R/label_vals.R"))
source(file.path(dqfolder, "R/funcs.R"))
source(str_glue("{utils}/R/slide_general_funcs.R"))    # func_slide_v, func_slide_bb, etc.
source(str_glue("{utils}/R/slide_production_funcs.R")) # func_slide_v_txt, func_slide_v_tlm, etc.
source(paste0(SubnatFuncDir, "/user_functions_outliers.R"))
source(paste0(SubnatFuncDir, "/data_quality_funcs.R"))

# regional info file
regional_info <- read_csv(file.path(DummyUtils, "regional-groups_2026-release.csv"))

# read in wiise schedule (new version that has the most recent year of schedule data for each country, not just rev_yr)
wiise_schedule <- read_excel(str_glue("{wiisefolder}/output/wiise-schedule-dta_mostrecent.xlsx"))

# WIISE schedule and introductions
wiise_intro <- read_excel(str_glue("{wiisefolder}/output/wiise-intro-dta_{rev_yr}rev.xlsx"))
wiise_stockouts <- read_excel(str_glue("{wiisefolder}/output/wiise-stock-dta_{rev_yr}rev.xlsx"))

# clean vaccine names
wiise_intro <- wiise_intro %>%
  mutate(vaccine = case_when(
    vaccine == "HEPB"         ~ "HepB3",
    vaccine == "HepB_BD"      ~ "HepB_BD", # Often kept as BD or HepB0
    vaccine == "HIB"          ~ "Hib3",
    vaccine == "IPV"          ~ "IPV1",
    vaccine == "IPV2"         ~ "IPVC",
    vaccine == "MMCV"         ~ "MCV1",
    vaccine == "MCV2"         ~ "MCV2",
    vaccine == "PNEUMO_CONJ"  ~ "PCVC",
    vaccine == "ROTAVIRUS"    ~ "RotaC",
    vaccine == "RUBELLA"      ~ "RCV1",
    vaccine == "YF"           ~ "YFV",
    TRUE ~ vaccine
  ))

# latest WUENIC revision (for heatmap / stockout helpers)
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

# stockout data — clean to match WUENIC vaccine names
wuenic_stockouts_clean <- wiise_stockouts %>%
  rename(code = vaccine) %>%
  mutate(vaccine = tolower(code),
         vaccine = case_when(
           vaccine == "opv" ~ "pol3",
           vaccine == "pcv" ~ "pcv3",
           vaccine == "ipv" ~ "ipv1;ipv2",
           vaccine == "hepb" ~ "hepbb;hepb3",
           vaccine == "hib" ~ "hib3",
           vaccine == "rotavirus" ~ "rotac",
           vaccine %in% c("measles-rubella (mr)","measles-mumps-rubella (mmr)") ~ "mcv1;mcv2;rcv1",
           vaccine == "dtp-hib-hepb-ipv" ~ "dtp1;dtp3;hib3;hepb3",
           vaccine == "dtp-containing vaccine"~ "dtp1;dtp3;hib3;hepb3",
           vaccine == "dtp-hepb-ipv" ~ "dtp1;dtp3;ipv1;hepb3;ipv2",
           vaccine == "dtp-hib-ipv" ~ "dtp1;dtp3;hib3;ipv1;ipv2",
           vaccine == "dtp-hib-hepb" ~ "dtp1;dtp3;hib3;hepb3",
           vaccine == "mcv" ~ "mcv1;mcv2",
           vaccine == "mena" ~ "menga",
           vaccine == "rcv" ~ "rcv1",
           TRUE ~ vaccine)) %>%
  mutate(v = strsplit(vaccine, ";")) %>%
  unnest_wider(v, names_sep = "_") %>%
  select(iso3c, year, starts_with("v_")) %>%
  pivot_longer(-c(iso3c, year), names_to = "v", values_to = "vaccine") %>%
  select(-v) %>%
  filter(vaccine %in% wvax) %>%
  mutate(any_stockout = 1) %>%
  distinct()

# ad_comments data from wiisemart
comments <- read.csv(file.path(DataDir, "data_export_WIISE_AD_COMMENTS.csv")) %>% 
  rename(iso3c = COUNTRY, year = YEAR) %>% 
  filter(CMT_FIELDS %in% c("FCTACCNUMERCMT", "FCTACCDENOMSOURCECMT", "FCTACCDENOMCMT")) %>% 
  mutate(CMT_FIELDS = case_when(
    CMT_FIELDS == "FCTACCNUMERCMT" ~ "factor_accuracy_num",
    CMT_FIELDS == "FCTACCDENOMSOURCECMT" ~ "explanation_denom_source",
    CMT_FIELDS == "FCTACCDENOMCMT" ~ "factor_accuracy_denom",
    TRUE ~ CMT_FIELDS
  )) %>% 
  arrange(iso3c, year)

# HPV data
hpv_dta <- read_excel(str_glue(file.path(HPVDir, '{hpv_rev_yr} revision/final/hpv_estimates_wuenic{hpv_rev_yr}rev.xlsx'))) %>%
  filter(lvl_2 %in% c("region_unicef_global_old", "region_unicef_ops")) %>%
  mutate(vaccine = case_when(
    # programme coverage
    vaccine_code == "PRHPV1_M" ~ "HPV1 Males",
    vaccine_code == "PRHPVC_M" ~ "HPVc Males",
    vaccine_code == "PRHPV1_F" ~ "HPV1 Females",
    vaccine_code == "PRHPVC_F" ~ "HPVc Females",
    # by age 15
    vaccine_code == "15HPV1_M" ~ "HPV1 Males, by age 15",
    vaccine_code == "15HPVC_M" ~ "HPVc Males, by age 15",
    vaccine_code == "15HPV1_F" ~ "HPV1 Females, by age 15",
    vaccine_code == "15HPVC_F" ~ "HPVc Females, by age 15",
    TRUE ~ NA_character_)) %>%
  label_vals_millions(target, "target_lbl") %>%
  label_vals_millions(vaccinated, "vaccinated_lbl") %>%
  label_vals_millions(unvaccinated, "unvaccinated_lbl") 

hpv_dta <- clean_reg_names(hpv_dta)  %>% # clean region names
  filter(!is.na(coverage)) %>%
  select(lvl_1, lvl_2, lvl_3, iso3c, country, year, vaccine_code, vaccine, coverage) %>%
  mutate(coverage = round(coverage, 0))