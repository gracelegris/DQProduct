# ==========================================================================================================================================
# Script Name: Figures and Tables for 2025 WUENIC Data Quality Powerpoint
# Author: Grace Legris, glegris@unicef.org
# ==========================================================================================================================================

source("user_profiles.R")
source("01_setup.R") 

## -----------------------------------------SET PARAMETERS--------------------------------------------------------------
CountryName <- "Ethiopia"
Current_ISO3 <- "ETH"
Year <- 2025
## ---------------------------------------------------------------------------------------------------------------------

# master wuenic dataset
wuenic_master <- readRDS(file.path(DataDir, "clean_wuenic_MASTER_2025rev.rds"))

# data cleaning functions from subnational folder
source(paste0(SubnatFuncDir, "/user_functions_outliers.R"))
source(paste0(SubnatFuncDir, "/data_quality_funcs.R"))

# wpp (world population prospects) data for denominators
denominators <- read.csv(file.path(DummyUtils, "WPP_denoms_WPP2024.csv"))

# read in geojson with country outlines
cty_outline_shp <- st_read(file.path(DummyUtils, "adm0.geojson")) %>% 
  filter(ISO3 == Current_ISO3)

# regional info file
regional_info <- read_csv(file.path(DummyUtils, "regional-groups_2026-release.csv")) %>% 
  filter(iso3c == Current_ISO3)

# read in languages file
language <- read_excel(file.path(DummyUtils, "Languages.xlsx")) %>% 
  filter(iso3c == Current_ISO3) %>% 
  pull(Language)

# set system environment to display in the country's language (for use in plots)
Sys.setenv(LANGUAGE = language) 

## ---------------------------------------------------------------------------------------------------------------------
### Data Prep
## ---------------------------------------------------------------------------------------------------------------------

wuenic_master <- wuenic_master %>%
  mutate(iso3c = toupper(iso3c)) %>% 
  filter(country == CountryName, year == Year) %>%
  left_join(denominators %>% select(iso3c, year, target, value, value_raw), by = c("iso3c", "year", "target_grp" = "target"))

cty_outline_shp <- cty_outline_shp %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 0.01) %>%
  st_transform(crs = st_crs(4326))

## ---------------------------------------------------------------------------------------------------------------------
### Intro Slides
## ---------------------------------------------------------------------------------------------------------------------

cty_map <- ggplot(cty_outline_shp) +
  geom_sf(fill = "#1CABE2", color = "white", linewidth = 0.5) +
  theme_void()

# intro paragraph using regional info df
intro_paragraph <- paste0(
  regional_info$country, " is located in ", str_to_title(regional_info$un_region), 
  ", within the UNICEF ", toupper(regional_info$region_unicef_ops), " region.",
  
  if_else(regional_info$region_ldc == "ldc", " It is classified as a least developed country (LDC).", ""),
  if_else(regional_info$region_lmic == "lmic", " It is a low- or middle-income country (LMIC).", ""),
  if_else(regional_info$fragility == "conflict", " The country is classified as a fragile and conflict-affected state.", ""),
  if_else(regional_info$gavi == "gavi", " It is eligible for Gavi support.", ""),
  
  if_else(regional_info$region_imm_sp_priority == "imm_sp_priority", 
          paste0(" ", regional_info$country, " is an immunization special priority country."), ""),
  if_else(regional_info$unaids_highimpact == "hiimpact", " It is a UNAIDS high-impact country.", ""),
  if_else(regional_info$big_forty == "big_forty", " It is one of the Big 40 priority countries for immunization.", "")
)
intro_paragraph

## ---------------------------------------------------------------------------------------------------------------------
### Schedule
## ---------------------------------------------------------------------------------------------------------------------

sched_summary_cty <- sched_summary %>%
  filter(iso3c == Current_ISO3)

schedule_table <- sched_summary_cty %>%
  # clean up vaccine code for display
  mutate(
    vaccinecode      = str_replace_all(vaccinecode, "_", " "),
    ages_administered = if_else(is.na(ages_administered) | ages_administered == "", "—", ages_administered),
    targetpop        = if_else(is.na(targetpop) | targetpop == "", "Infants/General", str_to_title(tolower(targetpop)))
  ) %>%
  select(
    Vaccine          = vaccinecode,
    `Target Population` = targetpop,
    `No. Doses`      = n_scheduled_doses,
    `Ages Administered` = ages_administered
  ) %>%
  arrange(Vaccine)

schedule_table <- schedule_table %>%
  kbl(booktabs = TRUE, longtable = TRUE, align = c("l", "l", "c", "l"),
    caption = paste0(CountryName, ": Vaccination Schedule (", max(sched_summary_cty$year), ")")
  ) %>%
  kable_styling(latex_options = c("HOLD_position", "repeat_header", "striped"),
                stripe_color = "#f0f8ff", font_size = 10, full_width = TRUE) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#0083CF") %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(3, width = "1.5cm") %>%
  column_spec(4, width = "5cm")

## ---------------------------------------------------------------------------------------------------------------------
### Coverage
## ---------------------------------------------------------------------------------------------------------------------

## ---------------------------------------------------------------------------------------------------------------------
### Outlier Detection
## ---------------------------------------------------------------------------------------------------------------------

## ---------------------------------------------------------------------------------------------------------------------
### Denominator Checks
## ---------------------------------------------------------------------------------------------------------------------

## ---------------------------------------------------------------------------------------------------------------------
### Dropout & Vaccine Relationships
## ---------------------------------------------------------------------------------------------------------------------

## ---------------------------------------------------------------------------------------------------------------------
### Missing Data
## ---------------------------------------------------------------------------------------------------------------------

## ---------------------------------------------------------------------------------------------------------------------
### Admin vs. Official Comparison
## ---------------------------------------------------------------------------------------------------------------------

## ---------------------------------------------------------------------------------------------------------------------
### Data Prep
## ---------------------------------------------------------------------------------------------------------------------


