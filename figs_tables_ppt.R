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

# use schedule summary that only includes the most recent year for each country
sched_summary_recent_cty <- sched_summary_recent %>%
  filter(iso3c == Current_ISO3)

schedule_table <- sched_summary_recent_cty %>%
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

# check if there have been schedule changes since 2020
country_sched <- sched_summary %>%
  filter(iso3c == toupper(Current_ISO3)) %>%
  arrange(vaccinecode, targetpop, year)

min_yr <- min(country_sched$year, na.rm = TRUE)
max_yr <- max(country_sched$year, na.rm = TRUE)

# vaccines that appeared in early years but not in the most recent year
dropped_notes <- country_sched %>%
  group_by(vaccinecode, targetpop) %>%
  summarise(last_yr = max(year), first_yr = min(year), .groups = "drop") %>%
  filter(last_yr < (max_yr - 1)) %>%
  mutate(
    clean_vacc = str_replace_all(vaccinecode, "_", " "),
    note = paste0(clean_vacc, " last recorded on schedule in ", last_yr)
  ) %>%
  distinct(note) %>%
  pull(note)

# changes within vaccines over time
schedule_changes <- country_sched %>%
  group_by(vaccinecode, targetpop) %>%
  mutate(
    prev_doses    = lag(n_scheduled_doses),
    prev_ages     = lag(ages_administered),
    is_new        = is.na(prev_doses) & year > min_yr,
    doses_changed = !is.na(prev_doses) & (n_scheduled_doses != prev_doses),
    ages_changed  = !is.na(prev_ages) & (ages_administered != prev_ages),
    any_change    = is_new | doses_changed | ages_changed
  ) %>%
  filter(any_change) %>%
  mutate(
    clean_vacc  = str_replace_all(vaccinecode, "_", " "),
    change_note = case_when(
      is_new ~ paste0(clean_vacc, " introduced in ", year),
      doses_changed & ages_changed ~ paste0(
        clean_vacc, ": doses changed from ", prev_doses, " to ", n_scheduled_doses,
        ", ages from ", prev_ages, " to ", ages_administered, " (", year, ")"),
      doses_changed ~ paste0(
        clean_vacc, ": number of doses changed from ", prev_doses, " to ", n_scheduled_doses, " (", year, ")"),
      ages_changed ~ paste0(
        clean_vacc, ": age schedule changed from '", prev_ages, "' to '", ages_administered, "' (", year, ")")
    )
  ) %>%
  ungroup() %>%
  distinct(change_note) %>%
  pull(change_note) %>%
  na.omit()

all_changes <- c(dropped_notes, schedule_changes)

if (length(all_changes) == 0) {
  schedule_change_note <- paste0("No changes to the ", CountryName, " vaccination schedule were recorded between ", min_yr, " and ", max_yr, ".")
} else {
  schedule_change_note <- c(
    paste0("Schedule changes recorded for ", CountryName, " since ", min_yr, ":"),
    paste0("  \u2022 ", sort(all_changes))
  )
}

cat(schedule_change_note, sep = "\n")

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


