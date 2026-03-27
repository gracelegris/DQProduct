# ==========================================================================================================================================
# Script Name: Figures and Tables for 2025 WUENIC Data Quality Powerpoint
# Author: Grace Legris, glegris@unicef.org
# ==========================================================================================================================================

source("user_profiles.R")
source("01_setup.R") 
source(file.path(PrjDir, "R/label_vals.R"))
source(file.path(PrjDir, "R/funcs.R"))

## -----------------------------------------SET PARAMETERS--------------------------------------------------------------
CountryName <- "Ethiopia"
Current_ISO3 <- "ETH"
pct_threshold <- 0.20 # threshold for flagging large year-to-year changes in coverage data (e.g., 20% change)
rev_yr       <- 2025
hpv_rev_yr   <- 2024
wpp_rev_yr   <- 2024
type         <- "dummy"
x <- "eth"
## ---------------------------------------------------------------------------------------------------------------------

# master wuenic dataset
wuenic_master <- read.csv(file.path(DummyDataDir, "wuenic-master_2025rev.csv"))

# data cleaning functions from subnational folder
source(paste0(SubnatFuncDir, "/user_functions_outliers.R"))
source(paste0(SubnatFuncDir, "/data_quality_funcs.R"))

# wpp (world population prospects) data for denominators
denominators <- read.csv(file.path(DummyUtils, "WPP_denoms_WPP2024.csv"))

# read in geojson with country outlines
# cty_outline_shp <- st_read(file.path(DummyUtils, "adm0.geojson")) %>% 
#   filter(ISO3 == Current_ISO3)

# regional info file
regional_info <- read_csv(file.path(DummyUtils, "regional-groups_2026-release.csv")) %>% 
  filter(iso3c == Current_ISO3)

# read in languages file
language <- read_excel(file.path(DummyUtils, "Languages.xlsx")) %>% 
  filter(ISO3_code == Current_ISO3) %>% 
  pull(Language)

# set system environment to display in the country's language (for use in plots)
Sys.setenv(LANGUAGE = language) 

# ======================================================================================================================
### Data Prep
# ======================================================================================================================

wuenic_master <- wuenic_master %>% 
  mutate(ISOCountryCode = toupper(ISOCountryCode)) %>%
  filter(ISOCountryCode == Current_ISO3) %>% 
  filter(Year >= 2000, Year <= rev_yr)

# cty_outline_shp <- cty_outline_shp %>%
#   st_make_valid() %>%
#   st_simplify(dTolerance = 0.01) %>%
#   st_transform(crs = st_crs(4326))

# ======================================================================================================================
### Intro Slides
# ======================================================================================================================

# cty_map <- ggplot(cty_outline_shp) +
#   geom_sf(fill = "#1CABE2", color = "white", linewidth = 0.5) +
#   theme_void()

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

# ======================================================================================================================
### Schedule & Stockouts
# ======================================================================================================================

## ---------------------------------------------------------------------------------------------------------------------
### Create schedule table using tbl_schedule.R
## ---------------------------------------------------------------------------------------------------------------------

# read in wiise schedule and run tbl_schedule.R
wiise_schedule <- read_excel(str_glue("{wiisefolder}/output/wiise-schedule-dta_{rev_yr}rev.xlsx"))
source(file.path(PrjDir, "R/tbl_schedule.R"))

## ---------------------------------------------------------------------------------------------------------------------
### Create vaccine introductions table using tbl_intro.R
## ---------------------------------------------------------------------------------------------------------------------

wiise_intro <- read_excel(str_glue("{wiisefolder}/output/wiise-intro-dta_{rev_yr}rev.xlsx"))

## tbl_intro :: year of vaccine introduction ----
tbl_intro_r <- wiise_intro %>%
  mutate(iso3c = tolower(iso3c)) %>%
  filter(iso3c == x) %>%
  select(vaccine_name, contains("year_intro")) %>%
  arrange(vaccine_name) %>%
  rename(Vaccine = vaccine_name, 
         `National introduction` = year_intro_national,
         `Partial introduction` = year_intro_partial,
         `Risk groups` = year_intro_risk_groups,
         `Risk areas` = year_intro_risk_area) %>%
  # remove columns that are completey empty
  select(where(~ !all(is.na(.) | . == ""))) %>%
  # make numeric values character and set NA to ""
  mutate(across(where(is.numeric), ~ as.character(.)))

if (no_data(tbl_intro_r) == FALSE) {
  source(file.path(PrjDir, "R/tbl_intro.R"))
}

## ---------------------------------------------------------------------------------------------------------------------
### Create vaccine stockouts table using tbl_stock.R
## ---------------------------------------------------------------------------------------------------------------------

wiise_stockouts <- read_excel(str_glue("{wiisefolder}/output/wiise-stock-dta_{rev_yr}rev.xlsx"))

# latest revision complete
wueniclatestrev <- read_rds(here(str_glue(file.path(RevDir, "/unicef-products/{type}/01_wuenic_dataset-prep/clean_wuenic_MASTER_{rev_yr}rev.rds")))) %>%
  mutate(country = case_when(iso3c == "bol" ~ "Bolivia",
                             iso3c == "caf" ~ "CAR",
                             iso3c == "cod" ~ "DRC",
                             iso3c == "fsm" ~ "Micronesia",
                             iso3c == "irn" ~ "Iran",
                             iso3c == "png" ~ "PNG",
                             iso3c == "prk" ~ "DPRK",
                             iso3c == "syr" ~ "Syria",
                             iso3c == "lao" ~ "Laos",
                             iso3c == "tza" ~ "Tanzania",
                             iso3c == "ven" ~ "Venezuela",
                             iso3c == "tur" ~ "Turkiye",
                             TRUE ~ country)) %>%
  filter(year >= 2000,
         lvl_2 %in% c("region_unicef_global_old", "region_unicef_ops")) %>%
  label_vals_millions(target, "target_lbl") %>%
  label_vals_millions(vaccinated, "vaccinated_lbl") %>%
  label_vals_millions(unvaccinated, "unvaccinated_lbl") 

wueniclatestrev <- clean_reg_names(wueniclatestrev)  # clean region names

## prep data :: stock-outs ----
# list of wuenic vaccines
wvax <- unique(wueniclatestrev$vaccine) 

# df identifying wuenic vaccines with any stockout (national or subnational)
wuenic_stockouts <- wiise_stockouts %>%
  rename(code = vaccine) %>%
  # clean to match wuenic vaccine names
  mutate(vaccine = tolower(code),
         vaccine = case_when(vaccine == "opv" ~ "pol3",
                             vaccine == "pcv" ~ "pcv3",
                             vaccine == "ipv" ~ "ipv1;ipv2",
                             vaccine == "hepb" ~ "hepbb;hepb3",
                             vaccine == "hib" ~ "hib3",
                             vaccine == "rotavirus" ~ "rotac",
                             vaccine %in% c("measles-rubella (mr)","measles-mumps-rubella (mmr)") ~ "mcv1;mcv2;rcv1",
                             vaccine == "dtp-hib-hepb-ipv" ~ "dtp1;dtp3;hib3;hepb3",
                             vaccine == "dtp-containing vaccine" ~ "dtp1;dtp3;hib3;hepb3",
                             vaccine == "dtp-hepb-ipv" ~ "dtp1;dtp3;ipv1;hepb3;ipv2",
                             vaccine == "dtp-hib-ipv" ~ "dtp1;dtp3;hib3;ipv1;ipv2",
                             vaccine == "dtp-hib-hepb" ~ "dtp1;dtp3;hib3;hepb3",
                             vaccine == "mcv" ~ "mcv1;mcv2",
                             vaccine == "mena" ~ "menga",
                             vaccine == "rcv" ~ "rcv1",
                             TRUE ~ vaccine)) %>%
  # split vaccines into separate columns using ; delimeter
  mutate(v = strsplit(vaccine, ";")) %>%
  unnest_wider(v, names_sep = "_") %>%
  # select relevant columns
  select(iso3c, year, starts_with("v_")) %>%
  # pivot longer
  pivot_longer(-c(iso3c, year), names_to = "v", values_to = "vaccine") %>%
  select(-v) %>%
  filter(vaccine %in% wvax) %>%
  mutate(any_stockout = 1) %>%
  distinct()

## tbl_stock :: wiise stock data ----
source(file.path(PrjDir, "R/tbl_stock.R"))

## ---------------------------------------------------------------------------------------------------------------------
### Check for schedule changes since 2020
## ---------------------------------------------------------------------------------------------------------------------

# # use sched_summary from 01_setup.R, includes schedule info for all countries from 2020-2025
# country_sched <- sched_summary %>%
#   filter(iso3c == Current_ISO3) %>%
#   arrange(vaccinecode, targetpop, year)
# 
# min_yr <- min(country_sched$year, na.rm = TRUE)
# max_yr <- max(country_sched$year, na.rm = TRUE)
# 
# # vaccines that appeared in early years but not in the most recent year
# dropped_notes <- country_sched %>%
#   group_by(vaccinecode, targetpop) %>%
#   summarise(last_yr = max(year), first_yr = min(year), .groups = "drop") %>%
#   filter(last_yr < (max_yr - 1)) %>%
#   mutate(
#     clean_vacc = str_replace_all(vaccinecode, "_", " "),
#     note = paste0(clean_vacc, " last recorded on schedule in ", last_yr)
#   ) %>%
#   distinct(note) %>%
#   pull(note)
# 
# # changes within vaccines over time
# schedule_changes <- country_sched %>%
#   group_by(vaccinecode, targetpop) %>%
#   mutate(
#     prev_doses    = lag(n_scheduled_doses),
#     prev_ages     = lag(ages_administered),
#     is_new        = is.na(prev_doses) & year > min_yr,
#     doses_changed = !is.na(prev_doses) & (n_scheduled_doses != prev_doses),
#     ages_changed  = !is.na(prev_ages) & (ages_administered != prev_ages),
#     any_change    = is_new | doses_changed | ages_changed
#   ) %>%
#   filter(any_change) %>%
#   mutate(
#     clean_vacc  = str_replace_all(vaccinecode, "_", " "),
#     change_note = case_when(
#       is_new ~ paste0(clean_vacc, " introduced in ", year),
#       doses_changed & ages_changed ~ paste0(
#         clean_vacc, ": doses changed from ", prev_doses, " to ", n_scheduled_doses,
#         ", ages from ", prev_ages, " to ", ages_administered, " (", year, ")"),
#       doses_changed ~ paste0(
#         clean_vacc, ": number of doses changed from ", prev_doses, " to ", n_scheduled_doses, " (", year, ")"),
#       ages_changed ~ paste0(
#         clean_vacc, ": age schedule changed from '", prev_ages, "' to '", ages_administered, "' (", year, ")")
#     )
#   ) %>%
#   ungroup() %>%
#   distinct(change_note) %>%
#   pull(change_note) %>%
#   na.omit()
# 
# all_changes <- c(dropped_notes, schedule_changes)
# 
# if (length(all_changes) == 0) {
#   schedule_change_note <- paste0("No changes to the ", CountryName, " vaccination schedule were recorded between ", min_yr, " and ", max_yr, ".")
# } else {
#   schedule_change_note <- c(
#     paste0("Schedule changes recorded for ", CountryName, " since ", min_yr, ":"),
#     paste0("  \u2022 ", sort(all_changes))
#   )
# }
# 
# cat(schedule_change_note, sep = "\n")

## ---------------------------------------------------------------------------------------------------------------------
### Harmonize vaccine codes between most recent schedule and admin data
## ---------------------------------------------------------------------------------------------------------------------

cty_admin_data <- admin_data %>% 
  filter(iso3c == Current_ISO3, year == max_yr)

# define components
penta3_components <- c("DTP3", "HEPB3", "HIB3")

# validate that the penta components have the same numerator before combining
mismatches <- cty_admin_data %>%
  filter(vaccinecode %in% penta3_components) %>%
  group_by(iso3c, year) %>% 
  summarise(unique_counts = n_distinct(reportedNum), .groups = "drop") %>%
  filter(unique_counts > 1)

if (nrow(mismatches) > 0) {
  stop("Validation Failed: numerator is inconsistent across Penta3 components.")
}

# combine
cty_admin_clean <- cty_admin_data %>%
  mutate(vaccine_group = case_when(
    vaccinecode %in% penta3_components ~ "PENTA3",
    vaccinecode == "DTP1" ~ "PENTA1",
    TRUE ~ vaccinecode
  )) %>%
  group_by(
    iso3c, country, year, type, vaccine_group, 
    live_births, surviving_infants, comment, updated, 
    updatedBy, first_commit_dt, commit_dt, is_update
  ) %>%
  summarise(
    reportedNum = first(reportedNum),
    reportedDenom = first(reportedDenom),
    coverage = first(coverage),
    .groups = "drop"
  ) %>%
  rename(vaccinecode = vaccine_group) %>%
  relocate(vaccinecode, .after = country)


## ---------------------------------------------------------------------------------------------------------------------
### Flag any vaccines in the most recent schedule that have zero reported admin data
## ---------------------------------------------------------------------------------------------------------------------

# function to clean vaccine codes for comparison
clean_vaccine_code <- function(x) {
  
  # remove trailing dose numbers, make uppersase
  base <- str_remove(x, "\\d+$") %>% toupper()
  
  # add synonyms
  synonyms <- c(
    "POL"       = "OPV",
    "ROTAC"     = "ROTAVIRUS_",
    "ROTAVIRUS" = "ROTAVIRUS_",
    "PENTA"     = "DTP"
  )
  
  # if the base matches a synonym, replace it; otherwise keep base
  if (base %in% names(synonyms)) {
    return(synonyms[base])
  } else {
    return(base)
  }
}

# extract unique codes
vaccines_in_schedule <- unique(sched_summary_recent_cty$vaccinecode)
admin_vaccines <- unique(cty_admin_clean$vaccinecode)

# apply cleaning
vaccines_in_schedule_base <- unname(sapply(vaccines_in_schedule, clean_vaccine_code))
admin_vaccines_base <- unname(sapply(admin_vaccines, clean_vaccine_code))

# identify missing vaccines
missing_admin <- vaccines_in_schedule[!(vaccines_in_schedule_base %in% admin_vaccines_base)]

# print check result
if (length(missing_admin) == 0) {
  cat("Success: All vaccines in the schedule are present in the administrative data.\n")
} else {
  missing_list <- paste(missing_admin, collapse = ", ")
  cat(paste0("Check Required: The following vaccines from the schedule are missing in the admin data: ", missing_list, ".\n"))
}

# ======================================================================================================================
### Coverage
# ======================================================================================================================

## ---------------------------------------------------------------------------------------------------------------------
### Coverage Heatmap
## ---------------------------------------------------------------------------------------------------------------------

wuenic_dta <- wueniclatestrev %>% 
  filter(lvl_2 == "region_unicef_ops", iso3c == x, year >= 2000)

# HPV data
hpv_dta <- read_excel(str_glue('{wd}/HPV estimates/{hpv_rev_yr} revision/final/hpv_estimates_wuenic{hpv_rev_yr}rev.xlsx')) %>%
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

hpv <- hpv_dta %>%
  filter(lvl_2 == "region_unicef_ops",
         iso3c == x,
         year >= 2000)

## plt_all_vax_heatmap :: heatmap ----
# includes stock-out data
source(file.path(PrjDir, "R/all_vax_heatmap.R"))

## ---------------------------------------------------------------------------------------------------------------------
### Clean the admin data that includes all years
## ---------------------------------------------------------------------------------------------------------------------

cty_admin_data_all <- admin_data %>% 
  filter(iso3c == Current_ISO3) %>% 
  arrange(vaccinecode, year)

# validate that the penta components have the same numerator before combining
mismatches_all <- cty_admin_data_all %>%
  filter(vaccinecode %in% penta3_components) %>%
  group_by(iso3c, year) %>%
  summarise(unique_counts = n_distinct(reportedNum), .groups = "drop") %>%
  filter(unique_counts > 1)

if (nrow(mismatches_all) > 0) {
  stop("Validation Failed: numerator is inconsistent across Penta3 components.")
}

# combine
cty_admin_data_all <- cty_admin_data_all %>%
  mutate(vaccine_group = case_when(
    vaccinecode %in% penta3_components ~ "PENTA3",
    TRUE ~ vaccinecode
  )) %>%
  group_by(
    iso3c, country, year, type, vaccine_group,
    live_births, surviving_infants, comment, updated,
    updatedBy, first_commit_dt, commit_dt, is_update
  ) %>%
  summarise(
    reportedNum = first(reportedNum),
    reportedDenom = first(reportedDenom),
    coverage = first(coverage),
    .groups = "drop"
  ) %>%
  rename(vaccinecode = vaccine_group) %>%
  relocate(vaccinecode, .after = country)

## ---------------------------------------------------------------------------------------------------------------------
### Plot 1: Year-to-year % change in doses flagged if over % threshold (pct_threshold)
## ---------------------------------------------------------------------------------------------------------------------

plot_data <- cty_admin_data_all %>%
  arrange(vaccinecode, year) %>%
  group_by(vaccinecode) %>%
  mutate(
    prev_num = lag(reportedNum),
    # calculate % change (e.g., 0.25 for 25% increase)
    pct_change = (reportedNum - prev_num) / prev_num,
    # flag if the absolute change is over the threshold
    is_flagged = abs(pct_change) > pct_threshold
  ) %>%
  # remove the first year for each vaccine (since it has no previous year to compare)
  #filter(!is.na(pct_change)) %>%
  ungroup() %>% 
  filter(!vaccinecode == "YFV")

# create the plot (version without coverage line)
plt_perc_change_no_line <- ggplot(plot_data, aes(x = year, y = pct_change)) +
  # add horizontal lines for the threshold
  geom_hline(yintercept = c(pct_threshold, -pct_threshold), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "black") +
  geom_segment(aes(x = year, xend = year, y = 0, yend = pct_change), color = "grey60") +
  geom_point(aes(color = is_flagged), size = 3) +
  facet_wrap(~vaccinecode, scales = "free_y") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(
    # keep a grid line/break for every single year
    breaks = seq(min(plot_data$year), max(plot_data$year), by = 1),
    # only label every other year (e.g., 2020, 2022, 2024)
    labels = function(x) ifelse(x %% 2 == 0, as.character(x), "")
  ) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  theme_minimal() +
  labs(
    title = paste("Year-to-Year % Change in Reported Doses, ", CountryName),
    subtitle = paste0("Red Points Indicate Changes Exceeding +/- ", pct_threshold * 100, "%"),
    x = "Year",
    y = "% Change from Previous Year",
    color = "Threshold Exceeded"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "#0083CF"),
    strip.text = element_text(color = "white", face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
  )

# second version with coverage line
# calculate a global scale factor based on the MAX of all doses to ensure the secondary axis remains consistent across all facets
global_max_doses <- max(plot_data$reportedNum, na.rm = TRUE)
global_max_pct <- max(abs(plot_data$pct_change), na.rm = TRUE)
global_scale_factor <- global_max_pct / global_max_doses

plt_perc_change_line <- ggplot(plot_data, aes(x = year)) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  geom_hline(yintercept = c(pct_threshold, -pct_threshold), linetype = "dashed", color = "red", alpha = 0.3) +
  
  # layer 1: % change in numerator
  geom_segment(aes(x = year, xend = year, y = 0, yend = pct_change), color = "grey80") +
  geom_point(aes(y = pct_change, color = is_flagged), size = 2.5) +
  
  # layer 2: raw numerator
  geom_line(aes(y = reportedNum * global_scale_factor, group = vaccinecode), color = "#0083CF", size = 0.8, alpha = 0.6) +
  
  facet_wrap(~vaccinecode, scales = "fixed") + 
  scale_y_continuous(name = "Year-to-Year % Change", labels = scales::percent_format(),
                     sec.axis = sec_axis(~ . / global_scale_factor, name = "Raw Doses Reported", labels = scales::label_number(scale_cut = scales::cut_short_scale()))) +
  scale_x_continuous(breaks = seq(min(plot_data$year), max(plot_data$year), by = 1), labels = function(x) ifelse(x %% 2 == 0, as.character(x), "")) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  theme_minimal() +
  theme(axis.line.y.left = element_line(color = "black"),
        axis.line.y.right = element_line(color = "#0083CF"),
        axis.text.y.right = element_text(color = "#0083CF", size = 8),
        axis.title.y.right = element_text(color = "#0083CF", size = 9),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6),
        panel.spacing = unit(0.8, "lines"),
        legend.position = "bottom",
        strip.background = element_rect(fill = "#0083CF"),
        strip.text = element_text(color = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11)) +
  labs(
    title = paste("Year-to-Year % Change in Reported Doses, ", CountryName),
    subtitle = paste0("Red flags > ", pct_threshold*100, "% change."),
    x = "Year"
  )

## ---------------------------------------------------------------------------------------------------------------------
### Plot 2: Outlier detection
## ---------------------------------------------------------------------------------------------------------------------

# process admin data for outlier detection
admin_flags <- cty_admin_data_all %>%
  arrange(vaccinecode, year) %>%
  group_by(vaccinecode) %>%
  mutate(
    # year-to-year % change in doses
    prev_num = lag(reportedNum),
    pct_change = (reportedNum - prev_num) / prev_num,
    flag_change = abs(pct_change) > pct_threshold,
    
    # coverage > 100% flag
    flag_over100 = coverage > 100,
    
    # sudden drops to near zero (reporting failure check)
    # flags if current is < 5% of previous year but previous wasn't zero
    flag_zero_drop = (reportedNum < (0.05 * prev_num)) & prev_num > 0
  ) %>%
  ungroup()

# # 3. prepare wuenic data for comparison (2025 only)
# wuenic_2025 <- wuenic_master %>%
#   mutate(vaccine = toupper(vaccine)) %>% 
#   select(iso3c, year, vaccine, wuenic_cov = coverage) %>%
#   # handle naming differences to match your admin codes
#   mutate(vaccine = case_when(
#     vaccine == "DTP3" ~ "PENTA3",
#     vaccine == "DTP1" ~ "PENTA1",
#     TRUE ~ vaccine
#   ))
# 
# # 4. merge admin data with wuenic estimates
# # joining by vaccinecode and year (wuenic only has 2025 here)
# comparison_df <- admin_flags %>%
#   left_join(wuenic_2025, by = c("iso3c", "vaccinecode" = "vaccine")) %>%
#   mutate(
#     # flag large gaps between admin and wuenic (e.g., > 10 percentage points)
#     flag_wuenic_gap = ifelse(year == 2025, abs(coverage - wuenic_cov) > 10, FALSE)
#   )
# 
# # 5. create visualization with flagged years highlighted
# # creating a combined flag for the plot
# plot_data <- comparison_df %>%
#   mutate(any_flag = flag_change | flag_over100 | flag_zero_drop | flag_wuenic_gap)
# 
# ggplot(plot_data, aes(x = year, y = coverage, group = vaccinecode)) +
#   geom_line(color = "grey70", size = 1) +
#   # highlight flagged points in red
#   geom_point(aes(color = any_flag, size = any_flag)) +
#   # add wuenic 2025 benchmark as a blue diamond
#   geom_point(aes(y = wuenic_cov), shape = 18, size = 4, color = "#008CBA", na.rm = TRUE) +
#   facet_wrap(~vaccinecode, scales = "free_y") +
#   scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
#   scale_size_manual(values = c("FALSE" = 1.5, "TRUE" = 3)) +
#   theme_minimal() +
#   labs(
#     title = "ethiopia vaccine coverage trends (2020-2025)",
#     subtitle = "red points indicate outliers/flags; blue diamonds are 2025 wuenic estimates",
#     x = "year",
#     y = "reported admin coverage (%)",
#     color = "flagged issue",
#     size = "flagged issue"
#   ) +
#   theme(legend.position = "bottom")

# # 6. summary table of flagged issues
# flag_summary <- plot_data %>%
#   filter(any_flag == TRUE) %>%
#   select(year, vaccinecode, coverage, pct_change, flag_over100, flag_wuenic_gap)
# 
# print(flag_summary)

# ======================================================================================================================
### Outlier Detection
# ======================================================================================================================

# ======================================================================================================================
### Denominator Checks
# ======================================================================================================================
         
# ======================================================================================================================
### Dropout & Vaccine Relationships
# ======================================================================================================================

# ======================================================================================================================
### Missing Data
# ======================================================================================================================

# ======================================================================================================================
### Admin vs. Official Comparison
# ======================================================================================================================

# ======================================================================================================================
### Data Prep
# ======================================================================================================================


