# ==========================================================================================================================================
# Script Name: Data Setup for Data Quality Product
# Author: Grace Legris, glegris@unicef.org
# ==========================================================================================================================================

# most recent year of data available
rev_yr <- 2025

# country names/regions and list of unicef/wuenic countries (195)
reg_ref <- read_csv(file.path(DummyUtils, paste0('regional-groups_', rev_yr + 1, '-release.csv'))) %>% 
  select(iso3c, country, region_unicef_ops, wuenic)
wuenic_list <- pull(filter(reg_ref, !is.na(wuenic)), iso3c)

# read in data
wiise_admin_official <- readRDS(file.path(DataDir, paste0("wiise_admin_official_", rev_yr + 1, "-release.rds")))
wiise_sched <- readRDS(file.path(DataDir, "wiise_schedule.rds"))
wuenic_control <- readRDS(file.path(DataDir, "wuenic_control.rds"))
v_ref_pop <- readRDS(file.path(DataDir, "v_ref_populations.rds"))

## ---------------------------------------------------------------------------------------------------------------------
### Data Prep
## ---------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(janitor)

# ── 1. wiise_admin_official ────────────────────────────────────────────────────
# rename country to variables
wiise_admin_official <- wiise_admin_official %>%
  rename(iso3c = country, year = annum)

# wide version: one row per country/vaccine/year with admin & official side by side (for the "are admin and official the same?" check)
wiise_wide <- wiise_admin_official %>%
  select(iso3c, vaccine, year, type, coverage, reportedDenom, reportedNum, comment) %>%
  pivot_wider(
    names_from = type,
    values_from = c(coverage, reportedDenom, reportedNum, comment),
    names_sep = "_"
  ) %>%
  mutate(admin_eq_official = coverage_admin == coverage_official) %>% 
  left_join(reg_ref %>% select(iso3c, country), by = "iso3c") %>% 
  relocate(country, .after = iso3c) %>% 
  arrange(country)

# ── 2. wiise_sched ────────────────────────────────────────────────────────────
# fix case (iso3c is lowercase, uppercase to match others)
# parse commit_dt to proper datetime
# keep only national, routine schedule (exclude risk groups etc.) for the DQ context
wiise_sched <- wiise_sched %>%
  mutate(iso3c = toupper(iso3c)) %>% 
  filter(geoarea == "NATIONAL")

# one row per country/vaccine/year showing number of scheduled doses
sched_summary <- wiise_sched %>%
  group_by(iso3c, year, vaccinecode, targetpop) %>%
  summarise(n_scheduled_doses = max(schedulerounds, na.rm = TRUE), 
            ages_administered = paste(na.omit(ageadministered), collapse = ", "),
            .groups = "drop")

# filter schedule df to unicef/wuenic countries and add country name
sched_summary <- sched_summary %>%
  filter(iso3c %in% wuenic_list) %>% 
  left_join(reg_ref %>% select(iso3c, country), by = "iso3c") %>% 
  relocate(country, .after = iso3c) %>% 
  arrange(country)

# filter schedules to most recent year provided by each country
sched_summary <- sched_summary %>%
  group_by(iso3c) %>%
  filter(year == max(year)) %>%
  ungroup()

# ── 3. wuenic_control ─────────────────────────────────────────────────────────
# keep only columns useful for DQ report, filter to rev_yr range
wuenic_control_clean <- wuenic_control %>%
  select(iso3c = country, year = annum, vaccinecode = vaccine__CODE, wuenicReq, showReported,
         comment, ageAvg, globalAvg, estEqOtherVac) %>%
  mutate(iso3c = toupper(iso3c)) %>%
  filter(year >= 2010, wuenicReq == TRUE)  %>%  # after 2010, only where WUENIC estimate is required
  left_join(reg_ref %>% select(iso3c, country), by = "iso3c") %>% 
  relocate(country, .after = iso3c) %>% 
  arrange(country)

# ── 4. v_ref_pop ──────────────────────────────────────────────────────────────
# filter to BOTH genders only to avoid double counting
# separate births (BCG denominator) and surviving infants (DTP/Penta denominator)
v_ref_pop_clean <- v_ref_pop %>%
  clean_names() %>%
  filter(gender == "BOTH", year >= 2010) %>%
  rename(iso3c = country) %>% 
  left_join(reg_ref %>% select(iso3c, country), by = "iso3c") %>% 
  relocate(country, .after = iso3c) %>% 
  arrange(country) %>% 
  filter(iso3c %in% wuenic_list)

ref_births <- v_ref_pop_clean %>%
  filter(age_group == "BIRTH") %>%
  select(iso3c, country, year, live_births = value) %>%
  arrange(country)

ref_surviving <- v_ref_pop_clean %>%
  filter(age_group == "SURVIVING_INFANT") %>%
  select(iso3c, country, year, surviving_infants = value) %>%
  arrange(country)

ref_pop <- full_join(ref_births, ref_surviving, by = c("iso3c", "country", "year")) %>%
  arrange(country)

# ── 5. master analysis dataset ────────────────────────────────────────────────
# admin data only joined with reference populations for denominator checks
admin_data <- wiise_admin_official %>%
  filter(type == "admin") %>%
  rename(vaccinecode = vaccine) %>%
  left_join(ref_pop, by = c("iso3c", "year")) %>%
  left_join(wuenic_control_clean %>% select(iso3c, year, vaccinecode, wuenicReq, comment_wuenic = comment),
            by = c("iso3c", "year", "vaccinecode")) %>% 
  relocate(country, .after = iso3c) %>% 
  relocate(live_births, surviving_infants, .after = coverage)

# check that admin_data doesn't report any countries not in wuenic_list
admin_codes <- admin_data %>% select(iso3c) %>% distinct()
if (any(!admin_codes$iso3c %in% wuenic_list)) {
  warning("Admin data contains countries not in wuenic_list: ", paste(admin_codes$iso3c[!admin_codes$iso3c %in% wuenic_list], collapse = ", "))
} else {
  message("All countries in admin_data are in wuenic_list.")
}
