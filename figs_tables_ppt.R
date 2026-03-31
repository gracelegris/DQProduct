# ==========================================================================================================================================
# Script Name: Figures and Tables for 2025 WUENIC Data Quality Powerpoint
# Author: Grace Legris, glegris@unicef.org
# ==========================================================================================================================================

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

source("user_profiles.R")
#source("01_setup.R") 
source(file.path(PrjDir, "R/label_vals.R"))
source(file.path(PrjDir, "R/funcs.R"))

## ---------------------------------------------------------------------------------------------------------------------

# master wuenic dataset
wuenic_master <- read.csv(file.path(DummyDataDir, "wuenic-master_2025rev.csv"))

# data cleaning functions from subnational folder
source(paste0(SubnatFuncDir, "/user_functions_outliers.R"))
source(paste0(SubnatFuncDir, "/data_quality_funcs.R"))

# wpp (world population prospects) data for denominators
#denominators <- read.csv(file.path(DummyUtils, "WPP_denoms_WPP2024.csv"))

# read in geojson with country outlines
# cty_outline_shp <- st_read(file.path(DummyUtils, "adm0.geojson")) %>% 
#   filter(ISO3 == Current_ISO3)

# regional info file
regional_info <- read_csv(file.path(DummyUtils, "regional-groups_2026-release.csv")) %>% 
  filter(iso3c == Current_ISO3)

# read in languages file
# language <- read_excel(file.path(DummyUtils, "Languages.xlsx")) %>% 
#   filter(ISO3_code == Current_ISO3) %>% 
#   pull(Language)

# set system environment to display in the country's language (for use in plots)
#Sys.setenv(LANGUAGE = language) 

# ======================================================================================================================
### Data Prep
# ======================================================================================================================

wuenic_master <- wuenic_master %>% 
  mutate(ISOCountryCode = toupper(ISOCountryCode)) %>%
  filter(ISOCountryCode == Current_ISO3) %>% 
  filter(Year >= 2000, Year <= rev_yr) %>% 
  mutate(Vaccine = case_when(Vaccine == "hepbb" ~ "HepBB",
                             Vaccine == "hepb3" ~ "HepB3",
                             Vaccine == "hib3" ~ "Hib3",
                             Vaccine == "rotac" ~ "RotaC",
                             Vaccine == "menga" ~ "MengA",
                             Vaccine == "hpvc" ~ "HPVc",
                             TRUE ~ toupper(Vaccine)))

wuenic_master$Vaccine <- factor(wuenic_master$Vaccine, levels = c("BCG", "HepBB", "DTP1", "DTP3", "Hib3", "HepB3", "PCVC", "RotaC", "POL3", "IPV1", "IPVC", "MCV1", "RCV1", "MCV2", "YFV", "MengA","HPVc"))

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

# ======================================================================================================================
### Coverage & Outlier Detection
# ======================================================================================================================

## ---------------------------------------------------------------------------------------------------------------------
### Coverage Heatmap (WUENIC data)
## ---------------------------------------------------------------------------------------------------------------------

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

hpv <- hpv_dta %>%
  filter(lvl_2 == "region_unicef_ops", iso3c == x, year >= 2000)

## plt_all_vax_heatmap :: heatmap ----
# includes stock-out data
source(file.path(PrjDir, "R/all_vax_heatmap.R"))

## ---------------------------------------------------------------------------------------------------------------------
### Plot: Coverage > 100% and sudden drops flagged by vaccine and year (Admin data)
## ---------------------------------------------------------------------------------------------------------------------

combined_flag_data <- wuenic_master %>%
  #filter(!is.na(AdministrativeCoverage)) %>%
  arrange(Vaccine, Year) %>%
  group_by(Vaccine) %>%
  mutate(
    prev_coverage = lag(AdministrativeCoverage),
    flag_over100  = AdministrativeCoverage > 100,
    flag_zero_drop = (AdministrativeCoverage < 5) & (AdministrativeCoverage >= 5),
    flag_type = case_when(
      flag_over100   ~ ">100%",
      flag_zero_drop ~ "Near-zero drop",
      TRUE           ~ "Normal"
    )
  ) %>%
  ungroup()

plt_coverage_flags <- ggplot(combined_flag_data, aes(x = Year, y = AdministrativeCoverage)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "#E2231A", alpha = 1) +
  geom_hline(yintercept = 5,   linetype = "dashed", color = "#FFC20E", alpha = 1) +
  geom_line(color = "grey60") +
  geom_point(aes(color = flag_type), size = 2.5) +
  facet_wrap(~Vaccine) +
  scale_y_continuous(breaks = anchor_breaks <- pretty(combined_flag_data$AdministrativeCoverage)) +
  scale_x_continuous(breaks = seq(min(combined_flag_data$Year), max(combined_flag_data$Year), by = 2)) +
  scale_color_manual(values = c("Normal" = "black", ">100%" = "#E2231A", "Near-zero drop" = "#FFC20E")) +
  theme_minimal() +
  labs(
    title = paste("Admin Coverage Flags by Vaccine and Year,", CountryName),
    subtitle = "Red = coverage >100%  |  Orange = sudden drop to <5% from ≥5% prior year  |  Dashed lines mark thresholds",
    x = "Year", y = "Admin Coverage (%)",
    color = "Flag Type"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
    axis.ticks.x = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text = element_text(color = "white", face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )

## ---------------------------------------------------------------------------------------------------------------------
### Plot: Admin coverage vs WUENIC estimate — large gaps flagged (using wuenic_master)
## ---------------------------------------------------------------------------------------------------------------------

gap_data <- wuenic_master %>%
  #filter(!is.na(WUENIC), !is.na(AdministrativeCoverage)) %>%
  mutate(
    gap = AdministrativeCoverage - WUENIC,
    flag_large_gap = abs(gap) > 20
  )

plt_admin_vs_wuenic <- ggplot(gap_data, aes(x = Year)) +
  geom_ribbon(aes(ymin = pmin(AdministrativeCoverage, WUENIC),
                  ymax = pmax(AdministrativeCoverage, WUENIC),
                  fill = flag_large_gap), alpha = 0.2) +
  geom_line(aes(y = WUENIC,                linetype = "WUENIC"), color = "#0083CF", linewidth = 0.9) +
  geom_line(aes(y = AdministrativeCoverage, linetype = "Admin"), color = "black",   linewidth = 0.9) +
  geom_point(aes(y = AdministrativeCoverage, color = flag_large_gap), size = 2) +
  facet_wrap(~Vaccine) +
  scale_y_continuous(limits = c(floor(min(gap_data$AdministrativeCoverage, na.rm = TRUE) / 25) * 25,
                                ceiling(max(gap_data$AdministrativeCoverage, na.rm = TRUE) / 25) * 25),
                     breaks = seq(0, 100, by = 25)) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  scale_fill_manual(values  = c("FALSE" = "grey70", "TRUE" = "red")) +
  scale_linetype_manual(values = c("WUENIC" = "solid", "Admin" = "solid")) +
  scale_x_continuous(breaks = seq(min(gap_data$Year), max(gap_data$Year), by = 2)) +
  theme_minimal() +
  labs(
    title = paste("Admin vs WUENIC Coverage Estimate,", CountryName),
    subtitle = "Red shading indicates gaps > 20 percentage points — suggests potential data quality issues",
    x = "Year", y = "Coverage (%)",
    color = "Large Gap", fill = "Large Gap", linetype = "Source"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
    axis.ticks.x = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text = element_text(color = "white", face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )

## ---------------------------------------------------------------------------------------------------------------------
### Plot: Year-to-year % change in doses flagged if over % threshold (pct_threshold)
## ---------------------------------------------------------------------------------------------------------------------

plot_data <- wuenic_master %>%
  arrange(Vaccine, Year) %>%
  group_by(Vaccine) %>%
  mutate(
    prev_num = lag(ChildrenVaccinated),
    # calculate % change (e.g., 0.25 for 25% increase)
    pct_change = (ChildrenVaccinated - prev_num) / prev_num,
    # flag if the absolute change is over the threshold
    is_flagged = abs(pct_change) > pct_threshold
  ) %>%
  # remove the first year for each vaccine (since it has no previous year to compare)
  #filter(!is.na(pct_change)) %>%
  ungroup()

# create the plot (version without coverage line)
# plt_perc_change_no_line <- ggplot(plot_data, aes(x = Year, y = pct_change)) +
#   # add horizontal lines for the threshold
#   geom_hline(yintercept = c(pct_threshold, -pct_threshold), linetype = "dashed", color = "red", alpha = 0.5) +
#   geom_hline(yintercept = 0, color = "black") +
#   geom_segment(aes(x = Year, xend = Year, y = 0, yend = pct_change), color = "grey60") +
#   geom_point(aes(color = is_flagged), size = 3) +
#   facet_wrap(~Vaccine, scales = "free_y") +
#   scale_y_continuous(labels = percent_format()) +
#   scale_x_continuous(breaks = seq(min(plot_data$Year), max(plot_data$Year), by = 2)) +
#   scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
#   theme_minimal() +
#   labs(
#     title = paste("Year-to-Year % Change in # Children Vaccinated (Admin Data), ", CountryName),
#     subtitle = paste0("Red Points Indicate Changes Exceeding +/- ", pct_threshold * 100, "%"),
#     x = "Year",
#     y = "% Change from Previous Year",
#     color = "Threshold Exceeded"
#   ) +
#   theme(
#     legend.position = "bottom",
#     axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
#     axis.ticks.x = element_line(color = "black"),
#     panel.border = element_rect(color = "black", fill = NA, size = 0.6),
#     strip.background = element_rect(fill = "#0083CF"),
#     strip.text = element_text(color = "white", face = "bold"),
#     plot.title = element_text(hjust = 0.5, size = 14),
#     plot.subtitle = element_text(hjust = 0.5, size = 11),
#   )

# second version with coverage line
# calculate a global scale factor based on the MAX of all doses to ensure the secondary axis remains consistent across all facets
global_max_doses <- max(plot_data$ChildrenVaccinated, na.rm = TRUE)
global_max_pct <- max(abs(plot_data$pct_change), na.rm = TRUE)
global_scale_factor <- global_max_pct / global_max_doses

plt_perc_change_line <- ggplot(plot_data, aes(x = Year)) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  geom_hline(yintercept = c(pct_threshold, -pct_threshold), linetype = "dashed", color = "red", alpha = 1) +
  
  # layer 1: % change in numerator
  geom_segment(aes(x = Year, xend = Year, y = 0, yend = pct_change), color = "grey60") +
  geom_point(aes(y = pct_change, color = is_flagged), size = 2.5) +
  
  # layer 2: raw numerator
  geom_line(aes(y = ChildrenVaccinated * global_scale_factor, group = Vaccine), color = "#0083CF", size = 0.8, alpha = 0.6) +
  
  facet_wrap(~Vaccine, scales = "fixed") +
  coord_cartesian(clip = "off") +
  scale_y_continuous(
    name = "Year-to-Year % Change",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . / global_scale_factor, name = "Raw Doses Reported", 
                        labels = scales::label_number(scale_cut = scales::cut_short_scale()))
  ) +
  scale_x_continuous(
    breaks = seq(min(plot_data_line$Year), max(plot_data_line$Year), by = 1),
    labels = function(x) {ifelse(x %% 2 == 0, as.character(x), "")}
  ) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
    axis.ticks.x = element_line(color = "black"),
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
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  ) +
  labs(
    title = paste("Year-to-Year % Change in # Children Vaccinated (Admin Data),", CountryName),
    subtitle = paste0("Red Points Indicate Changes Exceeding +/- ", pct_threshold * 100, "%"),
    x = "Year"
  )

# ======================================================================================================================
### Denominator Checks
# ======================================================================================================================

## ---------------------------------------------------------------------------------------------------------------------
### Denom Plot 1: Year-to-year admin denominator change flagged if over threshold
## ---------------------------------------------------------------------------------------------------------------------

denom_change_data <- wuenic_master %>%
  group_by(Vaccine) %>%
  mutate(
    prev_target = lag(ChildrenInTarget),
    pct_change_denom = (ChildrenInTarget - prev_target) / prev_target,
    flag_denom_change = abs(pct_change_denom) > pct_threshold
  ) %>%
  #filter(!is.na(pct_change_denom)) %>%
  ungroup()

global_max_doses <- max(denom_change_data$ChildrenInTarget, na.rm = TRUE)
global_max_pct <- max(abs(denom_change_data$pct_change_denom), na.rm = TRUE)
global_scale_factor <- global_max_pct / global_max_doses

plt_denom_change <- ggplot(denom_change_data, aes(x = Year, y = pct_change_denom)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(pct_threshold, -pct_threshold), linetype = "dashed", color = "red", alpha = 0.5) +
  #geom_segment(aes(x = Year, xend = Year, y = 0, yend = pct_change_denom), color = "grey60") +
  #geom_point(aes(color = flag_denom_change), size = 2.5) +
  facet_wrap(~Vaccine) +
  
  # layer 1: % change in denominator
  geom_segment(aes(x = Year, xend = Year, y = 0, yend = pct_change_denom), color = "grey60") +
  geom_point(aes(y = pct_change_denom, color = flag_denom_change), size = 2.5) +
  
  # layer 2: raw denominator
  geom_line(aes(y = ChildrenInTarget * global_scale_factor, group = Vaccine), color = "#0083CF", size = 0.8, alpha = 0.6) +
  
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(
    breaks = seq(min(denom_change_data$Year), max(denom_change_data$Year), by = 2)
  ) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  theme_minimal() +
  labs(
    title    = paste("Year-to-Year % Change in Admin Denominator (Children in Target Population),", CountryName),
    subtitle = paste0("Red points indicate changes exceeding +/- ", pct_threshold * 100, "%"),
    x = "Year", y = "% Change from Previous Year",
    color = "Threshold Exceeded"
  ) +
  theme(
    legend.position  = "bottom",
    panel.border     = element_rect(color = "black", fill = NA, size = 0.6),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text       = element_text(color = "white", face = "bold"),
    plot.title       = element_text(hjust = 0.5, size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 11),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.ticks.x     = element_line(color = "black")
  )

## ---------------------------------------------------------------------------------------------------------------------
### Denom Plot 2: Live births (BCG) vs surviving infants (DTP1) over time
## ---------------------------------------------------------------------------------------------------------------------

# separate birth and surviving infants data from wuenic_master and reshape for plotting
births_vs_si_data <- wuenic_master %>%
  select(Year, Vaccine, ChildrenInTarget) %>%
  filter(Vaccine %in% c("BCG", "DTP1")) %>%
  mutate(target_grp = case_when(
    Vaccine == "BCG"  ~ "Live Births (BCG)",
    Vaccine == "DTP1" ~ "Surviving Infants (DTP1)",
    TRUE ~ NA_character_
  ))

plt_births_vs_si <- ggplot(births_vs_si_data, aes(x = Year, y = ChildrenInTarget, color = target_grp)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  scale_x_continuous(breaks = seq(min(births_vs_si_data$Year), max(births_vs_si_data$Year), by = 1)) +
  scale_color_manual(values = c("Live Births (BCG)" = "#E87722", "Surviving Infants (DTP1)" = "#0083CF")) +
  theme_minimal() +
  labs(
    title    = paste("Admin Data: Live Births (BCG) vs Surviving Infants (DTP1) Denominator,", CountryName),
    subtitle = "Large divergence between the two may indicate inconsistent denominator use",
    x = "Year", y = "Target Population",
    color = "Vaccine", linetype = "Target Group"
  ) +
  theme(
    legend.position  = "bottom",
    panel.border     = element_rect(color = "black", fill = NA, size = 0.6),
    plot.title       = element_text(hjust = 0.5, size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 11),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.ticks.x     = element_line(color = "black")
  )

# ======================================================================================================================
### Dropout & Vaccine Relationships
# ======================================================================================================================

dropout_base <- wuenic_master %>%
  #filter(!is.na(coverage)) %>%
  select(Year, Vaccine, AdministrativeCoverage)

## ---------------------------------------------------------------------------------------------------------------------
### Dropout Plot 1: Dropout rates over time (Penta1→Penta3, Penta1→MCV1, DTP1→DTP3, PCV1→PCV3)
## ---------------------------------------------------------------------------------------------------------------------

# define dropout pairs — only keep pairs where both vaccines exist in the data
dropout_pairs <- list(
  "Penta1 → Penta3" = c("PENTA1", "PENTA3"),
  "MCV1 → MCV2"   = c("MCV1", "MCV2"),
  "IPV1 → IPVC"   = c("IPV1", "IPVC"),
  "DTP1 → DTP3"     = c("DTP1", "DTP3"),
  "PCV1 → PCVC"     = c("PCV1", "PCVC")
)

available_vaccines <- unique(dropout_base$Vaccine)

dropout_long <- purrr::map_dfr(names(dropout_pairs), function(pair_name) {
  vaccines <- dropout_pairs[[pair_name]]
  v1 <- vaccines[1]; v2 <- vaccines[2]
  if (!v1 %in% available_vaccines | !v2 %in% available_vaccines) return(NULL)
  
  dropout_base %>%
    filter(Vaccine %in% c(v1, v2)) %>%
    mutate(pair = pair_name)
})

plt_dropout <- ggplot(dropout_long, aes(x = Year, y = AdministrativeCoverage, color = Vaccine)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~pair, scales = "free_y") +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  scale_x_continuous(
    breaks = seq(min(dropout_long$Year), max(dropout_long$Year), by = 2)
  ) +
  theme_minimal() +
  scale_color_manual(values = unicef_colors) +
  labs(
    title    = paste("Admin Coverage: Dropout Vaccine Pairs,", CountryName),
    subtitle = "Widening gap between lines suggests increasing dropout; crossing lines indicate negative dropout",
    x = "Year", y = "Coverage (%)",
    color = "Vaccine"
  ) +
  theme(
    legend.position  = "bottom",
    panel.border     = element_rect(color = "black", fill = NA, size = 0.6),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text       = element_text(color = "white", face = "bold"),
    plot.title       = element_text(hjust = 0.5, size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 11),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.ticks.x     = element_line(color = "black")
  )

## ---------------------------------------------------------------------------------------------------------------------
### Co-administration Plot: Doses for co-administered vaccines should be similar
## ---------------------------------------------------------------------------------------------------------------------

# 1. Define a mapping between the Schedule Table names and your Dataset names
val_map <- c(
  "BCG" = "BCG", 
  "DTWPHIBHEPB" = "DTP", 
  "PCV" = "PCV", 
  "MEASLES" = "MCV",
  "IPV" = "IPV",
  "OPV" = "OPV"
)

# 2. Refined function to capture the Dose Number
get_coadmin_pairs_with_dose <- function(sched_df, age_target) {
  
  pairs <- sched_df %>%
    # Pivot to look at every dose column
    pivot_longer(cols = `1`:`4`, names_to = "dose_num", values_to = "age") %>%
    filter(age == age_target) %>%
    mutate(
      base_name = val_map[Vaccine],
      # Custom logic for your specific naming conventions:
      final_name = case_when(
        Vaccine == "OPV" ~ paste0("OPV", as.numeric(dose_num)),
        # If it's the last dose for Rota/PCV/IPV, you used "C" in your dataset
        Vaccine %in% c("PCV", "Rotavirus", "IPV") & is.na(lead(age, default = NA)) ~ paste0(base_name, "C"),
        # Otherwise, standard Name + Dose Number
        TRUE ~ paste0(base_name, dose_num)
      )
    ) %>%
    filter(!is.na(base_name)) %>%
    pull(final_name)
  
  return(pairs)
}

# 3. Generate the lists
vax_6w  <- get_coadmin_pairs_with_dose(tbl_schedule_r, "6 weeks")
vax_14w <- get_coadmin_pairs_with_dose(tbl_schedule_r, "14 weeks")
  
# Replace "PCV3" with "PCVC"
vax_14w <- gsub("PCV3", "PCVC", vax_14w)
vax_14w <- gsub("OPV4", "POL4", vax_14w)

coadmin_pairs <- list(
  "6-Week Cluster"  = vax_6w,
  "14-Week Cluster" = vax_14w
)

# Create a single dataframe for all clusters
plot_data <- purrr::map_dfr(names(coadmin_pairs), function(pair_name) {
  vaccines_to_plot <- coadmin_pairs[[pair_name]]
  
  dropout_base %>%
    filter(Vaccine %in% vaccines_to_plot) %>%
    mutate(cluster = pair_name)
})

# --- 6-WEEK PLOT ---
plt_6wk <- plot_data %>% 
  filter(cluster == "6-Week Cluster") %>%
  ggplot(aes(x = Year, y = AdministrativeCoverage, color = Vaccine, group = Vaccine)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(min(plot_data$Year), max(plot_data$Year), by = 1)) +
  theme_minimal() +
  scale_color_manual(values = unicef_colors) +
  labs(
    title = paste("Coverage of Vaccines Administered at 6 Weeks -", CountryName),
    x = "Year", y = "Coverage (%)", color = "Vaccine"
  ) +
  theme(legend.position = "bottom",
        axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
        axis.ticks.x = element_line(color = "black"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6),
        strip.background = element_rect(fill = "#0083CF"),
        strip.text = element_text(color = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 14)
  )

# --- 14-WEEK PLOT ---
plt_14wk <- plot_data %>% 
  filter(cluster == "14-Week Cluster") %>%
  ggplot(aes(x = Year, y = AdministrativeCoverage, color = Vaccine, group = Vaccine)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(min(plot_data$Year), max(plot_data$Year), by = 1)) +
  theme_minimal() +
  scale_color_manual(values = unicef_colors) +
  labs(
    title = paste("Coverage of Vaccines Administered at 14 Weeks -", CountryName),
    x = "Year", y = "Coverage (%)", color = "Vaccine"
  ) +
  theme(legend.position = "bottom",
        axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
        axis.ticks.x = element_line(color = "black"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6),
        strip.background = element_rect(fill = "#0083CF"),
        strip.text = element_text(color = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 14)
  )

# ======================================================================================================================
### Missing Data
# ======================================================================================================================

# ensure all vaccine/year combinations exist and flag missingness (observed vaccines in country only)
heatmap_data_wuenic <- wuenic_master %>%
  select(Year, Vaccine, AdministrativeCoverage) %>%
  mutate(Vaccine = droplevels(Vaccine)) %>% # drop vaccines not in this country's dataset
  # fill in missing combinations only for the remaining vaccines
  complete(Year, Vaccine) %>%
  mutate(
    is_missing = is.na(AdministrativeCoverage),
    status = if_else(is_missing, "Missing", "Present")
  ) %>%
  arrange(Vaccine, Year)

plt_missing_heatmap <- ggplot(heatmap_data_wuenic, aes(x = factor(Year), y = Vaccine, fill = status)) +
  geom_tile(color = "white", size = 0.2) +
  scale_fill_manual(values = c("Missing" = "#E2231A", "Present" = "#00833D")) +
  theme_minimal() +
  labs(
    title = paste("WUENIC Data Availability Heatmap -", CountryName),
    subtitle = "Red indicates missing WUENIC coverage values for that year/vaccine",
    x = "Year", y = "Vaccine",
    fill = "Data Status"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 8),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

# ======================================================================================================================
### Admin vs. Official Comparison
# ======================================================================================================================

# ======================================================================================================================
### Data Prep
# ======================================================================================================================


