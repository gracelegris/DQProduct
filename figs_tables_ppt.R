# ==========================================================================================================================================
# Script Name: Figures and Tables for 2025 WUENIC Data Quality Powerpoint
# Author: Grace Legris, glegris@unicef.org
# ==========================================================================================================================================

## -----------------------------------------SET PARAMETERS--------------------------------------------------------------
CountryName <- "Ethiopia"
Current_ISO3 <- "ETH"
pct_threshold <- 0.10 # threshold for flagging large year-to-year changes in coverage data (e.g., 20% change)
second_pct_threshold <- 0.30
rev_yr       <- 2025
hpv_rev_yr   <- 2024
wpp_rev_yr   <- 2024
min_yr_plots <- 2000
type         <- "dummy"
x <- "eth"
## ---------------------------------------------------------------------------------------------------------------------

source("user_profiles.R")
#source("01_setup.R") 
source(file.path(PrjDir, "R/label_vals.R"))
source(file.path(PrjDir, "R/funcs.R"))

# data cleaning functions from subnational folder
source(paste0(SubnatFuncDir, "/user_functions_outliers.R"))
source(paste0(SubnatFuncDir, "/data_quality_funcs.R"))

source_colors <- c("WHO/UNICEF" = "#0083CF", "Admin" = "#6A1E74", "Official (Government Estimate)" = "#80BD41", "Survey" = "#FFC20E")
flag_colors <- c("FALSE" = "black", "TRUE" = "red")

## ---------------------------------------------------------------------------------------------------------------------

# wpp (world population prospects) data for denominators
#denominators <- read.csv(file.path(DummyUtils, "WPP_denoms_WPP2024.csv"))

# read in geojson with country outlines
# cty_outline_shp <- st_read(file.path(DummyUtils, "adm0.geojson")) %>% 
#   filter(ISO3 == Current_ISO3)

# read in languages file
# language <- read_excel(file.path(DummyUtils, "Languages.xlsx")) %>% 
#   filter(ISO3_code == Current_ISO3) %>% 
#   pull(Language)

# set system environment to display in the country's language (for use in plots)
#Sys.setenv(LANGUAGE = language) 

# ======================================================================================================================
### Data Prep
# ======================================================================================================================

# master wuenic dataset
wuenic_master <- read.csv(file.path(DummyDataDir, "wuenic-master_2025rev.csv"))

wuenic_master <- wuenic_master %>% 
  mutate(ISOCountryCode = toupper(ISOCountryCode)) %>%
  filter(ISOCountryCode == Current_ISO3) %>% 
  filter(Year >= min_yr_plots, Year <= rev_yr) %>% 
  mutate(Vaccine = case_when(Vaccine == "hepbb" ~ "HepBB",
                             Vaccine == "hepb3" ~ "HepB3",
                             Vaccine == "hib3" ~ "Hib3",
                             Vaccine == "rotac" ~ "RotaC",
                             Vaccine == "menga" ~ "MengA",
                             Vaccine == "hpvc" ~ "HPVc",
                             TRUE ~ toupper(Vaccine))) %>% 
  rename(Admin = AdministrativeCoverage, Official = GovernmentEstimate, Survey = SurveyInformation) %>% 
  relocate(Admin, .after = WUENIC) %>% relocate(Official, .after = Admin) %>% relocate(Survey, .after = Official) %>% 
  relocate(ChildrenVaccinated, .after = Survey) %>% relocate(ChildrenInTarget, .after = ChildrenVaccinated)

wuenic_master$Vaccine <- factor(wuenic_master$Vaccine, levels = c("BCG", "HepBB", "DTP1", "DTP3", "Hib3", "HepB3", "PCVC", "RotaC", 
                                                                  "POL3", "IPV1", "IPVC", "MCV1", "RCV1", "MCV2", "YFV", "MengA","HPVc"))

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

# # regional info file
regional_info <- read_csv(file.path(DummyUtils, "regional-groups_2026-release.csv")) %>%
  filter(iso3c == Current_ISO3)

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
  # remove columns that are completely empty
  select(where(~ !all(is.na(.) | . == ""))) %>%
  # make numeric values character and set NA to ""
  mutate(across(where(is.numeric), ~ as.character(.)))

if (no_data(tbl_intro_r) == FALSE) {
  source(file.path(PrjDir, "R/tbl_intro.R"))
}

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

# add stockout markers to wuenic_master
wuenic_stockouts <- wuenic_stockouts %>% 
  mutate(iso3c = toupper(iso3c)) %>% 
  mutate(vaccine = case_when(vaccine == "hepbb" ~ "HepBB",
                             vaccine == "hepb3" ~ "HepB3",
                             vaccine == "hib3" ~ "Hib3",
                             vaccine == "rotac" ~ "RotaC",
                             vaccine == "menga" ~ "MengA",
                             vaccine == "hpvc" ~ "HPVc",
                             TRUE ~ toupper(vaccine)))
wuenic_master <- wuenic_master %>% 
  left_join(wuenic_stockouts %>% select(iso3c, year, vaccine, any_stockout), 
            by = c("ISOCountryCode" = "iso3c", "Year" = "year", "Vaccine" = "vaccine")) %>%
  mutate(any_stockout = if_else(is.na(any_stockout), 0, any_stockout))

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
### Summary Table: Coverage by Vaccine and Source
## ---------------------------------------------------------------------------------------------------------------------

# data prep
recent_years <- sort(unique(wuenic_master$Year), decreasing = TRUE)[1:n_years_comparison_plot]

min_yr <- min(recent_years)
max_yr <- max(recent_years)

summary_long <- wuenic_master %>%
  filter(Year %in% recent_years) %>%
  select(Vaccine, Year, WUENIC, Admin, Official) %>%
  pivot_longer(cols = c(WUENIC, Admin, Official), names_to = "Source", values_to = "Coverage") %>%
  mutate(
    label = ifelse(is.na(Coverage), "—", sprintf("%.0f", Coverage)),
    Source  = factor(Source, levels = c("WUENIC", "Admin", "Official")),
    Year    = factor(Year),
    Vaccine = factor(Vaccine, levels = rev(c("BCG", "HepBB", "DTP1", "DTP3", "Hib3", "HepB3",
                                             "PCVC", "RotaC", "POL3", "IPV1", "IPVC",
                                             "MCV1", "RCV1", "MCV2", "YFV", "MengA", "HPVc")))
  ) %>% 
  mutate(Source = recode(Source, "WUENIC" = "WHO/UNICEF", "Official" = "Official (Government Estimate)"))

max_cov <- max(summary_long$Coverage, na.rm = TRUE)

# plot
plt_summary_table <- ggplot(summary_long, aes(x = Year, y = Vaccine, fill = Coverage)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = label, color = "white"), size = 4, fontface = "bold") +
  scale_fill_gradientn(
    colours  = c('#B50800', '#E2231A', '#F26A21', '#FFC20E', '#80BD41', '#00833D'),
    values   = scales::rescale(c(0, 57, 67, 77, 87, 94, 100)),
    guide    = "colorbar",
    limits   = c(0, 100),
    oob      = scales::squish,
    breaks   = c(0, 59, 69, 79, 89, 100),
    labels   = c("0", "60", "70", "80", "90", "100"),
    na.value = "#F2F2F2"
  ) +
  scale_color_identity() +
  facet_wrap(~Source, ncol = 3) +
  theme_minimal() +
  labs(
    title = paste0("Coverage Trends by Vaccine and Source, ", CountryName, ", ", min_yr, "–", max_yr),
    x = "Year", y = NULL,
    fill = "Coverage (%)",
    caption = "WHO/UNICEF HPV coverage estimates not included in table."
  ) +
  theme(
    plot.title       = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle    = element_text(hjust = 0.5, size = 11),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text       = element_text(color = "white", face = "bold", size = 11),
    panel.grid       = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y      = element_text(size = 9),
    panel.border     = element_rect(color = "grey80", fill = NA),
    legend.position  = "bottom",
    legend.key.width = unit(2, "cm")
  )
plt_summary_table

## ---------------------------------------------------------------------------------------------------------------------
### Coverage Line Plots for all 4 Indicators (WUENIC, Admin, Official, Survey)
## ---------------------------------------------------------------------------------------------------------------------

all_line_data <- wuenic_master %>% 
  arrange(Vaccine, Year) %>%
  group_by(Vaccine) %>%
  pivot_longer(cols = c(WUENIC, Admin, Official, Survey), names_to = "Source", values_to = "Coverage") %>%
  ungroup() %>% 
  mutate(Source = recode(Source, "WUENIC" = "WHO/UNICEF", "Official" = "Official (Government Estimate)"))

min_cov <- floor(min(all_line_data$Coverage, na.rm = TRUE) / 25) * 25
max_cov <- ceiling(max(all_line_data$Coverage, na.rm = TRUE) / 10) * 10

source_colors_line <- c("WHO/UNICEF" = "blue", "Admin" = "#e93626", 
                        "Official (Government Estimate)"= "lightpink", "Survey" = "green")

source_shapes <- c("WHO/UNICEF" = 16, "Admin" = 8, "Official (Government Estimate)" = 16, "Survey" = 17)

plt_all_vax_line <- ggplot(all_line_data, aes(x = Year, y = Coverage, color = Source, shape = Source)) +
  geom_line(data = all_line_data %>% filter(Source == "WHO/UNICEF"), size = 0.8, alpha = 0.7) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  geom_point(data = all_line_data %>% filter(Source == "Admin"), size = 2) +
  geom_point(data = all_line_data %>% filter(Source %in% c("Survey", "Official (Government Estimate)")), size = 3, alpha = 0.7) +
  facet_wrap(~Vaccine, scales = "fixed") +
  scale_color_manual(values = source_colors_line) +
  scale_shape_manual(values = source_shapes) +
  scale_x_continuous(breaks = seq(min(all_line_data$Year), max(all_line_data$Year), by = 2)) +
  scale_y_continuous(limits = c(min_cov, max_cov), breaks = seq(min_cov, max_cov, by = 25)) +
  theme_minimal() +
  labs(
    title  = paste0("Coverage Trends by Vaccine and Source, ", CountryName, ", ", min_yr_plots, "–", rev_yr),
    x = "Year", y = "Coverage (%)", color = "Data Source", shape = "Data Source"
  ) +
  theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.ticks.x     = element_line(color = "black"),
    axis.ticks.y     = element_line(color = "black"),
    panel.border     = element_rect(color = "black", fill = NA, size = 0.6),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text       = element_text(color = "white", face = "bold"),
    plot.title       = element_text(hjust = 0.5, size = 14)
  )
plt_all_vax_line

## ---------------------------------------------------------------------------------------------------------------------
### DTP1, DTP3, MCV 1: Coverage Line Plots for all 4 Indicators (WUENIC, Admin, Official, Survey)
## ---------------------------------------------------------------------------------------------------------------------

selected_line_data <- all_line_data %>% 
  filter(Vaccine %in% c("DTP1", "DTP3", "MCV1"))

min_cov <- floor(min(selected_line_data$Coverage, na.rm = TRUE) / 25) * 25
max_cov <- ceiling(max(selected_line_data$Coverage, na.rm = TRUE) / 10) * 10

plt_selected_vax_line <- ggplot(selected_line_data, aes(x = Year, y = Coverage, color = Source, shape = Source)) +
  geom_line(data = selected_line_data %>% filter(Source == "WHO/UNICEF"), size = 0.8, alpha = 0.7) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  geom_point(data = selected_line_data %>% filter(Source == "Admin"), size = 2) +
  geom_point(data = selected_line_data %>% filter(Source %in% c("Survey", "Official (Government Estimate)")), size = 3, alpha = 0.7) +
  facet_wrap(~Vaccine, scales = "fixed") +
  scale_color_manual(values = source_colors_line) +
  scale_shape_manual(values = source_shapes) +
  scale_x_continuous(breaks = seq(min(selected_line_data$Year), max(selected_line_data$Year), by = 2)) +
  scale_y_continuous(limits = c(min_cov, max_cov), breaks = seq(min_cov, max_cov, by = 25)) +
  theme_minimal() +
  labs(
    title  = paste0("Coverage Trends by Vaccine and Source, ", CountryName, ", ", min_yr_plots, "–", rev_yr),
    x = "Year", y = "Coverage (%)", color = "Data Source", shape = "Data Source"
  ) +
  theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.ticks.x     = element_line(color = "black"),
    axis.ticks.y     = element_line(color = "black"),
    panel.border     = element_rect(color = "black", fill = NA, size = 0.6),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text       = element_text(color = "white", face = "bold"),
    plot.title       = element_text(hjust = 0.5, size = 14)
  )

## ---------------------------------------------------------------------------------------------------------------------
### Coverage > 100% and sudden drops flagged by vaccine and year (Admin data)
## ---------------------------------------------------------------------------------------------------------------------

combined_flag_data <- wuenic_master %>%
  arrange(Vaccine, Year) %>%
  group_by(Vaccine) %>%
  mutate(
    prev_coverage   = lag(Admin),
    flag_over100    = Admin > 100,
    flag_large_chng = abs(Admin - prev_coverage) > 10,
    flag_type = case_when(
      flag_over100    ~ ">100%",
      flag_large_chng ~ "±10pp change",
      TRUE            ~ "Normal"
    )
  ) %>%
  ungroup()

plt_coverage_flags <- ggplot(combined_flag_data, aes(x = Year, y = Admin)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  geom_line(color = "grey60") +
  geom_point(aes(color = flag_type), size = 2.5) +
  facet_wrap(~Vaccine) +
  scale_y_continuous(breaks = anchor_breaks <- pretty(combined_flag_data$Admin)) +
  scale_x_continuous(breaks = seq(min(combined_flag_data$Year), max(combined_flag_data$Year), by = 2)) +
  scale_color_manual(values = c("Normal" = "black", ">100%" = "#FFC20E", "±10pp change" = "#E2231A")) +
  theme_minimal() +
  labs(
    title = paste0("Admin Coverage Flags by Vaccine and Year, ", CountryName, ", ", min_yr_plots, "–", rev_yr),
    subtitle = paste0("Orange = coverage > 100%  |  Red = ± ", pct_threshold*100, "pp change from previous year"),
    x = "Year", y = "Admin Coverage (%)",
    color = "Flag Type"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text = element_text(color = "white", face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )

## ---------------------------------------------------------------------------------------------------------------------
### DTP1, DTP3, and MCV1: Coverage > 100% and sudden drops flagged by year (Admin data)
## ---------------------------------------------------------------------------------------------------------------------

selected_flag_data <- combined_flag_data %>% 
  filter(Vaccine %in% c("DTP1", "DTP3", "MCV1"))

plt_selected_coverage_flags <- ggplot(selected_flag_data, aes(x = Year, y = Admin)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  geom_line(color = "grey60") +
  geom_point(aes(color = flag_type), size = 2.5) +
  facet_wrap(~Vaccine) +
  scale_y_continuous(breaks = anchor_breaks <- pretty(selected_flag_data$Admin)) +
  scale_x_continuous(breaks = seq(min(selected_flag_data$Year), max(selected_flag_data$Year), by = 2)) +
  scale_color_manual(values = c("Normal" = "black", ">100%" = "#FFC20E", "±10pp change" = "#E2231A")) +
  theme_minimal() +
  labs(
    title = paste0("DTP1, DTP3, and MCV1 Admin Coverage Flags by Year, ", CountryName, ", ", min_yr_plots, "–", rev_yr),
    subtitle = paste0("Orange = coverage > 100%  |  Red = ± ", pct_threshold*100, "pp change from previous year"),
    x = "Year", y = "Admin Coverage (%)",
    color = "Flag Type"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
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
  mutate(
    gap = Admin - WUENIC,
    flag_large_gap = abs(gap) > pct_threshold*100
  ) %>% 
  rename(`WHO/UNICEF` = WUENIC)

plt_admin_vs_wuenic <- ggplot(gap_data, aes(x = Year)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  geom_line(aes(y = `WHO/UNICEF`, linetype = "WHO/UNICEF"), color = source_colors["WHO/UNICEF"], linewidth = 0.9) +
  geom_line(aes(y = Admin, linetype = "Admin"), color = source_colors["Admin"], linewidth = 0.9) +
  geom_point(data = gap_data %>% filter(flag_large_gap == TRUE), aes(y = Admin), color = "red", size = 2) +
  facet_wrap(~Vaccine) +
  scale_linetype_manual(values = c("WHO/UNICEF" = "solid", "Admin" = "solid")) +
  scale_y_continuous(
    limits = c(floor(min(gap_data$Admin,   na.rm = TRUE) / 25) * 25,
               ceiling(max(gap_data$Admin, na.rm = TRUE) / 10) * 10),
    breaks = seq(0, 100, by = 25)
  ) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  scale_x_continuous(breaks = seq(min(gap_data$Year), max(gap_data$Year), by = 2)) +
  theme_minimal() +
  labs(
    title    = paste0("Admin vs. WHO/UNICEF Coverage Estimate, ", CountryName, ", ", min_yr_plots, "–", rev_yr),
    subtitle = paste0("Red points indicate gaps > ", pct_threshold * 100, " percentage points"),
    x = "Year", y = "Coverage (%)",
    color = "Large Gap", linetype = "Source"
  ) +
  theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.ticks.x     = element_line(color = "black"),
    axis.ticks.y     = element_line(color = "black"),
    panel.border     = element_rect(color = "black", fill = NA, size = 0.6),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text       = element_text(color = "white", face = "bold"),
    plot.title       = element_text(hjust = 0.5, size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 11)
  )
plt_admin_vs_wuenic

## ---------------------------------------------------------------------------------------------------------------------
### Plot: Same admin vs. wuenic plot but with second threshold
## ---------------------------------------------------------------------------------------------------------------------

gap_data2 <- wuenic_master %>%
  mutate(
    gap = Admin - WUENIC,
    flag_large_gap = abs(gap) > second_pct_threshold*100
  ) %>% 
  rename(`WHO/UNICEF` = WUENIC)

plt_admin_vs_wuenic2 <- ggplot(gap_data2, aes(x = Year)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  geom_line(aes(y = `WHO/UNICEF`, linetype = "WHO/UNICEF"), color = source_colors["WHO/UNICEF"], linewidth = 0.9) +
  geom_line(aes(y = Admin, linetype = "Admin"), color = source_colors["Admin"], linewidth = 0.9) +
  geom_point(data = gap_data2 %>% filter(flag_large_gap == TRUE), aes(y = Admin), color = "red", size = 2) +
  facet_wrap(~Vaccine) +
  scale_linetype_manual(values = c("WHO/UNICEF" = "solid", "Admin" = "solid")) +
  scale_y_continuous(
    limits = c(floor(min(gap_data2$Admin,   na.rm = TRUE) / 25) * 25,
               ceiling(max(gap_data2$Admin, na.rm = TRUE) / 10) * 10),
    breaks = seq(0, 100, by = 25)
  ) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  scale_x_continuous(breaks = seq(min(gap_data2$Year), max(gap_data2$Year), by = 2)) +
  theme_minimal() +
  labs(
    title    = paste0("Admin vs. WHO/UNICEF Coverage Estimate, ", CountryName, ", ", min_yr_plots, "–", rev_yr),
    subtitle = paste0("Red points indicate gaps > ", second_pct_threshold * 100, " percentage points"),
    x = "Year", y = "Coverage (%)",
    color = "Large Gap", linetype = "Source"
  ) +
  theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.ticks.x     = element_line(color = "black"),
    axis.ticks.y     = element_line(color = "black"),
    panel.border     = element_rect(color = "black", fill = NA, size = 0.6),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text       = element_text(color = "white", face = "bold"),
    plot.title       = element_text(hjust = 0.5, size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 11)
  )
plt_admin_vs_wuenic2

## ---------------------------------------------------------------------------------------------------------------------
### Plot: Admin coverage vs Official coverage — large gaps flagged (using wiise_admin_official)
## ---------------------------------------------------------------------------------------------------------------------

gap_data_2 <- wuenic_master %>%
  mutate(
    gap = Admin - Official,
    flag_large_gap = abs(gap) > pct_threshold*100
  ) %>% 
   rename(`Official (Government Estimate)` = Official)

plt_admin_vs_official <- ggplot(gap_data_2, aes(x = Year)) +
  geom_line(aes(y = `Official (Government Estimate)`, linetype = "Official (Government Estimate)"), color = source_colors["Official (Government Estimate)"], linewidth = 0.9) +
  geom_line(aes(y = Admin,    linetype = "Admin"),    color = source_colors["Admin"],    linewidth = 0.9) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  geom_point(data = gap_data_2 %>% filter(flag_large_gap == TRUE), aes(y = Admin), color = "red", size = 2) +
  facet_wrap(~Vaccine) +
  scale_y_continuous(
    limits = c(floor(min(gap_data_2$Admin,   na.rm = TRUE) / 25) * 25,
               ceiling(max(gap_data_2$Admin, na.rm = TRUE) / 10) * 10),
    breaks = seq(0, 100, by = 25)
  ) +
  scale_linetype_manual(values = c("Official (Government Estimate)" = "solid", "Admin" = "solid")) +
  scale_x_continuous(breaks = seq(min(gap_data_2$Year), max(gap_data_2$Year), by = 2)) +
  theme_minimal() +
  labs(
    title = paste0("Admin vs. Official (Government Estimate) Coverage, ", CountryName, ", ", min_yr_plots, "–", rev_yr),
    subtitle = paste0("Red points indicate gaps > ", pct_threshold * 100, " percentage points"),
    x = "Year", y = "Coverage (%)",
    linetype = "Source"
  ) +
  theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.ticks.x     = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    panel.border     = element_rect(color = "black", fill = NA, size = 0.6),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text       = element_text(color = "white", face = "bold"),
    plot.title       = element_text(hjust = 0.5, size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 11)
  )
plt_admin_vs_official


## ---------------------------------------------------------------------------------------------------------------------
### Plot: Year-to-year % change in numerator (# children vaccinated) flagged if over % threshold (pct_threshold)
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
  ungroup()

# 1. Ensure we have valid numbers for scaling
# Calculate global values, ensuring we handle NAs and empty data
max_doses <- max(plot_data$ChildrenVaccinated, na.rm = TRUE)
min_doses <- min(plot_data$ChildrenVaccinated, na.rm = TRUE)
max_pct   <- max(abs(plot_data$pct_change), na.rm = TRUE)

# Prevent division by zero if all pct_changes are 0
if(max_pct == 0 || is.na(max_pct)) max_pct <- 0.1 

# 2. Simplified Scaling Logic
# We want to map [min_doses, max_doses] into a primary axis space 
# that is roughly [-max_pct, max_pct]
scale_factor <- (max_doses - min_doses) / (2 * max_pct)
# If doses are all the same, scale_factor might be 0. Avoid this:
if(scale_factor == 0) scale_factor <- 1 

# prep stockout shading data
stockout_data <- plot_data %>%
  filter(any_stockout == 1) %>%
  select(Vaccine, Year) %>%
  distinct() %>%
  mutate(xmin = Year - 0.5, xmax = Year + 0.5)


  plt_perc_change_line <- ggplot(plot_data, aes(x = Year)) +
    
    # stockout shading (add first so it sits behind everything else)
    geom_rect(data = stockout_data,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "orange", alpha = 0.2, inherit.aes = FALSE) +
    
    geom_hline(yintercept = 0, color = "black", size = 0.5) +
    geom_hline(yintercept = c(pct_threshold, -pct_threshold),
               linetype = "dashed", color = "red", alpha = 1) +
    
    # layer 1: % change (Primary Axis)
    geom_segment(aes(x = Year, xend = Year, y = 0, yend = pct_change), color = "grey60") +
    geom_point(aes(y = pct_change, color = is_flagged), size = 2.5) +
    
    # layer 2: raw numerator (Scaled to Primary Axis)
    geom_line(aes(y = (ChildrenVaccinated - min_doses) / scale_factor - max_pct, group = Vaccine),
              color = "#00833D", linewidth = 0.8, alpha = 0.6) +
    
    facet_wrap(~Vaccine, scales = "fixed") +
    scale_y_continuous(
      name   = "Year-to-Year % Change",
      limits = c(-max_pct * 1.2, max_pct * 1.2),
      labels = scales::percent_format(accuracy = 1),
      sec.axis = sec_axis(
        trans  = ~ (. + max_pct) * scale_factor + min_doses,
        name   = "# Children Vaccinated",
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      )
    ) +
    scale_x_continuous(
      breaks = seq(min(plot_data$Year), max(plot_data$Year), by = 1),
      labels = function(x) {ifelse(x %% 2 == 0, as.character(x), "")}
    ) +
    scale_color_manual(
      values = c("FALSE" = "black", "TRUE" = "red"),
      labels = c("FALSE" = paste0("Change ≤ ±", pct_threshold * 100, "%"), "TRUE" = paste0("Change > ±", pct_threshold * 100, "%")),
      na.translate = FALSE
    ) +
    scale_fill_manual(values = c("Stockout" = "orange"), labels = c("Stockout" = "Vaccine Stockout")) +
    guides(color = guide_legend(title = NULL), fill  = guide_legend(title = NULL)) +
    theme_minimal() +
    theme(
      axis.line.y.left   = element_line(color = "black"),
      axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
      axis.ticks.x       = element_line(color = "black"),
      axis.ticks.y = element_line(color = "black"),
      axis.line.y.right  = element_line(color = "#00833D"),
      axis.text.y.right  = element_text(color = "#00833D", size = 8),
      axis.title.y.right = element_text(color = "#00833D", size = 9),
      panel.grid.minor   = element_blank(),
      panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.6),
      legend.position    = "bottom",
      strip.background   = element_rect(fill = "#0083CF"),
      strip.text         = element_text(color = "white", face = "bold"),
      plot.title         = element_text(hjust = 0.5, size = 14),
      plot.subtitle      = element_text(hjust = 0.5, size = 11)
    ) +
    labs(
      title = paste0("Year-to-Year % Change in Numerator (# Children Vaccinated), ", CountryName, ", ", min_yr_plots, "–", rev_yr),
      subtitle = paste0("Line shows raw counts; points show % annual change\n", 
                        "Red points indicate changes exceeding +/- ", pct_threshold * 100, "%\n",
                        "% | Orange shading indicates vaccine stockout"),
      x = "Year"
    )

plt_perc_change_line

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
  ungroup()

# define the range for the primary axis (% change)
y_limit <- max(abs(denom_change_data$pct_change_denom), na.rm = TRUE)
if(y_limit == 0 || is.na(y_limit)) y_limit <- 0.1
primary_min <- -y_limit
primary_max <- y_limit

# define the range for the secondary axis (# children)
sec_min <- 0
sec_max <- max(denom_change_data$ChildrenInTarget, na.rm = TRUE)
if(sec_max <= 0 || is.na(sec_max)) sec_max <- 100

# scaling factor for plotting
scale_m <- (sec_max - sec_min) / (primary_max - primary_min)
offset_c <- sec_max - (scale_m * primary_max)

plt_denom_change <- ggplot(denom_change_data, aes(x = Year)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(pct_threshold, -pct_threshold), linetype = "dashed", color = "red", alpha = 0.5) +
  
  # y axis 1: percent change
  geom_segment(aes(x = Year, xend = Year, y = 0, yend = pct_change_denom), color = "grey60") +
  geom_point(aes(y = pct_change_denom, color = flag_denom_change), size = 2.5) +
  
  # y axis 2: raw denominator (scaled to fit primary axis)
  geom_line(aes(y = (ChildrenInTarget - offset_c) / scale_m, group = Vaccine), color = "#00833D", linewidth = 0.8, alpha = 0.6) +
  
  facet_wrap(~Vaccine) +
  scale_y_continuous(name = "% Change from Previous Year", limits = c(primary_min, primary_max), labels = scales::percent_format(),
    sec.axis = sec_axis(
      trans = ~ . * scale_m + offset_c,
      name  = "Raw Children in Target Population",
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    )
  ) +
  scale_x_continuous(breaks = seq(min(denom_change_data$Year), max(denom_change_data$Year), by = 2)) +
  scale_color_manual(
    name = NULL,
    values = c("FALSE" = "black", "TRUE" = "red"),
    labels = c("FALSE" = paste0("Change ≤ ±", pct_threshold * 100, "%"), "TRUE" = paste0("Change > ±", pct_threshold * 100, "%")),
    na.translate = FALSE
  ) + 
  theme_minimal() +
  theme(
    legend.position    = "bottom",
    panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.6),
    strip.background   = element_rect(fill = "#0083CF"),
    strip.text         = element_text(color = "white", face = "bold"),
    axis.text.y.right  = element_text(color = "#00833D"),
    axis.title.y.right = element_text(color = "#00833D"),
    plot.title         = element_text(hjust = 0.5, size = 14),
    plot.subtitle      = element_text(hjust = 0.5, size = 11),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 8)
  ) +
  labs(
    title = paste0("Year-to-Year % Change in Denominator (# Children in Target), ", CountryName, ", ", min_yr_plots, "–", rev_yr),
    subtitle = paste0("Line shows raw counts; points show % annual change\n", 
                      "Red points indicate changes exceeding +/- ", pct_threshold * 100, "%"))
plt_denom_change

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
    title = paste0("Admin Data: Live Births (BCG) vs Surviving Infants (DTP1) Denominator, ", CountryName, ", ", min_yr_plots, "–", rev_yr),
    x = "Year", y = "Target Population",
    color = "Denominator Type"
  ) +
  theme(
    legend.position  = "bottom",
    panel.border     = element_rect(color = "black", fill = NA, size = 0.6),
    plot.title       = element_text(hjust = 0.5, size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 11),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.ticks.x     = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black")
  )

## ---------------------------------------------------------------------------------------------------------------------
### Denom Plot 3: Year-to-year % change for Live Births (BCG) and Surviving Infants (DTP1)
## ---------------------------------------------------------------------------------------------------------------------

denom_types_data <- wuenic_master %>%
  filter(Vaccine %in% c("BCG", "DTP1")) %>%
  mutate(DenomType = case_when(
    Vaccine == "BCG"  ~ "Live Births",
    Vaccine == "DTP1" ~ "Surviving Infants"
  )) %>%
  group_by(DenomType) %>%
  arrange(Year) %>%
  mutate(
    prev_target = lag(ChildrenInTarget),
    pct_change_denom = (ChildrenInTarget - prev_target) / prev_target,
    flag_denom_change = abs(pct_change_denom) > pct_threshold
  ) %>%
  ungroup()

# Calculate scaling for the secondary axis (raw counts)
y_limit_dt <- max(abs(denom_types_data$pct_change_denom), na.rm = TRUE)
if(y_limit_dt == 0 || is.na(y_limit_dt)) y_limit_dt <- 0.1

primary_max_dt <- y_limit_dt
primary_min_dt <- -y_limit_dt
sec_max_dt     <- max(denom_types_data$ChildrenInTarget, na.rm = TRUE)
scale_dt       <- sec_max_dt / (primary_max_dt - primary_min_dt)
offset_dt      <- sec_max_dt - (scale_dt * primary_max_dt)

plt_denom_pct_change <- ggplot(denom_types_data, aes(x = Year)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(pct_threshold, -pct_threshold), linetype = "dashed", color = "red", alpha = 0.5) +
  
  # Bar/Segment for % change
  geom_segment(aes(x = Year, xend = Year, y = 0, yend = pct_change_denom), color = "grey60") +
  geom_point(aes(y = pct_change_denom, color = flag_denom_change), size = 3) +
  
  # Line for raw population (scaled)
  geom_line(aes(y = (ChildrenInTarget - offset_dt) / scale_dt, group = DenomType), 
            color = "#00833D", linewidth = 1, alpha = 0.5) +
  
  facet_wrap(~DenomType) +
  scale_y_continuous(
    name = "% Change from Previous Year", 
    limits = c(primary_min_dt, primary_max_dt), 
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * scale_dt + offset_dt, 
                        name = "Raw Denominator Count",
                        labels = scales::label_number(scale_cut = scales::cut_short_scale()))
  ) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.6),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text = element_text(color = "white", face = "bold"),
    axis.title.y.right = element_text(color = "#00833D"),
    axis.text.y.right = element_text(color = "#00833D"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  ) +
  labs(
    title = paste0("Denominator Stability Check, ", CountryName, ", ", min_yr_plots, "–", rev_yr),
    subtitle = paste0("Line shows raw counts; points show % annual change\n", 
                      "Red points indicate changes exceeding +/- ", pct_threshold * 100, "%"))

plt_denom_pct_change

# ======================================================================================================================
### Dropout & Vaccine Relationships
# ======================================================================================================================

dropout_base <- wuenic_master %>%
  select(Year, Vaccine, Admin, ChildrenVaccinated)

## ---------------------------------------------------------------------------------------------------------------------
### Dropout Plot 1: Dropout rates over time (Penta1→Penta3, Penta1→MCV1, DTP1→DTP3, PCV1→PCV3)
## ---------------------------------------------------------------------------------------------------------------------

# define dropout pairs — only keep pairs where both vaccines exist in the data
dropout_pairs <- list(
  #"Penta1 → Penta3" = c("PENTA1", "PENTA3"),
  #"MCV1 → MCV2"   = c("MCV1", "MCV2"),
  #"IPV1 → IPVC"   = c("IPV1", "IPVC"),
  "DTP1 → DTP3"     = c("DTP1", "DTP3"),
  "DTP1 → MCV1"     = c("DTP1", "MCV1")
  #"PCV1 → PCVC"     = c("PCV1", "PCVC")
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

# Calculate the actual % dropout rate for each pair
dropout_rates <- dropout_long %>%
  select(Year, pair, Vaccine, ChildrenVaccinated) %>%
  tidyr::pivot_wider(names_from = Vaccine, values_from = ChildrenVaccinated) %>%
  group_by(pair) %>%
  group_modify(~ {
    # identify vaccines in this pair based on dropout_pairs list
    v_names <- dropout_pairs[[.y$pair]]
    v1 <- v_names[1]
    v2 <- v_names[2]
    
    # calculate dropout
    .x %>% mutate(
      dropout_pct = ((get(v1) - get(v2)) / get(v1)) * 100,
      # flag negative dropout (where dose 2 > dose 1)
      is_negative = dropout_pct < 0
    )
  }) %>%
  ungroup()

plt_dropout_with_rate <- ggplot() +
  # admin coverage
  geom_line(data = dropout_long, aes(x = Year, y = Admin, color = Vaccine), linewidth = 0.9) +
  geom_point(data = dropout_long, aes(x = Year, y = Admin, color = Vaccine), size = 2) +
  
  # dropout % as bars
  geom_col(data = dropout_rates, aes(x = Year, y = dropout_pct), fill = "grey50", alpha = 0.3, width = 0.5) +
  
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  facet_wrap(~pair, scales = "free_y") +
  scale_y_continuous(
    limits = c(0, ceiling(max(dropout_long$Admin, na.rm = TRUE) / 10) * 10),
    breaks = seq(0, 100, by = 10)
  ) +
  scale_x_continuous(breaks = seq(min(dropout_long$Year), max(dropout_long$Year), by = 2)) +
  scale_color_manual(values = unicef_colors) +
  theme_minimal() +
  labs(
    title = paste0("Key Vaccine Dropout, ", CountryName, ", ", min_yr_plots, "–", rev_yr),
    subtitle = "Lines = Admin Coverage | Bars = Dropout %",
    x = "Year", y = "Admin Coverage / Dropout %",
    caption = "Dropout % = (Dose 1 - Dose 2) / Dose 1 * 100"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "#0083CF"),
    strip.text = element_text(color = "white", face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )

plt_dropout_with_rate

## ---------------------------------------------------------------------------------------------------------------------
### DTP3-PCVC Co-administration Plot
## ---------------------------------------------------------------------------------------------------------------------

selected_coadmin <- wuenic_master %>% 
  filter(Vaccine %in% c("DTP3", "PCVC"))

plt_coadmin_dtp_pcv <- ggplot(selected_coadmin, aes(x = Year, y = Admin, color = Vaccine)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 0.8) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("DTP3" = "#0058AB", "PCVC" = "#E87722")) + 
  scale_y_continuous(limits = c(0, max(c(105, max(selected_coadmin$Admin, na.rm = TRUE)))), 
                     breaks = seq(0, 150, by = 10)) +
  scale_x_continuous(breaks = seq(min(selected_coadmin$Year), max(selected_coadmin$Year), by = 2)) +
  theme_minimal() +
  labs(
    title = paste0("Admin Coverage of DTP3 and PCVC (Co-administered), ", CountryName, ", ", min_yr_plots, "–", rev_yr),
    x = "Year", 
    y = "Admin Coverage (%)",
    color = "Vaccine"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text = element_text(color = "white", face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14),
  )

plt_coadmin_dtp_pcv

## ---------------------------------------------------------------------------------------------------------------------
### Co-administration Plot: Dynamic schedule check for 6- and 14-week vaccines
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
  ggplot(aes(x = Year, y = Admin, color = Vaccine, group = Vaccine)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(min(plot_data$Year), max(plot_data$Year), by = 1)) +
  theme_minimal() +
  scale_color_manual(values = unicef_colors) +
  labs(
    title = paste0("Admin Coverage of Vaccines Administered at 6 Weeks, ", CountryName, ", ", min_yr_plots, "–", rev_yr),
    x = "Year", y = "Admin Coverage (%)", color = "Vaccine"
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
  ggplot(aes(x = Year, y = Admin, color = Vaccine, group = Vaccine)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(min(plot_data$Year), max(plot_data$Year), by = 1)) +
  theme_minimal() +
  scale_color_manual(values = unicef_colors) +
  labs(
    title = paste0("Admin Coverage of Vaccines Administered at 14 Weeks, ", CountryName, ", ", min_yr_plots, "–", rev_yr),
    x = "Year", y = "Admin Coverage (%)", color = "Vaccine"
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
### Numerator check plot using the co-administered vaccines
# ======================================================================================================================

plot_data_6wk <- plot_data %>% filter(Vaccine %in% vax_6w)
plot_data_14wk <- plot_data %>% filter(Vaccine %in% vax_14w)

make_numerator_plot <- function(plot_data, time_point) {
  time_plot_vaccines <- plot_data %>% distinct(Vaccine) %>% pull(Vaccine)
  plot_data$Vaccine <- factor(plot_data$Vaccine, levels = time_plot_vaccines)
  
  numerator_plot <- ggplot(plot_data, aes(x = Year)) +
    
    # stockout shading (add first so it sits behind everything else)
    geom_rect(data = stockout_data,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "orange", alpha = 0.2, inherit.aes = FALSE) +
    
    geom_hline(yintercept = 0, color = "black", size = 0.5) +
    geom_hline(yintercept = c(pct_threshold, -pct_threshold),
               linetype = "dashed", color = "red", alpha = 1) +
    
    # layer 1: % change (Primary Axis)
    geom_segment(aes(x = Year, xend = Year, y = 0, yend = pct_change), color = "grey60") +
    geom_point(aes(y = pct_change, color = is_flagged), size = 2.5) +
    
    # layer 2: raw numerator (Scaled to Primary Axis)
    geom_line(aes(y = (ChildrenVaccinated - min_doses) / scale_factor - max_pct, group = Vaccine),
              color = "#00833D", linewidth = 0.8, alpha = 0.6) +
    
    facet_wrap(~Vaccine, scales = "fixed") +
    scale_y_continuous(
      name   = "Year-to-Year % Change",
      limits = c(-max_pct * 1.2, max_pct * 1.2),
      labels = scales::percent_format(accuracy = 1),
      sec.axis = sec_axis(
        trans  = ~ (. + max_pct) * scale_factor + min_doses,
        name   = "# Children Vaccinated",
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      )
    ) +
    scale_x_continuous(
      breaks = seq(min(plot_data$Year), max(plot_data$Year), by = 1),
      labels = function(x) {ifelse(x %% 2 == 0, as.character(x), "")}
    ) +
    scale_color_manual(
      values = c("FALSE" = "black", "TRUE" = "red"),
      labels = c("FALSE" = paste0("Change ≤ ±", pct_threshold * 100, "%"), "TRUE" = paste0("Change > ±", pct_threshold * 100, "%")),
      na.translate = FALSE
    ) +
    scale_fill_manual(values = c("Stockout" = "orange"), labels = c("Stockout" = "Vaccine Stockout")) +
    guides(color = guide_legend(title = NULL), fill  = guide_legend(title = NULL)) +
    theme_minimal() +
    theme(
      axis.line.y.left   = element_line(color = "black"),
      axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
      axis.ticks.x       = element_line(color = "black"),
      axis.ticks.y = element_line(color = "black"),
      axis.line.y.right  = element_line(color = "#00833D"),
      axis.text.y.right  = element_text(color = "#00833D", size = 8),
      axis.title.y.right = element_text(color = "#00833D", size = 9),
      panel.grid.minor   = element_blank(),
      panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.6),
      legend.position    = "bottom",
      strip.background   = element_rect(fill = "#0083CF"),
      strip.text         = element_text(color = "white", face = "bold"),
      plot.title         = element_text(hjust = 0.5, size = 14),
      plot.subtitle      = element_text(hjust = 0.5, size = 11)
    ) +
    labs(
      title = paste0("Year-to-Year % Change in Numerator (# Children Vaccinated), ", CountryName, ", ", min_yr_plots, "–", rev_yr,
                     "\nVaccines Administered at ", time_point),
      subtitle = paste0("Line shows raw counts; points show % annual change\n", 
                        "Red points indicate changes exceeding +/- ", pct_threshold * 100, "%\n",
                        "% | Orange shading indicates vaccine stockout"),
      x = "Year"
    )
  return(numerator_plot)
}

plt_numerator_6wk <- make_numerator_plot(plot_data_6wk, "6 weeks")
plt_numerator_14wk <- make_numerator_plot(plot_data_14wk, "14 weeks")

# ======================================================================================================================
### Missing Data Heatmap (Admin Data)
# ======================================================================================================================

# 1. Define the mapping
vaccine_map <- c(
  "MEASLES"     = "MCV1",
  "BCG"         = "BCG",
  "DTWPHIBHEPB" = "DTP3", 
  "PCV"         = "PCVC",
  "Rotavirus"   = "RotaC",
  "OPV"         = "POL3",
  "IPV"         = "IPV1"
)

# 2. Identify vaccines that ARE in the schedule
scheduled_vaccines <- wiise_schedule %>%
  filter(iso3c == tolower(Current_ISO3)) %>%
  mutate(mapped_name = vaccine_map[vaccine]) %>%
  filter(!is.na(mapped_name)) %>%
  pull(mapped_name) %>%
  unique()

# Add DTP1 manually if DTP3 is present
if("DTP3" %in% scheduled_vaccines) scheduled_vaccines <- c(scheduled_vaccines, "DTP1")

# 1. Rebuild wiise_long with smarter logic
wiise_long <- wiise_intro %>%
  filter(iso3c == Current_ISO3) %>% 
  select(vaccine, `2000`:`2024`) %>%
  pivot_longer(
    cols = `2000`:`2024`, 
    names_to = "Year", 
    values_to = "In_Schedule"
  ) %>%
  mutate(Year = as.numeric(Year)) %>%
  rename(Vaccine = vaccine) %>%
  # Ensure all combos exist
  full_join(
    expand_grid(
      Year = 2000:2024, 
      Vaccine = unique(c(scheduled_vaccines, "MCV2", "YFV", "HepBB", "BCG", "DTP1", "DTP3"))
    ), 
    by = c("Year", "Vaccine")
  ) %>%
  distinct(Year, Vaccine, .keep_all = TRUE) %>%
  # CHANGE HERE: Logic to handle introduction years correctly
  mutate(
    In_Schedule = case_when(
      # If the original data says "Yes", keep it
      In_Schedule == "Yes" ~ "Yes",
      
      # If it's IPV1 specifically, only force "Yes" from 2015 onwards
      Vaccine == "IPV1" & Year >= 2015 ~ "Yes",
      
      # For foundational vaccines (BCG, DTP), force "Yes" for all years
      Vaccine %in% c("BCG", "DTP1", "DTP3", "POL3", "MCV1") ~ "Yes",
      
      # Otherwise, if it was in our scheduled_vaccines list, 
      # but not foundational, default to the original value or "No"
      TRUE ~ coalesce(In_Schedule, "No")
    )
  )

# 2. Join and calculate status (same as before)
heatmap_data_wuenic <- wuenic_master %>%
  select(Year, Vaccine, Admin) %>%
  complete(Year = 2000:rev_yr, Vaccine = unique(wiise_long$Vaccine)) %>%
  left_join(wiise_long, by = c("Year", "Vaccine")) %>%
  mutate(
    status = case_when(
      In_Schedule == "Yes" & (is.na(Admin) | Admin == "") ~ "Missing",
      In_Schedule == "Yes" & !is.na(Admin) ~ "Present",
      TRUE ~ "Not Introduced"
    )
  )

# filter out vaccines that are NEVER in schedule and NEVER have data
heatmap_data_wuenic <- heatmap_data_wuenic %>%
  group_by(Vaccine) %>%
  filter(
    any(In_Schedule == "Yes") | any(!is.na(Admin))
  ) %>%
  ungroup() %>% 
  filter(Vaccine != "HPV") # filter out HPV as we only have WUENIC HPV data

# set most recent year vaccines to included
heatmap_data_wuenic <- heatmap_data_wuenic %>% 
  mutate(status = case_when(Year == rev_yr & !is.na(Admin) ~ "Present",  TRUE ~ status),
         status = case_when(Year == rev_yr &  is.na(Admin) ~ "Missing",  TRUE ~ status),
         In_Schedule = case_when(Year == rev_yr ~ "Yes", TRUE ~ In_Schedule),
         status = factor(status, levels = c("Present", "Missing", "Not Introduced")))

# order for heatmap
heatmap_data_wuenic$Vaccine <- factor(heatmap_data_wuenic$Vaccine, levels = c("BCG", "HepBB", "DTP1", "DTP3", "Hib3", "HepB3", "PCVC", "RotaC", 
                                                                  "POL3", "IPV1", "IPVC", "MCV1", "RCV1", "MCV2", "YFV", "MengA","HPVc"))

# heatmap
plt_missing_heatmap <- ggplot(heatmap_data_wuenic, aes(x = factor(Year), y = fct_rev(Vaccine), fill = status)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = c("Missing" = "#E2231A", "Present" = "#00833D", "Not Introduced" = "white"),
    na.value = "white" # for years not in the intro table
  ) +
  theme_minimal() +
  labs(
    title = paste0("Admin Data Availability Heatmap, ", CountryName, ", ", min_yr_plots, "–", rev_yr),
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

plt_missing_heatmap
