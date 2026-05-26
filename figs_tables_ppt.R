# ==========================================================================================================================================
# Script Name: Figures and Tables for 2025 WUENIC Data Quality Powerpoint
# Author: Grace Legris, glegris@unicef.org
# ==========================================================================================================================================

## ---------------------------------------------------------------------------------------------------------------------

source_colors <- c("WHO/UNICEF" = "#0083CF", "Admin" = "#6A1E74", "Official (Government Estimate)" = "#80BD41", "Survey" = "#FFC20E")
flag_colors <- c("FALSE" = "black", "TRUE" = "red")

## ---------------------------------------------------------------------------------------------------------------------

# wpp (world population prospects) data for denominators
#denominators <- read.csv(file.path(DummyUtils, "WPP_denoms_WPP2024.csv"))

# read in geojson with country outlines
# cty_outline_shp <- st_read(file.path(DummyUtils, "adm0.geojson")) %>% 
#   filter(ISO3 == .current_iso3c)

# read in languages file
# language <- read_excel(file.path(DummyUtils, "Languages.xlsx")) %>% 
#   filter(ISO3_code == .current_iso3c) %>% 
#   pull(Language)

# set system environment to display in the country's language (for use in plots)
#Sys.setenv(LANGUAGE = language) 

# ======================================================================================================================
### Intro Slides
# ======================================================================================================================

# regional info file
regional_info <- regional_info %>% filter(iso3c == .current_iso3c)

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
### Coverage & Outlier Detection
# ======================================================================================================================

## ---------------------------------------------------------------------------------------------------------------------
### Coverage Heatmap (WUENIC data)
## ---------------------------------------------------------------------------------------------------------------------

hpv <- hpv_dta %>%
  filter(lvl_2 == "region_unicef_ops", iso3c == x, year >= 2000)

## plt_all_vax_heatmap :: heatmap ----
# includes stock-out data
source(file.path(PrjDir, "R/all_vax_heatmap.R"))

## ---------------------------------------------------------------------------------------------------------------------
### Summary Table: Coverage by Vaccine and Source
## ---------------------------------------------------------------------------------------------------------------------

# data prep
recent_years <- sort(unique(wuenic_master_current$Year), decreasing = TRUE)[1:n_years_comparison_plot]

min_yr <- min(recent_years)
max_yr <- max(recent_years)

summary_long <- wuenic_master_current %>%
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
  geom_text(aes(label = label, color = "white"), size = 4) +
  scale_fill_gradientn(
    colours  = c('#B50800', '#E2231A', '#F26A21', '#FFC20E', '#80BD41', '#00833D'),
    values   = scales::rescale(c(0, 57, 67, 77, 87, 94, 100)),
    guide    = "colorbar",
    limits   = c(0, 100),
    oob      = scales::squish,
    breaks   = c(0, 59, 69, 79, 89, 100),
    labels   = c("0", "60", "70", "80", "90", "100"),
    na.value = "#d3d3d3"
  ) +
  scale_color_identity() +
  facet_wrap(~Source, ncol = 3) +
  theme_minimal() +
  labs(
    title = paste0("Coverage Trends by Vaccine and Source, ", .current_country, ", ", min_yr, "–", max_yr),
    x = "Year", y = NULL,
    fill = "Coverage (%)",
    caption = "WHO/UNICEF HPV coverage estimates not included in table."
  ) +
  theme(
    plot.title       = element_text(hjust = 0.5, size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 11),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text       = element_text(color = "white", face = "bold", size = 11),
    panel.grid       = element_blank(),
    axis.text.x      = element_text(size = 9),
    axis.text.y      = element_text(size = 9),
    panel.border     = element_rect(color = "grey80", fill = NA),
    legend.position  = "bottom",
    legend.key.width = unit(2, "cm")
  )
plt_summary_table

## ---------------------------------------------------------------------------------------------------------------------
### Coverage Line Plots for all 4 Indicators (WUENIC, Admin, Official, Survey)
## ---------------------------------------------------------------------------------------------------------------------

all_line_data <- wuenic_master_current %>% 
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
  geom_point(data = all_line_data %>% filter(Source %in% c("Survey", "Official (Government Estimate)")), size = 2.7, alpha = 0.8) +
  geom_point(data = all_line_data %>% filter(Source == "Admin"), size = 1.7, alpha = 0.9) +
  facet_wrap(~Vaccine, scales = "fixed") +
  scale_color_manual(values = source_colors_line) +
  scale_shape_manual(values = source_shapes) +
  scale_x_continuous(breaks = seq(min(all_line_data$Year), max(all_line_data$Year), by = 2)) +
  scale_y_continuous(limits = c(min_cov, max_cov), breaks = seq(min_cov, max_cov, by = 25), labels = scales::label_number(suffix = "%")) +
  theme_minimal() +
  labs(
    title  = paste0("Coverage Trends by Vaccine and Source, ", .current_country, ", ", min_yr_plots, "–", rev_yr),
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
  geom_point(data = selected_line_data %>% filter(Source %in% c("Survey", "Official (Government Estimate)")), size = 3, alpha = 0.7) +
  geom_point(data = selected_line_data %>% filter(Source == "Admin"), size = 2, alpha = 0.9) +
  facet_wrap(~Vaccine, scales = "fixed") +
  scale_color_manual(values = source_colors_line) +
  scale_shape_manual(values = source_shapes) +
  scale_x_continuous(breaks = seq(min(selected_line_data$Year), max(selected_line_data$Year), by = 2)) +
  scale_y_continuous(limits = c(min_cov, max_cov), breaks = seq(min_cov, max_cov, by = 25), labels = scales::label_number(suffix = "%")) +
  theme_minimal() +
  labs(
    title  = paste0("Coverage Trends by Vaccine and Source, ", .current_country, ", ", min_yr_plots, "–", rev_yr),
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

combined_flag_data <- wuenic_master_current %>%
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
  geom_point(aes(color = flag_type), size = 2.2) +
  facet_wrap(~Vaccine) +
  # make y axis 0 to 110
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 25), labels = scales::label_number(suffix = "%")) +
  scale_x_continuous(breaks = seq(min(combined_flag_data$Year), max(combined_flag_data$Year), by = 2)) +
  scale_color_manual(values = c("Normal" = "black", ">100%" = "#FFC20E", "±10pp change" = "#E2231A")) +
  theme_minimal() +
  labs(
    title = paste0("Admin Coverage Flags by Vaccine and Year, ", .current_country, ", ", min_yr_plots, "–", rev_yr),
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
  scale_y_continuous(breaks = anchor_breaks <- pretty(selected_flag_data$Admin), labels = scales::label_number(suffix = "%")) +
  scale_x_continuous(breaks = seq(min(selected_flag_data$Year), max(selected_flag_data$Year), by = 2)) +
  scale_color_manual(values = c("Normal" = "black", ">100%" = "#FFC20E", "±10pp change" = "#E2231A")) +
  theme_minimal() +
  labs(
    title = paste0("DTP1, DTP3, and MCV1 Admin Coverage Flags by Year, ", .current_country, ", ", min_yr_plots, "–", rev_yr),
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
### Plot: Admin coverage vs WUENIC estimate — large gaps flagged (using wuenic_master_current)
## ---------------------------------------------------------------------------------------------------------------------

gap_data <- wuenic_master_current %>%
  mutate(
    gap = Admin - WUENIC,
    flag_large_gap = abs(gap) > pct_threshold*100
  ) %>% 
  mutate(`WHO/UNICEF` = WUENIC)

plt_admin_vs_wuenic <- ggplot(gap_data, aes(x = Year)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  geom_line(aes(y = `WHO/UNICEF`, linetype = "WHO/UNICEF"), color = source_colors["WHO/UNICEF"], linewidth = 0.9, alpha = 0.7) +
  geom_line(aes(y = Admin, linetype = "Admin"), color = source_colors["Admin"], linewidth = 0.9, alpha = 0.7) +
  #geom_point(data = gap_data %>% filter(flag_large_gap == TRUE), aes(y = Admin), color = "red", size = 1.7) +
  geom_segment(data = gap_data %>% filter(flag_large_gap == TRUE),
               aes(x = Year, xend = Year, y = Admin, yend = `WHO/UNICEF`),
               color = "#ed6a64", linewidth = 0.8, alpha = 0.7) +
  # geom_point(data = gap_data %>% filter(flag_large_gap == TRUE),
  #            aes(y = Admin), color = "#ed6a64", size = 1) +
  # geom_point(data = gap_data %>% filter(flag_large_gap == TRUE),
  #            aes(y = WUENIC), color = "#ed6a64", size = 1) +
  facet_wrap(~Vaccine) +
  scale_linetype_manual(values = c("WHO/UNICEF" = "solid", "Admin" = "solid")) +
  scale_y_continuous(
    limits = c(floor(min(gap_data$Admin,   na.rm = TRUE) / 25) * 25,
               ceiling(max(gap_data$Admin, na.rm = TRUE) / 10) * 10),
    breaks = seq(0, 100, by = 25), , labels = scales::label_number(suffix = "%")
  ) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "#ed6a64")) +
  scale_x_continuous(breaks = seq(min(gap_data$Year), max(gap_data$Year), by = 2)) +
  theme_minimal() +
  labs(
    title    = paste0("Admin vs. WHO/UNICEF Coverage Estimate, ", .current_country, ", ", min_yr_plots, "–", rev_yr),
    subtitle = paste0("Red lines indicate gaps > ", pct_threshold * 100, " percentage points"),
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

gap_data2 <- wuenic_master_current %>%
  mutate(
    gap = Admin - WUENIC,
    flag_large_gap = abs(gap) > second_pct_threshold*100
  ) %>% 
  mutate(`WHO/UNICEF` = WUENIC)

plt_admin_vs_wuenic2 <- ggplot(gap_data2, aes(x = Year)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  geom_line(aes(y = `WHO/UNICEF`, linetype = "WHO/UNICEF"), color = source_colors["WHO/UNICEF"], linewidth = 0.9, alpha = 0.7) +
  geom_line(aes(y = Admin, linetype = "Admin"), color = source_colors["Admin"], linewidth = 0.9, alpha = 0.7) +
  #geom_point(data = gap_data2 %>% filter(flag_large_gap == TRUE), aes(y = Admin), color = "red", size = 1.7) +
  geom_segment(data = gap_data2 %>% filter(flag_large_gap == TRUE),
              aes(x = Year, xend = Year, y = Admin, yend = WUENIC),
              color = "#ed6a64", linewidth = 0.8, alpha = 0.7) +
  # geom_point(data = gap_data2 %>% filter(flag_large_gap == TRUE),
  #            aes(y = Admin), color = "#ed6a64", size = 1) +
  # geom_point(data = gap_data2 %>% filter(flag_large_gap == TRUE),
  #            aes(y = WUENIC), color = "#ed6a64", size = 1) +
  facet_wrap(~Vaccine) +
  scale_linetype_manual(values = c("WHO/UNICEF" = "solid", "Admin" = "solid")) +
  scale_y_continuous(
    limits = c(floor(min(gap_data2$Admin,   na.rm = TRUE) / 25) * 25,
               ceiling(max(gap_data2$Admin, na.rm = TRUE) / 10) * 10),
    breaks = seq(0, 100, by = 25), , labels = scales::label_number(suffix = "%")
  ) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "#ed6a64")) +
  scale_x_continuous(breaks = seq(min(gap_data2$Year), max(gap_data2$Year), by = 2)) +
  theme_minimal() +
  labs(
    title    = paste0("Admin vs. WHO/UNICEF Coverage Estimate, ", .current_country, ", ", min_yr_plots, "–", rev_yr),
    subtitle = paste0("Red lines indicate gaps > ", second_pct_threshold * 100, " percentage points"),
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

gap_data_2 <- wuenic_master_current %>%
  mutate(
    gap = Admin - Official,
    flag_large_gap = abs(gap) > pct_threshold*100
  ) %>% 
   rename(`Official (Government Estimate)` = Official)

plt_admin_vs_official <- ggplot(gap_data_2, aes(x = Year)) +
  geom_line(aes(y = `Official (Government Estimate)`, linetype = "Official (Government Estimate)"), color = source_colors["Official (Government Estimate)"], linewidth = 0.9, alpha = 0.7) +
  geom_line(aes(y = Admin,    linetype = "Admin"), color = source_colors["Admin"], linewidth = 0.9, alpha = 0.7) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  #geom_point(data = gap_data_2 %>% filter(flag_large_gap == TRUE), aes(y = Admin), color = "red", size = 1.7) +
  geom_segment(data = gap_data_2 %>% filter(flag_large_gap == TRUE),
               aes(x = Year, xend = Year, y = Admin, yend = `Official (Government Estimate)`),
               color = "#ed6a64", linewidth = 0.8, alpha = 0.7) +
  # geom_point(data = gap_data_2 %>% filter(flag_large_gap == TRUE),
  #            aes(y = Admin), color = "#ed6a64", size = 1) +
  # geom_point(data = gap_data_2 %>% filter(flag_large_gap == TRUE),
  #            aes(y = `Official (Government Estimate)`), color = "#ed6a64", size = 1) +
  facet_wrap(~Vaccine) +
  scale_y_continuous(
    limits = c(floor(min(gap_data_2$Admin,   na.rm = TRUE) / 25) * 25,
               ceiling(max(gap_data_2$Admin, na.rm = TRUE) / 10) * 10),
    breaks = seq(0, 100, by = 25), , labels = scales::label_number(suffix = "%")
  ) +
  scale_linetype_manual(values = c("Official (Government Estimate)" = "solid", "Admin" = "solid")) +
  scale_x_continuous(breaks = seq(min(gap_data_2$Year), max(gap_data_2$Year), by = 2)) +
  theme_minimal() +
  labs(
    title = paste0("Admin vs. Official (Government Estimate) Coverage, ", .current_country, ", ", min_yr_plots, "–", rev_yr),
    subtitle = paste0("Red lines indicate gaps > ", pct_threshold * 100, " percentage points"),
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

plot_data_numerators_all <- wuenic_master_current %>%
  arrange(Vaccine, Year) %>%
  group_by(Vaccine) %>%
  mutate(
    prev_num   = lag(ChildrenVaccinated),
    pct_change = ((ChildrenVaccinated - prev_num) / prev_num) * 100,
    is_flagged = abs(pct_change) > pct_threshold*100
  ) %>%
  ungroup()

# ── OUTLIER HANDLING ──────────────────────────────────────────────────────────
outlier_threshold <- 500

outliers_removed <- plot_data_numerators_all %>%
  filter(!is.na(pct_change), abs(pct_change) > outlier_threshold) %>%
  select(Vaccine, Year, pct_change, ChildrenVaccinated) %>%
  mutate(pct_change_label = round(pct_change, 1))

plot_data_numerators_all <- plot_data_numerators_all %>%
  mutate(
    pct_change = if_else(abs(pct_change) > outlier_threshold, NA_real_, pct_change),
    is_flagged = if_else(is.na(pct_change), NA, is_flagged)
  )

if (nrow(outliers_removed) > 0) {
  outlier_note <- outliers_removed %>%
    mutate(note = paste0(Vaccine, " (", Year, ": ", pct_change_label, "%)")) %>%
    pull(note) %>%
    paste(collapse = "; ")
  outlier_subtitle <- paste0("\nOutliers removed (|change| > ", outlier_threshold, "%): ", outlier_note)
} else {
  outlier_subtitle <- ""
}

# ── DATA AVAILABILITY CHECK (per vaccine) ────────────────────────────────────
vaccines_with_numerator_data <- plot_data_numerators_all %>%
  group_by(Vaccine) %>%
  summarise(n_valid = sum(!is.na(ChildrenVaccinated)), .groups = "drop") %>%
  filter(n_valid > 1) %>%
  pull(Vaccine)

vaccines_no_numerator_data <- plot_data_numerators_all %>%
  distinct(Vaccine) %>%
  filter(!Vaccine %in% vaccines_with_numerator_data) %>%
  pull(Vaccine)

# ── SCALING (only on vaccines with data) ──────────────────────────────────────
plot_data_for_scaling <- plot_data_numerators_all %>% filter(Vaccine %in% vaccines_with_numerator_data)

max_doses_num <- max(plot_data_for_scaling$ChildrenVaccinated, na.rm = TRUE)
max_pct_num   <- max(abs(plot_data_for_scaling$pct_change), na.rm = TRUE)

if (is.na(max_pct_num) || max_pct_num == 0) max_pct_num <- 0.1

if (max_pct_num > 500) {
  y_max_num <- ceiling(max_pct_num / 100) * 100
  step_size       <- 100
  division_factor <- ((ifelse(y_max_num > 0, y_max_num / step_size, 1)) * 2) + 1
} else if (max_pct_num > 150) {
  y_max_num <- ceiling(max_pct_num / 50) * 50
  step_size       <- 50
  division_factor <- ((ifelse(y_max_num > 0, y_max_num / step_size, 1)) * 2) + 1
} else {
  y_max_num <- ceiling(max_pct_num / 25) * 25
  step_size       <- 25
  division_factor <- ((ifelse(y_max_num > 0, y_max_num / step_size, 1)) * 2) + 1
}

y_min_num <- -y_max_num

scale_factor <- max_doses_num / (y_max_num - y_min_num)
if (is.na(scale_factor) || scale_factor == 0) scale_factor <- 1

# ── PLOT: vaccines WITH sufficient data ───────────────────────────────────────
if (length(vaccines_with_numerator_data) > 0) {
  
  stockout_data <- plot_data_numerators_all %>%
    filter(any_stockout == 1) %>%
    select(Vaccine, Year) %>%
    distinct() %>%
    mutate(xmin_num = Year - 0.5, xmax_num = Year + 0.5)
  
  first_year_w_numerator_data <- plot_data_numerators_all %>%
    filter(Vaccine %in% vaccines_with_numerator_data, !is.na(ChildrenVaccinated)) %>%
    group_by(Vaccine) %>%
    summarise(first_year = min(Year), .groups = "drop")
  
  plot_data_sufficient <- plot_data_numerators_all %>% filter(Vaccine %in% vaccines_with_numerator_data)
  stockout_sufficient  <- stockout_data %>% filter(Vaccine %in% vaccines_with_numerator_data)
  
  plt_perc_change_line <- local({
    
    # freeze all values inside local scope
    frozen_plot_data      <- plot_data_sufficient
    frozen_stockout       <- stockout_sufficient
    frozen_max_doses      <- max_doses_num
    frozen_y_min          <- y_min_num
    frozen_y_max          <- y_max_num
    frozen_scale_factor   <- max_doses_num / (y_max_num - y_min_num)
    frozen_division       <- division_factor
    frozen_pct_threshold  <- pct_threshold
    frozen_year_min       <- min(plot_data_sufficient$Year)
    frozen_year_max       <- max(plot_data_sufficient$Year)
    frozen_title          <- paste0("Year-to-Year % Change in Numerator (# Children Vaccinated), ", .current_country, ", ", min_yr_plots, "–", rev_yr)
    frozen_subtitle       <- paste0("Line shows raw counts; points show % annual change\n",
                                    "Red points indicate changes exceeding +/- ", pct_threshold * 100, "%\n",
                                    "Orange shading indicates vaccine stockout")
    
    ggplot(frozen_plot_data, aes(x = Year)) +
      geom_rect(data = frozen_stockout,
                aes(xmin = xmin_num, xmax = xmax_num, ymin = -Inf, ymax = Inf),
                fill = "orange", alpha = 0.2, inherit.aes = FALSE) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
      geom_hline(yintercept = c(frozen_pct_threshold * 100, -frozen_pct_threshold * 100),
                 linetype = "dashed", color = "red", alpha = 1) +
      geom_segment(aes(x = Year, xend = Year, y = 0, yend = pct_change),
                   color = "grey60", na.rm = TRUE) +
      geom_point(aes(y = pct_change, color = is_flagged), size = 1.75, na.rm = TRUE) +
      geom_line(aes(y = (ChildrenVaccinated / frozen_max_doses) * (frozen_y_max - frozen_y_min) + frozen_y_min,
                    group = Vaccine),
                color = "#00833D", linewidth = 0.8, alpha = 0.6) +
      geom_point(aes(y = (ChildrenVaccinated / frozen_max_doses) * (frozen_y_max - frozen_y_min) + frozen_y_min),
                 color = "#00833D", size = 1.5, na.rm = TRUE) +
      facet_wrap(~Vaccine, scales = "fixed") +
      scale_y_continuous(
        name   = "Year-to-Year % Change",
        limits = c(frozen_y_min, frozen_y_max),
        breaks = seq(frozen_y_min, frozen_y_max, length.out = frozen_division),
        labels = scales::label_number(suffix = "%", accuracy = 1),
        sec.axis = sec_axis(
          trans  = ~ (. - frozen_y_min) * frozen_scale_factor,
          name   = "# Children Vaccinated",
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        )
      ) +
      scale_x_continuous(
        breaks = seq(frozen_year_min, frozen_year_max, by = 1),
        labels = function(x) ifelse(x %% 2 == 0, as.character(x), "")
      ) +
      scale_color_manual(
        values = c("FALSE" = "black", "TRUE" = "red"),
        labels = c(
          "FALSE" = paste0("Change ≤ ±", frozen_pct_threshold * 100, "%"),
          "TRUE"  = paste0("Change > ±", frozen_pct_threshold * 100, "%")
        ),
        na.translate = FALSE
      ) +
      scale_fill_manual(values = c("Stockout" = "orange"), labels = c("Stockout" = "Vaccine Stockout")) +
      guides(color = guide_legend(title = NULL), fill = guide_legend(title = NULL)) +
      theme_minimal() +
      theme(
        axis.line.y.left   = element_line(color = "black"),
        axis.text.x        = element_text(angle = 45, hjust = 1, size = 6.5),
        axis.text.y.left   = element_text(color = "black", size = 7),
        axis.ticks.x       = element_line(color = "black"),
        axis.ticks.y       = element_line(color = "black"),
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
        title    = frozen_title,
        subtitle = frozen_subtitle,
        x        = "Year"
      )
  })
} else {
  plt_perc_change_line <- NULL
}

if(length(vaccines_no_numerator_data) > 0) {
  plt_perc_change_line <- plt_perc_change_line +
    labs(caption = paste0("NOTE: Not enough numerator data to calculate percent change for: ", 
                          paste(vaccines_no_numerator_data, collapse = ", "))) +
    theme(
      plot.caption = element_text(color = "red", size = 12)
    )
}
plt_perc_change_line

# ======================================================================================================================
### Denominator Checks
# ======================================================================================================================

## ---------------------------------------------------------------------------------------------------------------------
### Denom Plot 1: Year-to-year admin denominator change flagged if over threshold
## ---------------------------------------------------------------------------------------------------------------------

denom_change_data <- wuenic_master_current %>%
  group_by(Vaccine) %>%
  mutate(
    prev_target      = lag(ChildrenInTarget),
    pct_change_denom = ((ChildrenInTarget - prev_target) / prev_target) * 100,
    flag_denom_change = abs(pct_change_denom) > pct_threshold
  ) %>%
  ungroup()

# ── OUTLIER HANDLING ──────────────────────────────────────────────────────────
outlier_threshold <- 500

# ── 1. FLAG EXTREME ChildrenInTarget VALUES ───────────────────────────────────
denom_change_data <- denom_change_data %>%
  group_by(Vaccine) %>%
  mutate(
    median_target     = median(ChildrenInTarget, na.rm = TRUE),
    target_ratio      = ChildrenInTarget / median_target,
    target_is_extreme = !is.na(target_ratio) & target_ratio > 10
  ) %>%
  ungroup()

target_outliers_removed <- denom_change_data %>%
  filter(target_is_extreme) %>%
  select(Vaccine, Year, ChildrenInTarget, median_target, target_ratio) %>%
  mutate(
    ratio_rounded  = round(target_ratio, 1),
    target_rounded = scales::label_number(scale_cut = scales::cut_short_scale())(ChildrenInTarget)
  )

denom_change_data <- denom_change_data %>%
  mutate(
    ChildrenInTarget  = if_else(target_is_extreme, NA_real_, ChildrenInTarget),
    pct_change_denom  = if_else(target_is_extreme, NA_real_, pct_change_denom),
    flag_denom_change = if_else(target_is_extreme, NA,       flag_denom_change)
  )

# ── 2. FLAG EXTREME pct_change_denom VALUES ───────────────────────────────────
pct_outliers_removed <- denom_change_data %>%
  filter(!is.na(pct_change_denom), abs(pct_change_denom) > outlier_threshold) %>%
  select(Vaccine, Year, pct_change_denom, ChildrenInTarget) %>%
  mutate(pct_change_rounded = round(pct_change_denom, 1))

denom_change_data <- denom_change_data %>%
  mutate(
    pct_change_denom  = if_else(!is.na(pct_change_denom) & abs(pct_change_denom) > outlier_threshold, NA_real_, pct_change_denom),
    flag_denom_change = if_else(is.na(pct_change_denom), NA, flag_denom_change)
  )

# ── 3. BUILD CAPTION NOTES ────────────────────────────────────────────────────
if (nrow(pct_outliers_removed) > 0) {
  pct_outlier_note <- pct_outliers_removed %>%
    mutate(note = paste0(Vaccine, " (", Year, ": ", pct_change_rounded, "%)")) %>%
    pull(note) %>%
    paste(collapse = "; ")
  pct_outlier_subtitle <- paste0("Pct-change outliers removed (|change| > ", outlier_threshold, "%): ", pct_outlier_note)
} else {
  pct_outlier_subtitle <- ""
}

if (nrow(target_outliers_removed) > 0) {
  target_outlier_note <- target_outliers_removed %>%
    mutate(note = paste0(Vaccine, " (", Year, ": ", target_rounded, ", ", ratio_rounded, "× median)")) %>%
    pull(note) %>%
    paste(collapse = "; ")
  target_outlier_subtitle <- paste0("Extreme target population values removed (>10× vaccine median): ", target_outlier_note)
} else {
  target_outlier_subtitle <- ""
}

outlier_caption <- paste(
  c(pct_outlier_subtitle, target_outlier_subtitle)[nchar(c(pct_outlier_subtitle, target_outlier_subtitle)) > 0],
  collapse = "\n"
)

# ── DATA AVAILABILITY CHECK (per vaccine) ────────────────────────────────────
vaccines_with_denominator_data <- denom_change_data %>%
  group_by(Vaccine) %>%
  summarise(n_valid = sum(!is.na(ChildrenInTarget)), .groups = "drop") %>%
  filter(n_valid > 1) %>%
  pull(Vaccine)

vaccines_no_denominator_data <- denom_change_data %>%
  distinct(Vaccine) %>%
  filter(!Vaccine %in% vaccines_with_denominator_data) %>%
  pull(Vaccine)

# ── SCALING (only on vaccines with data) ──────────────────────────────────────
plot_data_for_scaling <- denom_change_data %>% filter(Vaccine %in% vaccines_with_denominator_data)

max_pct <- max(abs(plot_data_for_scaling$pct_change_denom), na.rm = TRUE)
if (is.na(max_pct) || max_pct == 0) max_pct <- 0.1

if (max_pct > 250) {
  y_max <- ceiling(max_pct / 100) * 100
  step_size       <- 100
  division_factor <- ((ifelse(y_max > 0, y_max / step_size, 1)) * 2) + 1
} else if (max_pct > 150) {
  y_max <- ceiling(max_pct / 50) * 50
  step_size       <- 50
  division_factor <- ((ifelse(y_max > 0, y_max / step_size, 1)) * 2) + 1
} else {
  y_max <- ceiling(max_pct / 25) * 25
  step_size       <- 25
  division_factor <- ((ifelse(y_max > 0, y_max / step_size, 1)) * 2) + 1
}

y_min <- -y_max

target_min <- min(plot_data_for_scaling$ChildrenInTarget, na.rm = TRUE)
target_max <- max(plot_data_for_scaling$ChildrenInTarget, na.rm = TRUE)
if (is.na(target_max) || target_max == 0) target_max <- 1
if (is.na(target_min)) target_min <- 0
scale_factor <- (target_max - target_min) / (y_max - y_min)
if (is.na(scale_factor) || scale_factor == 0) scale_factor <- 1

denom_change_data <- denom_change_data %>%
  mutate(
    target_scaled = (ChildrenInTarget - target_min) / (target_max - target_min) * (y_max - y_min) + y_min
  )

# ── PLOT: vaccines WITH sufficient data ───────────────────────────────────────
if (length(vaccines_with_denominator_data) > 0) {
  
  plot_data_sufficient <- denom_change_data %>% filter(Vaccine %in% vaccines_with_denominator_data)
  
  plt_denom_change <- local({
    
    frozen_plot_data    <- plot_data_sufficient
    frozen_y_min        <- y_min
    frozen_y_max        <- y_max
    frozen_target_min   <- target_min
    frozen_target_max   <- target_max
    frozen_division     <- division_factor
    frozen_pct_threshold <- pct_threshold
    frozen_outlier_caption <- outlier_caption
    frozen_year_min     <- min(plot_data_sufficient$Year)
    frozen_year_max     <- max(plot_data_sufficient$Year)
    frozen_title        <- paste0("Year-to-Year % Change in Denominator (# Children in Target), ", .current_country, ", ", min_yr_plots, "–", rev_yr)
    frozen_subtitle     <- paste0("Green line shows target population counts; points show % annual change\n",
                                  "Red points indicate changes exceeding ±", pct_threshold * 100, "%\n",
                                  "Orange shading indicates vaccine stockout")
    
    ggplot(frozen_plot_data, aes(x = Year)) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
      geom_hline(yintercept = c(frozen_pct_threshold * 100, -frozen_pct_threshold * 100),
                 linetype = "dashed", color = "red", alpha = 1) +
      geom_segment(aes(x = Year, xend = Year, y = 0, yend = pct_change_denom),
                   color = "grey60", na.rm = TRUE) +
      geom_point(aes(y = pct_change_denom, color = flag_denom_change), size = 1.75, na.rm = TRUE) +
      geom_line(aes(y = target_scaled, group = Vaccine),
                color = "#00833D", linewidth = 0.8, alpha = 0.6) +
      geom_point(aes(y = target_scaled),
                 color = "#00833D", size = 1.5, na.rm = TRUE) +
      facet_wrap(~Vaccine, scales = "fixed") +
      scale_y_continuous(
        name   = "Year-to-Year % Change",
        limits = c(frozen_y_min, frozen_y_max),
        breaks = seq(frozen_y_min, frozen_y_max, length.out = frozen_division),
        labels = scales::label_number(suffix = "%", accuracy = 1),
        sec.axis = sec_axis(
          trans  = ~ (. - frozen_y_min) / (frozen_y_max - frozen_y_min) * (frozen_target_max - frozen_target_min) + frozen_target_min,
          name   = "Children in Target Population",
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        )
      ) +
      scale_x_continuous(
        breaks = seq(frozen_year_min, frozen_year_max, by = 1),
        labels = function(x) ifelse(x %% 2 == 0, as.character(x), "")
      ) +
      scale_color_manual(
        values = c("FALSE" = "black", "TRUE" = "red"),
        labels = c(
          "FALSE" = paste0("Change ≤ ±", frozen_pct_threshold * 100, "%"),
          "TRUE"  = paste0("Change > ±", frozen_pct_threshold * 100, "%")
        ),
        na.translate = FALSE
      ) +
      guides(color = guide_legend(title = NULL)) +
      theme_minimal() +
      theme(
        axis.line.y.left   = element_line(color = "black"),
        axis.text.x        = element_text(angle = 45, hjust = 1, size = 6.5),
        axis.text.y.left   = element_text(color = "black", size = 8),
        axis.ticks.x       = element_line(color = "black"),
        axis.ticks.y       = element_line(color = "black"),
        axis.line.y.right  = element_line(color = "#00833D"),
        axis.text.y.right  = element_text(color = "#00833D", size = 8),
        axis.title.y.right = element_text(color = "#00833D", size = 9),
        panel.grid.minor   = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.6),
        legend.position    = "bottom",
        strip.background   = element_rect(fill = "#0083CF"),
        strip.text         = element_text(color = "white", face = "bold"),
        plot.title         = element_text(hjust = 0.5, size = 14),
        plot.subtitle      = element_text(hjust = 0.5, size = 11),
        plot.caption       = element_text(size = 8, hjust = 0)
      ) +
      labs(
        title    = frozen_title,
        subtitle = frozen_subtitle,
        caption  = frozen_outlier_caption,
        x        = "Year"
      )
  })
} else {
  plt_denom_change <- NULL
}

# ── APPEND NO-DATA NOTE TO CAPTION IF NEEDED ─────────────────────────────────
if (length(vaccines_no_denominator_data) > 0) {
  no_data_note <- paste0(
    "NOTE: Not enough denominator data to calculate percent change for: ",
    paste(vaccines_no_denominator_data, collapse = ", ")
  )
  plt_denom_change <- plt_denom_change +
    labs(caption = paste(c(outlier_caption, no_data_note)[nchar(c(outlier_caption, no_data_note)) > 0],
                         collapse = "\n")) +
    theme(plot.caption = element_text(color = "red", size = 12, hjust = 1))
}

plt_denom_change

## ---------------------------------------------------------------------------------------------------------------------
### Denom Plot 1 (version b): Year-to-year admin denominator change flagged if over threshold
## ---------------------------------------------------------------------------------------------------------------------

# denom_change_data <- wuenic_master_current %>%
#   group_by(Vaccine) %>%
#   mutate(
#     prev_target = lag(ChildrenInTarget),
#     pct_change_denom = (ChildrenInTarget - prev_target) / prev_target,
#   ) %>%
#   ungroup()
# 
# # use distribution of year-to-year changes to determine thresholds
# denom_change_vals <- denom_change_data$pct_change_denom[!is.na(denom_change_data$pct_change_denom)]
# denom_q1  <- quantile(denom_change_vals, 0.25)
# denom_q3  <- quantile(denom_change_vals, 0.75)
# denom_iqr <- denom_q3 - denom_q1
# denom_low_thresh  <- denom_q1 - 1.5 * denom_iqr  # outlier lower bound
# denom_high_thresh <- denom_q3 + 1.5 * denom_iqr  # outlier upper bound
# 
# denom_change_data <- denom_change_data %>%
#   mutate(
#     denom_flag = case_when(
#       is.na(pct_change_denom)                  ~ "No Data",
#       pct_change_denom < denom_low_thresh      ~ "Large Decrease",
#       pct_change_denom > denom_high_thresh     ~ "Large Increase",
#       TRUE                                     ~ "Normal"
#     )
#   )
# 
# # define the range for the primary axis (% change)
# y_limit <- max(abs(denom_change_data$pct_change_denom), na.rm = TRUE)
# if(y_limit == 0 || is.na(y_limit)) y_limit <- 0.1
# primary_min <- -y_limit
# primary_max <- y_limit
# 
# # define the range for the secondary axis (# children)
# sec_min <- 0
# sec_max <- max(denom_change_data$ChildrenInTarget, na.rm = TRUE)
# if(sec_max <= 0 || is.na(sec_max)) sec_max <- 100
# 
# # scaling factor for plotting
# scale_m <- (sec_max - sec_min) / (primary_max - primary_min)
# offset_c <- sec_max - (scale_m * primary_max)
# 
# plt_denom_change_b <- local({
#   ggplot(denom_change_data, aes(x = Year)) +
#   geom_hline(yintercept = 0, color = "black") +
#   geom_hline(yintercept = c(denom_high_thresh, denom_low_thresh), linetype = "dashed", color = "red", alpha = 0.5) +
#   
#   # y axis 1: percent change
#   geom_segment(aes(x = Year, xend = Year, y = 0, yend = pct_change_denom), color = "grey60") +
#   geom_point(aes(y = pct_change_denom, color = denom_flag), size = 2.5) +
#   
#   # y axis 2: raw denominator (scaled to fit primary axis)
#   geom_line(aes(y = (ChildrenInTarget - offset_c) / scale_m, group = Vaccine), color = "#00833D", linewidth = 0.8, alpha = 0.6) +
#   
#   facet_wrap(~Vaccine) +
#   scale_y_continuous(
#     name = "% Change from Previous Year", 
#     limits = c(primary_min, primary_max), 
#     labels = scales::percent_format(),
#     sec.axis = sec_axis(
#       trans = ~ . * scale_m + offset_c,
#       name  = "Raw Children in Target Population",
#       labels = scales::label_number(scale_cut = scales::cut_short_scale())
#     )
#   ) +
#   scale_x_continuous(breaks = seq(min(denom_change_data$Year), max(denom_change_data$Year), by = 2)) +
#   scale_color_manual(
#     name = NULL,
#     values = c(
#       "Normal"        = "black",
#       "Large Increase" = "red",
#       "Large Decrease" = "orange",
#       "No Data"       = "grey60"
#     ),
#     na.translate = FALSE
#   ) + 
#   theme_minimal() +
#   theme(
#     legend.position    = "bottom",
#     panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.6),
#     strip.background   = element_rect(fill = "#0083CF"),
#     strip.text         = element_text(color = "white", face = "bold"),
#     axis.text.y.right  = element_text(color = "#00833D"),
#     axis.title.y.right = element_text(color = "#00833D"),
#     plot.title         = element_text(hjust = 0.5, size = 14),
#     plot.subtitle      = element_text(hjust = 0.5, size = 11),
#     axis.ticks.x = element_line(color = "black"),
#     axis.ticks.y = element_line(color = "black"),
#     axis.text.x  = element_text(angle = 45, hjust = 1, size = 8)
#   ) +
#   labs(
#     title = paste0("Year-to-Year % Change in Denominator (# Children in Target), ", .current_country, ", ", min_yr_plots, "–", rev_yr),
#     subtitle = paste0(
#       "Line shows raw counts; points show % annual change\n",
#       "Dashed lines show IQR-based outlier thresholds: ",
#       sprintf("%.1f%%", denom_low_thresh * 100), " / +", sprintf("%.1f%%", denom_high_thresh * 100),
#       " (Q1/Q3 ± 1.5×IQR)"
#     )
#   )
# })
# plt_denom_change_b

## ---------------------------------------------------------------------------------------------------------------------
### Denom Plot 2: Live births (BCG) vs surviving infants (DTP1) over time
## ---------------------------------------------------------------------------------------------------------------------

# separate birth and surviving infants data from wuenic_master_current and reshape for plotting
births_vs_si_data <- wuenic_master_current %>%
  select(Year, Vaccine, ChildrenInTarget) %>%
  filter(Vaccine %in% c("BCG", "DTP1")) %>%
  mutate(target_grp = case_when(
    Vaccine == "BCG"  ~ "Live Births (BCG)",
    Vaccine == "DTP1" ~ "Surviving Infants (DTP1)",
    TRUE ~ NA_character_
  ))

bcg_data_available <- births_vs_si_data %>%
  filter(Vaccine == "BCG", !is.na(ChildrenInTarget)) %>%
  nrow() > 0
dtp1_data_available <- births_vs_si_data %>%
  filter(Vaccine == "DTP1", !is.na(ChildrenInTarget)) %>%
  nrow() > 0
bcg_in_schedule <- "BCG" %in% tbl_schedule_r$Vaccine
availability_footnote <- if (!bcg_data_available && !dtp1_data_available) {
  "NOTE: No denominator data available for either BCG or DTP1."
} else if (!bcg_data_available && !bcg_in_schedule) {
  paste0("NOTE: BCG is not in ", .current_country, "'s current schedule, so there is no denominator data available.")
} else if(!bcg_data_available && bcg_in_schedule) {
  "NOTE: No denominator data available for BCG (Live Births)."
} else if (!dtp1_data_available) {
  "NOTE: No denominator data available for DTP1 (Surviving Infants)."
} else {
  NULL
}

# set y axis min and max when there is only one data point available
data_available <- births_vs_si_data %>%
  filter(!is.na(ChildrenInTarget)) %>%
  nrow()

if(data_available == 1) {
  single_value <- births_vs_si_data$ChildrenInTarget[!is.na(births_vs_si_data$ChildrenInTarget)][1]
  
  # dynamically determine a clean step size based on the magnitude of the value
  # e.g., if value is 500,000, step is 100,000. If value is 5,000, step is 1,000.
  step_magnitude <- 10^floor(log10(single_value) - 0.5)
  
  # round down the min and round up the max to the nearest clean step
  y_min <- floor((single_value * 0.5) / step_magnitude) * step_magnitude
  y_max <- ceiling((single_value * 1.5) / step_magnitude) * step_magnitude
  
  # ensure y_min doesn't drop below 0 for target populations
  y_min <- max(0, y_min) 
} else {
  y_min <- NULL
  y_max <- NULL
}

plt_births_vs_si <- ggplot(births_vs_si_data, aes(x = Year, y = ChildrenInTarget, color = target_grp)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale()),
    limits = if(!is.null(y_min)) c(y_min, y_max) else NULL 
  ) +
  scale_x_continuous(breaks = seq(min(births_vs_si_data$Year), max(births_vs_si_data$Year), by = 1)) +
  scale_color_manual(values = c("Live Births (BCG)" = "#E87722", "Surviving Infants (DTP1)" = "#0083CF")) +
  theme_minimal() +
  labs(
    title = paste0("Admin Data: Live Births (BCG) vs Surviving Infants (DTP1) Denominators, ", .current_country, ", ", min_yr_plots, "–", rev_yr),
    x = "Year", y = "Target Population",
    color = "Denominator Type",
    caption = availability_footnote
  ) +
  theme(
    legend.position  = "bottom",
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.6),
    plot.title       = element_text(hjust = 0.5, size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 11),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.ticks.x     = element_line(color = "black"),
    axis.ticks.y     = element_line(color = "black"),
    plot.caption     = element_text(color = "red", size = 12)
  )

## ---------------------------------------------------------------------------------------------------------------------
### Denom Plot 3: Year-to-year % change for Live Births (BCG) and Surviving Infants (DTP1)
## ---------------------------------------------------------------------------------------------------------------------

denom_types_data <- wuenic_master_current %>%
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

# see if there is enough denominator data available to compute percent change
denom_data_available <- denom_types_data %>%
  filter(!is.na(ChildrenInTarget)) %>%
  nrow() > 1

if(denom_data_available) {
  plt_denom_pct_change <- local({
    ggplot(denom_types_data, aes(x = Year)) +
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
      title = paste0("Denominator Stability Check, ", .current_country, ", ", min_yr_plots, "–", rev_yr),
      subtitle = paste0("Line shows raw counts; points show % annual change\n", 
                        "Red points indicate changes exceeding +/- ", pct_threshold * 100, "%"))
  })
} else {
  plt_denom_pct_change <- local({
    ggplot() +
    geom_text(aes(x = 1, y = 1, label = "Not enough denominator data to compute % change"), 
              color = "red", size = 5) +
    theme_void() +
    labs(
      title = paste0("Denominator Stability Check, ", .current_country, ", ", min_yr_plots, "–", rev_yr),
      subtitle = "Insufficient data for year-to-year % change calculation"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11)
    )
  })
}


# ======================================================================================================================
### Dropout & Vaccine Relationships
# ======================================================================================================================

dropout_base <- wuenic_master_current %>%
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
  geom_line(data = dropout_long, aes(x = Year, y = Admin, color = Vaccine), linewidth = 0.9, alpha = 0.7) +
  geom_point(data = dropout_long, aes(x = Year, y = Admin, color = Vaccine), size = 2) +
  
  # dropout % as bars
  geom_col(data = dropout_rates, aes(x = Year, y = dropout_pct), fill = "grey50", alpha = 0.3, width = 0.5) +
  
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  facet_wrap(~pair, scales = "free_y") +
  scale_y_continuous(
    limits = c(0, ceiling(max(dropout_long$Admin, na.rm = TRUE) / 10) * 10),
    breaks = seq(0, 100, by = 10), labels = scales::label_number(suffix = "%")
  ) +
  scale_x_continuous(breaks = seq(min(dropout_long$Year), max(dropout_long$Year), by = 2)) +
  scale_color_manual(values = unicef_colors) +
  theme_minimal() +
  labs(
    title = paste0("Key Vaccine Dropout, ", .current_country, ", ", min_yr_plots, "–", rev_yr),
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

selected_coadmin <- wuenic_master_current %>% 
  filter(Vaccine %in% c("DTP3", "PCVC"))

plt_coadmin_dtp_pcv <- ggplot(selected_coadmin, aes(x = Year, y = Admin, color = Vaccine)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 0.8) +
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("DTP3" = "#0058AB", "PCVC" = "#E87722")) + 
  scale_y_continuous(limits = c(0, max(c(105, max(selected_coadmin$Admin, na.rm = TRUE)))), 
                     breaks = seq(0, 150, by = 10), labels = scales::label_number(suffix = "%")) +
  scale_x_continuous(breaks = seq(min(selected_coadmin$Year), max(selected_coadmin$Year), by = 2)) +
  theme_minimal() +
  labs(
    title = paste0("Admin Coverage of DTP3 and PCVC (Co-administered), ", .current_country, ", ", min_yr_plots, "–", rev_yr),
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

# get which time the dtp3 and pcvc vaccines are administered
dtp3_pcvc_time <- tbl_schedule_r %>%
  filter(Vaccine %in% c("PCV", "DTAPHIBHEPBIPV", "DTWPHIBHEPB", "DTAPHIBIPV", "DTAPIPV", "DTWPHIBHEPBIPV",
                        "DTAP", "DTAPHIBHEPB", "DTAPHEPBIPV", "DTAPHIB", "DTWP")) %>%
  pivot_longer(cols = any_of(as.character(1:20)), names_to = "dose_num", values_to = "age") %>%
  filter(dose_num == 3) %>% 
  select(Vaccine, age) %>%
  distinct() %>%
  pivot_wider(names_from = Vaccine, values_from = age) %>% 
  pull(1)

## ---------------------------------------------------------------------------------------------------------------------
### Co-administration Plot: Dynamic schedule check — clusters based on shared time points
## ---------------------------------------------------------------------------------------------------------------------

# mapping between schedule table names and admin data names
val_map <- c(
  "BCG"           = "BCG",
  "DTWPHIBHEPB"   = "DTP",
  "DTAPHIBHEPBIPV" = "DTP",
  "PCV"           = "PCV",
  "MEASLES"       = "MCV",
  "MMR"           = "MCV",
  "IPV"           = "IPV",
  "OPV"           = "OPV",
  "HPV"           = "HPVc",
  "Rotavirus"     = "RotaC"
)

# function to detect co-administration clusters from the schedule table
get_coadmin_clusters <- function(sched_df, min_cluster_size = 3) {
  
  dose_cols <- colnames(sched_df)[colnames(sched_df) %in% as.character(1:20)]
  
  # pivot to long: one row per vaccine x dose
  long_sched <- sched_df %>%
    pivot_longer(
      cols      = any_of(dose_cols),
      names_to  = "dose_num",
      values_to = "age"
    ) %>%
    filter(!is.na(age), age != "NA", age != "") %>%
    mutate(
      base_name = val_map[Vaccine],
      last_dose_for_vax = dose_num == max(dose_num),  # flag last dose per vaccine
      final_name = case_when(
        Vaccine == "OPV" ~ paste0("OPV", as.numeric(dose_num)),
        Vaccine %in% c("PCV", "Rotavirus", "IPV") & dose_num == max(dose_num) ~ paste0(base_name, "C"),
        TRUE ~ paste0(base_name, dose_num)
      )
    ) %>%
    filter(!is.na(base_name))
  
  # find all unique time points and which vaccines share them
  time_point_groups <- long_sched %>%
    group_by(age) %>%
    summarise(
      vaccines   = list(final_name),
      n_vaccines = n_distinct(final_name),
      .groups    = "drop"
    ) %>%
    filter(n_vaccines >= min_cluster_size) %>%   # only keep time points with 2+ vaccines
    arrange(age)
  
  # convert to named list: time point label -> vector of vaccine names
  clusters <- setNames(
    time_point_groups$vaccines,
    time_point_groups$age
  )
  
  return(clusters)
}

# generate clusters dynamically
coadmin_pairs <- get_coadmin_clusters(tbl_schedule_r, min_cluster_size = 3)

# standardise vaccine name variants
coadmin_pairs <- lapply(coadmin_pairs, function(v) {
  v <- gsub("PCV3",  "PCVC",  v)
  v <- gsub("OPV4",  "POL4",  v)
  v <- gsub("RotaC", "ROTA",  v) # temporarily clean "RotaC1" -> "ROTA1"
  v
})

# find the maximum Rota dose number currently present across all clusters
all_elements <- unlist(coadmin_pairs)
rota_elements <- all_elements[grep("^ROTA[0-9]+$", all_elements)]
rota_numbers <- as.numeric(gsub("ROTA", "", rota_elements))

if (length(rota_numbers) > 0) {
  max_rota_dose <- max(rota_numbers)
  
  # loop back through clusters to apply the proper ROTA1, ROTA2, and ROTAC names
  coadmin_pairs <- lapply(coadmin_pairs, function(v) {
    # replace the highest dose with ROTAC
    v <- gsub(paste0("ROTA", max_rota_dose), "RotaC", v)
    v
  })
}

# if 6-week and 14-week vaccines are in the coadmin pairs, just use those two
if ("6 weeks" %in% names(coadmin_pairs) & "14 weeks" %in% names(coadmin_pairs)) {
  coadmin_pairs <- coadmin_pairs[c("6 weeks", "14 weeks")]
  names(coadmin_pairs) <- c("6-week", "14-week")
}

# print the result to confirm rotas are named correctly
#print(coadmin_pairs)

#message(paste0("Co-administration clusters detected for ", .current_country, ": ", paste(names(coadmin_pairs), collapse = ", ")))

# text labels for each cluster
coadmin_labels <- purrr::imap_chr(coadmin_pairs, function(vaccines, cluster_name) {
  
  available <- vaccines[vaccines %in% available_vaccines]
  missing   <- vaccines[!vaccines %in% available_vaccines]
  
  intro_text <- if (length(available) > 0) {
    paste0(
      "According to the official national vaccination schedule for ", .current_country,
      ", the following antigens are recommended for co-administration at the ",
      cluster_name, " visit: ", paste(vaccines, collapse = ", "), ". "
    )
  } else {
    paste0("No recommended antigens for the ", cluster_name, " visit were identified in the current dataset.")
  }
  
  missing_text <- if (length(missing) > 0) {
    paste0(
      "Note: Data for the following scheduled antigens (",
      paste(missing, collapse = ", "),
      ") were expected but are currently missing from the reported administrative database."
    )
  } else {
    "All antigens scheduled for this time point are present in the dataset."
  }
  
  paste0(intro_text, missing_text)
})

# build plot data for all clusters
plot_data <- purrr::map_dfr(names(coadmin_pairs), function(cluster_name) {
  vaccines_to_plot <- coadmin_pairs[[cluster_name]]
  dropout_base %>%
    filter(Vaccine %in% vaccines_to_plot) %>%
    mutate(cluster = cluster_name)
})

# generate one plot per cluster, stored in a named list
plt_coadmin_list <- purrr::imap(coadmin_pairs, function(vaccines, cluster_name) {
  
  plot_data_cluster <- plot_data %>% filter(cluster == cluster_name)
  
  if (nrow(plot_data_cluster) == 0) return(NULL)
  
  # get vaccines in this cluster that are actually present in the data (for color mapping)
  vaccines_in_cluster <- unique(plot_data_cluster$Vaccine)
  
  ggplot(plot_data_cluster, aes(x = Year, y = Admin, color = Vaccine, group = Vaccine)) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
    geom_line(linewidth = 1, alpha = 0.7) +
    geom_point(size = 2) +
    scale_y_continuous(limits = c(0, 125), breaks = seq(0, 100, by = 25), labels = scales::label_number(suffix = "%")) +
    scale_x_continuous(breaks = seq(min(plot_data_cluster$Year),
                                    max(plot_data_cluster$Year), by = 1)) +
    scale_color_manual(values = setNames(unicef_colors[seq_along(vaccines_in_cluster)], vaccines_in_cluster)) +
    theme_minimal() +
    labs(
      title = paste0("Admin Coverage of Vaccines Administered at ", cluster_name, " visit, ", .current_country, ", ", min_yr_plots, "–", rev_yr),
      x = "Year", y = "Admin Coverage (%)", color = "Vaccine") +
    theme(
      legend.position  = "bottom",
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
      axis.ticks.x     = element_line(color = "black"),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.6),
      strip.background = element_rect(fill = "#0083CF"),
      strip.text       = element_text(color = "white", face = "bold"),
      plot.title       = element_text(hjust = 0.5, size = 14)
    )
})

# remove NULL plots (clusters with no data)
plt_coadmin_list <- Filter(Negate(is.null), plt_coadmin_list)

# ======================================================================================================================
### Numerator check plot using the co-administered vaccines
# ======================================================================================================================

# filter plot_data_numerators based on the vaccines in each co-administration cluster
plot_data_numerator_clusters <- purrr::map_dfr(names(coadmin_pairs), function(cluster_name) {
  vaccines_to_plot <- coadmin_pairs[[cluster_name]]
  plot_data_numerators_all %>%
    filter(Vaccine %in% vaccines_to_plot) %>%
    mutate(cluster = cluster_name)
})

# ==============================================================================
# 2. UPDATED NUMERATOR PLOT FUNCTION
# ==============================================================================

make_numerator_plot <- function(plot_data, time_point) {
  
  if (nrow(plot_data) == 0) return(NULL)
  
  plot_data <- droplevels(plot_data)
  
  time_plot_vaccines <- plot_data %>% distinct(Vaccine) %>% pull(Vaccine)
  plot_data$Vaccine  <- factor(plot_data$Vaccine, levels = time_plot_vaccines)
  
  # ── OUTLIER HANDLING ────────────────────────────────────────────────────────
  outlier_threshold <- 500
  
  outliers_removed <- plot_data %>%
    filter(!is.na(pct_change), abs(pct_change) > outlier_threshold) %>%
    select(Vaccine, Year, pct_change, ChildrenVaccinated) %>%
    mutate(pct_change_label = round(pct_change, 1))
  
  plot_data <- plot_data %>%
    mutate(
      pct_change = if_else(abs(pct_change) > outlier_threshold, NA_real_, pct_change),
      is_flagged = if_else(is.na(pct_change), NA, is_flagged)
    )
  
  if (nrow(outliers_removed) > 0) {
    outlier_note <- outliers_removed %>%
      mutate(note = paste0(Vaccine, " (", Year, ": ", pct_change_label, "%)")) %>%
      pull(note) %>%
      paste(collapse = "; ")
    outlier_subtitle <- paste0("\nOutliers removed (|change| > ", outlier_threshold, "%): ", outlier_note)
  } else {
    outlier_subtitle <- ""
  }
  
  # ── DATA AVAILABILITY CHECK ──────────────────────────────────────────────────
  numerator_data_available <- plot_data %>%
    filter(!is.na(ChildrenVaccinated)) %>%
    nrow() > 1
  
  if (!numerator_data_available) {
    return(
      ggplot() +
        geom_text(aes(x = 1, y = 1,
                      label = paste0(time_plot_vaccines, ": Not enough numerator data to compute % change")),
                  color = "red", size = 5) +
        theme_void() +
        labs(
          title = paste0(
            "Year-to-Year % Change in Numerator (# Children Vaccinated), ",
            .current_country, ", ", min_yr_plots, "–", rev_yr,
            "\nVaccines Administered at ", time_point, " visit"
          ),
          subtitle = "Insufficient data for year-to-year % change calculation"
        ) +
        theme(
          plot.title    = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 11)
        )
    )
  }
  
  # ── SCALING ─────────────────────────────────────────────────────────────────
  max_doses <- max(plot_data$ChildrenVaccinated, na.rm = TRUE)
  max_pct   <- max(abs(plot_data$pct_change), na.rm = TRUE)
  
  if (is.na(max_pct) || max_pct == 0) max_pct <- 0.1
  
  y_min <- -max_pct * 1.2
  y_max <-  max_pct * 1.2
  
  scale_factor <- max_doses / (y_max - y_min)
  if (is.na(scale_factor) || scale_factor == 0) scale_factor <- 1
  
  # ── STOCKOUT DATA ────────────────────────────────────────────────────────────
  current_vax      <- unique(plot_data$Vaccine)
  current_stockout <- stockout_data %>% filter(Vaccine %in% current_vax)
  
  # ── PLOT ─────────────────────────────────────────────────────────────────────
  numerator_plot <- ggplot(plot_data, aes(x = Year)) +
    
    # stockout shading (behind everything)
    geom_rect(data = current_stockout, aes(xmin = xmin_num, xmax = xmax_num, ymin = -Inf, ymax = Inf),
              fill = "orange", alpha = 0.2, inherit.aes = FALSE) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_hline(yintercept = c(pct_threshold * 100, -pct_threshold * 100),
               linetype = "dashed", color = "red", alpha = 1) +
    # layer 1: % change lollipops (primary axis) — outlier NAs silently dropped
    geom_segment(aes(x = Year, xend = Year, y = 0, yend = pct_change),
                 color = "grey60", na.rm = TRUE) +
    geom_point(aes(y = pct_change, color = is_flagged), size = 2.5, na.rm = TRUE) +
    # layer 2: raw counts scaled to primary axis — 0 anchors to y_min, max_doses to y_max
    geom_line(aes(y = (ChildrenVaccinated / max_doses) * (y_max - y_min) + y_min,
                  group = Vaccine), color = "#00833D", linewidth = 0.8, alpha = 0.6) +
    geom_point(aes(y = (ChildrenVaccinated / max_doses) * (y_max - y_min) + y_min),
               color = "#00833D", size = 1.5, na.rm = TRUE) +
    facet_wrap(~Vaccine, scales = "fixed", drop = TRUE) +
    scale_y_continuous(
      name   = "Year-to-Year % Change",
      limits = c(y_min, y_max),
      labels = scales::label_number(suffix = "%", accuracy = 1),
      sec.axis = sec_axis(
        trans  = ~ (. - y_min) * scale_factor,  # y_min → 0, y_max → max_doses
        name   = "# Children Vaccinated",
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      )
    ) +
    scale_x_continuous(
      breaks = seq(min(plot_data$Year), max(plot_data$Year), by = 1),
      labels = function(x) ifelse(x %% 2 == 0, as.character(x), "")
    ) +
    scale_color_manual(
      values = c("FALSE" = "black", "TRUE" = "red"),
      labels = c(
        "FALSE" = paste0("Change ≤ ±", pct_threshold * 100, "%"),
        "TRUE"  = paste0("Change > ±", pct_threshold * 100, "%")
      ),
      na.translate = FALSE
    ) +
    scale_fill_manual(
      values = c("Stockout" = "orange"),
      labels = c("Stockout" = "Vaccine Stockout")
    ) +
    guides(color = guide_legend(title = NULL), fill = guide_legend(title = NULL)) +
    theme_minimal() +
    theme(
      axis.line.y.left   = element_line(color = "black"),
      axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
      axis.ticks.x       = element_line(color = "black"),
      axis.ticks.y       = element_line(color = "black"),
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
      title = paste0(
        "Year-to-Year % Change in Numerator (# Children Vaccinated), ",
        .current_country, ", ", min_yr_plots, "–", rev_yr,
        "\nVaccines Administered at ", time_point, " visit"
      ),
      subtitle = paste0(
        "Line shows raw counts; points show % annual change\n",
        "Red points indicate changes exceeding +/- ", pct_threshold * 100, "%\n",
        "Orange shading indicates vaccine stockout"
      ),
      x = "Year"
    )
  
  return(numerator_plot)
}

# ==============================================================================
# 3. DYNAMICALLY GENERATE ONE PLOT PER CLUSTER (Stored in a Named List)
# ==============================================================================
plt_numerator_list <- purrr::imap(coadmin_pairs, function(vaccines, cluster_name) {
  
  # Filter our stacked dataset to just this cluster
  plot_data_cluster <- plot_data_numerator_clusters %>% filter(cluster == cluster_name)
  
  # Generate the plot dynamically passing the cluster's name as the time_point
  make_numerator_plot(plot_data_cluster, cluster_name)
})

# Clean up list by removing any clusters that didn't have data matches
plt_numerator_list <- Filter(Negate(is.null), plt_numerator_list)

# ======================================================================================================================
### Missing Data Heatmap (Admin Data)
# ======================================================================================================================

# vaccine name mapping
vaccine_map <- c(
  "MEASLES"           = "MCV1",
  "MMR"               = "MCV1",  # Italy uses MMR
  "BCG"               = "BCG",
  "DTWPHIBHEPB"       = "DTP3",
  "DTAPHIBHEPBIPV"    = "DTP3",  # Italy's combo
  "PCV"               = "PCVC",
  "Rotavirus"         = "RotaC",
  "OPV"               = "POL3",
  "IPV"               = "IPV1",
  "HPV (females and males)" = "HPVc",
  "HepB"              = "HepB3",
  "Hib"               = "Hib3",
  "MCV2"              = "MCV2",
  "RCV"               = "RCV1"
)

# vaccines actually present in this country's wuenic data
wuenic_vaccines <- unique(wuenic_master_current$Vaccine)

# get introduction years per vaccine from wiise_intro
# wiise_intro has one row per country x vaccine, with year_intro_national
intro_years <- wiise_intro %>%
  filter(tolower(iso3c) == tolower(.current_iso3c)) %>%
  mutate(mapped_name = coalesce(vaccine_map[vaccine], vaccine)) %>%
  relocate(mapped_name, .after = vaccine_name) %>% 
  filter(!is.na(mapped_name), mapped_name %in% wuenic_vaccines) %>%
  group_by(mapped_name) %>%
  summarise(intro_year = min(year_intro_national, na.rm = TRUE), .groups = "drop") %>%
  rename(Vaccine = mapped_name)

# after building intro_years, override intro_year with the earliest year
# that admin data actually exists — in case wiise_intro is wrong/mislabeled
admin_earliest <- wuenic_master_current %>%
  filter(!is.na(Admin)) %>%
  group_by(Vaccine) %>%
  summarise(earliest_admin = min(Year, na.rm = TRUE), .groups = "drop")

intro_years <- intro_years %>%
  left_join(admin_earliest, by = "Vaccine") %>%
  mutate(
    intro_year = case_when(
      !is.na(earliest_admin) & earliest_admin < intro_year ~ earliest_admin,
      TRUE ~ intro_year
    )
  ) %>%
  select(-earliest_admin)

# add DTP1 with same intro year as DTP3 if present
if ("DTP3" %in% intro_years$Vaccine) {
  dtp3_year <- intro_years %>% filter(Vaccine == "DTP3") %>% pull(intro_year)
  intro_years <- bind_rows(intro_years, tibble(Vaccine = "DTP1", intro_year = dtp3_year))
}

# for vaccines in wuenic_master_current but NOT in wiise schedule at all,
# assume they were always available (intro_year = min_yr_plots)
# so they show red when missing rather than blank
missing_from_intro <- setdiff(wuenic_vaccines, intro_years$Vaccine)
if (length(missing_from_intro) > 0) {
  message("Vaccines in wuenic_master but not mapped in wiise schedule — assuming always introduced: ",
          paste(missing_from_intro, collapse = ", "))
  intro_years <- bind_rows(
    intro_years,
    tibble(Vaccine = missing_from_intro, intro_year = min_yr_plots)
  )
}

# build heatmap data — only vaccines in wuenic_master_current
heatmap_data_wuenic <- wuenic_master_current %>%
  select(Year, Vaccine, Admin) %>%
  # ensure all year x vaccine combos exist
  complete(Year = min_yr_plots:rev_yr, Vaccine = wuenic_vaccines) %>%
  left_join(intro_years, by = "Vaccine") %>%
  mutate(
    status = case_when(
      is.na(intro_year) | Year < intro_year ~ "Not Introduced",  # before intro: blank
      !is.na(Admin) ~ "Present", # introduced + data: green
      TRUE ~ "Missing" # introduced + no data: red
    ),
    status = factor(status, levels = c("Present", "Missing", "Not Introduced"))
  )

# order vaccines for heatmap
vaccine_order <- c("BCG", "HepBB", "DTP1", "DTP3", "Hib3", "HepB3", "PCVC", "RotaC",
                   "POL3", "IPV1", "IPVC", "MCV1", "RCV1", "MCV2", "YFV", "MengA", "HPVc")

heatmap_data_wuenic$Vaccine <- factor(
  heatmap_data_wuenic$Vaccine,
  levels = intersect(vaccine_order, wuenic_vaccines)  # only levels that exist
)

# plot
plt_missing_heatmap <- ggplot(heatmap_data_wuenic, aes(x = factor(Year), y = fct_rev(Vaccine), fill = status)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_manual(values   = c("Missing" = "#E2231A", "Present" = "#00833D"), na.value = "white") +
  theme_minimal() +
  labs(title = paste0("Admin Data Availability Heatmap, ", .current_country, ", ", min_yr_plots, "–", rev_yr),
       x = "Year", y = "Vaccine", fill = "Data Status") +
  theme(
    axis.text.x    = element_text(angle = 45, vjust = 0.5, size = 8),
    panel.grid     = element_blank(),
    legend.position = "bottom",
    plot.title     = element_text(hjust = 0.5, size = 14)
  )
plt_missing_heatmap

