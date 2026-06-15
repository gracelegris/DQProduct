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

# cty_admin_data <- admin_data %>% 
#   filter(iso3c == Current_ISO3, year == max_yr)
# 
# # define components
# penta3_components <- c("DTP3", "HEPB3", "HIB3")
# 
# # validate that the penta components have the same numerator before combining
# mismatches <- cty_admin_data %>%
#   filter(vaccinecode %in% penta3_components) %>%
#   group_by(iso3c, year) %>% 
#   summarise(unique_counts = n_distinct(reportedNum), .groups = "drop") %>%
#   filter(unique_counts > 1)
# 
# if (nrow(mismatches) > 0) {
#   stop("Validation Failed: numerator is inconsistent across Penta3 components.")
# }
# 
# # combine
# cty_admin_clean <- cty_admin_data %>%
#   mutate(vaccine_group = case_when(
#     vaccinecode %in% penta3_components ~ "PENTA3",
#     vaccinecode == "DTP1" ~ "PENTA1",
#     TRUE ~ vaccinecode
#   )) %>%
#   group_by(
#     iso3c, country, year, type, vaccine_group, 
#     live_births, surviving_infants, comment, updated, 
#     updatedBy, first_commit_dt, commit_dt, is_update
#   ) %>%
#   summarise(
#     reportedNum = first(reportedNum),
#     reportedDenom = first(reportedDenom),
#     coverage = first(coverage),
#     .groups = "drop"
#   ) %>%
#   rename(vaccinecode = vaccine_group) %>%
#   relocate(vaccinecode, .after = country)


## ---------------------------------------------------------------------------------------------------------------------
### Flag any vaccines in the most recent schedule that have zero reported admin data
## ---------------------------------------------------------------------------------------------------------------------

# # function to clean vaccine codes for comparison
# clean_vaccine_code <- function(x) {
#   
#   # remove trailing dose numbers, make uppersase
#   base <- str_remove(x, "\\d+$") %>% toupper()
#   
#   # add synonyms
#   synonyms <- c(
#     "POL"       = "OPV",
#     "ROTAC"     = "ROTAVIRUS_",
#     "ROTAVIRUS" = "ROTAVIRUS_",
#     "PENTA"     = "DTP"
#   )
#   
#   # if the base matches a synonym, replace it; otherwise keep base
#   if (base %in% names(synonyms)) {
#     return(synonyms[base])
#   } else {
#     return(base)
#   }
# }
# 
# # extract unique codes
# vaccines_in_schedule <- unique(sched_summary_recent_cty$vaccinecode)
# admin_vaccines <- unique(cty_admin_clean$vaccinecode)
# 
# # apply cleaning
# vaccines_in_schedule_base <- unname(sapply(vaccines_in_schedule, clean_vaccine_code))
# admin_vaccines_base <- unname(sapply(admin_vaccines, clean_vaccine_code))
# 
# # identify missing vaccines
# missing_admin <- vaccines_in_schedule[!(vaccines_in_schedule_base %in% admin_vaccines_base)]
# 
# # print check result
# if (length(missing_admin) == 0) {
#   cat("Success: All vaccines in the schedule are present in the administrative data.\n")
# } else {
#   missing_list <- paste(missing_admin, collapse = ", ")
#   cat(paste0("Check Required: The following vaccines from the schedule are missing in the admin data: ", missing_list, ".\n"))
# }


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


## ---------------------------------------------------------------------------------------------------------------------
### Clean the admin data that includes all years
## ---------------------------------------------------------------------------------------------------------------------

# cty_admin_data_all <- admin_data %>% 
#   filter(iso3c == Current_ISO3) %>% 
#   arrange(vaccinecode, year)
# 
# # validate that the penta components have the same numerator before combining
# mismatches_all <- cty_admin_data_all %>%
#   filter(vaccinecode %in% penta3_components) %>%
#   group_by(iso3c, year) %>%
#   summarise(unique_counts = n_distinct(reportedNum), .groups = "drop") %>%
#   filter(unique_counts > 1)
# 
# if (nrow(mismatches_all) > 0) {
#   stop("Validation Failed: numerator is inconsistent across Penta3 components.")
# }
# 
# # combine
# cty_admin_data_all <- cty_admin_data_all %>%
#   mutate(vaccine_group = case_when(
#     vaccinecode %in% penta3_components ~ "PENTA3",
#     TRUE ~ vaccinecode
#   )) %>%
#   group_by(
#     iso3c, country, year, type, vaccine_group,
#     live_births, surviving_infants, comment, updated,
#     updatedBy, first_commit_dt, commit_dt, is_update
#   ) %>%
#   summarise(
#     reportedNum = first(reportedNum),
#     reportedDenom = first(reportedDenom),
#     coverage = first(coverage),
#     .groups = "drop"
#   ) %>%
#   rename(vaccinecode = vaccine_group) %>%
#   relocate(vaccinecode, .after = country)

## ---------------------------------------------------------------------------------------------------------------------
### Denom Plot 3: Reported denominator vs UN reference population
## ---------------------------------------------------------------------------------------------------------------------

# use births and surviving infants from wuenic_dta as the reported denominators
# UN reference = BirthsUNPD and SurvivingInfantsUNPD from wuenic_master
un_ref_data <- wuenic_master %>%
  select(Year, BirthsUNPD, SurvivingInfantsUNPD) %>%
  distinct() %>%
  rename(year = Year)

reported_denom_data <- denom_data %>%
  filter(target_grp %in% c("births", "si")) %>%
  select(year, target_grp, target) %>%
  distinct() %>%
  left_join(un_ref_data, by = "year") %>%
  mutate(
    un_ref       = ifelse(target_grp == "births", BirthsUNPD, SurvivingInfantsUNPD),
    denom_gap    = target - un_ref,
    pct_gap      = (target - un_ref) / un_ref,
    flag_un_gap  = abs(pct_gap) > pct_threshold
  ) %>%
  filter(!is.na(un_ref))

plt_denom_vs_un <- ggplot(reported_denom_data, aes(x = year)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", alpha = 0.6) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = pct_gap), color = "grey60") +
  geom_point(aes(y = pct_gap, color = flag_un_gap), size = 2.5) +
  facet_wrap(~target_grp, labeller = labeller(target_grp = c("births" = "Live Births (BCG)", "si" = "Surviving Infants (DTP1)"))) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(
    breaks = seq(min(reported_denom_data$year), max(reported_denom_data$year), by = 2)
  ) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  theme_minimal() +
  labs(
    title    = paste("Reported Denominator vs UN Reference Population,", CountryName),
    subtitle = paste0("Red points indicate deviation from UN estimate exceeding +/- ", pct_threshold * 100, "%"),
    x = "Year", y = "% Deviation from UN Estimate",
    color = "Large Gap"
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
### Denom Plot 3: Consistency of denominator across vaccines in the same year
## ---------------------------------------------------------------------------------------------------------------------

denom_consistency_data <- denom_data %>%
  group_by(year) %>%
  mutate(
    median_target = median(target, na.rm = TRUE),
    pct_dev       = (target - median_target) / median_target,
    flag_inconsistent = abs(pct_dev) > pct_threshold
  ) %>%
  ungroup()

plt_denom_consistency <- ggplot(denom_consistency_data, aes(x = year, y = pct_dev)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(pct_threshold, -pct_threshold), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = pct_dev), color = "grey60") +
  geom_point(aes(color = flag_inconsistent), size = 2) +
  facet_wrap(~vaccine) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(
    breaks = seq(min(denom_consistency_data$year), max(denom_consistency_data$year), by = 2)
  ) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  theme_minimal() +
  labs(
    title    = paste("Denominator Consistency Across Vaccines,", CountryName),
    subtitle = paste0("% deviation from median denominator across all vaccines that year — red = exceeds +/- ", pct_threshold * 100, "%"),
    x = "Year", y = "% Deviation from Cross-Vaccine Median",
    color = "Inconsistent"
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
#     name = t_lookup("perc_change_previous", language), 
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


# plt_denom_change <- local({
#   
#   frozen_plot_data    <- plot_data_sufficient
#   frozen_y_min        <- y_min
#   frozen_y_max        <- y_max
#   frozen_target_min   <- target_min
#   frozen_target_max   <- target_max
#   frozen_division     <- division_factor
#   frozen_pct_threshold <- pct_threshold
#   frozen_outlier_caption <- outlier_caption
#   frozen_year_min     <- min(plot_data_sufficient$Year)
#   frozen_year_max     <- max(plot_data_sufficient$Year)
#   frozen_title        <- paste0("Percent Change from Previous Year in Denominator (# Children in Target), ", .current_country, ", ", min_yr_plots, "–", rev_yr)
#   # frozen_subtitle     <- paste0("Green line shows target population counts; points show % annual change\n",
#   #                               "Red points indicate changes exceeding ±", pct_threshold * 100, "%\n",
#   #                               "Orange shading indicates vaccine stockout")
#   
#   ggplot(frozen_plot_data, aes(x = Year)) +
#     geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
#     geom_hline(yintercept = c(frozen_pct_threshold * 100, -frozen_pct_threshold * 100),
#                linetype = "dashed", color = "red", alpha = 1) +
#     geom_segment(aes(x = Year, xend = Year, y = 0, yend = pct_change_denom),
#                  color = "grey60", na.rm = TRUE) +
#     geom_point(aes(y = pct_change_denom, color = flag_denom_change), size = 1.75, na.rm = TRUE) +
#     geom_line(aes(y = target_scaled, group = Vaccine, linetype = "Raw Count of Children in Target Population"), color = "#00833D", linewidth = 0.8, alpha = 0.6) +
#     geom_point(aes(y = target_scaled), color = "#00833D", size = 1.5, na.rm = TRUE) +
#     facet_wrap(~Vaccine, scales = "fixed") +
#     scale_y_continuous(
#       name   = "Percent Change from Previous Year",
#       limits = c(frozen_y_min, frozen_y_max),
#       breaks = seq(frozen_y_min, frozen_y_max, length.out = frozen_division),
#       labels = scales::label_number(suffix = "%", accuracy = 1),
#       sec.axis = sec_axis(
#         trans  = ~ (. - frozen_y_min) / (frozen_y_max - frozen_y_min) * (frozen_target_max - frozen_target_min) + frozen_target_min,
#         name   = "Children in Target Population",
#         labels = scales::label_number(scale_cut = scales::cut_short_scale())
#       )
#     ) +
#     scale_x_continuous(
#       breaks = seq(frozen_year_min, frozen_year_max, by = 1),
#       labels = function(x) ifelse(x %% 2 == 0, as.character(x), "")
#     ) +
#     scale_color_manual(
#       values = c("FALSE" = "black", "TRUE" = "red"),
#       labels = c(
#         "FALSE" = paste0("Change ≤ ±", frozen_pct_threshold * 100, "%"),
#         "TRUE"  = paste0("Change > ±", frozen_pct_threshold * 100, "%")
#       ),
#       na.translate = FALSE
#     ) +
#     scale_linetype_manual(
#       name = NULL,
#       values = c("Raw Count of Children in Target Population" = "solid"),
#       guide = guide_legend(
#         order = 2, # Forces the green line legend entry to come first
#         override.aes = list(color = "#00833D", linewidth = 1) 
#       )
#     ) +
#     guides(
#       color = guide_legend(title = NULL, order = 1), # 1st: Black/Red percentage points
#       linetype = guide_legend(title = NULL, order = 2)  # 2nd: Green line
#     ) +
#     theme_minimal() +
#     theme(
#       axis.line.y.left   = element_line(color = "black"),
#       axis.text.x        = element_text(angle = 45, hjust = 1, size = 6.5),
#       axis.text.y.left   = element_text(color = "black", size = 8),
#       axis.ticks.x       = element_line(color = "black"),
#       axis.ticks.y       = element_line(color = "black"),
#       axis.line.y.right  = element_line(color = "#00833D"),
#       axis.text.y.right  = element_text(color = "#00833D", size = 8),
#       axis.title.y.right = element_text(color = "#00833D", size = 9),
#       panel.grid.minor   = element_blank(),
#       panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.6),
#       legend.position    = "bottom",
#       legend.box         = "horizontal",
#       strip.background   = element_rect(fill = "#0083CF"),
#       strip.text         = element_text(color = "white", face = "bold"),
#       plot.title         = element_text(hjust = 0.5, size = 14),
#       plot.subtitle      = element_text(hjust = 0.5, size = 11),
#       plot.caption       = element_text(size = 9, hjust = 0)
#     ) +
#     labs(
#       title    = frozen_title,
#       #subtitle = frozen_subtitle,
#       caption  = paste0(frozen_outlier_caption, "\n\nGaps in line indicate missing admin data for that year."),
#       x        = t_lookup("axis_year", language)
#     )
# })


## ---------------------------------------------------------------------------------------------------------------------
### Summary Table: Coverage by Vaccine and Source
## ---------------------------------------------------------------------------------------------------------------------

# # data prep
# recent_years <- sort(unique(wuenic_master_current$Year), decreasing = TRUE)[1:n_years_comparison_plot]
# 
# min_yr <- min(recent_years)
# max_yr <- max(recent_years)
# 
# summary_long <- wuenic_master_current %>%
#   filter(Year %in% recent_years) %>%
#   select(Vaccine, Year, WUENIC, Admin, Official) %>%
#   pivot_longer(cols = c(WUENIC, Admin, Official), names_to = "Source", values_to = "Coverage") %>%
#   mutate(
#     label = ifelse(is.na(Coverage), "—", sprintf("%.0f", Coverage)),
#     Source  = factor(Source, levels = c("WUENIC", "Admin", "Official")),
#     Year    = factor(Year),
#     Vaccine = factor(Vaccine, levels = rev(c("BCG", "HepBB", "DTP1", "DTP3", "Hib3", "HepB3",
#                                              "PCVC", "RotaC", "POL3", "IPV1", "IPVC",
#                                              "MCV1", "RCV1", "MCV2", "YFV", "MengA", "HPVc")))
#   ) %>% 
#   mutate(Source = recode(Source, "WUENIC" = "WHO/UNICEF", "Official" = "Official (Government Estimate)"))
# 
# survey_data_long <- wuenic_master_current %>% 
#   filter(Year %in% recent_years) %>%
#   select(Vaccine, Year, Survey) %>%
#   pivot_longer(cols = c(Survey), names_to = "Source", values_to = "Coverage") %>%
#   mutate(
#     Source  = factor(Source, levels = c("Survey")),
#     Year    = factor(Year),
#     Vaccine = factor(Vaccine, levels = rev(c("BCG", "HepBB", "DTP1", "DTP3", "Hib3", "HepB3",
#                                              "PCVC", "RotaC", "POL3", "IPV1", "IPVC",
#                                              "MCV1", "RCV1", "MCV2", "YFV", "MengA", "HPVc")))
#   )
# 
# max_cov <- max(summary_long$Coverage, na.rm = TRUE)
# 
# # plot
# plt_summary_table <- ggplot(summary_long, aes(x = Year, y = Vaccine, fill = Coverage)) +
#   geom_tile(color = "white", linewidth = 0.4) +
#   geom_text(aes(label = label, color = "white"), size = 4) +
#   scale_fill_gradientn(
#     colours  = c('#B50800', '#E2231A', '#F26A21', '#FFC20E', '#80BD41', '#00833D'),
#     values   = scales::rescale(c(0, 57, 67, 77, 87, 94, 100)),
#     guide    = "colorbar",
#     limits   = c(0, 100),
#     oob      = scales::squish,
#     breaks   = c(0, 59, 69, 79, 89, 100),
#     labels   = c("0", "60", "70", "80", "90", "100"),
#     na.value = "#d3d3d3"
#   ) +
#   scale_color_identity() +
#   facet_wrap(~Source, ncol = 3) +
#   theme_minimal() +
#   labs(
#     title = paste0(t_lookup("plt_coverage_trends_title", language), ", ", .current_country, ", ", min_yr_plots, "–", rev_yr),
#     x = t_lookup("axis_year", language), y = NULL,
#     fill = t_lookup("coverage", language),
#     caption = "WHO/UNICEF HPV coverage estimates not included in table."
#   ) +
#   theme(
#     plot.title       = element_text(hjust = 0.5, size = 14),
#     plot.subtitle    = element_text(hjust = 0.5, size = 11),
#     strip.background = element_rect(fill = "#0083CF"),
#     strip.text       = element_text(color = "white", face = "bold", size = 11),
#     panel.grid       = element_blank(),
#     axis.text.x      = element_text(size = 9),
#     axis.text.y      = element_text(size = 9),
#     panel.border     = element_rect(color = "grey80", fill = NA),
#     legend.position  = "bottom",
#     legend.key.width = unit(2, "cm")
#   )
# plt_summary_table


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
regional_info <- regional_info %>% filter(iso3c == toupper(.current_iso3c))

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


### OLD NUMERATOR PLOT CODE 
# numerator_plot <- ggplot(plot_data, aes(x = Year)) +
#   geom_rect(data = current_stockout, aes(xmin = xmin_num, xmax = xmax_num, ymin = -Inf, ymax = Inf),
#             fill = "orange", alpha = 0.2, inherit.aes = FALSE) +
#   geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
#   geom_hline(yintercept = c(pct_threshold * 100, -pct_threshold * 100),
#              linetype = "dashed", color = "red", alpha = 1) +
#   geom_segment(aes(x = Year, xend = Year, y = 0, yend = pct_change),
#                color = "grey60", na.rm = TRUE) +
#   geom_point(aes(y = pct_change, color = is_flagged), size = 2.5, na.rm = TRUE) +
#   geom_line(aes(y = (ChildrenVaccinated / max_doses) * (y_max - y_min) + y_min,
#                 group = Vaccine,
#                 linetype = "Raw Count of Children Vaccinated"), 
#             color = "#00833D", linewidth = 0.8, alpha = 0.6) +
#   geom_point(aes(y = (ChildrenVaccinated / max_doses) * (y_max - y_min) + y_min),
#              color = "#00833D", size = 1.5, na.rm = TRUE) +
#   facet_wrap(~Vaccine, scales = "fixed", drop = TRUE) +
#   scale_y_continuous(
#     name   = t_lookup("perc_change_previous", language),
#     limits = c(y_min, y_max),
#     labels = scales::label_number(suffix = "%", accuracy = 1),
#     sec.axis = sec_axis(
#       trans  = ~ (. - y_min) * scale_factor,  # y_min → 0, y_max → max_doses
#       name   = t_lookup("num_vaccinated", language),
#       labels = scales::label_number(scale_cut = scales::cut_short_scale())
#     )
#   ) +
#   scale_x_continuous(
#     breaks = seq(min(plot_data$Year), max(plot_data$Year), by = 1),
#     labels = function(x) ifelse(x %% 2 == 0, as.character(x), "")
#   ) +
#   scale_color_manual(
#     values = c("FALSE" = "black", "TRUE" = "red"),
#     labels = c(
#       "FALSE" = paste0("Change ≤ ±", pct_threshold * 100, "%"),
#       "TRUE"  = paste0("Change > ±", pct_threshold * 100, "%")
#     ),
#     na.translate = FALSE
#   ) +
#   scale_linetype_manual(
#     name = NULL,
#     values = c("Raw Counts of Children Vaccinated" = "solid"),
#     guide = guide_legend(
#       order = 2,
#       override.aes = list(color = "#00833D", linewidth = 1)
#     )
#   ) +
#   scale_fill_manual(
#     values = c("Stockout" = "orange"),
#     labels = c("Stockout" = "Vaccine Stockout")
#   ) +
#   guides(
#     color    = guide_legend(title = NULL, order = 1),  # First: Points (Black/Red)
#     linetype = guide_legend(title = NULL, order = 2),  # Second: Line (Green)
#     fill     = guide_legend(title = NULL, order = 3)   # Third: Shading (Orange Box)
#   ) +
#   theme_minimal() +
#   theme(
#     axis.line.y.left   = element_line(color = "black"),
#     axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
#     axis.ticks.x       = element_line(color = "black"),
#     axis.ticks.y       = element_line(color = "black"),
#     axis.line.y.right  = element_line(color = "#00833D"),
#     axis.text.y.right  = element_text(color = "#00833D", size = 8),
#     axis.title.y.right = element_text(color = "#00833D", size = 9),
#     panel.grid.minor   = element_blank(),
#     panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.6),
#     legend.position    = "bottom",
#     legend.box         = "horizontal",
#     strip.background   = element_rect(fill = "#0083CF"),
#     strip.text         = element_text(color = "white", face = "bold"),
#     plot.title         = element_text(hjust = 0.5, size = 14),
#     plot.subtitle      = element_text(hjust = 0.5, size = 11)
#   ) +
#   labs(
#     title = paste0(
#       "Percent Change from Previous Year in Numerator (# Children Vaccinated), ",
#       .current_country, ", ", min_yr_plots, "–", rev_yr,
#       "\nVaccines Administered at ", time_point, " visit"
#     ),
#     # subtitle = paste0(
#     #   "Line shows raw counts; points show % annual change\n",
#     #   "Red points indicate changes exceeding +/- ", pct_threshold * 100, "%\n",
#     #   "Orange shading indicates vaccine stockout"
#     # ),
#     x = t_lookup("axis_year", language)
#   )


## ---------------------------------------------------------------------------------------------------------------------
### Coverage Line Plots for all 4 Indicators (WUENIC, Admin, Official, Survey)
## ---------------------------------------------------------------------------------------------------------------------

all_line_data <- wuenic_master_current %>% 
  arrange(Vaccine, Year) %>%
  group_by(Vaccine) %>%
  pivot_longer(cols = c(WUENIC, Admin, Official, Survey), names_to = "Source", values_to = "Coverage") %>%
  ungroup() %>% 
  mutate(Source = recode(Source, "WUENIC" = "WUENIC", "Official" = "Official Estimate"))

min_cov <- floor(min(all_line_data$Coverage, na.rm = TRUE) / 25) * 25
max_cov <- ceiling(max(all_line_data$Coverage, na.rm = TRUE) / 10) * 10

source_colors_line <- c("WUENIC" = "blue", "Admin" = "#e93626", 
                        "Official Estimate"= "lightpink", "Survey" = "green")

source_shapes <- c("WUENIC" = 16, "Admin" = 8, "Official Estimate" = 16, "Survey" = 17)

# for most recent year, find the difference between the admin and wuenic
gap_admin_wuenic <- all_line_data %>% 
  filter(Year == rev_yr) %>% 
  filter(Source %in% c("Admin", "WUENIC")) %>% 
  group_by(Vaccine) %>% 
  pivot_wider(names_from = Source, values_from = Coverage) %>% 
  mutate(gap = Admin - `WUENIC`) %>% 
  select(Vaccine, Year, Admin, `WUENIC`, gap)

data_year <- all_line_data %>% filter(Source == "WUENIC") %>% group_by(Vaccine) %>% slice(n()) %>% pull(Year) %>% unique()

# set the bottom and top year for the bracket betweeb rev_yr admin and wuenic estimates
bracket_data <- all_line_data %>%
  filter(Year == rev_yr, Source %in% c("WUENIC", "Admin")) %>%
  select(Vaccine, Year, Source, Coverage) %>%
  tidyr::pivot_wider(names_from = Source, values_from = Coverage) %>%
  mutate(
    y_max = pmax(`WUENIC`, Admin, na.rm = TRUE), 
    y_min = pmin(`WUENIC`, Admin, na.rm = TRUE), 
    x_position = rev_yr + 1.4,
    cap_width = 0.5
  )

plt_all_vax_line <- ggplot(all_line_data, aes(x = Year, y = Coverage, color = Source, shape = Source)) +
  geom_line(data = all_line_data %>% filter(Source == "WUENIC"), size = 0.8, alpha = 0.7) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  geom_point(data = all_line_data %>% filter(Source %in% c("Survey", "Official Estimate")), size = 2.7, alpha = 0.8) +
  geom_point(data = all_line_data %>% filter(Source == "Admin"), size = 1.7, alpha = 0.9) +
  facet_wrap(~Vaccine, scales = "fixed") +
  geom_text(data = gap_admin_wuenic, aes(x = (data_year - 9.5), y = 0, label = paste0(t_lookup("label_admin_wuenic_gap", language), ": ", gap, t_lookup("unit_pp", language))),
            color = "#6A1E74", size = 2.5, vjust = 0, hjust = 0.5, inherit.aes = FALSE) +
  geom_segment(data = gap_admin_wuenic %>% filter(Year == rev_yr), aes(x = Year, xend = Year, y = 13, yend = 27), # arrow showing the gap is from rev_yr
               arrow = arrow(length = unit(0.15, "cm"), type = "open"), color = "#6A1E74", linewidth = 0.8, inherit.aes = FALSE) +
  geom_vline(data = gap_admin_wuenic %>% filter(Year == rev_yr), aes(xintercept = Year), color = "#6A1E74", linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE, alpha = 0.3) +
  # bracket between admin and wuenic for the most recent year
  geom_segment(data = bracket_data, aes(x = x_position, xend = x_position, y = y_min, yend = y_max), color = "#6A1E74", linewidth = 0.7, inherit.aes = FALSE) +
  geom_segment(data = bracket_data, aes(x = x_position, xend = x_position - cap_width, y = y_max, yend = y_max), color = "#6A1E74", linewidth = 0.7, inherit.aes = FALSE) +
  geom_segment(data = bracket_data, aes(x = x_position, xend = x_position - cap_width, y = y_min, yend = y_min), color = "#6A1E74", linewidth = 0.7, inherit.aes = FALSE) +
  scale_color_manual(
    values = source_colors_line,
    labels = c(
      "WUENIC" = t_lookup("source_who_unicef", language),
      "Admin" = t_lookup("source_admin",    language),
      "Official Estimate" = t_lookup("source_official", language),
      "Survey" = t_lookup("source_survey",   language)
    )
  ) +
  scale_shape_manual(
    values = source_shapes,
    labels = c(
      "WUENIC" = t_lookup("source_who_unicef", language),
      "Admin" = t_lookup("source_admin",    language),
      "Official Estimate" = t_lookup("source_official", language),
      "Survey" = t_lookup("source_survey",   language)
    )
  ) +
  scale_x_continuous(
    breaks = seq(min(all_line_data$Year), max(all_line_data$Year), by = 2),
    limits = c(min(all_line_data$Year), max(all_line_data$Year) + 1.5)
  ) +
  scale_y_continuous(limits = c(0, max_cov), breaks = seq(0, max_cov, by = 25), labels = scales::label_number(suffix = "%")) +
  theme_minimal() +
  labs(
    title = paste0(t_lookup("plt_coverage_trends_title", language), ", ", .current_country, ", ", min_yr_plots, "–", rev_yr),
    x = t_lookup("axis_year", language), y = t_lookup("coverage", language), color = t_lookup("legend_data_source", language), shape = t_lookup("legend_data_source", language)
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
### Plot: Admin coverage vs WUENIC estimate — large gaps flagged (using wuenic_master_current)
## ---------------------------------------------------------------------------------------------------------------------

make_admin_wuenic_plot <- function(pct_threshold) {
  
  gap_data <- wuenic_master_current %>%
    mutate(gap = Admin - WUENIC, flag_large_gap = abs(gap) > pct_threshold * 100, `WUENIC` = WUENIC)
  
  avg_gap_last_5_years <- gap_data %>%
    filter(Year >= (rev_yr - 4)) %>%
    group_by(Vaccine) %>%
    summarise(avg_gap = mean(gap, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(abs(avg_gap)))
  
  gap_label <- paste0(t_lookup("label_large_gap", language), pct_threshold * 100, t_lookup("unit_pp", language), ")")
  gap_colors <- setNames("#ed6a64", gap_label)
  
  avg_gap_label <- paste0(t_lookup("label_avg_gap", language), " ", sprintf("%.1f", avg_gap_last_5_years$avg_gap), t_lookup("unit_pp", language))
  
  y_min <- floor(min(gap_data$Admin, na.rm = TRUE) / 25) * 25
  y_bracket <- y_min + 15
  y_bracket_cap <- y_min + 18
  
  ggplot(gap_data, aes(x = Year)) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
    geom_line(aes(y = `WUENIC`, linetype = "WUENIC"), color = source_colors["WUENIC"], linewidth = 0.9, alpha = 0.7) +
    geom_line(aes(y = Admin, linetype = "Admin"), color = source_colors["Admin"], linewidth = 0.9, alpha = 0.7) +
    geom_segment(data = gap_data %>% filter(flag_large_gap == TRUE),
                 aes(x = Year, xend = Year, y = Admin, yend = `WUENIC`, color = gap_label),
                 linewidth = 0.8, alpha = 0.7, key_glyph = draw_key_vpath) +
    geom_text(data = avg_gap_last_5_years,
              aes(x = rev_yr, y = y_min + 5, label = paste0(t_lookup("label_avg_gap", language), " ", sprintf("%.1f", avg_gap), t_lookup("unit_pp", language))),
              color = "#00833D", size = 2.7, hjust = 1) +
    geom_segment(data = avg_gap_last_5_years, aes(x = rev_yr - 4, xend = rev_yr, y = y_bracket, yend = y_bracket), color = "#00833D", linewidth = 0.5, inherit.aes = FALSE) +
    geom_segment(data = avg_gap_last_5_years, aes(x = rev_yr - 4, xend = rev_yr - 4, y = y_bracket, yend = y_bracket_cap), color = "#00833D", linewidth = 0.5, inherit.aes = FALSE) +
    geom_segment(data = avg_gap_last_5_years, aes(x = rev_yr, xend = rev_yr, y = y_bracket, yend = y_bracket_cap), color = "#00833D", linewidth = 0.5, inherit.aes = FALSE) +
    facet_wrap(~Vaccine) +
    scale_linetype_manual(
      values = c("WUENIC" = "solid", "Admin" = "solid"),
      labels = c("WUENIC" = t_lookup("source_who_unicef", language), "Admin" = t_lookup("source_admin", language))
    ) +
    scale_color_manual(name = NULL, values = gap_colors, guide = guide_legend(order = 2, override.aes = list(shape = NA, linewidth = 1.2))) +
    scale_y_continuous(limits = c(y_min, ceiling(max(gap_data$Admin, na.rm = TRUE) / 10) * 10),
                       breaks = seq(0, 100, by = 25), labels = scales::label_number(suffix = "%")) +
    scale_x_continuous(breaks = seq(min(gap_data$Year), max(gap_data$Year), by = 2)) +
    guides(linetype = guide_legend(title = t_lookup("legend_data_source", language), order = 1), color = guide_legend(title = NULL, order = 2)) +
    labs(
      title   = paste0(get_text2("txt_title_admin_wuenic", text_vars), ", ", .current_country, ", ", min_yr_plots, "–", rev_yr),
      x       = t_lookup("axis_year", language),
      y       = t_lookup("coverage", language),
      linetype = t_lookup("legend_data_source", language),
      caption = get_text2("txt_caption_average_gap", text_vars)
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom", legend.box = "horizontal",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.ticks.x = element_line(color = "black"),
      axis.ticks.y = element_line(color = "black"),
      panel.border = element_rect(color = "black", fill = NA, size = 0.6),
      strip.background = element_rect(fill = "#0083CF"),
      strip.text = element_text(color = "white", face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 14),
      plot.caption = element_text(size = 9, color = "#00833D")
    )
}

plt_admin_wuenic_1 <- make_admin_wuenic_plot(pct_threshold)
plt_admin_wuenic_2 <- make_admin_wuenic_plot(second_pct_threshold)

## ---------------------------------------------------------------------------------------------------------------------
### Plot: Admin coverage vs Official coverage — large gaps flagged (using wiise_admin_official)
## ---------------------------------------------------------------------------------------------------------------------

gap_data3 <- wuenic_master_current %>%
  mutate(
    gap = Admin - Official,
    flag_large_gap = abs(gap) > pct_threshold*100
  ) %>% 
  rename(`Official Estimate` = Official)

# get average gap in the last 5 years for each vaccine
avg_gap_last_5_years_3 <- gap_data3 %>%
  filter(Year >= (rev_yr - 4)) %>%
  group_by(Vaccine) %>%
  summarise(avg_gap = mean(gap, na.rm = TRUE)) %>%
  arrange(desc(abs(avg_gap)))

plt_admin_vs_official <- ggplot(gap_data3, aes(x = Year)) +
  geom_line(aes(y = `Official Estimate`, linetype = "Official Estimate"), color = source_colors["Official Estimate"], linewidth = 0.9, alpha = 0.7) +
  geom_line(aes(y = Admin, linetype = "Admin"), color = source_colors["Admin"], linewidth = 0.9, alpha = 0.7) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  geom_segment(data = gap_data3 %>% filter(flag_large_gap == TRUE),
               aes(x = Year, xend = Year, y = Admin, yend = `Official Estimate`),
               color = "#ed6a64", linewidth = 0.8, alpha = 0.7) +
  geom_text(data = avg_gap_last_5_years_3,
            aes(x = rev_yr, y = (floor(min(gap_data3$Admin, na.rm = TRUE) / 25) * 25) + 5,
                label = paste0(t_lookup("label_avg_gap", language), " ", sprintf("%.1f", avg_gap), t_lookup("unit_pp", language))),
            color = "#00833D", size = 2.7, hjust = 1) +
  geom_segment(data = avg_gap_last_5_years_3, aes(x = rev_yr - 4, xend = rev_yr, y = (floor(min(gap_data3$Admin, na.rm = TRUE) / 25) * 25) + 15, yend = (floor(min(gap_data3$Admin, na.rm = TRUE) / 25) * 25) + 15), color = "#00833D", linewidth = 0.5, inherit.aes = FALSE) +
  geom_segment(data = avg_gap_last_5_years_3, aes(x = rev_yr - 4, xend = rev_yr - 4, y = (floor(min(gap_data3$Admin, na.rm = TRUE) / 25) * 25) + 15, yend = (floor(min(gap_data3$Admin, na.rm = TRUE) / 25) * 25) + 17), color = "#00833D", linewidth = 0.5, inherit.aes = FALSE) +
  geom_segment(data = avg_gap_last_5_years_3, aes(x = rev_yr, xend = rev_yr, y = (floor(min(gap_data3$Admin, na.rm = TRUE) / 25) * 25) + 15, yend = (floor(min(gap_data3$Admin, na.rm = TRUE) / 25) * 25) + 17), color = "#00833D", linewidth = 0.5, inherit.aes = FALSE) +
  facet_wrap(~Vaccine) +
  scale_linetype_manual(
    values = c("Official Estimate" = "solid", "Admin" = "solid"),
    labels = c("Official Estimate" = t_lookup("source_official", language), "Admin" = t_lookup("source_admin", language))
  ) +
  scale_y_continuous(limits = c(floor(min(gap_data3$Admin, na.rm = TRUE) / 25) * 25, ceiling(max(gap_data3$Admin, na.rm = TRUE) / 10) * 10),
                     breaks = seq(0, 100, by = 25), labels = scales::label_number(suffix = "%")) +
  scale_x_continuous(breaks = seq(min(gap_data3$Year), max(gap_data3$Year), by = 2)) +
  guides(linetype = guide_legend(title = t_lookup("legend_data_source", language), order = 1)) +
  labs(
    title   = paste0(get_text2("txt_title_admin_official", text_vars), ", ", .current_country, ", ", min_yr_plots, "–", rev_yr),
    x       = t_lookup("axis_year", language),
    y       = t_lookup("coverage", language),
    linetype = t_lookup("legend_data_source", language),
    caption = get_text2("txt_caption_average_gap", text_vars)
  ) +
  theme_minimal() +
  theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.ticks.x     = element_line(color = "black"),
    axis.ticks.y     = element_line(color = "black"),
    panel.border     = element_rect(color = "black", fill = NA, size = 0.6),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text       = element_text(color = "white", face = "bold"),
    plot.title       = element_text(hjust = 0.5, size = 14),
    plot.caption     = element_text(size = 9, color = "#00833D")
  )
plt_admin_vs_official


## ---------------------------------------------------------------------------------------------------------------------
### Coverage Heatmap (WUENIC data) -- removed from supplement
## ---------------------------------------------------------------------------------------------------------------------

hpv <- hpv_dta %>%
  filter(lvl_2 == "region_unicef_ops", iso3c == x, year >= 2000)

## plt_all_vax_heatmap :: heatmap ----
# includes stock-out data
source(file.path(PrjDir, "R/all_vax_heatmap.R"))


### OLD RUN LOOP WITHOUT TRANSLATIONS
# for (current_country in countries) {
#   
#   cat("Processing:", current_country, "\n")
#   
#   # global variable that 'DQ_ppt_formatted.R' should use to filter data
#   .current_country <- current_country 
#   
#   # get iso3c for this country
#   wuenic_master <- read.csv(file.path(DummyDataDir, "wuenic-master_2025rev.csv"))
#   .current_iso3c <- wuenic_master %>% filter(Country == current_country) %>% pull(ISOCountryCode) %>% unique()
#   x <- tolower(.current_iso3c)
#   
#   # execute DQ_ppt_formatted.R
#   tryCatch({
#     suppressWarnings(
#       suppressMessages(
#         source(ppt_script_path, local = FALSE)
#       )
#     )
#     cat("✅ Successfully generated PPT for:", current_country, "\n\n")
#   }, error = function(e) {
#     cat("❌ ERROR processing:", current_country, "\n")
#     message(e)
#   })
# }



#### NUMERATOR PLOT WITH PERCENT CHANGE LINE (ORIGINAL VERSION)

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

# ── EARLY EXIT IF NO DATA ─────────────────────────────────────────────────────
if (nrow(plot_data_numerators_all) == 0) {
  message("Skipping ", .current_country, ": no numerator data available")
  plt_perc_change_line <- NULL
  next
}

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

# vaccines with numerator data but no consecutive years of data (cannot calculate year-to-year change)
vaccines_no_consecutive_data <- plot_data_numerators_all %>%
  filter(Vaccine %in% vaccines_with_numerator_data) %>%
  arrange(Vaccine, Year) %>%
  group_by(Vaccine) %>%
  summarise(
    has_consecutive = any(!is.na(ChildrenVaccinated) & !is.na(lag(ChildrenVaccinated))),
    .groups = "drop"
  ) %>%
  filter(!has_consecutive) %>%
  pull(Vaccine)

# ── SCALING (only on vaccines with data) ──────────────────────────────────────
plot_data_for_scaling <- plot_data_numerators_all %>% filter(Vaccine %in% vaccines_with_numerator_data)

# check if there is any numerator & percent change data
if (!is.null(plot_data_for_scaling) && nrow(plot_data_for_scaling) > 0) {
  max_doses_num <- max(plot_data_for_scaling$ChildrenVaccinated, na.rm = TRUE)
  
  pct_vals <- plot_data_for_scaling$pct_change
  max_pct_num <- if (all(is.na(pct_vals))) NA else max(abs(pct_vals), na.rm = TRUE)
} else {
  max_doses_num <- NA
  max_pct_num   <- NA
}

if (is.na(max_pct_num) || max_pct_num == 0) max_pct_num <- 0.1

if (max_pct_num > 500) {
  y_max_num <- ceiling(max_pct_num / 100) * 100
  step_size       <- 100
  division_factor <- ((ifelse(y_max_num > 0, y_max_num / step_size, 1)) * 2) + 1
} else if (max_pct_num > 100) {
  y_max_num <- ceiling(max_pct_num / 50) * 50
  step_size       <- 50
  division_factor <- ((ifelse(y_max_num > 0, y_max_num / step_size, 1)) * 2) + 1
} else if (max_pct_num <= 100) {
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
  
  plot_data_sufficient <- plot_data_sufficient %>%
    dplyr::mutate(pct_color = dplyr::case_when(
      abs(pct_change) > 10 ~ "high",
      abs(pct_change) >= 5 ~ "mid",
      !is.na(pct_change)   ~ "low"
    ))
  
  plt_perc_change_line <- local({
    
    frozen_plot_data    <- plot_data_sufficient
    frozen_stockout     <- stockout_sufficient
    frozen_max_doses    <- max_doses_num
    frozen_y_min        <- y_min_num
    frozen_y_max        <- y_max_num
    frozen_scale_factor <- max_doses_num / (y_max_num - y_min_num)
    frozen_division     <- division_factor
    frozen_pct_threshold <- pct_threshold
    frozen_year_min     <- min(plot_data_sufficient$Year)
    frozen_year_max     <- max(plot_data_sufficient$Year)
    frozen_title        <- paste0(get_text2("txt_num_change_title", text_vars), ", ", .current_country, ", ", min_yr_plots, "–", rev_yr)
    
    raw_count_label <- get_text2("txt_raw_count", text_vars)
    
    ggplot(frozen_plot_data, aes(x = Year)) +
      geom_rect(data = frozen_stockout, aes(xmin = xmin_num, xmax = xmax_num, ymin = -Inf, ymax = Inf, fill = "Stockout"), alpha = 0.2, inherit.aes = FALSE) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
      geom_line(aes(y = pct_change, group = Vaccine), color = "grey60", linewidth = 0.6, na.rm = TRUE) +
      geom_point(aes(y = pct_change, color = pct_color), size = 1.75, na.rm = TRUE) +
      geom_line(aes(y = (ChildrenVaccinated / frozen_max_doses) * (frozen_y_max - frozen_y_min) + frozen_y_min, group = Vaccine, linetype = raw_count_label), color = "#0058AB", linewidth = 0.8, alpha = 0.6) +
      geom_point(aes(y = (ChildrenVaccinated / frozen_max_doses) * (frozen_y_max - frozen_y_min) + frozen_y_min), color = "#0058AB", size = 1.5, na.rm = TRUE) +
      facet_wrap(~Vaccine, scales = "fixed") +
      scale_y_continuous(
        name = t_lookup("perc_change_previous", language), limits = c(frozen_y_min, frozen_y_max),
        breaks = seq(frozen_y_min, frozen_y_max, length.out = frozen_division), labels = scales::label_number(suffix = "%", accuracy = 1),
        sec.axis = sec_axis(trans = ~ (. - frozen_y_min) * frozen_scale_factor, name = t_lookup("num_vaccinated", language), labels = scales::label_number(scale_cut = scales::cut_short_scale()))
      ) +
      scale_x_continuous(breaks = seq(frozen_year_min, frozen_year_max, by = 1), labels = function(x) ifelse(x %% 2 == 0, as.character(x), "")) +
      scale_color_manual(
        name = NULL, values = c("low" = "#2CA02C", "mid" = "orange", "high" = "red"),
        labels = c("low" = t_lookup("label_change_low", language), "mid" = t_lookup("label_change_mid", language), "high" = t_lookup("label_change_high", language)),
        breaks = c("high", "mid", "low"), na.translate = FALSE
      ) +
      scale_linetype_manual(name = NULL, values = setNames("solid", raw_count_label), guide = guide_legend(order = 2, override.aes = list(color = "#0058AB", linewidth = 1))) +
      scale_fill_manual(values = c("Stockout" = "orange"), labels = setNames(t_lookup("vaccine_stockout", language), "Stockout")) +
      guides(color = guide_legend(title = NULL, order = 1), linetype = guide_legend(title = NULL, order = 2), fill = guide_legend(title = NULL, order = 3)) +
      theme_minimal() +
      theme(
        axis.line.y.left = element_line(color = "black"), axis.text.x = element_text(angle = 45, hjust = 1, size = 7.5),
        axis.text.y.left = element_text(color = "black", size = 7), axis.ticks.x = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"), axis.line.y.right = element_line(color = "#0058AB"),
        axis.text.y.right = element_text(color = "#0058AB", size = 8), axis.title.y.right = element_text(color = "#0058AB", size = 9),
        panel.grid.minor = element_blank(), panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
        legend.position = "bottom", legend.box = "horizontal", strip.background = element_rect(fill = "#0083CF"),
        strip.text = element_text(color = "white", face = "bold"), plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11), plot.caption = element_text(size = 9)
      ) +
      labs(title = frozen_title, caption = get_text2("txt_caption_missing_admin", text_vars), x = t_lookup("axis_year", language))
  })
} else {
  plt_perc_change_line <- ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = paste0("No vaccines with available numerator data for ", .current_country),
             color = "red", size = 6, hjust = 0.5, vjust = 0.5) +
    theme_void()
}

# add caption for vaccines with no numerator data or insufficient consecutive data to calculate percent change
if (!is.null(plt_perc_change_line)) {
  caption_parts <- c()
  if (length(vaccines_no_numerator_data) > 0) {
    caption_parts <- c(caption_parts,
                       paste0("No numerator data: ", paste(vaccines_no_numerator_data, collapse = ", "))
    )
  }
  if (length(vaccines_no_consecutive_data) > 0) {
    caption_parts <- c(caption_parts,
                       paste0("Numerator data exists but no consecutive years to calculate percent change: ",
                              paste(vaccines_no_consecutive_data, collapse = ", "))
    )
  }
  if (length(caption_parts) > 0) {
    plt_perc_change_line <- plt_perc_change_line +
      labs(caption = paste0("NOTE: ", paste(caption_parts, collapse = "\n"))) +
      theme(
        plot.caption = element_text(color = "red", size = 12)
      )
  }
}
plt_perc_change_line



#### ORIGINAL DENOMINATOR WITH PERCENT CHANGE LINE
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
    frozen_plot_data <- plot_data_sufficient
    frozen_y_min <- y_min
    frozen_y_max <- y_max
    frozen_target_min <- target_min
    frozen_target_max <- target_max
    frozen_division <- division_factor
    frozen_pct_threshold <- pct_threshold
    frozen_outlier_caption <- outlier_caption
    frozen_year_min <- min(plot_data_sufficient$Year)
    frozen_year_max <- max(plot_data_sufficient$Year)
    frozen_title <- paste0(get_text2("txt_den_change_title", text_vars), ",\n", .current_country, ", ", min_yr_plots, "–", rev_yr)
    
    target_pop_label <- get_text2("txt_target_pop", text_vars)
    
    ggplot(frozen_plot_data, aes(x = Year)) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
      geom_line(aes(y = pct_change_denom, group = Vaccine), color = "grey60", linewidth = 0.6, na.rm = TRUE) +
      geom_point(aes(y = pct_change_denom, color = case_when(abs(pct_change_denom) > 10 ~ "high", abs(pct_change_denom) >= 5 ~ "mid", !is.na(pct_change_denom) ~ "low")), size = 2.5, na.rm = TRUE) +
      geom_line(aes(y = target_scaled, group = Vaccine, linetype = target_pop_label), color = "#0058AB", linewidth = 0.8, alpha = 0.6, na.rm = TRUE) +
      geom_point(aes(y = target_scaled), color = "#0058AB", size = 1.5, na.rm = TRUE) +
      facet_wrap(~Vaccine, scales = "fixed") +
      scale_y_continuous(
        name = t_lookup("perc_change_previous", language), limits = c(frozen_y_min, frozen_y_max), breaks = seq(frozen_y_min, frozen_y_max, length.out = frozen_division), labels = scales::label_number(suffix = "%", accuracy = 1), 
        sec.axis = sec_axis(trans = ~ (. - frozen_y_min) / (frozen_y_max - frozen_y_min) * (frozen_target_max - frozen_target_min) + frozen_target_min, name = target_pop_label, labels = scales::label_number(scale_cut = scales::cut_short_scale()))
      ) +
      scale_x_continuous(breaks = seq(frozen_year_min, frozen_year_max, by = 1), labels = function(x) ifelse(x %% 2 == 0, as.character(x), "")) +
      scale_color_manual(
        name = NULL, values = c("high" = "#E2231A", "mid" = "#Ff7f00", "low" = "#80BD41"), 
        labels = c("high" = t_lookup("label_change_high", language), "mid" = t_lookup("label_change_mid", language), "low" = t_lookup("label_change_low", language)), breaks = c("high", "mid", "low"), na.translate = FALSE
      ) +
      scale_linetype_manual(name = NULL, values = setNames("solid", target_pop_label), guide = guide_legend(order = 2, override.aes = list(color = "#0058AB", linewidth = 1))) +
      guides(color = guide_legend(title = NULL, order = 1), linetype = guide_legend(title = NULL, order = 2)) +
      theme_minimal() +
      theme(
        axis.line.y.left = element_line(color = "black"), axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5), axis.text.y.left = element_text(color = "black", size = 8), axis.ticks.x = element_line(color = "black"), axis.ticks.y = element_line(color = "black"), 
        axis.line.y.right = element_line(color = "#0058AB"), axis.text.y.right = element_text(color = "#0058AB", size = 8), axis.title.y.right = element_text(color = "#0058AB", size = 9), panel.grid.minor = element_blank(), panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6), 
        legend.position = "bottom", legend.box = "horizontal", strip.background = element_rect(fill = "#0083CF"), strip.text = element_text(color = "white", face = "bold"), plot.title = element_text(hjust = 0.5, size = 14), plot.caption = element_text(size = 9)
      ) +
      labs(title = frozen_title, caption = paste0(frozen_outlier_caption, "\n\n", get_text2("txt_caption_missing_admin", text_vars)), x = t_lookup("axis_year", language))
  })
} else {
  plt_denom_change <- ggplot() +
    annotate("text",
             x = 0.5, y = 0.5,
             label = paste0(get_text2("txt_no_denom_avail_msg", text_vars), " ", .current_country),
             color = "red", size = 6, hjust = 0.5, vjust = 0.5) +
    theme_void()
}

# ── APPEND NO-DATA NOTE TO CAPTION IF NEEDED ─────────────────────────────────
if (length(vaccines_no_denominator_data) > 0) {
  no_data_note <- paste0(
    get_text2("txt_note_no_denom_avail", text_vars), " ",
    paste(vaccines_no_denominator_data, collapse = ", ")
  )
  plt_denom_change <- plt_denom_change +
    labs(caption = paste(c(outlier_caption, no_data_note)[nchar(c(outlier_caption, no_data_note)) > 0],
                         collapse = "\n")) +
    theme(plot.caption = element_text(color = "red", size = 12, hjust = 1))
}

plt_denom_change



### OLD DENOM PLOT 3: ADMIN DENOMINATORS FOR BCG AND DTP
denom_types_data <- wuenic_master_current %>%
  filter(Vaccine %in% c("BCG", "DTP1")) %>%
  mutate(DenomType = case_when(Vaccine == "BCG" ~ "Live Births", Vaccine == "DTP1" ~ "Surviving Infants")) %>%
  select(Country, Vaccine, Year, DenomType, ChildrenInTarget) %>%
  group_by(Vaccine) %>%
  arrange(Vaccine, Year) %>%
  mutate(
    prev_denominator = lag(ChildrenInTarget),
    pct_change_denom = (ChildrenInTarget - prev_denominator) / prev_denominator,
    flag_denom_change = abs(pct_change_denom) > pct_threshold
    # ) %>%
    # ungroup()
  ) %>% 
  mutate(DenomType = case_when(
    Vaccine == "BCG" ~ t_lookup("caption_live_births", language),
    Vaccine == "DTP1" ~ t_lookup("caption_si", language),
    TRUE ~ DenomType
  ))

y_limit_dt <- max(abs(denom_types_data$pct_change_denom), na.rm = TRUE)
if(y_limit_dt == 0 || is.na(y_limit_dt)) y_limit_dt <- 0.1
primary_max_dt <- y_limit_dt
primary_min_dt <- -y_limit_dt
sec_max_dt <- max(denom_types_data$ChildrenInTarget, na.rm = TRUE)
scale_dt <- sec_max_dt / (primary_max_dt - primary_min_dt)
offset_dt <- sec_max_dt - (scale_dt * primary_max_dt)

denom_data_available <- denom_types_data %>% filter(!is.na(ChildrenInTarget)) %>% nrow() > 1

if(denom_data_available) {
  plt_denom_pct_change <- local({
    
    frozen_data <- denom_types_data %>%
      dplyr::mutate(pct_color = dplyr::case_when(
        abs(pct_change_denom) > pct_threshold ~ "high",
        abs(pct_change_denom) >= pct_threshold * 0.5 ~ "mid",
        !is.na(pct_change_denom) ~ "low"
      ))
    frozen_year_min <- min(denom_types_data$Year)
    frozen_year_max <- max(denom_types_data$Year)
    
    ggplot(frozen_data, aes(x = Year)) +
      geom_hline(yintercept = 0, color = "black") +
      geom_vline(xintercept = seq(frozen_year_min, frozen_year_max, by = 1), color = "grey90", linewidth = 0.3) +
      geom_line(aes(y = pct_change_denom, group = DenomType), color = "grey60", linewidth = 0.6, na.rm = TRUE) +
      geom_point(aes(y = pct_change_denom, color = pct_color), size = 2.5, na.rm = TRUE) +
      geom_line(aes(y = (ChildrenInTarget - offset_dt) / scale_dt, group = DenomType,
                    linetype = get_text2("txt_raw_denom", text_vars)),
                color = "#0058AB", linewidth = 0.8, alpha = 0.6, na.rm = TRUE) +
      geom_point(aes(y = (ChildrenInTarget - offset_dt) / scale_dt), color = "#0058AB", size = 1.5, na.rm = TRUE) +
      facet_wrap(~DenomType) +
      scale_y_continuous(
        name = t_lookup("perc_change_previous", language),
        limits = c(primary_min_dt, primary_max_dt),
        labels = scales::percent_format(),
        sec.axis = sec_axis(~ . * scale_dt + offset_dt, name = get_text2("txt_raw_denom", text_vars),
                            labels = scales::label_number(scale_cut = scales::cut_short_scale()))
      ) +
      scale_x_continuous(
        breaks = seq(frozen_year_min, frozen_year_max, by = 1),
        labels = function(x) ifelse(x %% 2 == 0, as.character(x), "")
      ) +
      scale_color_manual(
        name = NULL,
        values = c("low" = "#80BD41", "mid" = "#Ff7f00", "high" = "#E2231A"),
        labels = c("low" = t_lookup("label_change_low", language),
                   "mid" = t_lookup("label_change_mid", language),
                   "high" = t_lookup("label_change_high", language)),
        breaks = c("high", "mid", "low"), na.translate = FALSE
      ) +
      scale_linetype_manual(
        name = NULL,
        values = setNames("solid", get_text2("txt_raw_denom", text_vars)),
        guide = guide_legend(order = 2, override.aes = list(color = "#0058AB", linewidth = 1))
      ) +
      guides(
        color    = guide_legend(title = NULL, order = 1),
        linetype = guide_legend(title = NULL, order = 2)
      ) +
      theme_minimal() +
      theme(
        legend.position    = "bottom", legend.box = "horizontal",
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.6),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        strip.background   = element_rect(fill = "#0083CF"),
        strip.text         = element_text(color = "white", face = "bold"),
        axis.title.y.right = element_text(color = "#0058AB"),
        axis.text.y.right  = element_text(color = "#0058AB"),
        axis.line.y.right  = element_line(color = "#0058AB"),
        axis.ticks.x       = element_line(color = "black"),
        axis.ticks.y       = element_line(color = "black"),
        axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
        plot.title         = element_text(hjust = 0.5, size = 14),
        plot.subtitle      = element_text(hjust = 0.5, size = 11)
      ) +
      labs(
        title   = paste0(get_text2("txt_denom_stability", text_vars), ", ", .current_country, ", ", min_yr_plots, "–", rev_yr),
        x       = t_lookup("axis_year", language),
        caption = get_text2("txt_caption_missing_admin", text_vars)
      )
  })
} else {
  plt_denom_pct_change <- local({
    ggplot() +
      geom_text(aes(x = 1, y = 1, label = "Not enough denominator data to compute % change"), color = "red", size = 5) +
      theme_void() +
      labs(title = paste0("Denominator Stability Check, ", .current_country, ", ", min_yr_plots, "–", rev_yr), subtitle = "Insufficient data for year-to-year % change calculation") +
      theme(plot.title = element_text(hjust = 0.5, size = 14), plot.subtitle = element_text(hjust = 0.5, size = 11))
  })
}