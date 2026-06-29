# ==========================================================================================================================================
# Script Name: Figures and Tables for 2025 WUENIC Data Quality Powerpoint
# Author: Grace Legris, glegris@unicef.org
# ==========================================================================================================================================

## ---------------------------------------------------------------------------------------------------------------------

source_colors <- c("WUENIC" = "#0083CF", "Admin" = "#6A1E74", "Official Estimate" = "#80BD41", "Survey" = "#FFC20E")
flag_colors <- c("FALSE" = "black", "TRUE" = "red")

## ---------------------------------------------------------------------------------------------------------------------

# ======================================================================================================================
### Coverage & Outlier Detection
# ======================================================================================================================

## ---------------------------------------------------------------------------------------------------------------------
### DTP1, DTP3, MCV 1: Coverage Line Plots for all 4 Indicators (WUENIC, Admin, Official, Survey)
## ---------------------------------------------------------------------------------------------------------------------

all_line_data <- wuenic_master_current %>% 
  arrange(Vaccine, Year) %>%
  group_by(Vaccine) %>%
  pivot_longer(cols = c(WUENIC, Admin, Official, Survey), names_to = "Source", values_to = "Coverage") %>%
  ungroup() %>% 
  mutate(Source = recode(Source, "WUENIC" = "WUENIC", "Official" = "Official Estimate"))

selected_line_data <- all_line_data %>% 
  filter(Vaccine %in% c("DTP1", "DTP3", "MCV1"))

source_colors_line <- c("WUENIC" = "blue", "Admin" = "#e93626", 
                        "Official Estimate"= "lightpink", "Survey" = "green")

source_shapes <- c("WUENIC" = 16, "Admin" = 8, "Official Estimate" = 16, "Survey" = 17)

min_cov <- floor(min(selected_line_data$Coverage, na.rm = TRUE) / 25) * 25
max_cov <- ceiling(max(selected_line_data$Coverage, na.rm = TRUE) / 10) * 10

# for most recent year, find the difference between the admin and wuenic
gap_admin_wuenic_small <- selected_line_data %>% 
  filter(Year == rev_yr) %>% 
  filter(Source %in% c("Admin", "WUENIC")) %>% 
  group_by(Vaccine) %>% 
  pivot_wider(names_from = Source, values_from = Coverage) %>% 
  mutate(gap = Admin - `WUENIC`) %>% 
  select(Vaccine, Year, Admin, `WUENIC`, gap)

data_year <- all_line_data %>% filter(Source == "WUENIC") %>% group_by(Vaccine) %>% slice(n()) %>% pull(Year) %>% unique()

# set the bottom and top year for the bracket betweeb rev_yr admin and wuenic estimates
bracket_data_small <- selected_line_data %>%
  filter(Year == rev_yr, Source %in% c("WUENIC", "Admin")) %>%
  select(Vaccine, Year, Source, Coverage) %>%
  tidyr::pivot_wider(names_from = Source, values_from = Coverage) %>%
  mutate(
    y_max = pmax(`WUENIC`, Admin, na.rm = TRUE), 
    y_min = pmin(`WUENIC`, Admin, na.rm = TRUE), 
    x_position = rev_yr + 1.4,
    cap_width = 0.5
  )

plt_selected_vax_line <- ggplot(selected_line_data, aes(x = Year, y = Coverage, color = Source, shape = Source)) +
  geom_line(data = selected_line_data %>% filter(Source == "WUENIC"), size = 0.8, alpha = 0.7) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  geom_point(data = selected_line_data %>% filter(Source %in% c("Survey", "Official Estimate")), size = 3, alpha = 0.7) +
  geom_point(data = selected_line_data %>% filter(Source == "Admin"), size = 2, alpha = 0.9) +
  facet_wrap(~Vaccine, scales = "fixed") +
  geom_text(data = gap_admin_wuenic_small, aes(x = (data_year - 5.5), y = min_cov, label = paste0(t_lookup("label_admin_wuenic_gap", language), ": ", gap, t_lookup("unit_pp", language))),
                        color = "#6A1E74", size = 3.5, vjust = 0, hjust = 0.5, inherit.aes = FALSE, label.color = NA) +
  geom_segment(data = gap_admin_wuenic_small %>% filter(Year == rev_yr), aes(x = Year, xend = Year, y = (min_cov + 5), yend = (min_cov + 10)), # arrow showing the gap is from rev_yr
               arrow = arrow(length = unit(0.15, "cm"), type = "open"), color = "#6A1E74", linewidth = 0.8, inherit.aes = FALSE) +
  geom_vline(data = gap_admin_wuenic_small %>% filter(Year == rev_yr), aes(xintercept = Year), color = "#6A1E74", linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE, alpha = 0.3) +
  # bracket between admin and wuenic for the most recent year
  geom_segment(data = bracket_data_small, aes(x = x_position, xend = x_position, y = y_min, yend = y_max), color = "#6A1E74", linewidth = 0.7, inherit.aes = FALSE) +
  geom_segment(data = bracket_data_small, aes(x = x_position, xend = x_position - cap_width, y = y_max, yend = y_max), color = "#6A1E74", linewidth = 0.7, inherit.aes = FALSE) +
  geom_segment(data = bracket_data_small, aes(x = x_position, xend = x_position - cap_width, y = y_min, yend = y_min), color = "#6A1E74", linewidth = 0.7, inherit.aes = FALSE) +
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
  scale_x_continuous(breaks = seq(min(selected_line_data$Year), max(selected_line_data$Year), by = 2)) +
  scale_y_continuous(limits = c(min_cov, max_cov), breaks = seq(min_cov, max_cov, by = 25), labels = scales::label_number(suffix = "%")) +
  theme_minimal() +
  labs(
    title = paste0(t_lookup("plt_coverage_trends_title", language), ", ", .current_country, ", ", min_yr_plots, "–", rev_yr),
    x = t_lookup("axis_year", language), y = t_lookup("coverage", language), color = t_lookup("legend_data_source", language), shape = t_lookup("legend_data_source", language)
  ) +
  theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.title.x = element_blank(),
    axis.ticks.x     = element_line(color = "black"),
    axis.ticks.y     = element_line(color = "black"),
    panel.border     = element_rect(color = "black", fill = NA, size = 0.6),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text       = element_text(color = "white", face = "bold"),
    plot.title       = element_text(hjust = 0.5, size = 14)
  )
plt_selected_vax_line

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
      flag_over100 & flag_large_chng ~ "±10pp change & >100%",
      TRUE            ~ "≤100%"
    )
  ) %>%
  ungroup()

plt_coverage_flags <- ggplot(combined_flag_data, aes(x = Year, y = Admin)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  geom_line(color = "grey60") +
  geom_point(aes(color = flag_type, fill = flag_type, shape = flag_type), size = 2.2, stroke = 1) +
  facet_wrap(~Vaccine) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 25), labels = scales::label_number(suffix = "%")) +
  scale_x_continuous(
    breaks = seq(min(combined_flag_data$Year), max(combined_flag_data$Year), by = 1),
    labels = function(x) ifelse(x %% 2 == 0, as.character(x), "")
  ) +
  scale_color_manual(
    values = c("≤100%" = "black", ">100%" = "#FFC20E", "±10pp change" = "#E2231A", "±10pp change & >100%" = "#E2231A"),
    labels = c(
      "≤100%", ">100%",
      "±10pp change" = t_lookup("flag_large_change", language),
      "±10pp change & >100%" = t_lookup("flag_both", language)
    )
  ) +
  scale_fill_manual(
    values = c("≤100%" = "black", ">100%" = "#FFC20E", "±10pp change" = "#E2231A", "±10pp change & >100%" = "#FFC20E"),
    guide = "none"
  ) +
  scale_shape_manual(
    values = c("≤100%" = 19, ">100%" = 19, "±10pp change" = 19, ">100% & ±10pp change" = 21),
    guide = "none"
  ) +
  labs(
    title    = paste0(t_lookup("plt_admin_flags_title", language), " ", .current_country, ", ", min_yr_plots, "–", rev_yr),
    subtitle = paste0(t_lookup("plt_flags_subtitle", language), pct_threshold * 100, "pp"),
    x        = t_lookup("axis_year", language),
    y        = t_lookup("axis_admin_coverage_pct", language),
    caption  = get_text2("txt_caption_missing_admin", text_vars),
    color    = t_lookup("legend_flag_type", language)
  ) +
  theme_minimal() +
  theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.title.x = element_blank(),
    axis.ticks.x     = element_line(color = "black"),
    axis.ticks.y     = element_line(color = "black"),
    panel.border     = element_rect(color = "black", fill = NA, size = 0.6),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text       = element_text(color = "white", face = "bold"),
    plot.title       = element_text(hjust = 0.5, size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 11),
    plot.caption     = element_text(size = 9),
    panel.grid.minor = element_blank()
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
  scale_x_continuous(
    breaks = seq(min(selected_flag_data$Year), max(selected_flag_data$Year), by = 1),
    labels = function(x) ifelse(x %% 2 == 0, as.character(x), "")
  ) +
  scale_color_manual(
    values = c("≤100%" = "black", ">100%" = "#FFC20E", "±10pp change" = "#E2231A"),
    labels = c("≤100%", ">100%", "±10pp change" = t_lookup("flag_large_change", language))
  ) +
  theme_minimal() +
  labs(
    title    = paste0(t_lookup("plt_selected_flags_title", language), " ", .current_country, ", ", min_yr_plots, "–", rev_yr),
    subtitle = paste0(t_lookup("plt_flags_subtitle", language), pct_threshold * 100, "pp"),
    x = t_lookup("axis_year", language), y = t_lookup("axis_admin_coverage_pct", language),
    caption = get_text2("txt_caption_missing_admin", text_vars),
    color = "Flag Type"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
    axis.text.y  = element_text(size = 9, color = "black"),
    axis.title.x = element_blank(),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    strip.background = element_rect(fill = "#0083CF"),
    strip.text = element_text(color = "white", face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    plot.caption = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

## ---------------------------------------------------------------------------------------------------------------------
### Plot: Numerator bar charts
## ---------------------------------------------------------------------------------------------------------------------

all_numerators <- wuenic_master_current %>%
  select(Vaccine, Year, ChildrenVaccinated, any_stockout) %>%
  filter(!is.na(ChildrenVaccinated))

# filter to past 5 years
recent_numerators <- all_numerators %>%
  filter(Year >= (rev_yr - 4))

recent_dtp_mcv <- recent_numerators %>%
  filter(Vaccine %in% c("DTP1", "DTP3", "MCV1"))

# y breaks
y_breaks_all <- pretty(c(0, max(recent_numerators$ChildrenVaccinated, na.rm = TRUE)), n = 5)
y_breaks_sub <- pretty(c(0, max(recent_dtp_mcv$ChildrenVaccinated, na.rm = TRUE)), n = 5)

# color palette - one color per vaccine
vaccine_colors <- c(
  "BCG"    = "#006884", "HepBB"  = "#0000ff", "DTP1"   = "#00833D",
  "DTP3"   = "#80BD41", "Hib3"   = "#6A1E74", "HepB3"  = "#b47ede",
  "PCVC"   = "#ffdb58", "RotaC"  = "#e86100", "IPV1"   = "#B50800", 
  "IPVC"   = "#FFa500", "MCV1"   = "#93b9e1", "RCV1"   = "#9E9E9E", 
  "MCV2"   = "#FFB6C1", "YFV"    = "#5B4A9C", "MengA"  = "#E2231A", 
  "HPVc"   = "#D45F9E"
)

# plot function
make_bar_plt <- function(data, y_breaks, title_str) {
  
  vaccines <- vaccine_levels[vaccine_levels %in% unique(data$Vaccine)]  # global order, filtered
  n_vax <- length(vaccines)
  dodge_width <- 0.8
  bar_width <- dodge_width / n_vax
  
  offsets <- setNames(seq(-dodge_width/2 + bar_width/2, dodge_width/2 - bar_width/2, length.out = n_vax), vaccines)
  
  year_levels <- sort(unique(data$Year))
  
  stockout_data <- data %>%
    filter(any_stockout == 1) %>%
    mutate(x_pos = match(Year, year_levels) + offsets[Vaccine])
  
  data <- data %>%
    mutate(Vaccine = factor(Vaccine, levels = vaccines))
  
  ggplot(data, aes(x = factor(Year, levels = year_levels), y = ChildrenVaccinated, fill = Vaccine, group = Vaccine)) +
    geom_col_pattern(
      position = position_dodge(width = dodge_width, preserve = "single"),
      width = bar_width * n_vax * 0.875,
      aes(
        pattern = ifelse(any_stockout == 1, "stripe", "none"),
        colour = factor(Vaccine, levels = vaccine_levels)
      ),
      linewidth = 0.05, pattern_colour = "black", pattern_angle = 45, pattern_density = 0.1,
      pattern_spacing = 0.02, pattern_fill = "black", pattern_key_scale_factor = 0.6
    ) +
    geom_col(
      position = position_dodge(width = dodge_width, preserve = "single"),
      width = bar_width * n_vax * 0.875,
      aes(colour = factor(Vaccine, levels = vaccine_levels)),
      fill = NA,
      linewidth = 0.5
    ) +
    scale_pattern_manual(
      values = c("stripe" = "stripe", "none" = "none"),
      labels = c("stripe" = t_lookup("vaccine_stockout", language), "none" = ""),
      guide = guide_legend(title = NULL, order = 2,
                           override.aes = list(
                             fill = "white", colour = "transparent", pattern_colour = "black",
                             pattern_fill = "black", pattern_density = 0.1, pattern_spacing = 0.009))
    ) +
    scale_fill_manual(
      values = vaccine_colors,
      aesthetics = c("fill", "colour"), 
      guide = guide_legend(
        title = t_lookup("tbl_schedule_vaccine", language),
        order = 1,
        override.aes = list(pattern = "none")
      )
    ) +
    scale_y_continuous(
      limits = c(min(y_breaks), max(y_breaks)), breaks = y_breaks,
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    theme_minimal() +
    labs(
      title   = title_str,
      x       = t_lookup("axis_year", language),
      y       = get_text2("txt_label_num_vax", text_vars),
      fill    = t_lookup("tbl_schedule_vaccine", language)
    ) +
    theme(
      axis.text.x    = element_text(size = 10),
      axis.text.y    = element_text(size = 10),
      axis.title.x = element_blank(),
      axis.ticks.x   = element_line(color = "black"),
      axis.ticks.y   = element_line(color = "black"),
      panel.border   = element_rect(color = "black", fill = NA, size = 0.6),
      plot.title     = element_text(hjust = 0.5, size = 14),
      plot.caption   = element_text(size = 9),
      panel.grid.minor = element_blank(),
      legend.key.size = unit(1, "lines")
    )
}

plt_numerators_bar_all <- make_bar_plt(
  recent_numerators, y_breaks_all,
  paste0(get_text2("txt_label_num_vax2", text_vars), ", ", .current_country, ", ", rev_yr - 4, "–", rev_yr)
)

plt_numerators_bar_dtpmcv <- make_bar_plt(
  recent_dtp_mcv, y_breaks_sub,
  paste0(get_text2("txt_label_num_vax2", text_vars), " (DTP1, DTP3, MCV1), ", .current_country, ", ", rev_yr - 4, "–", rev_yr)
)

plt_numerators_bar_all
plt_numerators_bar_dtpmcv

## ---------------------------------------------------------------------------------------------------------------------
### Plot: Year-to-year % change in numerator (# children vaccinated) flagged if over % threshold (pct_threshold)
## ---------------------------------------------------------------------------------------------------------------------

plot_data_numerators_all <- wuenic_master_current %>%
  arrange(Vaccine, Year) %>%
  group_by(Vaccine) %>%
  mutate(
    prev_num   = lag(ChildrenVaccinated),
    pct_change = ((ChildrenVaccinated - prev_num) / prev_num) * 100,
    is_flagged = abs(pct_change) > pct_threshold * 100
  ) %>%
  ungroup()

if (nrow(plot_data_numerators_all) == 0) {
  message("Skipping ", .current_country, ": no numerator data available")
  plt_perc_change_line <- NULL
  next
}

# ── data availability ─────────────────────────────────────────────────────────
vaccines_with_numerator_data <- plot_data_numerators_all %>%
  group_by(Vaccine) %>%
  summarise(n_valid = sum(!is.na(ChildrenVaccinated)), .groups = "drop") %>%
  filter(n_valid > 1) %>%
  pull(Vaccine)

vaccines_no_numerator_data <- plot_data_numerators_all %>%
  distinct(Vaccine) %>%
  filter(!Vaccine %in% vaccines_with_numerator_data) %>%
  pull(Vaccine)

vaccines_no_consecutive_data <- plot_data_numerators_all %>%
  filter(Vaccine %in% vaccines_with_numerator_data) %>%
  arrange(Vaccine, Year) %>%
  group_by(Vaccine) %>%
  summarise(has_consecutive = any(!is.na(ChildrenVaccinated) & !is.na(lag(ChildrenVaccinated))), .groups = "drop") %>%
  filter(!has_consecutive) %>%
  pull(Vaccine)

# ── plot ──────────────────────────────────────────────────────────────────────
if (length(vaccines_with_numerator_data) > 0) {
  
  stockout_data <- plot_data_numerators_all %>%
    filter(any_stockout == 1) %>%
    select(Vaccine, Year) %>%
    distinct() %>%
    mutate(xmin_num = Year - 0.5, xmax_num = Year + 0.5)
  
  plot_data_sufficient <- plot_data_numerators_all %>%
    filter(Vaccine %in% vaccines_with_numerator_data) %>%
    mutate(pct_color = case_when(
      abs(pct_change) > 10 ~ "high",
      abs(pct_change) >= 5 ~ "mid",
      !is.na(pct_change)   ~ "low"
    ))
  
  stockout_sufficient <- stockout_data %>% filter(Vaccine %in% vaccines_with_numerator_data)
  
  # build segments colored by pct_change magnitude of the END point
  seg_data <- plot_data_sufficient %>%
    arrange(Vaccine, Year) %>%
    group_by(Vaccine) %>%
    mutate(
      x_end      = lead(Year),
      y_end      = lead(ChildrenVaccinated),
      seg_color = case_when(
        abs(lead(pct_change)) > 10 ~ "high",
        abs(lead(pct_change)) >= 5 ~ "mid",
        !is.na(lead(pct_change))   ~ "low"
      )
    ) %>%
    filter(!is.na(x_end), !is.na(y_end), !is.na(seg_color)) %>%
    ungroup()
  
  plt_perc_change_line <- local({
    frozen_data       <- plot_data_sufficient
    frozen_stockout       <- stockout_sufficient
    segment_data      <- seg_data
    yr_min   <- min(frozen_data$Year)
    yr_max   <- max(frozen_data$Year)
    title_str <- paste0(get_text2("txt_label_num_vax2", text_vars), ", ", .current_country, ", ", min_yr_plots, "–", rev_yr)
    
    ggplot(frozen_data, aes(x = Year)) +
      geom_rect(data = frozen_stockout, aes(xmin = xmin_num, xmax = xmax_num, ymin = -Inf, ymax = Inf, fill = "Stockout"),
                alpha = 0.2, inherit.aes = FALSE) +
      geom_segment(data = segment_data, aes(x = Year, xend = x_end, y = ChildrenVaccinated, yend = y_end, color = seg_color),
                   linewidth = 0.9) +
      geom_point(aes(y = ChildrenVaccinated, color = pct_color), size = 1.75, na.rm = TRUE) +
      geom_point(data = frozen_data %>% group_by(Vaccine) %>% filter(!is.na(ChildrenVaccinated)) %>% filter(Year == min(Year)),
                 aes(y = ChildrenVaccinated, shape = "first_yr"), color = "black", size = 1.75) +
      scale_shape_manual(
        values = c("first_yr" = 16),
        labels = c("first_yr" = t_lookup("label_first_year", language)),
        guide  = guide_legend(title = NULL, order = 3, override.aes = list(color = "black"))
      ) +
      facet_wrap(~Vaccine, scales = "fixed") +
      scale_y_continuous(name = t_lookup("num_vaccinated", language),
                         labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
      scale_x_continuous(breaks = seq(yr_min, yr_max, by = 1),
                         labels = function(x) ifelse(x %% 2 == 0, as.character(x), "")) +
      scale_color_manual(
        values = c("low" = "#2CA02C", "mid" = "orange", "high" = "red"),
        labels = c("low" = t_lookup("label_change_low", language), "mid" = t_lookup("label_change_mid", language), "high" = t_lookup("label_change_high", language)),
        breaks = c("high", "mid", "low"), na.translate = FALSE
      ) +
      scale_fill_manual(values = c("Stockout" = "orange"),
                        labels = setNames(t_lookup("vaccine_stockout", language), "Stockout")) +
      guides(color = guide_legend(title = NULL, order = 1), fill = guide_legend(title = NULL, order = 2)) +
      theme_minimal() +
      theme(
        axis.text.x      = element_text(angle = 45, hjust = 1, size = 8.5, color = "black"),
        axis.text.y      = element_text(color = "black", size = 8.5),
        axis.title.x = element_blank(),
        axis.ticks.x     = element_line(color = "black"),
        axis.ticks.y     = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.6),
        legend.position  = "bottom",
        legend.box       = "horizontal",
        strip.background = element_rect(fill = "#0083CF"),
        strip.text       = element_text(color = "white", face = "bold"),
        plot.title       = element_text(hjust = 0.5, size = 14),
        plot.caption     = element_text(size = 9)
      ) +
      labs(title = title_str, x = t_lookup("axis_year", language),
           caption = paste0(get_text2("txt_caption_missing_admin", text_vars), "\n"))
  })
} else {
  plt_perc_change_line <- ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = paste0("No vaccines with available numerator data for ", .current_country),
             color = "red", size = 6, hjust = 0.5, vjust = 0.5) +
    theme_void()
}

# ── append caption notes for missing/insufficient vaccines ───────────────────
if (!is.null(plt_perc_change_line)) {
  caption_parts <- c(
    if (length(vaccines_no_numerator_data) > 0) paste0("No numerator data: ", paste(vaccines_no_numerator_data, collapse = ", ")),
    if (length(vaccines_no_consecutive_data) > 0) paste0("Numerator data exists but no consecutive years to calculate percent change: ", paste(vaccines_no_consecutive_data, collapse = ", "))
  )
  if (length(caption_parts) > 0) {
    plt_perc_change_line <- plt_perc_change_line +
      labs(caption = paste0("NOTE: ", paste(caption_parts, collapse = "\n"))) +
      theme(plot.caption = element_text(color = "red", size = 12))
  }
}

plt_perc_change_line

# ======================================================================================================================
### Denominator Checks
# ======================================================================================================================

## ---------------------------------------------------------------------------------------------------------------------
### Denom Plot 1: Year-to-year admin denominator change flagged if direction switches
## ---------------------------------------------------------------------------------------------------------------------

denom_change_data <- wuenic_master_current %>%
  group_by(Vaccine) %>%
  mutate(
    prev_target       = lag(ChildrenInTarget),
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

# ── 3. DETECT DIRECTIONAL SWITCHES & PCT COLOR ────────────────────────────────
denom_change_data <- denom_change_data %>%
  group_by(Vaccine) %>%
  arrange(Year) %>%
  mutate(
    current_sign = sign(pct_change_denom),
    prev_sign_1  = lag(current_sign, 1),
    prev_sign_2  = lag(current_sign, 2),
    direction_switch = case_when(
      current_sign == -1 & prev_sign_1 == 1 ~ "switch",
      current_sign == 1 & prev_sign_1 == -1 & (is.na(prev_sign_2) | prev_sign_2 == -1) ~ "switch",
      TRUE ~ "normal"
    ),
    pct_color = case_when(
      is.na(pct_change_denom)        ~ "none",
      abs(pct_change_denom) > 10     ~ "high",
      abs(pct_change_denom) >= 5     ~ "mid",
      TRUE                           ~ "low"
    ),
    # segment color = color of the point it leads into (next point's color)
    seg_color = lead(pct_color)
  ) %>%
  select(-prev_sign_1, -prev_sign_2) %>%
  ungroup()

# build direction switch background bands
switch_bands <- denom_change_data %>%
  filter(direction_switch == "switch") %>%
  select(Vaccine, Year) %>%
  mutate(xmin = Year - 0.5, xmax = Year + 0.5)

# build segment data (one segment per consecutive pair of non-NA points)
seg_data <- denom_change_data %>%
  group_by(Vaccine) %>%
  arrange(Year) %>%
  mutate(
    x_end = lead(Year),
    y_end = lead(ChildrenInTarget)
  ) %>%
  filter(!is.na(ChildrenInTarget), !is.na(x_end), !is.na(y_end),
         x_end == Year + 1) %>%  # only connect consecutive years
  ungroup()

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
    frozen_target_min <- target_min
    frozen_target_max <- target_max
    frozen_year_min <- min(plot_data_sufficient$Year)
    frozen_year_max <- max(plot_data_sufficient$Year)
    frozen_title <- paste0(t_lookup("title_denominator", language), ", ", .current_country, ", ", min_yr_plots, "–", rev_yr)
    
    target_pop_label <- get_text2("txt_target_pop", text_vars)
    
    ggplot(frozen_plot_data, aes(x = Year)) +
      geom_rect(data = switch_bands %>% filter(Vaccine %in% vaccines_with_denominator_data),
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = "Direction switch"),
                alpha = 0.25, inherit.aes = FALSE) +
      geom_segment(data = seg_data %>% filter(Vaccine %in% vaccines_with_denominator_data),
                   aes(x = Year, xend = x_end, y = ChildrenInTarget, yend = y_end, color = seg_color),
                   linewidth = 0.8) +
      geom_point(aes(y = ChildrenInTarget, color = pct_color), size = 2.5, na.rm = TRUE) +
      facet_wrap(~Vaccine, scales = "fixed") +
      scale_y_continuous(
        name   = target_pop_label,
        limits = c(frozen_target_min * 0.9, frozen_target_max * 1.1),
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      scale_x_continuous(
        breaks = seq(frozen_year_min, frozen_year_max, by = 1),
        labels = function(x) ifelse(x %% 2 == 0, as.character(x), "")
      ) +
      scale_color_manual(
        name   = NULL,
        values = c("low" = "#2CA02C", "mid" = "orange", "high" = "#E2231A", "none" = "black"),
        labels = c("low"  = t_lookup("label_change_low", language),
                   "mid"  = t_lookup("label_change_mid", language),
                   "high" = t_lookup("label_change_high", language),
                   "none" = t_lookup("label_no_prev_year", language)),
        breaks = c("high", "mid", "low", "none"), na.translate = FALSE
      ) +
      scale_fill_manual(
        name   = NULL,
        values = c("Direction switch" = "#1CABE2"),
        labels = c("Direction switch" = t_lookup("label_direction_switch", language)),
        guide  = guide_legend(order = 2, override.aes = list(alpha = 0.9))
      ) +
      guides(
        color = guide_legend(title = NULL, order = 1),
        fill  = guide_legend(title = NULL, order = 2)
      ) +
      theme_minimal() +
      theme(
        axis.line.y.left   = element_line(color = "black"),
        axis.text.x        = element_text(angle = 45, hjust = 1, size = 9),
        axis.title.x       = element_blank(),
        axis.text.y.left   = element_text(color = "black", size = 9),
        axis.ticks.x       = element_line(color = "black"),
        axis.ticks.y       = element_line(color = "black"),
        panel.grid.minor   = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.6),
        legend.position    = "bottom", legend.box = "horizontal",
        strip.background   = element_rect(fill = "#0083CF"),
        strip.text         = element_text(color = "white", face = "bold"),
        plot.title         = element_text(hjust = 0.5, size = 14),
        plot.caption       = element_text(size = 9)
      ) +
      labs(title   = frozen_title,
           caption = get_text2("txt_caption_missing_admin", text_vars),
           x       = t_lookup("axis_year", language))
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


## ---------------------------------------------------------------------------------------------------------------------
### Denom Plot 2: Live births (BCG) vs surviving infants (DTP1) over time
## ---------------------------------------------------------------------------------------------------------------------

# Reshape data: UNPD denominators (long format)
births_vs_si_data <- wuenic_master_current %>%
  filter(Vaccine %in% c("BCG", "DTP1")) %>%
  select(Year, Vaccine, BirthsUNPD, SurvivingInfantsUNPD, ChildrenInTarget)

# UNPD lines: one row per year
if (nrow(births_vs_si_data) == 0) {
  message("Skipping ", .current_country, ": no UNPD denominator data available for BCG or DTP1")
  plt_births_vs_si <- NULL
} else {
  denominator_data <- births_vs_si_data %>%
    select(Year, Vaccine, BirthsUNPD, SurvivingInfantsUNPD, ChildrenInTarget) %>%
    pivot_longer(
      cols = c(BirthsUNPD, SurvivingInfantsUNPD, ChildrenInTarget),
      names_to  = "target_grp",
      values_to = "Population"
    ) %>%
    # 1. Swapped && for & here
    mutate(target_grp = case_when(
      target_grp == "BirthsUNPD" ~ "Live Births (UNPD)",
      target_grp == "SurvivingInfantsUNPD" ~ "Surviving Infants (UNPD)",
      target_grp == "ChildrenInTarget" & Vaccine == "BCG" ~ "BCG Admin Target Population",
      target_grp == "ChildrenInTarget" & Vaccine == "DTP1" ~ "DTP1 Admin Target Population",
      TRUE ~ target_grp
    )) %>%
    filter(
      (Vaccine == "BCG" & target_grp %in% c("Live Births (UNPD)", "BCG Admin Target Population")) |
        (Vaccine == "DTP1" & target_grp %in% c("Surviving Infants (UNPD)", "DTP1 Admin Target Population"))
    )
}

# --- Availability checks (now based on UNPD columns) ---
bcg_unpd_available  <- births_vs_si_data %>%
  filter(Vaccine == "BCG",  !is.na(BirthsUNPD)) %>% nrow() > 0

si_unpd_available <- births_vs_si_data %>%
  filter(Vaccine == "DTP1",  !is.na(SurvivingInfantsUNPD)) %>% nrow() > 0

bcg_in_schedule <- "BCG" %in% tbl_schedule_r$Vaccine

availability_footnote <- if (!bcg_unpd_available && !si_unpd_available) {
  "NOTE: No UNPD denominator data available for Live Births or Surviving Infants."
} else if (!bcg_unpd_available && !bcg_in_schedule) {
  paste0("NOTE: BCG is not in ", .current_country, "'s current schedule; no Live Births data available.")
} else if (!bcg_unpd_available) {
  "NOTE: No UNPD data available for Live Births."
} else if (!si_unpd_available) {
  "NOTE: No UNPD data available for Surviving Infants."
} else {
  NULL
}
data_source_footnote <- get_text2("txt_caption_wpp_source", text_vars)

# --- Y-axis limits for single data point edge case ---
data_available <- denominator_data %>%
  filter(!is.na(Population)) %>%
  nrow()

if (data_available == 1) {
  single_value    <- denominator_data$Population[!is.na(denominator_data$Population)][1]
  step_magnitude  <- 10^floor(log10(single_value) - 0.5)
  y_min           <- max(0, floor((single_value * 0.5) / step_magnitude) * step_magnitude)
  y_max           <- ceiling((single_value * 1.5) / step_magnitude) * step_magnitude
} else {
  y_min <- NULL
  y_max <- NULL
}

# set order
denominator_data$target_grp <- factor(denominator_data$target_grp, levels = c("Live Births (UNPD)", "BCG Admin Target Population", 
                                                                              "Surviving Infants (UNPD)", "DTP1 Admin Target Population"))

# --- Plot ---
lbl_births  <- t_lookup("unpd_live_births", language)
lbl_surv_inf <- t_lookup("unpd_si", language)
lbl_bcg_tgt  <- t_lookup("bcg_admin_target", language)
lbl_dtp1_tgt <- t_lookup("dtp_admin_target", language)

# map categories
denominator_data <- denominator_data %>%
  mutate(target_grp = case_when(
    target_grp == "Live Births (UNPD)" ~ lbl_births,
    target_grp == "Surviving Infants (UNPD)" ~ lbl_surv_inf,
    target_grp == "BCG Admin Target Population" ~ lbl_bcg_tgt,
    target_grp == "DTP1 Admin Target Population" ~ lbl_dtp1_tgt,
    TRUE ~ target_grp
  ))

# plot
plt_births_vs_si <- ggplot(denominator_data, aes(x = Year, y = Population, color = target_grp)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()), limits = if (!is.null(y_min)) c(y_min, y_max) else NULL) +
  scale_x_continuous(breaks = seq(min(denominator_data$Year), max(denominator_data$Year), by = 1)) +
  scale_color_manual(
    values = setNames(
      c("#E2231A", "#0058AB", "#f4a4a0", "#349cff"),
      c(lbl_births, lbl_surv_inf, lbl_bcg_tgt, lbl_dtp1_tgt)
    )
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal() +
  labs(
    title = paste0(get_text2("txt_title_births_si", text_vars), ",\n", .current_country, ", ", min_yr_plots, "–", rev_yr),
    x = t_lookup("axis_year", language), 
    y = get_text2("txt_target_population", text_vars),
    color = get_text2("txt_denominator", text_vars),
    caption = paste0(availability_footnote, "\n", data_source_footnote)
  ) +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
    axis.text.y = element_text(color = "black", size = 9),
    axis.title.x = element_blank(),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    plot.caption = element_text(color = "black", size = 9),
    panel.grid.minor.x = element_blank()
  )

## ---------------------------------------------------------------------------------------------------------------------
### Denom Plot 3: Year-to-year % change for Live Births (BCG) and Surviving Infants (DTP1)
## ---------------------------------------------------------------------------------------------------------------------

denom_types_data <- wuenic_master_current %>%
  filter(Vaccine %in% c("BCG", "DTP1")) %>%
  mutate(DenomType = case_when(Vaccine == "BCG" ~ "Live Births", Vaccine == "DTP1" ~ "Surviving Infants")) %>%
  select(Country, Vaccine, Year, DenomType, ChildrenInTarget) %>%
  group_by(Vaccine) %>%
  arrange(Vaccine, Year) %>%
  mutate(
    prev_denominator = lag(ChildrenInTarget),
    pct_change_denom = (ChildrenInTarget - prev_denominator) / prev_denominator,
    current_sign     = sign(pct_change_denom),
    prev_sign_1      = lag(current_sign, 1),
    prev_sign_2      = lag(current_sign, 2),
    direction_switch = case_when(
      current_sign == -1 & prev_sign_1 == 1 ~ "switch",
      current_sign == 1 & prev_sign_1 == -1 & (is.na(prev_sign_2) | prev_sign_2 == -1) ~ "switch",
      TRUE ~ "normal"
    ),
    flag_denom_change = direction_switch == "switch",
    pct_color = case_when(
      is.na(pct_change_denom)        ~ "none",
      abs(pct_change_denom) >= 0.10  ~ "high",
      abs(pct_change_denom) >= 0.05  ~ "mid",
      TRUE                           ~ "low"
    )
  ) %>%
  mutate(DenomType = case_when(
    Vaccine == "BCG"  ~ t_lookup("caption_live_births", language),
    Vaccine == "DTP1" ~ t_lookup("caption_si", language),
    TRUE ~ DenomType
  )) %>%
  ungroup()

denom_data_available <- denom_types_data %>% filter(!is.na(ChildrenInTarget)) %>% nrow() > 1

# segments connecting consecutive points, colored by magnitude of change
seg_data <- denom_types_data %>%
  group_by(Vaccine, DenomType) %>%
  arrange(Year) %>%
  mutate(
    x_end  = lead(Year),
    y_end  = lead(ChildrenInTarget),
    seg_color = pct_color
  ) %>%
  filter(!is.na(x_end), !is.na(y_end)) %>%
  ungroup()

# shading bands for direction switches
switch_bands <- denom_types_data %>%
  filter(flag_denom_change) %>%
  group_by(Vaccine, DenomType) %>%
  mutate(xmin = Year - 0.5, xmax = Year + 0.5) %>%
  select(Vaccine, DenomType, xmin, xmax) %>%
  ungroup()

if(denom_data_available) {
  plt_denom_pct_change <- local({
    
    frozen_data <- denom_types_data
    frozen_year_min <- min(denom_types_data$Year)
    frozen_year_max <- max(denom_types_data$Year)
    
    target_min <- min(frozen_data$ChildrenInTarget, na.rm = TRUE)
    target_max <- max(frozen_data$ChildrenInTarget, na.rm = TRUE)
    
    raw_denom_label <- get_text2("txt_raw_denom", text_vars)
    frozen_title <- paste0(get_text2("txt_denom_stability", text_vars), ", ", .current_country, ", ", min_yr_plots, "–", rev_yr)
    
    ggplot(frozen_data, aes(x = Year)) +
      geom_rect(data = switch_bands %>% filter(DenomType %in% unique(frozen_data$DenomType)),
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = "Direction switch"),
                alpha = 0.25, inherit.aes = FALSE) +
      geom_segment(data = seg_data %>% filter(DenomType %in% unique(frozen_data$DenomType)),
                   aes(x = Year, xend = x_end, y = ChildrenInTarget, yend = y_end, color = seg_color),
                   linewidth = 0.8) +
      geom_point(aes(y = ChildrenInTarget, color = pct_color), size = 2.5, na.rm = TRUE) +
      facet_wrap(~DenomType) +
      scale_y_continuous(
        name = raw_denom_label,
        limits = c(target_min * 0.9, target_max * 1.1),
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      scale_x_continuous(
        breaks = seq(frozen_year_min, frozen_year_max, by = 1),
        #labels = function(x) ifelse(x %% 2 == 0, as.character(x), "")
      ) +
      scale_color_manual(
        name   = NULL,
        values = c("low" = "#2CA02C", "mid" = "orange", "high" = "#E2231A", "none" = "black"),
        labels = c("low"  = t_lookup("label_change_low", language),
                   "mid"  = t_lookup("label_change_mid", language),
                   "high" = t_lookup("label_change_high", language),
                   "none" = t_lookup("label_no_prev_year", language)),
        breaks = c("high", "mid", "low", "none"), na.translate = FALSE
      ) +
      scale_fill_manual(
        name   = NULL,
        values = c("Direction switch" = "#1CABE2"),
        labels = c("Direction switch" = t_lookup("label_direction_switch", language)),
        guide  = guide_legend(order = 2, override.aes = list(alpha = 0.9))
      ) +
      guides(
        color = guide_legend(title = NULL, order = 1),
        fill  = guide_legend(title = NULL, order = 2)
      ) +
      theme_minimal() +
      theme(
        legend.position    = "bottom", legend.box = "horizontal",
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.6),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        strip.background   = element_rect(fill = "#0083CF"),
        strip.text         = element_text(color = "white", face = "bold"),
        axis.line.y.left   = element_line(color = "black"),
        axis.ticks.x       = element_line(color = "black"),
        axis.ticks.y       = element_line(color = "black"),
        axis.text.x        = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y.left   = element_text(color = "black", size = 9),
        axis.title.x       = element_blank(),
        plot.title         = element_text(hjust = 0.5, size = 14),
        plot.caption       = element_text(size = 9)
      ) +
      labs(
        title   = frozen_title,
        x       = t_lookup("axis_year", language),
        caption = get_text2("txt_caption_missing_admin", text_vars)
      )
  })
} else {
  plt_denom_pct_change <- local({
    ggplot() +
      geom_text(aes(x = 1, y = 1, label = "Not enough denominator data to compute changes"), color = "red", size = 5) +
      theme_void() +
      labs(title = paste0("Denominator Stability Check, ", .current_country, ", ", min_yr_plots, "–", rev_yr), subtitle = "Insufficient data for calculations") +
      theme(plot.title = element_text(hjust = 0.5, size = 14), plot.subtitle = element_text(hjust = 0.5, size = 11))
  })
}

plt_denom_pct_change

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

# round dropout percentages to nearest 1% then set "is_negative" to FALSE if dropout pct is 0
dropout_rates <- dropout_rates %>%
  mutate(dropout_pct = round(dropout_pct)) %>% 
  mutate(is_negative = ifelse(dropout_pct == 0, FALSE, is_negative))

# highest and lowest values between admin coverage and dropout pct, rounded up or down to nearest 10%
max_y <- ceiling(max(max(dropout_long$Admin, na.rm = TRUE), max(dropout_rates$dropout_pct, na.rm = TRUE)) / 10) * 10
min_y <- floor(min(min(dropout_long$Admin, na.rm = TRUE), min(dropout_rates$dropout_pct, na.rm = TRUE)) / 10) * 10

plt_dropout_with_rate <- ggplot() +
  # admin coverage
  geom_line(data = dropout_long, aes(x = Year, y = Admin, color = Vaccine), linewidth = 0.9, alpha = 0.7) +
  geom_point(data = dropout_long, aes(x = Year, y = Admin, color = Vaccine), size = 2) +
  # dropout line
  geom_line(data = dropout_rates, aes(x = Year, y = dropout_pct, color = "Dropout Rate"), alpha = 0.7, linewidth = 0.9) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", alpha = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 1) +
  # dropout triangles
  geom_point(data = dropout_rates, aes(x = Year, y = dropout_pct, fill = is_negative), 
             size = 3, shape = 24, color = "transparent", alpha = 0.8) +
  facet_wrap(~pair, scales = "free_y") +
  scale_y_continuous(
    limits = c(min_y, max_y),
    breaks = seq(min_y, max_y, by = 10), labels = scales::label_number(suffix = "%")
  ) +
  scale_x_continuous(breaks = seq(min(dropout_long$Year), max(dropout_long$Year))) +
  # legend 1: vaccine & dropout lines
  scale_color_manual(
    name = t_lookup("tbl_schedule_vaccine", language),
    values = c(setNames(unicef_colors, levels(factor(dropout_long$Vaccine))), "Dropout Rate" = "black"),
    labels = c(setNames(levels(factor(dropout_long$Vaccine)), levels(factor(dropout_long$Vaccine))),
               "Dropout Rate" = t_lookup("plt_dropout_y", language)),
    guide = guide_legend(order = 1) 
  ) +
  # legend 2: dropout negative or positive
  scale_fill_manual(
    name = t_lookup("dropout_direction", language),
    values = c("TRUE" = "red", "FALSE" = "black"),
    labels = c("TRUE" = t_lookup("dropout_negative", language), "FALSE" = t_lookup("dropout_normal", language)), 
    guide = guide_legend(order = 2, override.aes = list(shape = 24, size = 3))
  ) +
  
  theme_minimal() +
  labs(
    title = paste0(get_text2("txt_key_dropout", text_vars), ", ", .current_country, ", ", min_yr_plots, "–", rev_yr),
    subtitle = t_lookup("subtitle_dropout", language),
    x = t_lookup("axis_year", language), y = t_lookup("admin_dropout", language),
    caption = t_lookup("caption_dropout_formula", language)
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.background = element_rect(fill = "#0083CF"),
    strip.text = element_text(color = "white", face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks.x = element_line(color = "black"),
    axis.title.x = element_blank(),
    axis.ticks.y = element_line(color = "black"),
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y  = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    panel.grid.minor.x = element_blank()
  )

plt_dropout_with_rate

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

coadmin_pairs_all <- coadmin_pairs

# if 6-week and 14-week vaccines are in the coadmin pairs, just use those two
if ("6 weeks" %in% names(coadmin_pairs) & "14 weeks" %in% names(coadmin_pairs)) {
  coadmin_pairs <- coadmin_pairs[c("6 weeks", "14 weeks")]
  names(coadmin_pairs) <- c("6-week", "14-week")
}

# print the result to confirm rotas are named correctly
#print(coadmin_pairs)

#message(paste0("Co-administration clusters detected for ", .current_country, ": ", paste(names(coadmin_pairs), collapse = ", ")))

# text labels for each cluster - removed "missing" text as we are focusing on WUENIC vaccines only
coadmin_labels <- purrr::imap_chr(coadmin_pairs, function(vaccines, cluster_name) {
  
  available <- vaccines[vaccines %in% available_vaccines]
  #missing   <- vaccines[!vaccines %in% available_vaccines]
  
  # make strings to put in the translated text
  vaccine_list    <- paste(vaccines, collapse = ", ")
  #missing_list    <- paste(missing, collapse = ", ")
  current_country <- .current_country 
  
  # select only the WUENIC vaccines from the cluster for the intro text
  wuenic_at_time_point <- vaccines[vaccines %in% wuenic_vaccines]
  wuenic_at_time_point <- paste(wuenic_at_time_point, collapse = ", ")

  intro_template <- if (length(available) > 0) {
    t_lookup("coadmin_intro_available", language)
  } else {
    t_lookup("coadmin_intro_empty", language)
  }
  intro_text <- str_glue(intro_template)
  
  # missing_text <- if (length(missing) > 0) {
  #   missing_template <- t_lookup("coadmin_missing_notes", language)
  #   str_glue(missing_template)
  # } else {
  #   get_text2("txt_all_antigens_present", text_vars)
  # }
  
  paste0(intro_text)
  #paste0(intro_text, "\n\n", missing_text)
})

coadmin_labels_all <- purrr::imap_chr(coadmin_pairs_all, function(vaccines, cluster_name) {
  available <- vaccines[vaccines %in% available_vaccines]
  #missing   <- vaccines[!vaccines %in% available_vaccines]
  vaccine_list    <- paste(vaccines, collapse = ", ")
  #missing_list    <- paste(missing, collapse = ", ")
  current_country <- .current_country
  wuenic_at_time_point <- vaccines[vaccines %in% wuenic_vaccines]
  wuenic_at_time_point <- paste(wuenic_at_time_point, collapse = ", ")
  intro_template <- if (length(available) > 0) {
    t_lookup("coadmin_intro_available", language)
  } else {
    t_lookup("coadmin_intro_empty", language)
  }
  intro_text <- str_glue(intro_template)
  # missing_text <- if (length(missing) > 0) {
  #   missing_template <- t_lookup("coadmin_missing_notes", language)
  #   str_glue(missing_template)
  # } else {
  #   get_text2("txt_all_antigens_present", text_vars)
  # }
  paste0(intro_text)
})

# translation lookup for the cluster names
cluster_name_translations <- c(
  "6 weeks"  = get_text2("txt_timepoint_6weeks", text_vars),
  "10 weeks" = get_text2("txt_timepoint_10weeks", text_vars),
  "14 weeks" = get_text2("txt_timepoint_14weeks", text_vars),
  "2 months" = get_text2("txt_timepoint_2months", text_vars),
  "4 months" = get_text2("txt_timepoint_4months", text_vars)
)

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
      title = paste0(glue::glue(t_lookup("title_admin_coverage", language), cluster_name = cluster_name),
                     ", ", .current_country, ", ", min_yr_plots, "–", rev_yr),
      x = t_lookup("axis_year", language), y = t_lookup("axis_admin_coverage_pct", language), color = t_lookup("tbl_schedule_vaccine", language)) +
    theme(
      legend.position  = "bottom",
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
      axis.title.x = element_blank(),
      axis.ticks.x     = element_line(color = "black"),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.6),
      strip.background = element_rect(fill = "#0083CF"),
      strip.text       = element_text(color = "white", face = "bold"),
      plot.title       = element_text(hjust = 0.5, size = 14),
      panel.grid.minor = element_blank()
    )
})

# remove NULL plots (clusters with no data)
plt_coadmin_list <- Filter(Negate(is.null), plt_coadmin_list)


# ======================================================================================================================
### Numerator bar charts using the co-administered vaccines
# ======================================================================================================================

# rebuild using coadmin_pairs_all, keeping only vaccines present in the data
available_vaccines <- unique(plot_data_numerators_all$Vaccine)

plot_data_numerator_clusters_bars_all <- purrr::map_dfr(names(coadmin_pairs_all), function(cluster_name) {
  vaccines_to_plot <- intersect(coadmin_pairs_all[[cluster_name]], available_vaccines)
  if (length(vaccines_to_plot) == 0) return(NULL)
  plot_data_numerators_all %>%
    filter(Vaccine %in% vaccines_to_plot) %>%
    mutate(cluster = cluster_name)
})

# reorder vaccines
coadmin_pairs_all <- coadmin_pairs_all[order(as.numeric(gsub("[^0-9]", "", names(coadmin_pairs_all))))]

# generate plots
plt_numerators_coadmin_bars <- purrr::imap(coadmin_pairs_all, function(vaccines, cluster_name) {
  cluster_data <- plot_data_numerator_clusters_bars_all %>% filter(cluster == cluster_name)
  if (nrow(cluster_data) == 0) return(NULL)
  translated_cluster <- cluster_name_translations[[cluster_name]]
  txt_subtitle <- glue::glue(get_text2("txt_subtitle_coadmin", text_vars), cluster_name = translated_cluster)
  y_breaks <- pretty(c(0, max(cluster_data$ChildrenVaccinated, na.rm = TRUE)))
  make_bar_plt(cluster_data, y_breaks,
               paste0(get_text2("txt_label_num_vax2", text_vars), ", ", .current_country, ", ", rev_yr - 4, "–", rev_yr)) +
    labs(subtitle = txt_subtitle) +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 12))
}) %>% purrr::compact()

names(plt_numerators_coadmin_bars) <- gsub(" weeks", "-week", names(plt_numerators_coadmin_bars))

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
            get_text2("txt_num_change_title", text_vars), ", ",
            .current_country, ", ", min_yr_plots, "–", rev_yr,
            "\nVaccines Administered at ", time_point, " visit"
          ),
          subtitle = "Insufficient data for percent change from previous year calculation"
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
  # raw count string for legend and secondary y-axis
  raw_count_label <- get_text2("txt_raw_count", text_vars)
  
  num_facets <- length(unique(plot_data$Vaccine))
  
  numerator_plot <- ggplot(plot_data, aes(x = Year)) +
    geom_rect(data = current_stockout, aes(xmin = xmin_num, xmax = xmax_num, ymin = -Inf, ymax = Inf, fill = "Stockout"),
              alpha = 0.2, inherit.aes = FALSE) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_line(aes(y = pct_change, group = Vaccine), color = "grey60", linewidth = 0.6, na.rm = TRUE) +
    geom_point(aes(y = pct_change, color = case_when(abs(pct_change) > 10 ~ "high", abs(pct_change) >= 5 ~ "mid", !is.na(pct_change) ~ "low")), size = 2.5, na.rm = TRUE) +
    geom_line(aes(y = (ChildrenVaccinated / max_doses) * (y_max - y_min) + y_min, group = Vaccine, linetype = raw_count_label), 
              color = "#0058AB", linewidth = 0.8, alpha = 0.6, na.rm = TRUE) +
    geom_point(aes(y = (ChildrenVaccinated / max_doses) * (y_max - y_min) + y_min), color = "#0058AB", size = 1.5, na.rm = TRUE) +
    facet_wrap(~Vaccine, scales = "fixed", drop = TRUE) +
    scale_y_continuous(name = t_lookup("perc_change_previous", language), limits = c(y_min, y_max), 
                       labels = scales::label_number(suffix = "%", accuracy = 1), 
                       sec.axis = sec_axis(trans = ~ (. - y_min) * scale_factor, name = t_lookup("num_vaccinated", language), 
                                           labels = scales::label_number(scale_cut = scales::cut_short_scale()))) +
    scale_x_continuous(breaks = seq(min(plot_data$Year), max(plot_data$Year), by = 1), 
                       labels = function(x) ifelse(num_facets == 1 | x %% 2 == 0, as.character(x), "")) +
    scale_color_manual(name = NULL, values = c("high" = "#E2231A", "mid" = "#Ff7f00", "low" = "#80BD41"), 
                       labels = c("high" = t_lookup("label_change_high", language),
                                  "mid"  = t_lookup("label_change_mid", language),
                                  "low"  = t_lookup("label_change_low", language)),
                       breaks = c("high", "mid", "low"), na.translate = FALSE) +
    scale_linetype_manual(name = NULL, values = setNames("solid", raw_count_label), 
                          guide = guide_legend(order = 2, override.aes = list(color = "#0058AB", linewidth = 1))) +
    scale_fill_manual(values = c("Stockout" = "orange"), labels = setNames(t_lookup("vaccine_stockout", language), "Stockout")) +
    guides(color = guide_legend(title = NULL, order = 1), linetype = guide_legend(title = NULL, order = 2), fill = guide_legend(title = NULL, order = 3)) +
    theme_minimal() +
    theme(axis.line.y.left = element_line(color = "black"), 
          axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
          axis.title.x = element_blank(),
          axis.ticks.x = element_line(color = "black"), 
          axis.ticks.y = element_line(color = "black"), 
          axis.line.y.right = element_line(color = "#0058AB"), 
          axis.text.y.right = element_text(color = "#0058AB", size = 8), 
          axis.title.y.right = element_text(color = "#0058AB", size = 9), 
          panel.grid.minor = element_blank(), 
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6), 
          legend.position = "bottom", legend.box = "horizontal", 
          strip.background = element_rect(fill = "#0083CF"), strip.text = element_text(color = "white", face = "bold"), 
          plot.title = element_text(hjust = 0.5, size = 14), 
          plot.subtitle = element_text(hjust = 0.5, size = 14),
          plot.caption = element_text(size = 9)) +
    labs(title = paste0(get_text2("txt_num_change_title", text_vars), ",\n", .current_country, ", ", min_yr_plots, "–", rev_yr), 
         subtitle = paste0(glue::glue(t_lookup("title_vaccines_timepoint", language), cluster_name = time_point)),
         x = t_lookup("axis_year", language),
         caption = get_text2("txt_caption_missing_admin", text_vars))
  
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
### Admin Data Availability Heatmap
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
  #"OPV"               = "POL3", # not presenting anymore
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
      is.na(intro_year) | Year <= intro_year ~ "Not Introduced",  # before or in intro year: blank
      !is.na(Admin) ~ "Present",
      TRUE ~ "Missing"
    ),
    status = factor(status, levels = c("Present", "Missing", "Not Introduced"))
  )

# order vaccines for heatmap
vaccine_order <- c("BCG", "HepBB", "DTP1", "DTP3", "Hib3", "HepB3", "PCVC", "RotaC",
                   "IPV1", "IPVC", "MCV1", "RCV1", "MCV2", "YFV", "MengA", "HPVc")

heatmap_data_wuenic$Vaccine <- factor(
  heatmap_data_wuenic$Vaccine,
)

# plot
plt_missing_heatmap <- ggplot(heatmap_data_wuenic, aes(x = factor(Year), y = fct_rev(Vaccine), fill = status)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_manual(values   = c("Missing" = "#E2231A", "Present" = "#00833D"), na.value = "white",
                    labels = setNames(c(t_lookup("missing", language), t_lookup("present", language)), c("Missing", "Present"))) +
  scale_x_discrete(position = "top") +
  theme_minimal() +
  labs(title = paste0(t_lookup("availability_heatmap_title", language), ", ", .current_country, ", ", min_yr_plots, "–", rev_yr),
       x = t_lookup("axis_year", language), y = t_lookup("tbl_schedule_vaccine", language), fill = t_lookup("data_status", language)) +
  theme(
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid     = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 15))
  )
plt_missing_heatmap

## ---------------------------------------------------------------------------------------------------------------------
### Comparison Heatmap: Coverage by Vaccine and Source
## ---------------------------------------------------------------------------------------------------------------------

# translate the facet labels (source) for use in the next two plots
facet_source_labels <- c(
  "WUENIC" = t_lookup("source_who_unicef", language),
  "Admin" = t_lookup("source_admin", language),
  "Official Estimate" = t_lookup("source_official", language),
  "Survey" = t_lookup("source_survey", language)
)

recent_years <- sort(unique(wuenic_master_current$Year), decreasing = TRUE)[1:n_years_comparison_plot]
min_yr <- min(recent_years)
max_yr <- max(recent_years)

introduction_data <- heatmap_data_wuenic %>% 
  filter(status == "Not Introduced") %>%
  select(Year, Vaccine, status) %>% 
  arrange(Vaccine, Year)

summary_long <- wuenic_master_current %>%
  filter(Year %in% recent_years) %>%
  select(Vaccine, Year, WUENIC, Admin, Official) %>%
  pivot_longer(cols = c(WUENIC, Admin, Official), names_to = "Source", values_to = "Coverage") %>%
  # complete the table for all recent years (have coverage NA)
  complete(Year = recent_years, Vaccine = wuenic_vaccines, Source = c("WUENIC", "Admin", "Official")) %>%
  mutate(
    Source  = factor(Source, levels = c("WUENIC", "Admin", "Official")),
    Year    = as.integer(as.character(Year)),
    Vaccine = factor(Vaccine, levels = rev(c("BCG", "HepBB", "DTP1", "DTP3", "Hib3", "HepB3",
                                             "PCVC", "RotaC", "IPV1", "IPVC",
                                             "MCV1", "RCV1", "MCV2", "YFV", "MengA", "HPVc")))
  ) %>%
  mutate(Source = recode(Source, "WUENIC" = "WUENIC", "Official" = "Official Estimate")) %>% 
  arrange(Vaccine, Year)

# join vaccine introduction status so we can correctly color in the heatmap
summary_long <- summary_long %>% 
  left_join(introduction_data, by = c("Year", "Vaccine")) %>%
  mutate(
    status = case_when(
      is.na(Coverage) & is.na(status) ~ "Missing",
      !is.na(Coverage) ~ "Present",
      TRUE ~ status
    )
  )

# prep survey data, filter to only years that have survey data
survey_years_with_data <- wuenic_master_current %>%
  filter(Year %in% recent_years, !is.na(Survey)) %>%
  pull(Year) %>%
  unique()

survey_long <- wuenic_master_current %>%
  filter(Year %in% survey_years_with_data) %>%
  select(Vaccine, Year, Survey) %>%
  pivot_longer(cols = Survey, names_to = "Source", values_to = "Coverage") %>%
  mutate(
    Source  = "Survey",
    Year    = as.integer(as.character(Year)),
    Vaccine = factor(Vaccine, levels = rev(c("BCG", "HepBB", "DTP1", "DTP3", "Hib3", "HepB3",
                                             "PCVC", "RotaC", "IPV1", "IPVC",
                                             "MCV1", "RCV1", "MCV2", "YFV", "MengA", "HPVc")))
  ) 

survey_long <- survey_long %>% 
  left_join(introduction_data, by = c("Year", "Vaccine")) %>%
  mutate(
    status = case_when(
      is.na(Coverage) & is.na(status) ~ "Missing",
      !is.na(Coverage) ~ "Present",
      TRUE ~ status
    )
  )

# combine with summary_long for plotting
survey_years_label <- paste(sort(survey_years_with_data), collapse = ", ")
combined_long <- bind_rows(summary_long, survey_long) %>% 
  mutate(
    Source = factor(Source, levels = c("WUENIC", "Admin", "Official Estimate", "Survey")),
    Year_discrete = factor(Year) # <-- Create a factor version of Year for the x-axis
  )

# set missing values separately for the fill (white for not introduced, grey for missing) and the label (blank for not introduced, "—" for missing)
combined_long <- combined_long %>%
  mutate(
    fill_val = case_when(
      status == "Not Introduced" ~ NA_real_,  # white via na.value
      status == "Missing"        ~ NA_real_,         # grey via sentinel
      TRUE                       ~ Coverage
    ),
    label = case_when(
      status == "Not Introduced" ~ "",
      status == "Missing"        ~ "—",
      TRUE                       ~ sprintf("%.0f", Coverage)
    )
  ) 

plt_summary_table <- ggplot(combined_long, aes(x = Year_discrete, y = Vaccine)) +
  geom_tile(aes(fill = fill_val), color = "white", linewidth = 0.4) +
  geom_tile(data = combined_long %>% filter(status == "Missing"),
            aes(color = t_lookup("legend_missing_data", language)), fill = "#d3d3d3", linewidth = 0.4) +
  geom_text(aes(label = label), color = "white", size = 4) +
  scale_fill_gradientn(
    colours  = c("#B50800", "#E2231A", "#F26A21", "#FFC20E", "#80BD41", "#00833D"),
    values   = scales::rescale(c(0, 57, 67, 77, 87, 94, 100)), 
    limits   = c(0, 100), 
    oob      = scales::squish,
    breaks   = c(0, 59, 69, 79, 89, 100),
    labels   = c("0", "60", "70", "80", "90", "100"),
    na.value = "white",
    guide    = guide_colorbar(
      order     = 1,
      barheight = unit(0.4, "cm")
    ) 
  ) +
  scale_color_manual(
    name   = NULL, 
    values = setNames("#d3d3d3", t_lookup("legend_missing_data", language)),
    guide  = guide_legend(
      order        = 2, 
      label.position = "bottom", 
      label.vjust  = 1.5,      
      keywidth     = unit(0.2, "cm"), 
      keyheight    = unit(0.5, "cm"),
      override.aes = list(fill = "#d3d3d3", color = "#d3d3d3")
    )
  ) + 
  facet_grid(. ~ Source, scales = "free_x", space = "free_x", labeller = as_labeller(facet_source_labels)) +
  theme_minimal() +
  labs(
    title = paste0(t_lookup("plt_coverage_trends_title", language), ", ", .current_country, ", ", min_yr_plots, "–", rev_yr),
    x = t_lookup("axis_year", language), 
    y = NULL, 
    fill = t_lookup("coverage", language),
    caption = paste0(
      t_lookup("caption_summary_hpv", language), " ",
      t_lookup("caption_summary_colors", language), "\n",
      if (survey_years_label != "") {
        paste0(t_lookup("caption_summary_survey_prefix", language), " (", survey_years_label, ").")
      } else {
        # Wrap the lookup in gsub to swap the placeholder with the country name
        gsub("{ctryn}", ctryn, t_lookup("caption_summary_no_survey", language), fixed = TRUE)
      }
    )
  ) +
  theme(
    plot.title         = element_text(hjust = 0.5, size = 14),
    plot.subtitle      = element_text(hjust = 0.5, size = 11),
    strip.background   = element_rect(fill = "#0083CF", color = NA),
    strip.text         = element_text(color = "white", face = "bold", size = 11),
    panel.grid         = element_blank(),
    axis.text.x        = element_text(size = 9),
    axis.text.y        = element_text(size = 9),
    axis.title.x = element_blank(),
    panel.border       = element_rect(color = "grey80", fill = NA),
    panel.spacing      = unit(0.5, "lines"),
    legend.position    = "bottom",
    legend.box         = "horizontal",       
    legend.box.just    = "bottom",
    legend.box.spacing = unit(1, "cm"),    
    legend.key.width   = unit(2, "cm"),
    legend.text        = element_text(size = 11, vjust = 0.5)
  )

plt_summary_table

## ---------------------------------------------------------------------------------------------------------------------
### Percent Comparison Heatmap - each type compared to WUENIC
## ---------------------------------------------------------------------------------------------------------------------

# ── BUILD DIFF DATA ───────────────────────────────────────────────────────────
wuenic_ref <- summary_long %>%
  filter(Source == "WUENIC") %>%
  select(Vaccine, Year, wuenic_coverage = Coverage)

diff_long <- combined_long %>%
  filter(Source != "WUENIC") %>%
  left_join(wuenic_ref, by = c("Vaccine", "Year")) %>%
  mutate(
    diff_val = case_when(
      status == "Not Introduced" ~ NA_real_,
      status == "Missing"        ~ NA_real_,
      is.na(wuenic_coverage)     ~ NA_real_,
      TRUE                       ~ Coverage - wuenic_coverage
    ),
    diff_label = case_when(
      status == "Not Introduced" ~ "",
      status == "Missing"        ~ "—",
      is.na(wuenic_coverage)     ~ "—",
      TRUE                       ~ sprintf("%+.0f", diff_val)
    )
  )

wuenic_long <- combined_long %>% 
  filter(Source == "WUENIC")

# min and max differences for setting the scale
min_diff <- min(diff_long$diff_val, na.rm = TRUE)
max_diff <- max(diff_long$diff_val, na.rm = TRUE)
#max_abs_diff <- ceiling(max(abs(min_diff), abs(max_diff)) / 5) * 5 # round to nearest 5

# ── PLOT ──────────────────────────────────────────────────────────────────────
plt_diff_table <- ggplot() +
  
  # ── wuenic facet ──
  geom_tile(data = wuenic_long %>% filter(!status %in% c("Not Introduced", "Missing")),
            aes(x = Year_discrete, y = Vaccine, fill = fill_val),
            color = "white", linewidth = 0.4) +
  geom_tile(data = wuenic_long %>% filter(status == "Missing"),
            aes(x = Year_discrete, y = Vaccine, color = t_lookup("legend_missing_data", language)),
            fill = "#d3d3d3", linewidth = 0.4) +
  geom_text(data = wuenic_long, aes(x = Year_discrete, y = Vaccine, label = label),
            color = "white", size = 3.5) +
  # blue gradient for wuenic
  scale_fill_gradient(
    name = t_lookup("source_who_unicef_coverage", language),
    low = "#A5CDE3", high = "#0083CF",
    guide = guide_colorbar(order = 1, barheight = unit(0.4, "cm"), barwidth = unit(6, "cm"))
  ) +
  
  # ── diff facets: colored by pp difference ──
  ggnewscale::new_scale_fill() +
  geom_tile(data = diff_long, aes(x = Year_discrete, y = Vaccine, fill = diff_val),
            color = "white", linewidth = 0.4) +
  geom_tile(data = diff_long %>% filter(status == "Missing" | (is.na(diff_val) & status != "Not Introduced")),
            aes(x = Year_discrete, y = Vaccine, color = t_lookup("legend_missing_data", language)),
            fill = "#d3d3d3", linewidth = 0.4) +
  geom_text(data = diff_long, aes(x = Year_discrete, y = Vaccine, label = diff_label,
                                  color = ifelse(diff_val >= -10 & diff_val <= 10, "diff_black", "diff_white")), size = 3.5) +
  scale_color_identity() +
  scale_fill_gradientn(
    name     = t_lookup("legend_pp_difference", language),
    #colors  = c("#4b2a64", "#7f4aa6", "#b57edc", "#d6b8ee", "#e9ddf5", "#d6b8ee", "#b57edc", "#7f4aa6", "#4b2a64"),
    colours  = c("#4b2a64", "#7f4aa6", "#b57edc", "#d6b8ee", "white", "#d6b8ee", "#b57edc", "#7f4aa6", "#4b2a64"),
    values   = scales::rescale(seq(-max_diff, max_diff, length.out = 9)),
    limits   = c(-max_diff, max_diff),
    oob      = scales::squish,
    breaks = c(-max_diff, -max_diff / 2, 0, max_diff / 2, max_diff),
    labels   = c(paste0("−", max_diff, t_lookup("unit_pp", language)), 
                 paste0("−", max_diff / 2, t_lookup("unit_pp", language)), 
                 "0", 
                 paste0("+", max_diff / 2, t_lookup("unit_pp", language)), 
                 paste0("+", max_diff, t_lookup("unit_pp", language))),
    na.value = "white",
    guide    = guide_colorbar(order = 2, barheight = unit(0.4, "cm"), barwidth = unit(6, "cm"))
  ) +
  scale_color_manual(
    name = NULL, 
    values = c(
      "diff_black" = "black",
      "diff_white" = "white",
      setNames("#d3d3d3", t_lookup("legend_missing_data", language))
    ),
    breaks = t_lookup("legend_missing_data", language),
    guide = guide_legend(
      order = 3, label.position = "bottom", label.vjust = 1.5,
      keywidth = unit(0.2, "cm"), keyheight = unit(0.5, "cm"),
      override.aes = list(fill = "#d3d3d3", color = "#d3d3d3")
    )
  ) +
  facet_grid(. ~ Source, scales = "free_x", space = "free_x", labeller = as_labeller(facet_source_labels)) +
  theme_minimal() +
  labs(
    title = paste0(t_lookup("plt_coverage_compare_title", language), ", ", .current_country, ", ", min_yr, "–", max_yr),
    x = t_lookup("axis_year", language), 
    y = NULL,
    caption = paste0(
      get_text2("txt_caption_comparison_explanation", text_vars), "\n\n ",
      t_lookup("caption_summary_colors", language), "\n",
      if (survey_years_label != "") {
        paste0(t_lookup("caption_summary_survey_prefix", language), " (", survey_years_label, ").")
      } else {
        gsub("{ctryn}", ctryn, t_lookup("caption_summary_no_survey", language), fixed = TRUE)
      }
    )
  ) +
  theme(
    plot.title         = element_text(hjust = 0.5, size = 14),
    plot.subtitle      = element_text(hjust = 0.5, size = 11),
    strip.background   = element_rect(fill = "#0083CF", color = NA),
    strip.text         = element_text(color = "white", face = "bold", size = 11),
    panel.grid         = element_blank(),
    axis.text.x        = element_text(size = 9),
    axis.text.y        = element_text(size = 9),
    axis.title.x = element_blank(),
    panel.border       = element_rect(color = "grey80", fill = NA),
    panel.spacing      = unit(0.5, "lines"),
    legend.position    = "bottom",
    legend.box         = "horizontal",
    legend.box.just    = "bottom",
    legend.box.spacing = unit(1, "cm"),
    legend.key.width   = unit(2, "cm"),
    legend.text        = element_text(size = 11, vjust = 0.5)
  )

plt_diff_table

