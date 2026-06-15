# ── PREP COMMENTS DATA ────────────────────────────────────────────────────────
last_5_yrs <- (rev_yr - n_years_comparison_plot + 1):rev_yr

comments_country <- comments %>%
  filter(iso3c == toupper(.current_iso3c),
         year %in% last_5_yrs,
         !is.na(SOURCE_CMT), SOURCE_CMT != "", SOURCE_CMT != "NK") %>%
  select(iso3c, year, CMT_FIELDS, SOURCE_CMT) %>%
  mutate(SOURCE_CMT = case_when(
    SOURCE_CMT %in% c("-2222", "-4444") ~ "No comment provided",
    TRUE ~ SOURCE_CMT)) %>%
  mutate(CMT_FIELDS = case_when(
    CMT_FIELDS == "factor_accuracy_num"      ~ "Numerator Accuracy",
    CMT_FIELDS == "explanation_denom_source" ~ "Denominator Source",
    CMT_FIELDS == "factor_accuracy_denom"    ~ "Denominator Accuracy",
    TRUE ~ CMT_FIELDS)) %>%
  arrange(CMT_FIELDS, desc(year))

most_recent_yr <- comments_country %>% pull(year) %>% max(na.rm = TRUE)

# ── TXT_ VARIABLES FOR TRANSLATION ───────────────────────────────────────────
txt_comments_col_year          <- "Year"
txt_comments_col_comment       <- "Comment"
txt_comments_col_type          <- "Comment Type"
txt_comments_label_num         <- "Numerator Accuracy"
txt_comments_label_den_src     <- "Denominator Source"
txt_comments_label_den_acc     <- "Denominator Accuracy"
txt_comments_title_prefix      <- "Admin Data Comments:"
txt_comments_title_summary     <- "Admin Data Comments Summary:"
txt_comments_note_num          <- paste0("Factors limiting the accuracy of the numerator as reported by ", .current_country, ".")
txt_comments_note_den_src      <- "Explanation of how the denominators (target population) are obtained."
txt_comments_note_den_acc      <- paste0("Factors limiting the accuracy of the denominator as reported by ", .current_country, ".")
txt_comments_footnote_years    <- paste0("Last ", n_years_comparison_plot, " years shown (", min(last_5_yrs), "–", rev_yr, ").")
txt_comments_note_summary      <- paste0("Summary of all admin data comment fields for the most recent year with available data (", most_recent_yr, ").")
txt_appendix_title <- "Supplementary Information"

# in comments_data.R (or a new file e.g. text_vars_main.R sourced before narrative-translation_deepl.R)

txt_cover_subtitle         <- "Administrative Data Quality Review Report, {rev_yr}"
txt_sched_title            <- "Vaccine Schedule, {.current_country}"
txt_sched_footnote         <- "Vaccine schedule from {country_sched_year} (most recent for {CountryName}), as reported to WIISE."
txt_sec_coverage_comparison <- "Coverage Comparison Between WUENIC, Admin, and Official Estimates"
txt_summary_table          <- "Coverage by vaccine and data source (WUENIC, Admin, Official, Survey) for the most recent {n_years_comparison_plot} years."
txt_diff_table             <- "Difference in percentage points between administrative, official, and survey coverage and WUENIC estimates (e.g., Admin - WUENIC) for the most recent {n_years_comparison_plot} years."
txt_vax_line               <- "Time-series of WUENIC estimates, administrative coverage, official government estimates, and survey data by vaccine, {min_yr_plots}–{rev_yr}."
txt_three_lines            <- "Coverage trends for DTP1, DTP3, and MCV1 by data source, {min_yr_plots}–{rev_yr}."
txt_admin_wuenic           <- "Comparison of administrative coverage and WUENIC estimates by vaccine. Red lines indicate gaps exceeding ±{pct_threshold * 100} percentage points."
txt_admin_wuenic2          <- "Comparison of administrative coverage and WUENIC estimates by vaccine. Red lines indicate gaps exceeding ±{second_pct_threshold * 100} percentage points."
txt_admin_official         <- "Comparison of administrative coverage and official government estimates. Red lines indicate gaps exceeding ±{pct_threshold * 100} percentage points."
txt_sec_admin_flags        <- "Admin Coverage Flags"
txt_sec_threshold_expl     <- "This is why we use the 10% threshold."
txt_cov_flags              <- "Admin coverage flagged where values exceed 100% (orange) or change by more than ±{pct_threshold * 100} percentage points from the previous year (red), {min_yr_plots}–{rev_yr}."
txt_three_flags            <- "Same flag logic applied to DTP1, DTP3, and MCV1 only, {min_yr_plots}–{rev_yr}."
txt_sec_numerator_checks   <- "Numerator Checks"
txt_stockout_title         <- "Vaccine Stockouts"
txt_stockout_table         <- "Years in which a national or subnational vaccine stockout was recorded.\n\nThis table provides context for interpreting the numerator and denominator trends and flags in the following slides, as stockouts may contribute to drops in coverage or data quality issues."
txt_all_numerators         <- "Number of children vaccinated by vaccine and year."
txt_numerator_line         <- "Percent change from previous year in the number of children vaccinated (numerator) by vaccine. Red and orange points flag changes exceeding ± 10pp and ± 5pp, respectively. Blue line shows raw counts on the secondary axis. Orange shading indicates a vaccine stockout in that year.\n\n{outlier_subtitle}"
txt_admin_cov_visit        <- "Admin coverage of vaccines co-administered at the {cluster_name} visit per the national schedule. {coadmin_labels[cluster_name]}"
txt_sec_denominator_checks <- "Denominator Checks"
txt_denom_line             <- "Number of children in the target population (denominator) by vaccine and year. Blue points show normal trends in population growth, while red points flag years with a direction switch in population growth.\n\nUnder normal circumstances, target populations should follow a smooth, steady trajectory rather than fluctuating. Sudden switches in direction—such as an increasing population abruptly decreasing the following year—are demographically unnatural and typically indicate administrative errors, shifting census boundaries, or changes in reporting data sources. Genuine reversals are rare and generally limited to extreme disruptions like humanitarian refugee crises, cross-border migrations, or the statistical noise inherent to very small island populations."
txt_births_si              <- "Comparison of live births (BCG denominator) and surviving infants (DTP1 denominator) over time, {min_yr_plots}–{rev_yr}."
txt_denom_pct_change       <- "Number of children in the live births (BCG) and surviving infants (DTP1) administrative populations by year. Blue points show normal trends in population growth, while red points flag years with a direction switch in population growth.\n\nUnder normal circumstances, target populations should follow a smooth, steady trajectory rather than fluctuating. Sudden switches in direction—such as an increasing population abruptly decreasing the following year—are demographically unnatural and typically indicate administrative errors, shifting census boundaries, or changes in reporting data sources. Genuine reversals are rare and generally limited to extreme disruptions like humanitarian refugee crises, cross-border migrations, or the statistical noise inherent to very small island populations."
txt_sec_dropout_coadmin    <- "Vaccine Dropout & Co-administration"
txt_dropout                <- "Admin coverage for key vaccine pairs (DTP1→DTP3, DTP1→MCV1) with dropout rate shown as triangles. High dropout may indicate supply, access, or demand barriers. \n\nA normal dropout rate means fewer children received the second dose than the first, which is expected. A negative dropout rate means more children received the second dose than the first—an impossible result that points to data quality issues."
txt_coadmin                <- "Admin coverage for DTP3 and PCVC, which are co-administered at {dtp3_pcvc_time} in {.current_country}.\n\nDivergence between the two series may indicate a reporting inconsistency."
txt_sec_coadmin_schedule   <- "Co-administration by Schedule Time Point"
txt_coadmin_schedule       <- "Admin coverage of vaccines co-administered at the {cluster_name} visit per the national schedule. {coadmin_labels[cluster_name]}"
txt_sec_data_availability  <- "Data Availability"
txt_intro_table_title      <- "Vaccine Introductions"
txt_intro_table_foot       <- "Year in which each vaccine was introduced nationally, partially, for risk groups, or risk areas.\n\nWe present the vaccination introductions table here to contextualize the data availability heatmap on the following slide."
txt_missing_heatmap        <- "Heatmap showing whether admin data are present (green), missing (red), or not applicable because the vaccine had not yet been introduced or reporting had not started (white), by vaccine and year."
txt_sec_admin_comments     <- "Admin Data Comments"
txt_appendix_title         <- "Supplementary Information"
txt_wuenic_heatmap         <- "WUENIC coverage estimates across all vaccines, {min_yr}–{max_yr}. Blank cells indicate years prior to vaccine introduction."
txt_caption_missing_admin  <- "Gaps in line indicate missing admin data for that year."
txt_caption_average_gap    <- "Average gap calculated for the most recent 5 years of data, with NAs removed."
txt_label_num_vax          <- "Number of Children Vaccinated"
txt_label_num_vax2         <- "Number of Children Vaccinated by Vaccine and Year"
txt_title_perc_comparison  <- "WUENIC panel shows absolute coverage. All other panels show percentage point difference vs. WUENIC estimate. "
txt_caption_comparison_explanation <- "WUENIC panel shows absolute coverage. All other panels show percentage point difference vs. WUENIC estimate."
txt_title_admin_wuenic     <- "Admin vs. WUENIC Coverage Estimate"
txt_title_admin_official   <- "Admin vs. Official (Government Estimate) Coverage"
txt_vaccines_supplies      <- "Vaccines / supplies"
txt_num_change_title       <- "Percent Change from Previous Year in Numerator (# Children Vaccinated)"
txt_den_change_title       <- "Percent Change from Previous Year in Denominator (# Children in Target)"
txt_raw_count              <- "Raw Count of Children Vaccinated"
txt_target_pop             <- "Children in Target Population"
txt_no_denom_avail_msg     <- "No denominator data available for"
txt_note_no_denom_avail    <- "NOTE: Not enough denominator data to calculate percent change for:"
txt_title_births_si        <- "Live Births & Surviving Infants (UNPD) vs. Admin Denominators"
txt_live_births <- "Live Births (UNPD)"
txt_surviving_infants <- "Surviving Infants (UNPD)"
txt_bcg_target <- "BCG Admin Target Population"
txt_dtp1_target <- "DTP1 Admin Target Population"
txt_denominator <- "Denominator"
txt_target_population <- "Target Population"
txt_key_dropout <- "Key Vaccine Dropout"
txt_denom_stability <- "Admin Denominator Stability Check"
txt_raw_denom <- "Raw Denominator Count"
txt_all_antigens_present <- "All antigens scheduled for this time point are present in the dataset."
txt_national_intro <- "National introduction"
txt_not_introduced <- "Not introduced"
txt_hpv <- "HPV (Human Papilloma Virus) vaccine"
txt_num_change_explanation <- "Raw counts of children vaccinated (numerator) by vaccine and year. Points are colored by the percent change from previous year in the number of children vaccinated. Green point = stable year-to-year count of children vaccinated; orange/red point = high fluctuation in number of children vaccinated, which may indicate data quality issues."
txt_numerator_bars <- "Number of children vaccinated (numerator) by vaccine and year. White diagonal lines indicate years with a vaccine stockout, which may explain any drops in the number of children vaccinated."
txt_numerator_bars_selection <- "Number of children vaccinated (numerator) by vaccine (DTP1, DTP3, and MCV1 only) and year. White diagonal lines indicate years with a vaccine stockout, which may explain any drops in the number of children vaccinated."



