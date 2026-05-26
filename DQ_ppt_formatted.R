# ==========================================================================================================================================
# Script Name: Produce Formatted Data Quality PPT
# Author: Grace Legris, glegris@unicef.org
# ==========================================================================================================================================

options(scipen = 999)
x <- .current_iso3c

## ── DATA LOADING ─────────────────────────────────────────────────────────────

# master WUENIC dataset
wuenic_master <- read.csv(file.path(DummyDataDir, "wuenic-master_2025rev.csv")) %>% 
  mutate(Vaccine = case_when(
    Vaccine == "hepbb" ~ "HepBB", Vaccine == "hepb3" ~ "HepB3",
    Vaccine == "hib3"  ~ "Hib3",  Vaccine == "rotac" ~ "RotaC",
    Vaccine == "menga" ~ "MengA", Vaccine == "hpvc"  ~ "HPVc",
    TRUE ~ toupper(Vaccine))) %>%
  rename(Admin = AdministrativeCoverage,
         Official = GovernmentEstimate,
         Survey = SurveyInformation) %>%
  relocate(Admin, .after = WUENIC) %>%
  relocate(Official, .after = Admin) %>%
  relocate(Survey, .after = Official) %>%
  relocate(ChildrenVaccinated, .after = Survey) %>%
  relocate(ChildrenInTarget, .after = ChildrenVaccinated)

wuenic_master$Vaccine <- factor(
  wuenic_master$Vaccine,
  levels = c("BCG","HepBB","DTP1","DTP3","Hib3","HepB3","PCVC","RotaC",
             "POL3","IPV1","IPVC","MCV1","RCV1","MCV2","YFV","MengA","HPVc"))

wuenic_master_current <- wuenic_master %>% 
  filter(ISOCountryCode == .current_iso3c) %>% 
  filter(Year >= min_yr_plots, Year <= rev_yr)

# regional info
regional_info <- regional_info %>%
  filter(iso3c == toupper(.current_iso3c))

# add stockout markers
wuenic_stockouts_master <- wuenic_stockouts_clean %>%
  mutate(iso3c = toupper(iso3c),
         vaccine = case_when(
           vaccine == "hepbb" ~ "HepBB", vaccine == "hepb3" ~ "HepB3",
           vaccine == "hib3"  ~ "Hib3",  vaccine == "rotac" ~ "RotaC",
           vaccine == "menga" ~ "MengA", vaccine == "hpvc"  ~ "HPVc",
           TRUE ~ toupper(vaccine)))

wuenic_master_current <- wuenic_master_current %>%
  mutate(iso3c_upper = toupper(ISOCountryCode)) %>% 
  left_join(wuenic_stockouts_master %>% select(iso3c, year, vaccine, any_stockout),
            by = c("iso3c_upper" = "iso3c", "Year" = "year", "Vaccine" = "vaccine")) %>%
  mutate(any_stockout = if_else(is.na(any_stockout), 0, any_stockout))

# vaccine introduction table
tbl_intro_r <- wiise_intro %>%
  mutate(iso3c = tolower(iso3c)) %>%
  filter(iso3c == .current_iso3c) %>%
  select(vaccine_name, contains("year_intro")) %>%
  arrange(vaccine_name) %>%
  rename(Vaccine              = vaccine_name,
         `National introduction` = year_intro_national,
         `Partial introduction`  = year_intro_partial,
         `Risk groups`           = year_intro_risk_groups,
         `Risk areas`            = year_intro_risk_area) %>%
  select(where(~ !all(is.na(.) | . == ""))) %>%
  mutate(across(where(is.numeric), ~ as.character(.)))

no_data <- function(df) nrow(df) == 0

## ── GENERATE ALL DATA QUALITY PLOTS ─────────────────────────────────────────
source(file.path(PrjDir, "R/tbl_schedule.R"))

if (!no_data(tbl_intro_r)) {
  source(file.path(PrjDir, "R/tbl_intro.R"))
}

source(file.path(PrjDir, "R/tbl_stock.R"))

# ── dq plots from figs_tables_ppt.R script ─────────────────────────────
source(file.path(PrjDir, "figs_tables_ppt.R"), echo = FALSE, print.eval = FALSE)

# intro paragraph
intro_paragraph <- paste0(
  regional_info$country, " is located in ", str_to_title(regional_info$un_region),
  ", within the UNICEF ", toupper(regional_info$region_unicef_ops), " region.",
  if_else(regional_info$region_ldc    == "ldc",          " It is classified as a least developed country (LDC).", ""),
  if_else(regional_info$region_lmic   == "lmic",         " It is a low- or middle-income country (LMIC).", ""),
  if_else(regional_info$fragility     == "conflict",      " The country is classified as a fragile and conflict-affected state.", ""),
  if_else(regional_info$gavi          == "gavi",          " It is eligible for Gavi support.", ""),
  if_else(regional_info$region_imm_sp_priority == "imm_sp_priority",
          paste0(" ", regional_info$country, " is an immunization special priority country."), ""),
  if_else(regional_info$unaids_highimpact == "hiimpact",  " It is a UNAIDS high-impact country.", ""),
  if_else(regional_info$big_forty     == "big_forty",     " It is one of the Big 40 priority countries for immunization.", "")
)

## ── CONVERT PLOTS TO EDITABLE DML ───────────────────────────────────────────
plot_names     <- ls(pattern = "^plt_")
ggplot_objects <- mget(plot_names, envir = .GlobalEnv)

for (name in names(ggplot_objects)) {
  assign(
    paste0("dml_", name),
    rvg::dml(ggobj = ggplot_objects[[name]], editable = TRUE),
    envir = .GlobalEnv
  )
}

## ── BUILD POWERPOINT ─────────────────────────────────────────────────────────
doc <- read_pptx(str_glue("{utils}/region-specific-blank-slides copy.pptx"))

slide_title <- .current_country
rect <- rectGrob(gp = gpar(fill = "black", col = NA)) # top-line message underline

# ── COVER ────────────────────────────────────────────────────────────────────
doc <- add_slide(doc, layout = "cover_country", master = "Office Theme")
doc <- ph_with(
  x = doc,
  block_list(fpar(
    ftext(.current_country,
          prop = fp_text(font.size = 40, color = "white")),
    fp_p = fp_par(text.align = "center"))),
  location = ph_location("body", left = 1.9, top = 2.86, width = 9.5, height = 2))

# subtitle: report type
doc <- ph_with(
  x = doc,
  block_list(fpar(
    ftext(paste("Administrative Data Quality Review Report,", rev_yr),
          prop = fp_text(font.size = 22, color = "white")),
    fp_p = fp_par(text.align = "center"))),
  location = ph_location("body", left = 3.34, top = 3.92, width = 7, height = 1.22))

doc <- remove_slide(doc, index = 1) # remove the blank first slide in the template

# ── INTRO SLIDE (wuenic info) ─────────────────────────────────────────────────
#doc <- add_slide(doc, layout = "intro_slide", master = "Office Theme")

# ── EJRF INFO-- ───────────────────────────────────────────────────────────────
doc <- add_slide(doc, layout = "1_intro_slide", master = "Office Theme")

# ── DEFINITIONS ───────────────────────────────────────────────────────────────
doc <- add_slide(doc, layout = "definitions", master = "Office Theme")

# Country context paragraph
# doc <- ph_with(
#   x = doc,
#   block_list(fpar(
#     ftext(intro_paragraph,
#           prop = fp_text(font.size = 14, color = "white")),
#     fp_p = fp_par(text.align = "left"))),
#   location = ph_location("body", left = 0.57, top = 1.5, width = 12.2, height = 4, bg = "#203864"))

# ── SECTION DIVIDER: SCHEDULE & STOCKOUTS ────────────────────────────────────
func_slide_bb("Schedule & Stockouts")

# vaccine schedule table
schedule_year <- wiise_schedule %>% 
  filter(iso3c == x) %>% 
  pull(year) %>% 
  max(na.rm = TRUE)
if (nrow(tbl_schedule_r) > 0 && ncol(tbl_schedule_r) > 0) {
  doc <- add_slide(doc, layout = "data_1", master = "Office Theme")
  doc <- ph_with(doc,
                 value = fpar(ftext("Vaccine Schedule",
                                    prop = fp_text(font.size = 28, color = "black", font.family = "Calibri"))),
                 location = ph_location(left = 0.6, top = 0.6, width = 8, height = 1))
  doc <- ph_with(x = doc, value = tbl_schedule,
                 location = ph_location(left = 0.6, top = 1.5, width = total_width_sched, height = 5))
  func_slide_v_txt(paste0("Vaccine schedule as reported to WIISE. Latest update: ", schedule_year, "."))
}

# vaccine introductions table
if (!no_data(tbl_intro_r) && nrow(tbl_intro_r) > 0 && ncol(tbl_intro_r) > 0) {
  doc <- add_slide(doc, layout = "data_1", master = "Office Theme")
  doc <- ph_with(doc,
                 value = fpar(ftext("Vaccine Introductions",
                                    prop = fp_text(font.size = 28, color = "black", font.family = "Calibri"))),
                 location = ph_location(left = 0.6, top = 0.6, width = 8, height = 1))
  doc <- ph_with(x = doc, value = tbl_intro,
                 location = ph_location(left = 0.6, top = 1.5, width = 10, height = 5))
  func_slide_v_txt("Year in which each vaccine was introduced nationally, partially, for risk groups, or risk areas.")
}

# stockout table
if (nrow(tbl_stock_r) > 0 && ncol(tbl_stock_r) > 0) {
  doc <- add_slide(doc, layout = "data_no_logos", master = "Office Theme")
  doc <- ph_with(doc,
                 value = fpar(ftext("Vaccine Stockouts",
                                    prop = fp_text(font.size = 28, color = "black", font.family = "Calibri"))),
                 location = ph_location(left = 0.6, top = 0.6, width = 8, height = 1))
  doc <- ph_with(x = doc, value = tbl_stock,
                 location = ph_location(left = 0.6, top = 1.5, width = 10, height = 5))
  func_slide_v_txt("Years in which a national or subnational vaccine stockout was recorded.")
}

# ── SECTION DIVIDER: COVERAGE OVERVIEW ───────────────────────────────────────
func_slide_bb("Coverage Overview")

# WUENIC coverage heatmap
min_yr <- min(df$year)
max_yr <- max(df$year)
func_slide_v(dml_plt_all_vax_heatmap)
func_slide_v_txt(paste0("WHO/UNICEF coverage estimates across all vaccines, ", min_yr, "–", max_yr, ".",
                        " Blank cells indicate years prior to vaccine introduction."))

# summary table: coverage by source
func_slide_v(dml_plt_summary_table)
func_slide_v_txt(paste0(
  "Coverage by vaccine and data source (WHO/UNICEF, Admin, Official) for the most recent ",
  n_years_comparison_plot, " years. Colour scale: red (<60%) to green (≥90%). Missing cells shown in grey. Blank cells indicate years prior to vaccine introduction."))

# ── SECTION DIVIDER: COVERAGE TRENDS BY SOURCE ───────────────────────────────
func_slide_bb("Coverage Trends by Source")

# all vaccines — line chart
func_slide_v(dml_plt_all_vax_line)
func_slide_v_txt(paste0(
  "Time-series of WHO/UNICEF estimates, administrative coverage, official government estimates, ",
  "and survey data by vaccine, ", min_yr_plots, "–", rev_yr, "."))

# DTP1, DTP3, MCV1 — line chart
func_slide_v(dml_plt_selected_vax_line)
func_slide_v_txt(paste0(
  "Coverage trends for DTP1, DTP3, and MCV1 by data source, ", min_yr_plots, "–", rev_yr, "."))

# ── SECTION DIVIDER: ADMIN DATA FLAGS ────────────────────────────────────────
func_slide_bb("Admin Coverage Flags")

# ── THRESHOLD EXPLANATION ─────────────────────────────────────────────────────
func_slide_bb("this is why we use the 10% threshold..")

# all vaccines — flag chart
func_slide_v(dml_plt_coverage_flags)
func_slide_v_txt(paste0(
  "Admin coverage flagged where values exceed 100% (orange) or change by more than ±",
  pct_threshold * 100, " percentage points from the previous year (red), ", min_yr_plots, "–", rev_yr, "."))

# DTP1, DTP3, MCV1 — flag chart
func_slide_v(dml_plt_selected_coverage_flags)
func_slide_v_txt(paste0(
  "Same flag logic applied to DTP1, DTP3, and MCV1 only, ", min_yr_plots, "–", rev_yr, "."))

# ── SECTION DIVIDER: ADMIN VS ESTIMATES ──────────────────────────────────────
func_slide_bb("Admin vs. Estimates")

# admin vs WHO/UNICEF (first pct change threshold)
func_slide_v(dml_plt_admin_vs_wuenic)
func_slide_v_txt(paste0(
  "Comparison of administrative coverage and WHO/UNICEF estimates by vaccine. ",
  "Red lines indicate gaps exceeding ±", pct_threshold * 100, " percentage points."))

# admin vs WHO/UNICEF (second pct change threshold)
func_slide_v(dml_plt_admin_vs_wuenic2)
func_slide_v_txt(paste0(
  "Comparison of administrative coverage and WHO/UNICEF estimates by vaccine. ",
  "Red lines indicate gaps exceeding ±", second_pct_threshold * 100, " percentage points."))

# admin vs official
func_slide_v(dml_plt_admin_vs_official)
func_slide_v_txt(paste0(
  "Comparison of administrative coverage and official government estimates. ",
  "Red lines indicate gaps exceeding ±", pct_threshold * 100, " percentage points."))

# ── SECTION DIVIDER: NUMERATOR CHECKS ────────────────────────────────────────
func_slide_bb("Numerator Checks")

# year-to-year % change in vaccinated children (all vaccines)
func_slide_v(dml_plt_perc_change_line)
func_slide_v_txt(paste0(
  "Year-to-year percentage change in the number of children vaccinated (numerator) by vaccine. ",
  "Red points flag changes exceeding ±", pct_threshold * 100,
  "%. Green line shows raw counts on the secondary axis. ",
  "Orange shading indicates a vaccine stockout in that year. \n\n", outlier_subtitle))

for (cluster_name in names(plt_numerator_list)) {
  
  dml_plt <- rvg::dml(ggobj = plt_numerator_list[[cluster_name]], editable = TRUE)
  func_slide_v(dml_plt)
  func_slide_v_txt(paste0(
    "Admin coverage of vaccines co-administered at the ", cluster_name, " visit ",
    "per the national schedule. ", coadmin_labels[cluster_name]
  ))
}


# ── SECTION DIVIDER: DENOMINATOR CHECKS ──────────────────────────────────────
func_slide_bb("Denominator Checks")

# year-to-year % change in target population (all vaccines)
func_slide_v(dml_plt_denom_change)
func_slide_v_txt(paste0(
  "Year-to-year percentage change in the number of children in the target population (denominator) ",
  "by vaccine. Red points flag changes exceeding ±", pct_threshold * 100,
  "%. Green line shows raw counts on the secondary axis."))

# live births (BCG) vs surviving infants (DTP1)
func_slide_v(dml_plt_births_vs_si)
func_slide_v_txt(paste0(
  "Comparison of live births (BCG denominator) and surviving infants (DTP1 denominator) ",
  "over time, ", min_yr_plots, "–", rev_yr, ". Large or persistent divergence may indicate a denominator issue."))

# % change in BCG and DTP1 denominators
func_slide_v(dml_plt_denom_pct_change)
func_slide_v_txt(paste0(
  "Year-to-year percentage change in BCG (live births) and DTP1 (surviving infants) denominators. ",
  "Red points flag changes exceeding ±", pct_threshold * 100, "%."))

# ── SECTION DIVIDER: DROPOUT & CO-ADMINISTRATION ────────────────────────────
func_slide_bb("Dropout & Co-administration")

# dropout rates
func_slide_vnologo(dml_plt_dropout_with_rate)
func_slide_v_txt(paste0(
  "Admin coverage for key vaccine pairs (DTP1→DTP3, DTP1→MCV1) with dropout rate shown as bars. ",
  "High dropout may indicate supply, access, or demand barriers."))

# DTP3–PCVC co-administration
func_slide_v(dml_plt_coadmin_dtp_pcv)
func_slide_v_txt(paste0(
  "Admin coverage for DTP3 and PCVC, which are co-administered at ", dtp3_pcvc_time, " in ", .current_country,
  ".\n\nDivergence between the two series may indicate a reporting inconsistency."))


# Co-administration by schedule time point (dynamic checks for whichever vaccines are co-administered in this country)
func_slide_bb("Co-administration by Schedule Time Point")

for (cluster_name in names(plt_coadmin_list)) {
  
  dml_plt <- rvg::dml(ggobj = plt_coadmin_list[[cluster_name]], editable = TRUE)
  func_slide_v(dml_plt)
  func_slide_v_txt(paste0(
    "Admin coverage of vaccines co-administered at the ", cluster_name, " visit ",
    "per the national schedule. ", coadmin_labels[cluster_name]
  ))
}

# # 6-week vaccine cluster
# func_slide_v(dml_plt_6wk)
# func_slide_v_txt(paste0("Admin coverage of all vaccines administered at 6 weeks per the national schedule.\n\n", text_label_6wk))
# 
# # 14-week vaccine cluster
# func_slide_v(dml_plt_14wk)
# func_slide_v_txt(paste0("Admin coverage of all vaccines administered at 14 weeks per the national schedule.\n\n", text_label_14wk))

# ── SECTION DIVIDER: DATA AVAILABILITY ───────────────────────────────────────
func_slide_bb("Data Availability")

# missing data heatmap
func_slide_v(dml_plt_missing_heatmap)
func_slide_v_txt(paste0(
  "Heatmap showing whether admin data are present (green), missing (red), or not applicable ",
  "because the vaccine had not yet been introduced (white), by vaccine and year."))

## ── SECTION DIVIDER: ADMIN DATA COMMENTS ─────────────────────────────────────
func_slide_bb("Admin Data Comments")

# ── PREP COMMENTS DATA ────────────────────────────────────────────────────────
last_5_yrs <- (rev_yr - n_years_comparison_plot + 1):rev_yr

comments_country <- comments %>%
  filter(iso3c == .current_iso3c,
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

# ── HELPER: BUILD A FLEXTABLE FOR ONE CMT_FIELDS CATEGORY ────────────────────
make_comments_tbl <- function(df, category) {
  df_cat <- df %>% filter(CMT_FIELDS == category) %>% select(year, SOURCE_CMT)
  
  if (nrow(df_cat) == 0) return(NULL)
  
  colnames(df_cat) <- c("Year", "Comment")
  
  df_cat$Year <- as.character(df_cat$Year)
  
  ft <- flextable(df_cat) %>%
    set_header_labels(Year = "Year", Comment = "Comment") %>%
    width(j = "Year",    width = 0.8) %>%
    width(j = "Comment", width = 8.2) %>%
    hrule(rule = "auto", part = "body") %>%       # ← allow rows to auto-expand
    set_table_properties(layout = "fixed", opts_word = list(split = FALSE)) %>%
    fontsize(size = 13, part = "all") %>%
    font(fontname = "Calibri", part = "all") %>%
    bold(part = "header") %>%
    bg(bg = "#0058AB", part = "header") %>%
    color(color = "white", part = "header") %>%
    bg(i = seq(2, nrow(df_cat), by = 2), bg = "#EEF4FB", part = "body") %>%
    border_remove() %>%
    hline(part = "body", border = fp_border(color = "#D0D0D0", width = 0.5)) %>%
    align(j = "Year",    align = "center", part = "all") %>%
    align(j = "Comment", align = "left",   part = "body") %>%
    valign(valign = "top", part = "body")
  
  ft
}

# ── RENDER ONE SLIDE PER CATEGORY (only if data exists) ──────────────────────
comment_categories <- list(
  list(label = "Numerator Accuracy",  field = "Numerator Accuracy",
       note  = paste0("Factors limiting the accuracy of the numerator as reported by ", .current_country, ".")),
  list(label = "Denominator Source",  field = "Denominator Source",
       note  = "Explanation of how the denominators (target population) are obtained."),
  list(label = "Denominator Accuracy", field = "Denominator Accuracy",
       note  = paste0("Factors limiting the accuracy of the denominator as reported by ", .current_country, "."))
)

for (cat in comment_categories) {
  ft <- make_comments_tbl(comments_country, cat$field)
  
  if (is.null(ft)) next   # skip slide entirely if no data for this category
  
  doc <- add_slide(doc, layout = "data_1", master = "Office Theme")
  
  # slide title
  doc <- ph_with(
    x        = doc,
    value    = fpar(ftext(
      paste0("Admin Data Comments: ", cat$label, " (", min(last_5_yrs), "–", rev_yr, ")"),
      prop   = fp_text(font.size = 24, color = "black", font.family = "Calibri"))),
    location = ph_location(left = 0.6, top = 0.4, width = 12, height = 0.8))
  
  # table body
  doc <- ph_with(
    x        = doc,
    value    = ft,
    location = ph_location(left = 0.6, top = 1.3, width = 8.2, height = 5.2))
  
  # footnote via your existing helper
  func_slide_v_txt(paste0(
    cat$note, " Last ", n_years_comparison_plot, " years shown (", min(last_5_yrs), "–", rev_yr, ")."))
}

# ── OPTIONAL: COMBINED SUMMARY SLIDE (all 3 categories, most recent year only) ─
most_recent_yr <- comments_country %>% pull(year) %>% max(na.rm = TRUE)

comments_summary <- comments_country %>%
  filter(year == most_recent_yr) %>%
  select(CMT_FIELDS, SOURCE_CMT) %>%
  rename(`Comment Type` = CMT_FIELDS, Comment = SOURCE_CMT)

if (nrow(comments_summary) > 0) {
  
  ft_summary <- flextable(comments_summary) %>%
    width(j = "`Comment Type`", width = 2.2) %>%
    width(j = "Comment",        width = 7) %>%
    fontsize(size = 13, part = "all") %>%
    font(fontname = "Calibri", part = "all") %>%
    bold(part = "header") %>%
    bg(bg = "#0058AB", part = "header") %>%
    color(color = "white", part = "header") %>%
    bg(i = seq(2, nrow(comments_summary), by = 2), bg = "#EEF4FB", part = "body") %>%
    border_remove() %>%
    hline(part = "body", border = fp_border(color = "#D0D0D0", width = 0.5)) %>%
    align(j = "`Comment Type`", align = "left", part = "all") %>%
    align(j = "Comment",        align = "left", part = "body") %>%
    valign(valign = "top", part = "body") %>%
    set_table_properties(layout = "fixed")
  
  doc <- add_slide(doc, layout = "data_1", master = "Office Theme")
  
  doc <- ph_with(
    x        = doc,
    value    = fpar(ftext(
      paste0("Admin Data Comments Summary: ", most_recent_yr),
      prop   = fp_text(font.size = 24, color = "black", font.family = "Calibri"))),
    location = ph_location(left = 0.6, top = 0.4, width = 12, height = 0.8))
  
  doc <- ph_with(
    x        = doc,
    value    = ft_summary,
    location = ph_location(left = 0.6, top = 1.3, width = 8, height = 4))
  
  func_slide_v_txt(paste0(
    "Summary of all admin data comment fields for the most recent year with available data (",
    most_recent_yr, ")."))
}

# ── SAVE ─────────────────────────────────────────────────────────────────────
folder_path <- str_glue("{dqfolder}/outputs/all_countries")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

print(doc, target = file.path(folder_path, paste0(.current_country, "_DQ.pptx")))

#message("✓ Data quality report saved to: ", folder_path)
