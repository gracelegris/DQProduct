# ==========================================================================================================================================
# Script Name: Produce Formatted Data Quality PPT
# Author: Grace Legris, glegris@unicef.org
# ==========================================================================================================================================

options(scipen = 999)

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
doc <- read_pptx(str_glue("{utils}/intervention_analysis_template.pptx"))

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

# ── INTRO TO REPORT SLIDE ─────────────────────────────────────────────────────
doc <- add_slide(doc, layout = "report_introduction", master = "Office Theme")

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

# vaccine schedule table
schedule_year <- wiise_schedule %>% 
  filter(iso3c == x) %>% 
  pull(year) %>% 
  max(na.rm = TRUE)
if (nrow(tbl_schedule_r) > 0 && ncol(tbl_schedule_r) > 0) {
  doc <- add_slide(doc, layout = "data_1", master = "Office Theme")
  doc <- ph_with(doc,
                 value = fpar(ftext(paste0("Vaccine Schedule, ", current_country),
                                    prop = fp_text(font.size = 28, color = "black", font.family = "Calibri"))),
                 location = ph_location(left = 0.6, top = 0.6, width = 8, height = 1))
  doc <- ph_with(x = doc, value = tbl_schedule,
                 location = ph_location(left = 0.6, top = 1.5, width = total_width_sched, height = 5))
  txt_schedule <- paste0("Vaccine schedule from ", country_sched_year, " (most recent for ", CountryName, "), as reported to WIISE.")
  func_slide_v_txt(txt_schedule)
}

# ── SECTION DIVIDER: COVERAGE TRENDS BY SOURCE ───────────────────────────────
func_slide_bb("Coverage Comparison Between WUENIC, Admin, and Official Estimates")

# summary table: coverage by source
func_slide_v(dml_plt_summary_table)
txt_summary_table <- paste0(
  "Coverage by vaccine and data source (WHO/UNICEF, Admin, Official) for the most recent ",
  n_years_comparison_plot, " years.")
func_slide_v_txt(txt_summary_table)

# coverage trends by source (comparison to who/unicef coverage)
func_slide_v(dml_plt_diff_table)
txt_diff_table <- paste0(
  "Difference in percentage points between administrative, official, and survey coverage and WHO/UNICEF estimates (e.g., Admin - WUENIC) for the most recent ", 
  n_years_comparison_plot, " years.")
func_slide_v_txt(txt_diff_table)

# all vaccines — line chart
func_slide_v(dml_plt_all_vax_line)
txt_vax_line <- paste0(
  "Time-series of WHO/UNICEF estimates, administrative coverage, official government estimates, ",
  "and survey data by vaccine, ", min_yr_plots, "–", rev_yr, ".")
func_slide_v_txt(txt_vax_line)

# DTP1, DTP3, MCV1 — line chart
func_slide_v(dml_plt_selected_vax_line)
txt_three_lines <- paste0("Coverage trends for DTP1, DTP3, and MCV1 by data source, ", min_yr_plots, "–", rev_yr, ".")
func_slide_v_txt(txt_three_lines)

# admin vs WHO/UNICEF (first pct change threshold)
func_slide_v(dml_plt_admin_wuenic_1)
txt_admin_wuenic <- paste0("Comparison of administrative coverage and WHO/UNICEF estimates by vaccine. ",
                           "Red lines indicate gaps exceeding ±", pct_threshold * 100, " percentage points.")
func_slide_v_txt(txt_admin_wuenic)

# admin vs WHO/UNICEF (second pct change threshold)
func_slide_v(dml_plt_admin_wuenic_2)
txt_admin_wuenic2 <- paste0("Comparison of administrative coverage and WHO/UNICEF estimates by vaccine. ",
                            "Red lines indicate gaps exceeding ±", second_pct_threshold * 100, " percentage points.")
func_slide_v_txt(txt_admin_wuenic2)

# admin vs official
func_slide_v(dml_plt_admin_vs_official)
txt_admin_official <- paste0("Comparison of administrative coverage and official government estimates. ",
                             "Red lines indicate gaps exceeding ±", pct_threshold * 100, " percentage points.")
func_slide_v_txt(txt_admin_official)

# ── SECTION DIVIDER: ADMIN DATA FLAGS ────────────────────────────────────────
func_slide_bb("Admin Coverage Flags")

# ── THRESHOLD EXPLANATION ─────────────────────────────────────────────────────
func_slide_bb("this is why we use the 10% threshold..")

# all vaccines — flag chart
func_slide_v(dml_plt_coverage_flags)
txt_cov_flags <- paste0(
  "Admin coverage flagged where values exceed 100% (orange) or change by more than ±",
  pct_threshold * 100, " percentage points from the previous year (red), ", min_yr_plots, "–", rev_yr, ".")
func_slide_v_txt(txt_cov_flags)

# DTP1, DTP3, MCV1 — flag chart
func_slide_v(dml_plt_selected_coverage_flags)
txt_three_flags <- paste0("Same flag logic applied to DTP1, DTP3, and MCV1 only, ", min_yr_plots, "–", rev_yr, ".")
func_slide_v_txt(txt_three_flags)

# ── SECTION DIVIDER: NUMERATOR CHECKS ────────────────────────────────────────
func_slide_bb("Numerator Checks")

# stockout table
if (nrow(tbl_stock_r) > 0 && ncol(tbl_stock_r) > 0) {
  doc <- add_slide(doc, layout = "data_no_logos", master = "Office Theme")
  doc <- ph_with(doc,
                 value = fpar(ftext("Vaccine Stockouts",
                                    prop = fp_text(font.size = 28, color = "black", font.family = "Calibri"))),
                 location = ph_location(left = 0.6, top = 0.6, width = 8, height = 1))
  doc <- ph_with(x = doc, value = tbl_stock,
                 location = ph_location(left = 0.6, top = 1.5, width = 10, height = 5))
  txt_stockout_table <- "Years in which a national or subnational vaccine stockout was recorded.\n\n This table provides context for interpreting the numerator and denominator trends and flags in the following slides, as stockouts may contribute to drops in coverage or data quality issues."
  func_slide_v_txt(txt_stockout_table)
}

# all numerators line plot
func_slide_v(dml_plt_all_numerators)
txt_all_numerators <- "Number of children vaccinated by vaccine and year."
func_slide_v_txt(txt_all_numerators)

# year-to-year % change in vaccinated children (all vaccines)
func_slide_v(dml_plt_perc_change_line)
txt_numerator_line <- paste0("Percent change from previous year in the number of children vaccinated (numerator) by vaccine. ",
                             "Red and orange points flag changes exceeding ± 10pp and ± 5pp, respectively. Blue line shows raw counts on the secondary axis. ",
                             "Orange shading indicates a vaccine stockout in that year. \n\n", outlier_subtitle)
func_slide_v_txt(txt_numerator_line)

for (cluster_name in names(plt_numerator_list)) {
  
  dml_plt <- rvg::dml(ggobj = plt_numerator_list[[cluster_name]], editable = TRUE)
  func_slide_v(dml_plt)
  txt_admin_cov_visit <- paste0("Admin coverage of vaccines co-administered at the ", cluster_name, " visit ",
                                "per the national schedule. ", coadmin_labels[cluster_name])
  func_slide_v_txt(txt_admin_cov_visit)
}


# ── SECTION DIVIDER: DENOMINATOR CHECKS ──────────────────────────────────────
func_slide_bb("Denominator Checks")

# year-to-year % change in target population (all vaccines)
func_slide_v(dml_plt_denom_change)
txt_denom_line <- paste0("Percent change from previous year in the number of children in the target population (denominator) by vaccine. Red and orange points flag changes exceeding ± 10pp and ± 5pp, respectively. Blue line shows raw counts on the secondary axis. Blue line shows raw counts on the secondary axis.")
func_slide_v_txt(txt_denom_line)

# live births (BCG) vs surviving infants (DTP1)
func_slide_v(dml_plt_births_vs_si)
txt_births_si <- paste0("Comparison of live births (BCG denominator) and surviving infants (DTP1 denominator) ",
                        "over time, ", min_yr_plots, "–", rev_yr)
func_slide_v_txt(txt_births_si)

# % change in BCG and DTP1 denominators
func_slide_v(dml_plt_denom_pct_change)
txt_perc_change_denom <- paste0("Percent change from previous year in BCG (live births) and DTP1 (surviving infants) denominators. ",
                                "Red and orange points flag changes exceeding ± 10pp and ± 5pp, respectively.")
func_slide_v_txt(txt_perc_change_denom)

# ── SECTION DIVIDER: DROPOUT & CO-ADMINISTRATION ────────────────────────────
func_slide_bb("Dropout & Co-administration")

# dropout rates
func_slide_vnologo(dml_plt_dropout_with_rate)
txt_dropout <- paste0("Admin coverage for key vaccine pairs (DTP1→DTP3, DTP1→MCV1) with dropout rate shown as bars. ",
                      "High dropout may indicate supply, access, or demand barriers.")
func_slide_v_txt(txt_dropout)

# DTP3–PCVC co-administration
func_slide_v(dml_plt_coadmin_dtp_pcv)
txt_coadmin <- paste0("Admin coverage for DTP3 and PCVC, which are co-administered at ", dtp3_pcvc_time, " in ", .current_country,
                      ".\n\nDivergence between the two series may indicate a reporting inconsistency.")
func_slide_v_txt(txt_coadmin)

# Co-administration by schedule time point (dynamic checks for whichever vaccines are co-administered in this country)
func_slide_bb("Co-administration by Schedule Time Point")

for (cluster_name in names(plt_coadmin_list)) {
  
  dml_plt <- rvg::dml(ggobj = plt_coadmin_list[[cluster_name]], editable = TRUE)
  func_slide_v(dml_plt)
  txt_coadmin_schedule <- paste0("Admin coverage of vaccines co-administered at the ", cluster_name, " visit ",
                                 "per the national schedule. ", coadmin_labels[cluster_name])
  func_slide_v_txt(txt_coadmin_schedule)
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

# vaccine introductions table
if (!no_data(tbl_intro_r) && nrow(tbl_intro_r) > 0 && ncol(tbl_intro_r) > 0) {
  doc <- add_slide(doc, layout = "data_1", master = "Office Theme")
  doc <- ph_with(doc,
                 value = fpar(ftext("Vaccine Introductions",
                                    prop = fp_text(font.size = 28, color = "black", font.family = "Calibri"))),
                 location = ph_location(left = 0.6, top = 0.6, width = 8, height = 1))
  doc <- ph_with(x = doc, value = tbl_intro,
                 location = ph_location(left = 0.6, top = 1.5, width = 10, height = 5))
  txt_intro_table <- "Year in which each vaccine was introduced nationally, partially, for risk groups, or risk areas.\n\n We present the vaccination introductions table here to contextualize the data availability heatmap on the following slide."
  func_slide_v_txt(txt_intro_table)
}

# missing data heatmap
func_slide_v(dml_plt_missing_heatmap)
txt_missing_heatmap <- paste0("Heatmap showing whether admin data are present (green), missing (red), or not applicable ",
                              "because the vaccine had not yet been introduced (white), by vaccine and year.") 
func_slide_v_txt(txt_missing_heatmap)

# code to pull & organize comments tables is in comments_script.R


# ── SECTION DIVIDER: ADMIN DATA COMMENTS ─────────────────────────────────────
func_slide_bb("Admin Data Comments")

# Step 1: Prep data and define all txt_ variables (language-agnostic/English base)
source(file.path(dqfolder, "comments_data.R"), local = FALSE)

# Step 2: Collect ALL txt_ vars AFTER they've been defined by the source file
text_vars_en <- mget(ls(pattern = "^txt_"), envir = .GlobalEnv)

# ── SAVE DOC CHECKPOINT (before language-specific comments) ──────────────────
doc_before_comments <- doc

# ── LANGUAGE LOOP ─────────────────────────────────────────────────────────────
for (language in languages) {
  
  # 1. Restore doc to pre-comments state
  doc <- doc_before_comments
  
  # 2. Source the DeepL environment structures
  source(str_glue("{utils}/R/narrative-translation_deepl.R"))
  
  # 3. Fetch translations (Returns text_vars_en directly if language == "en")
  text_vars <- get_text_vars(lang = language)
  
  # 4. Step 3: Build comments slides using text_vars for this language
  source(file.path(dqfolder, "comments_slides.R"), local = FALSE)
  
  # ── APPENDIX ────────────────────────────────────────────────────────────────
  
  # Safe Check: Extract translated Appendix Title without crashing if missing
  if ("txt_appendix_title" %in% names(text_vars)) {
    translated_title <- text_vars[["txt_appendix_title"]]
  } else {
    translated_title <- "Supplementary Information" # Fallback default
  }
  func_slide_bb(translated_title)
  
  # Render heatmap visualization
  min_yr <- min(df$year)
  max_yr <- max(df$year)
  func_slide_v(dml_plt_all_vax_heatmap)
  
  # Safe Check: Dynamic string mapping for Heatmap Footnote Text
  if ("txt_wuenic_heatmap" %in% names(text_vars)) {
    heatmap_template <- text_vars[["txt_wuenic_heatmap"]]
  } else {
    heatmap_template <- "WHO/UNICEF coverage estimates across all vaccines, {min_yr}–{max_yr}. Blank cells indicate years prior to vaccine introduction."
  }
  
  # str_glue formats the years safely into whatever language DeepL returned
  txt_wuenic_heatmap <- str_glue(heatmap_template)
  func_slide_v_txt(txt_wuenic_heatmap)
  
  # ── SAVE ────────────────────────────────────────────────────────────────────
  folder_path <- file.path(dq_folder, "DQProduct/outputs/all_countries")
  dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
  
  suffix <- str_glue("_{language}")
  print(doc, target = file.path(folder_path, paste0(.current_country, "_DQ", suffix, ".pptx")))
  cat("  💾 Saved:", language, "version\n")
}

# # ── LANGUAGE LOOP ─────────────────────────────────────────────────────────────
# for (language in languages) {
#   
#   # restore doc to pre-comments state
#   doc <- doc_before_comments
#   
#   source(str_glue("{utils}/R/narrative-translation_deepl.R"))
#   source(file.path(dqfolder, "comments_data.R"), local = FALSE)
#   
#   if (language == "en") {
#     text_vars <- text_vars_en
#   }
#   
#   text_vars <- get_text_vars(lang = language)
#   
#   # Step 3: build comments slides using text_vars for this language
#   source(file.path(dqfolder, "comments_slides.R"), local = FALSE)
#   
#   # ── APPENDIX ────────────────────────────────────────────────────────────────
#   translated_title <- get_text2("txt_appendix_title", text_vars)
#   func_slide_bb(translated_title)
#   
#   min_yr <- min(df$year)
#   max_yr <- max(df$year)
#   func_slide_v(dml_plt_all_vax_heatmap)
#   txt_wuenic_heatmap <- paste0("WHO/UNICEF coverage estimates across all vaccines, ", min_yr, "–", max_yr, ".",
#                                " Blank cells indicate years prior to vaccine introduction.")
#   func_slide_v_txt(txt_wuenic_heatmap)
#   
#   # ── SAVE ────────────────────────────────────────────────────────────────────
#   folder_path <- file.path(dq_folder, "DQProduct/outputs/all_countries")
#   dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
#   
#   suffix <- str_glue("_{language}")
#   print(doc, target = file.path(folder_path, paste0(.current_country, "_DQ", suffix, ".pptx")))
#   cat("  💾 Saved:", language, "version\n")
# }
