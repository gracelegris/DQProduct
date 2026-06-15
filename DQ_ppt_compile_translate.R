# ==========================================================================================================================================
# Script Name: Produce Formatted Data Quality PPT
# Author: Grace Legris, glegris@unicef.org
# ==========================================================================================================================================

options(scipen = 999)

# ── TRANSLATION TABLE ────────────────────────────────────────────────────────
translation_table <- read.csv(file.path(RevDir, "unicef-products/dummy/country-specific-charts/translation-table_charts_ctry.csv"))

# ── DATA LOADING ─────────────────────────────────────────────────────────────
wuenic_master <- read.csv(file.path(DummyDataDir, "wuenic-master_2025rev.csv")) %>%  
  mutate(Vaccine = case_when(
    Vaccine == "hepbb" ~ "HepBB", Vaccine == "hepb3" ~ "HepB3",
    Vaccine == "hib3"  ~ "Hib3",  Vaccine == "rotac" ~ "RotaC",
    Vaccine == "menga" ~ "MengA", Vaccine == "hpvc"  ~ "HPVc", TRUE ~ toupper(Vaccine))) %>%
  rename(Admin = AdministrativeCoverage, Official = GovernmentEstimate, Survey = SurveyInformation) %>%
  relocate(Admin, .after = WUENIC) %>% relocate(Official, .after = Admin) %>%
  relocate(Survey, .after = Official) %>% relocate(ChildrenVaccinated, .after = Survey) %>%
  relocate(ChildrenInTarget, .after = ChildrenVaccinated) %>% 
  filter(Vaccine != "POL3") # remove polio3 as we are not presenting it anymore

wuenic_master$Vaccine <- factor(wuenic_master$Vaccine, levels = c("BCG","HepBB","DTP1","DTP3","Hib3","HepB3","PCVC","RotaC","IPV1","IPVC","MCV1","RCV1","MCV2","YFV","MengA","HPVc"))
wuenic_master_current <- wuenic_master %>% filter(ISOCountryCode == .current_iso3c, Year >= min_yr_plots, Year <= rev_yr)
regional_info <- regional_info %>% filter(iso3c == toupper(.current_iso3c))

wuenic_stockouts_master <- wuenic_stockouts_clean %>%
  mutate(iso3c = toupper(iso3c), vaccine = case_when(vaccine == "hepbb" ~ "HepBB", vaccine == "hepb3" ~ "HepB3", vaccine == "hib3"  ~ "Hib3",  vaccine == "rotac" ~ "RotaC", vaccine == "menga" ~ "MengA", vaccine == "hpvc"  ~ "HPVc", TRUE ~ toupper(vaccine)))

wuenic_master_current <- wuenic_master_current %>% mutate(iso3c_upper = toupper(ISOCountryCode)) %>% 
  left_join(wuenic_stockouts_master %>% select(iso3c, year, vaccine, any_stockout), by = c("iso3c_upper" = "iso3c", "Year" = "year", "Vaccine" = "vaccine")) %>%
  mutate(any_stockout = if_else(is.na(any_stockout), 0, any_stockout))

tbl_intro_r <- wiise_intro %>% mutate(iso3c = tolower(iso3c)) %>% filter(iso3c == .current_iso3c) %>% select(vaccine_name, contains("year_intro")) %>% arrange(vaccine_name) %>% 
  rename(Vaccine = vaccine_name, `National introduction` = year_intro_national, `Partial introduction` = year_intro_partial, `Risk groups` = year_intro_risk_groups, `Risk areas` = year_intro_risk_area) %>%
  select(where(~ !all(is.na(.) | . == ""))) %>% mutate(across(where(is.numeric), ~ as.character(.)))

no_data <- function(df) nrow(df) == 0

rect <- rectGrob(gp = gpar(fill = "black", col = NA))

# ── GLOBAL MULTI-LANGUAGE LOOP ────────────────────────────────────────────────
for (language in languages) {
  
  # load the template translation text mappings
  source(file.path(dqfolder, "comments_data.R"), local = FALSE)
  text_vars_en <- mget(ls(pattern = "^txt_"), envir = .GlobalEnv)
  
  # source deepl structures and dynamically translate current loop language
  source(str_glue("{utils}/R/narrative-translation_deepl.R"))
  text_vars <- get_text_vars(lang = language)
  
  # generate tables and plots
  source(file.path(PrjDir, "R/tbl_schedule.R"))
  if (!no_data(tbl_intro_r)) source(file.path(PrjDir, "R/tbl_intro.R"))
  source(file.path(PrjDir, "R/tbl_stock.R"))
  cat("Building plots in language:", language, "\n")
  source(file.path(PrjDir, "figs_tables_ppt.R"), echo = FALSE, print.eval = FALSE)
  
  # convert plots to editable dml structures
  plot_names <- ls(pattern = "^plt_")
  ggplot_objects <- mget(plot_names, envir = .GlobalEnv)
  for (name in names(ggplot_objects)) {
    assign(paste0("dml_", name), rvg::dml(ggobj = ggplot_objects[[name]], editable = TRUE), envir = .GlobalEnv)
  }
  
  # build presentation document layer
  doc <- read_pptx(str_glue("{utils}/intervention_analysis_template.pptx"))
  
  # cover slide layout execution
  doc <- add_slide(doc, layout = "cover_country", master = "Office Theme")
  doc <- ph_with(x = doc, block_list(fpar(ftext(.current_country, prop = fp_text(font.size = 40, color = "white")), fp_p = fp_par(text.align = "center"))), location = ph_location("body", left = 1.9, top = 2.86, width = 9.5, height = 2))
  
  # dynamic title generation
  report_title_tpl <- get_text2("txt_cover_subtitle", text_vars)
  doc <- ph_with(x = doc, block_list(fpar(ftext(str_glue(report_title_tpl), prop = fp_text(font.size = 22, color = "white")), fp_p = fp_par(text.align = "center"))), location = ph_location("body", left = 3.34, top = 3.92, width = 7, height = 1.22))
  doc <- remove_slide(doc, index = 1) # drop first default template slide
  
  # standard static master text slide inserts
  doc <- add_slide(doc, layout = "report_introduction", master = "Office Theme")
  doc <- add_slide(doc, layout = "1_intro_slide", master = "Office Theme")
  
  # definitions slide by language
  if(language == "en") {
    doc <- add_slide(doc, layout = "definitions", master = "Office Theme")
  } else {
    doc <- add_slide(doc, layout = paste0("definitions_", language), master = "Office Theme")
  }
  
  # data source definitions
  doc <- add_slide(doc, layout = "data_source_descriptions", master = "Office Theme")
  
  # vaccine immunization schedule table generation
  schedule_year <- wiise_schedule %>% filter(iso3c == x) %>% pull(year) %>% max(na.rm = TRUE)
  if (nrow(tbl_schedule_r) > 0 && ncol(tbl_schedule_r) > 0) {
    doc <- add_slide(doc, layout = "data_1", master = "Office Theme")
    sched_title_tpl <- get_text2("txt_sched_title", text_vars)
    doc <- ph_with(doc, value = fpar(ftext(str_glue(sched_title_tpl), prop = fp_text(font.size = 28, color = "black", font.family = "Calibri"))), location = ph_location(left = 0.6, top = 0.6, width = 8, height = 1))
    doc <- ph_with(x = doc, value = tbl_schedule, location = ph_location(left = 0.6, top = 1.5, width = total_width_sched, height = 5))
    sched_foot_tpl <- t_lookup("tbl_schedule_narrative", language)
    func_slide_v_txt(str_glue(sched_foot_tpl))
  }
  
  if (!no_data(tbl_intro_r) && nrow(tbl_intro_r) > 0 && ncol(tbl_intro_r) > 0) {
    doc <- add_slide(doc, layout = "data_1", master = "Office Theme")
    intro_title <- get_text2("txt_intro_table_title", text_vars)
    doc <- ph_with(doc, value = fpar(ftext(intro_title, prop = fp_text(font.size = 28, color = "black", font.family = "Calibri"))), location = ph_location(left = 0.6, top = 0.6, width = 8, height = 1))
    doc <- ph_with(x = doc, value = tbl_intro, location = ph_location(left = 0.6, top = 1.5, width = 10, height = 5))
    func_slide_v_txt(get_text2("txt_intro_table_foot", text_vars))
  }
  
  func_slide_v(dml_plt_missing_heatmap)
  func_slide_v_txt(get_text2("txt_missing_heatmap", text_vars))
  
  # section: administrative data flags
  func_slide_bb(get_text2("txt_sec_admin_flags", text_vars))
  #func_slide_bb(get_text2("txt_sec_threshold_expl", text_vars))
  
  func_slide_v(dml_plt_coverage_flags)
  cov_flags_tpl <- get_text2("txt_cov_flags", text_vars)
  func_slide_v_txt(str_glue(cov_flags_tpl))
  
  func_slide_v(dml_plt_selected_coverage_flags)
  three_flags_tpl <- get_text2("txt_three_flags", text_vars)
  func_slide_v_txt(str_glue(three_flags_tpl))
  
  # section: numerator validation analysis
  func_slide_bb(get_text2("txt_sec_numerator_checks", text_vars))
  
  if (nrow(tbl_stock_r) > 0 && ncol(tbl_stock_r) > 0) {
    doc <- add_slide(doc, layout = "data_no_logos", master = "Office Theme")
    stock_title <- get_text2("txt_stockout_title", text_vars)
    doc <- ph_with(doc, value = fpar(ftext(stock_title, prop = fp_text(font.size = 28, color = "black", font.family = "Calibri"))), location = ph_location(left = 0.6, top = 0.6, width = 8, height = 1))
    doc <- ph_with(x = doc, value = tbl_stock, location = ph_location(left = 0.6, top = 1.5, width = 10, height = 5))
    stock_foot <- get_text2("txt_stockout_table", text_vars)
    func_slide_v_txt(stock_foot)
  }
  
  # func_slide_v(dml_plt_all_numerators)
  # func_slide_v_txt(get_text2("txt_all_numerators", text_vars))
  
  func_slide_v(dml_plt_numerators_bar_all)
  func_slide_v_txt(get_text2("txt_numerator_bars", text_vars))
  
  func_slide_v(dml_plt_numerators_bar_dtpmcv)
  func_slide_v_txt(get_text2("txt_numerator_bars_selection", text_vars))
  
  # func_slide_v(dml_plt_perc_change_line)
  # num_line_tpl <- get_text2("txt_numerator_line", text_vars)
  # func_slide_v_txt(str_glue(num_line_tpl))
  
  func_slide_v(dml_plt_perc_change_line)
  func_slide_v_txt(get_text2("txt_num_change_explanation", text_vars))
  
  # for (cluster_name in names(plt_numerator_list)) {
  #   dml_plt <- rvg::dml(ggobj = plt_numerator_list[[cluster_name]], editable = TRUE)
  #   func_slide_v(dml_plt)
  #   admin_visit_tpl <- get_text2("txt_admin_cov_visit", text_vars)
  #   func_slide_v_txt(str_glue(admin_visit_tpl))
  # }
  
  # section: denominator verification analysis
  func_slide_bb(get_text2("txt_sec_denominator_checks", text_vars))
  
  func_slide_v(dml_plt_denom_change)
  func_slide_v_txt(get_text2("txt_denom_line", text_vars))
  
  func_slide_v(dml_plt_births_vs_si)
  birth_si_tpl_raw <- get_text2("txt_births_si", text_vars)
  birth_si_tpl <- stringr::str_glue(birth_si_tpl_raw)
  unpd_message <- t_lookup("unpd_description", language)
  func_slide_v_txt(paste0(birth_si_tpl, "\n\n", unpd_message))
  
  func_slide_v(dml_plt_denom_pct_change)
  func_slide_v_txt(get_text2("txt_denom_pct_change", text_vars))
  
  # section: drop-out metrics & co-administration pairings
  func_slide_bb(get_text2("txt_sec_dropout_coadmin", text_vars))
  
  func_slide_vnologo(dml_plt_dropout_with_rate)
  func_slide_v_txt(get_text2("txt_dropout", text_vars))
  
  func_slide_v(dml_plt_coadmin_dtp_pcv)
  coadmin_tpl <- get_text2("txt_coadmin", text_vars)
  func_slide_v_txt(str_glue(coadmin_tpl))
  
  func_slide_bb(get_text2("txt_sec_coadmin_schedule", text_vars))
  for (cluster_name in names(plt_coadmin_list)) {
    dml_plt <- rvg::dml(ggobj = plt_coadmin_list[[cluster_name]], editable = TRUE)
    func_slide_v(dml_plt)
    coadmin_sched_tpl <- get_text2("txt_coadmin_schedule", text_vars)
    func_slide_v_txt(str_glue(coadmin_sched_tpl))
  }
  
  # section: admin data availability heatmap
  #func_slide_bb(get_text2("txt_sec_data_availability", text_vars))
  
  # section: coverage trends by data source
  func_slide_bb(get_text2("txt_sec_coverage_comparison", text_vars))
  
  func_slide_v(dml_plt_summary_table)
  sum_tbl_tpl <- get_text2("txt_summary_table", text_vars)
  func_slide_v_txt(str_glue(sum_tbl_tpl))
  
  func_slide_v(dml_plt_diff_table)
  diff_tbl_tpl <- get_text2("txt_diff_table", text_vars)
  func_slide_v_txt(str_glue(diff_tbl_tpl))
  
  # func_slide_v(dml_plt_all_vax_line)
  # vax_line_tpl <- get_text2("txt_vax_line", text_vars)
  # func_slide_v_txt(str_glue(vax_line_tpl))
  
  func_slide_v(dml_plt_selected_vax_line)
  three_line_tpl <- get_text2("txt_three_lines", text_vars)
  func_slide_v_txt(str_glue(three_line_tpl))
  
  # func_slide_v(dml_plt_admin_wuenic_1)
  # adm_wuenic_tpl1 <- get_text2("txt_admin_wuenic", text_vars)
  # func_slide_v_txt(str_glue(adm_wuenic_tpl1))
  # 
  # func_slide_v(dml_plt_admin_wuenic_2)
  # adm_wuenic_tpl2 <- get_text2("txt_admin_wuenic2", text_vars)
  # func_slide_v_txt(str_glue(adm_wuenic_tpl2))
  # 
  # func_slide_v(dml_plt_admin_vs_official)
  # adm_off_tpl <- get_text2("txt_admin_official", text_vars)
  # func_slide_v_txt(str_glue(adm_off_tpl))
  
  # section: admin comments
  func_slide_bb(get_text2("txt_sec_admin_comments", text_vars))
  source(file.path(dqfolder, "comments_slides.R"), local = FALSE)
  
  # # appendix
  # func_slide_bb(get_text2("txt_appendix_title", text_vars))
  # min_yr <- min(df$year); max_yr <- max(df$year)
  # func_slide_v(dml_plt_all_vax_heatmap)
  # 
  # heatmap_tpl <- get_text2("txt_wuenic_heatmap", text_vars)
  # func_slide_v_txt(str_glue(heatmap_tpl))
  
  # save ppt
  folder_path <- file.path(dq_folder, "DQProduct/outputs/all_countries")
  dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
  suffix <- str_glue("_{language}")
  print(doc, target = file.path(folder_path, paste0(.current_country, "_DQ", suffix, ".pptx")))
  cat("  💾 Saved:", language, "version\n")
}
