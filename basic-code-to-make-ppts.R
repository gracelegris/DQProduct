
source("user_profiles.R")
source("figs_tables_ppt.R")
source(file.path(DummyUtils, "R/slide_production_funcs.R"))

library(officer)

## functions ----
# function for slides
func_slide <- function(dml) {
  doc <- doc %>% add_slide(layout = "blank", master = "Office Theme")
  doc <- ph_with(x = doc, value = dml, location = ph_location("body", left = 0.3, top = 0.4, width = 12, height = 7))
}

# function for text to paste on slide
func_slide_txt <- function(txt) {
  doc <- ph_with(x = doc, block_list(fpar(ftext(txt, prop = fp_text(font.size = 12.3, color = "white")), fp_p = fp_par(text.align = "left"))),
                 location = ph_location("body", left = 10.25, top = 0.3, width = 2.9, height = 7, bg='#203864'))
  assign("doc", doc, envir = .GlobalEnv)
}
## functions ----

## convert charts to dml objects ----
ggplot_objects = list("plt_all_vax_line" = plt_all_vax_line,
                      "plt_selected_vax_line" = plt_selected_vax_line,
                      "plt_perc_change_line" = plt_perc_change_line,
                      "plt_selected_coverage_flags" = plt_selected_coverage_flags,
                      "plt_all_vax_heatmap" = plt_all_vax_heatmap,
                      "plt_summary_table" = plt_summary_table,
                      "plt_coverage_flags" = plt_coverage_flags,
                      "plt_admin_vs_official" = plt_admin_vs_official,
                      "plt_admin_vs_wuenic" = plt_admin_vs_wuenic,
                      "plt_denom_change" = plt_denom_change,
                      "plt_births_vs_si" = plt_births_vs_si,
                      "plt_denom_pct_change" = plt_denom_pct_change,
                      "plt_dropout_with_rate" = plt_dropout_with_rate,
                      "plt_coadmin_dtp_pcv" = plt_coadmin_dtp_pcv,
                      "plt_6wk" = plt_6wk,
                      "plt_14wk" = plt_14wk,
                      "plt_missing_heatmap" = plt_missing_heatmap)

# loop over plots to convert to editable dml objects
for (name in names(ggplot_objects)) {
  
  original_name <- ggplot_objects[[name]]
  new_name <- paste("dml_", name, sep = "")
  
  # Convert to dml object using rvg::dml
  obj <- rvg::dml(ggobj = original_name, editable = TRUE)
  
  # Assign the dml object to a variable with the new name
  assign(new_name, obj)
}
## convert charts to dml objects ----

## ppt ----


## ======================================================================================================================
### Create Slides
## ======================================================================================================================

# blank ppt template
doc <- read_pptx(file.path(UtilsDir, "blank-slide-master.pptx"))

# title slide with country name
doc <- ph_with(doc, value = fpar(ftext(CountryName, prop = fp_text(font.size = 36, bold = TRUE, font.family = "Calibri", color = "black"))),
               location = ph_location(left = 1, top = 3, width = 8, height = 1.5))

# slides: vaccine schedule & stockouts
func_slide(tbl_schedule) # vaccine schedule
func_slide(tbl_intro) # vaccine introduction years
func_slide(tbl_stock) # stockouts

# section: coverage & outlier detection
func_slide(dml_plt_all_vax_heatmap)
func_slide(dml_plt_summary_table)
func_slide(dml_plt_all_vax_line)
func_slide(dml_plt_selected_vax_line)
func_slide(dml_plt_coverage_flags)
func_slide(dml_plt_selected_coverage_flags)
func_slide(dml_plt_admin_vs_official)
func_slide(dml_plt_admin_vs_wuenic)

# section: numerators & denominators
func_slide(dml_plt_perc_change_line)
func_slide(dml_plt_denom_change)
func_slide(dml_plt_births_vs_si)
func_slide(dml_plt_denom_pct_change)

# section: dropout & vaccine relationships
func_slide(dml_plt_dropout_with_rate)
func_slide(dml_plt_coadmin_dtp_pcv)
func_slide(dml_plt_6wk)
func_slide(dml_plt_14wk)

# section: missingness
func_slide(dml_plt_missing_heatmap)

# save ppt
print(doc, file.path(OutputDir, paste0(tolower(Current_ISO3), "_", rev_yr, "_test.pptx")))
