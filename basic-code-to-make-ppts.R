library(officer)

## functions ----
# function for slides
func_slide <- function(dml) {
  doc <- doc %>% add_slide(layout = "blank", master = "Office Theme")   # replace "blank" with the name of slide in the slide master
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
ggplot_objects = list("gg_map_pol3" = gg_map_pol3,
                      "gg_map_ipv1" = gg_map_ipv1,
                      "gg_map_ipv2" = gg_map_ipv2,
                      "gg_stacked_bar" = gg_stacked_bar,
                      "gg_trends_pol3" = gg_trends_pol3,
                      "gg_trends_ipv1" = gg_trends_ipv1,
                      "gg_trends_line" = gg_trends_line,
                      "gg_bar_cases" = gg_bar_cases
)

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
# Load the existing PowerPoint document
doc <- read_pptx("xxx/blank-slide-master.pptx")

# use the function to paste dml plots to ppt and the corresponding narrative
func_slide(dml_gg_map_pol3)
func_slide_txt(gg_map_pol3_txt)
## ppt ----

# save ppt
print(doc, "xxx/xxxx.pptx")







