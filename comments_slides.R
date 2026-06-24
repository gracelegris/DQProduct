# ── COLLECT AND TRANSLATE ─────────────────────────────────────────────────────
text_vars_en <- mget(ls(pattern = "^txt_"))

# ── HELPER: BUILD A FLEXTABLE FOR ONE CMT_FIELDS CATEGORY ────────────────────
make_comments_tbl <- function(df, category, text_vars) {
  df_cat <- df %>% filter(CMT_FIELDS == category) %>% select(year, SOURCE_CMT)
  
  if (nrow(df_cat) == 0) return(NULL)
  
  colnames(df_cat) <- c(get_text2("txt_comments_col_year",    text_vars),
                        get_text2("txt_comments_col_comment", text_vars))
  
  df_cat[[1]] <- as.character(df_cat[[1]])
  
  ft <- flextable(df_cat) %>%
    width(j = 1, width = 0.8) %>%
    width(j = 2, width = 8.2) %>%
    hrule(rule = "auto", part = "body") %>%
    set_table_properties(layout = "fixed", opts_word = list(split = FALSE)) %>%
    fontsize(size = 13, part = "all") %>%
    font(fontname = "Calibri", part = "all") %>%
    bold(part = "header") %>%
    bg(bg = "#0058AB", part = "header") %>%
    color(color = "white", part = "header") %>%
    bg(i = seq(2, nrow(df_cat), by = 2), bg = "#EEF4FB", part = "body") %>%
    border_remove() %>%
    hline(part = "body", border = fp_border(color = "#D0D0D0", width = 0.5)) %>%
    align(j = 1, align = "center", part = "all") %>%
    align(j = 2, align = "left",   part = "body") %>%
    valign(valign = "top", part = "body")
  
  ft
}

render_comment_slides <- function(category_field, doc, comments_country, text_vars, last_5_yrs, rev_yr, most_recent_yr) {
  cat_meta <- list(
    "Numerator Accuracy"   = list(label_key = "txt_comments_label_num",     note_key = "txt_comments_note_num"),
    "Denominator Source"   = list(label_key = "txt_comments_label_den_src", note_key = "txt_comments_note_den_src"),
    "Denominator Accuracy" = list(label_key = "txt_comments_label_den_acc", note_key = "txt_comments_note_den_acc")
  )
  meta <- cat_meta[[category_field]]
  ft   <- make_comments_tbl(comments_country, category_field, text_vars)
  if (is.null(ft)) return(doc)
  doc <- add_slide(doc, layout = "data_1", master = "Office Theme")
  doc <- ph_with(
    x        = doc,
    value    = fpar(ftext(
      paste0(get_text2("txt_comments_title_prefix", text_vars), " ",
             get_text2(meta$label_key, text_vars),
             " (", min(last_5_yrs), "–", rev_yr, ")"),
      prop   = fp_text(font.size = 24, color = "black", font.family = "Calibri"))),
    location = ph_location(left = 0.6, top = 0.4, width = 12, height = 0.8))
  doc <- ph_with(
    x        = doc,
    value    = ft,
    location = ph_location(left = 0.6, top = 1.3, width = 8.2, height = 5.2))
  return(doc)
}

# function to make each table
render_comment_summary_slide <- function(doc, comments_country, text_vars, most_recent_yr) {
  comments_summary <- comments_country %>%
    filter(year == most_recent_yr) %>%
    select(CMT_FIELDS, SOURCE_CMT) %>%
    rename(!!get_text2("txt_comments_col_type",    text_vars) := CMT_FIELDS,
           !!get_text2("txt_comments_col_comment", text_vars) := SOURCE_CMT)
  
  if (nrow(comments_summary) == 0) return(doc)
  
  ft_summary <- flextable(comments_summary) %>%
    width(j = 1, width = 2.2) %>%
    width(j = 2, width = 7) %>%
    fontsize(size = 13, part = "all") %>%
    font(fontname = "Calibri", part = "all") %>%
    bold(part = "header") %>%
    bg(bg = "#0058AB", part = "header") %>%
    color(color = "white", part = "header") %>%
    bg(i = seq(2, nrow(comments_summary), by = 2), bg = "#EEF4FB", part = "body") %>%
    border_remove() %>%
    hline(part = "body", border = fp_border(color = "#D0D0D0", width = 0.5)) %>%
    align(j = 1, align = "left", part = "all") %>%
    align(j = 2, align = "left", part = "body") %>%
    valign(valign = "top", part = "body") %>%
    set_table_properties(layout = "fixed")
  
  doc <- add_slide(doc, layout = "data_1", master = "Office Theme")
  doc <- ph_with(
    x        = doc,
    value    = fpar(ftext(
      paste0(get_text2("txt_comments_title_summary", text_vars), " ", most_recent_yr),
      prop   = fp_text(font.size = 24, color = "black", font.family = "Calibri"))),
    location = ph_location(left = 0.6, top = 0.4, width = 12, height = 0.8))
  doc <- ph_with(
    x        = doc,
    value    = ft_summary,
    location = ph_location(left = 0.6, top = 1.3, width = 8, height = 4))

  return(doc)
}


# 
# # ── RENDER ONE SLIDE PER CATEGORY (only if data exists) ──────────────────────
# comment_categories <- list(
#   list(label_key = "txt_comments_label_num",
#        field     = "Numerator Accuracy",
#        note_key  = "txt_comments_note_num"),
#   list(label_key = "txt_comments_label_den_src",
#        field     = "Denominator Source",
#        note_key  = "txt_comments_note_den_src"),
#   list(label_key = "txt_comments_label_den_acc",
#        field     = "Denominator Accuracy",
#        note_key  = "txt_comments_note_den_acc")
# )
# 
# for (cat in comment_categories) {
#   ft <- make_comments_tbl(comments_country, cat$field, text_vars)
#   
#   if (is.null(ft)) next
#   
#   doc <- add_slide(doc, layout = "data_1", master = "Office Theme")
#   
#   doc <- ph_with(
#     x        = doc,
#     value    = fpar(ftext(
#       paste0(get_text2("txt_comments_title_prefix", text_vars), " ",
#              get_text2(cat$label_key, text_vars),
#              " (", min(last_5_yrs), "–", rev_yr, ")"),
#       prop   = fp_text(font.size = 24, color = "black", font.family = "Calibri"))),
#     location = ph_location(left = 0.6, top = 0.4, width = 12, height = 0.8))
#   
#   doc <- ph_with(
#     x        = doc,
#     value    = ft,
#     location = ph_location(left = 0.6, top = 1.3, width = 8.2, height = 5.2))
#   
#   func_slide_v_txt(paste0(
#     get_text2(cat$note_key, text_vars), " ",
#     get_text2("txt_comments_footnote_years", text_vars)))
# }
# 
# # ── COMBINED SUMMARY SLIDE ────────────────────────────────────────────────────
# comments_summary <- comments_country %>%
#   filter(year == most_recent_yr) %>%
#   select(CMT_FIELDS, SOURCE_CMT) %>%
#   rename(!!get_text2("txt_comments_col_type",    text_vars) := CMT_FIELDS,
#          !!get_text2("txt_comments_col_comment", text_vars) := SOURCE_CMT)
# 
# if (nrow(comments_summary) > 0) {
#   
#   ft_summary <- flextable(comments_summary) %>%
#     width(j = 1, width = 2.2) %>%
#     width(j = 2, width = 7) %>%
#     fontsize(size = 13, part = "all") %>%
#     font(fontname = "Calibri", part = "all") %>%
#     bold(part = "header") %>%
#     bg(bg = "#0058AB", part = "header") %>%
#     color(color = "white", part = "header") %>%
#     bg(i = seq(2, nrow(comments_summary), by = 2), bg = "#EEF4FB", part = "body") %>%
#     border_remove() %>%
#     hline(part = "body", border = fp_border(color = "#D0D0D0", width = 0.5)) %>%
#     align(j = 1, align = "left", part = "all") %>%
#     align(j = 2, align = "left", part = "body") %>%
#     valign(valign = "top", part = "body") %>%
#     set_table_properties(layout = "fixed")
#   
#   doc <- add_slide(doc, layout = "data_1", master = "Office Theme")
#   
#   doc <- ph_with(
#     x        = doc,
#     value    = fpar(ftext(
#       paste0(get_text2("txt_comments_title_summary", text_vars), " ", most_recent_yr),
#       prop   = fp_text(font.size = 24, color = "black", font.family = "Calibri"))),
#     location = ph_location(left = 0.6, top = 0.4, width = 12, height = 0.8))
#   
#   doc <- ph_with(
#     x        = doc,
#     value    = ft_summary,
#     location = ph_location(left = 0.6, top = 1.3, width = 8, height = 4))
#   
#   func_slide_v_txt(get_text2("txt_comments_note_summary", text_vars))
# }