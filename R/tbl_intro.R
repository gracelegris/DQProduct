###### tbl_intro :: wiise intro year data ######
## prep data ----
# initial prep in main script
tbl_intro_r <- tbl_intro_r %>%
  mutate(`National introduction` = case_when(is.na(`National introduction`) ~ "Not introduced",
                                             TRUE ~ `National introduction`))
  # mutate(across(where(is.character), ~ ifelse(is.na(.), "Not introduced", .)))


### TRANSLATION CODE
# Build a translation map for vaccine names found under a specific type or key prefix
vaccine_translation_map <- translation_table %>%
  filter(type == "vaccine_names") %>% # or whichever type you use to group vaccine strings
  select(en, all_of(language)) %>%
  filter(!is.na(.[[language]]), .[[language]] != "") %>%
  { setNames(.[[language]], .[["en"]]) }

# Get the translated string for "Not introduced"
not_introduced_trans <- get_text2("txt_not_introduced", text_vars)


## 2. Prep and Translate Content ───────────────────────────────────────────────

# Replace NAs with translated string or placeholder
tbl_intro_r <- tbl_intro_r %>%
  mutate(`National introduction` = case_when(
    is.na(`National introduction`) ~ "Not introduced",
    TRUE ~ as.character(`National introduction`)
  ))

# Translate the data values inside the table columns *before* renaming columns
tbl_intro_r <- tbl_intro_r %>%
  mutate(
    # Translate Vaccine strings if they exist in your mapping dictionary
    Vaccine = ifelse(Vaccine %in% names(vaccine_translation_map), 
                     vaccine_translation_map[Vaccine], 
                     Vaccine),
    
    # Translate the "Not introduced" value in the National introduction column
    `National introduction` = ifelse(`National introduction` == "Not introduced", 
                                     not_introduced_trans, 
                                     `National introduction`)
  )


## 3. Translate Column Headers ──────────────────────────────────────────────────

# Dynamic unquoting via bang-bang (!!) and walrus operator (:=)
tbl_intro_r_display <- tbl_intro_r %>% 
  rename(
    !!t_lookup("tbl_schedule_vaccine", language) := "Vaccine",
    !!get_text2("txt_national_intro", text_vars) := "National introduction"
  )


## 4. Convert to Flextable ─────────────────────────────────────────────────────

tbl <- flextable(tbl_intro_r_display) %>%
  set_table_properties(layout = "fixed") 

# Specify column widths
n_cols <- ncol(tbl_intro_r_display)
col_widths <- c(3, rep(1.5, n_cols - 1))

# Apply widths per column
for (i in seq_len(n_cols)) {
  tbl <- width(tbl, j = i, width = col_widths[i])
}

# Align text: first column left, all others center
tbl <- align(tbl, j = 1, align = "left") %>%
  align(j = 2:n_cols, align = "center")

tbl <- tbl %>%
  # Align first column header left
  align(j = 1, part = "header", align = "left") %>%
  # Align other column headers center
  align(j = 2:n_cols, part = "header", align = "center") %>%
  # make column headers bold
  bold(part = "header", bold = TRUE)

# Define border colour
border_clr <- fp_border(color = "darkgrey", width = 1)

tbl <- tbl %>%
  hline(i = 1:(nrow(tbl_intro_r_display)-1), border = border_clr, part = "body")  # add line between every pair of rows

tbl_intro <- tbl


## OLD CODE BEFORE ADDING TRANSLATION
# ## convert to flextable ----
# tbl <- flextable(tbl_intro_r) %>%
#   set_table_properties(layout = "fixed") 
# 
# # specify column widths
# n_cols <- ncol(tbl_intro_r)
# col_widths <- c(3, rep(1.5, n_cols - 1))
# 
# # apply widths per column
# for (i in seq_len(n_cols)) {
#   tbl <- width(tbl, j = i, width = col_widths[i])
# }
# 
# # align text: first column left, all others center
# tbl <- align(tbl, j = 1, align = "left") %>%
#   align(j = 2:n_cols, align = "center")
# 
# tbl <- tbl %>%
#   # Align first column header left
#   align(j = 1, part = "header", align = "left") %>%
#   # Align other column headers center
#   align(j = 2:n_cols, part = "header", align = "center") %>%
#   # make column headers bold
#   bold(part = "header", bold = TRUE)
# 
# # Define border colour
# border_clr <- fp_border(color = "darkgrey", width = 1)
# 
# tbl <- tbl %>%
#   hline(i = 1:(nrow(tbl_intro_r)-1), border = border_clr, part = "body")  # add line between every pair of rows
# 
# 
# tbl_intro <- tbl