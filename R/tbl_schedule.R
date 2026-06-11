###### tbl_schedule :: wiise vaccine schedule data ######
## prep data ----
# clean table
tbl_schedule_r <- wiise_schedule %>%
  mutate(geoarea = str_to_title(geoarea)) %>%
  
  # change A (annee [year in french]) to Y (year)
  mutate(ageadministered = gsub("A","Y",ageadministered)) %>%
  
  # extract operators at the begining (>, = , +) and remove from original column
  mutate(
    operator = str_extract(ageadministered, "^(>=|<=|>|<|\\+|=)"),
    after_operator = str_remove(ageadministered, "^(>=|<=|>|<|\\+|=)")
  ) %>%
  
  # add units to age administered
  mutate(
    # replace any "B" with "Birth"
    ageadmin = str_replace_all(after_operator, "B", "Birth"),
    
    # if there is a Y followed by a number, remove the Y and paste years at the end
    # same for months, weeks and days
    ageadmin = case_when(
      str_detect(after_operator, "^Y\\d+(\\.\\d+)?$") ~ str_glue("{str_remove(after_operator, 'Y')} years"),
      str_detect(after_operator, "^M\\d+(\\.\\d+)?$") ~ str_glue("{str_remove(after_operator, 'M')} months"),
      str_detect(after_operator, "^W\\d+(\\.\\d+)?$") ~ str_glue("{str_remove(after_operator, 'W')} weeks"),
      str_detect(after_operator, "^D\\d+(\\.\\d+)?$") ~ str_glue("{str_remove(after_operator, 'D')} days"),
      TRUE ~ ageadmin
    ),
    
    # strip extra Y/W from ageadmin (if any)
    ageadmin = str_replace_all(ageadmin, "Y", ""),
    ageadmin = str_replace_all(ageadmin, "W", "")
  ) %>%
  
  # fix examples where after_operator = Y1-Y2
  mutate(
    ageadmin = case_when(
      str_detect(after_operator, "^Y[0-9.]+-Y[0-9.]+$") ~ 
        str_replace(after_operator, "^Y([0-9.]+)-Y([0-9.]+)$", "\\1-\\2 years"),
      TRUE ~ ageadmin
    )
  ) %>%
  
  # fix examples where after_operator = M1-M2
  mutate(
    ageadmin = case_when(
      str_detect(after_operator, "^M[0-9.]+-M[0-9.]+$") ~ 
        str_replace(after_operator, "^M([0-9.]+)-M([0-9.]+)$", "\\1-\\2 months"),
      TRUE ~ ageadmin
    )
  ) %>%
  
  # fix examples where after_operator = W1-W2
  mutate(
    ageadmin = case_when(
      str_detect(after_operator, "^W[0-9.]+-W[0-9.]+$") ~ 
        str_replace(after_operator, "^W([0-9.]+)-W([0-9.]+)$", "\\1-\\2 weeks"),
      TRUE ~ ageadmin
    )
  ) %>%
  
  # fix examples where after_operator = D1-D2
  mutate(
    ageadmin = case_when(
      str_detect(after_operator, "^D[0-9.]+-D[0-9.]+$") ~ 
        str_replace(after_operator, "^D([0-9.]+)-D([0-9.]+)$", "\\1-\\2 days"),
      TRUE ~ ageadmin
    )
  ) %>%
  
  # dealing with ranges
  mutate(
    ageadmin = case_when(
      # Range: Birth to days (e.g. B-D5)
      str_detect(after_operator, "^B-D\\d+(\\.\\d+)?$") ~
        str_glue("Birth–{str_remove(after_operator, 'B-D')} days"),
      
      # Range: Birth to months (e.g. B-M1)
      str_detect(after_operator, "^B-M\\d+(\\.\\d+)?$") ~
        str_glue("Birth–{str_remove(after_operator, 'B-M')} months"),
      
      # Range: Birth to weeks (e.g. B-W6)
      str_detect(after_operator, "^B-W\\d+(\\.\\d+)?$") ~
        str_glue("Birth–{str_remove(after_operator, 'B-W')} weeks"),
      
      # Range: Birth to years (e.g. B-Y1.5)
      str_detect(after_operator, "^B-Y\\d+(\\.\\d+)?$") ~
        str_glue("Birth–{str_remove(after_operator, 'B-Y')} years"),
      
      # Single values: years, months, weeks, days (support decimals)
      str_detect(after_operator, "^Y\\d+(\\.\\d+)?$") ~ str_glue("{str_remove(after_operator, 'Y')} years"),
      str_detect(after_operator, "^M\\d+(\\.\\d+)?$") ~ str_glue("{str_remove(after_operator, 'M')} months"),
      str_detect(after_operator, "^W\\d+(\\.\\d+)?$") ~ str_glue("{str_remove(after_operator, 'W')} weeks"),
      str_detect(after_operator, "^D\\d+(\\.\\d+)?$") ~ str_glue("{str_remove(after_operator, 'D')} days"),
      
      # fallback
      TRUE ~ ageadmin
    )
  ) %>%
  
  # singularise when it is 1
  mutate(ageadmin = if_else(ageadmin %in% c("1 years", "<1 years", "1 days", "1 weeks"), str_replace(ageadmin, "s", ""), ageadmin),
  ) %>%
  
  mutate(ageadmin = case_when(!is.na(operator) ~ str_glue("{operator}{ageadmin}"),
                              TRUE ~ ageadmin),
         year = as.character(year)) %>%
  distinct()

# check
# tbl3 %>% select(ageadministered, operator, ageadmin) %>% unique() %>% arrange(ageadministered) %>% View()
# make a flag for if the country does not have hpv data so we can hide the HPV pages
no_data <- function(df) {
  return(nrow(df) == 0)
}

# final table
tbl_schedule_r <- tbl_schedule_r %>%
  filter(iso3c == x) %>%
  select(Level = geoarea,
         Vaccine = vaccine,
         `Dose number` = dosenumber,
         `Age administered` = ageadmin) %>%
  pivot_wider(., names_from = `Dose number`, values_from = `Age administered`)

## convert to flextable ----
if (no_data(tbl_schedule_r) == FALSE) {
  
  # ── ASSIGN COLORS TO UNIQUE AGE VALUES ──────────────────────────────────────
  dose_cols <- setdiff(names(tbl_schedule_r), c("Level", "Vaccine"))
  
  all_ages <- tbl_schedule_r %>%
    select(all_of(dose_cols)) %>%
    unlist() %>%
    na.omit() %>%
    unique() %>%
    sort()
  
  age_colors <- c("#0058AB","#1CABE2","#00833D","#80BD41","#6A1E74",
                  "#961A49","#E2231A","#F26A21","#FFC20E","#A8DADC")
  
  color_map <- setNames(rep_len(age_colors, length(all_ages)), all_ages)
  
  # ── RENAME LEVEL AND VACCINE COLUMNS USING LOOKUP ───────────────────────────
  
  # look up "national" or "subnational
  level_translation_map <- translation_table %>%
    filter(type == "tbl_schedule_cell") %>%
    select(en, all_of(language)) %>%
    filter(!is.na(.[[language]]), .[[language]] != "") %>%
    { setNames(.[[language]], .[["en"]]) }
  
  tbl_schedule_r_display <- tbl_schedule_r %>%
    rename(
      !!t_lookup("tbl_schedule_level",   language) := Level,
      !!t_lookup("tbl_schedule_vaccine", language) := Vaccine
    ) %>%
    mutate(across(
      all_of(t_lookup("tbl_schedule_level", language)),
      ~ ifelse(.x %in% names(level_translation_map), level_translation_map[.x], .x)
    ))
  
  display_cols <- setdiff(names(tbl_schedule_r_display),
                          c(t_lookup("tbl_schedule_level",   language),
                            t_lookup("tbl_schedule_vaccine", language)))
  
  # ── BUILD FLEXTABLE ──────────────────────────────────────────────────────────
  tbl3 <- flextable(tbl_schedule_r_display) %>%
    set_table_properties(layout = "fixed")
  
  n_cols    <- ncol(tbl_schedule_r_display)
  col_widths <- c(1, 1.9, rep(1, n_cols - 2))
  
  for (i in seq_len(n_cols)) {
    tbl3 <- width(tbl3, j = i, width = col_widths[i])
  }
  
  # ── APPLY COLORS CELL BY CELL ────────────────────────────────────────────────
  for (col in dose_cols) {
    col_idx <- which(names(tbl_schedule_r) == col)
    for (row_idx in seq_len(nrow(tbl_schedule_r))) {
      cell_val <- tbl_schedule_r[[col]][row_idx]
      if (!is.na(cell_val) && cell_val != "" && cell_val %in% names(color_map)) {
        tbl3 <- color(tbl3, i = row_idx, j = col_idx, color = color_map[[cell_val]], part = "body")
        tbl3 <- bold(tbl3,  i = row_idx, j = col_idx, part = "body")
      }
    }
  }
  
  # ── ADD HEADER ROW ───────────────────────────────────────────────────────────
  tbl <- add_header_row(
    tbl3,
    values    = c("", t_lookup("tbl_schedule_header", language)),
    colwidths = c(2, ncol(tbl_schedule_r_display) - 2)
  ) %>%
    bold(part = "header", bold = TRUE)
  
  total_width_sched <- sum(col_widths)
  tbl_schedule <- tbl
}

# get year schedule is from for this country
country_sched_year <- wiise_schedule %>%
  filter(iso3c == x) %>%
  pull(year) %>%
  unique() %>%
  sort(decreasing = TRUE) %>%
  first()





# OLD CODE TO MAKE TABLE

# ###### tbl_schedule :: wiise vaccine schedule data ######
# ## prep data ----
# # clean table
# tbl_schedule_r <- wiise_schedule %>%
#   mutate(geoarea = str_to_title(geoarea)) %>%
#   
#   # change A (annee [year in french]) to Y (year)
#   mutate(ageadministered = gsub("A","Y",ageadministered)) %>%
#   
#   # extract operators at the begining (>, = , +) and remove from original column
#   mutate(
#     operator = str_extract(ageadministered, "^(>=|<=|>|<|\\+|=)"),
#     after_operator = str_remove(ageadministered, "^(>=|<=|>|<|\\+|=)")
#   ) %>%
#   
#   # add units to age administered
#   mutate(
#     # replace any "B" with "Birth"
#     ageadmin = str_replace_all(after_operator, "B", "Birth"),
#     
#     # if there is a Y followed by a number, remove the Y and paste years at the end
#     # same for months, weeks and days
#     ageadmin = case_when(
#       str_detect(after_operator, "^Y\\d+(\\.\\d+)?$") ~ str_glue("{str_remove(after_operator, 'Y')} years"),
#       str_detect(after_operator, "^M\\d+(\\.\\d+)?$") ~ str_glue("{str_remove(after_operator, 'M')} months"),
#       str_detect(after_operator, "^W\\d+(\\.\\d+)?$") ~ str_glue("{str_remove(after_operator, 'W')} weeks"),
#       str_detect(after_operator, "^D\\d+(\\.\\d+)?$") ~ str_glue("{str_remove(after_operator, 'D')} days"),
#       TRUE ~ ageadmin
#     ),
#     
#     # strip extra Y/W from ageadmin (if any)
#     ageadmin = str_replace_all(ageadmin, "Y", ""),
#     ageadmin = str_replace_all(ageadmin, "W", "")
#   ) %>%
#   
#   # fix examples where after_operator = Y1-Y2
#   mutate(
#     ageadmin = case_when(
#       str_detect(after_operator, "^Y[0-9.]+-Y[0-9.]+$") ~ 
#         str_replace(after_operator, "^Y([0-9.]+)-Y([0-9.]+)$", "\\1-\\2 years"),
#       TRUE ~ ageadmin
#     )
#   ) %>%
#   
#   # fix examples where after_operator = M1-M2
#   mutate(
#     ageadmin = case_when(
#       str_detect(after_operator, "^M[0-9.]+-M[0-9.]+$") ~ 
#         str_replace(after_operator, "^M([0-9.]+)-M([0-9.]+)$", "\\1-\\2 months"),
#       TRUE ~ ageadmin
#     )
#   ) %>%
#   
#   # fix examples where after_operator = W1-W2
#   mutate(
#     ageadmin = case_when(
#       str_detect(after_operator, "^W[0-9.]+-W[0-9.]+$") ~ 
#         str_replace(after_operator, "^W([0-9.]+)-W([0-9.]+)$", "\\1-\\2 weeks"),
#       TRUE ~ ageadmin
#     )
#   ) %>%
#   
#   # fix examples where after_operator = D1-D2
#   mutate(
#     ageadmin = case_when(
#       str_detect(after_operator, "^D[0-9.]+-D[0-9.]+$") ~ 
#         str_replace(after_operator, "^D([0-9.]+)-D([0-9.]+)$", "\\1-\\2 days"),
#       TRUE ~ ageadmin
#     )
#   ) %>%
#   
#   # dealing with ranges
#   mutate(
#     ageadmin = case_when(
#       # Range: Birth to days (e.g. B-D5)
#       str_detect(after_operator, "^B-D\\d+(\\.\\d+)?$") ~
#         str_glue("Birth–{str_remove(after_operator, 'B-D')} days"),
#       
#       # Range: Birth to months (e.g. B-M1)
#       str_detect(after_operator, "^B-M\\d+(\\.\\d+)?$") ~
#         str_glue("Birth–{str_remove(after_operator, 'B-M')} months"),
#       
#       # Range: Birth to weeks (e.g. B-W6)
#       str_detect(after_operator, "^B-W\\d+(\\.\\d+)?$") ~
#         str_glue("Birth–{str_remove(after_operator, 'B-W')} weeks"),
#       
#       # Range: Birth to years (e.g. B-Y1.5)
#       str_detect(after_operator, "^B-Y\\d+(\\.\\d+)?$") ~
#         str_glue("Birth–{str_remove(after_operator, 'B-Y')} years"),
#       
#       # Single values: years, months, weeks, days (support decimals)
#       str_detect(after_operator, "^Y\\d+(\\.\\d+)?$") ~ str_glue("{str_remove(after_operator, 'Y')} years"),
#       str_detect(after_operator, "^M\\d+(\\.\\d+)?$") ~ str_glue("{str_remove(after_operator, 'M')} months"),
#       str_detect(after_operator, "^W\\d+(\\.\\d+)?$") ~ str_glue("{str_remove(after_operator, 'W')} weeks"),
#       str_detect(after_operator, "^D\\d+(\\.\\d+)?$") ~ str_glue("{str_remove(after_operator, 'D')} days"),
#       
#       # fallback
#       TRUE ~ ageadmin
#     )
#   ) %>%
#   
#   # singularise when it is 1
#   mutate(ageadmin = if_else(ageadmin %in% c("1 years", "<1 years", "1 days", "1 weeks"), str_replace(ageadmin, "s", ""), ageadmin),
#   ) %>%
#   
#   mutate(ageadmin = case_when(!is.na(operator) ~ str_glue("{operator}{ageadmin}"),
#                               TRUE ~ ageadmin),
#          year = as.character(year)) %>%
#   distinct()
# 
# # check
# # tbl3 %>% select(ageadministered, operator, ageadmin) %>% unique() %>% arrange(ageadministered) %>% View()
# # make a flag for if the country does not have hpv data so we can hide the HPV pages
# no_data <- function(df) {
#   return(nrow(df) == 0)
# }
# 
# # final table
# tbl_schedule_r <- tbl_schedule_r %>%
#   filter(iso3c == x) %>%
#   select(Level = geoarea,
#          Vaccine = vaccine,
#          `Dose number` = dosenumber,
#          `Age administered` = ageadmin) %>%
#   pivot_wider(., names_from = `Dose number`, values_from = `Age administered`)
# 
# 
# ## convert to flextable ----
# if (no_data(tbl_schedule_r) == FALSE) {
#   tbl3 <- flextable(tbl_schedule_r) %>%
#     set_table_properties(layout = "fixed")
#   
#   # specify column widths
#   n_cols <- ncol(tbl_schedule_r)
#   col_widths <- c(1, 1.9, rep(1, n_cols - 2))
#   
#   # apply widths per column
#   for (i in seq_len(n_cols)) {
#     tbl3 <- width(tbl3, j = i, width = col_widths[i])
#   }
#   
#   tbl <- add_header_row(
#     tbl3,
#     values = c("", "Dose number and age administered"),
#     colwidths = c(2, ncol(tbl_schedule_r) - 2)  
#   ) %>%
#     # make column headers bold
#     bold(part = "header", bold = TRUE)
#   
#   total_width_sched <- sum(col_widths)
#   tbl_schedule <- tbl
# }
# 
# # get year schedule is from for this country
# country_sched_year <- wiise_schedule %>%
#   filter(iso3c == x) %>%
#   pull(year) %>%
#   unique() %>%
#   sort(decreasing = TRUE) %>%
#   first()
# 
