###### tbl_stock :: wiise stock data ######

# grace - function to translate stockout levels based on the components of the string (e.g., "National", "Subnational", months in parentheses, "and subnational" at the end)
translate_stockout_level <- function(string, lang) {
  if (is.na(string) || string == "") return(string)
  
  # 1. Fetch core translation blocks
  txt_nat   <- t_lookup("tbl_schedule_lvl_nat", lang)
  txt_sub   <- t_lookup("tbl_schedule_lvl_subnat", lang)
  txt_and   <- t_lookup("txt_and_subnational", lang)
  txt_m     <- t_lookup("txt_months", lang)
  
  # 2. Case: Pure Subnational
  if (string == "Subnational") return(txt_sub)
  
  # 3. Extract months if they exist inside parentheses (e.g., "4.6")
  has_months <- grepl("\\((.*?)\\s*months?\\)", string)
  months_val <- if (has_months) gsub(".*\\((.*?)\\s*months?\\).*", "\\1", string) else NULL
  
  # 4. Check if it includes subnational tracking at the end
  has_subnational <- grepl("and subnational$", string)
  
  # 5. Build the translated string based on components
  # Base "National"
  out_string <- txt_nat
  
  # Add parsed months component: e.g., "National (4.6 mois)"
  if (has_months) {
    # Handles right-to-left layout spacing cleanly for Arabic if selected
    if (lang == "ar") {
      out_string <- paste0(out_string, " (", months_val, " ", txt_m, ")")
    } else {
      out_string <- paste0(out_string, " (", months_val, " ", txt_m, ")")
    }
  }
  
  # Add trailing subnational component: e.g., "National (4.6 mois) et subnational"
  if (has_subnational) {
    out_string <- paste0(out_string, " ", txt_and)
  }
  
  return(out_string)
}

## prep data ----
tbl_stock_r <- wiise_stockouts %>% 
  filter(iso3c == x,
         year >= rev_yr - 4) %>%
  arrange(year, iso3c, vaccine) %>%
  
  # 1. TRANSLATE CELL CONTENTS FIRST (while it's still a single column)
  mutate(
    national_subnational = purrr::map_chr(national_subnational, ~ translate_stockout_level(.x, language))
  ) %>%
  
  # 2. RUN TEXT REPLACEMENTS AFTER TRANSLATION
  # If you want to shorten "months" or your translated equivalent (e.g., "meses", "mois") to "m",
  # it's safest to target the English word before it translates, OR do it here:
  mutate(national_subnational = str_replace_all(national_subnational, regex(" months?| Months?", ignore_case = TRUE), "m")) %>%
  
  # 3. PIVOT WIDER NOW THAT CONTENTS ARE TRANSLATED
  pivot_wider(names_from = year, values_from = national_subnational) %>%
  select(-iso3c) %>%
  arrange(vaccine) %>%
  
  # 4. RENAME VACCINE COLUMN HEADERS DYNAMICALLY
  rename_with(~ get_text2("txt_vaccines_supplies", text_vars), .cols = vaccine) %>%
  
  # 5. CLEAN UP NA VALUES FOR DISPLAY
  # Note: Since pivot_wider can introduce missing years as NA, we clean characters here
  mutate(across(where(is.numeric), ~ as.character(.))) %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), "", .)))


## convert to flex table ----
if (no_data(tbl_stock_r) == FALSE) {
tbl <- flextable(tbl_stock_r) %>%
  set_table_properties(layout = "fixed") 

# specify column widths
n_cols <- ncol(tbl_stock_r)
col_widths <- c(2, rep(1.3, n_cols - 1))  # first column 3, others 1

for (i in seq_len(n_cols)) {
  tbl <- width(tbl, j = i, width = col_widths[i])
}

# align text: first column left, all others center
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

if (nrow(tbl_stock_r) > 1) {
tbl <- tbl %>%
  hline(i = 1:(nrow(tbl_stock_r)-1), border = border_clr, part = "body")  # add line between every pair of rows
  # border_outer(border = border_clr)  # optional: add border around the table

tbl_stock <- tbl
}
}




# OLD CODE

# ###### tbl_stock :: wiise stock data ######
# ## prep data ----
# tbl_stock_r <- wiise_stockouts %>% 
#   filter(iso3c == x,
#          year >= rev_yr - 4) %>%
#   arrange(year, iso3c, vaccine) %>%
#   pivot_wider(., names_from = year, values_from = national_subnational) %>%
#   select(-iso3c) %>%
#   arrange(vaccine) %>%
#   rename(`Vaccines / supplies` = vaccine) %>%
#   # make numeric values character and set NA to ""
#   mutate(across(where(is.numeric), ~ as.character(.))) %>%
#   mutate(across(where(is.character), ~ ifelse(is.na(.), "", .))) %>%
#   # replace 'month' or 'months' with 'm'
#   mutate(across(where(is.character), ~ str_replace_all(., regex(" months?| Months?", ignore_case = FALSE), "m")))
# 
# 
# ## convert to flex table ----
# if (no_data(tbl_stock_r) == FALSE) {
#   tbl <- flextable(tbl_stock_r) %>%
#     set_table_properties(layout = "fixed") 
#   
#   # specify column widths
#   n_cols <- ncol(tbl_stock_r)
#   col_widths <- c(2, rep(1.3, n_cols - 1))  # first column 3, others 1
#   
#   for (i in seq_len(n_cols)) {
#     tbl <- width(tbl, j = i, width = col_widths[i])
#   }
#   
#   # align text: first column left, all others center
#   tbl <- align(tbl, j = 1, align = "left") %>%
#     align(j = 2:n_cols, align = "center")
#   
#   tbl <- tbl %>%
#     # Align first column header left
#     align(j = 1, part = "header", align = "left") %>%
#     # Align other column headers center
#     align(j = 2:n_cols, part = "header", align = "center") %>%
#     # make column headers bold
#     bold(part = "header", bold = TRUE)
#   
#   # Define border colour
#   border_clr <- fp_border(color = "darkgrey", width = 1)
#   
#   if (nrow(tbl_stock_r) > 1) {
#     tbl <- tbl %>%
#       hline(i = 1:(nrow(tbl_stock_r)-1), border = border_clr, part = "body")  # add line between every pair of rows
#     # border_outer(border = border_clr)  # optional: add border around the table
#     
#     tbl_stock <- tbl
#   }
# }
