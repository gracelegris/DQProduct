###### tbl_stock :: wiise stock data ######
## prep data ----
tbl_stock_r <- wiise_stockouts %>% 
  filter(iso3c == x,
         year >= rev_yr - 4) %>%
  arrange(year, iso3c, vaccine) %>%
  pivot_wider(., names_from = year, values_from = national_subnational) %>%
  select(-iso3c) %>%
  arrange(vaccine) %>%
  rename(`Vaccines / supplies` = vaccine) %>%
  # make numeric values character and set NA to ""
  mutate(across(where(is.numeric), ~ as.character(.))) %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), "", .))) %>%
  # replace 'month' or 'months' with 'm'
  mutate(across(where(is.character), ~ str_replace_all(., regex(" months?| Months?", ignore_case = FALSE), "m")))


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
