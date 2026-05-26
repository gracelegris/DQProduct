###### tbl_intro :: wiise intro year data ######
## prep data ----
# initial prep in main script
tbl_intro_r <- tbl_intro_r %>%
  mutate(`National introduction` = case_when(is.na(`National introduction`) ~ "Not introduced",
                                             TRUE ~ `National introduction`))
  # mutate(across(where(is.character), ~ ifelse(is.na(.), "Not introduced", .)))


## convert to flextable ----
tbl <- flextable(tbl_intro_r) %>%
  set_table_properties(layout = "fixed") 

# specify column widths
n_cols <- ncol(tbl_intro_r)
col_widths <- c(3, rep(1.5, n_cols - 1))

# apply widths per column
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

tbl <- tbl %>%
  hline(i = 1:(nrow(tbl_intro_r)-1), border = border_clr, part = "body")  # add line between every pair of rows


tbl_intro <- tbl