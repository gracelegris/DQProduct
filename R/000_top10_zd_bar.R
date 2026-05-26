###### topn_zd_bar_dtp :: zd-top[n]-bar: top n countries with highest number of zd children  ######
################################ V1 ################################ 
# bar chart: top n high burden ZD countries
df_plt <-
  wuenic_dta %>% 
  filter(lvl_1 == 'country',
         lvl_3 == regn,
         vaccine == 'dtp1',
         year == rev_yr) %>% 
  label_vals(unvaccinated, 'unvacc_lbl') %>%  # number of zd children in thousands
  mutate(unvacc_lbl = str_c(country, '\n', unvacc_lbl), # country and number ZD children
         cvg_cat = case_when(coverage < 60 ~ '<60',
                             coverage %in% c(60:69) ~ '60-69',
                             coverage %in% c(70:79) ~ '70-79',
                             coverage %in% c(80:89) ~ '80-89',
                             coverage %in% c(90:94) ~ '90-94',
                             coverage >=95 ~ '>=95')
  ) %>% 
  slice_max(unvaccinated, n = nslice) %>%  # select the top n countries with highest ZD
  mutate(plt_grp = if_else(row_number() %in% 1:nslice, 1, 2)) %>% 
  group_by(plt_grp) %>% 
  mutate(plt_row = row_number()) %>% 
  ungroup() %>% 
  select(-matches('lvl'), -comment) # drop columns that have prefix "lvl" and comment col


df <- df_plt %>%
  mutate(unvacc_prev = 100 - coverage) %>%
  select(iso3c, country, vaccine, year, unvaccinated, unvacc_prev, cvg_cat) %>%
  pivot_longer(-c(iso3c:year, cvg_cat), names_to = "measure", values_to = "value") %>%
  mutate(measure = factor(measure, levels = c("unvaccinated", "unvacc_prev"))) %>%
  mutate(
    prev_cat = case_when(value >= 40 ~ '>=40',
                         value %in% c(30:39) ~ '30-39',   # 31-40
                         value %in% c(20:29) ~ '20-29',   # 21-30
                         value %in% c(10:19) ~ '10-19',   # 11-20
                         value %in% c(5:9) ~ '5-9',     # 6-10
                         value <5 ~ '<5')              # <=5
  ) %>%
  label_vals_millions(value, "value_lbl")


library(ggforce)  # for facetted_pos_scales

# Reorder countries by unvaccinated values
df <- df %>%
  group_by(country) %>%
  mutate(unvaccinated_value = value[measure == "unvaccinated"]) %>%
  ungroup() %>%
  mutate(country = fct_reorder(country, unvaccinated_value))

ggplot(df, aes(x = country, y = value)) +
  geom_col(
    data = df %>% filter(measure == "unvaccinated"),
    fill = "#1CABE2"
  ) +
  geom_col(
    data = df %>% filter(measure == "unvacc_prev"),
    aes(fill = prev_cat)
  ) +
  geom_text(
    aes(label = ifelse(
      measure == "unvaccinated",
      value_lbl,
      paste0(value, "%")
    )),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip(clip = "off") +
  facet_wrap(
    ~ measure,
    scales = "free_x",
    labeller = as_labeller(c(
      unvaccinated = "Zero-dose (#)",
      unvacc_prev = "Zero-dose prevalence (%)"
    ))
  ) +
  facetted_pos_scales(
    y = list(
      measure == "unvaccinated" ~
        scale_y_continuous(
          labels = scales::comma,
          expand = expansion(mult = c(0, .1))
        ),
      measure == "unvacc_prev" ~
        scale_y_continuous(
          limits = c(0, 100),
          expand = expansion(mult = c(0, .1))
        )
    )
  ) +
  scale_fill_manual(
    values = c(
      ">=40" = "#B50800",
      "30-39" = "#E2231A",
      "20-29" = "#F26A21",
      "10-19" = "#FFC20E",
      "5-9" = "#80BD41",
      "<5" = "#00833D"
    ),
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(x = NULL, y = NULL, fill = "Zero-dose prevalence (%)",
       title = str_glue("Top 10 countries with the most zero-dose children, {rev_yr}"),
       caption = str_glue("{wuenic_src}")) +
  theme_minimal() +
  theme(legend.position = "bottom")




################################ V2 ################################ 
# bar chart: top n high burden ZD countries
df_plt <-
  wuenic_dta %>% 
  filter(lvl_1 == 'country',
         lvl_3 == regn,
         vaccine == 'dtp1',
         year == rev_yr) %>% 
  label_vals(unvaccinated, 'unvacc_lbl') %>%  # number of zd children in thousands
  mutate(unvacc_prev = 100 - (vaccinated / target * 100)) %>%
  select(iso3c, country, vaccine, year, unvaccinated, unvacc_prev)

# top zd
df1 <- df_plt %>%
  slice_max(unvaccinated, n = nslice) %>%  # select the top n countries with highest ZD
  mutate(plt_grp = if_else(row_number() %in% 1:nslice, 1, 2)) %>% 
  group_by(plt_grp) %>% 
  mutate(plt_row = row_number()) %>% 
  ungroup() %>%
  select(iso3c:unvaccinated) %>%
  mutate(measure = "unvaccinated") %>%
  rename(value = unvaccinated) %>%
  label_vals_millions(value, "value_lbl")


# top zd prevalence
df2 <- df_plt %>%
  slice_max(unvacc_prev, n = nslice) %>%  # select the top n countries with highest ZD
  mutate(plt_grp = if_else(row_number() %in% 1:nslice, 1, 2)) %>% 
  group_by(plt_grp) %>% 
  mutate(plt_row = row_number()) %>% 
  ungroup() %>%
  select(iso3c:year, unvacc_prev) %>%
  mutate(measure = "unvacc_prev") %>%
  rename(value = unvacc_prev) %>%
  mutate(value = round(value, 0)) %>%
  mutate(
    prev_cat = case_when(value >= 40 ~ '>=40',
                         value %in% c(30:39) ~ '30-39',   # 31-40
                         value %in% c(20:29) ~ '20-29',   # 21-30
                         value %in% c(10:19) ~ '10-19',   # 11-20
                         value %in% c(5:9) ~ '5-9',     # 6-10
                         value <5 ~ '<5')              # <=5
  )


# plot 1
df <- df1 

gg1 <-
  ggplot(df, aes(x = reorder(country, value), y = value)) +
  geom_col(
    fill = "#1CABE2"
  ) +
  geom_text(
    aes(label = value_lbl),
    hjust = -0.2,
    size = 3
  ) +
  coord_flip(clip = "off") +
  labs(x = NULL, y = NULL, 
       title = "Number of zero-dose children (#)") +
  theme_minimal() +
  scale_y_continuous(
    labels = scales::comma
    # expand = expansion(mult = c(0, .1))
  ) +
  theme(
    plot.title = element_text(size = 12) 
  )


# plot 2
df <- df2 

gg2 <-
  ggplot(df, aes(x = reorder(country, value), y = value)) +
  geom_col(
    fill = "#1CABE2"
  ) +
  geom_text(
    aes(label = value),
    hjust = -0.5,
    size = 3
  ) +
  coord_flip(clip = "off") +
  labs(x = NULL, y = NULL, 
       title = "Zero-dose prevalence (%)",
       caption = str_glue("{wuenic_src}")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12) 
  )
# scale_y_continuous(
#   limits = c(0, 100),
#   expand = expansion(mult = c(0, .1))
# )

# Combine plots
# combined <- 
gg1 + gg2 +
  plot_layout(ncol = 2, guides = "collect") +  # stack vertically & collect legends
  plot_annotation(
    title = str_glue("Top 10 countries with highest number and prevalence of zero-dose children, {rev_yr}")
  ) & 
  theme(
    legend.position = "bottom"  # move the collected legend below
  )



################################ V3 ################################ 
# bar chart: top n high burden ZD countries
df_plt <-
  wuenic_dta %>% 
  filter(lvl_1 == 'country',
         lvl_3 == regn,
         vaccine == 'dtp1',
         year == rev_yr) %>% 
  label_vals(unvaccinated, 'unvacc_lbl') %>%  # number of zd children in thousands
  mutate(unvacc_lbl = str_c(country, '\n', unvacc_lbl), # country and number ZD children
         cvg_cat = case_when(coverage < 60 ~ '<60',
                             coverage %in% c(60:69) ~ '60-69',
                             coverage %in% c(70:79) ~ '70-79',
                             coverage %in% c(80:89) ~ '80-89',
                             coverage %in% c(90:94) ~ '90-94',
                             coverage >=95 ~ '>=95')
  ) 

# top zd
df1 <- df_plt %>%
  arrange(desc(unvaccinated)) %>%
  slice_max(unvaccinated, n = nslice) %>%  # select the top n countries with highest ZD
  mutate(plt_grp = if_else(row_number() %in% 1:nslice, 1, 2)) %>%
  group_by(plt_grp) %>%
  mutate(plt_row = row_number()) %>%
  ungroup() %>%
  select(iso3c:year, unvaccinated, coverage, cvg_cat) %>%
  mutate(measure = "unvaccinated") %>%
  rename(value = unvaccinated)


# top zd prevalence
df2 <- df_plt %>%
  mutate(unvacc_prev = 100 - (vaccinated / target * 100)) %>%
  arrange(desc(unvacc_prev)) %>%
  slice_max(unvacc_prev, n = nslice) %>%  # select the top n countries with highest ZD
  mutate(plt_grp = if_else(row_number() %in% 1:nslice, 1, 2)) %>%
  group_by(plt_grp) %>%
  mutate(plt_row = row_number()) %>%
  ungroup() %>%
  select(iso3c:year, unvacc_prev, coverage, cvg_cat) %>%
  mutate(measure = "unvacc_prev") %>%
  rename(value = unvacc_prev) %>%
  mutate(value = round(value, 0))

df <- bind_rows(df1, df2) %>%
  label_vals_millions(value, "value_lbl") %>%
  mutate(country = ifelse(measure == "unvacc_prev", str_glue("{country} "), country)) %>%
  # create a facet-specific country factor
  group_by(measure) %>%
  arrange(value, .by_group = TRUE) %>%  # ascending order within each measure
  mutate(country_f = factor(country, levels = unique(country))) %>%
  ungroup() %>%
  mutate(measure = factor(measure, levels = c("unvaccinated", "unvacc_prev"))) 

ggplot(df, aes(x = country_f, y = value, fill = cvg_cat)) +
  geom_col() +
  facet_wrap(
    ~ measure,
    scales = "free",
    labeller = as_labeller(c(
      unvaccinated = "Zero-dose (#)",
      unvacc_prev = "Zero-dose prevalence (%)"
    ))
  ) +
  coord_flip(clip = "off") +
  geom_text(
    aes(label = ifelse(
      measure == "unvaccinated",
      value_lbl,
      paste0(value, "%")
    )),
    hjust = -0.1,
    size = 3
  ) +
  scale_fill_manual(
    values = c(
      "<60" = "#B50800",
      "60-69" = "#E2231A",
      "70-79" = "#F26A21",
      "80-89" = "#FFC20E",
      "90-95" = "#80BD41",
      ">=95" = "#00833D"
    )
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = NULL, y = NULL, fill = "DTP1 coverage (%)",
    title = str_glue("Top 10 countries with the highest number and prevalence of zero-dose children, {rev_yr}"),
    caption = str_glue("{wuenic_src}")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facetted_pos_scales(
    y = list(
      measure == "unvaccinated" ~
        scale_y_continuous(
          labels = scales::comma,
          expand = expansion(mult = c(0, .1))
        ),
      measure == "unvacc_prev" ~
        scale_y_continuous(
          limits = c(0, 75),
          expand = expansion(mult = c(0, .1))
        )
    )
  )




################################ V4 ################################ 
# bar chart: top n high burden ZD countries
df_plt <-
  wuenic_dta %>% 
  filter(lvl_1 == 'country',
         lvl_3 == regn,
         vaccine == 'dtp1',
         year == rev_yr) %>% 
  label_vals(unvaccinated, 'unvacc_lbl') %>%  # number of zd children in thousands
  mutate(unvacc_lbl = str_c(country, '\n', unvacc_lbl), # country and number ZD children
         cvg_cat = case_when(coverage < 60 ~ '<60',
                             coverage %in% c(60:69) ~ '60-69',
                             coverage %in% c(70:79) ~ '70-79',
                             coverage %in% c(80:89) ~ '80-89',
                             coverage %in% c(90:94) ~ '90-94',
                             coverage >=95 ~ '>=95')
  ) %>% 
  slice_max(unvaccinated, n = nslice) %>%  # select the top n countries with highest ZD
  mutate(plt_grp = if_else(row_number() %in% 1:nslice, 1, 2)) %>% 
  group_by(plt_grp) %>% 
  mutate(plt_row = row_number()) %>% 
  ungroup() %>% 
  select(-matches('lvl'), -comment) # drop columns that have prefix "lvl" and comment col


df <- df_plt %>%
  select(iso3c, country, vaccine, year, unvaccinated, unvaccinated_lbl, cvg_cat)

ggplot() +
  geom_col(
    data = df,
    aes(x = reorder(country, unvaccinated), y = unvaccinated, fill = cvg_cat)
  ) +
  geom_text(
    data = df,
    aes(
      x = reorder(country, unvaccinated),
      y = unvaccinated,
      label = unvaccinated_lbl
    ),
    hjust = -0.2,
    size = 3
  ) +
  coord_flip(clip = "off") +  # clip off stops labels being cut off
  scale_fill_manual(values = c(
    "<60" = "#B50800",
    "60-69" = "#E2231A",
    "70-79" = "#F26A21",
    "80-89" = "#FFC20E",
    "90-95" = "#80BD41",
    ">=95" = "#00833D"
  )) +
  labs(
    x = NULL,
    y = "\n# zero-dose children",
    fill = "DTP1 coverage (%)",
    title = str_glue("Top 10 countries with the most zero-dose children, {rev_yr}"),
    caption = str_glue("{wuenic_src}")
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  expand_limits(y = max(df$unvaccinated) * 1.1)

