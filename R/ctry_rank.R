###### ctry_rank :: rank chart  ######
## prep data ----
nrank <- wueniclatestrev %>% filter(lvl_1 == 'country', lvl_2 == 'region_unicef_ops', lvl_3 == regn) %>%
  mutate(n_ctry = n_distinct(country)) %>%
  pull(n_ctry) %>%
  unique()

top20_rank <- wueniclatestrev %>%
  filter(lvl_1 == 'country', 
         lvl_2 == 'region_unicef_ops', 
         lvl_3 == regn,
         vaccine == 'dtp1',
         year %in% c(2021:rev_yr)) %>% 
  label_vals(unvaccinated, 'unvacc_lbl') %>%
  group_by(year) %>%
  arrange(desc(unvaccinated)) %>%
  mutate(rank = seq(n())) %>%
  ungroup() %>%
  # use translted country names
  translate_ctry_names() %>%
  # filter(rank<=20) %>%
  select(lvl_3, iso3c, country, year, unvacc_lbl, rank) %>%
  mutate(country_lbl = paste0(country, " (", unvacc_lbl, ")")) %>%
  # text size, LACR should be smaller because there are more countries to fit in the rank chart
  mutate(text_size = case_when(lvl_3 %in% c('LACR', 'Non-programme') ~ 2.9,
                               lvl_3 == 'ROSA' ~ 4,
                               TRUE ~ 3.5)) %>%
  mutate(region_country = ifelse(iso3c == x, paste0(lvl_3, "_", country), "AAA"))

txt_size <- unique(top20_rank$text_size)


## plot ----
gg <-
  ggplot(top20_rank, aes(x = year, y = rank, group=country)) +
  geom_bump(aes(colour = region_country), linewidth = 1.5) +
  geom_point(aes(colour = region_country), size = 4) +
  scale_colour_manual(values = c36) +
  geom_text(data = top20_rank %>% filter(year == min(year)),
            aes(x = year - 0.1, label = country_lbl),
            size = txt_size, hjust = 1) +
  geom_text(data = top20_rank %>% filter(year == max(year)),
            aes(x = year + 0.1, label = country_lbl),
            size = txt_size, hjust = 0) +
  scale_y_reverse( lim=c(nrank,1), breaks = seq(1,nrank, by = 1))+
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),  # remove minor y axis grid lines
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size=11)) +  # remove x axis grid lines
  labs(title = get_text("plt_ctry_rank_title", language), x='', y='', 
       caption = get_text("plt_ctry_rank_cpt", language)) +
  # scale_x_discrete(limits = c(2021:rev_yr), breaks = seq(2021,rev_yr, by = 1)) +
  scale_x_continuous(
    limits = c(2019.8, rev_yr + 1.3),  # reduce white space
    breaks = seq(2021, rev_yr, by = 1)
  )

plt_ctry_rank <- gg


## narrative ----
df_txt <- top20_rank %>%
  group_by(country) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(top3 = case_when(n == 3 ~ str_glue("The same countries remained in the top 3 in 2021 and {rev_yr}."),
                          TRUE ~ str_glue("There was some change in the top 3 countries between 2021 and {rev_yr}."))) %>%
  filter(n==min(n))

# note :: first and last sentences are in the translation table
txt <- str_glue("In 2021, {ctryn} ranked number {df_txt %>% filter(country==ctryn & year==2021) %>% pull(rank)} out of {n_distinct(df_txt$country)} countries with {df_txt %>% filter(country==ctryn & year==2021) %>% pull(unvacc_lbl)} zero-dose children.\n
                     In {rev_yr}, {ctryn} ranked number {df_txt %>% filter(country==ctryn & year==rev_yr) %>% pull(rank)} out of {n_distinct(df_txt$country)} countries with {df_txt %>% filter(country==ctryn & year==rev_yr) %>% pull(unvacc_lbl)} zero-dose children.")

# fix text for non-programme countries
txt <- gsub("countries in Non-programme","non-programme countries", txt)

txt_ctry_rank <- txt