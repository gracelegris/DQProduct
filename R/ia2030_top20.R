###### ia2030_top20 :: IA2030 top 20 countries :: bump chart ###### 
## prep data ----
wuenic2021rev <- read_rds(here(str_glue(paste0(wd,"/WUENIC Revision/2021 rev/clean_wuenic_MASTER_2021rev.rds")))) %>%
  mutate(country = case_when(iso3c == "bol" ~ "Bolivia",
                             iso3c == "caf" ~ "CAR",
                             iso3c == "cod" ~ "DRC",
                             iso3c == "fsm" ~ "Micronesia",
                             iso3c == "irn" ~ "Iran",
                             iso3c == "png" ~ "PNG",
                             iso3c == "prk" ~ "DPRK",
                             iso3c == "syr" ~ "Syria",
                             iso3c == "lao" ~ "Laos",
                             iso3c == "tza" ~ "Tanzania",
                             iso3c == "ven" ~ "Venezuela",
                             iso3c == "tur" ~ "Turkiye",
                             TRUE ~ country)) %>%
  left_join(., reg_ref) %>%
  filter(lvl_1 == 'country', region_zd == 'region_zd', vaccine == 'dtp1', year == 2021) %>%
  distinct() %>%
  # mutate(lvl_3 = !!sym(grp_name)) %>%
  # select(-c(region_zd, !!sym(grp_name))) %>%
  select(-c(wuenic, region_zd, region_unicef_ops, country_fr, country_es, country_pt))

wueniclatestrev2 <- wueniclatestrev %>%
  filter(lvl_1 == 'country',
         year %in% c(2022:rev_yr))  %>%
  filter(lvl_2=='region_unicef_ops') %>%
  filter(lvl_1 == 'country', 
         vaccine == 'dtp1') %>%
  distinct()

top20_rank2 <- rbind(wuenic2021rev, wueniclatestrev2) %>%
  # # use translted country names
  translate_ctry_names() %>%
  # left_join(reg_ref %>% mutate(iso3c = tolower(iso3c)) %>% select(iso3c, country_fr, country_es, country_pt)) %>%
  # mutate(country = case_when(language == "fr" & iso3c != "REG" ~ country_fr,
  #                            language == "es" & iso3c != "REG"  ~ country_es,
  #                            language == "pt" & iso3c != "REG"  ~ country_pt,
  #                            TRUE ~ country)) %>%
  select(iso3c, country, year, vaccine, unvaccinated, unvaccinated_lbl) %>%
  distinct() %>%
  group_by(year) %>%
  arrange(year, desc(unvaccinated)) %>%
  mutate(rank = seq(n())) %>%
  ungroup() %>%
  filter(rank<=20) %>%
  select(iso3c, country, year, rank) %>%
  # join on region of interest name
  left_join(., wuenic_dta[c('lvl_3', 'country')], by = 'country') %>%
  distinct() %>%
  rename(region=lvl_3) %>%
  mutate(region_country = ifelse(!is.na(region), paste0(region, "_", country), "AAA")) %>%
  filter(year %in% c(2021, rev_yr))


## plot ----
gg <-
  ggplot(top20_rank2, aes(x = year, y = rank, group = country)) +
  geom_bump(linewidth = 1.5, aes(colour = region_country)) +
  geom_point(size = 6, aes(colour = region_country)) +
  geom_text(data = top20_rank2 %>% filter(year == min(year)),
            aes(x = year - 0.1, label = country),
            size = 4, hjust = 1) +
  geom_text(data = top20_rank2 %>% filter(year == max(year)),
            aes(x = year + 0.1, label = country),
            size = 4, hjust = 0) +
  # geom_text(data = top20_rank2, aes(label=rank), size=3) +
  scale_colour_manual(values = c36) +
  scale_y_reverse( lim=c(20,1), breaks = seq(1,20, by = 1))+
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),  # remove minor y axis grid lines
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size=11)) +  # remove x axis grid lines
  labs(title = get_text("plt_ia2030_top20_title", language), x='', y='', 
       caption = get_text("plt_ia2030_top20_cpt", language)) +
  scale_x_continuous(
    limits = c(2020.2, rev_yr + 1),  # adjust spacing to tighten width
    breaks = c(2021, rev_yr),          # show only start and end year
    labels = c("2021", as.character(rev_yr))
  )

plt_ia2030_top20 <- gg


## narrative ----
in_top20_Zd <- top20_rank2 %>%
  filter(iso3c == x)

df_txt <- top20_rank2 %>%
  filter(iso3c == x) %>%
  mutate(n = n_distinct(year)) %>%
  ungroup()

if (no_data(in_top20_Zd) == FALSE) {

  if (unique(df_txt$n) == 2) {
    txt <- str_glue("{ctryn} was in the top 20 zero-dose countries globally in both 2021 and {rev_yr}.")
    
  } else if (unique(df_txt$n) == 1 & df_txt$year == 2021){
    txt <- str_glue("{ctryn} was in the top 20 zero-dose countries globally in 2021 but not in {rev_yr}.")
    
  } else if (unique(df_txt$n) == 1 & df_txt$year == rev_yr) {
    txt <- str_glue("{ctryn} was not in the top 20 zero-dose countries globally in 2021 but was in {rev_yr}.")
  } else {
    txt <- str_glue("{ctryn} was not in the top 20 zero-dose countries globally in 2021 or in {rev_yr}.")
  }
} else {
  txt <- str_glue(".")
}

txt_ia2030_top20 <- txt


## top line message ----
tlm_ia2030_top20 <- tlm <- txt