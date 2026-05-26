###### plt_ia2030_zd_proj_bar_line :: progress towards zd target :: bar-chart, linechart ######
zd <- wuenic_dta %>%
  filter(vaccine == 'dtp1') %>%
  mutate(measure = 'unvaccinated') %>%
  select(lvl_1:vaccine, measure, unvaccinated)

zd_target <- zd %>%
  filter(year == 2019) %>%
  mutate(unvaccinated = unvaccinated/2,
         measure = 'IA2030 zero-dose target',
         year = 2030)

# data for bars
zd2 <- zd %>%
  mutate(measure = 'Estimate') %>%
  rbind(zd_target) %>%
  mutate(chart = 'bar',
         unvaccinated = round(unvaccinated, 0))

# target 
target <- zd %>%
  filter(vaccine == 'dtp1',
         year %in% c(2019)) %>%
  select(lvl_1:country, vaccine, year, unvaccinated) %>%
  pivot_wider(., names_from = year, values_from = unvaccinated) %>%
  mutate(target = `2019`/2) %>%
  mutate(`2020` = `2019`) %>%
  mutate(factor = (`2020`-target)/(2030-2020)) %>%
  mutate(`2021` = `2020` - factor) %>%
  mutate(`2022` = `2021` - factor) %>%
  mutate(`2023` = `2022` - factor) %>%
  mutate(`2024` = `2023` - factor) %>%
  mutate(`2025` = `2024` - factor) %>%
  mutate(`2026` = `2025` - factor) %>%
  mutate(`2027` = `2026` - factor) %>%
  mutate(`2028` = `2027` - factor) %>%
  mutate(`2029` = `2028` - factor) %>%
  mutate(`2030` = `2029` - factor) %>%
  pivot_longer(-c(lvl_1:vaccine), names_to = 'year', values_to = 'zerodose_mt') %>%
  filter(year != 'factor', year != 'target') %>%
  mutate(year = as.numeric(year)) %>%
  select(lvl_1:year, unvaccinated_new = zerodose_mt) %>%
  left_join(., zd) %>%
  filter(year %in% c(2020:2030)) %>%
  mutate(diff = unvaccinated - unvaccinated_new,
         measure = "unvaccinated")  %>%
  select(lvl_1:year, measure, unvaccinated_new, unvaccinated, diff)

# data for lines
target_lines <- target %>%
  select(lvl_1:unvaccinated_new) %>%
  rename(unvaccinated = unvaccinated_new) %>%
  mutate(chart = 'line',
         measure = 'IA2030')

ia2030_2019 <- zd2 %>%
  filter(year == 2019) %>%
  mutate(chart = 'line',
         measure = 'IA2030')

# append
zd2 <- rbind(zd2, target_lines, ia2030_2019)  %>%
  filter(!(year == 2019 & chart == 'line')) %>%
  distinct()

plt_ia2030_zd_proj_bar_line <- gg <-
  ggplot() + 
  geom_col(data = zd2 %>% filter(chart=='bar'), aes(y=unvaccinated, x=year, fill=measure)) +
  geom_line(data = zd2 %>% filter(chart=='line'), aes(y=unvaccinated, x=year)) +
  geom_point(data = zd2 %>% filter(chart=='line'), aes(y=unvaccinated, x=year)) +
  # geom_text(data = zd2 %>% filter(chart=='bar'), aes(y=unvaccinated, x=year, label = format(unvaccinated, big.mark=",")), vjust = -0.5, size = 3) +
  labs(y = get_text("plt_ia2030_zd_proj_bar_line_y", language),
       x = "",
       title = get_text("plt_ia2030_zd_proj_bar_line_title", language),
       caption = get_text("plt_ia2030_zd_proj_bar_line_cpt", language)) +
  theme_minimal()+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8.5),
        axis.text.y = element_text(size=9),
        legend.text = element_text(size=11),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#0058AB","#69DBFF"),name = "") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(seq(base_yr_lo,rev_yr,1), 2025,
                                2030))


## narrative ----
df_txt <- zd2 %>%
  filter(year == rev_yr) %>%
  select(lvl_3, measure, unvaccinated) %>%
  pivot_wider(., names_from = measure, values_from = unvaccinated) %>%
  mutate(diff = Estimate - IA2030,
         diff_txt = case_when(diff > 0 ~ "higher than",
                              diff < 0 ~ "lower than (ie. the country had achieved)",
                              diff == 0 ~ "same as"),
         pcnt_diff = round((Estimate / IA2030 - 1) * 100,0))

# txt_plt_ia2030_zd_proj_bar_line <- str_glue("IA2030 aims to leave no one behind with immunization and calls on all countries to reduce the number of zero dose children by half by 2030.\n
# This chart shows:
# • Estimated number of zero-dose children in 2000-{rev_yr} (dark blue bars)
# • Zero-dose target by 2030 (light blue bar)
# • Annual goals to reach the 2030 target based on a linear trajectory of decline (points)\n
# In {rev_yr}, the number of zero-dose children was approximately {df_txt %>% pull(pcnt_diff) %>% abs()}% {df_txt %>% pull(diff_txt)} the annual goal.")


# the first part is the same for all countries, so will come from the translation table instead of translating in DeepL
txt <- str_glue("In {rev_yr}, the number of zero-dose children was approximately {df_txt %>% pull(pcnt_diff) %>% abs()}% {df_txt %>% pull(diff_txt)} the annual number proposed to reach the target, based on a linear trajectory of decline.")

txt <- gsub("NaN%", "the", txt)
txt_ia2030_zd_proj_bar_line <- txt


## top line message ----
df_tlm <- df_txt %>%
  mutate(diff_txt2 = case_when(grepl("higher", diff_txt) ~ "above",
                               grepl("lower", diff_txt) ~ "below",
                               TRUE ~ "approximately the same as"))

tlm <- str_glue("In {rev_yr}, the number of zero-dose children was {abs(df_tlm$pcnt_diff)}% {df_tlm$diff_txt2} the annual IA2030 goal")

if (df_tlm$pcnt_diff < -10) {
  diff <- "lower"
  tlm <- str_glue("{tlm}, positioning the programme to reach, and potentially exceed the 2030 goal if current progress continues.")
  
} else if (df_tlm$pcnt_diff >= -10 & df_tlm$pcnt_diff < 10) {
  diff <- "approx_same"
  tlm <- str_glue("{tlm}, bringing the 2030 goal within reach if progress is sustained.")
  
} else if (df_tlm$pcnt_diff >= 10 & df_tlm$pcnt_diff  <= 20) {
  diff <- "higher"
  tlm <- str_glue("{tlm}, putting the 2030 goal at risk without accelerated action.")
  
} else if (df_tlm$pcnt_diff  > 20) {
  diff <- "cons_higher"
  tlm <- str_glue("{tlm}, putting the 2030 goal at serious risk without accelerated action.")
} 

tlm_ia2030_zd_proj_bar_line <- tlm