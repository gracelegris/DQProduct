###### plt_dumbell :: dumbell ######
## prep data ----
df <- wuenic_dta %>%
  filter(year %in% c(comp_yr:rev_yr)) %>%
  select(country, vaccine, year, coverage) %>%
  mutate(year = as.character(year)) %>%
  # filter for vaccines in the schedule all years 2019-rev_yr
  group_by(vaccine) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == max(n))

# identify countries where latest value is less than comparison year and join flag to df
lt_compyr <- df %>% filter(year %in% c(comp_yr, rev_yr)) %>% pivot_wider(., names_from = year, values_from = coverage) %>%
  mutate(diff = .[[5]] - .[[4]],
         flag = case_when(diff < 0 ~ "lower",
                          diff == 0 ~ "same",
                          diff > 0 ~ "higher")) %>%
  arrange(vaccine) %>%
  mutate(order = seq(n())) %>%
  select(vaccine, flag, order)

df <- left_join(df, lt_compyr) 

df <- df %>%
  mutate(vaccine2 = case_when(vaccine == 'hepb3' ~ 'HepB3',
                              vaccine == 'hib3' ~ 'Hib3',
                              vaccine == 'hepbb' ~ 'HepBB',
                              vaccine == 'pcv3' ~ 'PCV3',
                              vaccine == 'pol3' ~ 'Polio3',
                              vaccine == 'rcv1' ~ 'Rubella',
                              vaccine == 'rotac' ~ 'RotaC',
                              vaccine == 'menga' ~ 'MengA',
                              vaccine == 'hpv' ~ 'HPVc',
                              TRUE ~ toupper(vaccine))) %>%
  mutate(vaccine2 = case_when(flag == "lower" ~ paste0("<span style=\"color: ", "red", "\">", vaccine2, "</span>"),
                              flag == "same" ~ paste0("<span style=\"color: ", "#1CABE2", "\">", vaccine2, "</span>"),
                              TRUE ~ paste0("<span style=\"color: ", "#70AD47", "\">", vaccine2, "</span>")))



## plot ----
plt_dumbell <- gg <-
  ggplot(df, aes(x = coverage, reorder(vaccine2, order))) +
  geom_line(linewidth = 4.3, color = '#E8E8E8') +
  geom_point(data = df, color = '#E8E8E8', size = 0) +
  geom_point(data = df %>% filter(year %in% c(comp_yr, rev_yr-1, rev_yr)), aes(color = year, alpha=1), size = 4.3) +
  theme(legend.position = "bottom") + 
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, 25)) +
  scale_y_discrete(limits=rev) +  # reverse y-axis
  labs(x = get_text("coverage", language), y = '',
       title = get_text("plt_dumbell_title", language),
       caption = get_text("plt_dumbell_cpt", language)) +
  scale_color_manual(name = '', values = c("#1CABE2","#00833D","#F26A21")) +
  theme_minimal() +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 13),
        axis.text.y = ggtext::element_markdown(size = 13),
        panel.grid.minor.x = element_blank(),
        legend.text=element_text(size=13)) +
  guides(alpha = "none")  # remove alpha legend


## narrative ----
df_txt <- df

# rev_yr-1 compared to 2019
df_txt_y0y1 <- df %>%
  filter(year %in% c(comp_yr, rev_yr-1)) %>%
  arrange(vaccine, year) %>%
  group_by(vaccine) %>%
  mutate(lag_cvg = lag(coverage)) %>%
  ungroup() %>%
  mutate(chng = case_when(coverage > lag_cvg ~ "increased",
                          coverage < lag_cvg ~ "declined",
                          coverage == lag_cvg ~ "remained the same")) %>%
  mutate(decline_txt = ifelse(chng == 'declined', 1, 0),
         decline_txt_sum = sum(decline_txt, na.rm=T),
         decline_txt_sum = ifelse(decline_txt_sum == 1, paste0(decline_txt_sum, ' vaccine'), paste0(decline_txt_sum, ' vaccines')))

# rev_yr compared to rev_yr-1
df_txt_y1y2 <- df %>%
  filter(year %in% c(rev_yr-1, rev_yr)) %>%
  group_by(vaccine) %>%
  mutate(lag_cvg = lag(coverage)) %>%
  ungroup() %>%
  mutate(chng = case_when(coverage > lag_cvg ~ "increased",
                          coverage < lag_cvg ~ "declined",
                          coverage == lag_cvg ~ "remained the same")) %>%
  mutate(decline_txt = ifelse(chng == 'declined', 1, 0),
         decline_txt_sum = sum(decline_txt, na.rm=T),
         decline_txt_sum = ifelse(decline_txt_sum == 1, paste0(decline_txt_sum, ' vaccine'), paste0(decline_txt_sum, ' vaccines')))

# rev_yr compared to 2019
df_txt_y0y2 <- df %>%
  filter(year %in% c(comp_yr, rev_yr)) %>%
  arrange(vaccine, year) %>%
  group_by(vaccine) %>%
  mutate(lag_cvg = lag(coverage)) %>%
  ungroup() %>%
  mutate(chng = case_when(coverage > lag_cvg ~ str_glue("and was higher than in {comp_yr}"),
                          coverage < lag_cvg ~ str_glue("and was lower than in {comp_yr}"),
                          coverage == lag_cvg ~ str_glue("and was the same as in {comp_yr}"))) %>%
  mutate(decline_txt = ifelse(grepl('lower', chng), 1, 0),
         decline_txt_sum = sum(decline_txt, na.rm=T),
         decline_txt_sum = ifelse(decline_txt_sum == 1, paste0(decline_txt_sum, ' vaccine'), paste0(decline_txt_sum, ' vaccines')))

# identify if dtp1 coverage was the same all years 2019-rev_yr
same_cvg <- df_txt %>%
  filter(vaccine == 'dtp1') %>%
  mutate(n = n_distinct(coverage)) %>%
  pull(n) %>%
  unique()

# note :: the first sentence is in the translation table
# text for if dtp1 coverage was thw same in all years
if (same_cvg == 1) {
  txt <- str_glue("In {rev_yr-1}, {unique(df_txt_y0y1$decline_txt_sum)} had lower coverage than in {comp_yr}.\n
In {rev_yr}, {unique(df_txt_y0y2$decline_txt_sum)} had lower coverage than in {comp_yr}.\n
In {rev_yr}, {unique(df_txt_y1y2$decline_txt_sum)} had lower coverage than in {rev_yr-1}.")
  
} else {
  txt <- str_glue("DTP1 coverage {df_txt_y0y1 %>% filter(year==max(year) & vaccine=='dtp1') %>% pull(chng) %>% unique()} between {comp_yr} ({df_txt %>% filter(vaccine=='dtp1' & year==comp_yr) %>% pull(coverage) %>% unique()}%) and {rev_yr-1} ({df_txt %>% filter(vaccine=='dtp1' & year==rev_yr-1) %>% pull(coverage) %>% unique()}%). DTP1 coverage {df_txt_y1y2 %>% filter(year==rev_yr & vaccine=='dtp1') %>% pull(chng) %>% unique()} in {rev_yr} ({df_txt %>% filter(vaccine=='dtp1' & year==rev_yr) %>% pull(coverage) %>% unique()}%) compared to {rev_yr-1}, {df_txt_y0y2 %>% filter(year==rev_yr & vaccine=='dtp1') %>% pull(chng) %>% unique()}. In {comp_yr}-{rev_yr}, DTP1 coverage was at it's lowest level in {df_txt %>% filter(vaccine=='dtp1') %>% filter(coverage==min(coverage)) %>% pull(year) %>% unique() %>% knitr::combine_words()} ({df_txt %>% filter(vaccine=='dtp1') %>% filter(coverage==min(coverage)) %>% pull(coverage) %>% unique()}%).\n
In {rev_yr-1}, {unique(df_txt_y0y1$decline_txt_sum)} had lower coverage than in {comp_yr}.\n
In {rev_yr}, {unique(df_txt_y0y2$decline_txt_sum)} had lower coverage than in {comp_yr}.\n
In {rev_yr}, {unique(df_txt_y1y2$decline_txt_sum)} had lower coverage than in {rev_yr-1}.")
}

txt_dumbell <- txt