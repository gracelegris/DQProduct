###### plt_all_vax_line :: vaccine coverage :: line chart ######
## prep data ----
df_hpv <- hpv %>%
  filter(vaccine_code == "PRHPVC_F") %>%
  mutate(vaccine = "hpv") %>%
  select(-vaccine_code)

# wuenic indicators for bop
df <- wuenic_dta %>%
  filter(year %in% c(2000:rev_yr),
         vaccine %in% c('dtp3','pol3','mcv1','hepb3','rcv1','hib3','mcv2','rotac','pcv3','ipv1')) %>%
  bind_rows(df_hpv) %>%
  select(country:vaccine, coverage)

df <- df %>%
  arrange(vaccine, year) %>%
  mutate(vaccine2 = case_when(vaccine == 'dtp3' ~ 'DTP3',
                              vaccine == 'hepb3' ~ 'HepB3',
                              vaccine == 'hib3' ~ 'Hib3',
                              vaccine == 'ipv1' ~ 'IPV1',
                              vaccine == 'mcv1' ~ 'MCV1',
                              vaccine == 'mcv2' ~ 'MCV2',
                              vaccine == 'pcv3' ~ 'PcV3',
                              vaccine == 'pol3' ~ 'Polio3',
                              vaccine == 'rcv1' ~ 'Rubella',
                              vaccine == 'rotac' ~ 'RotaC',
                              vaccine == 'hpv' ~ 'HPVc',
                              TRUE ~ vaccine)) %>%
  filter(!is.na(coverage)) %>%
  # find latest year of reporting for each vaccine
  group_by(vaccine) %>%
  mutate(max_yr_vax = max(year),
         max_yr_cvg = ifelse(year == max(year), coverage, NA_real_)) %>%
  fill(max_yr_cvg, .direction="updown") %>%
  # data label :: vaccine and coverage in latest year of reporting
  mutate(
    # label :: vaccine and coverage in rev_yr
    vaccine2 = ifelse(year == max(year), str_glue("{vaccine2}: {max_yr_cvg}%"), NA_character_),
    vaccine2 = gsub(": NA%", "", vaccine2)) %>%
  fill(vaccine2, .direction = "updown") %>%
  filter(!is.na(coverage))

clrs <- c('dodgerblue2','#80BD41','magenta2',
          '#6A3D9A','#E31A1C','#FF7F00','#FFC20E',
          'green4','darkorchid3','slateblue1','#FB9A99')

# '#002759' # bop colour

n_vax <- n_distinct(df$vaccine)

# flag where a vaccine only has one datapoint (ie. was introduced in rev_yr)
n_yrs <- df %>% group_by(vaccine) %>% mutate(n = n_distinct(year)) %>% ungroup() %>% 
  mutate(flag = ifelse(min(n) == 1, 1, 0))

# create label data in descending order
label_data <- df %>%
  group_by(vaccine) %>%
  filter(year == max(year)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(coverage)) %>%
  mutate(
    label = str_glue("{vaccine2}"),
    rank_y = rev(seq_along(coverage)),  # rank from top to bottom
    fake_y = max(coverage) + 2 - rank_y  # staggered y to force descending display
  )


## plot ----
gg <-
  df %>%
  ggplot() +
  geom_line(df, mapping=aes(x=year, y=coverage, color=vaccine2), linewidth=1) +
  # data labels :: vaccine
  geom_label_repel(data = label_data,
                   aes(x = year, y = coverage, label = label, color = vaccine2),
                   nudge_x = 4,
                   nudge_y = label_data$nudge_y,
                   size = 3,
                   show.legend = FALSE,
                   direction = "y",
                   hjust = 0,
                   segment.size = 0,
                   segment.colour = NA) +
  theme_minimal() +
  scale_x_continuous(breaks = c(seq(2000,2015,5), comp_yr:rev_yr)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  labs(x='', y = get_text("coverage", language),
       title = get_text("plt_all_vax_line_title", language),
       caption = get_text("plt_all_vax_line_cpt", language)) +
  theme(legend.position = "none",
        plot.title = element_text(size=12, hjust = 0.5),
        legend.text=element_text(size=11),
        legend.title=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 11),       
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "grey", fill=NA, linewidth=0.5)) +
  scale_color_manual(name = '', values = clrs) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")

# if there is a vaccine with data for only one year (ie. introduced in rev_yr), then put a point because otherwise it doesn't show up
if (unique(n_yrs$flag) == 1) {
  n_yrs <- n_yrs %>% filter(n == 1)
  
  gg <- gg +
    geom_point(n_yrs, mapping=aes(x=year, y=coverage, color=vaccine2), size = 2)
  
} 

plt_all_vax_line <- gg


## narrative ----
func_txt <- function(df, y1, y2) {
  df %>%
    filter(year %in% c(y1, y2)) %>%
    group_by(vaccine) %>%
    mutate(n_yrs = n_distinct(year)) %>%
    ungroup() %>%
    filter(n_yrs > 1) %>%
    mutate(vaccine = case_when(vaccine == 'dtp3' ~ 'DTP3',
                               vaccine == 'hepb3' ~ 'HepB3',
                               vaccine == 'hib3' ~ 'Hib3',
                               vaccine == 'ipv1' ~ 'IPV1',
                               vaccine == 'mcv1' ~ 'MCV1',
                               vaccine == 'mcv2' ~ 'MCV2',
                               vaccine == 'pcv3' ~ 'PcV3',
                               vaccine == 'pol3' ~ 'Polio3',
                               vaccine == 'rcv1' ~ 'Rubella',
                               vaccine == 'rotac' ~ 'RotaC',
                               vaccine == 'hpv' ~ 'HPVc',
                               TRUE ~ toupper(vaccine))) %>%
    arrange(vaccine, year) %>%
    group_by(vaccine) %>%
    mutate(lag_cvg = lag(coverage)) %>%
    ungroup() %>%
    mutate(chng = case_when(coverage > lag_cvg ~ "increased",
                            coverage < lag_cvg ~ "decreased",
                            coverage == lag_cvg ~ "were the same")) %>%
    filter(year == rev_yr) %>%
    group_by(chng) %>%
    mutate(n = n(),
           n = paste0(n, ' vaccines ', chng)) %>%
    mutate(vaccine_list = paste(vaccine, collapse = ", "),
           vaccine_list = sub(",([^,]*)$", " and\\1", vaccine_list),
           n = paste0(n, " (", vaccine_list, ")")) %>%
    ungroup()
}

hpv_txt <- hpv %>%
  filter(vaccine_code == "PRHPVC_F") %>%
  mutate(vaccine = "hpv") %>%
  select(-vaccine_code)

# wuenic indicators for bop
df_txt <- wuenic_dta %>%
  filter(year %in% c(2000:rev_yr),
         vaccine %in% c('dtp3','pol3','mcv1','hepb3','rcv1','hib3','mcv2','rotac','pcv3','ipv1')) %>%
  bind_rows(hpv_txt) %>%
  select(country:vaccine, coverage) %>%
  mutate(vaccine = case_when(vaccine == 'dtp3' ~ 'DTP3',
                             vaccine == 'hepb3' ~ 'HepB3',
                             vaccine == 'hib3' ~ 'Hib3',
                             vaccine == 'ipv1' ~ 'IPV1',
                             vaccine == 'mcv1' ~ 'MCV1',
                             vaccine == 'mcv2' ~ 'MCV2',
                             vaccine == 'pcv3' ~ 'PcV3',
                             vaccine == 'pol3' ~ 'Polio3',
                             vaccine == 'rcv1' ~ 'Rubella',
                             vaccine == 'rotac' ~ 'RotaC',
                             vaccine == 'hpv' ~ 'HPVc',
                             TRUE ~ toupper(vaccine)))

df_txt <- df_txt %>%
  arrange(vaccine, year)

# df_txt <- dta %>% filter(!is.na(coverage)) %>% mutate(coverage = as.numeric(coverage))
comp_2019   <- func_txt(df_txt, comp_yr, rev_yr)
comp_lastyr <- func_txt(df_txt, rev_yr-1, rev_yr)

min_cvg_val <- df_txt %>% filter(year == rev_yr) %>% filter(coverage == min(coverage, na.rm=T)) %>% pull(coverage) %>% unique()
max_cvg_val <- df_txt %>% filter(year == rev_yr) %>% filter(coverage == max(coverage, na.rm=T)) %>% pull(coverage) %>% unique()


# text for if all vaccines had the same coverage in rev-yr
if (min_cvg_val == max_cvg_val) {
  txt <- str_glue("This chart shows trends in coverage of {n_distinct(df_txt$vaccine) %>% unique()} vaccines (complete series).\n
                      In {rev_yr}, all childhood vaccines in the schedule had achieved {min_cvg_val}% coverage.\n
                      Coverage of {comp_2019 %>% pull(n) %>% unique() %>% knitr::combine_words()} as in {comp_yr}.\n
                      Coverage of {comp_lastyr %>% pull(n) %>% unique() %>% knitr::combine_words()} as in {rev_yr-1}.")
  
} else {
  txt <- str_glue("This chart shows trends in coverage of {n_distinct(df_txt$vaccine) %>% unique()} vaccines (complete series).\n
                      In {rev_yr}, {df_txt %>% filter(year==rev_yr) %>% filter(coverage==min(coverage, na.rm=T)) %>% pull(vaccine) %>% knitr::combine_words()} had the lowest coverage of all vaccines ({df_txt %>% filter(year==rev_yr) %>% filter(coverage==min(coverage, na.rm=T)) %>% pull(coverage) %>% unique()}%), followed by {df_txt %>% filter(year==rev_yr) %>% filter(coverage!=min(coverage, na.rm=T)) %>% filter(coverage==min(coverage, na.rm=T)) %>% pull(vaccine) %>% knitr::combine_words()} ({df_txt %>% filter(year==rev_yr) %>% filter(coverage!=min(coverage, na.rm=T)) %>% filter(coverage==min(coverage, na.rm=T)) %>% pull(coverage) %>% unique()}%).\n
                      Coverage of {comp_2019 %>% pull(n) %>% unique() %>% knitr::combine_words()} compared to respective coverage in {comp_yr}.\n
                      Coverage of {comp_lastyr %>% pull(n) %>% unique() %>% knitr::combine_words()} compared to respective coverage in {rev_yr-1}.")
}

# clean up text
txt <- gsub(" 1 vaccines were"," 1 vaccine was", txt)
txt <- gsub(" 1 vaccines"," 1 vaccine", txt)
txt_all_vax_line <- txt