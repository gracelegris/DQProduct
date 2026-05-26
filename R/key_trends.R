###### plt_key_trends :: key indicator trends [dt1, dtp3, mcv1] :: line-chart ######
## prep data ----
df <- wuenic_dta %>%
  filter(vaccine %in% c('dtp1','dtp3','mcv1','mcv2')) %>%
  select(country, year, vaccine, coverage) %>%
  mutate(vaccine = toupper(vaccine)) %>%
  filter(!is.na(coverage)) 

vax <- c('DTP1','DTP3','MCV1','MCV2')
df$vaccine <- factor(df$vaccine, levels = vax)

cpt <- wuenic_src

# axis/labels text size
axistitle <- 11 
lblsize <- 3.5

# create label data in descending order
label_data <- df %>%
  group_by(vaccine) %>%
  filter(year == max(year)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(coverage), vaccine != "DTP1") %>%  # Prioritize DTP1 if tied
  mutate(
    label = str_glue("{vaccine}: {coverage}%"),
    rank_y = rev(seq_along(coverage)),
    fake_y = max(coverage) + 2 - rank_y
  )


## plot ----
plt_key_trends <- gg <-
  df %>% ggplot() +
  geom_line(aes(x=year, y=coverage, color=vaccine), lwd=1) +
  geom_hline(yintercept = 90, linetype='dashed', colour='darkgrey') +
  # data point for latest year
  geom_point(data = df %>% filter(year == rev_yr),
             mapping = aes(x=year, y=coverage, color=vaccine),
             size = 2.5,
             show.legend = F) +
  # data point for the first year of each vaccine
  geom_point(data = df %>% group_by(vaccine) %>% filter(year == min(year)),
             mapping = aes(x=year, y=coverage, color=vaccine),
             size = 2.5,
             show.legend = F) +
  scale_y_continuous(limits = c(0, 110), breaks = c(seq(0, 100, by = 20),90)) +
  scale_x_continuous(breaks = c(seq(2000,2015,5), 
                                2019:rev_yr),
                     limits = c(1999, rev_yr+3)) +
  labs(x = '', y = get_text("coverage", language),
       title = get_text("plt_key_trends_title", language),
       caption = str_glue(cpt)) +
  theme_minimal() + 
  theme(legend.position = 'bottom',
        # plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = axistitle),
        axis.text.y = element_text(size = axistitle),
        legend.text=element_text(size = 11),
        axis.title = element_text(size = 11),
        panel.grid.minor.x = element_blank(),
        legend.title=element_blank()) +
  geom_label_repel(data = label_data,
                   aes(x = year, y = coverage, label = label, color = vaccine),
                   nudge_x = 1,
                   nudge_y = label_data$nudge_y,
                   size = lblsize,
                   show.legend = FALSE,
                   direction = "y",
                   hjust = 0,
                   segment.size = 0,
                   segment.colour = NA) 


## narrative ----
df_txt_dta <- df

df_txt_dtp1 <- df_txt_dta %>% 
  filter(vaccine == "DTP1", year == rev_yr) 

df_txt_dtp3 <- df_txt_dta %>% 
  filter(vaccine == "DTP3", year == rev_yr) %>%
  mutate(progress = case_when(coverage > 90 ~ "exceeded the 90% target set for 2030",
                              coverage == 90 ~ "achieved the 90% target",
                              coverage %in% c(85:89) ~ "fell short of, but was close to the 90% target",
                              coverage < 85 ~ "was below the 90% target set for 2030"))

df_txt_mcv1 <- df_txt_dta %>% 
  filter(vaccine == "MCV1", year == rev_yr) %>%
  mutate(progress = case_when(coverage > 95 ~ "exceeded the 95% target",
                              coverage == 95 ~ "achieved the 95% target",
                              coverage %in% c(90:94) ~ "was below, but close to the 95% target",
                              coverage < 80 ~ "was below the 95% target"))

df_txt_mcv2 <- df_txt_dta %>% 
  filter(vaccine == "MCV2", year == rev_yr) %>%
  mutate(progress = case_when(coverage > 95 ~ "exceeded the 95% target",
                              coverage == 95 ~ "achieved the 95% target",
                              coverage %in% c(90:94) ~ "was below, but close to the 95% target",
                              coverage < 80 ~ "was below the 95% target"))

# number of vaccines by change status
df_txt_change <- df_txt_dta %>%
  arrange(vaccine, year) %>%
  group_by(vaccine) %>%
  mutate(lag_coverage = lag(coverage),
         change = case_when(coverage > lag_coverage ~ "increased",
                            coverage == lag_coverage ~ "same",
                            coverage < lag_coverage ~ "declined")) %>%
  ungroup() %>%
  filter(year == rev_yr) %>%
  mutate(increased = ifelse(change == "increased", 1, 0),
         declined = ifelse(change == "declined", 1, 0),
         same = ifelse(change == "same", 1, 0)) %>%
  group_by(change) %>%
  mutate(all_vaccines_in_group = str_c(unique(vaccine), collapse = ", ")) %>%
  ungroup() %>%
  mutate(increased = sum(increased),
         declined = sum(declined),
         same = sum(same))


# note :: the first and last sentences come from the translation table
txt_key_trends1 <- str_glue("In {rev_yr}, DTP1 coverage (a proxy for access to immunization services) was {df_txt_mcv1$coverage}%.\n
DTP3 coverage - a marker of how well countries are delivering immunization services to children - {df_txt_dtp3$progress}.")

if (no_data(df_txt_mcv2) == FALSE) {
  
  # countries with MCV2
  txt_key_trends2 <- str_glue("In {rev_yr}, MCV1 coverage {df_txt_mcv1$progress} and MCV2 coverage {df_txt_mcv2$progress}.\n
Between {rev_yr-1} and {rev_yr}, {unique(df_txt_change$increased)} vaccines increased coverage, {unique(df_txt_change$declined)} declined and {unique(df_txt_change$same)} remained the same.")
  
} else {
  # countries without MCV2
  txt_key_trends2 <- str_glue("In {rev_yr}, MCV1 coverage {df_txt_mcv1$progress} and MCV2 was not yet introduced.\n
                                Between {rev_yr-1} and {rev_yr}, {unique(df_txt_change$increased)} vaccines increased coverage, {unique(df_txt_change$declined)} declined and {unique(df_txt_change$same)} remained the same.")
}

