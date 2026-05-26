###### plt_sdg_trends :: SDG indicator trends :: line-chart ######
## prep data ----
sdg_hpv <- hpv %>%
  filter(vaccine_code == "PRHPVC_F") %>%
  mutate(vaccine = "hpv")


sdg <- wuenic_dta %>%
  filter(vaccine %in% c('dtp3','mcv2','pcv3')) %>%
  select(country, year, vaccine, coverage) %>%
  bind_rows(., sdg_hpv) %>%
  mutate(vaccine = toupper(vaccine)) %>%
  mutate(vaccine = ifelse(vaccine=='HPV', 'HPVc', vaccine)) %>%
  filter(!is.na(coverage)) 

sdg_vax <- c('DTP3','MCV2','PCV3','HPVc')
sdg$vaccine <- factor(sdg$vaccine, levels = sdg_vax)

# conditional caption based on if any sdg vaccines are missing
missing_vaccines <- setdiff(sdg_vax, sdg$vaccine)
missing_vaccines <- knitr::combine_words(missing_vaccines)

caption_text <- if (length(missing_vaccines) == 0) {
  cpt = get_text("plt_sdg_trends_cpt1", language)
} else {
  cpt = get_text("plt_sdg_trends_cpt2", language)
  
}

# axis/labels text size
axistitle <- 11 
lblsize <- 3.5

# create label data in descending order
label_data <- sdg %>%
  group_by(vaccine) %>%
  filter(year == max(year)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(coverage)) %>%
  mutate(
    label = str_glue("{vaccine} : {coverage}%"),
    rank_y = rev(seq_along(coverage)),  # rank from top to bottom
    fake_y = max(coverage) + 2 - rank_y  # staggered y to force descending display
  )


## plot ----
gg <-
  sdg %>% ggplot() +
  geom_line(aes(x=year, y=coverage, color=vaccine), lwd=1) +
  geom_hline(yintercept = 90, linetype='dashed', colour='darkgrey') +
  # data point for latest year
  geom_point(data = sdg %>% filter(year == rev_yr),
             mapping = aes(x=year, y=coverage, color=vaccine),
             size = 2.5,
             show.legend = F) +
  # data point for the first year of each vaccine
  geom_point(data = sdg %>% group_by(vaccine) %>% filter(year == min(year)),
             mapping = aes(x=year, y=coverage, color=vaccine),
             size = 2.5,
             show.legend = F) +
  scale_y_continuous(limits = c(0, 100), breaks = c(seq(0, 100, by = 20),90)) +
  scale_x_continuous(breaks = c(seq(2000,2015,5), 
                                2019:rev_yr),
                     limits = c(1999, rev_yr+3)) +
  labs(x = '', y = get_text("coverage", language),
       title = get_text("plt_sdg_trends_title", language),
       caption = str_glue(cpt)) +
  theme_minimal() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
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


# flag where a vaccine only has one datapoint (ie. was introduced in rev_yr)
n_yrs <- sdg %>% group_by(vaccine) %>% mutate(n = n_distinct(year)) %>% ungroup() %>% 
  mutate(flag = ifelse(min(n) == 1, 1, 0))

# if a vaccine only has one datapoint, then only show the latest year data label
# otherwise show the first year data label and latest year data label
if (unique(n_yrs$flag) == 1) {
  n_yrs <- n_yrs %>% filter(n == 1)
  
  gg <- gg 
  
} else {
  gg <- gg +
    geom_label_repel(
      data = sdg %>%
        group_by(vaccine) %>%
        filter(year == min(year)) %>%
        ungroup(),
      aes(
        x = year,
        y = coverage,
        label = coverage,
        color = vaccine          # border color
      ),
      fill = "white",            # label box fill
      size = lblsize,
      label.size = 0.4,          # thickness of border; adjust as needed
      segment.color = NA,        # removes the connecting line
      show.legend = FALSE
    )
}

plt_sdg_trends <- gg


## narrative ----
df_txt <- sdg %>%
  mutate(cvggt90 = ifelse(coverage >= 90, 1, 0)) %>%
  group_by(year) %>%
  mutate(cvggt90_sum = sum(cvggt90),
         cvggt90_sum = case_when(cvggt90_sum == 0 ~ "none",
                                 TRUE ~ as.character(cvggt90_sum))) %>%
  ungroup()

# note :: first two sentences come from translation table
txt <- str_glue("{ctryn} has {n_distinct(df_txt$vaccine)} out of the 4 SDG vaccines.\n
                      In {rev_yr}, {ctryn} had achieved at least 90% coverage of {df_txt %>% filter(year==rev_yr) %>% pull(cvggt90_sum) %>% unique} out of the 4 vaccines.")

txt <- gsub("4 out of the 4", "all 4 of the ", txt)
txt_sdg_trends <- txt


## tlm ----
# vaccines present in country
vax_present <- df_txt %>%
  filter(year == rev_yr) %>%
  pull(vaccine) %>%
  unique()

n_present <- length(vax_present)

# vaccines ≥90% in latest year
vax_90 <- df_txt %>%
  filter(year == rev_yr, coverage >= 90) %>%
  pull(vaccine)

n_90 <- length(vax_90)

vax_90_txt <- knitr::combine_words(vax_90)

# # number meeting 90% in previous year
# n_90_prev <- df_txt %>%
#   filter(year == rev_yr - 1, coverage >= 90) %>%
#   pull(vaccine) %>%
#   length()
# 
# # change in count
# diff_90 <- n_90 - n_90_prev
# 
# trend_phrase <- case_when(
#   diff_90 > 0 ~ "an improvement from the previous year",
#   diff_90 < 0 ~ "a decline from the previous year",
#   diff_90 == 0 ~ "the same number as in the previous year"
# )


# build message
tlm <- case_when(
  n_90 == 0 ~ str_glue(
    "In {rev_yr}, no SDG vaccines in {ctryn} met the 90% SDG coverage target."
  ),
  n_90 == n_present ~ str_glue(
    "In {rev_yr}, {ctryn} met the 90% SDG coverage target for all SDG vaccines."
  ),
  TRUE ~ str_glue(
    "In {rev_yr}, {ctryn} met the 90% SDG coverage target for {vax_90_txt}, but not for the remaining vaccines."
  )
)
