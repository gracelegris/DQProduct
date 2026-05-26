###### plt_vax_dropout_zd_cvg :: vaccinated, partially vaccinated and zero-dose, and DTP coverage, incl projections :: stacked bar chart ######
## prep data ----
# wpp target populations from 2023-2030
wpp_dta <- read.csv(str_glue('{utils}/WPP_denoms_WPP{wpp_rev_yr}.csv')) %>%
  filter(year > 2010) %>%
  # rename(iso3c = `?..iso3c`) %>%
  mutate(iso3c = tolower(iso3c)) %>%
  left_join(., reg_ref, by = join_by('iso3c')) %>%
  filter(!is.na(wuenic),
         target %in% c('si')) %>%
  select(-c(wuenic, region_zd, target))

wpp_dta_plt12d <- wpp_dta %>% 
  filter(!is.na(wpp_dta[, 7])) %>%
  filter(iso3c == x) %>%
  select(iso3c, year, value)


# calculate projected number of zero-dose up to 2030
# 2030 target to reduce the number of zero-dose children by half by 2030 (using baseline 2019, so zd 2030 = zd in 2019/2)
end_yr <- 2030
proj_years <- (rev_yr + 1):end_yr
n_steps <- end_yr - rev_yr

zd_proj <- wuenic_dta %>% 
  filter(lvl_1 == "country",
         vaccine == "dtp1",
         year %in% c(2019, rev_yr)) %>%
  select(iso3c, country, year, unvaccinated) %>%
  pivot_wider(names_from = year, values_from = unvaccinated) %>%
  mutate(
    target = `2019` / 2,
    factor = (target / .data[[as.character(rev_yr)]])^(1 / n_steps)
  )

# generate projections iteratively
for (yr in proj_years) {
  prev <- as.character(yr - 1)
  cur  <- as.character(yr)
  
  zd_proj[[cur]] <- zd_proj[[prev]] * zd_proj$factor
}

zd_proj <- zd_proj %>%
  select(-`2019`, -all_of(as.character(rev_yr)), -target, -factor) %>%
  pivot_longer(-c(iso3c, country), names_to = 'year', values_to = 'zerodose_mt') %>%
  mutate(year = as.numeric(year)) %>%
  left_join(., wpp_dta_plt12d) %>%
  rename(target = value) %>%
  mutate(dtp_mt = target-zerodose_mt) %>%
  select(-c(target)) %>%
  pivot_longer(-c(iso3c:year), names_to = 'measure', values_to = 'value')

# number of children vaccinated with dtp3 up to latest year
demo1 <- wuenic_dta %>%
  filter(lvl_1 == "country",
         vaccine %in% c('dtp3')) %>%
  select(iso3c, country, year, dtp3=vaccinated)

# number of zd children and target pop up to 2023
demo2 <- wuenic_dta %>%
  filter(lvl_1 == "country",
         vaccine %in% c('dtp1')) %>%
  select(iso3c, country, year, zerodose=unvaccinated, target)

demo3 <- left_join(demo1, demo2) %>%
  # calculate # partially vaccinated
  mutate(partial_dtp = target-zerodose-dtp3) %>%
  select(-c(target)) %>%
  pivot_longer(-c(iso3c:year), names_to = "measure", values_to = "value") %>%
  rbind(zd_proj) %>%
  mutate(proj = ifelse(year > rev_yr, 'Projection', 'Estimate')) %>%
  mutate(measure_translated = case_when(measure == 'dtp3' ~ get_text("plt_vax_dropout_zd_cvg_dtp_full", language),
                                        measure == 'zerodose' ~ get_text("plt_vax_dropout_zd_cvg_zerodose", language),
                                        measure == 'partial_dtp' ~ get_text("plt_vax_dropout_zd_cvg_partial_dtp", language),
                                        measure == 'zerodose_mt' ~ get_text("plt_vax_dropout_zd_cvg_zerodose_mt", language),
                                        measure == 'dtp_mt' ~ get_text("plt_vax_dropout_zd_cvg_dtp_mt", language))) %>%
  filter(year >= 2000)

demo3$measure_translated <- factor(demo3$measure_translated, levels = c(get_text("plt_vax_dropout_zd_cvg_dtp_full", language),
                                                                        get_text("plt_vax_dropout_zd_cvg_partial_dtp", language),
                                                                        get_text("plt_vax_dropout_zd_cvg_zerodose", language),
                                                                        get_text("plt_vax_dropout_zd_cvg_dtp_mt", language),
                                                                        get_text("plt_vax_dropout_zd_cvg_zerodose_mt", language)
))




cvg <- wuenic_dta %>%
  filter(lvl_1 == 'country',
         vaccine %in% c('dtp1','dtp3')) %>%
  mutate(measure = str_glue("{toupper(vaccine)} coverage")) %>%
  select(country, year, vaccine, measure, value = coverage)

# get the factor to multiply coverage by to align y-axes
zd_factor <- demo3 %>% 
  select(value) %>% arrange(desc(value)) %>% slice(which.max(1)) %>%
  mutate(value=value/50)

zd_factor <- zd_factor$value

yscale <- zd_factor*105

# use these for narrative as they are associated with this specific chart
plt12d_demo <- demo3  
plt12d_cvg <- cvg %>% 
  mutate(y_scaled = value * zd_factor,
         # translate measure
         measure_translated = paste0(toupper(vaccine), " ", get_text("coverage_no_sign", language))) 


## plot ----
plt_vax_dropout_zd_cvg <- gg <-
  plt12d_demo %>%
  ggplot(aes(
    x = year,
    y = value,
    fill = factor(measure_translated, levels = c(
      get_text("plt_vax_dropout_zd_cvg_dtp_full", language),
      get_text("plt_vax_dropout_zd_cvg_partial_dtp", language),
      get_text("plt_vax_dropout_zd_cvg_zerodose", language),
      get_text("plt_vax_dropout_zd_cvg_dtp_mt", language),
      get_text("plt_vax_dropout_zd_cvg_zerodose_mt", language)
    ))
  )) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity") +
  
  # Line chart
  geom_line(
    data = plt12d_cvg,
    aes(x = year, y = y_scaled, color = measure_translated, group = vaccine),
    size = 1,
    inherit.aes = FALSE
  ) +
  
  # Data labels for dtp1 (above line)
  geom_text(data = plt12d_cvg %>% 
              filter(grepl('dtp1', vaccine),
                     year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)), 
            aes(year, value * zd_factor, label = value, colour = measure_translated),
            vjust = -1.3, size = 3.4,
            show.legend = FALSE,
            inherit.aes = FALSE
  ) + 
  
  # Data labels for dtp3 (below line)
  geom_text(data = plt12d_cvg %>% 
              filter(grepl('dtp3', vaccine),
                     year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)), 
            aes(year, value * zd_factor, label = value, colour = measure_translated),
            vjust = 1.8, size = 3.4,
            show.legend = FALSE,
            inherit.aes = FALSE
  ) + 
  
  labs(title = paste(get_text("plt_vax_dropout_zd_cvg_title", language), ",", ctryn),
       y = paste0("# ", get_text("children", language)),
       x = "",
       caption = get_text("plt_vax_dropout_zd_cvg_cpt", language)) +
  
  scale_fill_manual(values = c("#80BD41","#1CABE2","#0058AB", "#FFC20E", "#F26A21"), name = "") +
  scale_colour_manual(values = c("#0058AB","#80BD41"), name = "") +
  
  scale_y_continuous(
    limits = c(0, yscale),
    labels = scales::comma,
    sec.axis = sec_axis(trans = ~ . / zd_factor, name = get_text("coverage", language))
  ) +
  
  scale_x_continuous(breaks = c(seq(2000,2030,5), 2019:rev_yr)) +
  
  guides(
    fill = guide_legend(nrow = 1, order = 1, override.aes = list(pattern = "none")),
    colour = guide_legend(nrow = 1, order = 2)
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",   # Stack legends vertically
    legend.title = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 9),
    legend.text = element_text(size = 11),
    panel.grid.minor.x = element_blank()  # Hide minor grid lines on x-axis
  )


## narrative ----
df_txt <- wpp_dta_plt12d %>%
  filter(year %in% c(rev_yr+1, 2030)) %>%
  pivot_wider(., names_from = year, values_from = value) %>%
  # column 3 - column 2
  mutate(diff = .[[3]] - .[[2]]) %>%
  arrange(desc(diff)) %>%
  mutate(diff_txt = case_when(diff < 0 ~ "decline",
                              diff > 0 & diff < 10000 ~ "small increase",
                              diff >= 10000 & diff < 50000 ~ "moderate increase",
                              diff >= 50000 ~ "large increase"))

# target to vaccinate with DTP1 in 2030 to reduce number of zd by half
target_vax_2030 <- plt12d_demo %>%
  arrange(country, measure, year) %>%
  filter(measure == "dtp_mt",
         year == 2030) %>%
  pull(value)

# calculate average annual percentage change in number of children vaccinated with DTP1 required to meet the target of reducing the number of zd in 2019 by half in 2030
df_txt2 <- wuenic_dta %>%
  filter(vaccine == "dtp1", 
         year == rev_yr) %>%
  select(lvl_1, iso3c, country, vaccine, year, vaccinated) %>%
  mutate(ia2030_target = target_vax_2030,
         ia2030_target_yr = 2030,
         years_to_target = ia2030_target_yr - year,
         # Calculate average annual percentage change (compound annual growth rate)
         aapc_required = ((ia2030_target / vaccinated)^(1 / years_to_target) - 1) * 100,
         aapc_required = round(aapc_required, 2),
         aaprc_required_size = case_when(aapc_required > 0 & aapc_required < 1 ~ "slightly more",
                                         aapc_required >= 1 & aapc_required < 5 ~ "more",
                                         aapc_required >= 5 ~ "considerably more",
                                         aapc_required <= 0 ~ "approximately the same"))


if (df_txt$diff_txt == "large increase" & df_txt2$aapc_required >= 2) {
  txt <- str_glue("IA2030 calls on all countries to reduce the number of zero dose children in 2019 by half by 2030. This chart shows the annual number of children required to be vaccinated to reach the ZD target.\n
                           {ctryn} is projected to have a  {df_txt$diff_txt} (>=50,000) in the number of surviving infants by 2030. Maintaining current coverage requires vaccinating an increasing number of children.\n 
                           To achieve the IA2030 ZD target, {df_txt2 %>% pull(aaprc_required_size)} (~{df_txt2 %>% pull(aapc_required)}%) children need to be vaccinated with DTP1 each year.\n
                           To improve coverage and reach the 2030 ZD target will require substantial increases in immunization programme and health system capacity.\n
                           ")
  
} else if (df_txt$diff_txt == "large increase" & df_txt2$aapc_required <2) {
  txt <- str_glue("IA2030 calls on all countries to reduce the number of zero dose children in 2019 by half by 2030. This chart shows the annual number of children required to be vaccinated to reach the ZD target.\n
                           {ctryn} is projected to have a  {df_txt$diff_txt} (10,000-50,000) in the number of surviving infants by 2030. Therefore, maintaining current coverage requires vaccinating an increasing number of children.\n 
                           ")
  
} else if (df_txt$diff_txt == "moderate increase") {
  txt <- str_glue("IA2030 calls on all countries to reduce the number of zero dose children in 2019 by half by 2030. This chart shows the annual number of children required to be vaccinated to reach the ZD target.\n
                           {ctryn} is projected to have a  {df_txt$diff_txt} (10,000-50,000) in the number of surviving infants by 2030. Therefore, maintaining current coverage requires vaccinating an increasing number of children.\n 
                           To achieve the IA2030 ZD target, {df_txt2 %>% pull(aaprc_required_size)} (~{df_txt2 %>% pull(aapc_required)}%) children need to be vaccinated with DTP1 each year. This will require increases in immunization programme and health system capacity.
                           ")
  
} else if (df_txt$diff_txt == "small increase") {
  txt <- str_glue("IA2030 calls on all countries to reduce the number of zero dose children in 2019 by half by 2030. This chart shows the annual number of children required to be vaccinated to reach the ZD target.\n
                           {ctryn} is projected to have a  {df_txt$diff_txt} (<10,000) in the number of surviving infants by 2030. Therefore, to achieve the IA2030 ZD target, {df_txt2 %>% pull(aaprc_required_size)} (~{df_txt2 %>% pull(aapc_required)}%) children need to be vaccinated with DTP1 each year.
                           ")    
  
} else if (df_txt$diff_txt == "decline" & df_txt2$aapc_required < 1) {
  txt <- str_glue("IA2030 calls on all countries to reduce the number of zero dose children in 2019 by half by 2030. This chart shows the annual number of children required to be vaccinated to reach the ZD target.\n
                           {ctryn} is projected to have a  {df_txt$diff_txt} in the number of surviving infants by 2030. To achieve the IA2030 ZD target, current efforts would be sufficient, however, countries must strengthen beyond the targets.
                           ")
  
} else if (df_txt$diff_txt == "decline" & df_txt2$aapc_required >= 1 & df_txt2$aapc_required <5) {
  txt <- str_glue("IA2030 calls on all countries to reduce the number of zero dose children in 2019 by half by 2030. This chart shows the annual number of children required to be vaccinated to reach the ZD target.\n
                           {ctryn} is projected to have a  {df_txt$diff_txt} in the number of surviving infants by 2030. To achieve the IA2030 ZD target, {df_txt2 %>% pull(aaprc_required_size)} (~{df_txt2 %>% pull(aapc_required)}%) children need to be vaccinated with DTP1 each year.
                           ")
  
} else if (df_txt$diff_txt == "decline" & df_txt2$aapc_required >= 5) {
  txt <- str_glue("IA2030 calls on all countries to reduce the number of zero dose children in 2019 by half by 2030. This chart shows the annual number of children required to be vaccinated to reach the ZD target.\n
                           {ctryn} is projected to have a  {df_txt$diff_txt} in the number of surviving infants by 2030. {df_txt2 %>% pull(aaprc_required_size)} (~{df_txt2 %>% pull(aapc_required)}%) children need to be vaccinated with DTP1 each year.
                           ")
}

txt_vax_dropout_zd_cvg <- txt


## top line message ----
if (df_txt$diff_txt == "decline" & df_txt2$aapc_required < 1) {
  tlm <- str_glue("{regn_txt} is on track to halve zero-dose children by 2030, but sustaining progress will require continued strengthening of immunization systems.")
} else {
  # tlm <- str_glue("{regn_txt} must vaccinate {df_txt2 %>% pull(aaprc_required_size)} children each year to achieve the IA20 goal of halving the number of zero-dose children by 2030, despite projected {df_txt$diff_txt} in surviving infants.")
  tlm <- str_glue("{df_txt2 %>% pull(aaprc_required_size) %>% str_to_sentence()} children need to be vaccinated each year to achieve the IA2030 goal of halving the number of zero-dose children by 2030, with a projected {df_txt$diff_txt} in surviving infants.")
}

tlm <- gsub("Slightly more", "More", tlm)
tlm_vax_dropout_zd_cvg <- tlm


## old code for above ----
# zd_proj <- wuenic_dta %>% 
# filter(lvl_1 == "country",
#        vaccine == "dtp1",
#        year %in% c(2019, rev_yr)) %>%
# select(iso3c, country, year, unvaccinated) %>%
# pivot_wider(., names_from = year, values_from = unvaccinated) %>%
# mutate(target = `2019`/2) %>%
# mutate(`2026` = `2025`*(target/`2025`)^(1/(2030-rev_yr)),
#        `2027` = `2026`*(target/`2025`)^(1/(2030-rev_yr)),
#        `2028` = `2027`*(target/`2025`)^(1/(2030-rev_yr)),
#        `2029` = `2028`*(target/`2025`)^(1/(2030-rev_yr)),
#        `2030` = `2029`*(target/`2025`)^(1/(2030-rev_yr))) %>%
# select(-c(`2019`,`2025`,target)) %>%