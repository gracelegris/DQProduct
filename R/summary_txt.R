###### summary_txt ######
region_names_long <- c(
  EAPR = str_glue("East Asia and the Pacific ({regn_txt})"),
  ECAR = str_glue("Europe and Central Asia ({regn_txt})"),
  ESAR = str_glue("Eastern and Southern Africa ({regn_txt})"),
  LACR = str_glue("Latin America and the Caribbean ({regn_txt})"),
  MENA = str_glue("Middle East and North Africa ({regn_txt})"),
  ROSA = str_glue("South Asia ({regn_txt})"),
  WCAR = str_glue("West and Central Africa ({regn_txt})")
)

if (regn %in% names(region_names_long)) {
  reg_name_long <- region_names_long[regn]
} else {
  reg_name_long <- regn
}

# vaccine coverage and unvaccinated change
df_txt_sum1 <- wuenic_dta %>%
  filter(lvl_1 == "country",
         vaccine %in% c("dtp1","dtp3","mcv1","mcv2"),
         year %in% c(rev_yr-1, rev_yr)) %>%
  select(country, vaccine, year, coverage, unvaccinated, unvaccinated_lbl) %>%
  arrange(country, vaccine, year) %>%
  group_by(country, vaccine) %>%
  # lag values
  mutate(coverage_lag = lag(coverage),
         unvaccinated_lag = lag(unvaccinated),
         unvaccinated_lbl_lag = lag(unvaccinated_lbl)) %>%
  ungroup() %>% 
  # filter for latest year (and prev year's values)
  filter(year == rev_yr) %>%
  # type of unvaccinated based on vaccine
  mutate(unvacc_type = case_when(vaccine == "dtp1" ~ "zero-dose",
                                 vaccine == "dtp3" ~ "un- and under-vaccinated",
                                 vaccine == "mcv1" ~ "unvaccinated",
                                 vaccine == "mcv2" ~ "un- and under-vaccinated",),
         # difference in coverage between rev_yr and rev_yr-1
         cvg_diff = coverage - coverage_lag,
         # text for coverage change
         cvg_change_txt = case_when(cvg_diff > 0 ~ str_glue("increased {cvg_diff} percentage points from {coverage_lag}% in {rev_yr-1} to {coverage}% in {rev_yr}"),
                                    cvg_diff < 0 ~ str_glue("declined {abs(cvg_diff)} percentage points from {coverage_lag}% in {rev_yr-1} to {coverage}% in {rev_yr}"),
                                    cvg_diff == 0 ~ str_glue("remained constant at {coverage}% between {rev_yr-1} and {rev_yr}")),
         # difference in number unvaccinated between rev_yr and rev_yr-1
         unvacc_diff = unvaccinated - unvaccinated_lag,
         unvacc_diff_abs = abs(unvacc_diff),
         # direction of change in number unvaccinated
         unvacc_change = case_when(unvaccinated_lbl == unvaccinated_lbl_lag ~ "same", 
                                   unvaccinated_lbl != unvaccinated_lbl_lag & unvacc_diff > 0 ~ "more",
                                   unvaccinated_lbl != unvaccinated_lbl_lag & unvacc_diff < 0 ~ "less")
  ) %>%
  # label calculated values
  label_vals(unvacc_diff, "unvacc_diff_lbl") %>%
  label_vals(unvacc_diff_abs, "unvacc_diff_abs_lbl") %>%
  # fix labels where >1 million
  mutate(unvacc_diff_abs_lbl = case_when(unvacc_diff_abs >= 1000000 ~ str_glue("{as.character(round(unvacc_diff_abs / 1000000, 1))} million"),
                                         TRUE ~ unvacc_diff_abs_lbl)) %>%
  mutate(unvaccinated_lbl = gsub("m", " million", unvaccinated_lbl)) %>%
  mutate(unvacc_change_txt = case_when(unvacc_change == "more" ~ str_glue("there were {unvacc_diff_abs_lbl} more {unvacc_type} children"),
                                       unvacc_change == "less" ~ str_glue("there were {unvacc_diff_abs_lbl} fewer {unvacc_type} children"),
                                       TRUE ~ str_glue("there were approximately the same number of {unvacc_type} children")))

# dropout
df_txt_sum2 <- df_txt_sum1 %>%
  filter(vaccine %in% c("dtp3", "mcv2")) %>%
  select(country, vaccine_v2 = vaccine, unvacc_v2 = unvaccinated) %>%
  left_join(df_txt_sum1 %>% filter(vaccine %in% c("dtp1", "mcv1")) %>% select(country, vaccine, unvaccinated)) %>%
  distinct() %>%
  rename(vaccine_v1 = vaccine, 
         unvacc_v1 = unvaccinated) %>%
  filter(vaccine_v1 == "dtp1" & vaccine_v2 == "dtp3" | vaccine_v1 == "mcv1" & vaccine_v2 == "mcv2") %>%
  mutate(dropout = unvacc_v2 - unvacc_v1) %>%
  # label calculated values
  label_vals(dropout, "dropout_lbl") %>%
  mutate(dropout_lbl = case_when(dropout >= 1000000 ~ str_glue("{as.character(round(dropout / 1000000, 1))} million"),
                                 TRUE ~ dropout_lbl)) 

# number of countries accounting for over half of zd
df_txt_sum3 <- global_df %>%
  rbind(., wuenic_dta) %>%
  filter(vaccine %in% c('dtp1'),
         year == rev_yr,
         country %in% c(ctryn, regn, "Global")) %>%
  select(iso3c, country, vaccine, year, unvaccinated) %>%
  mutate(ctry_unvacc = ifelse(iso3c == x, unvaccinated, NA_real_)) %>%
  fill(ctry_unvacc, .direction = "updown") %>%
  # calculate % of global and regional zd the country accounts for
  mutate(unvacc_pcnt = round(ctry_unvacc / unvaccinated * 100, 1),
         unvacc_pcnt_lbl = case_when(unvacc_pcnt < 0.1 ~ "<0.1%",
                                     TRUE ~ str_glue("{as.character(unvacc_pcnt)}%")))


# hpvc (females) data for summary
df_txt_sum4 <- hpv %>%
  filter(lvl_1 == 'country',
         vaccine_code == 'PRHPVC_F') %>%
  select(iso3c:coverage) %>%
  distinct() %>%
  arrange(country, vaccine_code, year) %>%
  group_by(country, vaccine_code) %>%
  mutate(coverage_lag = lag(coverage)) %>%
  ungroup() %>%
  filter(year == hpv_rev_yr) %>%
  mutate(cvg_diff = coverage - coverage_lag,
         cvg_change = case_when(cvg_diff > 0 ~ str_glue("increased from {coverage_lag}% to {coverage}% in {rev_yr} due to improved programme performance"),
                                cvg_diff < 0 ~ str_glue("decreased from {coverage_lag}% to {coverage}% in {rev_yr}"),
                                TRUE ~ str_glue("remained constant at {coverage}% in {rev_yr}")))

# flags
no_data_mcv2 <- wuenic_dta %>% filter(vaccine == "mcv2")

# countries with hpv vaccine introduced (fully or partially) in 2024
countries_with_hpv_revyr <- wiise_hpv_intro_yrs %>%
  filter(year == rev_yr) %>%
  distinct() %>%
  # count number of countries with nationwide or partial intro, by year
  filter(nationwide == "yes" | partially == "yes") %>%
  pull(iso3c)



txt <- str_glue("• DTP1 coverage {df_txt_sum1 %>% filter(vaccine == 'dtp1') %>% pull(cvg_change_txt)}.\n
  • DTP3 coverage {df_txt_sum1 %>% filter(vaccine == 'dtp3') %>% pull(cvg_change_txt)}.\n
  • There were {df_txt_sum1 %>% filter(vaccine == 'dtp1') %>% pull(unvacc_change_txt)} in {rev_yr}. This leaves {df_txt_sum1 %>% filter(vaccine == 'dtp1') %>% pull(unvaccinated_lbl)} children without vaccination, vulnerable to vaccine-preventable diseases and a further {df_txt_sum2 %>% filter(vaccine_v1 == 'dtp1') %>% pull(dropout_lbl)} with incomplete protection.\n 
  • {ctryn} accounted for {df_txt_sum3 %>% filter(country == regn) %>% pull(unvacc_pcnt_lbl)} of zero-dose children in {reg_name_long} and {df_txt_sum3 %>% filter(country == 'Global') %>% pull(unvacc_pcnt_lbl)} of zero-dose children globally.\n
  • MCV1 coverage {df_txt_sum1 %>% filter(vaccine == 'mcv1') %>% pull(cvg_change_txt)}. There were {df_txt_sum1 %>% filter(vaccine == 'mcv1') %>% pull(unvaccinated_lbl)} children who missed out on the first measles vaccination.")

# text about mcv2
if (no_data(no_data_mcv2) == FALSE) {
  txt <- str_glue("{txt}\n
                            • MCV2 coverage {df_txt_sum1 %>% filter(vaccine == 'mcv2') %>% pull(cvg_change_txt)}.")
} else {
  txt <- str_glue("{txt}\n
                            • The second dose of measles-containing vaccine (MCV2) was not introduced.")
}

# text about hpv
if (no_hpv == FALSE) {
  txt <- str_glue("{txt}\n
                            • Last dose coverage of HPV vaccination (HPVc) among girls {df_txt_sum4 %>% pull(cvg_change)}.")
  
  # no hpv estimates, but the country has introduced
} else if (no_hpv == TRUE & (x %in% countries_with_hpv_revyr)) {
  txt <- str_glue("{txt}\n
                            • In {rev_yr}, there were no HPV vaccine coverage estimates.")
  
  # no hpv estimates as country has not introduced
} else {
  txt <- str_glue("{txt}\n
                            • The Human papillomavirus (HPV) vaccine was not introduced.")
}

txt_summary <- txt <- gsub("1 percentage points", "1 percentage point", txt)