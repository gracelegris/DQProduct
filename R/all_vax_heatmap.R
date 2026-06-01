###### plt_all_vax_heatmap :: heatmap ######
# uses stockout data

CountryName <- .current_country
wuenic_master <- wuenic_master_current
wuenic_stockouts <- wuenic_stockouts_master %>% filter(iso3c == toupper(x))

## prep data :: heatmap ----
df <- wuenic_master %>%
  select(iso3c = ISOCountryCode, country = Country, vaccine = Vaccine, year = Year, coverage = WUENIC) %>%
  rbind(hpv %>% filter(vaccine_code == "PRHPVC_F") %>% mutate(vaccine = "hpvc") %>% select(iso3c, country, vaccine, year, coverage))

# join on any stockouts
df <- df %>%
  left_join(wuenic_stockouts, by = c("vaccine", "year")) %>%
  distinct() %>%
  mutate(coverage_lbl = case_when(any_stockout == 1 ~ str_glue("{as.character(coverage)}*"),
                                  TRUE ~ as.character(coverage)))

# set data labels and coverage category
df <- df %>%
  mutate(vaccine = case_when(vaccine == "hpvc" ~ "HPVc", TRUE ~ vaccine),
        # vaccine = case_when(vaccine == "hepbb" ~ "HepBB",
        #                      vaccine == "hepb3" ~ "HepB3",
        #                      vaccine == "hib3" ~ "Hib3",
        #                      vaccine == "rotac" ~ "RotaC",
        #                      vaccine == "menga" ~ "MengA",
        #                      vaccine == "hpvc" ~ "HPVc",
        #                      TRUE ~ toupper(vaccine)),
         cvg_cat = case_when(coverage < 60 ~ '<60',
                             coverage %in% c(60:69) ~ '60-69',
                             coverage %in% c(70:79) ~ '70-79',
                             coverage %in% c(80:89) ~ '80-89',
                             coverage %in% c(90:94) ~ '90-94',
                             coverage >=95 ~ '>=95',
                             TRUE ~ NA_character_),
         year_char = as.character(year)) %>% 
  rename(Coverage = coverage)

# vaccine order
df$vaccine <- factor(df$vaccine, levels = c("BCG", "HepBB", "DTP1", "DTP3", "Hib3", "HepB3", "PCVC", "RotaC", "POL3", "IPV1", "IPVC", "MCV1", "RCV1", "MCV2", "YFV", "MengA","HPVc"))

df_txt_size <- 4

# need to use this later
df_all_vax_heatmap <- df


## plot ----
plt_all_vax_heatmap <- gg <-
  df %>%
  ggplot(aes(x = year_char, y = vaccine, fill = Coverage)) +
  geom_tile(color = "white") +
  theme_minimal() +
  geom_text(data = df %>% filter(Coverage>=60), aes(label = paste0(as.character(coverage_lbl))), colour='white', vjust = 0.5, size=df_txt_size) + # Add data labels
  geom_text(data = df %>% filter(Coverage<60), aes(label = paste0(as.character(coverage_lbl))), colour='white', vjust = 0.5, size=df_txt_size) + # Add data labels
  # labs(x = '', y = '',
  #      title = get_text("plt_all_vax_heatmap_title", language),
  #      caption = get_text("plt_all_vax_heatmap_cpt", language)) +
  labs(title = paste("WHO/UNICEF Coverage Estimates, ", CountryName, ", ", min(df$year), "–", max(df$year), sep=""),
       x = "Year",
       y = "Vaccine",
       caption = paste0("Source: WHO/UNICEF Estimates of National Immunization Coverage, ", rev_yr, " revision.\n",
                        "Note: Stock information available from 2003.\n",
                        "An asterisk (*) indicates where there was a vaccine stockout at the national or subnational level.")) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits=rev) +  # reverse y-axis
  theme(strip.placement = "outside",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="bottom",
        legend.key.width = unit(1.2, "cm"),
        plot.title = element_text(size = 14, hjust = 0.5),
  ) +
  scale_fill_gradientn(
    colours = c('#B50800', '#E2231A', '#F26A21', '#FFC20E', '#80BD41', '#00833D') ,
    values = scales::rescale(c(0,57,67,77,87,94,100)),  # scales::rescale is used to normalize the values to the range [0, 1] because the values argument expects values between 0 and 1
    guide = "colorbar", limits=c(0,100),
    #name = get_text("plt_all_vax_heatmap_legend_title", language),
    breaks = c(0,59,69,79,89,100),  # Specify where the labels should appear
    labels = c("0", "60", "70", "80", "90","100")  # Labels to show at the specified
  ) 


# ## narrative ----
# df_txt <- df %>%
#   mutate(cvggt90 = ifelse(Coverage >= 90, 1, 0)) %>%
#   group_by(country, year) %>%
#   mutate(n_vax = n_distinct(vaccine),
#          cvggt90_sum = sum(cvggt90)) %>%
#   ungroup() %>%
#   select(country, year, n_vax, cvggt90_sum) %>%
#   distinct() %>%
#   mutate(cvggt90_pcnt = round((cvggt90_sum / n_vax) * 100, 0),
#          regn_cvggt90 = ifelse(cvggt90_sum > 0, 1, 0),
#          regn_cvggt90_sum = sum(regn_cvggt90)) %>%
#   group_by(cvggt90_pcnt) %>%
#   mutate(n_ctry = n_distinct(country)) %>%
#   ungroup() 
# 
# df_txt_cat <- df %>%
#   select(vaccine, year, Coverage, cvg_cat) %>%
#   arrange(vaccine, year) %>%
#   group_by(vaccine) %>%
#   mutate(lag_cvg = lag(Coverage),
#          chng = case_when(Coverage > lag_cvg ~ "increased",
#                           Coverage < lag_cvg ~ "decreased",
#                           Coverage == lag_cvg ~ "same"),
#          lag_cvg_cat = lag(cvg_cat)) %>%
#   ungroup() %>%
#   mutate(cvg_cat_min = str_extract(cvg_cat, "\\d{2}"),
#          lag_cvg_cat_min = str_extract(lag_cvg_cat, "\\d{2}"),
#          
#          cvg_cat_min = as.numeric(cvg_cat_min),
#          lag_cvg_cat_min = as.numeric(lag_cvg_cat_min),
#          
#          cvg_cat_chng =   case_when(cvg_cat_min > lag_cvg_cat_min ~ "increased",
#                                     cvg_cat_min < lag_cvg_cat_min ~ "decreased",
#                                     cvg_cat_min == lag_cvg_cat_min ~ "same"))
# 
# # earliest year of new vax intro
# df_txt_intro <- df %>%
#   group_by(vaccine) %>%
#   mutate(n = n_distinct(year)) %>%
#   ungroup() %>%
#   filter(n != max(n)) %>%
#   mutate(n_vax = n_distinct(vaccine),
#          coverage = paste0(as.character(Coverage),"%")) 
# 
# data_in_revr <- df_txt_intro %>% filter(year == rev_yr) %>%
#   pull(vaccine)
# 
# df_txt_intro <- df_txt_intro %>% filter(vaccine %in% data_in_revr)
# 
# # coverage range in rev_yr-10
# range_10yrs_ago <- wuenic_dta %>% 
#   filter(year == rev_yr-10) %>%
#   mutate(range = str_glue("Vaccine coverage ranged from {min(coverage)}% to {max(coverage)}%")) %>%
#   mutate(range = ifelse(min(coverage) == max(coverage), str_glue("All vaccines achieved {max(coverage)}% coverage"), range)) %>%
#   pull(range) %>%
#   unique()
# 
# # coverage range in rev_yr
# range_latest <- wuenic_dta %>% 
#   filter(year == rev_yr) %>%
#   mutate(range = str_glue("Vaccine coverage ranged from {min(coverage)}% to {max(coverage)}%")) %>%
#   mutate(range = ifelse(min(coverage) == max(coverage), str_glue("All vaccines achieved {max(coverage)}% coverage"), range)) %>%
#   pull(range) %>%
#   unique()
# 
# # stockouts
# temp_stockout <- wiise_stockouts %>% filter(iso3c == x, year == rev_yr)
# no_stockout <- no_data(temp_stockout)  # if table is empty, this will be TRUE (i.e no stockouts)
# 
# # note :: the first sentence come from the translation table 
# txt <- str_glue("In {rev_yr}, {df_txt %>% filter(year==rev_yr) %>% pull(cvggt90_sum)} out of {n_distinct(df$vaccine)} ({df_txt %>% filter(year==rev_yr) %>% pull(cvggt90_pcnt)}%) vaccines in the schedule achieved coverage of 90% or more. {range_latest}.\n  
# Since {min(df_txt_intro$year) %>% unique()}, estimates have been made for {unique(df_txt_intro$n_vax)} new vaccines. {df_txt_intro %>% filter(n==min(n)) %>% pull(vaccine) %>% unique() %>% knitr::combine_words()} is the newest vaccine reported ({df_txt_intro %>% filter(n==min(n)) %>% filter(year==min(year)) %>% pull(year) %>% unique()}), which achieved {df_txt_intro %>% filter(n==min(n)) %>% filter(year==rev_yr) %>% pull(coverage) %>% unique() %>% knitr::combine_words()} coverage in {rev_yr}.")
# 
# # add stock narrative if a country reported stockouts in rev_yr
# if (no_stockout == FALSE) {
#   txt <- str_glue("{txt}\n
#                         In {rev_yr}, {CountryName} reported stockouts of vaccines/supplies ({temp_stockout %>% filter(year == rev_yr) %>% pull(vaccine) %>% knitr::combine_words()}) (more information on slide 8).")
# } else {
#   txt <- str_glue("{txt}\n
#                         In {rev_yr}, {CountryName} did not report any stockouts of vaccines or supplies.")
# }
# 
# # fix text where no vaccines reached 90%
# txt <- gsub(" 0 out of", " none of the", txt)
# txt <- gsub(" \\(0\\%\\) ", " ", txt)
# # fix string so that where a number appears twice (eg. "11 out of 11"), replace with the string "all 11"
# txt <- gsub("(\\b\\d+\\b) out of \\1", "all \\1", txt) 
# txt_all_vax_heatmap <- txt
# 
# 
# ## top line message ----
# df_tlm <- df %>%
#   filter(year %in% c(rev_yr-1, rev_yr)) %>%
#   select(country, vaccine, year, Coverage) %>%
#   arrange(country, vaccine, year) %>%
#   mutate(lag_cvg = lag(Coverage),
#          chng = case_when(Coverage > lag_cvg ~ "increase",
#                           Coverage == lag_cvg ~ "same",
#                           Coverage < lag_cvg ~ "decrease")) %>%
#   filter(year == rev_yr) %>%
#   mutate(increase = ifelse(chng == "increase", 1, 0),
#          decrease = ifelse(chng == "decrease", 1, 0),
#          same = ifelse(chng == "same", 1, 0),
#          gt90 = ifelse(Coverage >= 90, 1, 0),
#          sum_increase = sum(increase),
#          sum_decrease = sum(decrease),
#          sum_same = sum(same),
#          sum_gt90 = sum(gt90))
# 
# tlm <- str_glue("In {rev_yr}, {unique(df_tlm$sum_increase)} antigens recorded higher coverage, and {unique(df_tlm$sum_gt90)} achieved at least 90%")
# 
# # text based on different scenarios
# # all increased and at least 1 >=90%
# if (unique(df_tlm$sum_increase) == nrow(df_tlm) & unique(df_tlm$sum_gt90) > 0) {
#   tlm <- 
#     str_glue( "Universal coverage gains were observed in {rev_yr}, with all {unique(df_tlm$sum_increase)} antigens increasing since {rev_yr-1} and {unique(df_tlm$sum_gt90)} achieving at least 90% coverage.")
#   
#   # all increased and none >=90%
# } else if (unique(df_tlm$sum_increase) == nrow(df_tlm) & unique(df_tlm$sum_gt90) == 0) {
#   tlm <- 
#     str_glue("Universal coverage gains were observed in {rev_yr}, with all {unique(df_tlm$sum_increase)} antigens increasing since {rev_yr-1}, however, none of the antigens achieved at least 90% coverage and further gains are needed to ensure all children are reached.")
#   
#   # all decreased and none >=90%
# } else if (unique(df_tlm$sum_decrease) == nrow(df_tlm) & unique(df_tlm$sum_gt90) == 0) {
#   tlm <- 
#     str_glue("Universal coverage reversals were observed in {rev_yr}, with all {unique(df_tlm$sum_decrease)} antigens declining since {rev_yr-1}, with none of the antigens achieving at least 90% coverage, indicating vast improvements are needed to ensure all children are reached.")
#   
#   # all decreased but at least one >=90%
# } else if (unique(df_tlm$sum_decrease) == nrow(df_tlm) & unique(df_tlm$sum_gt90) > 0) {
#   tlm <- 
#     str_glue("Universal coverage reversals were observed in {rev_yr}, with all {unique(df_tlm$sum_decrease)} antigens declining since {rev_yr-1}, however, {unique(df_tlm$sum_gt90)} achieved at least 90% coverage, indicating good reach of at least some antigens.")
#   
#   # all antigens remained the same and none >= 90
# } else if (unique(df_tlm$sum_decrease) == 0 & unique(df_tlm$sum_increase) == 0 & unique(df_tlm$sum_gt90) == 0) {
#   tlm <- 
#     str_glue("In {rev_yr}, coverage remained unchanged since {rev_yr-1} for all antigens, with none achieving at least 90% coverage, indicating limited progression and further gains are needed to ensure all children are reached.")
#   
#   # all antigens remained the same and at least one >= 90
# } else if (unique(df_tlm$sum_decrease) == 0 & unique(df_tlm$sum_increase) == 0 & unique(df_tlm$sum_gt90) > 0) {
#   tlm <- 
#     str_glue("In {rev_yr}, coverage remained unchanged since {rev_yr-1} for all antigens, with {spell_n(unique(df_tlm$sum_gt90))} achieving at least 90% coverage, indicating good reach of at least some antigens.")
#   
#   # mixture of increase/decrease/same
# } else {
#   # uses the spell_n function
#   tlm <- 
#     str_glue("In {rev_yr}, coverage increased for {spell_n(unique(df_tlm$sum_increase))} antigens, remained unchanged for {spell_n(unique(df_tlm$sum_same))}, and declined for {spell_n(unique(df_tlm$sum_decrease))}, indicating {ifelse(unique(df_tlm$sum_increase) > unique(df_tlm$sum_decrease), 'overall progress', 'limited improvement')} in routine immunization performance; {spell_n(unique(df_tlm$sum_gt90))} achieved at least 90% coverage.")
#   tlm <- gsub("zero", "none", tlm)
#   
#   # str_glue("In {rev_yr}, coverage increased for {unique(df_tlm$sum_increase)} antigens, remained unchanged for {unique(df_tlm$sum_same)}, and declined for {unique(df_tlm$sum_decrease)}, indicating {ifelse(unique(df_tlm$sum_increase) > unique(df_tlm$sum_decrease), 'overall progress', 'limited improvement')} in routine immunization performance; {unique(df_tlm$sum_gt90)} achieved at least 90% coverage.")
#   tlm <- gsub(" 0 ", " none ", tlm)
# }
# 
# 
# tlm_all_vax_heatmap <- tlm
