###### plt_hpv_cvg :: hpv :: line chart ######
## prep data :: hpv intro yr ----
# # year of introduction of 1 dose schedule (using wiise data)
# hpv_nround_1 <- wiise_schedule_hpv %>%
#   filter(nrounds == 1,
#          !is.na(targetpop)) %>%
#   group_by(iso3c) %>%
#   filter(year == min(year)) %>%
#   ungroup() %>%
#   filter(iso3c == x)

# year of introduction of or switch to 1 dose schedule
hpv_nround_1 <- hpv_dose_schedule  %>%
  mutate(iso3c = tolower(iso3code)) %>%
  filter(!is.na(note)) %>%
  mutate(note2 = case_when(grepl("switch", note) ~ "switch",
                           grepl("introduction", note) ~ "intro",
                           TRUE ~ NA_character_),
         year = str_extract_all(note, "\\d+"),
         year = as.numeric(year),
         note_translated = case_when(note2 == "switch" ~ get_text("plt_hpv_cvg_switch1d", language),
                                     note2 == "intro" ~ get_text("plt_hpv_cvg_intro1d", language),
                                     TRUE ~ NA_character_)) %>%
  filter(year <= rev_yr) %>%
  filter(iso3c == x)


## prep data :: hpv coverage ----
df_prep <- hpv %>%
  filter(lvl_1 == "country") %>%
  mutate(type = ifelse(grepl("15", vaccine_code), "HPV coverage by age 15", "HPV programme coverage"),
         type2 = ifelse(grepl("15", vaccine_code), "by15", "programme")) %>%
  filter(type2 == "programme") %>%
  mutate(
    highest_value = max(coverage, na.rm = TRUE),
    closest_multiple_of_10 = ceiling(highest_value / 10) * 10
  ) 

max_val <- df_prep %>% pull(closest_multiple_of_10) %>% unique()
n_intro <- hpv %>% filter(lvl_1 == "country") %>% mutate(n = n_distinct(iso3c)) %>% pull(n) %>% unique()


df <- df_prep %>% 
  mutate(type = case_when(type2 == "by15" ~ get_text("plt_hpv_cvg_by15", language),
                          type2 == "programme" ~ get_text("plt_hpv_cvg_programme", language)),
         vaccine = case_when(grepl("HPV1 F", vaccine) ~ get_text("hpv1_f", language),
                             grepl("HPV1 M", vaccine) ~ get_text("hpv1_m", language),
                             grepl("HPVc F", vaccine) ~ get_text("hpvc_f", language),
                             grepl("HPVc M", vaccine) ~ get_text("hpvc_m", language))) %>%
  # ensure "HPV programme coverage" comes first
  mutate(type = factor(type, levels = c(get_text("plt_hpv_cvg_programme", language), get_text("plt_hpv_cvg_by15", language)))) %>%
  # column for sex and vaccine
  mutate(sex = case_when(grepl("_F", vaccine_code) ~ "female",
                         grepl("_M", vaccine_code) ~ "male",
                         TRUE ~ NA_character_),
         sex_translated = case_when(grepl("_F", vaccine_code) ~ str_to_title(get_text("female_pl", language)),
                                    grepl("_M", vaccine_code) ~ str_to_title(get_text("male_pl", language)),
                                    TRUE ~ NA_character_),
         vaccine2 = case_when(grepl("HPV1", vaccine_code) ~ "HPV1",
                              grepl("HPVC", vaccine_code) ~ "HPVc",
                              TRUE ~ NA_character_))


## plot ----
gg <-
  df %>%
  ggplot() + 
  geom_line(df, mapping = aes(x = year, y = coverage, colour = vaccine2), lwd=1.1, 
            show.legend=T) +
  geom_point(df, mapping = aes(x = year, y = coverage, color = vaccine2),
             show.legend=F) +
  facet_grid(. ~ sex_translated) +
  # space below 0 for data labels
  scale_y_continuous(limits = c(-10, 110), breaks = c(seq(0, 100, by = 20))) +
  scale_x_continuous(breaks = c(seq(min(df$year),max(df$year),1)),
                     limits = c(min(df$year)-1, rev_yr+1)) +
  labs(y = get_text("coverage", language),
       x = "",
       title = get_text("plt_hpv_cvg_title", language),
       caption = unique(get_text("plt_hpv_cvg_cpt", language)))  +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        strip.text.x = element_text(size = 11),
        strip.text.y = element_text(size = 11),
        panel.border = element_rect(colour = "grey", fill=NA, size=0.5),
        panel.grid.minor.x = element_blank()  # hide minor x-axis grid lines
  ) +
  scale_colour_manual(values = c("red","#0058AB")) +
  # data labels  (last dose, labels below the line)
  geom_text(data = df %>% 
              filter(
                grepl('HPVC', vaccine_code)
              ),
            aes(year, coverage, label = coverage, color=vaccine2),
            vjust = 2, hjust = 0, size=2.75,
            show.legend = FALSE) + 
  # data labels for (first dose, labels above the line)
  geom_text(data = df %>% 
              filter(
                grepl('HPV1', vaccine_code)
              ),
            aes(year, coverage, label = coverage, color=vaccine2),
            vjust = -1.4, hjust = 0, size=2.75,
            show.legend = FALSE) +
  # zero-line
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey")

# if country introduced a 1-dose schedule indicate what year
if (no_data(hpv_nround_1) == FALSE) {
  first_yr <- hpv_nround_1 %>% pull(year)
  
  gg <-
    gg +
    geom_vline(xintercept = first_yr, linetype = "longdash", color = "grey") +
    annotate("text",
             x = hpv_nround_1$year,           # centered on the vertical line
             y = 110,                # top of y-axis
             label = hpv_nround_1$note_translated,
             hjust = 0.5,            # center-align
             size = 3,
             color = "grey30")
  
}

plt_hpv_cvg <- gg


## narrative ----
# plt_hpv_cvg_no_dta <- df_prep %>% pull(country) %>% unique()
hpv_prg_cvg <- df_prep %>% filter(vaccine_code %in% c('PRHPV1_F','PRHPVC_F') | country == "none")

# note :: the first sentences are in the translation table
txt <- str_glue("The first year of HPV programme coverage estimates in {ctryn} was {min(hpv_prg_cvg$year)}.\n
                In {max(hpv_prg_cvg$year)}, first dose (HPV1) programme coverage among girls was {hpv_prg_cvg %>% filter(year==max(hpv_prg_cvg$year), vaccine_code=='PRHPV1_F') %>% pull(coverage)}% and last dose (HPVc) programme coverage was {hpv_prg_cvg %>% filter(year==max(hpv_prg_cvg$year), vaccine_code=='PRHPVC_F') %>% pull(coverage)}%.\n
                ")

txt_hpv_cvg <- txt


## top line message ----
df_tlm <- df_prep %>%
  arrange(vaccine_code, year) %>%
  group_by(vaccine_code) %>%
  mutate(lag_cvg = lag(coverage),
         diff_txt = case_when(coverage > lag_cvg ~ "increased to",
                              coverage < lag_cvg ~ "declined to",
                              coverage == lag_cvg ~ "remained at",
                              TRUE ~ NA_character_)) %>%
  ungroup() %>%
  filter(year == hpv_rev_yr)

chng_girls <- df_tlm %>% filter(vaccine_code == "PRHPVC_F") %>% pull(diff_txt)
chng_boys <- df_tlm %>% filter(vaccine_code == "PRHPVC_M") %>% pull(diff_txt)

# top line message based on scenario for girls
if (nrow(df_tlm %>% filter(vaccine_code == "PRHPVC_F")) > 0 & !is.na(df_tlm %>% filter(vaccine_code == "PRHPVC_F") %>% pull(diff_txt))) {
  # girls for more than one year
  tlm <- str_glue("In {rev_yr}, HPV last dose coverage among girls {df_tlm %>% filter(vaccine_code == 'PRHPVC_F') %>% pull(diff_txt)} {df_tlm %>% filter(vaccine_code == 'PRHPVC_F') %>% pull(coverage)}%")
  
} else if (nrow(df_tlm %>% filter(vaccine_code == "PRHPVC_F")) > 0 & is.na(df_tlm %>% filter(vaccine_code == "PRHPVC_F") %>% pull(diff_txt))) {
  # girls (one year only)
  tlm <- str_glue("In {rev_yr}, HPV last dose coverage among girls stood at {df_tlm %>% filter(vaccine_code == 'PRHPVC_F') %>% pull(coverage)}%")
  
} else {
  tlm <- str_glue("")
}

# top line message based on scenario for boys
if (nrow(df_tlm %>% filter(vaccine_code == "PRHPVC_M")) > 0 & !is.na(df_tlm %>% filter(vaccine_code == "PRHPVC_M") %>% pull(diff_txt))) {
  # boys for more than one year
  tlm <- str_glue("{tlm} and coverage among boys {df_tlm %>% filter(vaccine_code == 'PRHPVC_M') %>% pull(diff_txt)} {df_tlm %>% filter(vaccine_code == 'PRHPVC_M') %>% pull(coverage)}%")

} else if (nrow(df_tlm %>% filter(vaccine_code == "PRHPVC_M")) > 0 & is.na(df_tlm %>% filter(vaccine_code == "PRHPVC_M") %>% pull(diff_txt))) {
 # boys (one year only)
  tlm <- str_glue("{tlm} and coverage among boys stood at {df_tlm %>% filter(vaccine_code == 'PRHPVC_M') %>% pull(coverage)}%")
  
} else {
  # no data for boys
  tlm <- str_glue("")
}

# add note if there is one
if (nrow(hpv_nround_1) > 0) {
  tlm <- str_glue("{tlm}, following {tolower(hpv_nround_1$note)}.")
  
} else {
  tlm <- str_glue("{tlm}.")
}

tlm_hpv_cvg <- tlm