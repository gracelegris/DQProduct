###### plt_mcv_cases_cvg :: measles cases and mcv coverage ######
## prep data ---
# country-specific measles cases data
who_measles_dta_iso <- who_measles_dta %>% 
  filter(iso3c == x,
         year <= rev_yr)

# mcv coverage data
mcv_dta <- wuenic_dta %>%
  filter(vaccine %in% c("mcv1","mcv2"),
         year >= min(who_measles_dta_iso$year)) %>%
  select(iso3c, year, vaccine, coverage) %>%
  # data label :: vaccine and coverage in latest year of reporting
  mutate(
    max_yr_cvg = ifelse(year == max(year), coverage, NA_real_),
    vaccine2 = ifelse(year == max(year), str_glue("{vaccine}: {max_yr_cvg}%"), NA_character_),
    vaccine2 = gsub(": NA%", "", vaccine2))

# years with any measles vaccine stockouts (national, subnational)
mcv_stockout <- wiise_stockouts %>%
  filter(iso3c == x,
         grepl("mcv|measles", tolower(vaccine)))

mcv_stockout_yr <- mcv_stockout %>%
  pull(year) %>%
  unique()

# years with any measles campaign data (national, subnational)
mcv_campaign <- wiise_sia %>% 
  filter(type == "measles",
         iso3c == x)

mcv_campaign_yr <- mcv_campaign %>% 
  pull(year) %>%
  unique()

# prep dfs :: cases and coverage
df1 <- temp_cases <- who_measles_dta_iso %>% 
  mutate(across(year:measles_incidence_rate_per_1000000_total_population, as.numeric)) %>%
  mutate(cases = measles_total_confirmed_measles_cases) %>%
  # flag years with mcv stockout
  mutate(year_chr = case_when(year %in% mcv_stockout_yr ~ str_glue("{as.character(year)}*"),
                              TRUE ~ as.character(year))) %>%
  # flag years with measles campaign
  mutate(year_chr = case_when(year %in% mcv_campaign_yr ~ str_glue("{year_chr}^"),
                              TRUE ~ year_chr))

df2 <- temp_cvg <- mcv_dta %>%
  # flag years with mcv stockout
  mutate(year_chr = case_when(year %in% mcv_stockout_yr ~ str_glue("{as.character(year)}*"),
                              TRUE ~ as.character(year))) %>%
  # flag years with measles campaign
  mutate(year_chr = case_when(year %in% mcv_campaign_yr ~ str_glue("{year_chr}^"),
                              TRUE ~ year_chr))

# get the factor to multiply coverage by to align y-axes
factor <- df1 %>% filter(cases == max(cases)) %>%
  mutate(val=cases/30)
factor <- factor$val
yscale <- factor*103


## plot ----
plt_mcv_cases_cvg <- gg <-
  ggplot() +
  # bars → map to legend
  geom_col(df1,
           mapping = aes(x = year_chr, y = cases, fill = "cases")) +
  # lines + points
  geom_line(df2,
            mapping = aes(x = year_chr, y = coverage * factor,
                          color = vaccine, group = vaccine),
            linewidth = 1) +
  geom_point(df2,
             mapping = aes(x = year_chr, y = coverage * factor,
                           color = vaccine, group = vaccine),
             size = 2) +
  geom_text(data = df1,
            mapping = aes(x = year_chr, y = cases,
                          label = scales::comma(cases)),
            color = "#0058AB",
            size = 3.3,
            vjust = -0.5) +
  
  # manual legend styling
  scale_fill_manual(
    values = c(cases = "#1CABE2"),
    labels = c(cases = "Measles cases"),
    name = ""
  ) +
  scale_color_manual(
    values = c("mcv1" = "#00833D",
               "mcv2" = "#002759"),
    labels = c("mcv1" = "MCV1 coverage",
               "mcv2" = "MCV2 coverage"),
    name = ""
  ) +
  scale_y_continuous(
    limits = c(0, yscale),
    labels = scales::comma,
    sec.axis = sec_axis(
      trans = ~ . / factor,
      name = get_text("vaccine_coverage", language),
      breaks = seq(0, yscale / factor, by = 10)
    )
  ) +
  geom_label_repel(
    data = df2 %>% group_by(vaccine) %>% filter(year == max(year)),
    aes(x = year_chr, y = coverage * factor,
        label = toupper(vaccine2), color = vaccine),
    nudge_x = 3,
    nudge_y = seq(-2, -1 * n_vax, length.out = n_vax),
    size = 3.3,
    show.legend = FALSE,
    direction = "y",
    hjust = 0,
    segment.size = 1,
    segment.colour = NA
  ) +
  labs(title = get_text("plt_mcv_cases_cvg_title", language),
       x = "",
       y = get_text("plt_mcv_cases_cvg_y", language),
       caption = get_text("plt_mcv_cases_cvg_cpt", language)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(size = 12, hjust = 0.5),
    legend.text = element_text(size = 11),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.border = element_rect(colour = "grey", fill = NA, size = 0.5),
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_line()
  )

# old plot
  # ggplot() +
  # geom_col(df1, mapping=aes(x = year_chr, y = cases), fill = "#1CABE2") +
  # geom_line(df2, mapping=aes(x = year_chr, y = coverage * factor, color = vaccine, 
  #                            group = vaccine), linewidth = 1) +
  # geom_point(df2, mapping=aes(x = year_chr, y = coverage * factor, color = vaccine, 
  #                             group = vaccine), size = 2) +
  # geom_text(data = df1,
  #           aes(x = year_chr, y = cases, label = scales::comma(cases)),
  #           color = "#0058AB",
  #           size = 3.3,
  #           vjust = -0.5) +
  # scale_color_manual(values = c("mcv1" = "#00833D", "mcv2" = "#002759")) +
  # # prioritises primary y-axis grid
  # scale_y_continuous(
  #   limits = c(0, yscale),
  #   labels = scales::comma,
  #   sec.axis = sec_axis(
  #     trans = ~ . / factor,
  #     name = get_text("vaccine_coverage", language),
  #     breaks = seq(0, yscale / factor, by = 10)
  #   )
  # ) +
  # geom_label_repel(data = df2 %>% group_by(vaccine) %>% filter(year == max(year)),
  #                  aes(x = year_chr, y = coverage * factor, label = toupper(vaccine2), color = vaccine),
  #                  nudge_x = 1.7, nudge_y = seq(-2, -1 * n_vax, length.out = n_vax),
  #                  size = 3.3,
  #                  show.legend = FALSE,
  #                  direction = "y",
  #                  hjust = 0,
  #                  segment.size = 1,
  #                  segment.colour = NA
  # ) +
  # labs(title = get_text("plt_mcv_cases_cvg_title", language),
  #      x = "",
  #      y = get_text("plt_mcv_cases_cvg_y", language),
  #      caption = get_text("plt_mcv_cases_cvg_cpt", language)) +
  # theme_minimal() +
  # theme(legend.position = "none",
  #       plot.title = element_text(size=12, hjust = 0.5),
  #       legend.text=element_text(size=11),
  #       legend.title=element_blank(),
  #       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
  #       axis.text.y = element_text(size=10),
  #       panel.border = element_rect(colour = "grey", fill=NA, size=0.5),
  #       panel.grid.major.x = element_line(),     # Show major x grid lines
  #       panel.grid.minor.x = element_blank(),    # Hide minor x grid lines
  #       panel.grid.major.y = element_line(),
  #       panel.grid.minor.y = element_line()
  # )   


## narrative ----
low_thrs <- 100

if (no_data(who_measles_dta_iso) == FALSE & no_data(measles_cases_revyr) == FALSE & sum(as.numeric(who_measles_dta_iso$measles_total_confirmed_measles_cases), na.rm=T) > 0) {
  
  # text
  df_txt1 <- temp_cases %>%
    select(iso3c, year, cases) %>%
    arrange(year) %>%
    mutate(lag_cases = lag(cases),
           chr_diff = case_when(cases > lag_cases ~ "more",
                                cases < lag_cases ~ "less",
                                cases == lag_cases ~ "same"),
           pcnt_diff = cases / lag_cases,
           pcnt_diff2 = case_when(chr_diff == "less" ~ (pcnt_diff - 1) * 100,
                                  chr_diff == "more" & pcnt_diff < 2 ~ (pcnt_diff - 1) * 100,
                                  TRUE ~ pcnt_diff),
           diff = case_when(chr_diff == "more" & lag_cases == 0 ~ str_glue("There were no confirmed cases in the previous year"),
                            chr_diff == "more" & pcnt_diff >= 2 ~ str_glue("The number of cases in {year} was {as.character(round(pcnt_diff2, 1))} times more cases than in {year-1}"),
                            chr_diff == "less" ~ str_glue("The number of cases in {year} was {as.character(abs(round(pcnt_diff2, 0)))}% lower than in {year-1}"),
                            chr_diff == "more" & pcnt_diff < 2 ~ str_glue("The number of cases in {year} was {as.character(abs(round(pcnt_diff2, 0)))}% more than in {year-1}"),
                            chr_diff == "same" ~ str_glue("The number of cases in {year} was the same as in {year-1}"),
                            TRUE ~ NA_character_))
  
  
  df_txt2 <- temp_cvg
  
  # number of vaccines with coverage >= 90%
  cvg_gt90 <- temp_cvg %>% filter(year == rev_yr) %>% 
    mutate(gt90 = ifelse(coverage >= 90, 1, 0),
           gt90 = sum(gt90))%>%
    pull(gt90) %>%
    unique()
  
  # total number of cases across all years
  total_cases <- sum(df_txt1$cases)
  # number of years with the same maximum number of cases
  one_yr_max_cases <- df_txt1 %>% filter(cases == max(cases)) %>% mutate(n_yr = n_distinct(year)) %>% pull(n_yr) %>% unique()
  # year of the highest number of cases
  yr_max_cases <- df_txt1 %>% filter(cases == max(cases)) %>% pull(year)
  # number of measles cases over the last 5 years
  cases_last5yrs <- df_txt1 %>% filter(year >= rev_yr - 4) %>% mutate(sum = sum(cases)) %>% pull(sum) %>% unique()
  
  # text for mcv2 coverage in rev_yr
  # mcv2 in rev_yr? yes or no
  mcv2_revyr <- temp_cvg %>% filter(year == rev_yr, vaccine == "mcv2")
  if (no_data(mcv2_revyr) == FALSE) {
    mcv2_revyr_txt <- str_glue(" and MCV2 coverage was {mcv2_revyr$coverage}%.")
  } else {
    mcv2_revyr_txt <- str_glue(". There was no MCV2 in {rev_yr}.")
  }
  
  # text
  if (total_cases > 0) {
    if (one_yr_max_cases == 1) {
      txt <- str_glue("In {rev_yr}, there was a total of {df_txt1 %>% filter(year == rev_yr) %>% pull(cases) %>% scales::comma()} confirmed measles cases in {ctryn}. In the same year, MCV1 coverage was {df_txt2 %>% filter(vaccine == 'mcv1' & year == rev_yr) %>% pull(coverage)}%{mcv2_revyr_txt}\n
{df_txt1 %>% filter(year == rev_yr) %>% pull(diff)} (n={df_txt1 %>% filter(year == rev_yr) %>% pull(lag_cases) %>% scales::comma()}).\n
The highest number of measles cases was reported in {df_txt1 %>% filter(cases == max(cases)) %>% pull(year) %>% knitr::combine_words()} (n={df_txt1 %>% filter(cases == max(cases)) %>% pull(cases) %>% unique() %>% scales::comma()}). In this year, MCV1 coverage was {df_txt2 %>% filter(vaccine == 'mcv1' & year == yr_max_cases) %>% pull(coverage)}%.")
      
    } else if (total_cases > 0 & one_yr_max_cases != 1) {
      # there is more than one year with the highest number of cases reported
      txt <- str_glue("In {rev_yr}, there was a total of {df_txt1 %>% filter(year == rev_yr) %>% pull(cases) %>% scales::comma()} confirmed measles cases in {ctryn}. In the same year, MCV1 coverage was {df_txt2 %>% filter(vaccine == 'mcv1' & year == rev_yr) %>% pull(coverage)}%{mcv2_revyr_txt}\n
{df_txt1 %>% filter(year == rev_yr) %>% pull(diff)} (n={df_txt1 %>% filter(year == rev_yr) %>% pull(lag_cases) %>% scales::comma()}).\n
The highest number of measles cases was reported in {df_txt1 %>% filter(cases == max(cases)) %>% pull(year) %>% knitr::combine_words()} (n={df_txt1 %>% filter(cases == max(cases)) %>% pull(cases) %>% unique() %>% scales::comma()}).")
      
    }
    
    # add note if country has not reported any measles cases over the last 5 years
    if (cases_last5yrs == 0) {
      txt <- str_glue("{txt}\n
                        {ctryn} has not reported any measles cases over the last 5 years ({rev_yr-4}-{rev_yr}).")
    }
  }
  
  
  # text for if country has never reported any cases
  if (total_cases == 0) {
    txt <- str_glue("{ctryn} has not reported any confirmed measles cases since {min(df_txt1$year)}.")
  }
  
  # text for if country reported stockouts
  if (no_data(mcv_stockout) == FALSE) {
    df_txt3 <- mcv_stockout %>%
      arrange(year) %>%
      # mutate(time = str_glue("{as.character(year)} [{tolower(national_subnational)}]"))
      mutate(time = str_glue("{as.character(year)}"))
    
    txt <- str_glue("{txt}\n
                        {ctryn} reported measles vaccine stockouts in {df_txt3 %>% pull(time) %>% knitr::combine_words()}.")
    
  }
  
  # text for if country reported measles vaccination campaigns
  if (no_data(mcv_campaign) == FALSE) {
    df_txt4 <- mcv_campaign %>%
      arrange(year) %>%
      select(iso3c, year, geoarea) %>%
      distinct() %>%
      group_by(year) %>%
      # mutate(time = str_glue("{as.character(year)} [{tolower(geoarea) %>% knitr::combine_words()}]")) %>%
      mutate(time = str_glue("{as.character(year)}")) %>%
      ungroup()
    
    txt <- str_glue("{txt}\n
                        There were measles-containing vaccine supplementary immunization activities/campaigns in {df_txt4 %>% pull(time) %>% knitr::combine_words()}.")
    
  }
}

txt_mcv_cases_cvg <- txt


## top line message ----
cases_rev  <- df_txt1 %>% filter(year == rev_yr) %>% pull(cases)
cases_prev <- df_txt1 %>% filter(year == rev_yr - 1) %>% pull(cases)
diff_txt   <- df_txt1 %>% filter(year == rev_yr) %>% pull(chr_diff)

mcv1_cov <- df_txt2 %>% filter(vaccine == "mcv1", year == rev_yr) %>% pull(coverage)
mcv2_cov <- df_txt2 %>% filter(vaccine == "mcv2", year == rev_yr) %>% pull(coverage)

# cases trend phrase
cases_phrase <- case_when(
  cases_rev == 0 ~ "reported no confirmed measles cases",
  diff_txt == "less" ~ str_glue("saw a decrease in measles cases compared with {rev_yr-1}"),
  diff_txt == "more" ~ str_glue("experienced an increase in measles cases compared with {rev_yr-1}"),
  diff_txt == "same" ~ "had a similar number of measles cases as the previous year",
  TRUE ~ str_glue("reported {scales::comma(cases_rev)} measles cases")
)


# final top-line message focused on trends
if (length(mcv2_cov) == 0) {
  tlm <- str_glue(
    "In {rev_yr}, {ctryn} {cases_phrase}, while MCV1 coverage was {mcv1_cov}%."
  )
  
} else {
  tlm <- str_glue(
    "In {rev_yr}, {ctryn} {cases_phrase}, while MCV1 coverage was {mcv1_cov}% and MCV2 coverage was {ifelse(length(mcv2_cov) == 0, 'not reported', mcv2_cov)}%."
  )
}

tlm_mcv_cases_cvg <- tlm