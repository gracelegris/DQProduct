###### plt_all_vax_line_facet :: all recommended routine vaccines :: facet line-chart ######
## prep data ----
df <- wuenic_dta %>%
  filter(vaccine %in% c('bcg','dtp1','dtp3','mcv1','mcv2','ipv1','pol3','pcv3','rotac','yfv')) %>%
  select(country, year, vaccine, coverage) %>%
  # rbind(., hpv) %>%
  left_join(wuenic_stockouts %>% filter(iso3c == x) %>% select(vaccine, year, any_stockout)) %>%
  mutate(any_stockout = ifelse(is.na(any_stockout), 0, any_stockout)) %>%
  mutate(vaccine = toupper(vaccine)) %>%
  mutate(vaccine = ifelse(vaccine=='HPV', 'HPVc', vaccine)) %>%
  filter(!is.na(coverage)) %>%
  # specify order
  mutate(reg_order = factor(country, levels = c("Global", sort(setdiff(unique(country), "Global")))))


## plot ----
# plt_all_vax_line_facet <- gg <-
  df %>% ggplot() +
  # coverage trend line
  geom_line(aes(x=year, y=coverage), lwd=1, colour = "#1CABE2") +
  # add coloured points indicating if there was a stockout
  geom_point(
    aes(x=year, y=coverage, colour = factor(any_stockout,
                        levels = c(0, 1),
                        labels = c("No stockout", "Any stockout"))),
    size = 1
  ) +
  scale_y_continuous(limits = c(0, 110), breaks = c(seq(0, 100, by = 20))) +
  scale_x_continuous(breaks = c(seq(2000,2015,5), 
                                2019, rev_yr-1, rev_yr),
                     limits = c(1998, rev_yr+2)) +
  scale_colour_manual(
    values = c("No stockout" = "#1CABE2",
               "Any stockout" = "red"),
    name = NULL
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 3))
  ) +
  labs(x = '', y = get_text("coverage", language),
       title = get_text("plt_all_vax_line_facet_title", language),
       caption = get_text("plt_all_vax_line_facet_cpt", language)) +
  facet_wrap(. ~ vaccine) +
  geom_text(data = df %>% group_by(vaccine) %>%
              filter(year == min(year)) %>%
              ungroup(),
            aes(year, coverage, label = coverage),
            colour = "black",
            vjust = -0.3, hjust=1.4, size=2.7,
            show.legend = FALSE) +
  geom_text(data = df %>% group_by(vaccine) %>%
              filter(year == rev_yr) %>%
              ungroup(),
            aes(year, coverage, label = coverage),
            colour = "black",
            vjust = -0.3, hjust=-0.3, size=2.7,
            show.legend = FALSE) +
  geom_text(data = df %>% group_by(vaccine) %>%
              filter(year %in% c(comp_yr, rev_yr-1)) %>%
              ungroup(),
            aes(year, coverage, label = coverage),
            colour = "black",
            vjust = -1, hjust=0.5, size=2.7,
            show.legend = FALSE) +
  theme_minimal() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8.5),
        axis.text.y = element_text(size = 9),
        legend.text=element_text(size = 11),
        axis.title = element_text(size = 11),
        panel.grid.minor.x = element_blank(),
        legend.title=element_blank()) 


## narrative ----
# function
func_txt <- function(df, y1, y2) {
  df %>%
    filter(year %in% c(y1, y2)) %>%
    arrange(vaccine, year) %>%
    group_by(vaccine) %>%
    mutate(lag_cvg = lag(coverage)) %>%
    ungroup() %>%
    mutate(chng = case_when(coverage > lag_cvg ~ "increased",
                            coverage < lag_cvg ~ "decreased",
                            coverage == lag_cvg ~ "remained constant")) %>%
    filter(year == rev_yr) %>%
    filter(!is.na(chng)) %>%
    group_by(chng) %>%
    mutate(n = n(),
           n = paste0(n, ' vaccines ', chng)) %>%
    mutate(vaccine_list = paste(vaccine, collapse = ", "),
           vaccine_list = sub(",([^,]*)$", " and\\1", vaccine_list),
           n = paste0(n, " (", vaccine_list, ")")) %>%
    ungroup()
}

# text data
df_txt <- df

# flag if min coverage is the same as max coverage
min_max_same <- df_txt %>% filter(year==rev_yr) %>%
  mutate(n_cvg = n_distinct(coverage)) %>%
  pull(n_cvg) %>%
  unique()


comp_2019   <- func_txt(df_txt, comp_yr, rev_yr)
comp_lastyr <- func_txt(df_txt, rev_yr-1, rev_yr)

# note :: the first sentence is in the translation table
if (min_max_same == 1) {
  txt <- str_glue("In {rev_yr}, all vaccines had the same coverage ({df_txt %>% filter(year==rev_yr) %>% pull(coverage) %>% unique()}%).\n
                      Compared to {comp_yr}, coverage of {comp_2019 %>% pull(n) %>% unique() %>% knitr::combine_words()}.\n
                      Compared to {rev_yr-1}, coverage of {comp_lastyr %>% pull(n) %>% unique() %>% knitr::combine_words()}.")
  
} else {
  txt <- str_glue("In {rev_yr}, {df_txt %>% filter(year==rev_yr) %>% filter(coverage==min(coverage)) %>% pull(vaccine) %>% knitr::combine_words()} had the lowest coverage ({df_txt %>% filter(year==rev_yr) %>% filter(coverage==min(coverage)) %>% pull(coverage) %>% unique()}%), followed by {df_txt %>% filter(year==rev_yr) %>% filter(coverage!=min(coverage)) %>% filter(coverage==min(coverage)) %>% pull(vaccine) %>% knitr::combine_words()} ({df_txt %>% filter(year==rev_yr) %>% filter(coverage!=min(coverage)) %>% filter(coverage==min(coverage)) %>% pull(coverage) %>% unique()}%).\n
                      Compared to {comp_yr}, coverage of {comp_2019 %>% pull(n) %>% unique() %>% knitr::combine_words()}.\n
                      Compared to {rev_yr-1}, coverage of {comp_lastyr %>% pull(n) %>% unique() %>% knitr::combine_words()}.")
}

txt <- gsub("1 vaccines", "one vaccine", txt)
txt_all_vax_line_facet <- txt


## topline message ----
df_rev <- df_txt %>% filter(year == rev_yr)

# case 1: all vaccines same coverage
if (n_distinct(df_rev$coverage) == 1) {
  
  tlm <- str_glue(
    "In {rev_yr}, all reported routine vaccines in {ctryn} had the same coverage ({unique(df_rev$coverage)}%)."
  )
  
} else {
  
  # lowest coverage vaccine(s)
  low_vax <- df_rev %>% filter(coverage == min(coverage))
  low_name <- low_vax %>% pull(vaccine) %>% knitr::combine_words()
  low_val  <- unique(low_vax$coverage)
  
  # remaining range
  range_vals <- df_rev %>%
    filter(coverage != min(coverage)) %>%
    summarise(min = min(coverage), max = max(coverage))
  
  range_txt <- if (range_vals$min == range_vals$max) {
    paste0(range_vals$min, "%")
  } else {
    paste0(range_vals$min, "-", range_vals$max, "%")
  }
  
  # dominant direciton of change since 2019 
  dom_2019 <- comp_2019 %>%
    count(chng) %>%
    slice_max(n, with_ties = FALSE)
  
  txt_2019 <- str_glue(
    "{dom_2019$n} antigen{ifelse(dom_2019$n>1,'s','')} {dom_2019$chng} since {comp_yr}"
  )
  
  # dominant direction of change since last year 
  dom_last <- comp_lastyr %>%
    count(chng) %>%
    slice_max(n, with_ties = FALSE)
  
  txt_last <- str_glue(
    "{dom_last$n} antigen{ifelse(dom_last$n>1,'s','')} {dom_last$chng} since {rev_yr-1}"
  )
  
  # topline message
  tlm <- str_glue(
    "In {rev_yr}, {ctryn} had its lowest coverage in {low_name} ({low_val}%), ",
    "while most other routine vaccines clustered around {range_txt}; ",
    "{txt_2019}, and {txt_last}."
  )
}

tlm_all_vax_line_facet <- tlm



# # data for review year
# df_tlm <- df_txt %>% filter(year == rev_yr)
# 
# # lowest coverage vaccine
# low_vax <- df_tlm %>% filter(coverage == min(coverage))
# low_name <- low_vax %>% pull(vaccine) %>% knitr::combine_words()
# low_val  <- low_vax %>% pull(coverage) %>% unique()
# 
# # range of remaining vaccines
# range_vals <- df_tlm %>%
#   filter(coverage != min(coverage)) %>%
#   summarise(min = min(coverage), max = max(coverage))
# 
# range_txt <- if (range_vals$min == range_vals$max) {
#   paste0(range_vals$min, "%")
# } else {
#   paste0(range_vals$min, "–", range_vals$max, "%")
# }
# 
# # comparison summaries
# decl_2019 <- comp_2019 %>% filter(chng == "decreased") %>% pull(n) %>% unique()
# const_2024 <- comp_lastyr %>% filter(chng == "remained constant") %>% pull(n) %>% unique()
# 
# decl_2019_txt <- ifelse(length(decl_2019) == 0, "no vaccines declined", decl_2019)
# const_2024_txt <- ifelse(length(const_2024) == 0, "coverage changed for most vaccines", const_2024)
# 
# # topline message
# tlm <- str_glue(
#   "In {rev_yr}, {ctryn} had its lowest coverage in {low_name} ({low_val}%), ",
#   "while most other routine vaccines clustered around {range_txt}"
# )
