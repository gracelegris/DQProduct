###### plt_dropout :: drop-out :: line chart ######
## plot function ----
# function for plot
plot_dropout <- function(data, title, caption = NULL) {
  # Create label_y for country of interest at specific years
  data <- data %>%
    mutate(label_y = ifelse(
      country == ctryn & year %in% c(seq(min(year), 2015, 5), 2019, rev_yr-1, rev_yr),
      dropout + 3,  # Adjust offset as needed
      NA
    ))
  
  ggplot(data, aes(x = year, y = dropout)) + 
    geom_line(aes(colour = country), lwd = 1, alpha = 0.8, show.legend = TRUE) +
    
    # Emphasize country of interest
    geom_line(data = data %>% filter(country == ctryn), 
              colour = "black", lwd = 1.6) +
    geom_line(data = data %>% filter(country == ctryn), 
              aes(colour = country), lwd = 1) +
    
    # Reference line
    geom_hline(yintercept = 0, colour = 'grey', linetype = "dashed") +
    
    # Axis formatting
    scale_x_continuous(
      breaks = c(seq(min(data$year), 2015, 5), 2019, rev_yr - 1, rev_yr),
      limits = c(min(plot_df$year), rev_yr)
    ) +
    scale_colour_manual(values = unicef_colors) +
    scale_y_continuous(
      limits = c(min(dropout_all$dropout), max(dropout_all$dropout + 5))
    ) +
    
    # Label and connecting segment for country of interest
    geom_segment(data = data %>% filter(!is.na(label_y)),
                 aes(x = year, xend = year, y = dropout, yend = label_y),
                 linetype = "dotted", color = "#0058AB") +
    geom_label(data = data %>% filter(!is.na(label_y)),
               aes(x = year, y = label_y, label = dropout), 
               size = 2.8,
               colour = "black",
               show.legend = FALSE) +
    
    # Other country labels at final year
    geom_text(data = data %>% filter(country != ctryn, year == max(year)),
              aes(year, dropout, label = dropout, colour = country), 
              hjust = -1, size = 3, show.legend = FALSE) +
    
    # Themes
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, size = 11, vjust = 0.5, hjust = 1),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      plot.title = element_text(size = 12, hjust = 0.5),
      legend.title = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom"
    ) +
    
    # Labels
    labs(
      title = title,
      x = "",
      y = get_text("plt_dropout_y", language),
      caption = caption
    )
}


## prep data ----
create_dropout_df <- function(vaccines, dropout_formula) {
  global_df %>% 
    rbind(., wuenic_dta) %>%
    filter(vaccine %in% vaccines,
           country %in% c(ctryn, regn, "Global")) %>%
    select(lvl_1, country, year, vaccine, vaccinated) %>%
    pivot_wider(names_from = vaccine, values_from = vaccinated) %>%
    mutate(dropout = eval(parse(text = dropout_formula)),
           dropout = round(dropout, 0)) %>%
    mutate(country = factor(country, levels = c(ctryn, 'Global', regn)))
}

# dropout data for dtp
dropout_df <- create_dropout_df(c("dtp3", "dtp1"), "(dtp1 - dtp3) / dtp1 * 100")

# dropout data for dtp-mcv
dtp_mcv_dropout_df <- create_dropout_df(c("mcv1", "dtp1"), "(dtp1 - mcv1) / dtp1 * 100")

# append both dfs so we can put both plots on the same scale by basing min and max values on dropout_all
dropout_all <- bind_rows(dropout_df, dtp_mcv_dropout_df)
dropout_max <- max(dropout_all$dropout) %>% unique()


## plot ----
# create the plots using the function (dtp and dtp-mcv)
p1 <- plot_dropout(dropout_df, get_text("plt_dropout_title2", language))
p2 <- plot_dropout(dtp_mcv_dropout_df, get_text("plt_dropout_title3", language), get_text("plt_dropout_cpt", language))


plt_dropout <- gg <- 
  p1 + p2 + 
  plot_annotation(title = get_text("plt_dropout_title", language),
                  theme = theme(plot.title = element_text(hjust = 0.5)))


## narrative ----
# first combine the two dataframes for dtp and dtp-mcv drop-out
df_txt <- dropout_df %>% 
  mutate(vax = 'dtp') %>%
  bind_rows(dtp_mcv_dropout_df) %>%
  mutate(vax = ifelse(is.na(vax), 'dtpmcv', vax)) %>%
  select(-c(dtp1, dtp3, mcv1)) %>%
  # classify dropout
  mutate(txt = case_when(dropout < 5 ~ "low",
                         dropout > 10 ~ "high",
                         dropout >= 5 & dropout <=10 ~ "medium"),
         # classify retention in imm programmes based on dropout classification
         retention = case_when(grepl("low", txt) ~ "good",
                               grepl("high", txt) ~ "poor",
                               grepl("medium", txt) ~ "moderate")) %>%
  # compare regional to global
  mutate(dropout_global = ifelse(country=='Global', dropout, NA_real_)) %>%
  group_by(year, vax) %>%
  fill(dropout_global, .direction="updown") %>%
  ungroup() %>%
  mutate(diff = dropout - dropout_global,
         diff_txt = case_when(diff < 0 ~ "lower",
                              diff > 0 ~ "higher",
                              diff == 0 ~ "the same as"))


# note :: the first two sentence are in the translation table
txt_dropout <- txt <- str_glue("In {rev_yr}, {df_txt %>% filter(year==rev_yr & country==ctryn & vax=='dtp') %>% pull(dropout)}% of children who received DTP1 did not receive DTP3 (left), and {df_txt %>% filter(year==rev_yr & country==ctryn & vax=='dtpmcv') %>% pull(dropout)}% of children who received DTP1 did not receive MCV1 (right).\n
                      The {df_txt %>% filter(year==rev_yr & country==ctryn & vax=='dtp') %>% pull(txt)} DTP drop-out rates imply {df_txt %>% filter(year==rev_yr & country==ctryn & vax=='dtp') %>% pull(retention)} ability to provide a complete series of vaccines early in life. The {df_txt %>% filter(year==rev_yr & country==ctryn & vax=='dtpmcv') %>% pull(txt)} DTP-MCV drop-out rates imply {df_txt %>% filter(year==rev_yr & country==ctryn & vax=='dtpmcv') %>% pull(retention)} retention in immunization programmes and ability to provide a full course of vaccines in infancy (up to one year).\n
                      In {rev_yr}, {ctryn} DTP drop-out was {df_txt %>% filter(year==rev_yr & country==ctryn & vax=='dtp') %>% pull(diff_txt)} and DTP-MCV drop-out was {df_txt %>% filter(year==rev_yr & country==ctryn & vax=='dtp') %>% pull(diff_txt)} than global drop-out rates, respectively.") 


## top line message ----
df_tlm <- df_txt %>%
  filter(lvl_1 == "country",
         year == rev_yr) %>%
  select(country, vax, dropout, txt, retention) %>%
  mutate(txt = ifelse(txt == "medium", "moderate", txt),
         cat = case_when(dropout < 5 & dropout >= 0 ~ "<5%",
                         dropout %in% c(5:10) ~ "5-10%",
                         dropout > 10 ~ ">10%",
                         dropout < 0 ~ "negative"),
         retention_type = case_when(vax == "dtp" ~ "ability to deliver the primary series early on",
                                  vax == "dtpmcv" ~ "ability to provide the full course of vaccines in infacy"))

# number of classifications
n_grp <- n_distinct(df_tlm$txt)

if (n_grp == 1) {
  # if drop-out category is the same for both antigens
  tlm <- str_glue("In {rev_yr}, DTP1 and DTP3/MCV1 drop-out was {unique(df_tlm$txt)} in {ctryn}, indicating {unique(df_tlm$retention)} retention of children in immunization programmes")
  
  if (unique(df_tlm$txt) == "high") {
    # both high drop out
  tlm <- str_glue("{tlm}, and the need for stronger follow-up of those initially reached.") 
  
  } else if (unique(df_tlm$txt) == "low") {
    # both low dropout
    tlm <- str_glue("{tlm}, and current efforts to ensure follow up are successful.") 
    
  } else if (unique(df_tlm$txt) == "moderate") {
    # both medium dropout
    tlm <- str_glue("{tlm}; additional efforts are needed to further reduce drop-out and ensure all children are reached during infancy.") 
  }
  
} else {
  # if drop-out categories are different 
  tlm <- str_glue("In {rev_yr}, DTP1-DTP3 drop-out was {df_tlm %>% filter(vax == 'dtp') %>% pull(txt)} ({df_tlm %>% filter(vax == 'dtp') %>% pull(dropout)}%) and DTP1-MCV1 drop-out was {df_tlm %>% filter(vax == 'dtpmcv') %>% pull(txt)} ({df_tlm %>% filter(vax == 'dtpmcv') %>% pull(dropout)}%) in {ctryn}, indicating {df_tlm %>% filter(vax == 'dtp') %>% pull(retention)} {df_tlm %>% filter(vax == 'dtp') %>% pull(retention_type)}, but {df_tlm %>% filter(vax == 'dtpmcv') %>% pull(retention)} {df_tlm %>% filter(vax == 'dtpmcv') %>% pull(retention_type)}.")
}

tlm_dropout <- tlm