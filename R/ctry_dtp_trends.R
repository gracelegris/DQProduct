###### plt_ctry_dtp_trends :: coverage trends [dtp1, dtp3], zd and unvacc trends, country; line and stacked bar ###### 
## funcs :: gg1 ----
gg1_func <- function(dta) {
  
  df %>% 
    ggplot(aes(x = year, y = !!sym(val_use))) + 
    geom_col(data = filter(df, 
                           indic != 'Undervaccinated'), 
             aes(fill = indic)
             # , show.legend = FALSE
    ) + 
    geom_line(data = wuenic_dta %>% 
                filter(vaccine %in% c('dtp3','dtp1')) %>%
                mutate(vaccine=toupper(vaccine)), 
              aes(year, coverage * dtp_factor, color = vaccine),  # Do this so that the two lines show on the chart in different styles
              alpha = 0.5, linewidth = 1) +  
    # data labels for dtp1 (above line)
    geom_text(data = wuenic_dta %>% 
                filter(vaccine %in% c('dtp1'),
                       year %in% c(2019:rev_yr)),
              aes(year, coverage * dtp_factor, label = coverage), color = '#0058AB',
              vjust = -1, size = 3) +  
    # data labels for dtp3 (below line)
    geom_text(data = wuenic_dta %>% 
                filter(vaccine == c('dtp3'),
                       year %in% c(2019:rev_yr)), 
              aes(year, coverage * dtp_factor, label = coverage), color = '#333333',
              vjust = 1.7, size = 3) + 
    scale_linetype(name = '') + 
    scale_fill_manual(name = '', values = c('#B3B3B3','#1CABE2')) +
    scale_color_manual(name = '', values = c('#0058AB','#333333')) +
    scale_x_continuous(breaks = c(seq(base_yr_lo,2015,5), 
                                  comp_yr:rev_yr)) +
    theme_minimal() + 
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90, size = 11, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size=11),
          legend.text=element_text(size=11),
          axis.title = element_text(size=11),
          panel.grid.minor.x = element_blank(),
          legend.title=element_blank()) + 
    labs(x = '', y = get_text("plt_ctry_dtp_trends_y", language),
         title = get_text("plt_ctry_dtp_trends_title", language)
    ) 
  
}


## funcs :: gg2 ----
gg2_func <- function(dta) {
  
  df %>% 
    filter(indic != 'Undervaccinated',
           year %in% c(comp_yr, rev_yr-1, rev_yr)) %>% 
    mutate(bar_lbl = !!sym(val_use)) %>% 
    ggplot(aes(x = year_chr, y = !!sym(val_use))) +
    geom_col(aes(fill = indic), width = 0.8, show.legend = FALSE) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ',')) +
    labs(x = '', y = get_text("plt_ctry_dtp_trends_y", language),
         caption = get_text("plt_ctry_dtp_trends_cpt", language))  +
    scale_fill_manual(values = c('#B3B3B3','#1CABE2')) + 
    # scale_x_continuous(breaks = c(comp_yr, rev_yr-1, rev_yr)) + 
    coord_flip() +
    theme_minimal() + 
    theme(plot.title.position = "plot") + 
    theme(legend.position = 'bottom',
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size=11),
          legend.text=element_text(size=11),
          axis.title = element_text(size=11)) +
    geom_text(data = rel_yrs %>% filter(indic == 'Undervaccinated'),
              aes(x = year_chr, y = !!sym(val_use), label = !!sym(val_lbl_use)), 
              size = 3.2, hjust=-0.5)
  
}


## prep data :: gg1 :: stacked bar and line chart ----
zd_dropout_df <- wuenic_dta %>% 
  filter(vaccine %in% c('dtp1','dtp3'),
         lvl_1 == 'country') %>% 
  select(country:vaccine, unvaccinated) %>% 
  spread(vaccine, unvaccinated) %>% 
  mutate(`Drop-out` = `dtp3` - `dtp1`) %>%  # Calculate drop out
  rename(`Zero-dose` = `dtp1`,
         Undervaccinated = `dtp3`) %>% 
  gather('indic', 'val', -year, -country) %>% 
  mutate(val_million = round(val / 1000000, 3),
         val_thousand = round(val / 1000, 2)) %>%
  label_vals(val, "valr_lbl") %>%
  mutate(val_thousand_lbl = case_when(val_thousand < 0.5 ~ "<0.5",
                                      val_thousand < 1 ~ "<1",
                                      TRUE ~ as.character(round(val_thousand,1))))

undervacc_tot <- wuenic_dta %>% 
  filter(vaccine == 'dtp3',
         lvl_1 == 'country',
         year %in% tgt_yrs) %>% 
  mutate(unvacc_m = round(unvaccinated / 1000000, 3),
         unvacc_th = round(unvaccinated / 1000, 2))

# for scaling
max_val <- zd_dropout_df %>% filter(val==max(val)) %>% pull(val) %>% unique()
divide <- wuenic_dta %>% filter(vaccine %in% c('dtp1','dtp3'), year >= 2015) %>% filter(coverage == min(coverage)) %>% pull(coverage) %>% unique()


## plot gg1 ----
# max val less then 1000
if (max_val < 1000) {
  
  val_use <- "val"
  val_lbl_use <- "valr_lbl"
  unit <- ""
  
  dp <- 0
  df <- zd_dropout_df
  
  if (divide %in% c(50:70)) {
    val_divide <- 40
  } else if (divide %in% c(0:49)) {
    val_divide <- 30
  } else {
    val_divide <- 60
  }
  
  # get the factor to multiply coverage by to align y-axes
  dtp_factor <- zd_dropout_df %>% filter(indic == 'Undervaccinated') %>% select(val) %>% arrange(desc(val)) %>% slice(which.max(1)) %>%
    mutate(val=val/val_divide)
  dtp_factor <- dtp_factor$val
  
  yscale <- dtp_factor*105
  
  gg1 <- gg1_func(zd_dropout_df) 
  gg1 <- gg1 + 
    geom_text(data = zd_dropout_df %>%
                filter(indic == 'Undervaccinated',
                       year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)),
              aes(year, val, label = valr_lbl),
              vjust = -0.5,
              color = 'black', size = 3) + 
    scale_y_continuous(limits = c(0, yscale),
                       labels = scales::comma,
                       sec.axis = sec_axis(trans = ~ . / dtp_factor,
                                           name = get_text("plt_ctry_dtp_trends_y2", language)))
  
} else if (max_val >= 1000 & max_val < 100000 ) {           # | max_val < 1000000 & max_val>=10000
  # ) {
  
  val_use <- "val"
  val_lbl_use <- "val_thousand_lbl"   # use valr_lbl or val
  unit <- ""
  
  if (divide %in% c(50:70)) {
    val_divide <- 40
  } else if (divide %in% c(0:49)) {
    val_divide <- 30
  } else {
    val_divide <- 60
  }
  
  # get the factor to multiply coverage by to align y-axes
  dtp_factor <- zd_dropout_df %>% filter(indic == 'Undervaccinated') %>% select(val) %>% arrange(desc(val)) %>% slice(which.max(1)) %>%
    mutate(val=val/val_divide)
  dtp_factor <- dtp_factor$val
  
  yscale <- dtp_factor*105
  
  dp <- 1
  df <- zd_dropout_df
  
  gg1 <- gg1_func(df) 
  gg1 <- gg1 + 
    geom_text(data = df %>%
                filter(indic != 'Undervaccinated',
                       !grepl("<", valr_lbl),
                       year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)),
              aes(year, val, label = !!sym(val_lbl_use)),  # round(val_thousand,1)
              position = position_stack(vjust = .5),
              color = 'white', size = 3) +
    geom_text(data = df %>%
                filter(indic == 'Undervaccinated',
                       year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)),
              aes(year, val, label = !!sym(val_lbl_use)),
              vjust = -0.5,
              color = 'black', size = 3) +
    scale_y_continuous(limits = c(0, yscale), 
                       labels = scales::label_number(suffix = 'K', scale = 1e-3),
                       sec.axis = sec_axis(trans = ~ . / dtp_factor, 
                                           name = get_text("plt_ctry_dtp_trends_y2", language))) +
    labs(y = paste0(get_text("plt_ctry_dtp_trends_y", language), "(", get_text("thousands", language), ")"))
  
  
} else if (max_val < 1000000 & max_val>=100000) {
  
  val_use <- "val"
  val_lbl_use <- "val_thousand_lbl"   # use valr_lbl or val
  unit <- ""
  
  if (divide %in% c(50:70)) {
    val_divide <- 40
  } else if (divide %in% c(0:49)) {
    val_divide <- 30
  } else {
    val_divide <- 60
  }
  
  # get the factor to multiply coverage by to align y-axes
  dtp_factor <- zd_dropout_df %>% filter(indic == 'Undervaccinated') %>% select(val) %>% arrange(desc(val)) %>% slice(which.max(1)) %>%
    mutate(val=val/val_divide)
  dtp_factor <- dtp_factor$val
  
  yscale <- dtp_factor*105
  
  dp <- 0
  df <- zd_dropout_df  %>% mutate(val_thousand_lbl = as.character(round(val_thousand,0)))
  
  gg1 <- gg1_func(df) 
  gg1 <- gg1 + 
    geom_text(data = df %>%
                filter(indic != 'Undervaccinated',
                       !grepl("<", valr_lbl),
                       year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)),
              aes(year, val, label = !!sym(val_lbl_use)),  # round(val_thousand,1)
              position = position_stack(vjust = .5),
              color = 'white', size = 3) +
    geom_text(data = df %>%
                filter(indic == 'Undervaccinated',
                       year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)),
              aes(year, val, label = !!sym(val_lbl_use)),
              vjust = -0.5,
              color = 'black', size = 3) +
    scale_y_continuous(limits = c(0, yscale), 
                       labels = scales::label_number(suffix = 'K', scale = 1e-3),
                       sec.axis = sec_axis(trans = ~ . / dtp_factor, 
                                           name = get_text("plt_ctry_dtp_trends_y2", language))) +
    labs(y = paste0(get_text("plt_ctry_dtp_trends_y", language), "(", get_text("thousands", language), ")"))
  
} else if (max_val >= 1000000) {
  
  val_use <- "val"
  val_lbl_use <- "val_million"  
  unit <- paste0("(", get_text("millions", language), ")")
  
  if (x %in% c('nga')) {
    val_divide <- 35
  } else {
    val_divide <- 100
  }
  
  # get the factor to multiply coverage by to align y-axes
  dtp_factor <- zd_dropout_df %>% filter(indic == 'Undervaccinated') %>% select(val) %>% arrange(desc(val)) %>% slice(which.max(1)) %>%
    mutate(val=val/val_divide)  # was previously 100
  dtp_factor <- dtp_factor$val
  
  yscale <- dtp_factor*105
  
  dp <- 1
  df <- zd_dropout_df
  
  gg1 <- gg1_func(df) 
  gg1 <- gg1 + 
    geom_text(data = df %>% 
                filter(indic != 'Undervaccinated',
                       year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)),
              aes(year, val, label = round(val_million,dp)),
              position = position_stack(vjust = .5), 
              color = 'white', size = 3) + 
    geom_text(data = df %>%
                filter(indic == 'Undervaccinated',
                       year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)),
              aes(year, val, label = round(val_million,dp)),
              vjust = -0.5,
              color = 'black', size = 3) +
    scale_y_continuous(limits = c(0, yscale), 
                       labels = scales::label_number(suffix = 'M', scale = 1e-6),
                       sec.axis = sec_axis(trans = ~ . / dtp_factor, 
                                           name = get_text("plt_ctry_dtp_trends_y2", language)))
  
}


## prep data :: gg2 :: horizontal bar chart :: number zero-dose in 2019 and rev_yr ----
rel_yrs <- zd_dropout_df %>% 
  filter(year %in% c(comp_yr, rev_yr-1, rev_yr)) %>%
  mutate(val_thousand = round(val/1000, 0),
         val_million = round(val/1000000, 1),
         year_chr = as.character(year))

# for scaling
max_val_zd <- rel_yrs %>% filter(val==max(val)) %>% pull(val) %>% unique()


## plot gg2 ----
# max val less than 1000
if (max_val_zd < 1000) {
  
  val_use <- "val"
  val_lbl_use <- "valr_lbl"
  unit <- ""
  df <- rel_yrs
  
  gg2 <- gg2_func(rel_yrs) 
  
} else if (max_val_zd >= 1000 & max_val_zd < 10000 | max_val_zd >= 10000 & max_val_zd < 1000000) {
  
  val_use <- "val"
  val_lbl_use <- "val_thousand_lbl"   # use valr_lbl or val
  unit <- ""

  df <- rel_yrs
  
  gg2 <- gg2_func(rel_yrs) 
  gg2 <- gg2 +
    geom_text(aes(label = !!sym(val_lbl_use)), position = position_stack(vjust = 0.5), size = 3.2, color = 'white') +
    scale_y_continuous(labels = scales::label_number(suffix = 'K', scale = 1e-3)) +
    labs(y = paste0(get_text("plt_ctry_dtp_trends_y", language), "(", get_text("thousands", language), ")")) 
  

} else if (max_val_zd >= 1000000) {
  
  val_use <- "val"
  val_lbl_use <- "val_million"  
  unit <- paste0("(", get_text("millions", language), ")")
  df <- rel_yrs
  
  gg2 <- gg2_func(rel_yrs)
  
  gg2 <- gg2 +
    geom_text(aes(label = !!sym(val_lbl_use)), position = position_stack(vjust = 0.5), size = 3.2, color = 'white') +
    scale_y_continuous(labels = scales::label_number(suffix = 'M', scale = 1e-6)) +
    labs(x = '', y = get_text("plt_ctry_dtp_trends_y", language),
         caption = get_text("plt_ctry_dtp_trends_cpt", language))  
  
  # a message will pop up here about another y-scale - you can ignore it
}

# flag where there are no zero-dose children in rev_yr or comp_yr
no_zd <- wuenic_dta %>% 
  filter(vaccine == "dtp1",
         year %in% c(rev_yr, comp_yr)) %>%
  mutate(n = n_distinct(unvaccinated)) %>%
  mutate(no_zd = ifelse(unvaccinated < 1 & n == 1, 1, 0)) %>%
  pull(no_zd) %>%
  unique()

no_undervacc <- wuenic_dta %>% 
  filter(vaccine == "dtp1",
         year %in% c(rev_yr, comp_yr)) %>%
  mutate(n = n_distinct(unvaccinated)) %>%
  mutate(no_undervacc = ifelse(unvaccinated < 1 & n == 1, 1, 0)) %>%
  pull(no_undervacc) %>%
  unique()


## combine plots ----
# combine gg1 and gg2 only if there are no zero-dose children in in rev_y or comp_yr
if (no_zd == 1 & no_undervacc == 1) {
  gg <- gg1 + 
    labs(caption = get_text("plt_ctry_dtp_trends_cpt", language))
  
} else {
  # combine plots 1 and 2 into one plot
  gg <- gg1 / gg2 + 
    plot_layout(heights = c(5,1))
}

plt_ctry_dtp_trends <- gg


## narrative ----
df_txt <- wuenic_dta %>% 
  arrange(vaccine, year) %>%
  filter(vaccine %in% c('dtp3','dtp1'),
         year >= 2018) %>% 
  label_vals(unvaccinated, "unvaccinated_lbl") %>%
  mutate(unvaccinated = unvaccinated_lbl,
         unvaccinated = gsub(",|<", "", unvaccinated),
         unvaccinated = as.numeric(unvaccinated)) %>%
  arrange(vaccine, year) %>%
  group_by(vaccine) %>%
  mutate(lag_cvg = lag(coverage),
         lag_unvaccinated = lag(unvaccinated))%>%
  ungroup() %>%
  mutate(txt1 = case_when(coverage == lag_cvg ~ "remained constant",
                          coverage >= lag_cvg-cvg_noise & coverage <= lag_cvg+cvg_noise &  coverage != lag_cvg ~ str_glue("remained relatively constant within {cvg_noise}% at"),
                          coverage < lag_cvg-cvg_noise ~ "declined at",
                          coverage > lag_cvg+cvg_noise ~ "increased to"),
         txt2 = case_when(unvaccinated == lag_unvaccinated ~ str_glue("remained constant at"),
                          unvaccinated < lag_unvaccinated ~ "improved",
                          unvaccinated > lag_unvaccinated  ~ "increased"))

dtp1_small_n <- df_txt %>% filter(year==rev_yr, vaccine=='dtp1')
unvacc_constant <- df_txt %>% filter(year==rev_yr, vaccine=='dtp1') %>% pull(txt2) %>% unique()

# note :: the first two sentences come from the translation table
if (grepl("<", dtp1_small_n$unvaccinated_lbl)) {
  txt <- str_glue("In {rev_yr}, DTP1 coverage in {ctryn} {df_txt %>% filter(year == rev_yr & vaccine == 'dtp1') %>% pull(txt1)} {df_txt %>% filter(year == rev_yr & vaccine == 'dtp1') %>% pull(coverage)}%. There were {unique(dtp1_small_n$unvaccinated_lbl)} children who missed out on any DTP vaccination (zero-dose children).\n
                     DTP3 coverage {df_txt %>% filter(year == rev_yr & vaccine == 'dtp3') %>% pull(txt1)} at {df_txt %>% filter(year == rev_yr & vaccine == 'dtp3') %>% pull(coverage)}% in {rev_yr}, leaving {df_txt %>% filter(year == rev_yr & vaccine == 'dtp3') %>% pull(unvaccinated_lbl)} children vulnerable to vaccine-preventable diseases.")
  
} else if (unvacc_constant == "remained constant") {
  # text for if number of zerodose children was the same in rev_yr as in previous year
  txt <- str_glue("In {rev_yr}, DTP1 coverage in {ctryn} {df_txt %>% filter(year == rev_yr & vaccine == 'dtp1') %>% pull(txt1)} {df_txt %>% filter(year == rev_yr & vaccine == 'dtp1') %>% pull(coverage)}%. The number of children missing out on any DTP vaccination (zero-dose children) in {rev_yr} was the same as in {rev_yr-1} (n={df_txt %>% filter(year == rev_yr & vaccine == 'dtp1') %>% pull(unvaccinated_lbl)}).\n
                     DTP3 coverage {df_txt %>% filter(year == rev_yr & vaccine == 'dtp3') %>% pull(txt1)} {df_txt %>% filter(year == rev_yr & vaccine == 'dtp3') %>% pull(coverage)}% in {rev_yr}, leaving {df_txt %>% filter(year == rev_yr & vaccine == 'dtp3') %>% pull(unvaccinated_lbl)} children vulnerable to vaccine-preventable diseases.")
  
} else {
  txt <- str_glue("In {rev_yr}, DTP1 coverage in {ctryn} {df_txt %>% filter(year == rev_yr & vaccine == 'dtp1') %>% pull(txt1)} {df_txt %>% filter(year == rev_yr & vaccine == 'dtp1') %>% pull(coverage)}%. The number of children missing out on any DTP vaccination (zero-dose children) {df_txt %>% filter(year == rev_yr & vaccine == 'dtp1') %>% pull(txt2)} from {df_txt %>% filter(year == rev_yr-1 & vaccine == 'dtp1') %>% pull(unvaccinated_lbl)} in {rev_yr-1} to {df_txt %>% filter(year == rev_yr & vaccine == 'dtp1') %>% pull(unvaccinated_lbl)} in {rev_yr}.\n
                     DTP3 coverage {df_txt %>% filter(year == rev_yr & vaccine == 'dtp3') %>% pull(txt1)} {df_txt %>% filter(year == rev_yr & vaccine == 'dtp3') %>% pull(coverage)}% in {rev_yr}, leaving {df_txt %>% filter(year == rev_yr & vaccine == 'dtp3') %>% pull(unvaccinated_lbl)} children vulnerable to vaccine-preventable diseases.")
}

txt_ctry_dtp_trends <- txt


## top line message ----
# dropout
df_txt_do <- zd_dropout_df %>%
  filter(year == rev_yr, 
         indic == "Drop-out") %>%
  mutate(val_lbl = valr_lbl)

tlm <- str_glue("In {rev_yr}, DTP1 coverage was {df_txt %>% filter(vaccine == 'dtp1' & year == rev_yr) %>% pull(coverage)}% and DTP3 was {df_txt %>% filter(vaccine == 'dtp3' & year == rev_yr) %>% pull(coverage)}%, leaving {df_txt %>% filter(vaccine == 'dtp3' & year == rev_yr) %>% pull(unvaccinated_lbl)} children un- or under-vaccinated, including {df_txt %>% filter(vaccine == 'dtp1' & year == rev_yr) %>% pull(unvaccinated_lbl)} zero-dose and {df_txt_do$val_lbl} with only partial protection.")

tlm_ctry_dtp_trends <- tlm