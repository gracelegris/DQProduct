###### plt_ctry_mcv_trends :: coverage trends [mcv1, mcv2], zd and unvacc trends, country; line and stacked bar ###### 
## funcs :: gg1 ----
gg1_func <- function(dta) {
  
  if (gg1_n_indic > 1){
    
    df %>% 
      ggplot(aes(x = year, y = !!sym(val_use))) + 
      geom_col(data = filter(df, 
                             indic != 'mcv2'), 
               aes(fill = forcats::fct_rev(indic_ftr))
      ) + 
      geom_line(data = wuenic_dta %>% 
                  filter(vaccine %in% c('mcv1','mcv2')) %>%
                  mutate(vaccine=toupper(vaccine)), 
                aes(year, coverage * mcv_factor, color = vaccine),  # Do this so that the two lines show on the chart in different styles
                linewidth = 1) +  
      # data labels for mcv1 (above line)
      geom_text(data = wuenic_dta %>% 
                  filter(vaccine %in% c('mcv1'),
                         year %in% c(2019:rev_yr)), #   filter(value>=5) %>%
                aes(year, coverage * mcv_factor, label = coverage),
                color = '#0058AB',
                vjust = -1, size = 3.5) +   # size is for side of data labels for lines
      # data labels for mcv2 (below line)
      geom_text(data = wuenic_dta %>% 
                  filter(vaccine == c('mcv2'),
                         year %in% c(2019:rev_yr)), #   filter(value>=5) %>%
                aes(year, coverage * mcv_factor, label = coverage),
                color = '#333333',
                vjust = 1.7, size = 3.5) + 
      scale_linetype(name = '') + 
      scale_color_manual(name = '', values = c('#0058AB','#333333')) +
      scale_fill_discrete(breaks=c('No MCV1', 'No MCV2'), type=c('#B3B3B3','#E2231A')) +
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
      labs(x = '', y = paste0(get_text("plt_ctry_mcv_trends_y", language), " (#)"),
           title = get_text("plt_ctry_mcv_trends_title", language)
           # caption = wuenic_src
      ) +
      guides(linetype = guide_legend(override.aes = aes(label = "", alpha = 1), nrow=1, reverse = F, order = 1),
             color = guide_legend(order = 2))  # show line legend first
    
  } else {
    
    df %>% 
      ggplot(aes(x = year, y = !!sym(val_use))) + 
      geom_col(data = filter(df, indic != 'mcv2'), 
               aes(fill = 'No MCV1') # Use a variable for fill
      ) + 
      geom_line(data = wuenic_dta %>% 
                  filter(vaccine %in% c('mcv1', 'mcv2')) %>%
                  mutate(vaccine = toupper(vaccine)), 
                aes(year, coverage * mcv_factor, color = vaccine),  
                linewidth = 1) +  
      geom_text(data = wuenic_dta %>% 
                  filter(vaccine %in% c('mcv1'),
                         year %in% c(2019:rev_yr)), 
                aes(year, coverage * mcv_factor, label = coverage),
                vjust = -1, size = 3.5) +   
      scale_linetype(name = '') + 
      scale_color_manual(name = '', values = c('MCV1' = '#0058AB', 'MCV2' = '#E2231A')) + # Make sure to specify both colors
      scale_fill_manual(name = 'Coverage', values = c('No MCV1' = '#E2231A')) + # Use scale_fill_manual for the fill legend
      scale_x_continuous(breaks = c(seq(base_yr_lo,2015,5), 
                                    comp_yr:rev_yr)) +
      theme_minimal() + 
      theme(legend.position = 'bottom',
            axis.text.x = element_text(angle = 90, size = 11, vjust = 0.5, hjust = 1),
            axis.text.y = element_text(size = 11),
            legend.text = element_text(size = 11),
            axis.title = element_text(size = 11),
            legend.title = element_blank()) + 
      labs(x = '', y = paste0("# ", get_text("plt_ctry_mcv_trends_y", language)),
           title = get_text("plt_ctry_mcv_trends_title", language)
      ) +
      guides(linetype = guide_legend(override.aes = aes(label = "", alpha = 1), nrow = 1, reverse = F, order = 1),
             color = guide_legend(order = 2))
  }
}


## funcs :: gg2 ----
# function for plot
gg2_func <- function(dta) {
  
  if (unique(dta$n_indic) > 1){
    # if the country has mcv1 and mcv2
    df %>% 
      filter(indic != 'mcv2',
             year %in% c(comp_yr, rev_yr-1, rev_yr)) %>% 
      mutate(bar_lbl = !!sym(val_use)) %>% 
      ggplot(aes(x = year_chr, y = !!sym(val_use))) +
      geom_col(aes(fill = forcats::fct_rev(indic_ftr)), width = 0.8, show.legend = FALSE) +
      scale_y_continuous(labels = scales::comma_format(big.mark = ',')) +
      labs(x = '', y = paste0("# ", get_text("plt_ctry_mcv_trends_y", language)),
           caption =  get_text("plt_ctry_mcv_trends_cpt", language))  +
      scale_fill_manual(values = c('#B3B3B3','#E2231A')) + 
      # scale_x_continuous(breaks = c(comp_yr, rev_yr)) + 
      coord_flip() +
      theme_minimal() + 
      theme(plot.title.position = "plot") + 
      theme(legend.position = 'bottom',
            axis.text.x = element_text(size = 11),
            axis.text.y = element_text(size=11),
            legend.text=element_text(size=11),
            axis.title = element_text(size=11)) +
      geom_text(data = rel_yrs_mcv %>% filter(indic == 'mcv2'),
                aes(x = year_chr, y = !!sym(val_use), label = !!sym(val_lbl_use)), 
                size = 3.2, hjust=-0.5)
  } else {
    # if the country has mcv1 only
    df %>% 
      filter(indic != 'mcv2',
             year %in% c(comp_yr, rev_yr-1, rev_yr)) %>% 
      mutate(bar_lbl = !!sym(val_use)) %>% 
      ggplot(aes(x = year_chr, y = !!sym(val_use))) +
      geom_col(aes(fill = forcats::fct_rev(indic_ftr)), width = 0.8, show.legend = FALSE) +
      scale_y_continuous(labels = scales::comma_format(big.mark = ',')) +
      labs(x = '', y = paste0(get_text("plt_ctry_mcv_trends_y", language), " (#)"),
           caption =  get_text("plt_ctry_mcv_trends_cpt", language))  +
      scale_fill_manual(values = c('#E2231A')) + 
      # scale_x_continuous(breaks = c(comp_yr, rev_yr-1, rev_yr)) + 
      coord_flip() +
      theme_minimal() + 
      theme(plot.title.position = "plot") + 
      theme(legend.position = 'bottom',
            axis.text.x = element_text(size = 11),
            axis.text.y = element_text(size=11),
            legend.text=element_text(size=11),
            axis.title = element_text(size=11)) +
      geom_text(data = rel_yrs_mcv %>% filter(indic == 'mcv2'),
                aes(x = year, y = !!sym(val_use), label = !!sym(val_lbl_use)), 
                size = 3.2, hjust=-0.5)
  }
}


## prep data :: gg1 ----
mcv_dropout_df <- wuenic_dta %>% 
  filter(vaccine %in% c('mcv1','mcv2')) %>% 
  select(country:vaccine, unvaccinated) %>% 
  spread(vaccine, unvaccinated) 

# if there is no mcv2 in the data, add a column for mcv2 as 0
if (!"mcv2" %in% names(mcv_dropout_df)) {
  mcv_dropout_df <- mcv_dropout_df %>%
    mutate(mcv2 = 0)
}

mcv_dropout_df <- mcv_dropout_df %>%
  mutate(`No MCV2` = mcv2 - mcv1) %>% 
  rename(`No MCV1` = mcv1) %>% 
  gather('indic', 'val', -year, -country) %>% 
  mutate(val_million = round(val / 1000000, 3),
         val_thousand = round(val / 1000, 1)) %>%
  label_vals(val, "valr_lbl") %>%
  mutate(val_thousand_lbl = case_when(val_thousand < 0.5 ~ "<0.5",
                                      val_thousand < 1 ~ "<1",
                                      TRUE ~ as.character(val_thousand))) %>%
  filter(val >= 0)

gg1_n_indic <- wuenic_dta %>% filter(vaccine %in% c('mcv1','mcv2')) %>% mutate(n_indic = n_distinct(vaccine)) %>% pull(n_indic) %>% unique()

# Determine the levels dynamically
factor_levels <- c("No MCV1", setdiff(unique(mcv_dropout_df$indic), "No MCV1|mcv2"), "mcv2")

# Ensure "No MCV1" appears only once at the beginning
if ("No MCV1" %in% factor_levels) {
  factor_levels <- c("No MCV1", setdiff(factor_levels, "No MCV1"))
}

# Adjust factor levels
mcv_dropout_df <- mcv_dropout_df %>%
  mutate(indic_ftr = fct_relevel(indic, factor_levels))

# identify years where there is negative dropout
neg_dropout <- mcv_dropout_df %>%
  filter(val < 0) %>%
  mutate(neg_dropout_flag = 1) %>%
  select(country, year, neg_dropout_flag)

# join flag to data and replace the values for mcv2 with missing where there is negative dropout so that the totals do not show on the chart
mcv_dropout_df <- left_join(mcv_dropout_df, neg_dropout) %>% distinct() %>%
  mutate(val = case_when(neg_dropout_flag==1 & indic == 'mcv2' ~ NA_real_, TRUE ~ val),
         val_million = case_when(neg_dropout_flag==1 & indic == 'mcv2'  ~ NA_real_, TRUE ~ val_million),
         val_thousand = case_when(neg_dropout_flag==1 & indic == 'mcv2'  ~ NA_real_, TRUE ~ val_thousand),
         valr_lbl = case_when(neg_dropout_flag==1 & indic == 'mcv2'  ~ NA_character_, TRUE ~ valr_lbl)) %>%
  filter(!is.na(val)) %>%
  filter(val >= 0)

if (gg1_n_indic == 1) {
  mcv_dropout_df <- mcv_dropout_df %>% filter(indic != 'mcv2')
} else {
  mcv_dropout_df <- mcv_dropout_df %>%
    # Filter out rows where indic is 'mcv2' if n_indic == 2 because we don't want the total showing where there is negative dropout (ie. mcv2 > mcv1)
    group_by(year) %>%
    mutate(n_indic = n_distinct(indic)) %>%
    ungroup() %>%
    mutate(indic = ifelse(n_indic==2 & indic=="mcv2", "remove", indic)) %>%
    filter(indic!="remove")
}


## plot gg1 ----
# for scaling
max_val_mcv <- mcv_dropout_df %>% filter(!is.na(val)) %>% filter(val==max(val)) %>% pull(val) %>% unique()
divide_mcv <- wuenic_dta %>% filter(vaccine %in% c('mcv1','mcv2'), year >= 2015) %>% filter(coverage == min(coverage)) %>% pull(coverage) %>% unique()


# max val less then 1000
if (max_val_mcv < 1000) {
  
  val_use <- "val"
  val_lbl_use <- "valr_lbl"
  unit <- ""
  
  dp <- 0
  df <- mcv_dropout_df
  
  if (divide_mcv %in% c(50:70)) {
    val_divide <- 40
  } else if (divide_mcv %in% c(0:49)) {
    val_divide <- 30
  } else {
    val_divide <- 60
  }
  
  # get the factor to multiply coverage by to align y-axes
  mcv_factor <- mcv_dropout_df %>% filter(indic %in% c('No MCV1','mcv2')) %>% select(val) %>% arrange(desc(val)) %>% slice(which.max(1)) %>%
    mutate(val=val/val_divide)
  mcv_factor <- mcv_factor$val
  
  yscale <- mcv_factor*105
  
  gg1 <- gg1_func(mcv_dropout_df) 
  gg1 <- gg1 + 
    geom_text(data = mcv_dropout_df %>% 
                filter(indic == 'mcv2',
                       year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)),
              aes(year, val, label = valr_lbl),
              vjust = -0.5, 
              color = 'black', size = 3) +
    scale_y_continuous(limits = c(0, yscale),
                       labels = scales::comma,
                       sec.axis = sec_axis(trans = ~ . / mcv_factor,
                                           name = get_text("plt_ctry_mcv_trends_y2", language)))
  
} else if (max_val_mcv >= 1000 & max_val_mcv < 100000) {
  
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
  mcv_factor <- mcv_dropout_df %>% filter(indic %in% c('No MCV1','mcv2')) %>% select(val) %>% arrange(desc(val)) %>% slice(which.max(1)) %>%
    mutate(val=val/val_divide)
  mcv_factor <- mcv_factor$val
  
  yscale <- mcv_factor*105
  
  dp <- 1
  df <- mcv_dropout_df
  
  gg1 <- gg1_func(mcv_dropout_df) 
  gg1 <- gg1 + 
    geom_text(data = mcv_dropout_df %>% 
                filter(indic != 'mcv2',
                       !grepl("<", valr_lbl),
                       year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)),
              aes(year, val, label = !!sym(val_lbl_use)),
              position = position_stack(vjust = .5), 
              color = 'white', size = 3) + 
    geom_text(data = mcv_dropout_df %>% 
                filter(indic == 'mcv2',
                       year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)),
              aes(year, val, label = !!sym(val_lbl_use)),
              vjust = -0.5, 
              color = 'black', size = 2.7) +
    scale_y_continuous(limits = c(0, yscale), 
                       labels = scales::label_number(suffix = 'K', scale = 1e-3),
                       sec.axis = sec_axis(trans = ~ . / mcv_factor, 
                                           name = get_text("plt_ctry_mcv_trends_y2", language))) +
    labs(y = paste0("# ", get_text("plt_ctry_mcv_trends_y", language), "(", get_text("thousands", language), ")"))
  
  
} else if (max_val_mcv >= 10000 & max_val_mcv < 1000000) {
  
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
  mcv_factor <- mcv_dropout_df %>% filter(indic %in% c('No MCV1','mcv2')) %>% select(val) %>% arrange(desc(val)) %>% slice(which.max(1)) %>%
    mutate(val=val/val_divide)
  mcv_factor <- mcv_factor$val
  
  yscale <- mcv_factor*105
  
  dp <- 1
  df <- mcv_dropout_df  %>% mutate(val_thousand_lbl = as.character(round(val_thousand,0)))
  
  gg1 <- gg1_func(df) 
  gg1 <- gg1 + 
    geom_text(data = df %>% 
                filter(indic != 'mcv2',
                       !grepl("<", valr_lbl),
                       year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)),
              aes(year, val, label = !!sym(val_lbl_use)),
              position = position_stack(vjust = .5), 
              color = 'white', size = 3) + 
    geom_text(data = df %>% 
                filter(indic == 'mcv2',
                       year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)),
              aes(year, val, label = !!sym(val_lbl_use)),
              vjust = -0.5, 
              color = 'black', size = 2.7) +
    scale_y_continuous(limits = c(0, yscale), 
                       labels = scales::label_number(suffix = 'K', scale = 1e-3),
                       sec.axis = sec_axis(trans = ~ . / mcv_factor, 
                                           name = get_text("plt_ctry_mcv_trends_y2", language))) +
    labs(y = paste0("# ", get_text("plt_ctry_mcv_trends_y", language), "(", get_text("thousands", language), ")"))
  
  
} else if (max_val_mcv >= 1000000) {
  
  val_use <- "val"
  val_lbl_use <- "val_million"  
  unit <- paste0("(", get_text("millions", language), ")")
  
  if (x %in% c('nga')) {
    val_divide <- 35
  } else {
    val_divide <- 100
  }
  
  # get the factor to multiply coverage by to align y-axes
  mcv_factor <- mcv_dropout_df %>% filter(indic %in% c('No MCV1','mcv2')) %>% select(val) %>% arrange(desc(val)) %>% slice(which.max(1)) %>%
    mutate(val=val/val_divide)
  mcv_factor <- mcv_factor$val
  yscale <- mcv_factor*105
  
  dp <- 1
  df <- mcv_dropout_df
  
  gg1 <- gg1_func(df) 
  gg1 <- gg1 + 
    geom_text(data = df %>% 
                filter(indic != 'mcv2',
                       year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)),
              aes(year, val, label = round(val_million,dp)),
              position = position_stack(vjust = .5), 
              color = 'white', size = 3) + 
    geom_text(data = df %>% 
                filter(indic == 'mcv2',
                       year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)),
              aes(year, val, label = round(val_million,dp)),
              vjust = -0.5, 
              color = 'black', size = 3) +
    scale_y_continuous(limits = c(0, yscale), 
                       labels = scales::label_number(suffix = 'M', scale = 1e-6),
                       sec.axis = sec_axis(trans = ~ . / mcv_factor, 
                                           name = get_text("plt_ctry_mcv_trends_y2", language)))
  
}



## prep data :: gg2 ----
rel_yrs_mcv <- mcv_dropout_df %>% 
  filter(year %in% c(comp_yr, rev_yr-1, rev_yr),
         !is.na(val)) %>%
  mutate(val_thousand = round(val/1000, 1),
         val_million = round(val/1000000, 1)) %>%
  filter(val >= 0) %>%
  mutate(n_indic = n_distinct(indic),
         year_chr = as.character(year))


## plot :: gg2 ----
# for scaling
max_val_mcv <- rel_yrs_mcv %>% filter(val==max(val)) %>% pull(val) %>% unique()

# max val less than 1000
if (max_val_mcv < 1000) {
  
  val_use <- "val"
  val_lbl_use <- "valr_lbl"
  unit <- ""
  df <- rel_yrs_mcv
  
  gg2 <- gg2_func(rel_yrs_mcv) 
  
} else if (max_val_mcv >= 1000 & max_val_mcv < 10000 | max_val_mcv >= 10000 & max_val_mcv < 1000000) {
  
  val_use <- "val"
  val_lbl_use <- "val_thousand_lbl"   # use valr_lbl or val
  unit <- ""

  df <- rel_yrs_mcv
  
  gg2 <- gg2_func(rel_yrs_mcv) 
  gg2 <- gg2 +
    geom_text(aes(label = !!sym(val_lbl_use)), position = position_stack(vjust = 0.5), size = 3.2, color = 'white') +
    scale_y_continuous(labels = scales::label_number(suffix = 'K', scale = 1e-3)) +
    labs(y = paste0("# ", get_text("plt_ctry_mcv_trends_y", language), "(", get_text("thousands", language), ")"))
  
  
} else if (max_val_mcv >= 1000000) {
  
  val_use <- "val"
  val_lbl_use <- "val_million"  
  unit <- paste0("(", get_text("millions", language), ")")
  df <- rel_yrs_mcv
  
  gg2 <- gg2_func(rel_yrs_mcv) +
    geom_text(aes(label = !!sym(val_lbl_use)), position = position_stack(vjust = 0.5), size = 3.2, color = 'white')
  
  gg2 <- gg2 +
    scale_y_continuous(labels = scales::label_number(suffix = 'M', scale = 1e-6))
  # a message will pop up here about another y-scale - you can ignore it
}

# flag where there are no zero-dose children in rev_yr or comp_yr
no_mcvzd <- wuenic_dta %>% 
  filter(vaccine == "dtp1",
         year %in% c(rev_yr, comp_yr)) %>%
  mutate(n = n_distinct(unvaccinated)) %>%
  mutate(no_mcvzd = ifelse(unvaccinated < 1 & n == 1, 1, 0)) %>%
  pull(no_mcvzd) %>%
  unique()


## combine plots ----
# combine gg1 and gg2 only if there are no zero-dose children in in rev_y ror comp_yr
if (no_mcvzd == 1) {
  gg <- gg1 + 
    labs(caption = get_text("plt_ctry_mcv_trends_cpt", language))
  
} else {
  # combine plots 1 and 2 into one plot
  gg <- gg1 / gg2 + 
    plot_layout(heights = c(5,1))
}

plt_ctry_mcv_trends <- gg


## narrative ----
df_txt <- wuenic_dta %>% 
  arrange(vaccine, year) %>%
  filter(vaccine %in% c('mcv1','mcv2'),
         year >= 2018) %>% 
  arrange(vaccine, year) %>%
  group_by(vaccine) %>%
  mutate(lag_cvg = lag(coverage),
         lag_unvaccinated = lag(unvaccinated),
         comp_cvg = ifelse(year == comp_yr, coverage, NA_real_))%>%
  fill(comp_cvg, .direction="updown") %>%
  ungroup() %>%
  # change in coverage compared to prev year
  mutate(txt1 = case_when(coverage == lag_cvg ~ "remained constant at",
                          coverage >= lag_cvg-cvg_noise & coverage <= lag_cvg+cvg_noise & coverage != lag_cvg ~ str_glue("remained relatively constant (within {cvg_noise}%) at"),
                          coverage < lag_cvg-cvg_noise ~ "declined at",
                          coverage > lag_cvg+cvg_noise ~ "increased to"),
         # change in number of unvaccinated children compared to prev year
         txt2 = case_when(unvaccinated == lag_unvaccinated ~ "remained constant",
                          unvaccinated < lag_unvaccinated ~ "improvement",
                          unvaccinated > lag_unvaccinated  ~ "increase"),
         # comparing cvg to 2019
         txt3 = case_when(coverage > comp_cvg+cvg_noise ~ "greater than in",
                          coverage < comp_cvg-cvg_noise  ~ "lower than in",
                          TRUE ~ "similar to in" ))

no_mcv2 <- df_txt %>% filter(vaccine == 'mcv2')

if (no_data(no_mcv2) == FALSE) {
  # note :: the first sentence is in the translation table
  txt <- str_glue("The percentage of children receiving MCV1 – typically at 9 or 12 months depending on the national vaccination schedule – {df_txt %>% filter(year == rev_yr & vaccine == 'mcv1') %>% pull(txt1)} {df_txt %>% filter(year == rev_yr & vaccine == 'mcv1') %>% pull(coverage)}%. This is {df_txt %>% filter(year == rev_yr & vaccine == 'mcv1') %>% pull(txt3)} {comp_yr}, where coverage was {df_txt %>% filter(year == rev_yr & vaccine == 'mcv1') %>% pull(comp_cvg) %>% unique()}%.\n
                     {df_txt %>% filter(year == rev_yr & vaccine == 'mcv1') %>% pull(unvaccinated_lbl)} children missed their routine first dose of measles vaccine.\n
                     MCV2 is typically administered to children between 18 months and five years old. MCV2 coverage {df_txt %>% filter(year == rev_yr & vaccine == 'mcv2') %>% pull(txt1)} {df_txt %>% filter(year == rev_yr & vaccine == 'mcv2') %>% pull(coverage)}% in {rev_yr}.")
} else{
  txt <- str_glue("The percentage of children receiving MCV1 – typically at 9 or 12 months depending on the national vaccination schedule – {df_txt %>% filter(year == rev_yr & vaccine == 'mcv1') %>% pull(txt1)} {df_txt %>% filter(year == rev_yr & vaccine == 'mcv1') %>% pull(coverage)}%. This is {df_txt %>% filter(year == rev_yr & vaccine == 'mcv1') %>% pull(txt3)} {comp_yr}, where coverage was {df_txt %>% filter(year == rev_yr & vaccine == 'mcv1') %>% pull(comp_cvg) %>% unique()}%.\n
                     {df_txt %>% filter(year == rev_yr & vaccine == 'mcv1') %>% pull(unvaccinated_lbl)} children missed their routine first dose of measles vaccine.\n
                     In {rev_yr}, {ctryn} did not have MCV2.")
}

txt_ctry_mcv_trends <- txt


## top line message ----
# prep tlm dfs
df_tlm <- df_txt %>%
  select(country, vaccine, year, coverage, lag_cvg, cvg_change = txt1, unvaccinated_lbl) %>%
  filter(year == rev_yr)

df_tlm_mcv1 <- df_tlm %>% filter(vaccine == 'mcv1')
df_tlm_mcv2 <- df_tlm %>% filter(vaccine == 'mcv2')


# threshold warnings
mcv1_warn <- ifelse(df_tlm_mcv1$coverage < 95,
                    " - below the 95% level needed to prevent outbreaks and",
                    "")

mcv2_warn <- if (no_data(df_tlm_mcv2) == FALSE) {
  ifelse(df_tlm_mcv2$coverage < 90,
         " - below the IA2030 target of 90%.",
         "")
} else {
  ""
}


# text for ending
if (df_tlm_mcv1$cvg_change == 'increased') {
  end <- "but this still left"
} else {
  end <- "leaving"
}

# MCV1 top-line
if (grepl('constant', df_tlm_mcv1$cvg_change)) {
  tlm <- str_glue(
    "MCV1 coverage {df_tlm_mcv1$cvg_change} {df_tlm_mcv1$lag_cvg}% in {rev_yr}{mcv1_warn}, ",
    "{end} {df_tlm_mcv1$unvaccinated_lbl} children without any protection against measles."
  )
} else {
  tlm <- str_glue(
    "MCV1 coverage {df_tlm_mcv1$cvg_change} from {df_tlm_mcv1$lag_cvg}% in {rev_yr-1} ",
    "to {df_tlm_mcv1$coverage}% in {rev_yr}{mcv1_warn}, ",
    "{end} {df_tlm_mcv1$unvaccinated_lbl} children without any protection against measles."
  )
}

# MCV2 extension if country has it
if (no_data(df_tlm_mcv2) == FALSE) {
  if (grepl('constant', df_tlm_mcv2$cvg_change) & grepl('constant', df_tlm_mcv1$cvg_change)) {
    tlm <- str_glue(
      "{tlm} MCV2 coverage also {df_tlm_mcv2$cvg_change} ",
      "{df_tlm_mcv2$lag_cvg}%{mcv2_warn}"
    )
  } else if (grepl('constant', df_tlm_mcv2$cvg_change)) {
    tlm <- str_glue(
      "{tlm} MCV2 coverage {df_tlm_mcv2$cvg_change} ",
      "{df_tlm_mcv2$lag_cvg}%{mcv2_warn}"
    )
  } else {
    tlm <- str_glue(
      "{tlm} MCV2 coverage {df_tlm_mcv2$cvg_change} ",
      "from {df_tlm_mcv2$lag_cvg}% in {rev_yr-1} ",
      "to {df_tlm_mcv2$coverage}% in {rev_yr}{mcv2_warn}"
    )
  }
}

tlm_ctry_mcv_trends <- tlm



# # top-line  message
# # text for ending
# if (df_tlm_mcv1$cvg_change == 'increased') {
#   end <- "but this still left"
# } else {
#   end <- "leaving"
# }
#
# if (grepl('constant', df_tlm_mcv1$cvg_change)) {
#   tlm <- str_glue("MCV1 coverage {df_tlm_mcv1$cvg_change} {df_tlm_mcv1$lag_cvg}% in {rev_yr}, {end} {df_tlm_mcv1$unvaccinated_lbl} children without any protection against measles.")
# } else {
#   tlm <- str_glue("MCV1 coverage {df_tlm_mcv1$cvg_change} from {df_tlm_mcv1$lag_cvg}% in {rev_yr-1} to {df_tlm_mcv1$coverage}% in {rev_yr}, {end} ~{df_tlm_mcv1$unvaccinated_lbl} children without any protection against measles.")
# }
# 
# if (no_data(df_tlm_mcv2) == FALSE) {
#   if (grepl('constant', df_tlm_mcv2$cvg_change) & grepl('constant', df_tlm_mcv1$cvg_change)) {
#   tlm <- str_glue("{tlm} MCV2 coverage also {df_tlm_mcv2$cvg_change} {df_tlm_mcv2$lag_cvg}%.")
#   } else if (grepl('constant', df_tlm_mcv2$cvg_change)) {
#     tlm <- str_glue("{tlm} MCV2 coverage {df_tlm_mcv2$cvg_change} {df_tlm_mcv2$lag_cvg}%.")
#   } else {
#     tlm <- str_glue("{tlm} MCV2 coverage {df_tlm_mcv2$cvg_change} from {df_tlm_mcv2$lag_cvg}% in {rev_yr-1} to {df_tlm_mcv2$coverage}% in {rev_yr}.")
#   }
# }

