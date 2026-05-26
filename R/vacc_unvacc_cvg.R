###### plt_vacc_unvacc_cvg :: vaccinated, unvaccinated and coverage [dtp1, dtp3, mcv1] ######
## plot func 1 ----
# function for plot 1 (all years)
func_plt1 <- function(df) {
  
  max_y <- max(df[[val_use]], na.rm = TRUE)
  
  df %>% 
    ggplot(aes(x = year, y = !!sym(val_use))) + 
    geom_col(data = df %>% filter(indic != 'Target'), 
             aes(fill = indic2)
             # , show.legend = FALSE
    ) + 
    geom_line(data = wuenic_dta %>% 
                filter(vaccine == v) %>%
                mutate(vaccine=toupper(vaccine)), 
              aes(year, coverage * dtp_factor2, color = vaccine),  # Do this so that the two lines show on the chart in different styles
              linewidth = 1) +  
    # data labels for dtp1 (above line)
    geom_text(data = wuenic_dta %>% 
                filter(vaccine == v,
                       year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)), #   filter(value>=5) %>%
              aes(year, 102 * dtp_factor2, label = coverage),
              # aes(year, coverage * dtp_factor2, label = coverage),
              colour = '#0058AB',
              vjust = 0, size = 3) +   # size is for side of data labels for lines
    geom_text(data = df %>% 
                filter(indic == 'Target',
                       year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)),
              aes(year, !!sym(val_use), label = !!sym(val_lbl_use)),
              vjust = -1, 
              color = 'black', size = 2.6) + 
    scale_linetype(name = '') + 
    scale_fill_manual(name = '', values = c(clr1, clr2)) + 
    scale_color_manual(name = '', values = c(clr3), labels = c(paste0(toupper(v), " ", get_text("coverage2", language)))) +
    scale_x_continuous(breaks = c(seq(base_yr_lo, 2015, 5), # Update next year
                                  comp_yr:rev_yr)) + # Update next year
    theme_minimal() + 
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90, size = 11, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(size = 11),
          legend.text = element_text(size = 11),
          axis.title = element_text(size = 11),
          panel.grid.minor.x = element_blank(),
          legend.title = element_blank()) + 
    labs(x = '', y = paste('# ', get_text("children", language), "", unit),
         title = get_text("plt_vacc_unvacc_cvg_title", language)
         # caption = wuenic_src
    ) +
    guides(linetype = guide_legend(override.aes = aes(label = "", alpha = 1), nrow = 1, reverse = F, order = 1),
           color = guide_legend(order = 2)) + # show the line legend
    geom_segment(data = wuenic_dta %>% 
                   filter(vaccine == v,
                          year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)), 
                 aes(x = year, xend = year, 
                     y = coverage * dtp_factor2, yend = 100 * dtp_factor2), 
                 linetype = "dotted", color = "#0058AB")
  
}


## plot func 2 ----
# function for plot 2 (comp_yr, rev_yr-1 and rev_yr)
func_plt2 <- function(dta) {
  
  df %>% 
    filter(indic != 'Target',
           year %in% c(comp_yr, rev_yr-1, rev_yr)) %>% 
    mutate(bar_lbl = str_c(indic, " ", !!sym(val_lbl_use))) %>% 
    ggplot(aes(x = year_chr, y = !!sym(val_use))) + 
    geom_col(aes(fill = indic), width = 0.8, show.legend = FALSE) + 
    geom_text(data = df %>% filter(year %in% c(comp_yr, rev_yr-1, rev_yr),
                                   indic == 'Target'),
              aes(x = year_chr, y = !!sym(val_use),
                  label = !!sym(val_lbl_use)),
              size = 3.2, position = position_stack(vjust = 1.05)) +
    scale_fill_manual(name = '', values = c(clr1, clr2)) + 
    # scale_x_continuous(breaks = c(comp_yr, rev_yr)) + 
    coord_flip() +
    theme_minimal() + 
    theme(plot.title.position = "plot") + 
    theme(legend.position = 'bottom',
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size=11),
          legend.text=element_text(size=11),
          axis.title = element_text(size=11)
    ) + 
    labs(x = '', 
         y = paste('# ', get_text("children", language), "", unit),
         caption = cpt)
}


## prep data ----
vacc_tgt <- wuenic_dta %>% 
  filter(vaccine %in% c('dtp1','dtp3','mcv1')) %>% 
  select(country:vaccine, vaccinated, unvaccinated, target) %>% 
  pivot_longer(-c(country:vaccine), names_to = 'indic', values_to = 'val') %>%
  label_vals(val, "valr_lbl") %>%
  mutate(val_m = round(val / 1000000, 1),
         val_th = round(val / 1000, 2),
         val_th_lbl = case_when(val_th < 1 ~ '<1',
                                TRUE ~ as.character(round(val_th,1))),
         type = case_when(vaccine == 'dtp1' ~ 'unvaccinated (zero-dose)',
                          TRUE ~ 'unvaccinated'),
         indic = str_to_title(indic),
         indic = ifelse(vaccine == 'dtp1' & indic == 'Unvaccinated', 'Unvaccinated (zero-dose)', indic),
         indic2 = case_when(indic == "Vaccinated" ~ str_to_title(get_text("vaccinated", language)),
                            indic == "Unvaccinated" ~ str_to_title(get_text("notvaccinated", language)),
                            indic == "Unvaccinated (zero-dose)" ~ paste0(str_to_title(get_text("notvaccinated", language)), " (", get_text("unvacc_dtp1", language), ")"))) %>% 
  mutate(data_label_y = 100)

# identify maximum value to decide which units to use (millions or thousands)
max_val <- vacc_tgt %>% filter(val==max(val)) %>% pull(val) %>% unique()

# number to produce our factor
divide <- wuenic_dta %>% filter(vaccine %in% c('dtp1','dtp3','mcv1'), year >= 2015) %>% filter(coverage == min(coverage)) %>% pull(coverage) %>% unique()


## plot 1 and 2 ----
for (v in c('dtp1', 'dtp3', 'mcv1')) {
  
  if (v == 'mcv1') {
    clr1 <- '#E2231A'  # unvaccinated
    clr2 <- '#80BD41'  # vaccinated
    clr3 <- '#0058AB'  # coverage
  } else {
    clr1 <- '#1CABE2'
    clr2 <- '#80BD41'
    clr3 <- '#0058AB'
  }
  
  if (v == "dtp3") {
    cpt = get_text("plt_vacc_unvacc_cvgb_cpt_dtp3", language)
  } else {
    cpt = str_glue("{wuenic_src}") 
  }
  
  # plot where target population < 1000
  if (max_val < 1000) {
    
    val_use <- "val"
    val_lbl_use <- "valr_lbl"
    unit <- ""
    
    if (divide %in% c(50:70)) {
      val_divide <- 40
    } else if (divide %in% c(0:49)) {
      val_divide <- 50
    } else {
      val_divide <- 60
    }
    
    # get the factor to multiply coverage by to align y-axes
    dtp_factor2 <- vacc_tgt %>% filter(indic == 'Target') %>% select(val) %>% arrange(desc(val)) %>% slice(which.max(1)) %>%
      mutate(val=val/val_divide)
    dtp_factor2 <- dtp_factor2$val
    
    yscale <- dtp_factor2*105
    
    df <- vacc_tgt %>% filter(vaccine == v) %>% mutate(year_chr = as.character(year))
    
    gg1 <- func_plt1(df) +
      scale_y_continuous(limits = c(0, yscale), 
                         labels = scales::comma,
                         sec.axis = sec_axis(trans = ~ . / dtp_factor2, 
                                             name = get_text("coverage", language))) 
    
    
    # plot 19b
    gg2 <- func_plt2(df) + scale_y_continuous(labels = scales::comma)
    
    
    
  } else if (max_val < 1000000 & max_val>=1000) {
    # target pop between 1000 and 1000000
    val_use <- "val_th"
    val_lbl_use <- "val_th_lbl"
    unit <- paste0("(", get_text("thousands", language), ")")
    
    if (divide %in% c(50:70)) {
      val_divide <- 40000
    } else if (divide %in% c(0:49)) {
      val_divide <- 50000
    } else {
      val_divide <- 60000
    }
    
    # get the factor to multiply coverage by to align y-axes
    dtp_factor2 <- vacc_tgt %>% filter(indic == 'Target') %>% select(val) %>% arrange(desc(val)) %>% slice(which.max(1)) %>%
      mutate(val=val/val_divide)
    dtp_factor2 <- dtp_factor2$val
    
    yscale <- dtp_factor2*105
    
    df <- vacc_tgt %>% filter(vaccine == v) %>% mutate(year_chr = as.character(year))
    
    # plot 19a
    gg1 <- func_plt1(df) +
      scale_y_continuous(limits = c(0, yscale), 
                         labels = scales::comma,
                         sec.axis = sec_axis(trans = ~ . / dtp_factor2, 
                                             name = get_text("coverage", language))) +
      geom_text(data = df %>% 
                  filter(indic != 'Target',
                         year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)),
                aes(year, !!sym(val_use), label = !!sym(val_lbl_use)),
                position = position_stack(vjust = .5), 
                color = 'white', size = 2.6) 
    
    # plot 19b
    gg2 <- func_plt2(df) +
      scale_y_continuous(labels = scales::comma) +
      geom_text(aes(label = !!sym(val_lbl_use)), position = position_stack(vjust = 0.5), size = 3, color = 'white')
    
    
  } else {
    
    # target pop >= 1 million
    val_use <- "val"
    val_lbl_use <- "val_m"
    unit <- paste0("(", get_text("millions", language), ")")
    
    if (divide %in% c(50:70)) {
      val_divide <- 40
    } else if (divide %in% c(0:49)) {
      val_divide <- 50
    } else {
      val_divide <- 60
    }
    
    # get the factor to multiply coverage by to align y-axes
    dtp_factor2 <- vacc_tgt %>% filter(indic == 'Target') %>% select(val) %>% arrange(desc(val)) %>% slice(which.max(1)) %>%
      mutate(val=val/val_divide)
    dtp_factor2 <- dtp_factor2$val
    
    yscale <- dtp_factor2*105
    
    df <- vacc_tgt %>% filter(vaccine == v) %>%
      mutate(year_chr = as.character(year),
             val_m = round(val/1000000, 2))
    
    gg1 <- func_plt1(df) +
      scale_y_continuous(limits = c(0, yscale), 
                         labels = scales::label_number(suffix = 'M', scale = 1e-6),
                         sec.axis = sec_axis(trans = ~ . / dtp_factor2, 
                                             name = get_text("coverage", language))) +
      geom_text(data = df %>% 
                  filter(indic != 'Target',
                         year %in% c(2000, 2005, 2010, 2015, 2019:rev_yr)),
                aes(year, !!sym(val_use), label = !!sym(val_lbl_use)),
                position = position_stack(vjust = .5), 
                color = 'white', size = 2.6) 
    
    # plot 19b
    gg2 <- func_plt2(df) + 
      scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
      geom_text(aes(label = !!sym(val_lbl_use)), position = position_stack(vjust = 0.5), size = 3, color = 'white')
    
  }
  
  # combine plots 1 and 2 into one plot
  gg <- gg1 / gg2 + 
    plot_layout(heights = c(5,1))
  
  # assign the ggplot chart with a name based on the vaccine
  name <- paste0("plt_vacc_unvacc_cvg_", v, sep = "")
  assign(name , gg)
  
}


## narrative ----
for (v in c('dtp1', 'dtp3', 'mcv1')) {
  
  df_txt <- vacc_tgt %>%
    filter(vaccine == v) %>%
    arrange(vaccine, indic, year) %>%
    group_by(vaccine, indic) %>%
    mutate(val_lag = lag(val)) %>%
    mutate(val_comp = ifelse(year == comp_yr, val, NA_real_)) %>%
    fill(val_comp, .direction="updown") %>%
    ungroup() %>%
    filter(year %in% c(comp_yr, rev_yr)) %>%
    # % change
    mutate(chng_pcnt = ((val / val_comp) - 1) * 100,
           chng_pcnt = round(chng_pcnt, 0),
           # absolute change 
           chng_diff_v0 = round((val - val_comp)/1000000, 1),
           chng_diff = ifelse(abs(chng_diff_v0)<1, format(abs(round(as.numeric(chng_diff_v0*1000000), 0)), nsmall=0, big.mark=","), paste0(as.character(abs(chng_diff_v0)),'m')),
           chng_diff = trimws(chng_diff),
           chng = case_when(chng_pcnt > 0 ~ 'increased',
                            chng_pcnt == 0 ~ 'remained constant',
                            chng_pcnt < 0 ~ 'decreased')) %>%
    mutate(chng2 = case_when(chng == 'increased' ~ 'more',
                             chng == 'decreased' ~ 'fewer',
                             chng == 'remained constant' ~ 'same'))
  
  df_txt_tgt <- df_txt %>% filter(indic == 'Target')
  df_txt_vacc <- df_txt %>% filter(indic == 'Vaccinated')
  max_val <- vacc_tgt %>% filter(val==max(val)) %>% pull(val) %>% unique()
  
  if (max_val < 1000) {
    
    val_use <- "val"
    val_lbl_use <- "valr_lbl"
    unit <- ""
    
  } else if (max_val < 1000000 & max_val>=1000) {
    # target pop between 1000 and 1000000
    val_use <- "val_th"
    val_lbl_use <- "valr_lbl"
    unit <- "(thousands)"
    
  } else {
    
    # target pop >= 1 million
    val_use <- "val"
    val_lbl_use <- "val_m"
    unit <- "(millions)"
    df_txt_tgt <- df_txt_tgt %>% mutate(val_m = paste0(as.character(val_m), "m"))
    df_txt_vacc <- df_txt_vacc %>% mutate(val_m = paste0(as.character(val_m), "m"))
  }
  
  df_txt_cvg_2019 <- wuenic_dta %>% 
    arrange(vaccine, year) %>%
    filter(vaccine == v,
           year %in% c(comp_yr, rev_yr)) %>% 
    group_by(vaccine) %>%
    mutate(lag_cvg = lag(coverage),
           lag_unvaccinated = lag(unvaccinated))%>%
    ungroup() %>%
    mutate(txt1 = case_when(coverage == lag_cvg ~ "the same as in",
                            coverage >= lag_cvg-cvg_noise & coverage <= lag_cvg+cvg_noise &  coverage != lag_cvg ~ str_glue("relatively constant within {cvg_noise}% compared to"),
                            coverage < lag_cvg-cvg_noise ~ "lower than in",
                            coverage > lag_cvg+cvg_noise ~ "higher than in"),
           txt2 = case_when(unvaccinated == lag_unvaccinated ~ str_glue("remained constant"),
                            unvaccinated < lag_unvaccinated ~ "improved",
                            unvaccinated > lag_unvaccinated  ~ "increased"))
  
  
  # identify countries with small target populations. for these countries, we won't specify the number of children vaccinated/target
  # small_tgt <- df_txt_tgt %>%
  #   mutate(flag = ifelse(grepl('<', valr_lbl), 1, 0),
  #          n_flag = n_distinct(flag)) %>%
  #   select(n_flag) %>%
  #   distinct() %>%
  #   pull()
  
  small_tgt <- df_txt_tgt %>%
    mutate(flag = ifelse(grepl('<', valr_lbl), 1, 0)) %>%
    mutate(flag_ctry = ifelse(flag==1, country, "no flag")) %>%
    pull(flag_ctry) %>%
    unique()
  
  
  # text depending on if population increased, decreased or remained the same between 2019 and rev_yr
  pop_chng <- df_txt_tgt %>% filter(year==rev_yr) %>%
    mutate(pop_chng = case_when(chng == 'increased' ~ 'For vaccine coverage to increase, the number of children vaccinated must increase at a faster rate than the population increases',
                                chng == 'decreased' ~ 'For vaccine coverage to increase, the number of children vaccinated needs to either increase or decline at a slower rate than the decline in surviving infant target population',
                                grepl('constant', chng) ~ 'For vaccine coverage to increase, the number of children vaccinated needs to increase'))
  
  
  if (ctryn %in% c(small_tgt)) {
    # narrative
    txt <- str_glue("{toupper(v)} coverage in {rev_yr} ({df_txt_cvg_2019 %>% filter(year==rev_yr) %>% pull(coverage)}%) was {df_txt_cvg_2019 %>% filter(year==rev_yr) %>% pull(txt1)} {comp_yr} ({df_txt_cvg_2019 %>% filter(year==comp_yr) %>% pull(coverage)}%).\n
                      The number of children vaccinated with {toupper(v)} {df_txt_vacc %>% filter(year==rev_yr) %>% pull(chng) %>% unique()} between {comp_yr} and {rev_yr}.\n
                      The number of surviving infants {df_txt_tgt %>% filter(year==rev_yr) %>% pull(chng) %>% unique()} between {comp_yr} and {rev_yr}.\n
                      In {rev_yr}, {df_txt_vacc %>% filter(year==rev_yr) %>% pull(chng_diff)} {df_txt_vacc %>% filter(year==rev_yr) %>% pull(chng2)} children were vaccinated than in {comp_yr}.\n
                      In {rev_yr}, there were {df_txt_tgt %>% filter(year==rev_yr) %>% pull(chng_diff)} {df_txt_tgt %>% filter(year==rev_yr) %>% pull(chng2)} surviving infants (target population) than in {comp_yr}.")
    
    # top line message
    if (df_txt_tgt %>% filter(year==rev_yr) %>% pull(chng) %>% unique() == "declined") {
      # if small population declining
      tlm <- str_glue("With the number of surviving infants declining in recent years, coverage can be sustained even if fewer children are vaccinated, while reaching more children translates directly into higher coverage.")
    
    } else if (df_txt_tgt %>% filter(year==rev_yr) %>% pull(chng) %>% unique() == "increased") {
      # if small population declining
      tlm <- str_glue("With the number of surviving infants increasing in recent years, maintaining coverage requires vaccinating more children each year, and improving coverage requires further reach.")
      
    } else {
      # if small population is relatively stable
      tlm <- str_glue("With the number of surviving infants remaining relatively stable in recent years, coverage can be sustained by vaccinating similar numbers of children that are currently being reached, while improving coverage requires further reach.")
    }
    
  } else {
    # narrative
    txt <- str_glue("{toupper(v)} coverage in {rev_yr} ({df_txt_cvg_2019 %>% filter(year==rev_yr) %>% pull(coverage)}%) was {df_txt_cvg_2019 %>% filter(year==rev_yr) %>% pull(txt1)} {comp_yr} ({df_txt_cvg_2019 %>% filter(year==comp_yr) %>% pull(coverage)}%).\n
                      The number of children vaccinated with {toupper(v)} {df_txt_vacc %>% filter(year==rev_yr) %>% pull(chng) %>% unique()} {df_txt_vacc %>% filter(year==rev_yr) %>% pull(chng_pcnt) %>% abs() %>% unique()}% compared to in 2019.\n
                      The number of surviving infants {df_txt_tgt %>% filter(year==rev_yr) %>% pull(chng) %>% unique()} approximately {df_txt_tgt %>% filter(year==rev_yr) %>% pull(chng_pcnt) %>% abs() %>% unique()}% compared to in 2019.\n
                      In {rev_yr}, {df_txt_vacc %>% filter(year==rev_yr) %>% pull(chng_diff)} {df_txt_vacc %>% filter(year==rev_yr) %>% pull(chng2)} children were vaccinated than in {comp_yr}.\n
                      In {rev_yr}, there were {df_txt_tgt %>% filter(year==rev_yr) %>% pull(chng_diff)} {df_txt_tgt %>% filter(year==rev_yr) %>% pull(chng2)} surviving infants (target population) than in {comp_yr}.\n
                      {pop_chng %>% pull(pop_chng)}.")
    
    # top line message
    tlm <- str_glue("{pop_chng %>% pull(pop_chng)}.")
  }
  
  txt <- gsub(" 0 fewer", " fewer", txt)
  txt <- gsub(" 0 more", " more", txt)
  # for small countries
  txt <- gsub(" 0 same children", " approximately the same number of children", txt)
  txt <- gsub(" 0 same surviving", " approximately the same number of surviving", txt)
  
  # fix narrative and tlm names
  name <- paste0("txt_vacc_unvacc_cvg_", v, sep = "")
  assign(name , txt)
  
  name <- paste0("tlm_vacc_unvacc_cvg_", v, sep = "")
  assign(name , tlm)
}


## top line message ----
# see above