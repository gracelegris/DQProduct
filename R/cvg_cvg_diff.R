###### cvg_cvg_diff :: coverage trends and difference compared to 2019 [dtp1, dtp3, mcv1], by region  ###### 
## prep data :: coverage trends ----
plot_df <- global_df %>%
  rbind(., wuenic_dta) %>%
  filter(vaccine %in% c('dtp1','dtp3','mcv1'),
         country %in% c(ctryn, regn, "Global"))

# set alpha for transparency of colours of other regions
alph <- 1


## plot 1 :: coverage trends ----
for (v in c('dtp1', 'dtp3', 'mcv1')) {
  
  df <- plot_df %>% filter(vaccine == v)
  
  # specify order in legend
  df$country <- factor(df$country, levels = c(ctryn,'Global', regn))
  
  gg1 <-
    ggplot(df, aes(x = year, y = coverage)) + 
    geom_line(df
              # %>% filter(country!=regn)
              , mapping = aes(x = year, y = coverage, colour = country), lwd=1,alpha=alph,
              show.legend=T) +
    geom_line(df %>% filter(country==ctryn), mapping = aes(x = year, y = coverage), colour = "black", lwd=1.6) + 
    geom_segment(data = wuenic_dta %>% 
                   filter(vaccine == v,
                          year %in% c(2000, 2005, 2010, 2015, 2019, rev_yr-1, rev_yr)), 
                 aes(x = year, xend = year, 
                     y = 102, yend = coverage), 
                 linetype = "dotted", color = "#0058AB")+
    # geom_point(df %>% filter(country==ctryn), mapping = aes(x = year, y = coverage), colour = "black", size=2.4) + 
    geom_line(df %>% filter(country==ctryn), mapping = aes(x = year, y = coverage, colour = country), lwd=1) + 
    # geom_point(df %>% filter(country==ctryn), mapping = aes(x = year, y = coverage, colour = country), size=2) +
    scale_x_continuous(breaks = c(seq(min(df$year), 2015, 5), 
                                  rev_yr-1,
                                  2019,
                                  rev_yr), 
                       limits = c(min(df$year), rev_yr)) + 
    scale_y_continuous(limits = c(0, 105),
                       breaks = c(0, 25, 50, 75, 100)) + 
    theme_minimal() + 
    scale_color_manual(values = unicef_colors3) +
    theme(axis.text.x = element_text(angle = 90, size=11, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size=11),
          axis.title = element_text(size=11),
          legend.text=element_text(size=11),
          plot.title = element_text(size=12, hjust = 0.5),
          panel.grid.minor.x = element_blank(),
          legend.position="bottom",
          legend.title=element_blank()
    ) +
    labs(title = get_text("plt_cvg_cvg_diff_title1", language), 
         x = "", y = get_text("coverage", language)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    geom_text(data = df %>% 
                filter(year %in% c(2000, 2005, 2010, 2015, 2019, rev_yr-1, rev_yr), country == ctryn),
              aes(year, 100, label = coverage), 
              vjust=-1.2, 
              size = 3,
              colour = "#0058AB",
              # fontface="bold",
              show.legend=F) +
    geom_text(data = df %>% 
                filter(year == rev_yr, country != ctryn),
              aes(year, coverage, label = coverage, colour = country), 
              hjust=-0.7, 
              size = 3,
              show.legend=F)
  
  # assign the ggplot chart with a name based on the vaccine
  name <- paste0("p1_", df$vaccine, sep = "")
  assign(name , gg1)
}


## prep data :: coverage difference compared to 2019 ----
df_chnge <- plot_df %>%
  select(lvl_1:vaccine, coverage) 

df_comp <- df_chnge %>% filter(year == comp_yr) %>%
  select(lvl_2, lvl_3, country, vaccine, cov_comp = coverage)

df_chnge <- left_join(df_chnge, df_comp, by = join_by(lvl_2, lvl_3, country, vaccine)) %>%
  mutate(diff = coverage - cov_comp)


## plot 2 :: coverage difference compared to 2019 ----
for (v in c('dtp1', 'dtp3', 'mcv1')) {
  
  df <- df_chnge %>% filter(vaccine == v)
  max1 <- df %>% filter(diff == max(diff)) %>% pull(diff) %>% unique()
  
  df$country <- factor(df$country, levels = c(ctryn,'Global', regn))
  
  gg2 <-
    ggplot(df, aes(x = year, y = diff)) + 
    geom_line(df 
              , mapping = aes(x = year, y = diff, colour = country), lwd=1, alpha=alph,
              show.legend=T) + 
    geom_line(aes(y = 0), linetype="dashed") +
    geom_line(df %>% filter(country==ctryn), mapping = aes(x = year, y = diff), colour = "black", lwd=1.6) + 
    geom_segment(data = df %>% 
                   filter(vaccine == v,
                          year %in% c(2000, 2005, 2010, 2015, 2019, rev_yr-1, rev_yr),
                          country == ctryn), 
                 aes(x = year, xend = year, 
                     y = diff, yend = max1+1.6), 
                 linetype = "dotted", color = "#0058AB")+
    # geom_point(df %>% filter(country==ctryn), mapping = aes(x = year, y = diff), colour = "black", size=2.4) + 
    geom_line(df %>% filter(country==ctryn), mapping = aes(x = year, y = diff, colour = country), lwd=1) + 
    # geom_point(df %>% filter(country==ctryn), mapping = aes(x = year, y = diff, colour = country), size=2) + 
    scale_x_continuous(breaks = c(seq(min(df$year), 2015, 5), 
                                  2019, 
                                  rev_yr-1,
                                  rev_yr), 
                       limits = c(min(df$year), rev_yr)) + 
    scale_y_continuous(limits = c(min(df$diff), max(df$diff)+10)
                       # breaks = c(min(df$diff), 0, max(df$diff)+1.5)
    ) + 
    theme_minimal() + 
    scale_color_manual(values = unicef_colors3) +
    theme(axis.text.x = element_text(angle = 90, size=11, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size=11),
          axis.title = element_text(size=12),
          legend.text=element_text(size=11),
          plot.title = element_text(size=12, hjust = 0.5),
          panel.grid.minor.x = element_blank(),
          legend.position="bottom",
          legend.title=element_blank()) +
    labs(title = get_text("plt_cvg_cvg_diff_title2", language),
         x = "", y = get_text("plt_cvg_cvg_diff_y2", language)) +
    geom_text(data = df %>% 
                filter(year %in% c(2000, 2005, 2010, 2015, 2019, rev_yr-1, rev_yr),
                       country == ctryn),
              aes(year, max1+2, label = diff), 
              # vjust=-1,
              color = "#0058AB",
              size = 3)  +
    geom_text(data = df %>% 
                filter(year == rev_yr,
                       country != ctryn),
              aes(year, diff, label = diff, colour = country), 
              hjust=-0.7,
              size = 3,
              show.legend=F) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) 
  
  # assign the ggplot chart with a name based on the vaccine
  name <- paste0("p2_", df$vaccine, sep = "")
  assign(name , gg2)
}


## combine plots 1 and 2 ----
gg_dtp1 <- p1_dtp1 + p2_dtp1
gg_dtp3 <- p1_dtp3 + p2_dtp3
gg_mcv1 <- p1_mcv1 + p2_mcv1


## prep data :: stacked bar rev_yr vs 2019 ----
# add stacked bar showing number unvaccinated in most recent year vs 2019
df_bar <- plot_df %>% 
  mutate(unvacc_m = round(unvaccinated / 1000000, 1),
         unvacc_m_lbl = case_when(unvacc_m < 1 ~ "<1",
                                  TRUE ~ as.character(round(unvacc_m, 0))),
         type = case_when(vaccine == 'dtp1' ~ get_text("unvacc_dtp1", language),
                          vaccine == 'dtp3' ~ get_text("unvacc_dtp3", language),
                          vaccine == 'mcv1' ~ get_text("unvacc_mcv1", language)),
         title = case_when(vaccine == 'dtp1' ~ get_text("plt_cvg_cvg_diff_title3_dtp1", language),
                           vaccine == 'dtp3' ~ get_text("plt_cvg_cvg_diff_title3_dtp3", language),
                           vaccine == 'mcv1' ~ get_text("plt_cvg_cvg_diff_title3_mcv1", language)),
         year_chr = as.character(year))


## plot 3 :: stacked bar rev_yr vs 2019 ----
for (v in c('dtp1', 'dtp3', 'mcv1')) {
  
  df <- df_bar %>% filter(vaccine == v, country == ctryn)
  
  df$country <- factor(df$country, levels = c(ctryn,'Global', regn))
  
  gg <-
    df %>% 
    filter(year %in% c(comp_yr, rev_yr-1, rev_yr)) %>%
    ggplot(aes(year_chr, unvaccinated)) +
    geom_col(aes(fill = country), width = 0.8, alpha = 0.8) + 
    geom_text(data = . %>% 
                filter(
                  # year %in% lbl_yrs,
                  unvacc_m != 0),
              aes(label = unvaccinated_lbl, group = country), 
              size = 3, position = position_dodge(width = .4),hjust=1.3) + 
    scale_fill_manual(values = "#1CABE2") +
    # scale_x_continuous(breaks = c(seq(base_yr_lo,2015,5), # Update next year
    #                               comp_yr, rev_yr-1, rev_yr)) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ',')) +
    theme_minimal() + 
    theme(legend.position = 'none', 
          axis.text.x = element_text(size = 11, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size=11),
          axis.title = element_text(size=8),
          legend.text=element_text(size=11),
          plot.title = element_text(size=12)) + 
    guides(fill = guide_legend(title = '', nrow = 1, reverse = F)) + 
    labs(title = paste0(str_glue('{df$title}, {comp_yr}, {rev_yr-1} ', get_text("and", language), ' {rev_yr}')),
         x = '', y = '',
         caption = get_text("plt_cvg_cvg_diff_cpt", language)) +
    coord_flip()
  
  if (v == 'mcv1') {
    gg <- gg + scale_fill_manual(name = '', values = c('#E2231A'))
    
  }
  
  # save plot as object
  name <- paste0("gg3_", as.character(df$vaccine), sep = "")
  assign(name , gg)
}


## combine ----
# if there are no zd/undervaccinated/measles-zd children, then do not show the bottom chart
# otherwise combine charts
if (no_zd == 1) {
  plt_cvg_cvg_diff_dtp1 <- gg_dtp1 + labs(caption = get_text("plt_cvg_cvg_diff_cpt", language))
} else {
  plt_cvg_cvg_diff_dtp1 <- gg_dtp1 / gg3_dtp1 +
    plot_layout(heights = c(5.5,1))
}

if (no_undervacc == 1) {
  plt_cvg_cvg_diff_dtp3 <- gg_dtp3 + labs(caption = get_text("plt_cvg_cvg_diff_cpt", language))
} else {
  plt_cvg_cvg_diff_dtp3 <- gg_dtp3 / gg3_dtp3 +
    plot_layout(heights = c(5.5,1))
}

if (no_mcvzd == 1) {
  plt_cvg_cvg_diff_mcv1 <- gg_mcv1 + labs(caption = get_text("plt_cvg_cvg_diff_cpt", language))
} else {
  plt_cvg_cvg_diff_mcv1 <- gg_mcv1 / gg3_mcv1 +
    plot_layout(heights = c(5.5,1))
}


## narrative ----
for (i in c("dtp1","dtp3","mcv1")) {
  
  # for change compared to 2019
  df_txt <- plot_df %>%
    filter(vaccine == i) %>%
    filter(year %in% c(comp_yr, rev_yr-1, rev_yr)) %>%
    group_by(country) %>%
    mutate(prev_cvg = ifelse(year == rev_yr-1, coverage, NA_real_),
           comp_cvg = ifelse(year == 2019, coverage, NA_real_)) %>%
    fill(prev_cvg, .direction = "updown") %>%
    fill(comp_cvg, .direction = "updown") %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(glb_cvg = ifelse(country == 'Global', coverage, NA_real_),
           reg_cvg = ifelse(country == regn, coverage, NA_real_)) %>%
    fill(glb_cvg, .direction = "updown") %>%
    fill(reg_cvg, .direction = "updown") %>%
    ungroup() %>%
    mutate(prev_cvg_txt = case_when(coverage < prev_cvg ~ "declined",
                                    coverage > prev_cvg ~ "increased",
                                    TRUE ~ "remained constant"),
           comp_cvg_txt = case_when(coverage < comp_cvg ~ "declined",
                                    coverage > comp_cvg ~ "increased",
                                    TRUE ~ "remained constant"),
           # % point difference compared to 2019/prev year
           prev_cvg_diff = coverage - prev_cvg,
           comp_cvg_diff = coverage - comp_cvg,
           # text for % point difference
           prev_cvg_diff_txt = case_when(prev_cvg_diff < -1 ~ str_glue("{abs(prev_cvg_diff)} percentage points lower than in"),
                                         prev_cvg_diff > 1 ~ str_glue("{abs(prev_cvg_diff)} percentage points higher than in"),
                                         prev_cvg_diff == -1 ~ str_glue("{abs(prev_cvg_diff)} percentage point lower than in"),
                                         prev_cvg_diff == 1 ~ str_glue("{abs(prev_cvg_diff)} percentage point higher than in"),
                                         prev_cvg_diff == 0 ~ "the same as in"),
           comp_cvg_diff_txt = case_when(comp_cvg_diff < -1 ~ str_glue("{abs(comp_cvg_diff)} percentage points lower than in"),
                                         comp_cvg_diff > 1 ~ str_glue("{abs(comp_cvg_diff)} percentage points higher than in"),
                                         comp_cvg_diff == -1 ~ str_glue("{abs(comp_cvg_diff)} percentage point lower than in"),
                                         comp_cvg_diff == 1 ~ str_glue("{abs(comp_cvg_diff)} percentage point higher than in"),
                                         comp_cvg_diff == 0 ~ "the same as in"),
           # % point difference compared to global/regional
           glb_cvg_diff = coverage - glb_cvg,
           reg_cvg_diff = coverage - reg_cvg,
           glb_cvg_diff_txt = case_when(glb_cvg_diff < -1 ~ str_glue("{abs(glb_cvg_diff)} percentage points lower than the global average ({glb_cvg}%)"),
                                        glb_cvg_diff > 1 ~ str_glue("{abs(glb_cvg_diff)} percentage points higher than the global average ({glb_cvg}%)"),
                                        glb_cvg_diff == -1 ~ str_glue("{abs(glb_cvg_diff)} percentage point lower than the global average ({glb_cvg}%)"),
                                        glb_cvg_diff == 1 ~ str_glue("{abs(glb_cvg_diff)} percentage point higher than the global average ({glb_cvg}%)"),
                                        glb_cvg_diff == 0 ~ "the same as the global average"),
           reg_cvg_diff_txt = case_when(reg_cvg_diff < -1 ~ str_glue("{abs(reg_cvg_diff)} percentage points lower than the average across all {regn_txt} countries ({reg_cvg}%)"),
                                        reg_cvg_diff > 1 ~ str_glue("{abs(reg_cvg_diff)} percentage points higher than the average across all {regn_txt} countries ({reg_cvg}%)"),
                                        reg_cvg_diff == -1 ~ str_glue("{abs(reg_cvg_diff)} percentage point lower than the average across all {regn_txt} countries ({reg_cvg}%)"),
                                        reg_cvg_diff == 1 ~ str_glue("{abs(reg_cvg_diff)} percentage point higher than average across all {regn_txt} countries ({reg_cvg}%)"),
                                        reg_cvg_diff == 0 ~ "the same the average across all {regn_txt} countries")) 
  
  # type of unvaccinated
  unvac_type <- df_bar %>% filter(vaccine==i) %>% pull(type) %>% unique()
  
  # identify small numbers of unvaccinated children and flag so we can apply special text where we do not reference the specific numbers of unvacc children
  unvacc_chng <- df_txt %>% 
    filter(year %in% c(comp_yr, rev_yr),
           iso3c == x) %>%
    mutate(unvaccinated = unvaccinated_lbl,
           unvaccinated = gsub(",|<", "", unvaccinated),
           unvaccinated = as.numeric(unvaccinated)) %>%
    select(lvl_1:unvaccinated, unvaccinated_lbl) %>%
    arrange(vaccine, year) %>%
    mutate(lag_unvacc = lag(unvaccinated),
           diff = unvaccinated - lag_unvacc,
           diff_txt = case_when(diff < 0 ~ 'declined (ie. improved) compared to in',
                                diff > 0 ~ 'increased (ie. worsened) compared to in',
                                diff == 0 ~ str_glue('remained approximately the same ({unvaccinated_lbl}) as in')),
           symbol = ifelse(grepl("<", unvaccinated_lbl), "<", ""),
           symbol_count = n_distinct(symbol)) %>%
    filter(year==rev_yr) %>%
    mutate(small_n = ifelse(symbol_count==1 & symbol=="<", 1, 0))
  
  # flag if number of unvaccinated is the same in rev_yr as previous year (based on unvaccinated_lbl)
  unvacc_constant <- unvacc_chng %>% mutate(unvacc_constant = ifelse(grepl("same", diff_txt), 1, 0)) %>% pull(unvacc_constant) %>% unique()
  
  
  # text where there are small numbers of unvaccinated children
  if (unvacc_chng$small_n == 1) {
    txt <- str_glue("In {rev_yr}, {toupper(i)} coverage in {ctryn} ({df_txt %>% filter(year==rev_yr & iso3c==x) %>% pull(coverage)}%) was {df_txt %>% filter(year==rev_yr & iso3c==x) %>% pull(glb_cvg_diff_txt)} and {df_txt %>% filter(year==rev_yr & iso3c==x) %>% pull(reg_cvg_diff_txt)}.\n
National {toupper(i)} coverage was {df_txt %>% filter(year==rev_yr & iso3c==x) %>% pull(comp_cvg_diff_txt)} {comp_yr} ({df_txt %>% filter(year==comp_yr & iso3c==x) %>% pull(coverage)}%).\n
The number of {unvac_type} children {unvacc_chng %>% pull(diff_txt)} {comp_yr}.")
    
  } else if (unvacc_constant == 1) {
    # text for where the number of unvaccinated children in rev_r is the same as in comp_yr
    txt <- str_glue("In {rev_yr}, {toupper(i)} coverage in {ctryn} ({df_txt %>% filter(year==rev_yr & iso3c==x) %>% pull(coverage)}%) was {df_txt %>% filter(year==rev_yr & iso3c==x) %>% pull(glb_cvg_diff_txt)} and {df_txt %>% filter(year==rev_yr & iso3c==x) %>% pull(reg_cvg_diff_txt)}.\n
National {toupper(i)} coverage was {df_txt %>% filter(year==rev_yr & iso3c==x) %>% pull(comp_cvg_diff_txt)} {comp_yr} ({df_txt %>% filter(year==comp_yr & iso3c==x) %>% pull(coverage)}%).\n
This equates to {df_txt %>% filter(year==rev_yr & iso3c==x) %>% pull(unvaccinated_lbl)} {unvac_type} children in {rev_yr} - the same number of {unvac_type} children as in {comp_yr}.")
    
  } else {
    txt <- str_glue("In {rev_yr}, {toupper(i)} coverage in {ctryn} ({df_txt %>% filter(year==rev_yr & iso3c==x) %>% pull(coverage)}%) was {df_txt %>% filter(year==rev_yr & iso3c==x) %>% pull(glb_cvg_diff_txt)} and {df_txt %>% filter(year==rev_yr & iso3c==x) %>% pull(reg_cvg_diff_txt)}.\n
National {toupper(i)} coverage was {df_txt %>% filter(year==rev_yr & iso3c==x) %>% pull(comp_cvg_diff_txt)} {comp_yr} ({df_txt %>% filter(year==comp_yr & iso3c==x) %>% pull(coverage)}%).\n
This equates to {df_txt %>% filter(year==rev_yr & iso3c==x) %>% pull(unvaccinated_lbl)} {unvac_type} children in {rev_yr} compared to {df_txt %>% filter(year==comp_yr & iso3c==x) %>% pull(unvaccinated_lbl)} {unvac_type} children in {comp_yr}.")
  }
  
  
  # fix non-programme
  txt <- gsub("Non-programme", "non-programme", txt)
  
  name <- paste0("txt_cvg_cvg_diff_", df_txt$vaccine, sep = "")
  assign(name , txt)
  
  
  # top line message
  df_tlm <- df_txt %>% 
    filter(iso3c == x,
           year == rev_yr)
  
  threshold <- 10
  
  
  if (df_tlm$glb_cvg_diff < -threshold & df_tlm$reg_cvg_diff < -threshold) {
    # coverage considerably lower than global and regional average
    tlm <- str_glue("In {rev_yr}, {ctryn} {toupper(i)} coverage was {df_tlm %>% filter(year==rev_yr & iso3c==x) %>% pull(coverage)}% - this is {df_tlm %>% filter(year==rev_yr & iso3c==x) %>% pull(comp_cvg_diff_txt)} {comp_yr} and more than {threshold} percentage points lower than both the global and regional averages.")
    
  } else if (df_tlm$glb_cvg_diff < 0 & df_tlm$reg_cvg_diff < 0) {
    # coverage lower than global and regional average
    tlm <- str_glue("In {rev_yr}, {ctryn} {toupper(i)} coverage was {df_tlm %>% filter(year==rev_yr & iso3c==x) %>% pull(coverage)}% - this is {df_tlm %>% filter(year==rev_yr & iso3c==x) %>% pull(comp_cvg_diff_txt)} {comp_yr} and lower than both the global and regional averages.")
    
  } else if (df_tlm$glb_cvg_diff > threshold & df_tlm$reg_cvg_diff > threshold) {
    # coverage considerably higher than global and regional average
    tlm <- str_glue("In {rev_yr}, {ctryn} {toupper(i)} coverage was {df_tlm %>% filter(year==rev_yr & iso3c==x) %>% pull(coverage)}% - this is {df_tlm %>% filter(year==rev_yr & iso3c==x) %>% pull(comp_cvg_diff_txt)} {comp_yr} and more than {threshold} percentage points higher than both the global and regional averages.")
    
  } else if (df_tlm$glb_cvg_diff > 0 & df_tlm$reg_cvg_diff > 0) {
    # coverage higher than global and regional average
    tlm <- str_glue("In {rev_yr}, {ctryn} {toupper(i)} coverage was {df_tlm %>% filter(year==rev_yr & iso3c==x) %>% pull(coverage)}% - this is {df_tlm %>% filter(year==rev_yr & iso3c==x) %>% pull(comp_cvg_diff_txt)} {comp_yr} and higher than both the global and regional averages.")
    
    
  } else if (df_tlm$comp_cvg_diff <= 0 & df_tlm$glb_cvg_diff > 0) {
    # coverage the same as or lower than 2019 but higher than global
    tlm <- str_glue("In {rev_yr}, {ctryn} {toupper(i)} coverage was {df_tlm %>% filter(year==rev_yr & iso3c==x) %>% pull(coverage)}% - this is {df_tlm %>% filter(year==rev_yr & iso3c==x) %>% pull(comp_cvg_diff_txt)} {comp_yr}, but {df_tlm %>% filter(year==rev_yr & iso3c==x) %>% pull(glb_cvg_diff_txt)}.")
    
  } else {
    tlm <- str_glue("In {rev_yr}, {ctryn} {toupper(i)} coverage was {df_tlm %>% filter(year==rev_yr & iso3c==x) %>% pull(coverage)}% - this is {df_tlm %>% filter(year==rev_yr & iso3c==x) %>% pull(comp_cvg_diff_txt)} {comp_yr} and {df_tlm %>% filter(year==rev_yr & iso3c==x) %>% pull(glb_cvg_diff_txt)}.")
  }
  
  name <- paste0("tlm_cvg_cvg_diff_", df_txt$vaccine, sep = "")
  assign(name , tlm)
}


## top line message ----
# see above



