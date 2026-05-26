###### ctry_comparison :: country coverage :: bar chart ######
## prep data ----
if (regn == "ROSA") {
  n_rank_bar <- 5
} else {
  n_rank_bar <- 10
} 

# geom_text sise
if (regn %in% c("LACR", "Non-programme")) {
  txt_sze <- 3.5
} else {
  txt_sze <- 4
}

# dtp1, dtp3 and mcv1 by country, latest year
ctry_cvg <- wueniclatestrev %>%
  filter(lvl_1=='country',
         lvl_2 == 'region_unicef_ops',
         lvl_3 == regn,
         year == rev_yr,
         vaccine %in% c('dtp3','dtp1','mcv1')) %>%
  # join translated country names
  translate_ctry_names() %>%
  select(iso3c, country, vaccine, year, coverage, unvaccinated) %>%
  distinct() %>%
  arrange(vaccine, desc(unvaccinated)) %>%
  group_by(vaccine, year) %>%
  mutate(rank = seq(n())) %>%
  ungroup() %>%
  mutate(year=as.character(year),
         iso3c = toupper(iso3c),
         type = case_when(vaccine == 'dtp1' ~ get_text("unvacc_dtp1", language),
                          vaccine == 'dtp3' ~ get_text("unvacc_dtp3", language),
                          vaccine == 'mcv1' ~ get_text("unvacc_mcv1", language)),
         top10 = case_when(vaccine == 'dtp1' & rank <= n_rank_bar ~ get_text("plt_ctry_comparison_top", language),
                           vaccine == 'dtp3' & rank <= n_rank_bar ~ get_text("plt_ctry_comparison_top", language),
                           vaccine == 'mcv1' & rank <= n_rank_bar ~ get_text("plt_ctry_comparison_top", language),
                           vaccine == 'dtp1' & rank > n_rank_bar ~ get_text("plt_ctry_comparison_nottop", language),
                           vaccine == 'dtp3' & rank > n_rank_bar ~ get_text("plt_ctry_comparison_nottop", language),
                           vaccine == 'mcv1' & rank > n_rank_bar ~ get_text("plt_ctry_comparison_nottop", language)
                           # TRUE ~ str_glue('Not top {n_rank_bar}')
         )) %>%
  distinct() %>%
  arrange(vaccine, coverage) %>%
  group_by(vaccine, year) %>%
  ungroup() %>%
  mutate(legend = ifelse(tolower(iso3c) == x, country, str_glue("Other countries in {regn}")))

ctry_cvg$legend <- factor(ctry_cvg$legend, levels = c(ctryn,str_glue("Other countries in {regn}")))


## plot ----
for (i in c("dtp1","dtp3","mcv1")) {
  
  df <- ctry_cvg %>% filter(vaccine == i)
  
  # dtp1
  gg <-
    df %>% 
    ggplot() +
    geom_col(aes(x=reorder(country, coverage), y=coverage, fill=legend), colour = "black") +
    geom_col_pattern(
      df %>% filter(grepl("Top", top10)),
      mapping = aes(x=reorder(country, coverage), y=coverage,
                    pattern = top10, fill=legend), 
      colour  = 'black',
      pattern_angle = 45,
      pattern_spacing = 0.02,
      pattern_key_scale_factor = 0.6,
      linewidth = 0.05,
      pattern_density = 0.1,
      pattern_fill  = 'black') +
    scale_pattern_manual(values = c('stripe')) +
    theme_minimal() +
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1, size=11),
          axis.text.y = element_text(size=11),
          legend.title=element_blank(),
          panel.grid.major.x = element_blank(),
          legend.text=element_text(size=11)) +
    geom_text(data = df,
              aes(country, coverage, label = coverage),
              # position = position_stack(vjust = 1.06),
              vjust = -0.8,
              color = 'black', size = txt_sze) +
    scale_y_continuous(breaks = seq(0, 200, by = 25), limits = c(0,105)) +
    scale_fill_manual(values = c("#F26A21", "#0083CF")) +
    guides(pattern = guide_legend(reverse=TRUE,
                                  override.aes = list(fill = "white")),
           fill = "none")  +  # hide colour legend
    geom_label(data = df %>% filter(rank<=n_rank_bar), aes(x=reorder(country, coverage), y=coverage/2, label = rank), nudge_y = 1, size = 3) +
    labs(x = '', y = get_text("coverage", language), 
         title = get_text("plt_ctry_comparison_title", language),
         caption = get_text("plt_ctry_comparison_cpt", language))
  
  # assign the ggplot chart with a name based on the vaccine
  name <- paste0("plt_ctry_comparison_", df$vaccine, sep = "")
  assign(name , gg)
  
}


## narrative ----
for (i in c("dtp1","dtp3","mcv1")) {
  df_txt <- ctry_cvg %>%
    filter(vaccine == i) %>%
    mutate(type = case_when(vaccine == 'dtp1' ~ 'zero-dose',
                            vaccine == 'dtp3' ~ 'un- and undervaccinated',
                            vaccine == 'mcv1' ~ 'unvaccinated'),
           cvg_gt90 = ifelse(coverage >= 90, 1, 0),
           cvg_gt90_sum = sum(cvg_gt90),
           cvg_gt90_pcnt = round((cvg_gt90_sum / n_distinct(country)) * 100,0),
           cvg_gt90_topzd = ifelse(coverage >= 90 & rank <= n_rank_bar, 1, 0),
           cvg_gt90_topzd_sum = sum(cvg_gt90_topzd),
           unvacc_rank_txt = case_when(rank <= n_rank_bar ~ str_glue('in the top {n_rank_bar} countries with the most {type} children (rank={rank})'),
                                       TRUE ~ str_glue('not in the top {n_rank_bar} countries with the most {type} children')))
  
  # produce coverage rank
  df_txt <- df_txt %>%
    arrange(coverage) %>%
    mutate(rank_cvg = min_rank(coverage))
  
  # note:: first and last sentences are in the translation table
  txt <- str_glue("In {rev_yr}, {ctryn} ranked number {df_txt %>% filter(country==ctryn) %>% pull(rank_cvg)} out of {n_distinct(df_txt$iso3c)} countries for lowest {toupper(i)} coverage (based on tied ranks).\n
{ctryn} was {df_txt %>% filter(country==ctryn) %>% pull(unvacc_rank_txt)}.")
  
  # fix text for non-programme countries
  if (regn == "Non-programme") {
    txt <- gsub("in countries in Non-programme", "across non-programme countries", txt)
    # Use gsub to replace any two-digit number followed by " with" with the number followed by "non-programme with"
    txt <- gsub("(\\b\\d{2})(\\s+countries\\s+with)", "\\1 non-programme\\2", txt)
  } else {
    
  }
  name <- paste0("txt_ctry_comparison_", df_txt$vaccine, sep = "")
  assign(name , txt)


# top line message
df_tlm <- df_txt %>% filter(iso3c == toupper(x))

# text based on different scenarios
if (df_tlm$rank_cvg == 1) {
  # lowest coverage (various scenarios for unvaccinated)
  if (df_tlm$rank == 1) {
    tlm <- str_glue("In {rev_yr}, {ctryn} had the lowest coverage and the highest number of {df_tlm$type} children in the region, highlighting it as a key priority country for support.")
  } else if (df_tlm$rank <= 3) {
    tlm <- str_glue("In {rev_yr}, {ctryn} had the lowest coverage and was among the top 3 countries with the highest number of {df_tlm$type} children in the region, highlighting it as a key country for support.")
  } else if (df_tlm$rank <= n_rank_bar) {
    tlm <- str_glue("In {rev_yr}, {ctryn} had the lowest coverage and was among the top {n_rank_bar} countries with the highest number of {df_tlm$type} children in the region.")
  } else {
    tlm <- str_glue("In {rev_yr}, {ctryn} had the lowest coverage, but was not among the top {n_rank_bar} countries with the highest number of {df_tlm$type} children in the region.")
  }
  
} else if(df_tlm$rank_cvg %in% c(2:3)) {
  # among the lowest coverage (various scenarios for unvaccinated)
  if (df_tlm$rank == 1) {
    tlm <- str_glue("In {rev_yr}, {ctryn} was among the countries with the lowest coverage and had the highest number of {df_tlm$type} children in the region, highlighting it as a key country requiring support.")
  } else if (df_tlm$rank <= 3) {
    tlm <- str_glue("In {rev_yr}, {ctryn} was among the countries with the lowest coverage and was in the top 3 countries with the highest number of {df_tlm$type} children in the region, highlighting it as a country requiring support.")
  } else if (df_tlm$rank <= n_rank_bar) {
    tlm <- str_glue("In {rev_yr}, {ctryn} was among the countries with the lowest coverage and in the top {n_rank_bar} countries with the highest number of {df_tlm$type} children in the region.")
  } else {
    tlm <- str_glue("In {rev_yr}, {ctryn} was among the countries with the lowest coverage, but was not among the top {n_rank_bar} countries with the highest number of {df_tlm$type} children in the region.")
  } 
  
} else if (df_tlm$rank_cvg == max(df_txt$rank_cvg)) {
  # highest coverage (various scenarios for unvaccinated)
  if (df_tlm$rank == 1) {
    tlm <- str_glue("In {rev_yr}, {ctryn} had the highest coverage, but also the highest number of {df_tlm$type} children in the region, contributing the most to the {df_tlm$type} burden.")
  } else if (df_tlm$rank <= 3) {
    tlm <- str_glue("In {rev_yr}, {ctryn} had the highest coverage, but was among the top 3 countries with the highest number of {df_tlm$type} children in the region, contributing substantially to the {df_tlm$type} burden.")
  } else if (df_tlm$rank <= n_rank_bar) {
    tlm <- str_glue("In {rev_yr}, {ctryn} had the highest coverage, but was among the top {n_rank_bar} countries with the highest number of {df_tlm$type} children in the region.")
  } else {
    tlm <- str_glue("In {rev_yr}, {ctryn} had the highest coverage and was not among the top {n_rank_bar} countries with the highest number of {df_tlm$type} children in the region.")
  }
  
} else if(df_tlm$rank_cvg %in% c(max(df_txt$rank_cvg)-1:max(df_txt$rank_cvg)-3)) {
  # among the lowest coverage (various scenarios for unvaccinated)
  if (df_tlm$rank == 1) {
    tlm <- str_glue("In {rev_yr}, {ctryn} was among the countries with the highest coverage, but had the highest number of {df_tlm$type} children in the region, contributing the most to the {df_tlm$type} burden.")
  } else if (df_tlm$rank <= 3) {
    tlm <- str_glue("In {rev_yr}, {ctryn} was among the countries with the highest coverage, but was in the top 3 countries with the highest number of {df_tlm$type} children in the region, contributing substantially to the {df_tlm$type} burden.")
  } else if (df_tlm$rank <= n_rank_bar) {
    tlm <- str_glue("In {rev_yr}, {ctryn} was among the countries with the highest coverage, but in the top {n_rank_bar} countries with the highest number of {df_tlm$type} children in the region.")
  } else {
    tlm <- str_glue("In {rev_yr}, {ctryn} was among the countries with the highest coverage and was not among the top {n_rank_bar} countries with the highest number of {df_tlm$type} children in the region.")
  } 
  
  # not among highest or lowest coverage
} else if (df_tlm$rank <= 3) {
  tlm <- str_glue("In {rev_yr}, {ctryn} had {toupper(i)} coverage of {df_tlm$coverage}% and was among the top 3 countries with the highest number of {df_tlm$type} children in the region.")
} else if (df_tlm$rank <= n_rank_bar) {
  tlm <- str_glue("In {rev_yr}, {ctryn} had {toupper(i)} coverage of {df_tlm$coverage}% and was among the top {n_rank_bar} countries with the highest number of {df_tlm$type} children in the region.")
} else {
  tlm <- str_glue("In {rev_yr}, {ctryn} had {toupper(i)} coverage of {df_tlm$coverage}% and was not among the top {n_rank_bar} countries with the highest number of {df_tlm$type} children in the region.")
}

name <- paste0("tlm_ctry_comparison_", df_txt$vaccine, sep = "")
assign(name , tlm)
}


## top line message ----
# see above