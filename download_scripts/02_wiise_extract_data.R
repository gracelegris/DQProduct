library(tidyverse)
library(stringr)

rev_yr <- 2025

########## CONNECT TO WIISE ########## 
# set working directory to For-Grace OneDrive
USERNAME <- Sys.getenv("USER")
UserDir <- file.path("/Users", USERNAME, "Library/CloudStorage/OneDrive-SharedLibraries-UNICEF")
wd <- file.path("/Users", USERNAME, "Library/CloudStorage/OneDrive-SharedLibraries-UNICEF/Health-HIV Data & Analytics - For-Grace")
utils <- file.path(wd, "utils")
dq_folder <- file.path(UserDir, "Health-HIV Data & Analytics - 2025 rev/unicef-products/dummy/data-quality")

#Read in functions and label values
source(file.path(wd, "wiise/00_wiise-funcs.R"))
source(file.path(wd, "wiise/label_vals.R"))


## set up ----
reg_ref <- read_csv(file.path(utils, paste0('regional-groups_', rev_yr + 1, '-release.csv'))) %>% 
  select(iso3c, country, region_unicef_ops, wuenic)

# list of unicef/wuenic countries (195)
wuenic_list <- pull(filter(reg_ref, !is.na(wuenic)), iso3c)

incl_vacc <- str_flatten(c("BCG","DTPCV1","DTPCV3","HEPB_BD$","HEPB3","HIB3",
                           "IPV(1|2)$","IPV2_FRAC","IPVC",
                           "MCV(1|2)","MEN_A_CONJ","MENA_C","PCV3","PCVC","POL3","ROTAC","YFV"), "|")

#Run this to see the different naming conventions between wiise and wuenic
vacc_ref <- read_csv(file.path(wd, "wiise/wiise-to-mdb_vacc.csv")) 



## API tokens ----
tkn          <- get_token(tkn_type = "prod")
base_url     <- "https://extranet.who.int/xmart-api/odata/WIISE/"



## JRF dta [WIISE] :: vaccine schedule ----
qry_sched <- str_glue("?$filter=YEAR ge 2020")   # table filters
  
wiise_sched <- fetch_wiise(
  url = base_url, 
  tbl = "AD_SCHEDULES",   # put name of table you want to extract
  qry = qry_sched, 
  token = tkn
) %>% 
  .[['value']] %>% 
  as_tibble() %>%
  janitor::clean_names() %>%
  mutate(iso3c = tolower(country)) %>%
  select(iso3c, year, vaccinecode, schedulerounds, targetpop, geoarea, ageadministered, sourcecomment, commit_dt = sys_commit_date_utc)





## JRF dta [WIISE] :: admin and official data ----
#This function grabs the coverage (admin and official) and the numerator denominator data (admin only) from wiise
grab_cvg_numdum <- function(df) {
  df %>% 
    filter(str_detect(VACCINECODE, incl_vacc)) %>% # grab only WUENIC vacc
    mutate(PERCENTAGE = round(PERCENTAGE, 0),
           updated = lubridate::today(), 
           updatedBy = "name",
           first_commit_dt = lubridate::ymd_hms(Sys_FirstCommitDateUtc),
           commit_dt = lubridate::ymd_hms(Sys_CommitDateUtc),
           is_update = commit_dt > first_commit_dt) %>% 
    left_join(vacc_ref, by = c("VACCINECODE" = "wiise_vacc")) %>% 
    select(country = COUNTRY, vaccine = mdb_vacc, annum = YEAR, type = COVERAGE_CATEGORY, 
           reportedDenom = TARGETNUMBER, reportedNum = DOSES,
           coverage = PERCENTAGE, comment = LEG_COMMENT, 
           updated, updatedBy, 
           first_commit_dt, commit_dt, is_update) %>% 
    mutate_at(vars(type), str_to_lower) %>% 
    mutate_if(is.character, replace_na, "") %>% 
    mutate(reportedDenom = ifelse(type == 'admin', reportedDenom, NA_real_),
           reportedNum = ifelse(type == 'admin', reportedNum, NA_real_)
           # vaccine = ifelse(vaccine == "IPV2_FRAC", "IPV1", vaccine)  # new 2024 :: convert IPV2_FRAC to IPV1
    ) %>%  
    arrange(country, type, vaccine)
}

# `PERCENTAGE ge 0` drops NAs -4444 -9999 etc; exclude 'delayed' doses [admin-only]
odta_qry <- str_glue("?$filter=(COVERAGE_CATEGORY eq 'ADMIN' or COVERAGE_CATEGORY eq 'OFFICIAL') 
                      and PERCENTAGE ge 0 
                      and TIMELYDOSES ne 'DELAYED'
                      and YEAR ge 2010")

wiise_dta <- fetch_wiise(
  url = base_url, 
  tbl = "AD_COVERAGES", 
  qry = odta_qry, 
  token = tkn
) %>% 
  .[['value']] %>% 
  as_tibble()

wiise_admin_official <- 
  grab_cvg_numdum(wiise_dta) %>% 
  filter(country %in% wuenic_list)

write.csv(wiise_admin_official, file.path(dq_folder, paste0("wiise_admin_official_", rev_yr + 1, "-release.csv")), row.names = FALSE)
