##### LISTS ALL OF THE WIISE TABLES AVAILABLE ##### 
# set working directory to Immunization OneDrive
USERNAME <- Sys.getenv("USER")
wd <- file.path("/Users", USERNAME, "Library/CloudStorage/OneDrive-SharedLibraries-UNICEF/Health-HIV Data & Analytics - For-Grace")
utils <- file.path(wd, "utils")

#Read in functions 
source(file.path(wd, "wiise/00_wiise-funcs.R"))


## API tokens ----
tkn          <- get_token(tkn_type = "prod")
base_url     <- "https://extranet.who.int/xmart-api/odata/WIISE/"


## func :: list all WIISE tables ----
list_wiise_tables <- function(base_url, tkn) {
  
  # Auth header
  hdrs <- httr::add_headers(
    Authorization = stringr::str_c(
      "Bearer",
      tkn$credentials$access_token,
      sep = " "
    )
  )
  
  # Get metadata
  resp <- httr::GET(
    url = paste0(base_url, "$metadata"),
    hdrs
  )
  
  if (httr::http_error(resp)) {
    httr::stop_for_status(resp)
  }
  
  meta_xml <- httr::content(resp, as = "text", encoding = "UTF-8") %>%
    xml2::read_xml()
  
  # Extract EntitySets 
  entity_sets <- xml2::xml_find_all(
    meta_xml,
    ".//*[local-name()='EntitySet']"
  )
  
  tbl_df <- tibble(
    table_name = xml2::xml_attr(entity_sets, "Name"),
    entity_type = xml2::xml_attr(entity_sets, "EntityType")
  ) %>%
    arrange(table_name)
  
  return(tbl_df)
}


## wiise tables list ----
wiise_tables <- list_wiise_tables(base_url, tkn)

print(wiise_tables, n = Inf)
