#### WIISE API funcs ####
#	This script sets up connection to the WIISE API
#	At some point, the WIISE API token will expire. When this happens, email Yoann (nedelecy@who.int) for renewed credentials and then re-run the script.


# WIISE Mart authentication
get_token <- function(tkn_type = "prod") {
  if (tkn_type == "prod") {
    # xMart PROD [https://extranet.who.int/xmart-api/odata/{mart_code}]
    AzureAuth::get_azure_token(resource = "712b0d0d-f9c5-4b7a-80d6-8a83ee014bca", 
                               tenant = "f610c0b7-bd24-4b39-810b-3dc280afb590", 
                               app = "712b0d0d-f9c5-4b7a-80d6-8a83ee014bca",
                               auth_type = "authorization_code",
                               password = "qQKa]APZ_0q.OwO.Oq1H3ndnFNsa16u7",
                               use_cache = TRUE)
    } else {
    #xMart UAT [https://portal-uat.who.int/xmart-api/odata/{mart_code}]
    AzureAuth::get_azure_token(resource = "b85362d6-c259-490b-bd51-c0a730011bef", 
                               tenant = "f610c0b7-bd24-4b39-810b-3dc280afb590", 
                               app = "b85362d6-c259-490b-bd51-c0a730011bef",
                               auth_type="authorization_code",
                               password = "utNZAZb8823NaRexQl[VPU=gK[YD/H1E",
                               use_cache = TRUE)
    }
  }

# pull WIISE Mart data
fetch_wiise <- function(url, tbl, qry = NULL, token) {
  # set headers
  hdrs <- httr::add_headers(Authorization = stringr::str_c("Bearer", token$credentials$access_token, sep = " "))
  # clean query string
  # qry <- URLencode(stringr::str_c("?$", qry)) # `?$format=csv` no-limit records-per-request
  # reguest & pull data
  resp <- httr::GET(stringr::str_c(url, tbl, URLencode(qry)), hdrs)
  if (httr::http_error(resp)) { 
    httr::message_for_status(resp) 
    } else {
    resp %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON()
    }
}

library(dplyr)
