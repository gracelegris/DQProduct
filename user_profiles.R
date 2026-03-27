# set up user profiles

USERNAME    <- Sys.getenv("USERNAME")
USERPROFILE <- Sys.getenv("USERPROFILE")
USERNAME <- Sys.getenv("USER")

# if (USERNAME == "laure"){
#   wd <- "C:/Users/laure/OneDrive - UNICEF"
# }


# if (USERNAME == "ykawa"){
#   prg_folder <- "C:/Users/ykawa/UNICEF/Health-HIV Data & Analytics - Documents/Immunization/Projects and products/Subnational data analysis" 
#   wd         <- "C:/Users/ykawa/UNICEF/Health-HIV Data & Analytics - Documents"  
#   ahead      <- "C:/Users/ykawa/UNICEF/Health-HIV Data & Analytics - Documents/UNICEF Ahead Database/01 Administrative Data"
#   git        <- "C:/Users/ykawa/GitHub/AHEAD"
# }
# 
# if (USERNAME == "okiya"){
#   prg_folder <- "C:/Users/okiya/UNICEF/Health-HIV Data & Analytics - Subnational data analysis" 
#   #wd         <- ""  
#   #ahead      <- ""
#   #git        <- ""
# }
# 
# if (USER == "UNICEF") { # grace
#   prg_folder <- "/Users/UNICEF/Library/CloudStorage/OneDrive-SharedLibraries-UNICEF/Health-HIV Data & Analytics - Subnational data analysis" 
#   EthDir <- "/Users/UNICEF/Documents/unicef_local/ethiopia_cleaning"
# }

# # Pre-create directory for AzureAuth
# dir.create("~/Library/Application Support/AzureR", recursive = TRUE, showWarnings = FALSE)
# options(azure_auth_create_dir = TRUE)

# file paths
UserDir <- file.path("/Users", USERNAME, "Library/CloudStorage/OneDrive-SharedLibraries-UNICEF")
wd <- file.path("/Users", USERNAME, "Library/CloudStorage/OneDrive-SharedLibraries-UNICEF/Health-HIV Data & Analytics - For-Grace")
dq_folder <- file.path(UserDir, "Health-HIV Data & Analytics - 2025 rev/unicef-products/dummy/data-quality")
PrjDir <- file.path(dq_folder, "DQProduct")
DataDir <- file.path(PrjDir, "data")
SubnatFuncDir <- file.path(UserDir, "Health-HIV Data & Analytics - Subnational data analysis/utils/R")
DummyUtils <- file.path(UserDir, "Health-HIV Data & Analytics - 2025 rev/unicef-products/dummy/utils")
UtilsDir <- file.path(wd, "utils")
OutputDir <- file.path(PrjDir, "outputs")
RevDir <- str_glue(file.path(UserDir, "Health-HIV Data & Analytics - {rev_yr} rev"))
wiisefolder <- str_glue(file.path(RevDir, "unicef-products/{type}/wiise-outputs"))
DummyDataDir <- str_glue(file.path(RevDir, "wuenic_master/dummy"))

# Load libraries
library(tidyverse)
library(readxl)
library(openxlsx)
library(AzureAuth)
library(httr)
library(curl)
library(ggplot2)
#library(ggrepel)
library(writexl)
library(sf)        # For spatial data operations
library(dplyr)     # For data manipulation
library(stringi)   # For string operations
library(stringr)   # For string formatting
library(readr)     # For reading CSV/text files
library(tidyr)     # For data tidying and reshaping
library(tmap)      # For creating thematic spatial maps
library(lwgeom)    # For advanced spatial analysis
library(purrr)
library(arrow)
library(zoo)
library(scales)
library(cowplot)
library(gt)
library(janitor)
library(here)
library(ggpattern)   # for hatched bars
library(patchwork)   # for combining charts
#library(ggbump)      # for rank chart
library(gghighlight)
library(officer)     # export to ppt
library(rvg)         # export to ppt
library(export)      # export to ppt
library(ggpubr)      # save to pdf
library(pals)        # distinct colour palettes: https://stackoverflow.com/questions/9563711/r-color-palettes-for-many-data-classes
library(ggtext)      # fixes alignment on geom_richtext when adjusting vjust :: geom_richtext()
library(showtext)
library(lubridate)
library(flextable)
library(kableExtra)
library(webshot2)

font_add_google("Roboto", "roboto")
showtext_auto()

# highlight styles for powerpoint
highlightStyle1 <- openxlsx::createStyle(fgFill = "#FFFFCC") # Use bgFill instead of bgColor
highlightStyle2 <- openxlsx::createStyle(fgFill = "#FFC20E") # Use bgFill instead of bgColor
highlightStyle3 <- openxlsx::createStyle(fgFill = "#F26A21") # Use bgFill instead of bgColor
highlightStyle4 <- openxlsx::createStyle(fgFill = "#E2231A") # Use bgFill instead of bgColor

unicef_colors <- c("#0058AB","#1CABE2","#00833D","#80BD41","#6A1E74",
                   "#961A49","#E2231A","#F26A21","#FFC20E","#FFF09C")

theme_unicef <- function() {
  theme_bw() + 
    theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(face = "plain", size = 13, hjust = 0.5),
      axis.text = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 12, colour = 'black'),
      legend.text = element_text(size = 12, colour = 'black'),
      legend.key.height = unit(1, "cm")
    )
}


'%!in%' <- function(x,y)!('%in%'(x,y))

