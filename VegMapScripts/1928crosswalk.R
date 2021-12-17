# Crosswalking veg map groups across years by functional group

# Load require packages
library(dplyr)
library(stringr)
library(tidyr)

# Read in clean datasheets
autocrosswalk <- read.csv("autocrosswalk_clean.csv")

attributetab1928 <- read.csv("at1928_clean.csv")


# How many functional group equivalents are there?
functionalgrouptab_site <- autocrosswalk %>%
  dplyr::group_by(Site, SiteName, StateName, FGUnite) %>%
  dplyr::summarise(Count = n())
# How many when grouping by generalized states?
functionalgrouptab_GS <- autocrosswalk %>%
  dplyr::group_by(GeneralizedStateNumber, GeneralizedStateName, FGUnite) %>%
  dplyr::summarise(Count = n())



# Second join on species? Or match on species?
# Species join, dom1
match_list <- apply(X = autocrosswalk,
                     vegmap = attributetab1928,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg1 <- current_row[["DomSp1"]]
                       
                       veg_matches <- sapply(X = vegmap$Name,
                                             current_veg1 = current_veg1,
                                             
                                             FUN = function(X, current_veg1) {
                                               
                                               vegs <- X
                                               
                                               current_veg1 <- trimws(unlist(stringr::str_split(current_veg1, pattern = ",")))
                                               
                                               
                                               any(vegs %in% current_veg1)
                                             })
                       data.frame(OBJECTID1928 = vegmap$OBJECTID,
                                  FG1928 = vegmap$Dom1FG,
                                  SP1915 = vegmap$Name,
                                  esite = vegmap$ecosite1,
                                  GeneralizedStateName = current_row[["GeneralizedStateName"]],
                                  GeneralizedStateNum = current_row[["GeneralizedStateNumber"]],
                                  Site = current_row[["Site"]],
                                  SiteName = current_row[["SiteName"]],
                                  StateName = current_row[["StateName"]],
                                  StateVegUnite = current_row[["FGUnite"]],
                                  VegUnite = current_row[["VegUnite"]],
                                  matches = as.integer(veg_matches),
                                  stringsAsFactors = FALSE)
                     })


results1928 <- do.call(rbind,
                    match_list)

# Site match
results1928 <- dplyr::filter(results1928, matches > 0 & esite == SiteName | 
                               FG1928 == GeneralizedStateName)
