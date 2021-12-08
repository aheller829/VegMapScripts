# Crosswalking veg map groups across years by species and functional group

# Load require packages
library(dplyr)
library(stringr)
library(tidyr)

# Read in clean datasheets
attributetab1915 <- read.csv("at1915_clean.csv")
attributetab1928 <- read.csv("at1928_clean.csv")
attributetab1998 <- read.csv("at1998_clean.csv")
attributetab2021 <- read.csv("at2021_clean.csv")







# Join 2021 and autocrosswalk at species level
# Look for T/F matches
match_list <- apply(X = autocrosswalk,
                    vegmap = attributetab2021,
                    MARGIN = 1,
                    FUN = function(X, vegmap){
                      current_row <- X
                      current_veg1 <- current_row[["DomSp1"]]
                      
                      veg_matches <- sapply(X = vegmap$DomVeg,
                                            current_veg1 = current_veg1,
                                            
                                            FUN = function(X, current_veg1) {
                                              
                                              vegs <- trimws(unlist(stringr::str_split(X, pattern = ",")))
                                              
                                              any(vegs %in% current_veg1)
                                            })
                      data.frame(Species = current_row[["DomSp1"]],
                                 StateName = current_row[["StateName"]],
                                 StateVegUnite = current_row[["VegUnite"]],
                                 DOMVEG2021 = vegmap$DomVeg,
                                 matches1 = as.integer(veg_matches),
                                 Site = current_row[["Site"]],
                                 SiteName = current_row[["SiteName"]],
                                 esite = vegmap$esite,
                                 StateChar = vegmap$State1Char,
                                 stringsAsFactors = FALSE)
                    })


results <- do.call(rbind,
                   match_list)

# 2nd Dom
match_list2 <- apply(X = autocrosswalk,
                     vegmap = attributetab2021,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg2 <- current_row[["DomSp2"]]
                       
                       veg_matches <- sapply(X = vegmap$DomVeg,
                                             current_veg2 = current_veg2,
                                             
                                             FUN = function(X, current_veg2) {
                                               
                                               vegs <- trimws(unlist(stringr::str_split(X, pattern = ",")))
                                               
                                               any(vegs %in% current_veg2)
                                             })
                       data.frame(Species2 = current_row[["DomSp2"]],
                                  StateName = current_row[["StateName"]],
                                  StateVegUnite = current_row[["VegUnite"]],
                                  DOMVEG2021 = vegmap$DomVeg,
                                  matches2 = as.integer(veg_matches),
                                  Site = current_row[["Site"]],
                                  SiteName = current_row[["SiteName"]],
                                  esite = vegmap$esite,
                                  StateChar = vegmap$State1Char,
                                  stringsAsFactors = FALSE)
                     })


results2 <- do.call(rbind,
                    match_list2)


# 3rd Dom
match_list3 <- apply(X = autocrosswalk,
                     vegmap = attributetab2021,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg3 <- current_row[["DomSp3"]]
                       
                       veg_matches <- sapply(X = vegmap$DomVeg,
                                             current_veg3 = current_veg3,
                                             
                                             FUN = function(X, current_veg3) {
                                               
                                               vegs <- trimws(unlist(stringr::str_split(X, pattern = ",")))
                                               
                                               any(vegs %in% current_veg3)
                                             })
                       data.frame(Species3 = current_row[["DomSp3"]],
                                  StateName = current_row[["StateName"]],
                                  StateVegUnite = current_row[["VegUnite"]],
                                  DOMVEG2021 = vegmap$DomVeg,
                                  matches3 = as.integer(veg_matches),
                                  Site = current_row[["Site"]],
                                  SiteName = current_row[["SiteName"]],
                                  esite = vegmap$esite,
                                  StateChar = vegmap$State1Char,
                                  stringsAsFactors = FALSE)
                     })


results3 <- do.call(rbind,
                    match_list3)



# Join results tables
results$ID <- 1:nrow(results)
results2$ID <- 1:nrow(results2)
results3$ID <- 1:nrow(results3)
fullresults2021 <- results %>%
  dplyr::left_join(results2) %>%
  dplyr::left_join(results3)
fullresults2021 <- dplyr::select(fullresults2021, Site, SiteName, StateName, StateVegUnite, DOMVEG2021, esite, StateChar, matches1, matches2, matches3)


# Count matches
fullresults2021 <- dplyr::mutate(fullresults2021, Tally = matches1 + matches2 + matches3)
fullresults2021 <- dplyr::distinct(fullresults2021)
fullresults2021 <- dplyr::filter(fullresults2021, Tally > 0)
fullresults2021 <- fullresults2021[, c(1, 2, 3, 4, 5, 6, 7, 11)]





















# Species join 1915, 1st dom
# Match 1st dom, then any from vector of subdoms
autocrosswalk <- tidyr::unite(autocrosswalk, SubdomString, DomSp2:Subdom2, na.rm = TRUE, remove = FALSE, sep = ",")
match_list <- apply(X = autocrosswalk,
                     vegmap = attributetab1915,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg1 <- current_row[["DomSp1"]]
                       
                       veg_matches <- sapply(X = vegmap$ZST_DOM,
                                             current_veg1 = current_veg1,
                                             
                                             FUN = function(X, current_veg1) {
                                               
                                               vegs <- X
                                               
                                               
                                               any(vegs %in% current_veg1)
                                             })
                       data.frame(Site = current_row[["Site"]],
                                  SiteName = current_row[["SiteName"]],
                                  StateName = current_row[["StateName"]],
                                  GSName = current_row[["GeneralizedStateName"]],
                                  GSNumber = current_row[["GeneralizedStateNumber"]],
                                  StateVegUnite = current_row[["FGUnite"]],
                                  VegUnite = current_row[["VegUnite"]],
                                  FG1915 = vegmap$FGUnite,
                                  SP1915 = vegmap$VegUnite,
                                  esite = vegmap$ecosite1,
                                  OBJECTID1915 = vegmap$OBJECTID,
                                  matches1 = as.integer(veg_matches),
                                  stringsAsFactors = FALSE)
                     })


results <- do.call(rbind,
                    match_list)


# Species join, dom2
match_list2 <- apply(X = autocrosswalk,
                     vegmap = attributetab1915,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg1 <- current_row[["DomSp2"]]
                       
                       veg_matches <- sapply(X = vegmap$SubdomString,
                                             current_veg1 = current_veg1,
                                             
                                             FUN = function(X, current_veg1) {
                                               
                                               vegs <- trimws(unlist(stringr::str_split(X, pattern = ",")))
                                               
                                               any(vegs %in% current_veg1)
                                             })
                       data.frame(Site = current_row[["Site"]],
                                  SiteName = current_row[["SiteName"]],
                                  StateName = current_row[["StateName"]],
                                  OBJECTID1915 = vegmap$OBJECTID,
                                  matches2 = as.integer(veg_matches),
                                  stringsAsFactors = FALSE)
                     })


results2 <- do.call(rbind,
                    match_list2)


# Species join, dom3
match_list3 <- apply(X = autocrosswalk,
                     vegmap = attributetab1915,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg1 <- current_row[["DomSp3"]]
                       
                       veg_matches <- sapply(X = vegmap$SubdomString,
                                             current_veg1 = current_veg1,
                                             
                                             FUN = function(X, current_veg1) {
                                               
                                               vegs <- trimws(unlist(stringr::str_split(X, pattern = ",")))
                                               
                                               any(vegs %in% current_veg1)
                                             })
                       data.frame(Site = current_row[["Site"]],
                                  SiteName = current_row[["SiteName"]],
                                  StateName = current_row[["StateName"]],
                                  OBJECTID1915 = vegmap$OBJECTID,
                                  matches3 = as.integer(veg_matches),
                                  stringsAsFactors = FALSE)
                     })


results3 <- do.call(rbind,
                    match_list3)



resultsjoined <- results %>%
  dplyr::left_join(results2) %>%
  dplyr::left_join(results3) %>%
  dplyr::filter(matches1 > 0) %>%
  dplyr::mutate(Tally = matches1 + matches2 + matches3)

resultsjoined <- resultsjoined %>%
  group_by(OBJECTID1915) %>%
  slice_max(order_by = Tally, with_ties = TRUE)









# Species join 1928, 1st dom
# Match 1st dom, then any from vector of subdoms
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
                                              
                                              
                                              any(vegs %in% current_veg1)
                                            })
                      data.frame(Site = current_row[["Site"]],
                                 SiteName = current_row[["SiteName"]],
                                 StateName = current_row[["StateName"]],
                                 GSName = current_row[["GeneralizedStateName"]],
                                 GSNumber = current_row[["GeneralizedStateNumber"]],
                                 StateVegUnite = current_row[["FGUnite"]],
                                 VegUnite = current_row[["VegUnite"]],
                                 FG1928 = vegmap$Dom1FG,
                                 SP1928 = vegmap$Name,
                                 esite = vegmap$ecosite1,
                                 OBJECTID1928 = vegmap$OBJECTID,
                                 matches1 = as.integer(veg_matches),
                                 stringsAsFactors = FALSE)
                    })


results <- do.call(rbind,
                   match_list)

results <- dplyr::filter(results, matches1 > 0)
