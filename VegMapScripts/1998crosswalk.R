# Crosswalking veg map groups across years by functional group

# Load require packages
library(dplyr)
library(stringr)
library(tidyr)

# Read in clean datasheets
attributetab1998 <- read.csv("at1998_clean.csv")
spgroups_site <- read.csv("spgroups_site.csv")

# Full species match
results1998 <- attributetab1998 %>%
  dplyr::left_join(spgroups_site) %>%
  dplyr::select(FID, OBJECTID1998 = OBJECTID, VegUnite, 
                ecosite1, ecosite2, ecosite3, GeneralizedStateNumber, SiteName)
# Remove those that didn't match
results1998 <- dplyr::filter(results1998, !is.na(GeneralizedStateNumber) & GeneralizedStateNumber != 0)

# Full match
results1998_fullmatch <- dplyr::filter(results1998, SiteName == ecosite1 |
                                         SiteName == ecosite2 | SiteName == ecosite3)

# Write to csv
write.csv(results1998_fullmatch, "results1998_fullmatch.csv", row.names = FALSE)

# Read in edited
results_fullmatch_edited <- read.csv("results1998_fullmatch_edited.csv")
# Subset attribute table to include unmatched
at1998_mod <- subset(attributetab1998, !(attributetab1998$OBJECTID %in% results_fullmatch_edited$OBJECTID1998))



# Join on specific species
# First split species in master sp assemblage list

# Species join, dom1
match_list <- apply(X = spgroups_site,
                    vegmap = at1998_mod,
                    MARGIN = 1,
                    FUN = function(X, vegmap){
                      current_row <- X
                      current_veg1 <- current_row[["VegUnite"]]
                      
                      veg_matches <- sapply(X = vegmap$DOM1,
                                            current_veg1 = current_veg1,
                                            
                                            FUN = function(X, current_veg1) {
                                              
                                              vegs <- trimws(unlist(stringr::str_split(X, pattern = ",")))
                                              
                                              
                                              any(vegs %in% current_veg1)
                                            })
                      data.frame(SiteName = current_row[["SiteName"]],
                                 VegUnite = current_row[["VegUnite"]],
                                 GeneralizedStateNum = current_row[["GeneralizedStateNumber"]],
                                 SP1998 = vegmap$VegUnite,
                                 ecosite1 = vegmap$ecosite1,
                                 ecosite2 = vegmap$ecosite2,
                                 ecosite3 = vegmap$ecosite3,
                                 OBJECTID1998 = vegmap$OBJECTID,
                                 matches = as.integer(veg_matches),
                                 stringsAsFactors = FALSE)
                    })


results <- do.call(rbind,
                   match_list)


# Species join, dom2
match_list2 <- apply(X = spgroups_site,
                     vegmap = at1998_mod,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg1 <- current_row[["VegUnite"]]
                       
                       veg_matches <- sapply(X = vegmap$DOM2,
                                             current_veg1 = current_veg1,
                                             
                                             FUN = function(X, current_veg1) {
                                               
                                               
                                               vegs <- trimws(unlist(stringr::str_split(X, pattern = ",")))
                                               
                                               
                                               
                                               any(vegs %in% current_veg1)
                                             })
                       data.frame(SiteName = current_row[["SiteName"]],
                                  OBJECTID1998 = vegmap$OBJECTID,
                                  matches2 = as.integer(veg_matches),
                                  stringsAsFactors = FALSE)
                     })


results2 <- do.call(rbind,
                    match_list2)
# Join results tables
SPmatch1998 <- results %>%
  dplyr::left_join(results2) 
# Count matches
SPmatch1998 <- dplyr::mutate(SPmatch1998, SPTally = matches + matches2)
SPmatch1998 <- dplyr::filter(SPmatch1998, SPTally > 0)
SPmatch1998 <- dplyr::distinct(SPmatch1998)
# Join matches based on species with FG assemblage match
SPmatch1998 <- dplyr::distinct(SPmatch1998)
# Reorder variables
fullresults1998 <- dplyr::select(SPmatch1998, OBJECTID1998, ecosite1, ecosite2,
                                 ecosite3, SP1998, FG1998,
                                 FG_GSNum,
                                 SPTally, GeneralizedStateNum,
                                 GeneralizedStateName, VegUnite, SiteName, Site)






names(SPmatch1998)

# Keep generalized state matches
fullresults1998 <- dplyr::filter(fullresults1998, FG_GSNum == GeneralizedStateNum)
# Keep site matches
fullresults1998_sitematch <- dplyr::filter(fullresults1998, ecosite1 == SiteName |
                                             ecosite2 == SiteName | ecosite3 == SiteName)















# Second join on species? Or match on species?
# Species join, dom1
match_list <- apply(X = autocrosswalk,
                     vegmap = at1998_mod,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg1 <- current_row[["DomSp1"]]
                       
                       veg_matches <- sapply(X = vegmap$VegUnite,
                                             current_veg1 = current_veg1,
                                             
                                             FUN = function(X, current_veg1) {
                                               
                                               vegs <- trimws(unlist(stringr::str_split(X, pattern = ",")))
                                               
                                               
                                               any(vegs %in% current_veg1)
                                             })
                       data.frame(Site = current_row[["Site"]],
                                  SiteName = current_row[["SiteName"]],
                                  StateName = current_row[["StateName"]],
                                  StateVegUnite = current_row[["FGUnite"]],
                                  VegUnite = current_row[["VegUnite"]],
                                  GeneralizedStateName = current_row[["GeneralizedStateName"]],
                                  GeneralizedStateNum = current_row[["GeneralizedStateNumber"]],
                                  FG1998 = vegmap$FGUnite,
                                  SP1998 = vegmap$VegUnite,
                                  ecosite1 = vegmap$ecosite1,
                                  ecosite2 = vegmap$ecosite2,
                                  ecosite3 = vegmap$ecosite3,
                                  OBJECTID1998 = vegmap$OBJECTID,
                                  matches = as.integer(veg_matches),
                                  stringsAsFactors = FALSE)
                     })


results <- do.call(rbind,
                    match_list)


# Species join, dom2
match_list2 <- apply(X = autocrosswalk,
                     vegmap = at1998_mod,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg1 <- current_row[["DomSp2"]]
                       
                       veg_matches <- sapply(X = vegmap$VegUnite,
                                             current_veg1 = current_veg1,
                                             
                                             FUN = function(X, current_veg1) {
                                               
                                               
                                               vegs <- trimws(unlist(stringr::str_split(X, pattern = ",")))
                                               
                                               
                                               
                                               any(vegs %in% current_veg1)
                                             })
                       data.frame(Site = current_row[["Site"]],
                                  SiteName = current_row[["SiteName"]],
                                  StateName = current_row[["StateName"]],
                                  OBJECTID1998 = vegmap$OBJECTID,
                                  matches2 = as.integer(veg_matches),
                                  stringsAsFactors = FALSE)
                     })


results2 <- do.call(rbind,
                    match_list2)
# Join results tables
SPmatch1998 <- results %>%
  dplyr::left_join(results2) 
# Count matches
SPmatch1998 <- dplyr::mutate(SPmatch1998, SPTally = matches + matches2)
SPmatch1998 <- dplyr::filter(SPmatch1998, SPTally > 0)
SPmatch1998 <- dplyr::distinct(SPmatch1998)
# Join matches based on species with FG assemblage match
SPmatch1998 <- dplyr::distinct(SPmatch1998)
# Reorder variables
fullresults1998 <- dplyr::select(SPmatch1998, OBJECTID1998, ecosite1, ecosite2,
                                 ecosite3, SP1998, FG1998,
                                 FG_GSNum,
                                 SPTally, GeneralizedStateNum,
                                 GeneralizedStateName, VegUnite, SiteName, Site)






names(SPmatch1998)

# Keep generalized state matches
fullresults1998 <- dplyr::filter(fullresults1998, FG_GSNum == GeneralizedStateNum)
# Keep site matches
fullresults1998_sitematch <- dplyr::filter(fullresults1998, ecosite1 == SiteName |
                                             ecosite2 == SiteName | ecosite3 == SiteName)


# How many polygons are present in 1998 attribute table?
unique(attributetab1998$OBJECTID) # 432
unique(fullresults1998_sitematch$OBJECTID1998)

# Subset to polygons not matched
unmatched1998 <- subset(attributetab1998, !(attributetab1998$OBJECTID %in% fullresults1998_sitematch$OBJECTID1998))

# write to csv
write.csv(fullresults1998_sitematch, "results1998_sitematched.csv", row.names = FALSE)
write.csv(unmatched1998, "unmatched1998.csv", row.names = FALSE)




