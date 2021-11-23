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













# Match FGs between 1915 and autocrosswalk
# Create string vector of dominant species
attributetab1915 <- tidyr::unite(attributetab1915, DomVeg, ZST_DOM:ZRD_DOM, remove = FALSE, sep = ",")
attributetab1915 <- tidyr::unite(attributetab1915, FGString, Dom1FG:Dom3FG, na.rm = TRUE, remove = FALSE, sep = ",")

match_list <- apply(X = attributetab1915,
                    vegmap = autocrosswalk,
                    MARGIN = 1,
                    FUN = function(X, vegmap){
                      current_row <- X
                      current_veg1 <- current_row[["FGString"]]
                      
                      veg_matches <- sapply(X = vegmap$FGUnite,
                                            current_veg1 = current_veg1,
                                            
                                            FUN = function(X, current_veg1) {
                                              
                                              vegs <- X
                                              
                                              any(vegs %in% current_veg1)
                                            })
                      
                      data.frame(FID1915 = current_row[["FID"]],
                                 FGString1915 = current_row[["FGString"]],
                                 esite = current_row[["ecosite1"]],
                                 autocFGString = vegmap$FGUnite,
                                 Site = vegmap$Site,
                                 SiteName = vegmap$SiteName,
                                 StateName = vegmap$StateName,
                                 matches = as.integer(veg_matches),
                                 DomVeg_2021 = current_row[["DomVeg"]],
                                 VegUniteAC = vegmap$VegUnite,
                                 stringsAsFactors = FALSE)
                    })


results <- do.call(rbind,
                   match_list)







































# First, join at species level
# Create string vector of dominant species in attribute table
attributetab1915 <- tidyr::unite(attributetab1915, DomVeg, ZST_DOM:ZRD_DOM, remove = FALSE, sep = ",")

# Create string vector of dominant species in crosswalk
autocrosswalk <- tidyr::unite(autocrosswalk, VegUnite, DomSp1:Subdom2, na.rm = TRUE, remove = FALSE, sep = ",")

# Look for T/F matches
match_list <- apply(X = autocrosswalk,
                    vegmap = attributetab1915,
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
                                 DOMVEG1915 = vegmap$DomVeg,
                                 matches1 = as.integer(veg_matches),
                                 Site = current_row[["Site"]],
                                 SiteName = current_row[["SiteName"]],
                                 ecosite1 = vegmap$ecosite1,
                                 stringsAsFactors = FALSE)
                    })


results <- do.call(rbind,
                   match_list)

# 2nd Dom
match_list2 <- apply(X = autocrosswalk,
                     vegmap = attributetab1915,
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
                                  DOMVEG1915 = vegmap$DomVeg,
                                  matches2 = as.integer(veg_matches),
                                  Site = current_row[["Site"]],
                                  SiteName = current_row[["SiteName"]],
                                  ecosite1 = vegmap$ecosite1,
                                  stringsAsFactors = FALSE)
                     })


results2 <- do.call(rbind,
                    match_list2)


# 3rd Dom
match_list3 <- apply(X = autocrosswalk,
                     vegmap = attributetab1915,
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
                                  DOMVEG1915 = vegmap$DomVeg,
                                  matches3 = as.integer(veg_matches),
                                  Site = current_row[["Site"]],
                                  SiteName = current_row[["SiteName"]],
                                  ecosite1 = vegmap$ecosite1,
                                  stringsAsFactors = FALSE)
                     })


results3 <- do.call(rbind,
                    match_list3)



# Join results tables
results$ID <- 1:nrow(results)
results2$ID <- 1:nrow(results2)
results3$ID <- 1:nrow(results3)
fullresults <- results %>%
  dplyr::left_join(results2) %>%
  dplyr::left_join(results3)
fullresults <- dplyr::select(fullresults, Site, SiteName, StateName, StateVegUnite, DOMVEG1915, ecosite1, matches1, matches2, matches3)


# Count matches
fullresults <- dplyr::mutate(fullresults, Tally = matches1 + matches2 + matches3)
fullresults <- dplyr::distinct(fullresults)
fullresults <- dplyr::filter(fullresults, Tally > 0)
fullresults <- fullresults[, c(1, 2, 3, 4, 5, 6, 10)]








# Join at functional group level
# Will have to repeat for each dom sp
ZST_fg <- dplyr::select(fglist, ZST_DOM = Code, ZST_DOM_FG = FG)
attributetab1915 <- dplyr::left_join(attributetab1915, ZST_fg, by = "ZST_DOM")

ZND_fg <- dplyr::select(fglist, ZND_DOM = Code, ZND_DOM_FG = FG)
attributetab1915 <- dplyr::left_join(attributetab1915, ZND_fg, by = "ZND_DOM")

ZRD_fg <- dplyr::select(fglist, ZRD_DOM = Code, ZRD_DOM_FG = FG)
attributetab1915 <- dplyr::left_join(attributetab1915, ZRD_fg, by = "ZRD_DOM")


# First, join at species level
# Create string vector of dominant species in attribute table
attributetab1915 <- tidyr::unite(attributetab1915, DomVegFG, ZST_DOM_FG:ZRD_DOM_FG, remove = FALSE, sep = ",")

# Create string vector of dominant species in crosswalk
autocrosswalk <- tidyr::unite(autocrosswalk, VegUniteFG, Dom1FG:Subdom2, na.rm = TRUE, remove = FALSE, sep = ",")

# Look for T/F matches
match_list <- apply(X = autocrosswalk,
                    vegmap = attributetab1915,
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
                                 DOMVEG1915 = vegmap$DomVeg,
                                 matches1 = as.integer(veg_matches),
                                 Site = current_row[["Site"]],
                                 SiteName = current_row[["SiteName"]],
                                 ecosite1 = vegmap$ecosite1,
                                 stringsAsFactors = FALSE)
                    })


results <- do.call(rbind,
                   match_list)

# 2nd Dom
match_list2 <- apply(X = autocrosswalk,
                     vegmap = attributetab1915,
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
                                  DOMVEG1915 = vegmap$DomVeg,
                                  matches2 = as.integer(veg_matches),
                                  Site = current_row[["Site"]],
                                  SiteName = current_row[["SiteName"]],
                                  ecosite1 = vegmap$ecosite1,
                                  stringsAsFactors = FALSE)
                     })


results2 <- do.call(rbind,
                    match_list2)


# 3rd Dom
match_list3 <- apply(X = autocrosswalk,
                     vegmap = attributetab1915,
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
                                  DOMVEG1915 = vegmap$DomVeg,
                                  matches3 = as.integer(veg_matches),
                                  Site = current_row[["Site"]],
                                  SiteName = current_row[["SiteName"]],
                                  ecosite1 = vegmap$ecosite1,
                                  stringsAsFactors = FALSE)
                     })


results3 <- do.call(rbind,
                    match_list3)



# Join results tables
results$ID <- 1:nrow(results)
results2$ID <- 1:nrow(results2)
results3$ID <- 1:nrow(results3)
fullresults <- results %>%
  dplyr::left_join(results2) %>%
  dplyr::left_join(results3)
fullresults <- dplyr::select(fullresults, Site, SiteName, StateName, StateVegUnite, DOMVEG1915, ecosite1, matches1, matches2, matches3)


# Count matches
fullresults <- dplyr::mutate(fullresults, Tally = matches1 + matches2 + matches3)
fullresults <- dplyr::distinct(fullresults)
fullresults <- dplyr::filter(fullresults, Tally > 0)
fullresults <- fullresults[, c(1, 2, 3, 4, 5, 6, 10)]































































# Matching repeated for subdominants

# 1st Subdom
match_list4 <- apply(X = autocrosswalk,
                     vegmap = attributetab1915,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg4 <- current_row[["Subdom1"]]
                       
                       veg_matches <- sapply(X = vegmap$DomVeg,
                                             current_veg4 = current_veg4,
                                             
                                             FUN = function(X, current_veg4) {
                                               
                                               vegs <- trimws(unlist(stringr::str_split(X, pattern = ",")))
                                               
                                               any(vegs %in% current_veg4)
                                             })
                       data.frame(Species4 = current_row[["Subdom1"]],
                                  StateName = current_row[["StateName"]],
                                  StateVegUnite = current_row[["VegUnite"]],
                                  DOMVEG1915 = vegmap$DomVeg,
                                  matches4 = as.integer(veg_matches),
                                  Site = current_row[["Site"]],
                                  SiteName = current_row[["SiteName"]],
                                  ecosite1 = vegmap$ecosite1,
                                  stringsAsFactors = FALSE)
                     })


results4 <- do.call(rbind,
                    match_list4)



# 2nd Subdom
match_list5 <- apply(X = autocrosswalk,
                     vegmap = attributetab1915,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg5 <- current_row[["Subdom2"]]
                       
                       veg_matches <- sapply(X = vegmap$DomVeg,
                                             current_veg5 = current_veg5,
                                             
                                             FUN = function(X, current_veg5) {
                                               
                                               vegs <- trimws(unlist(stringr::str_split(X, pattern = ",")))
                                               
                                               any(vegs %in% current_veg5)
                                             })
                       data.frame(Species5 = current_row[["Subdom2"]],
                                  StateName = current_row[["StateName"]],
                                  StateVegUnite = current_row[["VegUnite"]],
                                  DOMVEG1915 = vegmap$DomVeg,
                                  matches5 = as.integer(veg_matches),
                                  Site = current_row[["Site"]],
                                  SiteName = current_row[["SiteName"]],
                                  ecosite1 = vegmap$ecosite1,
                                  stringsAsFactors = FALSE)
                     })


results5 <- do.call(rbind,
                    match_list5)



# Join results tables
results$ID <- 1:nrow(results)
results2$ID <- 1:nrow(results2)
results3$ID <- 1:nrow(results3)
results4$ID <- 1:nrow(results4)
results5$ID <- 1:nrow(results5)
fullresults <- results %>%
  dplyr::left_join(results2) %>%
  dplyr::left_join(results3) %>%
  dplyr::left_join(results4) %>%
  dplyr::left_join(results5)
fullresults <- dplyr::select(fullresults, Site, SiteName, StateName, StateVegUnite, DOMVEG1915, ecosite1, matches1, matches2, matches3, matches4, matches5)


# Count matches
fullresults <- dplyr::mutate(fullresults, Tally = matches1 + matches2 + matches3 + matches4 + matches5)
fullresults <- fullresults[, c(1, 2, 3, 9)]
fullresults <- dplyr::distinct(fullresults)

fullresults <- dplyr::filter(fullresults, Tally > 0)
