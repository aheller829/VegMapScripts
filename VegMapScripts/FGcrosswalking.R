# Crosswalking veg map groups across years by functional group

# Load require packages
library(dplyr)
library(stringr)
library(tidyr)

# Read in clean datasheets
autocrosswalk <- read.csv("autocrosswalk_clean.csv")
attributetab1915 <- read.csv("at1915_clean.csv")
attributetab1928 <- read.csv("at1928_clean.csv")
attributetab1998 <- read.csv("at1998_clean.csv")
attributetab2021 <- read.csv("at2021_clean.csv")







# How many functional group equivalents are there?
functionalgrouptab_site <- autocrosswalk %>%
  dplyr::group_by(Site, SiteName, StateName, FGUnite) %>%
  dplyr::summarise(Count = n())
# How many when grouping by generalized states?
functionalgrouptab_GS <- autocrosswalk %>%
  dplyr::group_by(GeneralizedStateNumber, GeneralizedStateName, FGUnite) %>%
  dplyr::summarise(Count = n())



# Create FG assemblages by state
# Repeat across states to capture all combinations
fgassemblages <- attributetab2021 %>%
  dplyr::group_by(State1, FGString) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::select(-Count) %>%
  dplyr::filter(FGString != "NA,NA,NA")

fgassemblages2 <- attributetab2021 %>%
  dplyr::group_by(State2, FGString) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::select(-Count) %>%
  dplyr::filter(FGString != "NA,NA,NA") %>%
  dplyr::rename(State1 = State2)

fgassemblages3 <- attributetab2021 %>%
  dplyr::group_by(State3, FGString) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::select(-Count) %>%
  dplyr::filter(FGString != "NA,NA,NA") %>%
  dplyr::rename(State1 = State3)

# Bind together, remove double NA, remove duplicates
fgassemblages <- rbind(fgassemblages, fgassemblages2)
fgassemblages <- rbind(fgassemblages, fgassemblages3)

# fgassemblages <- fgassemblages[!grepl("NA,NA", fgassemblages$FGString), ]

# Remove blanks
fgassemblages$FGString <- trimws(fgassemblages$FGString)
fgassemblages <- dplyr::filter(fgassemblages, FGString != "")

fgassemblages <- dplyr::distinct(fgassemblages)
# Remove intermediary dfs
rm(fgassemblages2)
rm(fgassemblages3)




# Try leftjoin between functionalgrouptab_GS and dataframe
results1915 <- attributetab1915 %>%
  dplyr::left_join(functionalgrouptab_GS) %>%
  dplyr::select(FID, OBJECTID1915 = OBJECTID, VegUnite1915 = VegUnite, 
                FGUnite1915 = FGUnite, ecosite1, FG_GSNum = GeneralizedStateNumber, 
                FG_GSName = GeneralizedStateName)
names(results1915)
# Second join on species? Or match on species?
# Species join, dom1
match_list4 <- apply(X = autocrosswalk,
                     vegmap = results1915,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg1 <- current_row[["DomSp1"]]
                       
                       veg_matches <- sapply(X = vegmap$VegUnite1915,
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
                                  FG1915 = vegmap$FGUnite1915,
                                  SP1915 = vegmap$VegUnite1915,
                                  esite = vegmap$ecosite1,
                                  FG_GSName = vegmap$FG_GSName,
                                  FG_GSNum = vegmap$FG_GSNum,
                                  OBJECTID1915 = vegmap$OBJECTID,
                                  matches4 = as.integer(veg_matches),
                                  stringsAsFactors = FALSE)
                     })


results4 <- do.call(rbind,
                    match_list4)


# Species join, dom2
match_list5 <- apply(X = autocrosswalk,
                     vegmap = results1915,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg1 <- current_row[["DomSp2"]]
                       
                       veg_matches <- sapply(X = vegmap$VegUnite1915,
                                             current_veg1 = current_veg1,
                                             
                                             FUN = function(X, current_veg1) {
                                               
                                  
                                               vegs <- trimws(unlist(stringr::str_split(X, pattern = ",")))
                                               
                                               
                                               
                                               any(vegs %in% current_veg1)
                                             })
                       data.frame(Site = current_row[["Site"]],
                                  SiteName = current_row[["SiteName"]],
                                  StateName = current_row[["StateName"]],
                                  OBJECTID1915 = vegmap$OBJECTID1915,
                                  FG_GSName = vegmap$FG_GSName,
                                  FG_GSNum = vegmap$FG_GSNum,
                                  matches5 = as.integer(veg_matches),
                                  stringsAsFactors = FALSE)
                     })


results5 <- do.call(rbind,
                    match_list5)
# Join results tables
SPmatch1915 <- results4 %>%
  dplyr::left_join(results5) 
# Count matches
SPmatch1915 <- dplyr::mutate(SPmatch1915, SPTally = matches4 + matches5)
SPmatch1915 <- dplyr::filter(SPmatch1915, SPTally > 0)
SPmatch1915 <- dplyr::distinct(SPmatch1915)
# Join matches based on species with FG assemblage match
SPmatch1915 <- dplyr::distinct(SPmatch1915)
# Reorder variables
fullresults1915 <- dplyr::select(SPmatch1915, OBJECTID1915, esite, SP1915, FG1915,
                                 FG_GSName, FG_GSNum,
                                 SPTally, GeneralizedStateNum,
                             GeneralizedStateName, VegUnite, SiteName, Site)

names(SPmatch1915)

# Keep generalized state matches
fullresults1915 <- dplyr::filter(fullresults1915, FG_GSNum == GeneralizedStateNum)
# Keep site matches
fullresults1915_sitematch <- dplyr::filter(fullresults1915, esite == SiteName)
# Keep two species matchse
fullresults1915_sitematch <- dplyr::filter(fullresults1915_sitematch, SPTally > 1)
fullresults1915_sitematch <- dplyr::distinct(fullresults1915_sitematch)


# How many polygons are present in 1915 attribute table?
unique(attributetab1915$OBJECTID) # 207

# Subset to polygons not matched
unmatched1915 <- subset(attributetab1915, !(attributetab1915$OBJECTID %in% FGresults1915$OBJECTID1915))
























# FG matches 1915
# Probably just looking for first dominant (or across, but remove NA)
# Look for T/F matches
match_list <- apply(X = autocrosswalk,
                    vegmap = attributetab1915,
                    MARGIN = 1,
                    FUN = function(X, vegmap){
                      current_row <- X
                      current_veg1 <- current_row[["Dom1FG"]]
                      
                      veg_matches <- sapply(X = vegmap$Dom1FG,
                                            current_veg1 = current_veg1,
                                            
                                            FUN = function(X, current_veg1) {
                                              
                                              vegs <- X
                                              
                                              
                                              any(vegs %in% current_veg1)
                                            })
                      data.frame(Site = current_row[["Site"]],
                                 SiteName = current_row[["SiteName"]],
                                 StateName = current_row[["StateName"]],
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






# 2nd Dom
match_list2 <- apply(X = autocrosswalk,
                     vegmap = attributetab1915,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg1 <- current_row[["Dom2FG"]]
                       
                       veg_matches <- sapply(X = vegmap$Dom2FG,
                                             current_veg1 = current_veg1,
                                             
                                             FUN = function(X, current_veg1) {
                                               
                                               vegs <- X
                                               
                                               
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













# Match FGs between 2021 and autocrosswalk (matches whole FG string)
match_list <- apply(X = attributetab2021,
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
                      
                      data.frame(OBJECTID2021 = current_row[["OBJECTID.."]],
                                 FGString2021 = current_row[["FGString"]],
                                 StateChar2021 = current_row[["State1Char"]],
                                 esite = current_row[["esite"]],
                                 ecosite1 = current_row[["ecosite1"]],
                                 OBJECTIDautoc = vegmap$ID,
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


# Filter results
FGresults <- results %>%
  dplyr::filter(matches == 1) %>%
  dplyr::filter(FGString2021 != "NA,NA,NA")
# Match sites
FGresults <- FGresults %>%
  dplyr::filter(SiteName == esite | SiteName == ecosite1)
unique(FGresults$OBJECTID2021)



# Match FGs between 2021 and autocrosswalk (match based on single FG)
match_list <- apply(X = attributetab2021,
                    vegmap = autocrosswalk,
                    MARGIN = 1,
                    FUN = function(X, vegmap){
                      current_row <- X
                      current_veg1 <- current_row[["Dom1FG"]]
                      
                      veg_matches <- sapply(X = vegmap$Dom1FG,
                                            current_veg1 = current_veg1,
                                            
                                            FUN = function(X, current_veg1) {
                                              
                                              vegs <- X
                                              
                                              any(vegs %in% current_veg1)
                                            })
                      
                      data.frame(OBJECTID2021 = current_row[["OBJECTID.."]],
                                 FGString2021 = current_row[["FGString"]],
                                 StateChar2021 = current_row[["State1Char"]],
                                 esite = current_row[["esite"]],
                                 ecosite1 = current_row[["ecosite1"]],
                                 OBJECTIDautoc = vegmap$ID,
                                 autocFGString = vegmap$FGUnite,
                                 Site = vegmap$Site,
                                 SiteName = vegmap$SiteName,
                                 StateName = vegmap$StateName,
                                 matchesFG1 = as.integer(veg_matches),
                                 DomVeg_2021 = current_row[["DomVeg"]],
                                 VegUniteAC = vegmap$VegUnite,
                                 stringsAsFactors = FALSE)
                    })


results <- do.call(rbind,
                   match_list)

# FG2
match_list2 <- apply(X = attributetab2021,
                    vegmap = autocrosswalk,
                    MARGIN = 1,
                    FUN = function(X, vegmap){
                      current_row <- X
                      current_veg1 <- current_row[["Dom2FG"]]
                      
                      veg_matches <- sapply(X = vegmap$Dom2FG,
                                            current_veg1 = current_veg1,
                                            
                                            FUN = function(X, current_veg1) {
                                              
                                              vegs <- X
                                              
                                              any(vegs %in% current_veg1)
                                            })
                      
                      data.frame(OBJECTID2021 = current_row[["OBJECTID.."]],
                                 OBJECTIDautoc = vegmap$ID,
                                 matchesFG2 = as.integer(veg_matches),
                                 stringsAsFactors = FALSE)
                    })


results2 <- do.call(rbind,
                   match_list2)

# FG3
match_list3 <- apply(X = attributetab2021,
                     vegmap = autocrosswalk,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg1 <- current_row[["Dom3FG"]]
                       
                       veg_matches <- sapply(X = vegmap$Dom3FG,
                                             current_veg1 = current_veg1,
                                             
                                             FUN = function(X, current_veg1) {
                                               
                                               vegs <- X
                                               
                                               any(vegs %in% current_veg1)
                                             })
                       
                       data.frame(OBJECTID2021 = current_row[["OBJECTID.."]],
                                  OBJECTIDautoc = vegmap$ID,
                                  matchesFG3 = as.integer(veg_matches),
                                  stringsAsFactors = FALSE)
                     })


results3 <- do.call(rbind,
                    match_list3)

# Join tables
resultsjoin2021 <- results %>%
  dplyr::left_join(results2) %>%
  dplyr::left_join(results3)
# Tally matches
resultsjoin2021 <- dplyr::mutate(resultsjoin2021, Tally = matchesFG1 + matchesFG2 + matchesFG3)
resultsjoin2021 <- dplyr::distinct(resultsjoin2021)
resultsjoin2021  <- dplyr::filter(resultsjoin2021 , Tally > 0)
# Filter for matching ecosites
resultsjoin2021 <- resultsjoin2021 %>%
  dplyr::filter(SiteName == esite | SiteName == ecosite1)



# Do another join on species?
match_list4 <- apply(X = attributetab2021,
                    vegmap = autocrosswalk,
                    MARGIN = 1,
                    FUN = function(X, vegmap){
                      current_row <- X
                      current_veg1 <- current_row[["dom1"]]
                      
                      veg_matches <- sapply(X = vegmap$DomSp1,
                                            current_veg1 = current_veg1,
                                            
                                            FUN = function(X, current_veg1) {
                                              
                                              vegs <- X
                                              
                                              any(vegs %in% current_veg1)
                                            })
                      
                      data.frame(OBJECTID2021 = current_row[["OBJECTID.."]],
                                 OBJECTIDautoc = vegmap$ID,
                                 matchesSp1 = as.integer(veg_matches),
                                 stringsAsFactors = FALSE)
                    })


results4 <- do.call(rbind,
                   match_list4)

# Sp 2
match_list5 <- apply(X = attributetab2021,
                     vegmap = autocrosswalk,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg1 <- current_row[["dom2"]]
                       
                       veg_matches <- sapply(X = vegmap$DomSp2,
                                             current_veg1 = current_veg1,
                                             
                                             FUN = function(X, current_veg1) {
                                               
                                               vegs <- X
                                               
                                               any(vegs %in% current_veg1)
                                             })
                       
                       data.frame(OBJECTID2021 = current_row[["OBJECTID.."]],
                                  OBJECTIDautoc = vegmap$ID,
                                  matchesSp2 = as.integer(veg_matches),
                                  stringsAsFactors = FALSE)
                     })


results5 <- do.call(rbind,
                    match_list5)

# Dom sp 3
match_list6 <- apply(X = attributetab2021,
                     vegmap = autocrosswalk,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg1 <- current_row[["dom3"]]
                       
                       veg_matches <- sapply(X = vegmap$DomSp3,
                                             current_veg1 = current_veg1,
                                             
                                             FUN = function(X, current_veg1) {
                                               
                                               vegs <- X
                                               
                                               any(vegs %in% current_veg1)
                                             })
                       
                       data.frame(OBJECTID2021 = current_row[["OBJECTID.."]],
                                  OBJECTIDautoc = vegmap$ID,
                                  matchesSp3 = as.integer(veg_matches),
                                  stringsAsFactors = FALSE)
                     })


results6 <- do.call(rbind,
                    match_list6)


# Join tables
resultsjoin2021 <- results %>%
  dplyr::left_join(results2) %>%
  dplyr::left_join(results3) %>%
  dplyr::left_join(results4) %>%
  dplyr::left_join(results5) %>%
  dplyr::left_join(results6)

# Tally matches
resultsjoin2021 <- dplyr::mutate(resultsjoin2021, Tally = matchesFG1 + matchesFG2 + matchesFG3 +
                                   matchesSp1 + matchesSp2 + matchesSp3)
resultsjoin2021 <- dplyr::distinct(resultsjoin2021)
resultsjoin2021  <- dplyr::filter(resultsjoin2021 , Tally > 0)
# Filter for matching ecosites
resultsjoin2021 <- resultsjoin2021 %>%
  dplyr::filter(SiteName == esite | SiteName == ecosite1) %>%
  dplyr::filter(FGString2021 != "NA,NA,NA")





