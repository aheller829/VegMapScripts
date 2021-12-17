# Crosswalking veg map groups across years by functional group

# Load require packages
library(dplyr)
library(stringr)
library(tidyr)

# Read in clean datasheets
autocrosswalk <- read.csv("autocrosswalk_clean.csv")
attributetab1915 <- read.csv("at1915_clean.csv")



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
match_list <- apply(X = autocrosswalk,
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
                                  matches1 = as.integer(veg_matches),
                                  stringsAsFactors = FALSE)
                     })


results <- do.call(rbind,
                    match_list)


# Species join, dom2
match_list2 <- apply(X = autocrosswalk,
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
                                  matches2 = as.integer(veg_matches),
                                  stringsAsFactors = FALSE)
                     })


results2 <- do.call(rbind,
                    match_list2)


# Species join, dom3
match_list3 <- apply(X = autocrosswalk,
                     vegmap = results1915,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg1 <- current_row[["DomSp3"]]
                       
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
                                  matches3 = as.integer(veg_matches),
                                  stringsAsFactors = FALSE)
                     })


results3 <- do.call(rbind,
                    match_list3)



# Join results tables
SPmatch1915 <- results %>%
  dplyr::left_join(results2) %>%
  dplyr::left_join(results3)
# Count matches
SPmatch1915 <- dplyr::mutate(SPmatch1915, SPTally = matches1 + matches2 + matches3)
SPmatch1915 <- dplyr::filter(SPmatch1915, SPTally > 1)
SPmatch1915 <- dplyr::distinct(SPmatch1915)
# Join matches based on species with FG assemblage match
SPmatch1915 <- dplyr::distinct(SPmatch1915)
# Reorder variables
fullresults1915 <- dplyr::select(SPmatch1915, OBJECTID1915, esite, SP1915, FG1915,
                                 FG_GSName, FG_GSNum,
                                 SPTally, GeneralizedStateNum,
                             GeneralizedStateName, VegUnite, SiteName, Site)


# Keep generalized state matches
# fullresults1915 <- dplyr::filter(fullresults1915, FG_GSNum == GeneralizedStateNum)
# Keep site matches
# fullresults1915_sitematch <- dplyr::filter(fullresults1915, esite == SiteName)
# Keep two species matchse
# fullresults1915_sitematch <- dplyr::distinct(fullresults1915_sitematch)


# How many polygons are present in 1915 attribute table?
unique(attributetab1915$OBJECTID) # 207
unique(fullresults1915$OBJECTID1915)
# Subset to polygons not matched
unmatched1915 <- subset(attributetab1915, !(attributetab1915$OBJECTID %in% fullresults1915$OBJECTID1915))


# Summary by objectid
x <- fullresults1915 %>%
  dplyr::group_by(OBJECTID1915, esite, FG_GSName) %>%
  dplyr::summarise(Count = n())


