# Crosswalking veg map groups across years by functional group

# Load require packages
library(dplyr)
library(stringr)
library(tidyr)

# Read in clean datasheets
attributetab1915 <- read.csv("at1915_clean.csv")
attributetab1928 <- read.csv("at1928_clean.csv")
attributetab1998 <- read.csv("at1998_clean.csv")
attributetab2021 <- read.csv("at2021_clean.csv")




# How many functional group equivalents are there?
functionalgrouptab <- autocrosswalk %>%
  dplyr::group_by(Site, SiteName, StateName, FGUnite) %>%
  dplyr::summarise(Count = n())



# Create FG assemblages by state
# First, unite FGs
# Unite species and functional groups to strings
attributetab2021 <- tidyr::unite(attributetab2021, FGString, Dom1FG:Dom3FG, sep = ",", remove = FALSE, na.rm = FALSE)

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

fgassemblages <- fgassemblages[!grepl("NA,NA", fgassemblages$FGString), ]

fgassemblages <- dplyr::distinct(fgassemblages)
# Remove intermediary dfs
rm(fgassemblages2)
rm(fgassemblages3)

# Create string vector of dominant FGs in crosswalk 
autocrosswalk <- tidyr::unite(autocrosswalk, FGUnite, Dom1FG:Dom3FG, na.rm = FALSE, remove = FALSE, sep = ",")
# Create string vector of dominant FGs in attributetable
attributetab2021 <- attributetab2021 %>%
  mutate(dom1 = ifelse(dom1 == "<Null>" | dom1 == " ", NA, dom1)) # Make all NA codes the same

attributetab2021 <- attributetab2021 %>%
  dplyr::mutate_at(vars(c("dom1", "dom2", "dom3", "dom4")), ~ifelse( . == "<Null>" | . == " ", NA, .)) %>%
  tidyr::unite(DomVeg, dom1:dom4, na.rm = FALSE, remove = FALSE, sep = ",") # Unite codes at species level

# Unite autocrosswalk codes at species level
autocrosswalk <- tidyr::unite(autocrosswalk, VegUnite, DomSp1:Subdom2, na.rm = TRUE, remove = FALSE, sep = ",")


# Match FGs between 2021 and autocrosswalk
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



str(results)






# A FG alternative...match group by group, as with species
# Probably just looking for first dominant (or across, but remove NA)
# Look for T/F matches
# Do 1915 for simplicity

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
                      data.frame(Species = current_row[["Dom1FG"]],
                                 StateName = current_row[["StateName"]],
                                 StateVegUnite = current_row[["FGUnite"]],
                                 DOMVEG1915 = vegmap$FGString,
                                 matches1 = as.integer(veg_matches),
                                 OBJECTID1915 = vegmap$OBJECTID,
                                 Site = current_row[["Site"]],
                                 SiteName = current_row[["SiteName"]],
                                 esite = vegmap$ecosite1,
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
                      data.frame(Species = current_row[["Dom2FG"]],
                                 StateName = current_row[["StateName"]],
                                 StateVegUnite = current_row[["FGUnite"]],
                                 DOMVEG1915 = vegmap$FGString,
                                 matches2 = as.integer(veg_matches),
                                 OBJECTID1915 = vegmap$OBJECTID,
                                 Site = current_row[["Site"]],
                                 SiteName = current_row[["SiteName"]],
                                 esite = vegmap$ecosite1,
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
                       current_veg1 <- current_row[["Dom3FG"]]
                       
                       veg_matches <- sapply(X = vegmap$Dom3FG,
                                             current_veg1 = current_veg1,
                                             
                                             FUN = function(X, current_veg1) {
                                               
                                               vegs <- X
                                               
                                               
                                               any(vegs %in% current_veg1)
                                             })
                       data.frame(Species = current_row[["Dom3FG"]],
                                  StateName = current_row[["StateName"]],
                                  StateVegUnite = current_row[["FGUnite"]],
                                  DOMVEG1915 = vegmap$FGString,
                                  matches3 = as.integer(veg_matches),
                                  OBJECTID1915 = vegmap$OBJECTID,
                                  Site = current_row[["Site"]],
                                  SiteName = current_row[["SiteName"]],
                                  esite = vegmap$ecosite1,
                                  stringsAsFactors = FALSE)
                     })


results3 <- do.call(rbind,
                    match_list3)









# Join results tables
results$ID <- 1:nrow(results)
results2$ID <- 1:nrow(results2)
results3$ID <- 1:nrow(results3)
FGresults1915 <- results %>%
  dplyr::left_join(results2) %>%
  dplyr::left_join(results3)
FGresults1915 <- dplyr::select(FGresults1915, Site, SiteName, StateName, StateVegUnite, OBJECTID1915, DOMVEG1915, esite, matches1, matches2, matches3)


# Count matches
FGresults1915 <- dplyr::mutate(FGresults1915, Tally = matches1 + matches2 + matches3)
FGresults1915 <- dplyr::distinct(FGresults1915)
FGresults1915 <- dplyr::filter(FGresults1915, Tally > 0)
FGresults1915 <- FGresults1915[, c(1, 2, 3, 4, 5, 6, 10)]


