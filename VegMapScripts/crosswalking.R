# Crosswalking veg map groups across years by species and functional group

# Load require packages
library(dplyr)
library(stringr)
library(tidyr)


# Convert statecode to strings
attributetab2021$state_code <- as.character(attributetab2021$state_code)
# Split state codes
attributetab2021 <- attributetab2021 %>%
  tidyr::separate(state_code, into = c("State1", "State2"), sep = 1, remove = FALSE, convert = TRUE) %>%
  tidyr::separate(State2, into = c("State2", "State3"), sep = 1, remove = TRUE, convert = TRUE)
attributetab2021$state_code <- as.integer(attributetab2021$state_code)
# Add name
attributetab2021 <- attributetab2021 %>%
  dplyr::mutate(State1Char = ifelse(State1 == 1, "Grassland",
                                                ifelse(State1 == 2, "AlteredGrassland/Savanna",
                                                ifelse(State1 == 3, "ShrubGrassMix",
                                                ifelse(State1 == 4, "ShrubInvadedGrassland",
                                                ifelse(State1 == 5, "ShrubDominated",
                                                ifelse(State1 == 6, "Shrubland",
                                                ifelse(State1 == 7, "Bare/Annuals",
                                                ifelse(State1 == 9, "ExoticInvaded", NA))))))))) %>%
  dplyr::mutate(State2Char = ifelse(State2 == 1, "Grassland",
                                    ifelse(State2 == 2, "AlteredGrassland/Savanna",
                                           ifelse(State2 == 3, "ShrubGrassMix",
                                                  ifelse(State2 == 4, "ShrubInvadedGrassland",
                                                         ifelse(State2 == 5, "ShrubDominated",
                                                                ifelse(State2 == 6, "Shrubland",
                                                                       ifelse(State2 == 7, "Bare/Annuals",
                                                                              ifelse(State2 == 9, "ExoticInvaded", NA))))))))) %>%
  dplyr::mutate(State3Char = ifelse(State3 == 1, "Grassland",
                                    ifelse(State3 == 2, "AlteredGrassland/Savanna",
                                           ifelse(State3 == 3, "ShrubGrassMix",
                                                  ifelse(State3 == 4, "ShrubInvadedGrassland",
                                                         ifelse(State3 == 5, "ShrubDominated",
                                                                ifelse(State3 == 6, "Shrubland",
                                                                       ifelse(State3 == 7, "Bare/Annuals",
                                                                              ifelse(State3 == 9, "ExoticInvaded", NA)))))))))



# Create FG assemblages by state
# First, unite FGs
attributetab2021 <- tidyr::unite(attributetab2021, FGString, Dom1FG:Dom3FG, sep = ",", remove = FALSE, na.rm = FALSE)

# Check matches
x <- select(attributetab2021, state_code, State1Char, State2Char, State3Char, dom1, dom2, dom3, Dom1FG, Dom2FG, Dom3FG, FGString)

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
  dplyr::filter(FGString2021 != "NA,NA,NA" | FGString2021 != "NA,NA,NA,NA,NA")



str(results)

# Look at esite and site names...2021 site names differ from autocrosswalk
unique(FGresults$esite)
unique(attributetab2021$ecosite1)
unique(autocrosswalk$SiteName)
# Write to csv to compare
names(attributetab2021)
x <- attributetab2021 %>%
  select(ecosite1, ecosite2, ecosite3) %>%
  gather(SiteName, ecosite) %>%
  select(-SiteName) %>%
  distinct() %>%
  filter(!is.na(ecosite) & ecosite != "<Null>" & ecosite != "No ecosite" & ecosite != " " & ecosite != "")
y <- as.data.frame(attributetab2021$esite)
y <- select(y, ecosite = 1)
y <- distinct(y)
z <- autocrosswalk %>%
  select(ecosite = SiteName) %>%
  distinct() %>%
  full_join(y) %>%
  full_join(x)





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
                                 StateChar = vegmap$StateChar1,
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
                                  StateChar = vegmap$StateChar1,
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
                                  StateChar = vegmap$StateChar1,
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
fullresults <- dplyr::select(fullresults, Site, SiteName, StateName, StateVegUnite, DOMVEG2021, esite, StateChar, matches1, matches2, matches3)


# Count matches
fullresults <- dplyr::mutate(fullresults, Tally = matches1 + matches2 + matches3)
fullresults <- dplyr::distinct(fullresults)
fullresults <- dplyr::filter(fullresults, Tally > 0)
fullresults <- fullresults[, c(1, 2, 3, 4, 5, 6, 10)]


















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
