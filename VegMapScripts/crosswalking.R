# Auto-crosswalking the ecological site/state dominant species table with historic veg map attribute tables


# Want to rewrite this....should be matching 1st dom and 1st dom probably
# Is there any point at pulling this crosswalk out to the functional group level, e.g. to connect to what Erica's doing?


library(dplyr)
library(stringr)
library(tidyr)

# Read in site/state table
autocrosswalk <- read.csv("autocrosswalk.csv")

# Read in plant functional group table
fglist <- read.csv("speciesjoin_fg.csv")

# Add functional group columns to crosswalk table
Dom1_fg <- dplyr::select(fglist, DomSp1 = Code, Dom1FG = FG)
autocrosswalk <- dplyr::left_join(autocrosswalk, Dom1_fg, by = "DomSp1")
Dom2_fg <- dplyr::select(fglist, DomSp2 = Code, Dom2FG = FG)
autocrosswalk <- dplyr::left_join(autocrosswalk, Dom2_fg, by = "DomSp2")
Dom3_fg <- dplyr::select(fglist, DomSp3 = Code, Dom3FG = FG)
autocrosswalk <- dplyr::left_join(autocrosswalk, Dom3_fg, by = "DomSp3")







# Bring in generalized states from 2021
attributetab2021 <- read.csv("attributetab2020.csv")
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

# Add functional group columns to crosswalk table
# First, convert species codes to standards
# Codes from 2020
names(attributetab2021)
unique(attributetab2021$dom1)
unique(attributetab2021$dom2)
unique(attributetab2021$dom3)
unique(attributetab2021$dom4)

attributetab2021 <- attributetab2021 %>%
  dplyr::mutate(dom1 = sub("\\ -.*", "", dom1)) %>%
  dplyr::mutate(dom2 = sub("\\ -.*", "", dom2)) %>%
  dplyr::mutate(dom3 = sub("\\ -.*", "", dom3)) %>%
  dplyr::mutate(dom4 = sub("\\ -.*", "", dom4)) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "AMAC2", "annuals"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "EPTO", "EPHEDRA"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "OPENE", "OPUNT"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "EPTR", "EPHEDRA"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "ATCA4", "ATCA2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Aristida", "ARIST")))

Dom1_fg <- dplyr::select(fglist, dom1 = Code, Dom1FG = FG)
attributetab2021 <- dplyr::left_join(attributetab2021, Dom1_fg, by = "dom1")
Dom2_fg <- dplyr::select(fglist, dom2 = Code, Dom2FG = FG)
attributetab2021 <- dplyr::left_join(attributetab2021, Dom2_fg, by = "dom2")
Dom3_fg <- dplyr::select(fglist, dom3 = Code, Dom3FG = FG)
attributetab2021 <- dplyr::left_join(attributetab2021, Dom3_fg, by = "dom3")


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







# 1915 crosswalking

# Read in historic veg map tables
attributetab1915 <- read.csv("attributetab1915.csv")

# Will need to transform species codes to match
# Codes from 1915 map
# names(attributetab1915)
# unique(attributetab1915$ZST_DOM)
# unique(attributetab1915$ZND_DOM)
# unique(attributetab1915$ZRD_DOM)

# Codes from site/state table
# names(autocrosswalk)
# unique(autocrosswalk$DomSp1)
# unique(autocrosswalk$DomSp2)
# unique(autocrosswalk$DomSp3)
# unique(autocrosswalk$Subdom1)
# unique(autocrosswalk$Subdom2)


attributetab1915 <- attributetab1915 %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "ARFI", "ARFI2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "ARISTspp", "ARIST"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "BOER", "BOER4"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "GUSA", "GUSA2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "LATR", "LATR2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "PLMU", "PLMU3"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "PRGL", "PRGL2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "PRGL.", "PRGL2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "PSSC", "PSSC6"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "SCBR", "SCBR2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "SPORssp", "SPORO"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "MUARE", "MUAR"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "DAPU", "DAPU7"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "ATCA", "ATCA2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "MUPO", "MUPO2")))


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












































attributetab1928 <- read.csv("attributetab1928.csv")
attributetab1998 <- read.csv("attributetab1998.csv")
attributetab2020 <- read.csv("attributetab2020.csv")
# Codes from 1928
names(attributetab1928)
unique(attributetab1928$Name)
# Codes from site/state table
names(autocrosswalk)
unique(autocrosswalk$DomSp1)
unique(autocrosswalk$DomSp2)
unique(autocrosswalk$DomSp3)
unique(autocrosswalk$Subdom1)
unique(autocrosswalk$Subdom2)
attributetab1928 <- attributetab1928 %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Aristida spp.", "ARIST"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Black grama", "BOER4"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Broom snakeweed", "GUSA2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Burrograss", "SCBR2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Creosotebush", "LATR2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Mesquite", "PRGL2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Sporobolus spp.", "SPORO"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "TARBUSH", "FLCE"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Tobosa", "PLMU3")))


# Codes from 1998
names(attributetab1998)
unique(attributetab1998$DOM1)
unique(attributetab1998$DOM2)
unique(attributetab1998$DOM3)
attributetab1998 <- attributetab1998 %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "ARFI", "ARFI2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "PRGL", "PRGL2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "ARPU", "ARPU9"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "ATCA", "ATCA2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "BARE", "Bare"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "BOER", "BOER4"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "EPTR", "EPHEDRA"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "EPTRD", "EPHEDRA"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "GUSA", "GUSA2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "LATR", "LATR2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "PLMU", "PLMU3"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "PRGL", "PRGL2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "PRGLD", "PRGL2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "PRGLSH", "PRGL2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "PSSC", "PSSC6"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "SCBR", "SCBR2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "SPFL", "SPFL2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "SPNE", "SPORO"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "MUPO", "MUPO2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "RHMI", "RHMI3"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "EPTO", "EPHEDRA"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "OPSP", "OPUNT"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "QUTU", "QUERCUS")))


























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
