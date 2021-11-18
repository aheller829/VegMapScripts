# Auto-crosswalking the ecological site/state dominant species table with historic veg map attribute tables

# Load require packages
library(dplyr)
library(stringr)
library(tidyr)

# Read in autocrosswalk table...sites and states with dominant species listed, taken from Jeremy's state keys and the STMs
autocrosswalk <- read.csv("autocrosswalk.csv")

# Add rownumber
autocrosswalk <- dplyr::mutate(autocrosswalk, ID = row_number())

# Read in plant functional group table
# This table has all species from ESDs and STMs, as well as species from historic data tables, resolved to functional group
fglist <- read.csv("speciesjoin_fg.csv")
fglist <- dplyr::distinct(fglist)

# Add functional group columns to crosswalk table
Dom1_fg <- dplyr::select(fglist, DomSp1 = Code, Dom1FG = FG)
autocrosswalk <- dplyr::left_join(autocrosswalk, Dom1_fg, by = "DomSp1")
Dom2_fg <- dplyr::select(fglist, DomSp2 = Code, Dom2FG = FG)
autocrosswalk <- dplyr::left_join(autocrosswalk, Dom2_fg, by = "DomSp2")
Dom3_fg <- dplyr::select(fglist, DomSp3 = Code, Dom3FG = FG)
autocrosswalk <- dplyr::left_join(autocrosswalk, Dom3_fg, by = "DomSp3")
# Remove intermediary dfs
rm(Dom1_fg)
rm(Dom2_fg)
rm(Dom3_fg)




# Clean 1915 data
# Read in 1915 table
attributetab1915 <- read.csv("attributetab1915.csv")

# Will need to transform species codes to match USDA PLANTS codes used in autocrosswalk

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
  dplyr::mutate_all(funs(stringr::str_replace(., "PRGL2.", "PRGL2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "PSSC", "PSSC6"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "SCBR", "SCBR2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "SPORssp", "SPORO"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "MUARE", "MUAR"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "DAPU", "DAPU7"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "ATCA", "ATCA2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "NO GRASS", "Bare"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "MUPO", "MUPO2")))

# Add functional group columns to 1915 data
Dom1_fg <- dplyr::select(fglist, ZST_DOM = Code, Dom1FG = FG)
attributetab1915 <- dplyr::left_join(attributetab1915, Dom1_fg, by = "ZST_DOM")
Dom2_fg <- dplyr::select(fglist, ZND_DOM = Code, Dom2FG = FG)
attributetab1915 <- dplyr::left_join(attributetab1915, Dom2_fg, by = "ZND_DOM")
Dom3_fg <- dplyr::select(fglist, ZRD_DOM = Code, Dom3FG = FG)
attributetab1915 <- dplyr::left_join(attributetab1915, Dom3_fg, by = "ZRD_DOM")
attributetab1915 <- dplyr::distinct(attributetab1915)
# Remove intermediary dfs
rm(Dom1_fg)
rm(Dom2_fg)
rm(Dom3_fg)




# Clean 1928 data
attributetab1928 <- read.csv("attributetab1928.csv")
# Codes from 1928
# names(attributetab1928)
# unique(attributetab1928$Name)
# Codes from site/state table
# names(autocrosswalk)
# unique(autocrosswalk$DomSp1)
# unique(autocrosswalk$DomSp2)
# unique(autocrosswalk$DomSp3)
# unique(autocrosswalk$Subdom1)
# unique(autocrosswalk$Subdom2)
attributetab1928 <- attributetab1928 %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Aristida spp.", "ARIST"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Black grama", "BOER4"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Broom snakeweed", "GUSA2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Burrograss", "SCBR2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Creosotebush", "LATR2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Mesquite", "PRGL2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Sporobolus spp.", "SPORO"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Tarbush", "FLCE"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Tobosa", "PLMU3")))

# Add functional group columns to 1928 data
Dom1_fg <- dplyr::select(fglist, Name = Code, Dom1FG = FG)
attributetab1928 <- dplyr::left_join(attributetab1928, Dom1_fg, by = "Name")
attributetab1928 <- dplyr::distinct(attributetab1928)
# Remove intermediary dfs
rm(Dom1_fg)



# Clean 1998 data
attributetab1998 <- read.csv("attributetab1998.csv")
# names(attributetab1998)
# unique(attributetab1998$DOM1)
# unique(attributetab1998$DOM2)
# unique(attributetab1998$DOM3)
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
  dplyr::mutate_all(funs(stringr::str_replace(., "QUTU", "QUERCUS"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "EPHEDRAD", "EPHEDRA"))) 

attributetab1998 <- dplyr::mutate_all(attributetab1998, funs(stringr::str_replace(., "PRGL2D", "PRGL2")))
attributetab1998 <- dplyr::mutate_all(attributetab1998, funs(stringr::str_replace(., "PRGL2SH", "PRGL2"))) # Don't know why this is 
# necessary, but PRGLSH and PRGLD aren't transforming properly with code above

# Add functional group columns to 1998 data
Dom1_fg <- dplyr::select(fglist, DOM1 = Code, Dom1FG = FG)
attributetab1998 <- dplyr::left_join(attributetab1998 , Dom1_fg, by = "DOM1")
Dom2_fg <- dplyr::select(fglist, DOM2 = Code, Dom2FG = FG)
attributetab1998 <- dplyr::left_join(attributetab1998, Dom2_fg, by = "DOM2")
Dom3_fg <- dplyr::select(fglist, DOM3 = Code, Dom3FG = FG)
attributetab1998 <- dplyr::left_join(attributetab1998, Dom3_fg, by = "DOM3")
attributetab1998 <- dplyr::distinct(attributetab1998)
# Remove intermediary dfs
rm(Dom1_fg)
rm(Dom2_fg)
rm(Dom3_fg)



# Clean 2021 data
attributetab2021 <- read.csv("attributetab2020.csv")
# Add functional group columns to crosswalk table
# First, convert species codes to standards
# Codes from 2020
# names(attributetab2021)
# unique(attributetab2021$dom1)
# unique(attributetab2021$dom2)
# unique(attributetab2021$dom3)
# unique(attributetab2021$dom4)

attributetab2021 <- attributetab2021 %>%
  dplyr::mutate(dom1 = sub("\\ -.*", "", dom1)) %>%
  dplyr::mutate(dom2 = sub("\\ -.*", "", dom2)) %>%
  dplyr::mutate(dom3 = sub("\\ -.*", "", dom3)) %>%
  dplyr::mutate(dom4 = sub("\\ -.*", "", dom4)) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "AMAC2", "annuals"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "EPTO", "EPHEDRA"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "OPENE", "OPUNT"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "EPTR", "EPHEDRA"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "EPHEDRA ", "EPHEDRA"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "ATCA4", "ATCA2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "Aristida", "ARIST")))


# Join FG codes
Dom1_fg <- dplyr::select(fglist, dom1 = Code, Dom1FG = FG)
attributetab2021 <- dplyr::left_join(attributetab2021, Dom1_fg, by = "dom1")
Dom2_fg <- dplyr::select(fglist, dom2 = Code, Dom2FG = FG)
attributetab2021 <- dplyr::left_join(attributetab2021, Dom2_fg, by = "dom2")
Dom3_fg <- dplyr::select(fglist, dom3 = Code, Dom3FG = FG)
attributetab2021 <- dplyr::left_join(attributetab2021, Dom3_fg, by = "dom3")
attributetab2021 <- dplyr::distinct(attributetab2021)
# Remove intermediary dfs
rm(Dom1_fg)
rm(Dom2_fg)
rm(Dom3_fg)


# QC: what is left unclassified?
x <- dplyr::filter(attributetab1915, is.na(Dom1FG))
x <- dplyr::filter(attributetab1928, is.na(Dom1FG))
x <- dplyr::filter(attributetab1998, is.na(Dom1FG))
x <- dplyr::filter(attributetab2021, is.na(Dom1FG))
unique(x$dom1)








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
 mutate(dom1 = ifelse(dom1 == "<Null>" | dom1 == " ", NA, dom1))

attributetab2021 <- attributetab2021 %>%
  dplyr::mutate_at(vars(c("dom1", "dom2", "dom3", "dom4")), ~ifelse( . == "<Null>" | . == " ", NA, .)) %>%
  tidyr::unite(DomVeg, dom1:dom4, na.rm = FALSE, remove = FALSE, sep = ",")


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
  dplyr::filter(matches > 0) %>%
  dplyr::filter(FGString2021 != "NA,NA,NA")



str(results)






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
