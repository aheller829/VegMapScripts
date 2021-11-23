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
  dplyr::mutate_all(funs(stringr::str_replace(., "HOFFspp", "HOFF spp."))) %>%
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
  dplyr::mutate_all(funs(stringr::str_replace(., "PAIN", "PAIN2"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "MIBI", "MIACB"))) %>%
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
  dplyr::mutate_all(funs(stringr::str_replace(., "LACO", "LACO13"))) %>%
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












# QC: what is left unclassified?
# x <- dplyr::filter(attributetab1915, is.na(Dom1FG))
# x <- dplyr::filter(attributetab1928, is.na(Dom1FG))
# x <- dplyr::filter(attributetab1998, is.na(Dom1FG))
# x <- dplyr::filter(attributetab2021, is.na(Dom1FG))
# unique(x$dom1)

# Save cleaned csvs
write.csv(autocrosswalk, "autocrosswalk_clean.csv", row.names = FALSE)
write.csv(attributetab1915, "at1915_clean.csv", row.names = FALSE)
write.csv(attributetab1928, "at1928_clean.csv", row.names = FALSE)
write.csv(attributetab1998, "at1998_clean.csv", row.names = FALSE)
write.csv(attributetab2021, "at2021_clean.csv", row.names = FALSE)
