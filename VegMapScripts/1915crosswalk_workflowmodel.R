# Crosswalking veg map groups across years by functional group

# Load require packages
library(dplyr)
library(stringr)
library(tidyr)
library(qdapTools)

# Read in clean datasheets
attributetab1915 <- read.csv("at1915_clean.csv")
spgroups_site <- read.csv("spgroups_site_edited.csv")
sitelookup <- read.csv("sitetype_lookup.csv")

spgroups_site$GeneralizedStateNumber <- as.integer(spgroups_site$GeneralizedStateNumber)
spgroups_site <- dplyr::filter(spgroups_site, GeneralizedStateNumber > 0)

# How many per site?
table(spgroups_site$SiteName)

# Could do something where the potential ecological sites are assigned to either
# type 1 (grassland at potential) or type 2 (woody at potential), in cases
# where map units are cohesively one or the other
attributetab1915$esite1type <- qdapTools::lookup(attributetab1915$ecosite1, sitelookup[, 1:2])
attributetab1915$esite2type <- qdapTools::lookup(attributetab1915$ecosite2, sitelookup[, 1:2])
attributetab1915$esite3type <- qdapTools::lookup(attributetab1915$ecosite3, sitelookup[, 1:2])

write.csv(attributetab1915, "at1915_sitetype.csv")

# Recreate visual workflow
# Separate site 1 type
names(attributetab1915)
sitetype1 <- attributetab1915 %>%
  dplyr::filter(esite1type == 1) %>%
  mutate(GSC = ifelse(Dom1FG == PG & Dom2FG == PG & Dom3FG == PG, 1,))



# Match manually matched sheet with spatial data
at1915mm <- read.csv("at1915_sitetype_manuallymatched.csv")

attributetab1915 <- dplyr::left_join(attributetab1915, at1915mm)

attributetab1915 <- dplyr::rename(attributetab1915, GeneralizedState = GSC)

write.csv(attributetab1915, "at1915_spatial.csv", row.names = FALSE)





# Full species match
fullmatch1915 <- attributetab1915 %>%
  dplyr::left_join(spgroups_site) %>%
  dplyr::select(FID, OBJECTID, VegUnite, 
                ecosite1, ecosite2, ecosite3, GeneralizedStateNumber, SiteName,
                esite1type, esite2type, esite3type) %>%
  dplyr::filter(!is.na(GeneralizedStateNumber) & GeneralizedStateNumber != 0)

# How many unique polygons?
unique(fullmatch1915$OBJECTID) # Only 29

# Should we subset to site matching as well?
# There are many that match (e.g.) to Clayey, which is not listed as a site for that
# polygon, but bottomland or salt flat is 
fullmatch1915_sitesubset <- dplyr::filter(fullmatch1915, SiteName == ecosite1 |
                                            SiteName == ecosite2 | SiteName == ecosite3)

# So now we have a dataframe where full species assemblage and sites match
# Needs to be edited for one row per polygon, when multiple sites were matched

# Also a dataframe where there are full species matches but sites don't match
# Needs to be edited so there's one row per polygon, assigned to a site (?)
names(fullmatch1915_sitesubset)
fullmatch1915_sitematch_collapsed <- fullmatch1915_sitesubset %>%
  dplyr::select(FID, OBJECTID, VegUnite, ecosite1, ecosite2, ecosite3, GeneralizedStateNumber) %>%
  distinct() %>%
  group_by(FID) %>%
  slice(which.min(GeneralizedStateNumber))



# Now, pull out polygons that didn't have a full species match
unmatched1915 <- subset(attributetab1915, !(attributetab1915$OBJECTID %in% 
                                              fullmatch1915_sitematch_collapsed$OBJECTID))


# Run full match on FGs? Or partial species match?


# Join on specific species
# First split species in master sp assemblage list
spgroups_site <- tidyr::separate(spgroups_site, VegUnite, c("Veg1", "Veg2", "Veg3"),
                                 sep = ",", remove = FALSE)

# Species join, dom1
match_list <- apply(X = spgroups_site,
                    vegmap = unmatched1915,
                    MARGIN = 1,
                    FUN = function(X, vegmap){
                      current_row <- X
                      current_veg1 <- current_row[["Veg1"]]
                      
                      veg_matches <- sapply(X = vegmap$ZST_DOM,
                                            current_veg1 = current_veg1,
                                            
                                            FUN = function(X, current_veg1) {
                                              
                                              vegs <- X
                                              # vegs <- trimws(unlist(stringr::str_split(X, pattern = ",")))
                                              
                                              
                                              any(X %in% current_veg1)
                                            })
                      data.frame(SiteName = current_row[["SiteName"]],
                                 ACVegUnite = current_row[["VegUnite"]],
                                 GeneralizedStateNum = current_row[["GeneralizedStateNumber"]],
                                 VegUnite = vegmap$VegUnite,
                                 ecosite1 = vegmap$ecosite1,
                                 ecosite2 = vegmap$ecosite2,
                                 ecosite3 = vegmap$ecosite3,
                                 OBJECTID = vegmap$OBJECTID,
                                 matches = as.integer(veg_matches),
                                 stringsAsFactors = FALSE)
                    })


results <- do.call(rbind,
                   match_list)


# Species join, dom2
match_list2 <- apply(X = spgroups_site,
                     vegmap = unmatched1915,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg1 <- current_row[["Veg2"]]
                       
                       veg_matches <- sapply(X = vegmap$ZND_DOM,
                                             current_veg1 = current_veg1,
                                             
                                             FUN = function(X, current_veg1) {
                                               
                                               
                                               vegs <- X
                                               
                                               
                                               
                                               any(vegs %in% current_veg1)
                                             })
                       data.frame(SiteName = current_row[["SiteName"]],
                                  ACVegUnite = current_row[["VegUnite"]],
                                  GeneralizedStateNum = current_row[["GeneralizedStateNumber"]],
                                  VegUnite = vegmap$VegUnite,
                                  OBJECTID = vegmap$OBJECTID,
                                  matches2 = as.integer(veg_matches),
                                  stringsAsFactors = FALSE)
                     })


results2 <- do.call(rbind,
                    match_list2)
# Join results tables
SPmatch1915 <- results %>%
  dplyr::left_join(results2) 
# Count matches
SPmatch1915 <- dplyr::mutate(SPmatch1915, SPTally = matches + matches2)
SPmatch1915 <- dplyr::filter(SPmatch1915, SPTally > 0)
SPmatch1915 <- dplyr::distinct(SPmatch1915)

# Reorder variables
names(SPmatch1915)
fullSPmatch1915 <- dplyr::select(SPmatch1915, OBJECTID, ecosite1, ecosite2,
                                 ecosite3,
                                 SPTally, matches, GeneralizedStateNum,
                                 VegUnite, SiteName, ACVegUnite)






names(fullSPmatch1915)
str(fullSPmatch1915)
fullSPmatch1915$GeneralizedStateNum <- as.numeric(fullSPmatch1915$GeneralizedStateNum)
# Keep site matches
fullSPmatch1915_sitematch <- dplyr::filter(fullSPmatch1915, ecosite1 == SiteName
                                           & matches > 0 | ecosite2 == SiteName & matches > 0 |
                                             ecosite3 ==SiteName & matches > 0)



fullSPmatch1915_sitematch_collapsed <- fullSPmatch1915_sitematch %>%
  dplyr::select(OBJECTID, VegUnite, ecosite1, ecosite2, ecosite3, GeneralizedStateNum) %>%
  distinct() %>%
  group_by(OBJECTID) %>%
  slice(which.min(GeneralizedStateNum)) %>%
  ungroup()


# Bind matched tables and see if there's anything left
names(fullSPmatch1915_sitematch_collapsed)
names(fullmatch1915_sitematch_collapsed)
fullSPmatch1915_sitematch_collapsed <- dplyr::select(fullSPmatch1915_sitematch_collapsed,
                                                     OBJECTID, VegUnite, ecosite1, ecosite2, ecosite3, GeneralizedStateNumber = GeneralizedStateNum)



fullmatch1915_sitematch_collapsed <- dplyr::ungroup(fullmatch1915_sitematch_collapsed)


all1915 <- rbind(fullmatch1915_sitematch_collapsed, fullSPmatch1915_sitematch_collapsed)

# Bring in Jeremy's matches
unmatchedJS <- read.csv("unmatched1915_JS.csv")
# Get rid of any automatched that Jeremy matched
x <- subset(all1915, all1915$OBJECTID %in% unmatchedJS$OBJECTID)
# All have the same assigned states
all1915_x <- subset(all1915, !(all1915$OBJECTID %in% unmatchedJS$OBJECTID))
names(all1915)
names(unmatchedJS)
unmatchedJS <- select(unmatchedJS, OBJECTID, VegUnite, ecosite1, ecosite2, ecosite3, GeneralizedStateNumber)
all1915 <- rbind(all1915, unmatchedJS)




# Has been matched by hand
unmatched <- read.csv("unmatched1915_22822.csv")

unmatched <- dplyr::select(unmatched, OBJECTID, VegUnite, ecosite1, ecosite2, ecosite3,
                           GeneralizedStateNumber)
fullymatched1915 <- rbind(all1915, unmatched)


fullymatched1915 <- fullymatched1915 %>%
  dplyr::select(OBJECTID, VegUnite, GeneralizedStateNumber, ecosite1, ecosite2, ecosite3) %>%
  distinct() %>%
  group_by(OBJECTID) %>%
  slice(which.min(GeneralizedStateNumber)) %>%
  ungroup()


identifiers <- dplyr::select(attributetab1915, 1:4, 9, 10, 12:14)
names(attributetab1915)

fullymatched1915 <- left_join(fullymatched1915, identifiers, by = "OBJECTID")

write.csv(fullymatched1915, "fullymatched1915.csv", row.names = FALSE)



x <- filter(spgroups_site, SiteName == "Sandy" & Veg1 == "ARFI2")

