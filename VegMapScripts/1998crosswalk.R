# Crosswalking veg map groups across years by functional group

# Load require packages
library(dplyr)
library(stringr)
library(tidyr)
library(qdapTools)

# Read in clean datasheets
attributetab1998 <- read.csv("at1998_clean.csv")
spgroups_site <- read.csv("spgroups_site.csv")
sitelookup <- read.csv("sitetype_lookup.csv")

# For now, read in manual match and join
mm1998 <- read.csv("at1998_clean_manuallymatched.csv")

attributetab1998 <- left_join(attributetab1998, mm1998)
write.csv(attributetab1998, "at1998_manuallymatched_spatial.csv", row.names = FALSE)


spgroups_site$GeneralizedStateNumber <- as.integer(spgroups_site$GeneralizedStateNumber)
spgroups_site <- dplyr::filter(spgroups_site, GeneralizedStateNumber > 0)

# Could do something where the potential ecological sites are assigned to either
# type 1 (grassland at potential) or type 2 (woody at potential), in cases
# where map units are cohesively one or the other
attributetab1998$esite1type <- qdapTools::lookup(attributetab1998$ecosite1, sitelookup[, 1:2])
attributetab1998$esite2type <- qdapTools::lookup(attributetab1998$ecosite2, sitelookup[, 1:2])
attributetab1998$esite3type <- qdapTools::lookup(attributetab1998$ecosite3, sitelookup[, 1:2])



# Full species match
fullmatch1998 <- attributetab1998 %>%
  dplyr::left_join(spgroups_site) %>%
  dplyr::select(FID, OBJECTID1998 = OBJECTID, VegUnite, 
                ecosite1, ecosite2, ecosite3, GeneralizedStateNumber, SiteName,
                esite1type, esite2type, esite3type) %>%
  dplyr::filter(!is.na(GeneralizedStateNumber) & GeneralizedStateNumber != 0)

# How many unique polygons?
unique(fullmatch1998$OBJECTID1998) # Only 56

# Should we subset to site matching as well?
# There are many that match (e.g.) to Clayey, which is not listed as a site for that
# polygon, but bottomland or salt flat is 
fullmatch1998_sitesubset <- dplyr::filter(fullmatch1998, SiteName == ecosite1 |
                                         SiteName == ecosite2 | SiteName == ecosite3)

# So now we have a dataframe where full species assemblage and sites match
# Needs to be edited for one row per polygon, when multiple sites were matched

# Also a dataframe where there are full species matches but sites don't match
# Needs to be edited so there's one row per polygon, assigned to a site (?)
names(fullmatch1998_sitesubset)
fullmatch1998_sitematch_collapsed <- fullmatch1998_sitesubset %>%
  dplyr::select(FID, OBJECTID1998, VegUnite, ecosite1, GeneralizedStateNumber) %>%
  distinct() %>%
  group_by(FID) %>%
  slice(which.min(GeneralizedStateNumber))



# Now, pull out polygons that didn't have a full species match
unmatched1998 <- subset(attributetab1998, !(attributetab1998$OBJECTID %in% 
                                              fullmatch1998_sitematch_collapsed$OBJECTID1998))


# Run full match on FGs? Or partial species match?


# Join on specific species
# First split species in master sp assemblage list
spgroups_site <- tidyr::separate(spgroups_site, VegUnite, c("Veg1", "Veg2", "Veg3"),
                                 sep = ",", remove = FALSE)

# Species join, dom1
match_list <- apply(X = spgroups_site,
                    vegmap = unmatched1998,
                    MARGIN = 1,
                    FUN = function(X, vegmap){
                      current_row <- X
                      current_veg1 <- current_row[["Veg1"]]
                      
                      veg_matches <- sapply(X = vegmap$DOM1,
                                            current_veg1 = current_veg1,
                                            
                                            FUN = function(X, current_veg1) {
                                              
                                              vegs <- X
                                              # vegs <- trimws(unlist(stringr::str_split(X, pattern = ",")))
                                              
                                              
                                              any(X %in% current_veg1)
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
                     vegmap = unmatched1998,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg1 <- current_row[["Veg2"]]
                       
                       veg_matches <- sapply(X = vegmap$DOM2,
                                             current_veg1 = current_veg1,
                                             
                                             FUN = function(X, current_veg1) {
                                               
                                               
                                               vegs <- X
                                               
                                               
                                               
                                               any(vegs %in% current_veg1)
                                             })
                       data.frame(SiteName = current_row[["SiteName"]],
                                  VegUnite = current_row[["VegUnite"]],
                                  GeneralizedStateNum = current_row[["GeneralizedStateNumber"]],
                                  SP1998 = vegmap$VegUnite,
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

# Reorder variables
names(SPmatch1998)
fullSPmatch1998 <- dplyr::select(SPmatch1998, OBJECTID1998, ecosite1, ecosite2,
                                 ecosite3, SP1998,
                                 SPTally, matches, GeneralizedStateNum,
                                VegUnite, SiteName)






names(fullSPmatch1998)
str(fullSPmatch1998)
fullSPmatch1998$GeneralizedStateNum <- as.numeric(fullSPmatch1998$GeneralizedStateNum)
# Keep site matches
fullSPmatch1998_sitematch <- dplyr::filter(fullSPmatch1998, ecosite1 == SiteName & GeneralizedStateNum > 0
                                           & matches > 0)



fullSPmatch1998_sitematch_collapsed <- fullSPmatch1998_sitematch %>%
  dplyr::select(OBJECTID1998, VegUnite, ecosite1, GeneralizedStateNum) %>%
  distinct() %>%
  group_by(OBJECTID1998) %>%
  slice(which.min(GeneralizedStateNum)) %>%
  ungroup()


# Bind matched tables and see if there's anything left
names(fullSPmatch1998_sitematch_collapsed)
names(fullmatch1998_sitematch_collapsed)
fullSPmatch1998_sitematch_collapsed <- dplyr::select(fullSPmatch1998_sitematch_collapsed,
                                                     OBJECTID1998, VegUnite, ecosite1, GeneralizedStateNumber = GeneralizedStateNum)



fullmatch1998_sitematch_collapsed <- dplyr::ungroup(fullmatch1998_sitematch_collapsed)
                                                     
fullmatch1998_sitematch_collapsed <- dplyr::select(fullmatch1998_sitematch_collapsed,
                                                     OBJECTID1998, VegUnite, ecosite1, GeneralizedStateNumber)

all1998 <- rbind(fullmatch1998_sitematch_collapsed, fullSPmatch1998_sitematch_collapsed)

unmatched <- subset(attributetab1998, !(attributetab1998$OBJECTID %in% all1998$OBJECTID1998))

unmatched <- dplyr::rename(unmatched, Veg1 = DOM1)

unmatchedjoin <- dplyr::left_join(unmatched, spgroups_site, by = "Veg1")

names(unmatchedjoin)
unmatchedjoin <- dplyr::select(unmatchedjoin, FID, OBJECTID, ecosite1, ecosite2, ecosite3, 
                               Veg1, DOM2, DOM3, VegUnite = VegUnite.x, SiteName, GeneralizedStateNumber, VegUniteAC = VegUnite.y)

unmatchedjoin$esite1type <- qdapTools::lookup(unmatchedjoin$ecosite1, sitelookup[, 1:2])
unmatchedjoin$esite2type <- qdapTools::lookup(unmatchedjoin$ecosite2, sitelookup[, 1:2])
unmatchedjoin$esite3type <- qdapTools::lookup(unmatchedjoin$ecosite3, sitelookup[, 1:2])
unmatchedjoin$SiteNameType <- qdapTools::lookup(unmatchedjoin$SiteName, sitelookup[, 1:2])

sitetypematch <- unmatchedjoin %>%
  dplyr::filter(esite1type == SiteNameType | esite2type == SiteNameType | esite3type == SiteNameType)

sitetypematch <- sitetypematch %>%
dplyr::select(OBJECTID1998 = OBJECTID, VegUnite, ecosite1, GeneralizedStateNumber) %>%
  distinct() %>%
  group_by(OBJECTID1998) %>%
  slice(which.min(GeneralizedStateNumber)) %>%
  ungroup()

all1998 <- rbind(all1998, sitetypematch)


unmatched <- subset(attributetab1998, !(attributetab1998$OBJECTID %in% all1998$OBJECTID1998))
names(unmatched)
unmatched <- dplyr::select(unmatched, FID, OBJECTID, ecosite1, ecosite2, ecosite3,
                           VegUnite, esite1type, esite2type, esite3type)





write.csv(unmatched, "unmatched1998.csv")

ahedits_1998 <- read.csv("unmatched1998_ahedits.csv")

ahedits_1998 <- dplyr::select(ahedits_1998, OBJECTID1998 = OBJECTID, VegUnite, ecosite1, GeneralizedStateNumber)

all1998 <- rbind(all1998, ahedits_1998)


all1998 <- rename(all1998, OBJECTID = OBJECTID1998)

write.csv(all1998, "all1998.csv", row.names = FALSE)

all1998 <- dplyr::select(all1998, OBJECTID, GeneralizedStateNumber)

attributetab1998_matched <- dplyr::left_join(attributetab1998, all1998)
names(attributetab1998_matched)
attributetab1998_matched <- dplyr::select(attributetab1998_matched, -VegUnite, -FGUnite,
                                          -Dom1FG, -Dom2FG, -Dom3FG, -esite1type, -esite2type, -esite3type)

write.csv(attributetab1998_matched, "attributetab1998_matched.csv", row.names = FALSE)
