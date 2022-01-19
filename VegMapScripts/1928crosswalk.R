# Crosswalking veg map groups across years by functional group

# Load require packages
library(dplyr)
library(stringr)
library(tidyr)

# Read in clean datasheets
autocrosswalk <- read.csv("autocrosswalk_clean.csv")

attributetab1928 <- read.csv("at1928_clean.csv")

spgroups_site <- read.csv("spgroups_site.csv")


# Split spgroups into single species
spgroups_site <- tidyr::separate(spgroups_site, VegUnite, into = c("Sp1", "Sp2", 
                                                                   "Sp3"), sep = ",",
                                 remove = FALSE)

# Species join, dom1
match_list <- apply(X = spgroups_site,
                     vegmap = attributetab1928,
                     MARGIN = 1,
                     FUN = function(X, vegmap){
                       current_row <- X
                       current_veg1 <- current_row[["Sp1"]]
                       
                       veg_matches <- sapply(X = vegmap$Name,
                                             current_veg1 = current_veg1,
                                             
                                             FUN = function(X, current_veg1) {
                                               
                                               vegs <- X

                                               any(vegs %in% current_veg1)
                                             })
                       data.frame(OBJECTID1928 = vegmap$OBJECTID,
                                  ecosite1 = vegmap$ecosite1,
                                  ecosite2 = vegmap$ecosite2,
                                  ecosite3 = vegmap$ecosite3,
                                  VegUnite = current_row[["VegUnite"]],
                                  GeneralizedStateNum = current_row[["GeneralizedStateNumber"]],
                                  SiteName = current_row[["SiteName"]],
                                  FG1928 = vegmap$Dom1FG,
                                  SP1915 = vegmap$Name,
                                  matches = as.integer(veg_matches),
                                  stringsAsFactors = FALSE)
                     })


results1928 <- do.call(rbind,
                    match_list)

# Site match
results1928_sitematch <- dplyr::filter(results1928, matches > 0 & ecosite1 == SiteName |
                                         matches > 0 & ecosite2 == SiteName |
                                         matches > 0 & ecosite3 == SiteName)

# How many matched?
unique(results1928_sitematch$OBJECTID1928)

# Write to csv
write.csv(results1928_sitematch, "results1928_sitematch.csv")
