#### Sandy EcoSite Specific State Key created from ESD info only ####

## Load needed R packages ##
library(tidyverse)
library(terradactyl)

#### Set file paths for access to needed files ####

## Path to AIM GDB ## 
TerrADat_Path <- dsn <- "C:\\Users\\aheller\\Documents\\Raw Data\\AIMTerrestrialEdtBackup1-25-22.gdb"

## Path to list of accepted ecosite names from EDIT ##
EDIT_List_Path <- "C:\\Users\\aheller\\Documents\\Jornada_Veg_Maps\\VegMapScripts\\VegMapScripts\\EDIT_public_ecological_site_list.csv"

## Set MLRA of interest
plots <- sf::st_read(dsn = dsn, layer = "tblPlots")


## Read in LPI data
# Point to species file
species_file <- "C:\\Users\\aheller\\Documents\\Jornada_Veg_Maps\\VegMapScripts\\VegMapScripts\\NM_BLM_SpeciesList.csv"

# Gather LPI
tdat.lpi.tall <- terradactyl::gather_lpi (dsn, file_type = "gdb", source = "TerrADat")

# Gather species
gather.species <- terradactyl::gather_species(species_file, species_growth_habit_code = "GrowthHabitSub",
                                 growth_habit_file = species_file, growth_habit_code = "GrowthHabitCode")


# Create a tall LPI table
lpi.pct.cover <- terradactyl::pct_cover(lpi_tall = tdat.lpi.tall, tall = TRUE, by_line = FALSE, hit = "any", code)






### Set up custom indicator

###   Read in LPI and header
# Most recently run tall tables
lpi <-  readRDS(paste(Tall_Table_Path , "lpi_tall.RData" , sep = ""))
header <- readRDS(paste(Tall_Table_Path , "header.RData" , sep = "")) 

header_state <- header %>% filter(State == "NM") 
state_primarykeys <- header_state$PrimaryKey

lpi_NM <- lpi %>% filter(PrimaryKey %in% state_primarykeys) #Get LPI subset or custom indicator will take forever


CustomIndicator <- read.csv("C:/Users/schalln/OneDrive - New Mexico State University/Desktop/ESDKeys_JWS/ESD_Keys/OriginalCode_SiteKey/FullKeys_NotGeneralized/CustomIndicator_R042XB012NM.csv")

Bunchgrass <- CustomIndicator$Species

## Pull out everything in LPI with bunchgrass present (or the bunchgrasses we care about for this ecological site)

lpi_NM_bunchgrass <- lpi_NM %>% filter(code %in% Bunchgrass)

### This could probably be functionalized 

# Create a column named bunchgrass and give it the value of Bunchgrass

lpi_NM_bunchgrass$Bunchgrass <- "Bunchgrass"

#Pull out everything without bunchgrass 

lpi_NM_NonBunch <- lpi_NM %>% filter(!code %in% Bunchgrass)

# Create a column named bunchgrass and give it value Non-Bunchgrass

lpi_NM_NonBunch$Bunchgrass <- "Non-Bunchgrass"
#Now bind them back together

lpi_NM_bunchgrass_populated <- rbind(lpi_NM_bunchgrass , lpi_NM_NonBunch)

#We need AH and FH because FH would indicate interspaces (see STM)

bunchgrass_AH <- terradactyl::pct_cover(lpi_tall = lpi_NM_bunchgrass_populated,
                                        tall = TRUE,
                                        hit = "any",
                                        by_line = FALSE,
                                        Bunchgrass) # Group by bunchgrass variable (or other custom indicator in future)

bunchgrass_AH <- bunchgrass_AH %>% subset(indicator == "BUNCHGRASS") %>% rename(AH_Bunchgrass = percent)

bunchgrass_FH <- terradactyl::pct_cover(lpi_tall = lpi_NM_bunchgrass_populated,
                                        tall = TRUE,
                                        hit = "first",
                                        by_line = FALSE,
                                        Bunchgrass)

bunchgrass_FH <- bunchgrass_FH %>% subset(indicator == "BUNCHGRASS") %>% rename(FH_Bunchgrass = percent)

bunchgrass_indicator <- merge(bunchgrass_AH , bunchgrass_FH , by = "PrimaryKey") %>% select(PrimaryKey, AH_Bunchgrass, FH_Bunchgrass)

### Now that we have the custom indicator, we'll bind it with the rest of the data

### First get the data set up


# After the custom indicator is calculated, read in TerrADat and LMF. 


# Read in TerrADat

TerrADat <- sf::st_read(dsn=dsn, layer="TerrADat")

# Read in TerrADAt species indicator
TerrADat_Species <- sf::st_read(dsn=dsn, layer="TerrADatSpeciesIndicators")

TerrADat <- as.data.frame(TerrADat) 

TerrADat_Species <- as.data.frame(TerrADat_Species)

# Read in LMF

LMF <-  sf::st_read(dsn=dsn, layer="LMF")

# Read in LMF Species Indicator
LMF_Species <- sf::st_read(dsn=dsn, layer="LMFSpeciesIndicators")

LMF <- as.data.frame(LMF)

LMF_Species <- as.data.frame(LMF_Species)

LMF <- LMF %>% rename(DateEstablished = DateVisited) #this will be an issue if revists become a thing in LMF

# Read in the ecological site full name file 
# We have to fix the names of LMF Ecological Sites

EDIT <- read.csv("C:/Users/schalln/OneDrive - New Mexico State University/Desktop/ESDKeys_JWS/SiteSummaryTool/defaults/EDIT_public_ecological_site_list.csv")
EDIT[["EcoSiteId_Stripped"]] <- gsub(EDIT[["new_es_symbol"]],
                                     pattern = "^[RF]", replacement = "")
#Check to see if unique
ecosite_lut <- unique(EDIT[,c("new_es_symbol" , "EcoSiteId_Stripped")])
any(table(ecosite_lut[["EcoSiteId_Stripped"]]) > 1)
trouble_ids <- names(table(ecosite_lut[["EcoSiteId_Stripped"]]))[table(ecosite_lut[["EcoSiteId_Stripped"]]) > 1]

#Drop the repeat ids
ecosite_lut_drop_bad <- ecosite_lut %>% filter(!EcoSiteId_Stripped == trouble_ids)

#Add a new field called EcologicalSiteId that has the dropped R and F

EcoSites_Update <- ecosite_lut_drop_bad %>% mutate(EcologicalSiteId = EcoSiteId_Stripped)

#Merge the dataframe with the full EcologicalSiteId and dropped R/F Id with the LMF

LMF_EcoSite <- merge(LMF , EcoSites_Update, by = "EcologicalSiteId")

#Drop the EcologicalSiteId value that we added earlier
LMF_EcoSite <- LMF_EcoSite %>% dplyr::select(-EcologicalSiteId)

#Rename Ecological Site Id to the full Ecological Site Id code (= new_es_symbol)
LMF_EcoSite <- LMF_EcoSite %>% dplyr::rename(EcologicalSiteId = new_es_symbol)
LMF_EcoSite <- LMF_EcoSite %>% dplyr::select(-EcoSiteId_Stripped)

#Bind LMF and TerrADat
#Place NAs in non-matching columns
TerrADat[setdiff(names(LMF_EcoSite) , names(TerrADat))] <- NA
LMF_EcoSite[setdiff(names(TerrADat), names(LMF_EcoSite))] <- NA

TDat_LMF <- rbind(TerrADat , LMF_EcoSite)

#Now bind the species indicators
#Bind LMF and TerrADat Species Indicators
#First need names to match, TDat species got cut off (comment this out if did not first export to shapefile)
#place NAs in non-matching columns
LMF_Species[setdiff(names(TerrADat_Species) , names(LMF_Species))] <- NA
TerrADat_Species[setdiff(names(LMF_Species), names(TerrADat_Species))] <- NA
SpeciesIndicators <- rbind(TerrADat_Species , LMF_Species)

#Subset based on EcologicalSiteId

EcoSitePlots <- TDat_LMF[TDat_LMF[["EcologicalSiteId"]] %in% EcoSite, ]

#for non-species indicators
EcoSite_PKs <- EcoSitePlots$PrimaryKey

#Keeping only the plots from accumulated species that match our EcoSite
Species_plots_ecosite <- SpeciesIndicators[(SpeciesIndicators[["PrimaryKey"]] %in% EcoSite_PKs), ]

#Now we'll bind TDat_LMF with the species indicators
Species_plots_ecosite[setdiff(names(EcoSitePlots) , names(Species_plots_ecosite))] <- NA
EcoSitePlots[setdiff(names(Species_plots_ecosite), names(EcoSitePlots))] <- NA

FullData <- rbind(Species_plots_ecosite , EcoSitePlots)

#Subset the custom indicator to the ecological site 
names(bunchgrass_indicator)
any(bunchgrass_indicator[["PrimaryKey"]] %in% EcoSite_PKs)
bunchgrass_ecosite <- bunchgrass_indicator[(bunchgrass_indicator[["PrimaryKey"]] %in% EcoSite_PKs), ]

bunchgrass_tall <- gather(bunchgrass_ecosite, key = Species, value = Percent , AH_Bunchgrass:FH_Bunchgrass)


# Pull in species key

## Read in Excel Key built from SDM
SpeciesKey <- read.csv("C:/Users/schalln/OneDrive - New Mexico State University/Desktop/ESDKeys_JWS/ESD_Keys/OriginalCode_SiteKey/FullKeys_NotGeneralized/Key_R042XB012NM.csv")
KeySpeciesIndicators <- unique(SpeciesKey$Species)

FullDataTrim <- FullData %>% select(PrimaryKey, Species, AH_SpeciesCover,  matches(paste(KeySpeciesIndicators, collapse = "|")))  

##Turn NAs to 0##

FullDataTrim$AH_SpeciesCover[is.na(FullDataTrim$AH_SpeciesCover)]<-0

FullDataTrim <- FullDataTrim %>% rename(Percent = AH_SpeciesCover)

## Now combine

FullData_PlusCustom <- rbind(FullDataTrim, bunchgrass_tall)

### Now we need to get the species that are necessary for the key into the data
### It will get a value of 0 if in the key but missing from the data

all_species <- KeySpeciesIndicators[grepl(KeySpeciesIndicators , pattern = "^[A-Z]{3,6}\\d{0,5}$")] 

unique_species <- unique(as.character(all_species))

missing_species_list <- lapply(X = unique(FullData_PlusCustom$PrimaryKey) , data = FullData_PlusCustom , Species = unique_species ,
                               FUN = function(X , data, Species){
                                 current_data <- data[data$PrimaryKey == X ,]
                                 missing_species <- Species[!Species %in% current_data$Species]
                                 if(length(missing_species) > 0){
                                   output <- data.frame(PrimaryKey = X, 
                                                        Species = missing_species, 
                                                        Percent = 0 , stringsAsFactors = FALSE)
                                 }else{
                                   output <- NULL
                                 }
                                 return(output)
                                 
                               })

missing_data <- bind_rows(missing_species_list)

FullData_Tall_Populated <- rbind(missing_data, FullData_PlusCustom)


# to check if all custom indicator populated
ah_fh <- FullData_Tall_Populated %>% filter(Species == "AH_Bunchgrass" | Species == "FH_Bunchgrass")
all(ah_fh$PrimaryKey %in% FullData_Tall_Populated[["PrimaryKey"]])
# if true, yes

# join with the key to get a phase assigned to species and then filter NA's since we don't care about species that don't impact the key

Joined <- dplyr::full_join(FullData_Tall_Populated, SpeciesKey, by = c("Species")) %>% filter(!is.na(Phase)) %>% #join and filter out irrelevant species/indicators 
  #also filter out NA's in Percent because they are artifacts of merging TerrADat with TerrADat Species Indicators
  unique()

length(unique(Joined$PrimaryKey))

### Now apply function to the joined data frame to apply key logic

Conditional_Paste <- Joined %>% mutate(Eval_Lower = paste(Percent, Lower.Relation, Lower.Limit), Eval_Upper = paste(Percent, Upper.Relation, Upper.Limit))

eval_vars <- names(Conditional_Paste)[grep(names(Conditional_Paste) , pattern = "^Eval_")]

benchmark_vector <- sapply(X = 1:nrow(Conditional_Paste),
                           data = Conditional_Paste,
                           eval_vars = eval_vars,
                           FUN = function(X, data, eval_vars){
                             all(sapply(X = eval_vars,
                                        data = data[X, ],
                                        FUN = function(X, data){
                                          evalstring <- data[[X]]
                                          eval(parse(text = evalstring))
                                        }))
                           })

Conditional_Paste$benchmark_vector <- benchmark_vector

#This first summary will only include plots that meet all critera for a state/phase combo
Summary <- Conditional_Paste %>% group_by(PrimaryKey , Phase) %>% summarize(Final.Phase = all(benchmark_vector)) 
output_summary1 <- Summary[Summary$Final.Phase, c("PrimaryKey", "Phase")]

# This summary will rank the likelihood of a plot belonging to a certain state/phase
Summary2 <- Conditional_Paste %>% group_by(PrimaryKey , Phase) %>% summarize(Likelihood.Final.Phase = sum(benchmark_vector)/length(benchmark_vector))  
# Here I am checking to make sure that if a plot has a 1 for rank, it was icluded in the first summar
AllTrue <- Summary2 %>% subset(Likelihood.Final.Phase >= 1)
length(unique(Summary2$PrimaryKey)) #This should equal the number of total plots. 

Summary_Rank <- Summary2 %>% group_by(PrimaryKey) %>% top_n(3, Likelihood.Final.Phase)

Summary_Rank_Top <- Summary2 %>% group_by(PrimaryKey) %>% top_n(1, Likelihood.Final.Phase)

write.csv(Summary_Rank_Top, paste(Sys.Date(), "StateKeyOutput_specific", EcologicalSiteId, ".csv", sep = ""), row.names = FALSE)

good_fit<-Summary_Rank_Top %>% filter(Likelihood.Final.Phase > 0.5)
length(unique(good_fit$PrimaryKey))

duplicated(good_fit$PrimaryKey)

View(FullData_Tall_Populated %>% filter(PrimaryKey == "17120410561463082017-09-01"))
