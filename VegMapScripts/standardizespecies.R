# Read in species tables from EDIT
library(jsonlite)
library(dplyr)

# Define MLRA and state
mlra <- "042X"
# Read in sites of interest
sites <- read.csv("sites.csv")

# Build the base web services URL needed to retrieve the ecological site list
classlist <- jsonlite::fromJSON(paste0("https://edit.jornada.nmsu.edu/services/downloads/esd/", mlra, "/class-list.json"))

# Read the ecological site list into a data table, skipping the the first two lines, which contain metadata
ecoclasses <- as.data.frame(classlist$ecoclasses)
# Subset to sites of interest
ecoclasses <- subset(ecoclasses, ecoclasses$id %in% autocrosswalk$Site)

# Loop through ecological site list for MLRA and state to pull data of interest
base.url <- paste0("https://edit.jornada.nmsu.edu/services/descriptions/esd/", mlra)


# Extract nominal key features
# Landforms
species <- data.frame(NULL)
for(row in 1:nrow(ecoclasses)) {
  ecoclass.id <- ecoclasses[row, 2] 
  doc.url <- jsonlite::fromJSON(paste0(base.url, "/", ecoclass.id, ".json")) 
  specieslist <- data.frame(doc.url$ecologicalDynamics$narratives)
  landformlist <- as.data.frame(t(landformlist))
  if(ncol(landformlist) == 0) {
    next
  } else {
    if(ncol(landformlist) == 2) {
      landformlist$V3 <- NA 
    } else {
      if(ncol(landformlist) == 1) {
        landformlist$V2 <- NA
        landformlist$V3 <- NA }}}
  landformlist <- tidyr::unite(landformlist, String, V1, V2, V3, sep = " | ")
  landformlist$Property <- "Landform"
  landformlist$siteid <- ecoclass.id
  rownames(landformlist) <- NULL
  landforms <- rbind(landforms, landformlist)}

