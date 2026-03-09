# Living Collections Phenology QAQC Script for intermittent QAQC checks throughout the year
#Libraries---------------------------------------------------------------------------------------------------------------------------------------------------
library(DBI)
library(RPostgreSQL)
library(googlesheets4)
library(lubridate)
library(openxlsx)

# 0. CONFIG----
###set up parameters for organizing data/ pathing----
yr.now<- lubridate::year(Sys.Date())
date.now<- lubridate::date(Sys.Date())
stale.threshold <- 10  # days since last observation before flagging

dir.out <- "~/Google Drive/My Drive/LivingCollections_Phenology/QAQC data check"

# 1. PULL DATA------------------------------------------------------------------------------------------------------------------------------------------------
###connect to removed trees sheet ----
googlesheets4::gs4_auth(email = "breidy@mortonarb.org") # will need to be changed to seprate google account
removed <- googlesheets4::read_sheet( "16xMa6MyJlh3zKkELrDToyoPk_GfoN1NSCVji_ttOCoQ",sheet = "Removed Trees")# file path here may be better

##connect to pgadmin data base to to pull recent data, observer lists, and tree lists----
db_host     <- "164.92.83.213" # host IP for PgAdmin database
db_port     <- 5432 # connect to port
db_name     <- "arboretum"
db_user     <- "arboretum" #only one user name
db_password <- "arboretum1234" #only one password

con <- dbConnect(
  PostgreSQL(),
  host     = db_host,
  port     = db_port,
  dbname   = db_name,
  user     = db_user,
  password = db_password
)
#more tables can be pulled from but these are the useful ones for QAQC at this point
observers   <- dbReadTable(con, "Observers")
assignments <- dbReadTable(con, "ObserverListCollection")
treeLists   <- dbReadTable(con, "ObservingLists")
datAll      <- dbReadTable(con, "FormSubmission")
dbDisconnect(con) # disconnect from database


#2. Preparing data frames----

#### Make sure dates are being read as dates----
datAll$DateEntered  <- as.Date(datAll$DateEntered)
datAll$DateObserved <- as.Date(datAll$DateObserved)

## Remove test submissions and removed trees----
datAll <- datAll[datAll$ObserverID != "Test", ]
datAll <- datAll[!datAll$PlantID %in% removed$PlantNumber, ]

## Trim whitespace on key character fields in case an observer added a space "_"----
datAll$ObserverID <- trimws(datAll$ObserverID)
datAll$PlantID    <- trimws(datAll$PlantID)
datAll$Genus      <- trimws(datAll$Genus)
datAll$Species    <- trimws(datAll$Species)

 ## Subset all data to only incluse observations within the current year----
datNow <- datAll[!is.na(datAll$DateEntered) & datAll$DateEntered >= as.Date(paste0(yr.now, "-01-01")), ]
datNow <- droplevels(datNow)
summary(datNow)

# 3. DATA SNAPSHOT -----------------------------------------------------------------------------------------
# This is to get a quick look at number of phebnology observations 

# snap.sum will give an up to date picture of total observations, unique observers trees observed and genera, will also give 
#the range of dates observed and the # of missing trees that have been observed 
##Summary metrics set up----
snap.sum <- data.frame( Metric = c( "Total observations", "Unique observers","Unique trees","Unique genera",
"Earliest DateObserved","Latest DateObserved","Missing DateObserved" ),
  
Value = c(nrow(datNow),length(unique(datNow$ObserverID)),length(unique(datNow$PlantID)),length(unique(datNow$Genus)),
as.character(min(datNow$DateObserved, na.rm = TRUE)),as.character(max(datNow$DateObserved, na.rm = TRUE)),sum(is.na(datNow$DateObserved))))

## Current year at a glance----
snap.sum  

# Unique genera will give a snapshot of the number of observations in each genra up to this date
genera.observed <- data.frame(
  Genus = sort(unique(as.character(datNow$Genus))), N_observations = tabulate(match(datNow$Genus, sort(unique(datNow$Genus)))))
##Unique observartions of each genera----
genera.observed

# Unique observer IDs  will show the names of observers who have made observations,and if their names
#are currently on our observer list
observer.snap <- data.frame(
ObserverID = sort(unique(as.character(datNow$ObserverID))),  InReferenceTable = sort(unique(as.character(datNow$ObserverID))) %in%
as.character(observers$ObserverID))
##Quick observer name check----
observer.snap


# 4. Observer and plant name QAQC-----------------------------------------------------------------------------------------
#This section handles common errors in obvserver and plant name

##Incorrect observer ID ----
valid.observers    <- c(as.character(observers$ObserverID), "UNKNOWN")
qaqc.bad.observers <- datNow[!datNow$ObserverID %in% valid.observers, c("PlantID", "ObserverID", "DateObserved", "DateEntered")]
qaqc.bad.observers <- qaqc.bad.observers[order(qaqc.bad.observers$ObserverID), ]
qaqc.bad.observers

##PlantIDs not in the active tree list----
known.plants    <- c(as.character(treeLists$PlantID), as.character(removed$PlantNumber))
qaqc.bad.plants <- datNow[!datNow$PlantID %in% known.plants, c("PlantID", "ObserverID", "Genus", "Species", "DateObserved")]
qaqc.bad.plants <- qaqc.bad.plants[order(qaqc.bad.plants$PlantID), ]
qaqc.bad.plants

## Species name mismatch vs. tree list ----
# Loookup PlantID vs expected Taxon create columns for taxon names that are record and expected for comparison 
taxon.lookup          <- setNames(as.character(treeLists$Taxon), as.character(treeLists$PlantID))
datNow$Taxon.recorded <- paste(datNow$Genus, datNow$Species)
datNow$Taxon.expected <- taxon.lookup[as.character(datNow$PlantID)]

qaqc.species.mismatch <- datNow[!is.na(datNow$Taxon.expected) & datNow$Taxon.recorded != datNow$Taxon.expected,
c("PlantID", "ObserverID", "Taxon.recorded", "Taxon.expected", "DateObserved")]
qaqc.species.mismatch <- qaqc.species.mismatch[order(qaqc.species.mismatch$PlantID), ]
qaqc.species.mismatch

#remove expected and recorded columns
datNow$Taxon.recorded <- NULL
datNow$Taxon.expected <- NULL

##Observers monitoring trees outside their assigned list----
# Loookup ObserverID vs all PlantIDs they are assigned to
# Function to merge lists of assignments and treelist with observer ID
assign.merge    <- merge(assignments, treeLists, by.x = c("Collection", "List"), by.y  = c("Collection", "List"),all.x = TRUE)
assigned.plants <- aggregate(PlantID ~ ObserverID, data = assign.merge, FUN = function(x) unique(as.character(x)))

#Function to look at trees being observed by an observer outside of their list
datNow$AssignedPlants    <- assigned.plants$PlantID[ match(as.character(datNow$ObserverID), assigned.plants$ObserverID)]
datNow$OutsideAssignment <- mapply(function(pid, alist) {
  if (is.null(alist)) return(NA)
  !pid %in% alist
}, datNow$PlantID, datNow$AssignedPlants)

qaqc.outside.assign <- datNow[!is.na(datNow$OutsideAssignment) & datNow$OutsideAssignment == TRUE,c("PlantID", "ObserverID", "Genus", "Species", "DateObserved")]
qaqc.outside.assign <- qaqc.outside.assign[order(qaqc.outside.assign$ObserverID), ]
qaqc.outside.assign

#remove columns for assigned plants and plants outside of observer list
datNow$AssignedPlants    <- NULL
datNow$OutsideAssignment <- NULL



# 5. DATE QAQC-----------------------------------------------------------------------------------------
#Checking common date errors

## Missing DateObserved -----
qaqc.missing.date <- datNow[is.na(datNow$DateObserved),c("PlantID", "ObserverID", "DateEntered")]
qaqc.missing.date # records with no date observed

## DateObserved in the future -----
qaqc.future.date <- datNow[!is.na(datNow$DateObserved) & datNow$DateObserved > Sys.Date(),c("PlantID", "ObserverID", "DateObserved", "DateEntered")]
qaqc.future.date # records where the tree has been observed on a date in the future 

## DateObserved year doesn't match current year -----
qaqc.wrong.year <- datNow[!is.na(datNow$DateObserved) &lubridate::year(datNow$DateObserved) != yr.now,c("PlantID", "ObserverID", "DateObserved", "DateEntered")]
qaqc.wrong.year  # Observations with wrong year

## DateEntered before DateObserved -----
qaqc.entered.before.observed <- datNow[!is.na(datNow$DateObserved) &datNow$DateEntered < datNow$DateObserved,c("PlantID", "ObserverID", "DateObserved", "DateEntered")]
qaqc.entered.before.observed  #  Date Entered before the observation was supposedly made


# 6. Observer coverage QAQC-----------------------------------------------------------------------------------------
# Checcks for observers and trees that have no observations for the last ten days

## Observers who haven't submitted within ten days -----
obs.last <- aggregate(DateObserved ~ ObserverID, data = datNow, FUN = max)
names(obs.last)[2] <- "LastObservation"
obs.last$DaysSinceObs <- as.integer(Sys.Date() - obs.last$LastObservation)

qaqc.stale.observers <- obs.last[obs.last$DaysSinceObs > stale.threshold, ]
qaqc.stale.observers <- qaqc.stale.observers[order(-qaqc.stale.observers$DaysSinceObs), ]
qaqc.stale.observers  # Observers overdue for submission

## Observers with no observations this year -----
qaqc.no.obs.observers <- observers[!observers$ObserverID %in% datNow$ObserverID, c("ObserverID")]
qaqc.no.obs.observers

## Trees with no observation in ten days -----
tree.last <- aggregate(DateObserved ~ PlantID, data = datNow, FUN = max)
names(tree.last)[2] <- "LastObservation"
tree.last$DaysSinceObs <- as.integer(Sys.Date() - tree.last$LastObservation)

qaqc.stale.trees <- tree.last[tree.last$DaysSinceObs > stale.threshold, ]
qaqc.stale.trees <- qaqc.stale.trees[order(-qaqc.stale.trees$DaysSinceObs), ]
qaqc.stale.trees  # Trees overdue for observation

## Trees in the active list with zero observations this year -----
qaqc.unobserved.trees <- qaqc.unobserved.trees[order(qaqc.unobserved.trees$Collection, qaqc.unobserved.trees$Taxon), ]
qaqc.unobserved.trees  # Trees with no observations at all this year

# Summary of number of observations made by each observer and the days since they last observed
## Observer submission summary -----
obs.summary <- aggregate(DateObserved ~ ObserverID, data = datNow, FUN = length)
names(obs.summary)[2] <- "N_observations"
obs.last.merge <- aggregate(DateObserved ~ ObserverID, data = datNow, FUN = max)
names(obs.last.merge)[2] <- "LastObservation"
obs.summary <- merge(obs.summary, obs.last.merge, by = "ObserverID")
obs.summary$DaysSinceObs <- as.integer(Sys.Date() - obs.summary$LastObservation)
obs.summary <- obs.summary[order(-obs.summary$N_observations), ]
obs.summary  # Full observer activity summary


# 7. Odd phenology QAQC-----------------------------------------------------------------------------
# Check of common pheophase errors

# Configure data frame to use only the most recent observation per tree
most.recent.idx <- ave(as.numeric(datNow$DateObserved), datNow$PlantID, FUN = max)
pheno.now       <- datNow[as.numeric(datNow$DateObserved) == most.recent.idx, ]
pheno.now       <- droplevels(pheno.now)

#pairing phenology presence and intensity of phenology observed and setting possible values for 0%
pheno.pairs <- data.frame(
  obs.col = c(
    "leaf.buds.observed",       "leaf.present.observed",
    "leaf.increasing.observed", "leaf.color.observed",
    "leaf.falling.observed",    "flower.buds.observed",
    "flower.open.observed",     "flower.pollen.observed",
    "fruit.present.observed",   "fruit.ripe.observed",
    "fruit.drop.observed"
  ),
  int.col = c(
    "leaf.buds.intensity",       "leaf.present.intensity",
    "leaf.increasing.intensity", "leaf.color.intensity",
    "leaf.falling.intensity",    "flower.buds.intensity",
    "flower.open.intensity",     "flower.pollen.intensity",
    "fruit.present.intensity",   "fruit.ripe.intensity",
    "fruit.drop.intensity"
  ),
  zero.val = c(
    "0", "0%", "0%", "0%", "0%",
    "0", "0%", "None", "0", "0%", "0"
  ),
  stringsAsFactors = FALSE
)

# Keep only pairs where both columns exist in pheno.now
pheno.pairs <- pheno.pairs[pheno.pairs$obs.col %in% names(pheno.now) & pheno.pairs$int.col %in% names(pheno.now), ]

## Observed =No but intensity !zero----
qaqc.intensity.mismatch <- do.call(rbind, lapply(seq_len(nrow(pheno.pairs)), function(i) {
  obs.col  <- pheno.pairs$obs.col[i]
  int.col  <- pheno.pairs$int.col[i]
  zero.val <- pheno.pairs$zero.val[i]
  bad <- pheno.now[
    pheno.now[, obs.col] %in% c("No", "Did not look for") &
      !is.na(pheno.now[, int.col]) &
      !pheno.now[, int.col] %in% c(zero.val, NA),
    c("PlantID", "ObserverID", "DateObserved", obs.col, int.col)
  ]
  if (nrow(bad) == 0) return(NULL)
  bad
}))
if (!is.null(qaqc.intensity.mismatch)) qaqc.intensity.mismatch <- qaqc.intensity.mismatch[order(qaqc.intensity.mismatch$PlantID), ]

qaqc.intensity.mismatch


##Observed = Yes but intensity is zero or NA -----
qaqc.obs.no.intensity <- do.call(rbind, lapply(seq_len(nrow(pheno.pairs)), function(i) {
  obs.col  <- pheno.pairs$obs.col[i]
  int.col  <- pheno.pairs$int.col[i]
  zero.val <- pheno.pairs$zero.val[i]
  bad <- pheno.now[
    pheno.now[, obs.col] == "Yes" &
      (is.na(pheno.now[, int.col]) | pheno.now[, int.col] %in% zero.val),
    c("PlantID", "ObserverID", "DateObserved", obs.col, int.col)
  ]
  if (nrow(bad) == 0) return(NULL)
  bad
}))
if (!is.null(qaqc.obs.no.intensity)) qaqc.obs.no.intensity <- qaqc.obs.no.intensity[order(qaqc.obs.no.intensity$PlantID), ]
qaqc.obs.no.intensity

##Check of common sequence errors -----
# leaf.increasing = Yes requires leaf.present = Yes
# leaf.color = Yes requires leaf.present = Yes
# leaf.falling = Yes requires leaf.present = Yes

seq.checks <- data.frame(
  dependent = c("leaf.increasing.observed", "leaf.color.observed",   "leaf.falling.observed"),
  requires  = c("leaf.present.observed",    "leaf.present.observed", "leaf.present.observed"),
  stringsAsFactors = FALSE
)
seq.checks <- seq.checks[
  seq.checks$dependent %in% names(pheno.now) &
    seq.checks$requires  %in% names(pheno.now), ]

qaqc.sequence <- do.call(rbind, lapply(seq_len(nrow(seq.checks)), function(i) {
  dep <- seq.checks$dependent[i]
  req <- seq.checks$requires[i]
  bad <- pheno.now[
    pheno.now[, dep] == "Yes" & pheno.now[, req] == "No",
    c("PlantID", "ObserverID", "DateObserved", dep, req)
  ]
  if (nrow(bad) == 0) return(NULL)
  bad
}))
if (!is.null(qaqc.sequence)) qaqc.sequence <- qaqc.sequence[order(qaqc.sequence$PlantID), ]
qaqc.sequence


# 8. Export ---------------------------------------------------------------------------------------
# Writing out an xlsx file to check errors 
## Setting up a list of errors------
qaqc.errors <- list(
  bad_observer_id         = qaqc.bad.observers,
  bad_plant_id            = qaqc.bad.plants,
  species_mismatch        = qaqc.species.mismatch,
  outside_assignment      = qaqc.outside.assign,
  missing_date_observed   = qaqc.missing.date,
  future_date             = qaqc.future.date,
  wrong_year              = qaqc.wrong.year,
  entered_before_observed = qaqc.entered.before.observed,
  stale_observer          = qaqc.stale.observers,
  no_obs_observer         = qaqc.no.obs.observers,
  stale_tree              = qaqc.stale.trees,
  unobserved_tree         = qaqc.unobserved.trees,
  intensity_mismatch      = qaqc.intensity.mismatch,
  obs_without_intensity   = qaqc.obs.no.intensity,
  sequence_violation      = qaqc.sequence
)

#pathing xlsx file out
write.xlsx(qaqc.errors,
           file = file.path(dir.out, paste0("QAQC_", date.now, "_errors.xlsx")))


