# Living Collections Phenology — Script 1: QAQC
# Pulls current-year data from DB, runs all QAQC checks,
# and exports errors to Google Sheets (date-named tab).
# Fix errors in PgAdmin, then run Script 2.
# ============================================================

library(DBI)
library(RPostgreSQL)
library(googlesheets4)
library(lubridate)
library(dplyr)

# 0. CONFIG ------------------------------------------------------------------
yr.now          <- lubridate::year(Sys.Date())
date.now        <- lubridate::date(Sys.Date())
stale.threshold <- 10  # days since last observation before flagging

qaqc.sheet.id <- "1fElgeUZrxTfhiy1Kgt8ix70AsdWaURtmqC-p_pMpi0U"


# 1. PULL DATA ---------------------------------------------------------------
googlesheets4::gs4_auth(email = "breidy@mortonarb.org")
removed <- googlesheets4::read_sheet(
  "16xMa6MyJlh3zKkELrDToyoPk_GfoN1NSCVji_ttOCoQ",
  sheet = "Removed Trees")

con <- DBI::dbConnect(
  RPostgreSQL::PostgreSQL(),
  host     = "164.92.83.213",
  port     = 5432,
  dbname   = "arboretum",
  user     = "arboretum",
  password = "arboretum1234"
)
observers   <- dbReadTable(con, "Observers")
assignments <- dbReadTable(con, "ObserverListCollection")
treeLists   <- dbReadTable(con, "ObservingLists")
datAll      <- dbReadTable(con, "FormSubmission")
dbDisconnect(con)


# 2. PREPARE DATA FRAMES -----------------------------------------------------
datAll$DateEntered  <- as.Date(datAll$DateEntered)
datAll$DateObserved <- as.Date(datAll$DateObserved)

# Remove test submissions and removed trees
datAll <- datAll[datAll$ObserverID != "Test", ]
datAll <- datAll[!datAll$PlantID %in% removed$PlantNumber, ]

# Trim whitespace on key character fields
datAll$ObserverID <- trimws(datAll$ObserverID)
datAll$PlantID    <- trimws(datAll$PlantID)
datAll$Genus      <- trimws(datAll$Genus)
datAll$Species    <- trimws(datAll$Species)

# Subset to current year by DateEntered
datNow <- datAll[!is.na(datAll$DateEntered) & datAll$DateEntered >= as.Date(paste0(yr.now, "-01-01")), ]
datNow <- droplevels(datNow)


# 3. OBSERVER AND PLANT NAME QAQC --------------------------------------------

## Bad observer ID ----
valid.observers    <- c(as.character(observers$ObserverID), "UNKNOWN")
qaqc.bad.observers <- datNow[!datNow$ObserverID %in% valid.observers,
                             c("PlantID", "ObserverID", "DateObserved", "DateEntered")]
qaqc.bad.observers <- qaqc.bad.observers[order(qaqc.bad.observers$ObserverID), ]
qaqc.bad.observers

## PlantIDs not in active tree list ----
known.plants    <- c(as.character(treeLists$PlantID), as.character(removed$PlantNumber))
qaqc.bad.plants <- datNow[!datNow$PlantID %in% known.plants,
                          c("PlantID", "ObserverID", "Genus", "Species", "DateObserved")]
qaqc.bad.plants <- qaqc.bad.plants[order(qaqc.bad.plants$PlantID), ]
qaqc.bad.plants

## Species name mismatch vs. tree list ----
taxon.lookup          <- setNames(as.character(treeLists$Taxon), as.character(treeLists$PlantID))
datNow$Taxon.recorded <- paste(datNow$Genus, datNow$Species)
datNow$Taxon.expected <- taxon.lookup[as.character(datNow$PlantID)]

qaqc.species.mismatch <- datNow[
  !is.na(datNow$Taxon.expected) & datNow$Taxon.recorded != datNow$Taxon.expected,
  c("PlantID", "ObserverID", "Taxon.recorded", "Taxon.expected", "DateObserved")]
qaqc.species.mismatch <- qaqc.species.mismatch[order(qaqc.species.mismatch$PlantID), ]
qaqc.species.mismatch

datNow$Taxon.recorded <- NULL
datNow$Taxon.expected <- NULL

## Observers monitoring trees outside their assigned list ----
assign.merge    <- merge(assignments, treeLists, by = "List", all.x = TRUE)
assigned.plants <- aggregate(PlantID ~ ObserverID, data = assign.merge,
                             FUN = function(x) unique(as.character(x)))

datNow$AssignedPlants    <- assigned.plants$PlantID[match(as.character(datNow$ObserverID),
                                                          assigned.plants$ObserverID)]
datNow$OutsideAssignment <- mapply(function(pid, alist) {
  if (is.null(alist)) return(NA)
  !pid %in% alist
}, datNow$PlantID, datNow$AssignedPlants)

qaqc.outside.assign <- datNow[
  !is.na(datNow$OutsideAssignment) & datNow$OutsideAssignment == TRUE,
  c("PlantID", "ObserverID", "Genus", "Species", "DateObserved")]
qaqc.outside.assign <- qaqc.outside.assign[order(qaqc.outside.assign$ObserverID), ]
qaqc.outside.assign

datNow$AssignedPlants    <- NULL
datNow$OutsideAssignment <- NULL


# 4. DATE QAQC ---------------------------------------------------------------

## Missing DateObserved ----
qaqc.missing.date <- datNow[is.na(datNow$DateObserved),
                            c("PlantID", "ObserverID", "DateEntered")]
qaqc.missing.date

## DateObserved in the future ----
qaqc.future.date <- datNow[
  !is.na(datNow$DateObserved) & datNow$DateObserved > Sys.Date(),
  c("PlantID", "ObserverID", "DateObserved", "DateEntered")]
qaqc.future.date

## DateObserved year doesn't match current year ----
qaqc.wrong.year <- datNow[
  !is.na(datNow$DateObserved) & lubridate::year(datNow$DateObserved) != yr.now,
  c("PlantID", "ObserverID", "DateObserved", "DateEntered")]
qaqc.wrong.year

## DateEntered year doesn't match current year ----
# Catches records entered with a wrong year (e.g. Dec 2025 entry slipping into 2026 pull)
qaqc.wrong.entry.year <- datNow[
  lubridate::year(datNow$DateEntered) != yr.now,
  c("PlantID", "ObserverID", "DateObserved", "DateEntered")]
qaqc.wrong.entry.year

## DateEntered before DateObserved ----
qaqc.entered.before.observed <- datNow[
  !is.na(datNow$DateObserved) & datNow$DateEntered < datNow$DateObserved,
  c("PlantID", "ObserverID", "DateObserved", "DateEntered")]
qaqc.entered.before.observed


# 5. OBSERVER COVERAGE QAQC --------------------------------------------------

## Observers who haven't submitted within stale.threshold days ----
obs.last              <- aggregate(DateObserved ~ ObserverID, data = datNow, FUN = max)
names(obs.last)[2]    <- "LastObservation"
obs.last$DaysSinceObs <- as.integer(Sys.Date() - obs.last$LastObservation)

qaqc.stale.observers <- obs.last[obs.last$DaysSinceObs > stale.threshold, ]
qaqc.stale.observers <- qaqc.stale.observers[order(-qaqc.stale.observers$DaysSinceObs), ]
qaqc.stale.observers

## Observers with no observations this year ----
qaqc.no.obs.observers <- data.frame(
  ObserverID = observers$ObserverID[!observers$ObserverID %in% datNow$ObserverID])
qaqc.no.obs.observers

## Trees with no observation in stale.threshold days ----
tree.last              <- aggregate(DateObserved ~ PlantID, data = datNow, FUN = max)
names(tree.last)[2]    <- "LastObservation"
tree.last$DaysSinceObs <- as.integer(Sys.Date() - tree.last$LastObservation)

qaqc.stale.trees <- tree.last[tree.last$DaysSinceObs > stale.threshold, ]
qaqc.stale.trees <- qaqc.stale.trees[order(-qaqc.stale.trees$DaysSinceObs), ]
qaqc.stale.trees

## Trees in active list with zero observations this year ----
all.active.plants     <- treeLists[!treeLists$PlantID %in% removed$PlantNumber, ]
qaqc.unobserved.trees <- all.active.plants[
  !all.active.plants$PlantID %in% datNow$PlantID, c("PlantID", "Taxon")]
qaqc.unobserved.trees <- qaqc.unobserved.trees[order(qaqc.unobserved.trees$Taxon), ]
qaqc.unobserved.trees

## Observer submission summary (reference, not an error table) ----
obs.summary              <- aggregate(DateObserved ~ ObserverID, data = datNow, FUN = length)
names(obs.summary)[2]    <- "N_observations"
obs.last.merge           <- aggregate(DateObserved ~ ObserverID, data = datNow, FUN = max)
names(obs.last.merge)[2] <- "LastObservation"
obs.summary              <- merge(obs.summary, obs.last.merge, by = "ObserverID")
obs.summary$DaysSinceObs <- as.integer(Sys.Date() - obs.summary$LastObservation)
obs.summary              <- obs.summary[order(-obs.summary$N_observations), ]
obs.summary


# 6. PHENOLOGICAL CONSISTENCY QAQC -------------------------------------------
# Uses only the most recent observation per tree

most.recent.idx <- ave(as.numeric(datNow$DateObserved), datNow$PlantID, FUN = max)
pheno.now       <- datNow[as.numeric(datNow$DateObserved) == most.recent.idx, ]
pheno.now       <- droplevels(pheno.now)

pheno.pairs <- data.frame(
  obs.col  = c("BreakingLeafBudsObserved", "LeafObserved", "LeafIncreasingObserved",
               "LeafColorObserved", "LeafFallingObserved", "FlowerBudsObserved",
               "FlowerOpenObserved", "FlowerPollenObserved", "FruitPresentObserved",
               "FruitRipeObserved", "FruitDropObserved"),
  int.col  = c("BreakingLeafBudsIntensity", "LeafIntensity", "LeafIncreasingIntensity",
               "LeafColorIntensity", "LeafFallingIntensity", "FlowerBudsIntensity",
               "FlowerOpenIntensity", "FlowerPollenIntensity", "FruitPresentIntensity",
               "FruitRipeIntensity", "FruitDropIntensity"),
  zero.val = c("0", "0%", "0%", "0%", "0%", "0", "0%", "None", "0", "0%", "0"),
  stringsAsFactors = FALSE
)
pheno.pairs <- pheno.pairs[
  pheno.pairs$obs.col %in% names(pheno.now) &
    pheno.pairs$int.col %in% names(pheno.now), ]

## Observed = No/Did not look, but intensity is non-zero ----
qaqc.intensity.mismatch <- do.call(rbind, lapply(seq_len(nrow(pheno.pairs)), function(i) {
  obs.col  <- pheno.pairs$obs.col[i]
  int.col  <- pheno.pairs$int.col[i]
  zero.val <- pheno.pairs$zero.val[i]
  bad <- pheno.now[
    pheno.now[, obs.col] %in% c("No", "Did not look for") &
      !is.na(pheno.now[, int.col]) &
      !pheno.now[, int.col] %in% c(zero.val, NA),
    c("PlantID", "ObserverID", "DateObserved", obs.col, int.col)]
  if (nrow(bad) == 0) return(NULL)
  bad
}))
if (!is.null(qaqc.intensity.mismatch))
  qaqc.intensity.mismatch <- qaqc.intensity.mismatch[order(qaqc.intensity.mismatch$PlantID), ]
qaqc.intensity.mismatch

## Observed = Yes but intensity is zero or NA ----
qaqc.obs.no.intensity <- do.call(rbind, lapply(seq_len(nrow(pheno.pairs)), function(i) {
  obs.col  <- pheno.pairs$obs.col[i]
  int.col  <- pheno.pairs$int.col[i]
  zero.val <- pheno.pairs$zero.val[i]
  bad <- pheno.now[
    pheno.now[, obs.col] == "Yes" &
      (is.na(pheno.now[, int.col]) | pheno.now[, int.col] %in% zero.val),
    c("PlantID", "ObserverID", "DateObserved", obs.col, int.col)]
  if (nrow(bad) == 0) return(NULL)
  bad
}))
if (!is.null(qaqc.obs.no.intensity))
  qaqc.obs.no.intensity <- qaqc.obs.no.intensity[order(qaqc.obs.no.intensity$PlantID), ]
qaqc.obs.no.intensity

## Phenological sequence violations ----
# leaf.increasing / leaf.color / leaf.falling require leaf.present = Yes
seq.checks <- data.frame(
  dependent = c("LeafIncreasingObserved", "LeafColorObserved", "LeafFallingObserved"),
  requires  = c("LeafObserved",           "LeafObserved",      "LeafObserved"),
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
    c("PlantID", "ObserverID", "DateObserved", dep, req)]
  if (nrow(bad) == 0) return(NULL)
  bad
}))
if (!is.null(qaqc.sequence))
  qaqc.sequence <- qaqc.sequence[order(qaqc.sequence$PlantID), ]
qaqc.sequence


# 7. EXPORT TO GOOGLE SHEETS -------------------------------------------------
# One combined table, single API write, new date-named tab each run.

prep.sheet <- function(df, check.name) {
  if (is.null(df) || nrow(df) == 0) {
    df <- data.frame(Note = paste("No", check.name, "errors found"),
                     stringsAsFactors = FALSE)
  }
  df$QAQC_Check <- check.name
  df$Fixed      <- ""
  df$Notes      <- ""
  df
}

qaqc.errors <- list(
  bad_observer_id         = prep.sheet(qaqc.bad.observers,           "bad_observer_id"),
  bad_plant_id            = prep.sheet(qaqc.bad.plants,              "bad_plant_id"),
  species_mismatch        = prep.sheet(qaqc.species.mismatch,        "species_mismatch"),
  outside_assignment      = prep.sheet(qaqc.outside.assign,          "outside_assignment"),
  missing_date_observed   = prep.sheet(qaqc.missing.date,            "missing_date_observed"),
  future_date             = prep.sheet(qaqc.future.date,             "future_date"),
  wrong_year_observed     = prep.sheet(qaqc.wrong.year,              "wrong_year_observed"),
  wrong_year_entered      = prep.sheet(qaqc.wrong.entry.year,        "wrong_year_entered"),
  entered_before_observed = prep.sheet(qaqc.entered.before.observed, "entered_before_observed"),
  stale_observer          = prep.sheet(qaqc.stale.observers,         "stale_observer"),
  no_obs_observer         = prep.sheet(qaqc.no.obs.observers,        "no_obs_observer"),
  stale_tree              = prep.sheet(qaqc.stale.trees,             "stale_tree"),
  unobserved_tree         = prep.sheet(qaqc.unobserved.trees,        "unobserved_tree"),
  intensity_mismatch      = prep.sheet(qaqc.intensity.mismatch,      "intensity_mismatch"),
  obs_without_intensity   = prep.sheet(qaqc.obs.no.intensity,        "obs_without_intensity"),
  sequence_violation      = prep.sheet(qaqc.sequence,                "sequence_violation")
)

qaqc.combined <- dplyr::bind_rows(qaqc.errors)
front.cols    <- c("QAQC_Check", "Fixed", "Notes")
other.cols    <- setdiff(names(qaqc.combined), front.cols)
qaqc.combined <- qaqc.combined[, c(front.cols, other.cols)]

tab.name <- as.character(date.now)
googlesheets4::sheet_add(ss = qaqc.sheet.id, sheet = tab.name)
googlesheets4::sheet_write(data = qaqc.combined, ss = qaqc.sheet.id, sheet = tab.name)
