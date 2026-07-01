# #######################################################
## Pre-Archive QAQC — Living Collection Phenology Data
# #######################################################
# This script reads raw *_FINAL.csv files, checks them against the archiving
# standard, writes a multi-tab flag workbook for review, and writes CLEANED
# (whitespace-trimmed, placeholder-stripped) copies of each input file.
#
# It does NOT do the final archive split/write — that's 2_archive_phenology.R,
# which reads the cleaned files this script produces.

library(openxlsx)   # for the multi-tab .xlsx flag workbook  (install.packages("openxlsx") once)

# ---- paths ----
path.google <- "~/Google Drive/My Drive"
path.base   <- file.path(path.google, "LivingCollections_Phenology")
path.data   <- file.path(path.base, "Data_Observations")  # input: the *_FINAL.csv files
path.clean  <- file.path(path.base, "Data_Cleaned")        # output: cleaned per-file CSVs (input to script 2)
path.qaqc   <- file.path(path.base, "QAQC_flags")          # output: flag list (created if missing)

# Date format of Date.Observed in the raw files. as.Date() with no format only
# works for ISO (YYYY-MM-DD) or locale-default strings — anything else parses
# to NA *silently*, and an NA date was previously excluded from the
# "date outside file Year" check, so bad dates could slip through unflagged.
# Set this to match your actual source format, e.g. "%m/%d/%Y".
date.format <- "%Y-%m-%d"


# #######################################################
# The archiving standard
# #######################################################
expected.cols <- c("Timestamp", "Observer", "Date.Observed", "Species", "PlantNumber", "Notes",
                   "leaf.breaking.buds.observed", "leaf.breaking.buds.intensity",
                   "leaf.present.observed", "leaf.present.intensity",
                   "leaf.increasing.observed", "leaf.increasing.intensity",
                   "leaf.color.observed", "leaf.color.intensity",
                   "leaf.falling.observed",
                   "flower.buds.observed", "flower.buds.intensity",
                   "flower.open.observed", "flower.open.intensity",
                   "flower.pollen.observed", "flower.pollen.intensity",
                   "fruit.present.observed", "fruit.present.intensity",
                   "fruit.ripe.observed", "fruit.ripe.intensity",
                   "fruit.drop.observed", "fruit.drop.intensity",
                   "Collection", "Year")

required.cols <- c("Date.Observed", "Species", "PlantNumber", "Collection", "Year")
text.cols     <- c("Observer", "Species", "PlantNumber", "Notes", "Collection")
obs.cols      <- grep("\\.observed$",  expected.cols, value = TRUE)
int.cols      <- grep("\\.intensity$", expected.cols, value = TRUE)

observed.valid <- c("Yes", "No", "?", "Unsure", "No Observation", "Did not look for")

scale.percent <- c("0%", "<5%", "5-24%", "25-49%", "50-74%", "75-94%", ">95%")
scale.count   <- c("0", "<3", "3-10", "11-100", "101-1,000", "1,001-10,000", ">10,000")
scale.qual    <- c("None", "Little", "Some", "Lots")   # None = the zero level

int.percent <- c("leaf.present.intensity", "leaf.increasing.intensity", "leaf.color.intensity",
                 "flower.open.intensity", "fruit.ripe.intensity")
int.count   <- c("leaf.breaking.buds.intensity", "flower.buds.intensity",
                 "fruit.present.intensity", "fruit.drop.intensity")
int.qual    <- c("flower.pollen.intensity")

positive.int <- c(setdiff(scale.percent, "0%"), setdiff(scale.count, "0"), setdiff(scale.qual, "None"))
zeroish.int  <- c("0", "0%", "None")
placeholder  <- "Select an option"


# #######################################################
# Read every *_FINAL.csv into one frame
# #######################################################
files <- dir(path.data, pattern = "FINAL\\.csv$", full.names = TRUE)
if(length(files) == 0) stop("No *_FINAL.csv files found in ", path.data)

readOne <- function(f){
  d <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE,
                colClasses = "character", na.strings = c("", "NA"))
  d$File <- basename(f)
  d$Row  <- seq_len(nrow(d))   # row number within its own file (for the flag list)
  d
}
raw <- lapply(files, readOne)

# keep only real phenology files; anything missing the columns is recorded, not crashed on
ok <- sapply(raw, function(d) all(expected.cols %in% names(d)))
skipped.files <- basename(files)[!ok]                       # <- inspect this if it's non-empty
pheno <- do.call(rbind, lapply(raw[ok], function(d) d[, c(expected.cols, "File", "Row")]))

M   <- as.matrix(pheno[, expected.cols])   # all-character matrix -> check many columns at once


# #######################################################
# Eyeball  (run each line and look at what it shows)
# #######################################################
summary(as.Date(pheno$Date.Observed, format = date.format))   # date span / unparseable dates
table(pheno$File, pheno$Collection)            # each file should be a single collection
table(pheno$Observer, useNA = "ifany")
table(unlist(pheno[obs.cols]),  useNA = "ifany")   # every 'observed' level in one table
table(unlist(pheno[int.cols]), useNA = "ifany")    # every 'intensity' level in one table


# #######################################################
# Rule checks  (each is one vectorized expression -> rows added to 'flags')
# #######################################################
# Turn a set of (row, column) hits into flag rows.
mkFlags <- function(hit, cols, issue){
  if(nrow(hit) == 0) return(NULL)
  data.frame(File = pheno$File[hit[, 1]], Row = pheno$Row[hit[, 1]],
             PlantNumber = pheno$PlantNumber[hit[, 1]], Column = cols[hit[, 2]],
             Value = M[, cols][hit], Issue = issue, stringsAsFactors = FALSE)
}
# Same idea for one-column checks where 'r' is a vector of row numbers.
mkFlags1 <- function(r, column, value, issue){
  if(length(r) == 0) return(NULL)
  data.frame(File = pheno$File[r], Row = pheno$Row[r], PlantNumber = pheno$PlantNumber[r],
             Column = column, Value = value, Issue = issue, stringsAsFactors = FALSE)
}

flags <- data.frame()

# required fields blank
hit <- which(is.na(M[, required.cols]) | trimws(M[, required.cols]) == "", arr.ind = TRUE)
flags <- rbind(flags, mkFlags(hit, required.cols, "blank in required field"))

# stray whitespace in text fields
hit <- which(!is.na(M[, text.cols]) & M[, text.cols] != trimws(M[, text.cols]), arr.ind = TRUE)
flags <- rbind(flags, mkFlags(hit, text.cols, "leading/trailing whitespace"))

# observed values not in the allowed set
hit <- which(!is.na(M[, obs.cols]) & !(M[, obs.cols] %in% observed.valid), arr.ind = TRUE)
flags <- rbind(flags, mkFlags(hit, obs.cols, "value not an allowed 'observed' code"))

# intensity placeholders
I <- M[, int.cols]
hit <- which(!is.na(I) & I == placeholder, arr.ind = TRUE)
flags <- rbind(flags, mkFlags(hit, int.cols, "'Select an option' placeholder (treat as missing)"))

# intensity values outside this phenophase's scale
validI <- matrix(FALSE, nrow(pheno), length(int.cols), dimnames = list(NULL, int.cols))
validI[, int.percent] <- I[, int.percent] %in% scale.percent
validI[, int.count]   <- I[, int.count]   %in% scale.count
validI[, int.qual]    <- I[, int.qual]    %in% scale.qual
hit <- which(!is.na(I) & I != placeholder & !validI, arr.ind = TRUE)
flags <- rbind(flags, mkFlags(hit, int.cols, "intensity not in this phenophase's scale"))

# cross-field: each intensity column vs its matching observed column
O <- M[, sub("\\.intensity$", ".observed", int.cols)]
hit <- which(!is.na(O) & O == "No" & !is.na(I) & I %in% positive.int, arr.ind = TRUE)
flags <- rbind(flags, mkFlags(hit, int.cols, "observed=No but positive intensity"))
hit <- which(!is.na(O) & O == "Yes" & (is.na(I) | I %in% zeroish.int | I == placeholder), arr.ind = TRUE)
flags <- rbind(flags, mkFlags(hit, int.cols, "observed=Yes but blank/zero intensity (review, do not backfill)"))

# species genus vs Collection
# Strip a leading hybrid marker ("x " or "x" or the multiplication sign) before
# pulling the first word, so hybrids don't get miscategorized as parse failures.
species.stripped <- sub("^\\s*[x\u00d7]\\s+", "", pheno$Species, ignore.case = TRUE)
gen <- sub("^\\s*([A-Za-z]+).*$", "\\1", species.stripped)
gen[gen == species.stripped] <- NA   # regex found no leading word -> couldn't extract a genus
r <- which(!is.na(pheno$Species) & !is.na(gen) & gen != pheno$Collection)
flags <- rbind(flags, mkFlags1(r, "Species", pheno$Species[r], "genus does not match Collection"))
# Species values we couldn't extract a genus from at all (e.g. leading punctuation,
# non-Latin characters) — these previously fell through the mismatch check unflagged.
r <- which(!is.na(pheno$Species) & is.na(gen))
flags <- rbind(flags, mkFlags1(r, "Species", pheno$Species[r], "could not parse genus from Species"))

# date inside the file's Year
d <- as.Date(pheno$Date.Observed, format = date.format)
r <- which(!is.na(d) & format(d, "%Y") != pheno$Year)
flags <- rbind(flags, mkFlags1(r, "Date.Observed", as.character(d[r]), "date outside file Year"))
# dates that didn't parse under date.format at all (previously silently excluded above)
r <- which(is.na(d) & !is.na(pheno$Date.Observed) & trimws(pheno$Date.Observed) != "")
flags <- rbind(flags, mkFlags1(r, "Date.Observed", pheno$Date.Observed[r],
                               "date did not parse under configured date.format"))

# exact duplicate rows (within a file; File is part of the key so files don't collide)
r <- which(duplicated(pheno[, c("File", expected.cols)]))
flags <- rbind(flags, mkFlags1(r, "<row>", NA, "exact duplicate row"))

flags <- flags[flags$Column != "Notes", ]   # Notes is free-text; keep it out of the flag list

table(flags$Issue)   # <- run and look: the tally of everything found


# #######################################################
# Objective clean (safe fixes only) + write one cleaned file per input file
# #######################################################
pheno[text.cols] <- lapply(pheno[text.cols], trimws)                                 # trim whitespace
pheno[int.cols]  <- lapply(pheno[int.cols], function(x){ x[x == placeholder] <- NA; x }) # drop placeholders

if(!dir.exists(path.clean)) dir.create(path.clean, recursive = TRUE)
if(!dir.exists(path.qaqc))  dir.create(path.qaqc,  recursive = TRUE)

# One cleaned file per input file (same grouping as the raw data), named to
# mirror the source file. This is intermediate output — script 2 reads these
# and does the actual collection/year archive split.
writeCleaned <- function(d, fname){
  d <- d[, expected.cols]
  outName <- sub("FINAL\\.csv$", "cleaned.csv", fname)
  write.csv(d, file.path(path.clean, outName), row.names = FALSE, na = "")
}
invisible(Map(writeCleaned, split(pheno, pheno$File), names(split(pheno, pheno$File))))

# short, Excel-safe tab names for each issue (sheet names max 31 chars, no \ / ? * : [ ])
tab.names <- c(
  "blank in required field"                                         = "Required blank",
  "leading/trailing whitespace"                                     = "Whitespace",
  "value not an allowed 'observed' code"                            = "Bad observed code",
  "'Select an option' placeholder (treat as missing)"               = "Placeholder",
  "intensity not in this phenophase's scale"                        = "Bad intensity",
  "observed=No but positive intensity"                              = "No but positive intensity",
  "observed=Yes but blank/zero intensity (review, do not backfill)" = "Yes but blank intensity",
  "genus does not match Collection"                                 = "Genus mismatch",
  "could not parse genus from Species"                              = "Genus unparseable",
  "date outside file Year"                                          = "Date wrong year",
  "date did not parse under configured date.format"                 = "Date unparseable",
  "exact duplicate row"                                             = "Duplicate row")

# one tab per issue, with an "All flags" tab first
sheets <- c(list("All flags" = flags), split(flags, flags$Issue))
nm <- names(sheets)
nm[-1] <- ifelse(nm[-1] %in% names(tab.names), tab.names[nm[-1]], substr(nm[-1], 1, 31))
names(sheets) <- nm
write.xlsx(sheets, file.path(path.qaqc, "phenology_QAQC_flags.xlsx"))

