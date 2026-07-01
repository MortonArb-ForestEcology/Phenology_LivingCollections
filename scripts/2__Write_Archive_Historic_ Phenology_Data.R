# Archive Phenology Data — split cleaned files into final Collection/Year archives
# #######################################################
# Reads the cleaned per-file CSVs produced by 1_pre_archive_qaqc.R, and writes
# one final archive file per (Collection, Year) combination that actually
# appears in the data — not per input file. This matters because splitting by
# input file (the original approach) silently mis-names output if a single
# file happens to span more than one Year: it would name the whole file after
# row 1's Year only. Splitting on the real Collection/Year values avoids that.

# ---- paths ----
path.google <- "~/Google Drive/My Drive"
path.base   <- file.path(path.google, "LivingCollections_Phenology")
path.clean  <- file.path(path.base, "Data_Cleaned")   # input: *_cleaned.csv from script 1
path.out    <- file.path(path.base, "ToArchive")      # output: final archive files


# #######################################################
# The archiving standard (kept in sync with script 1)
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


# #######################################################
# Read every *_cleaned.csv into one frame
# #######################################################
files <- dir(path.clean, pattern = "cleaned\\.csv$", full.names = TRUE)
if(length(files) == 0) stop("No *_cleaned.csv files found in ", path.clean,
                            " -- run 1_pre_archive_qaqc.R first.")

readOne <- function(f){
  d <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE,
                colClasses = "character", na.strings = c("", "NA"))
  d$File <- basename(f)
  d
}
raw <- lapply(files, readOne)

ok <- sapply(raw, function(d) all(expected.cols %in% names(d)))
skipped.files <- basename(files)[!ok]      # <- inspect this if it's non-empty
if(length(skipped.files) > 0){
  warning("Skipping cleaned files missing expected columns: ",
          paste(skipped.files, collapse = ", "))
}
pheno <- do.call(rbind, lapply(raw[ok], function(d) d[, c(expected.cols, "File")]))


# #######################################################
# Guard against rows that shouldn't be archived
# #######################################################
# Collection/Year are required fields and should already be clean by this point
# (script 1 flags blanks), but this script doesn't assume that silently — rows
# missing either are excluded from archiving and reported rather than crashing
# or getting swept into a mis-named "NA_NA" file.
missing.key <- is.na(pheno$Collection) | trimws(pheno$Collection) == "" |
  is.na(pheno$Year)       | trimws(pheno$Year)       == ""
if(any(missing.key)){
  warning(sum(missing.key), " row(s) skipped: missing Collection and/or Year. ",
          "See 'skipped' in the environment for details.")
  skipped <- pheno[missing.key, ]
  pheno   <- pheno[!missing.key, ]
}

# Duplicate rows across different source files (script 1 only catches
# duplicates within a single file) - flagged here, not silently dropped.
dup.across.files <- duplicated(pheno[, expected.cols]) & !duplicated(pheno[, c("File", expected.cols)])
if(any(dup.across.files)){
  warning(sum(dup.across.files), " row(s) appear to be duplicates across different cleaned files. ",
          "See 'cross.file.dupes' in the environment -- resolve before archiving.")
  # both the row(s) that triggered the flag and their earlier match(es)
  is.dupe.pair <- duplicated(pheno[, expected.cols]) | duplicated(pheno[, expected.cols], fromLast = TRUE)
  cross.file.dupes <- pheno[is.dupe.pair, ]
}


# #######################################################
# Split by actual Collection x Year and write one archive file per group
# #######################################################
if(!dir.exists(path.out)) dir.create(path.out, recursive = TRUE)

group.key <- interaction(pheno$Collection, pheno$Year, drop = TRUE, sep = "_")

writeArchive <- function(d){
  d <- d[, expected.cols]   # drop the bookkeeping File column
  outName <- paste0("LivingCollectionPhenology_ObservationData_",
                    d$Collection[1], "_", d$Year[1], ".csv")
  write.csv(d, file.path(path.out, outName), row.names = FALSE, na = "")
  outName
}
written <- vapply(split(pheno, group.key), writeArchive, character(1))

message(length(written), " archive file(s) written to: ", path.out)
message(paste(" -", written), sep = "\n")
