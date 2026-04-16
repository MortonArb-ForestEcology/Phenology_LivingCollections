# Living Collections Phenology — Script 2: Post-QAQC Summary & Figures
# Run AFTER Script 1 errors have been corrected in PgAdmin.
# Re-pulls current-year data fresh from DB, generates report-style figures,
# and exports cleaned per-genus CSVs for the current year.
# ============================================================

library(DBI)
library(RPostgreSQL)
library(googlesheets4)
library(ggplot2)
library(lubridate)
library(dplyr)

# 0. CONFIG ------------------------------------------------------------------
yr.now    <- lubridate::year(Sys.Date())
date.now  <- lubridate::date(Sys.Date())

path.out  <- file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations")
path.figs <- file.path("~/Google Drive/My Drive/LivingCollections_Phenology/QAQC data check/figures")

if (!dir.exists(path.figs)) dir.create(path.figs, recursive = TRUE)

genera <- c("Acer", "Quercus", "Ulmus")


# 1. PULL CORRECTED DATA FROM DB ---------------------------------------------
# Same pull as Script 1 — run this after PgAdmin fixes are complete.

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
observers <- dbReadTable(con, "Observers")
treeLists <- dbReadTable(con, "ObservingLists")
datAll    <- dbReadTable(con, "FormSubmission")
dbDisconnect(con)


# 2. PREPARE DATA FRAME ------------------------------------------------------
datAll$DateEntered  <- as.Date(datAll$DateEntered)
datAll$DateObserved <- as.Date(datAll$DateObserved)

datAll <- datAll[datAll$ObserverID != "Test", ]
datAll <- datAll[!datAll$PlantID %in% removed$PlantNumber, ]

datAll$ObserverID <- trimws(datAll$ObserverID)
datAll$PlantID    <- trimws(datAll$PlantID)
datAll$Genus      <- trimws(datAll$Genus)
datAll$Species    <- trimws(datAll$Species)

# Subset to current year only by DateEntered
datNow <- datAll[!is.na(datAll$DateEntered) &
                   datAll$DateEntered >= as.Date(paste0(yr.now, "-01-01")) &
                   lubridate::year(datAll$DateEntered) == yr.now, ]
datNow <- droplevels(datNow)

# Add time columns
datNow$yday <- lubridate::yday(datNow$DateObserved)
datNow$week <- lubridate::week(datNow$DateObserved)
datNow$Year <- yr.now
# Normalised date for overlaying years on the same x-axis
datNow$Date <- as.Date(paste0(yr.now, "-", format(datNow$DateObserved, "%m-%d")))

dat.now <- datNow  


# 3. DATA SNAPSHOT -----------------------------------------------------------
# Quick-look metrics on the corrected data

snap.sum <- data.frame(
  Metric = c("Total observations", "Unique observers", "Unique trees",
             "Unique genera", "Earliest DateObserved", "Latest DateObserved",
             "Missing DateObserved"),
  Value  = c(nrow(dat.now),
             length(unique(dat.now$ObserverID)),
             length(unique(dat.now$PlantID)),
             length(unique(dat.now$Genus)),
             as.character(min(dat.now$DateObserved, na.rm = TRUE)),
             as.character(max(dat.now$DateObserved, na.rm = TRUE)),
             sum(is.na(dat.now$DateObserved)))
)
snap.sum

genera.observed <- data.frame(
  Genus          = sort(unique(as.character(dat.now$Genus))),
  N_observations = tabulate(match(dat.now$Genus, sort(unique(dat.now$Genus))))
)
genera.observed


# 3b. PHENOPHASE SNAPSHOT BY GENUS -------------------------------------------
# Proportion of active trees on the observing list showing each phenophase,
# based on the most recent observation per tree

most.recent.idx.snap <- ave(as.numeric(dat.now$DateObserved), dat.now$PlantID, FUN = max)
pheno.snap           <- dat.now[as.numeric(dat.now$DateObserved) == most.recent.idx.snap, ]

active.trees       <- treeLists[!treeLists$PlantID %in% removed$PlantNumber, ]
active.trees$Genus <- gsub(" .*", "", active.trees$Taxon)

genus.denom           <- aggregate(PlantID ~ Genus, data = active.trees, FUN = length)
names(genus.denom)[2] <- "N_trees"

pheno.prop <- function(obs.col, label, yes.val = "y") {
  hits <- pheno.snap[!is.na(pheno.snap[[obs.col]]) & pheno.snap[[obs.col]] == yes.val, ]
  if (nrow(hits) == 0) {
    merged          <- genus.denom
    merged[[label]] <- paste0("0% (0/", merged$N_trees, ")")
    return(merged[, c("Genus", label)])
  }
  genus.hits           <- aggregate(PlantID ~ Genus, data = hits, FUN = function(x) length(unique(x)))
  names(genus.hits)[2] <- "N_yes"
  merged               <- merge(genus.denom, genus.hits, by = "Genus", all.x = TRUE)
  merged$N_yes[is.na(merged$N_yes)] <- 0
  merged[[label]] <- paste0(round(merged$N_yes / merged$N_trees * 100),
                            "% (", merged$N_yes, "/", merged$N_trees, ")")
  merged[, c("Genus", label)]
}

prop.buds        <- pheno.prop("BreakingLeafBudsObserved", "Breaking_Leaf_Buds")
prop.leaves      <- pheno.prop("LeafObserved",             "Leaves_Present")
prop.flower.buds <- pheno.prop("FlowerBudsObserved",       "Flower_Buds")
prop.flowers     <- pheno.prop("FlowerOpenObserved",       "Flowers_Open")

pheno.by.genus <- Reduce(
  function(a, b) merge(a, b, by = "Genus", all = TRUE),
  list(genus.denom, prop.buds, prop.leaves, prop.flower.buds, prop.flowers)
)
pheno.by.genus


# 5. SPRING PHENOPHASE FIGURES -----------------------------------------------
# Histograms of yday for each spring phenophase, faceted by genus

spring.phases <- list(
  list(col = "BreakingLeafBudsObserved", yes = "y", label = "Breaking Leaf Buds"),
  list(col = "LeafObserved",             yes = "y", label = "Leaves Present"),
  list(col = "FlowerBudsObserved",       yes = "y", label = "Flower Buds"),
  list(col = "FlowerOpenObserved",       yes = "y", label = "Open Flowers")
)

for (ph in spring.phases) {
  col   <- ph$col
  yes   <- ph$yes
  label <- ph$label
  
  if (!col %in% names(dat.now)) next
  
  now.yes <- dat.now[!is.na(dat.now[[col]]) & dat.now[[col]] == yes &
                       !is.na(dat.now$yday) & dat.now$yday <= 200, ]
  
  if (nrow(now.yes) == 0) next
  
  ggplot(now.yes, aes(x = yday)) +
    png(file.path(path.figs, paste0(gsub(" ", "_", label), "_", yr.now, ".png")),
        height = 5.5, width = 9, units = "in", res = 150) +
    geom_histogram(fill = "#2196F3", alpha = 0.8, binwidth = 7, boundary = 1) +
    facet_wrap(~ Genus, scales = "free_y", ncol = 1) +
    scale_x_continuous(
      breaks = c(60, 91, 121, 152, 182),
      labels = c("Mar 1", "Apr 1", "May 1", "Jun 1", "Jul 1")) +
    labs(title = paste(label, "—", yr.now, "to", date.now),
         x = "Date", y = "Number of Observations") +
    theme_bw(base_size = 13)
  dev.off()
}


# 6. FALL PHENOPHASE FIGURES -------------------------------------------------

fall.phases <- list(
  list(col = "LeafColorObserved",   yes = "y", label = "Leaf Color"),
  list(col = "LeafFallingObserved", yes = "y", label = "Leaf Falling")
)

for (ph in fall.phases) {
  col   <- ph$col
  yes   <- ph$yes
  label <- ph$label
  
  if (!col %in% names(dat.now)) next
  
  now.yes <- dat.now[!is.na(dat.now[[col]]) & dat.now[[col]] == yes &
                       !is.na(dat.now$yday) & dat.now$yday > 200, ]
  
  if (nrow(now.yes) == 0) next
  
  ggplot(now.yes, aes(x = yday)) +
    png(file.path(path.figs, paste0(gsub(" ", "_", label), "_", yr.now, ".png")),
        height = 5.5, width = 9, units = "in", res = 150) +
    geom_histogram(fill = "#E65100", alpha = 0.8, binwidth = 7, boundary = 200) +
    facet_wrap(~ Genus, scales = "free_y", ncol = 1) +
    scale_x_continuous(
      breaks = c(213, 244, 274, 305, 335),
      labels = c("Aug 1", "Sep 1", "Oct 1", "Nov 1", "Dec 1")) +
    labs(title = paste(label, "—", yr.now, "to", date.now),
         x = "Date", y = "Number of Observations") +
    theme_bw(base_size = 13)
  dev.off()
}


# 7. PHENOPHASE PROPORTION OVER TIME (CURRENT YEAR ONLY) --------------------
# Weekly proportion of trees showing each phenophase, by genus

prop.phases <- c("BreakingLeafBudsObserved", "LeafObserved",
                 "LeafColorObserved", "LeafFallingObserved",
                 "FlowerOpenObserved", "FruitPresentObserved")
prop.phases <- prop.phases[prop.phases %in% names(dat.now)]

prop.labels <- c(
  BreakingLeafBudsObserved = "Breaking Leaf Buds",
  LeafObserved             = "Leaves Present",
  LeafColorObserved        = "Leaf Color",
  LeafFallingObserved      = "Leaf Falling",
  FlowerOpenObserved       = "Open Flowers",
  FruitPresentObserved     = "Fruit Present"
)

prop.list <- lapply(genera, function(g) {
  dg <- dat.now[dat.now$Genus == g & !is.na(dat.now$week), ]
  if (nrow(dg) == 0) return(NULL)
  
  week.rows <- lapply(prop.phases, function(ph) {
    if (!ph %in% names(dg)) return(NULL)
    agg <- aggregate(dg[[ph]], by = list(week = dg$week), FUN = function(x) {
      n.yes  <- sum(x == "y", na.rm = TRUE)
      n.tot  <- sum(!is.na(x))
      if (n.tot == 0) return(NA)
      n.yes / n.tot
    })
    names(agg)[2] <- "Proportion"
    agg$Phenophase <- prop.labels[ph]
    agg$Genus      <- g
    agg
  })
  dplyr::bind_rows(week.rows)
})

prop.dat <- dplyr::bind_rows(prop.list)

if (nrow(prop.dat) > 0 && !all(is.na(prop.dat$Proportion))) {
  ggplot(prop.dat[!is.na(prop.dat$Proportion), ],
         aes(x = week, y = Proportion, color = Phenophase, group = Phenophase)) +
    png(file.path(path.figs, paste0("phenophase_proportions_", yr.now, ".png")),
        height = 10, width = 9, units = "in", res = 150) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    facet_wrap(~ Genus, ncol = 1, scales = "free_y") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_color_brewer(palette = "Set1") +
    labs(title = paste("Phenophase Proportions by Week —", yr.now, "to", date.now),
         x = "Week of Year", y = "Proportion of Trees", color = NULL) +
    theme_bw(base_size = 13) +
    theme(legend.position = "bottom")
  dev.off()
}


# 8. LEAF COLOR INTENSITY (FALL) ---------------------------------------------
# Stacked bar of LeafColorIntensity categories by week, per genus

int.col    <- "LeafColorIntensity"
int.levels <- c("<5%", "5-24%", "25-49%", "50-74%", "75-94%", ">95%")

if (int.col %in% names(dat.now)) {
  dat.color <- dat.now[!is.na(dat.now[[int.col]]) &
                         dat.now[[int.col]] != "" &
                         dat.now$yday > 200, ]
  
  if (nrow(dat.color) > 0) {
    dat.color[[int.col]] <- factor(dat.color[[int.col]],
                                   levels = int.levels[int.levels %in% dat.color[[int.col]]])
    
    ggplot(dat.color, aes(x = factor(week), fill = .data[[int.col]])) +
      png(file.path(path.figs, paste0("leaf_color_intensity_", yr.now, ".png")),
          height = 5.5, width = 9, units = "in", res = 150) +
      geom_bar(position = "fill") +
      facet_wrap(~ Genus, ncol = 1) +
      scale_fill_brewer(palette = "YlOrRd", name = "Color Intensity") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = paste("Leaf Color Intensity by Week —", yr.now),
           x = "Week of Year", y = "Proportion") +
      theme_bw(base_size = 13) +
      theme(legend.position = "right")
    dev.off()
  }
}


# 9. EXPORT CORRECTED DATA BY GENUS ------------------------------------------
# Writes one CSV per genus from the post-fix DB pull, plus a combined file.
# These are the authoritative current-year cleaned data files.

for (g in genera) {
  dat.g <- dat.now[dat.now$Genus == g, ]
  if (nrow(dat.g) == 0) {
    warning("No records found for genus: ", g)
    next
  }
  out.name <- paste0("LivingCollectionPhenology_ObservationData_",
                     g, "_", yr.now, "_asof_", date.now, ".csv")
  write.csv(dat.g, file.path(path.out, out.name), row.names = FALSE)
  message("Exported: ", out.name, " (", nrow(dat.g), " records)")
}

# Combined all-genera export
out.all <- paste0("LivingCollectionPhenology_ObservationData_All_",
                  yr.now, "_asof_", date.now, ".csv")
write.csv(dat.now, file.path(path.out, out.all), row.names = FALSE)
