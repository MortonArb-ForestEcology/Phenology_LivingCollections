# -------------------------------------------------------------
# Checking weekly phenology observation data
# -------------------------------------------------------------


# -------------------------------------------------------------
# Set file paths, load libraries etc.
# -------------------------------------------------------------
library(googlesheets)
library(raster); library(rgdal); library(rgeos) # spatial analysis packages
library(ggplot2); library(grid) # graphing packages
library(lubridate)

# Source my cleaning function
source("clean_google_form.R")

dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"
path.figs <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/Reports/2019_01_MidYear_Report/figures_spring_2019"
if(!dir.exists(path.figs)) dir.create(path.figs, recursive=T)
# setwd(dir.base)

path.dat <- file.path(dir.base, "Observing Lists/2018_Quercus")
maps.out <- file.path(path.dat)
# path.gis <- "/Volumes/GIS/Collections" # Path on a Mac
# path.gis <- "Y:/Collections" # Path on a PC
# -------------------------------------------------------------


# -------------------------------------------------------------
# Access & format the observations
# -------------------------------------------------------------
# get the data from a particular sheet
quercus <- clean.google(pheno.title = "Phenology_Observations_GoogleForm", collection="Quercus", dat.yr=lubridate::year(Sys.Date()))
quercus$Collection <- "Quercus"
summary(quercus)

acer <- clean.google(pheno.title = "Phenology_Observations_GoogleForm", collection="Acer", dat.yr=lubridate::year(Sys.Date()))
acer$Collection <- "Acer"
summary(acer)

dat.all <- rbind(quercus, acer)
dat.all$fruit.drop.intensity <- as.factor(dat.all$fruit.drop.intensity)
summary(dat.all)

phenophase.obs <- names(dat.all)[grep(".observed", names(dat.all))] 
for(PHENO in phenophase.obs){
  dat.all[is.na(dat.all[,PHENO]),PHENO] <- "Did not look for"
  dat.all[,PHENO] <- factor(dat.all[,PHENO], levels=c("No", "Yes", "?", "Did not look for"))
}
summary(dat.all)

dat.all$Observer <- factor(dat.all$Observer, levels=c(sort(paste(unique(dat.all$Observer))[!unique(dat.all$Observer) %in% c("Rollinson", "Reidy")]), "Rollinson", "Reidy"))
#----------------------------


#----------------------------
# Doing some initial graphs and summaries at the collection-level: 
#  -- simple histograms based on collections & species
#----------------------------
# getting some some date of first events
first.event <- aggregate(dat.all[dat.all$leaf.present.observed=="Yes", "Date.Observed"],
                             by=dat.all[dat.all$leaf.present.observed=="Yes",c("Collection", "Species", "PlantNumber")],
                             FUN=min, na.rm=T)
names(first.event)[names(first.event)=="x"] <- "leaf.present.observed"

for(PHENO in phenophase.obs){
  if(nrow(dat.all[dat.all[,PHENO]=="Yes",])==0) next
  
  # Need to store it as a temporary data frame because some trees won't have particular phenophases
  dat.tmp <- aggregate(dat.all[dat.all[,PHENO]=="Yes", "Date.Observed"],
                       by=dat.all[dat.all[,PHENO]=="Yes",c("Collection", "Species", "PlantNumber")],
                       FUN=min, na.rm=T)
  names(dat.tmp)[names(dat.tmp)=="x"] <- PHENO
  
  first.event <- merge(first.event, dat.tmp, all=T)
  
}
summary(first.event)

# ------------
# Budburst & Flowering
# ------------
# Finding mean day of Bud Burst
mean(first.event[first.event$Collection=="Acer", "leaf.buds.observed"], na.rm=T); sd(first.event[first.event$Collection=="Acer", "leaf.buds.observed"], na.rm=T)
mean(first.event[first.event$Collection=="Quercus", "leaf.buds.observed"], na.rm=T); sd(first.event[first.event$Collection=="Quercus", "leaf.buds.observed"], na.rm=T)

# First/last bud burst
first.event[!is.na(first.event$leaf.buds.observed) & first.event$leaf.buds.observed==min(first.event$leaf.buds.observed, na.rm=T),];  
first.event[!is.na(first.event$leaf.buds.observed) & first.event$leaf.buds.observed==max(first.event$leaf.buds.observed, na.rm=T),];  

# First/last flower
first.event[!is.na(first.event$flower.open.observed) & first.event$flower.open.observed==min(first.event$flower.open.observed, na.rm=T),];  
first.event[!is.na(first.event$flower.open.observed) & first.event$flower.open.observed==max(first.event$flower.open.observed, na.rm=T),];  
# ------------

# ------------
# Leaf Out
# ------------
# Difference between bud burst & leaf out
leaf.delay <- as.numeric(first.event$leaf.present.observed - first.event$leaf.buds.observed)
mean(leaf.delay[leaf.delay>=0], na.rm=T); sd(leaf.delay[leaf.delay>=0], na.rm=T)

# Finding mean day of Bud Burst
# Note: during this analysis, discovered c("296-2013*5") hadn't really been observed until Brendon subbed in late-June, so it was showing weird data
mean(first.event[first.event$Collection=="Acer" & !first.event$PlantNumber %in% c("296-2013*5"), "leaf.present.observed"], na.rm=T); sd(first.event[first.event$Collection=="Acer" & !first.event$PlantNumber %in% c("296-2013*5"), "leaf.present.observed"], na.rm=T)
mean(first.event[first.event$Collection=="Quercus", "leaf.present.observed"], na.rm=T); sd(first.event[first.event$Collection=="Quercus", "leaf.present.observed"], na.rm=T)


# First/last leaf
first.event[!is.na(first.event$leaf.present.observed) & first.event$leaf.present.observed==min(first.event$leaf.present.observed, na.rm=T),];  
first.event[!is.na(first.event$leaf.present.observed) & first.event$leaf.present.observed==max(first.event$leaf.present.observed[!first.event$PlantNumber %in% c("296-2013*5")], na.rm=T) & !first.event$PlantNumber %in% c("296-2013*5"),];  
# ------------


png(file.path(path.figs, "Histogram_Collection_LeafPresent.png"), height=6, width=8, units="in", res=120)
ggplot(data=dat.all) +
  facet_grid(Collection~., scales="free_y") +
  geom_histogram(aes(x=Date.Observed, fill=leaf.present.observed), binwidth=7) +
  scale_x_date(name="Date", expand=c(0,0)) +
  scale_y_continuous("Observation Count",expand=c(0,0)) +
  scale_fill_manual("Leaf Present", values=c("gray50", "green4", "blue2", "black") ) +
  theme(legend.position="top",
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))
dev.off()

png(file.path(path.figs, "Histogram_Collection_LeafBudBreaking.png"), height=6, width=8, units="in", res=120)
ggplot(data=dat.all) +
  facet_grid(Collection~., scales="free_y") +
  geom_histogram(aes(x=Date.Observed, fill=leaf.buds.observed), binwidth=7) +
  scale_x_date(name="Date", expand=c(0,0)) +
  scale_y_continuous("Observation Count",expand=c(0,0)) +
  scale_fill_manual("Breaking Leaf Bud", values=c("gray50", "green4", "blue2", "black") ) +
  theme(legend.position="top",
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))
dev.off()

for(COLLECTION in unique(dat.all$Collection) ){
  png(file.path(path.figs, paste0("Histogram_", COLLECTION, "_LeafPresent.png")), height=10, width=8, units="in", res=120)
  print(
  ggplot(data=dat.all[dat.all$Collection==COLLECTION,]) +
    facet_grid(Species~., scales="free_y") +
    geom_histogram(aes(x=Date.Observed, fill=leaf.present.observed), binwidth=7) +
    scale_x_date(name="Date", limits = range(dat.all$Date.Observed), expand=c(0,0)) +
    scale_y_continuous("Observation Count",expand=c(0,0)) +
    scale_fill_manual("Leaf Present", values=c("gray50", "green4", "blue2", "black") ) +
    theme(legend.position="top",
          panel.grid = element_blank(),
          panel.background=element_rect(fill=NA, color="black"),
          panel.spacing=unit(0, "lines"),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          strip.text.y = element_text(angle=0))
  )
  dev.off()
  
  png(file.path(path.figs, paste0("Histogram_", COLLECTION, "_LeafBudBreaking.png")), height=10, width=8, units="in", res=120)
  print(
  ggplot(data=dat.all[dat.all$Collection==COLLECTION,]) +
    facet_grid(Species~., scales="free_y") +
    geom_histogram(aes(x=Date.Observed, fill=leaf.buds.observed), binwidth=7) +
    scale_x_date(name="Date", limits = range(dat.all$Date.Observed), expand=c(0,0)) +
    scale_y_continuous("Observation Count",expand=c(0,0)) +
    scale_fill_manual("Breaking Leaf Bud", values=c("gray50", "green4", "blue2", "black") ) +
    theme(legend.position="top",
          panel.grid = element_blank(),
          panel.background=element_rect(fill=NA, color="black"),
          panel.spacing=unit(0, "lines"),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          strip.text.y = element_text(angle=0))
  )
  dev.off()
  
}

pdf(file.path(path.figs, "Observations_LeafPresent_by_Observer_by_Tree.pdf"), width=8.5, height=11)
for(OBS in levels(dat.all$Observer)){
  print(
    ggplot(data=dat.all[dat.all$Observer==OBS ,]) +
      ggtitle(paste("Observer:", OBS)) +
      facet_grid(Species~., scales="free_y") +
      geom_bin2d(aes(x=Date.Observed, y=PlantNumber, fill=leaf.present.observed), binwidth=7) +
      scale_fill_manual("Leaf Present", values=c("gray50", "green4", "blue2", "black") ) +
      scale_x_date(name="Date", limits = range(dat.all$Date.Observed), expand=c(0,0)) +
      scale_y_discrete(expand=c(0,0)) +
      scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +
      theme(legend.position="bottom",
            legend.text = element_text(size=rel(1.5)),
            legend.title = element_text(size=rel(1.5)),
            plot.title = element_text(size=rel(3), face="bold", hjust=0.5),
            panel.grid = element_blank(),
            panel.background=element_rect(fill=NA, color="black"),
            panel.spacing=unit(0, "lines"),
            axis.text.x=element_text(size=rel(1.5)),
            axis.title.x=element_text(size=rel(2), face="bold"),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            strip.text.y=element_text(size=rel(1.5), angle=0))
  )
}
dev.off()

pdf(file.path(path.figs, "Observations_LeafBudBreaking_by_Observer_by_Tree.pdf"), width=8.5, height=11)
for(OBS in levels(dat.all$Observer)){
  print(
    ggplot(data=dat.all[dat.all$Observer==OBS ,]) +
      ggtitle(paste("Observer:", OBS)) +
      facet_grid(Species~., scales="free_y") +
      geom_bin2d(aes(x=Date.Observed, y=PlantNumber, fill=leaf.buds.observed), binwidth=7) +
      scale_fill_manual("Leaf Bud Breaking", values=c("gray50", "green4", "blue2", "black") ) +
      scale_x_date(name="Date", limits = range(dat.all$Date.Observed), expand=c(0,0)) +
      scale_y_discrete(expand=c(0,0)) +
      scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +
      theme(legend.position="bottom",
            legend.text = element_text(size=rel(1.5)),
            legend.title = element_text(size=rel(1.5)),
            plot.title = element_text(size=rel(3), face="bold", hjust=0.5),
            panel.grid = element_blank(),
            panel.background=element_rect(fill=NA, color="black"),
            panel.spacing=unit(0, "lines"),
            axis.text.x=element_text(size=rel(1.5)),
            axis.title.x=element_text(size=rel(2), face="bold"),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            strip.text.y=element_text(size=rel(1.5), angle=0))
  )
}
dev.off()

#----------------------------

#----------------------------
# Aggregating the data by species so we can show what's going on at the arb
#----------------------------
# Aggregating by each observer to get the fraction in each phenophase at each dates
obs.n.date.spp <- aggregate(dat.all[,c("PlantNumber")],
                            by=dat.all[,c("Date.Observed", "Collection", "Species")],
                            FUN=length)
names(obs.n.date.spp)[names(obs.n.date.spp)=="x"] <- "Trees.List"
dat.agg.spp <- aggregate(dat.all[,"PlantNumber"], 
                         by=dat.all[,c("Date.Observed", "Collection", "Species", phenophase.obs)],
                         FUN=length)
names(dat.agg.spp)[names(dat.agg.spp)=="x"] <- "Trees.Obs"
dat.agg.spp <- merge(dat.agg.spp, obs.n.date, all.x=T)
dat.agg.spp$prop.spp <- dat.agg.spp$Trees.Obs/dat.agg.spp$Trees.List

# summary(dat.all[dat.all$Observer=="Dock_Marsha",c("Date.Observed", "Observer", "Collection", "Species", phenophase.spp)])
# unique(dat.all[dat.all$Observer=="Dock_Marsha","PlantNumber"])

# summary(dat.agg.spp[dat.agg.spp$Observer=="Dock_Marsha",])

png(file.path(path.figs, "Observations_All_LeafPresent_by_Species_01_day.png"), width=8.5, height=11, unit="in", res=120)
print(
  ggplot(data=dat.agg.spp[,]) +
    ggtitle("Leaf Present") +
    facet_grid(Collection~., scales="free") +
    geom_bin2d(aes(x=Date.Observed, y=Species, fill=leaf.present.observed, alpha=prop.spp), binwidth=1) +
    scale_fill_manual("Leaf Present", values=c("gray50", "green4", "blue2", "black") ) +
    scale_x_date(name="Date", limits = range(dat.all$Date.Observed), expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +
    theme(legend.position="bottom",
          # legend.text = element_text(size=rel(1.5)),
          # legend.title = element_text(size=rel(1.5)),
          plot.title = element_text(size=rel(3), face="bold", hjust=0.5),
          panel.grid = element_blank(),
          panel.background=element_rect(fill=NA, color="black"),
          # axis.text=element_text(size=rel(1.5)),
          axis.title.x=element_text(size=rel(2), face="bold"),
          axis.title.y=element_blank(),
          strip.text=element_text(size=rel(2), face="bold"))
)
dev.off()

png(file.path(path.figs, "Observations_All_LeafPresent_by_Species_07_week.png"), width=8.5, height=11, unit="in", res=120)
print(
  ggplot(data=dat.agg.spp[,]) +
    ggtitle("Leaf Present") +
    facet_grid(Collection~., scales="free") +
    geom_bin2d(aes(x=Date.Observed, y=Species, fill=leaf.present.observed, alpha=prop.spp), binwidth=7) +
    scale_fill_manual("Leaf Present", values=c("gray50", "green4", "blue2", "black") ) +
    scale_x_date(name="Date", limits = range(dat.all$Date.Observed), expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +
    guides(alpha=F) +
    theme(legend.position="bottom",
          # legend.text = element_text(size=rel(1.5)),
          # legend.title = element_text(size=rel(1.5)),
          plot.title = element_text(size=rel(3), face="bold", hjust=0.5),
          panel.grid = element_blank(),
          panel.background=element_rect(fill=NA, color="black"),
          # axis.text=element_text(size=rel(1.5)),
          axis.title.x=element_text(size=rel(2), face="bold"),
          axis.title.y=element_blank(),
          strip.text=element_text(size=rel(2), face="bold"))
)
dev.off()

png(file.path(path.figs, "Observations_All_LeafPresent_by_Species_10_day.png"), width=8.5, height=11, unit="in", res=120)
print(
  ggplot(data=dat.agg.spp[,]) +
    ggtitle("Leaf Present") +
    facet_grid(Collection~., scales="free") +
    geom_bin2d(aes(x=Date.Observed, y=Species, fill=leaf.present.observed, alpha=prop.spp), binwidth=10) +
    scale_fill_manual("Leaf Present", values=c("gray50", "green4", "blue2", "black") ) +
    scale_x_date(name="Date", limits = range(dat.all$Date.Observed), expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +
    guides(alpha=F) +
    theme(legend.position="bottom",
          # legend.text = element_text(size=rel(1.5)),
          # legend.title = element_text(size=rel(1.5)),
          plot.title = element_text(size=rel(3), face="bold", hjust=0.5),
          panel.grid = element_blank(),
          panel.background=element_rect(fill=NA, color="black"),
          # axis.text=element_text(size=rel(1.5)),
          axis.title.x=element_text(size=rel(2), face="bold"),
          axis.title.y=element_blank(),
          strip.text=element_text(size=rel(2), face="bold"))
)
dev.off()


png(file.path(path.figs, "Observations_All_LeafBudBreaking_by_Species_01_day.png"), width=8.5, height=11, unit="in", res=120)
print(
  ggplot(data=dat.agg.spp[,]) +
    ggtitle("Leaf Bud Breaking") +
    facet_grid(Collection~., scales="free") +
    geom_bin2d(aes(x=Date.Observed, y=Species, fill=leaf.buds.observed, alpha=prop.spp), binwidth=1) +
    scale_fill_manual("Leaf Present", values=c("gray50", "green4", "blue2", "black") ) +
    scale_x_date(name="Date", limits = range(dat.all$Date.Observed), expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +
    guides(alpha=F) +
    theme(legend.position="bottom",
          # legend.text = element_text(size=rel(1.5)),
          # legend.title = element_text(size=rel(1.5)),
          plot.title = element_text(size=rel(3), face="bold", hjust=0.5),
          panel.grid = element_blank(),
          panel.background=element_rect(fill=NA, color="black"),
          # axis.text=element_text(size=rel(1.5)),
          axis.title.x=element_text(size=rel(2), face="bold"),
          axis.title.y=element_blank(),
          strip.text=element_text(size=rel(2), face="bold"))
)
dev.off()


png(file.path(path.figs, "Observations_All_LeafBudBreaking_by_Species_07_week.png"), width=8.5, height=11, unit="in", res=120)
print(
  ggplot(data=dat.agg.spp[,]) +
    ggtitle("Leaf Bud Breaking") +
    facet_grid(Collection~., scales="free") +
    geom_bin2d(aes(x=Date.Observed, y=Species, fill=leaf.buds.observed, alpha=prop.spp), binwidth=7) +
    scale_fill_manual("Leaf Present", values=c("gray50", "green4", "blue2", "black") ) +
    scale_x_date(name="Date", limits = range(dat.all$Date.Observed), expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +
    guides(alpha=F) +
    theme(legend.position="bottom",
          # legend.text = element_text(size=rel(1.5)),
          # legend.title = element_text(size=rel(1.5)),
          plot.title = element_text(size=rel(3), face="bold", hjust=0.5),
          panel.grid = element_blank(),
          panel.background=element_rect(fill=NA, color="black"),
          # axis.text=element_text(size=rel(1.5)),
          axis.title.x=element_text(size=rel(2), face="bold"),
          axis.title.y=element_blank(),
          strip.text=element_text(size=rel(2), face="bold"))
)
dev.off()

png(file.path(path.figs, "Observations_All_LeafBudBreaking_by_Species_10_day.png"), width=8.5, height=11, unit="in", res=120)
print(
  ggplot(data=dat.agg.spp[,]) +
    ggtitle("Leaf Bud Breaking") +
    facet_grid(Collection~., scales="free") +
    geom_bin2d(aes(x=Date.Observed, y=Species, fill=leaf.buds.observed, alpha=prop.spp), binwidth=10) +
    scale_fill_manual("Leaf Present", values=c("gray50", "green4", "blue2", "black") ) +
    scale_x_date(name="Date", limits = range(dat.all$Date.Observed), expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +
    guides(alpha=F) +
    theme(legend.position="bottom",
          # legend.text = element_text(size=rel(1.5)),
          # legend.title = element_text(size=rel(1.5)),
          plot.title = element_text(size=rel(3), face="bold", hjust=0.5),
          panel.grid = element_blank(),
          panel.background=element_rect(fill=NA, color="black"),
          # axis.text=element_text(size=rel(1.5)),
          axis.title.x=element_text(size=rel(2), face="bold"),
          axis.title.y=element_blank(),
          strip.text=element_text(size=rel(2), face="bold"))
)
dev.off()


for(COLLECTION in unique(dat.agg.spp$Collection)){
  png(file.path(path.figs, paste0("Observations_", COLLECTION, "_LeafPresent_by_Species_07_week.png")), width=8.5, height=11, unit="in", res=120)
  print(
    ggplot(data=dat.agg.spp[dat.agg.spp$Collection==COLLECTION,]) +
      ggtitle("Leaf Present") +
      # facet_grid(Collection~., scales="free") +
      geom_bin2d(aes(x=Date.Observed, y=Species, fill=leaf.present.observed, alpha=prop.spp), binwidth=7) +
      scale_fill_manual("Leaf Present", values=c("gray50", "green4", "blue2", "black") ) +
      scale_x_date(name="Date", limits = range(dat.all$Date.Observed), expand=c(0,0)) +
      scale_y_discrete(expand=c(0,0)) +
      scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +
      guides(alpha=F) +
      theme(legend.position="bottom",
            legend.text = element_text(size=rel(1.5)),
            legend.title = element_text(size=rel(1.5)),
            plot.title = element_text(size=rel(3), face="bold", hjust=0.5),
            panel.grid = element_blank(),
            panel.background=element_rect(fill=NA, color="black"),
            axis.text=element_text(size=rel(1.5)),
            axis.title.x=element_text(size=rel(2), face="bold"),
            axis.title.y=element_blank(),
            strip.text=element_text(size=rel(2), face="bold"))
  )
  dev.off()
  
  png(file.path(path.figs, paste0("Observations_", COLLECTION, "_LeafBudBreaking_by_Species_07_week.png")), width=8.5, height=11, unit="in", res=120)
  print(
    ggplot(data=dat.agg.spp[dat.agg.spp$Collection==COLLECTION,]) +
      ggtitle("Leaf Bud Breaking") +
      # facet_grid(Collection~., scales="free") +
      geom_bin2d(aes(x=Date.Observed, y=Species, fill=leaf.buds.observed, alpha=prop.spp), binwidth=7) +
      # stat_bin2d(aes(x=Date.Observed, y=Species, fill=leaf.buds.observed, alpha=stat("density")), binwidth=7) +
      scale_fill_manual("Leaf Present", values=c("gray50", "green4", "blue2", "black") ) +
      scale_x_date(name="Date", limits = range(dat.all$Date.Observed), expand=c(0,0)) +
      scale_y_discrete(expand=c(0,0)) +
      # scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +
      guides(alpha=F) +
      theme(legend.position="bottom",
            legend.text = element_text(size=rel(1.5)),
            legend.title = element_text(size=rel(1.5)),
            plot.title = element_text(size=rel(3), face="bold", hjust=0.5),
            panel.grid = element_blank(),
            panel.background=element_rect(fill=NA, color="black"),
            axis.text=element_text(size=rel(1.5)),
            axis.title.x=element_text(size=rel(2), face="bold"),
            axis.title.y=element_blank(),
            strip.text=element_text(size=rel(2), face="bold"))
  )
  dev.off()
  
}
#----------------------------


#----------------------------
# Aggregating the data by observer so we can show each person what they're doing
#----------------------------
# Aggregating by each observer to get the fraction in each phenophase at each dates
obs.n.date <- aggregate(dat.all[,c("PlantNumber")],
                        by=dat.all[,c("Date.Observed", "Observer", "Collection", "Species")],
                        FUN=length)
names(obs.n.date)[names(obs.n.date)=="x"] <- "Trees.List"
dat.agg.obs <- aggregate(dat.all[,"PlantNumber"], 
                         by=dat.all[,c("Date.Observed", "Observer", "Collection", "Species", phenophase.obs)],
                         FUN=length)
names(dat.agg.obs)[names(dat.agg.obs)=="x"] <- "Trees.Obs"
dat.agg.obs <- merge(dat.agg.obs, obs.n.date, all.x=T)
dat.agg.obs$prop.obs <- dat.agg.obs$Trees.Obs/dat.agg.obs$Trees.List

# summary(dat.all[dat.all$Observer=="Dock_Marsha",c("Date.Observed", "Observer", "Collection", "Species", phenophase.obs)])
# unique(dat.all[dat.all$Observer=="Dock_Marsha","PlantNumber"])

# summary(dat.agg.obs[dat.agg.obs$Observer=="Dock_Marsha",])

pdf(file.path(path.figs, "Observations_LeafPresent_by_Observer_by_Species.pdf"), width=8.5, height=11)
for(OBS in levels(dat.agg.obs$Observer)){
  print(
  ggplot(data=dat.agg.obs[dat.agg.obs$Observer==OBS ,]) +
    ggtitle(paste("Observer:", OBS)) +
    facet_grid(Collection~., scales="free") +
    geom_bin2d(aes(x=Date.Observed, y=Species, fill=leaf.present.observed, alpha=prop.obs), binwidth=7) +
    scale_fill_manual("Leaf Present", values=c("gray50", "green4", "blue2", "black") ) +
    scale_x_date(name="Date", limits = range(dat.all$Date.Observed), expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +
    theme(legend.position="bottom",
          legend.text = element_text(size=rel(1.5)),
          legend.title = element_text(size=rel(1.5)),
          plot.title = element_text(size=rel(3), face="bold", hjust=0.5),
          panel.grid = element_blank(),
          panel.background=element_rect(fill=NA, color="black"),
          axis.text=element_text(size=rel(1.5)),
          axis.title.x=element_text(size=rel(2), face="bold"),
          axis.title.y=element_blank(),
          strip.text=element_text(size=rel(2), face="bold"))
  )
}
dev.off()

pdf(file.path(path.figs, "Observations_LeafBudBreaking_by_Observer_by_Species.pdf"), width=8.5, height=11)
for(OBS in levels(dat.agg.obs$Observer)){
  print(
    ggplot(data=dat.agg.obs[dat.agg.obs$Observer==OBS ,]) +
      ggtitle(paste("Observer:", OBS)) +
      facet_grid(Collection~., scales="free") +
      geom_bin2d(aes(x=Date.Observed, y=Species, fill=leaf.buds.observed, alpha=prop.obs), binwidth=7) +
      scale_fill_manual("Leaf Bud Breaking", values=c("gray50", "green4", "blue2", "black") ) +
      scale_x_date(name="Date", limits = range(dat.all$Date.Observed), expand=c(0,0)) +
      scale_y_discrete(expand=c(0,0)) +
      scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +
      theme(legend.position="bottom",
            legend.text = element_text(size=rel(1.5)),
            legend.title = element_text(size=rel(1.5)),
            plot.title = element_text(size=rel(3), face="bold", hjust=0.5),
            panel.grid = element_blank(),
            panel.background=element_rect(fill=NA, color="black"),
            axis.text=element_text(size=rel(1.5)),
            axis.title.x=element_text(size=rel(2), face="bold"),
            axis.title.y=element_blank(),
            strip.text=element_text(size=rel(2), face="bold"))
  )
}
dev.off()
#----------------------------


#----------------------------
# Comparing Quercus in 2018 & 2019
#----------------------------
quercus.2018 <- clean.google(pheno.title = "Phenology_Observations_GoogleForm", collection="Quercus", dat.yr=2018)
quercus.2018$Collection <- "Quercus"
# quercus.2018
names(quercus.2018) <- names(quercus)
summary(quercus.2018)

quercus.all <- rbind(quercus.2018, quercus)
quercus.all$Year <- lubridate::year(quercus.all$Date.Observed)
summary(quercus.all)


# getting some some date of first events
first.event.quer <- aggregate(quercus.all[quercus.all$leaf.present.observed=="Yes", "Date.Observed"],
                              by=quercus.all[quercus.all$leaf.present.observed=="Yes",c("Year", "Species", "PlantNumber")],
                         FUN=min, na.rm=T)
names(first.event.quer)[names(first.event.quer)=="x"] <- "leaf.present.observed"
summary(first.event.quer)
for(PHENO in phenophase.obs){
  if(nrow(quercus.all[quercus.all[,PHENO]=="Yes",])==0) next
  
  # Need to store it as a temporary data frame because some trees won't have particular phenophases
  dat.tmp <- aggregate(quercus.all[quercus.all[,PHENO]=="Yes", "Date.Observed"],
                       by=quercus.all[quercus.all[,PHENO]=="Yes",c("Year", "Species", "PlantNumber")],
                       FUN=min, na.rm=T)
  names(dat.tmp)[names(dat.tmp)=="x"] <- PHENO
  
  first.event.quer <- merge(first.event.quer, dat.tmp, all=T)
  
}
summary(first.event.quer)

quer.spp <- aggregate(first.event.quer[,c("leaf.present.observed", "leaf.buds.observed", "flower.buds.observed", "flower.open.observed")],
                      by=first.event.quer[,c("Year", "Species")],
                      FUN=mean, na.rm=T)
summary(quer.spp)

quer.spp$leaf.budburst.first <- lubridate::yday(quer.spp$leaf.buds.observed)
quer.spp$leaf.emerge.first <- lubridate::yday(quer.spp$leaf.present.observed)
quer.spp$flower.budburst.first <- lubridate::yday(quer.spp$flower.buds.observed)
quer.spp$flower.open.first <- lubridate::yday(quer.spp$flower.open.observed)
summary(quer.spp)

ggplot(quer.spp) +
  facet_grid(Year~.) +
  geom_histogram(aes(x=leaf.budburst.first))

ggplot(quer.spp) +
  facet_grid(Year~.) +
  geom_histogram(aes(x=leaf.emerge.first))

mean(quer.spp[quer.spp$Year==2018,"leaf.present.observed"]); sd(quer.spp[quer.spp$Year==2018,"leaf.present.observed"])
mean(quer.spp[quer.spp$Year==2019,"leaf.present.observed"]); sd(quer.spp[quer.spp$Year==2019,"leaf.present.observed"])
summary(quer.spp[quer.spp$Year==2018,])
summary(quer.spp[quer.spp$Year==2019,])

for(SPP in unique(quer.spp$Species)){
  if(length(which(quer.spp$Species==SPP))<2) next
  ind.19 <- which(quer.spp$Species==SPP & quer.spp$Year==2019)
  
  dat.18 <- quer.spp[quer.spp$Species==SPP & quer.spp$Year==2018,]
  dat.19 <- quer.spp[ind.19,]

  quer.spp[ind.19,"d.leaf.budburst"] <- dat.18$leaf.budburst.first - dat.19$leaf.budburst.first
  quer.spp[ind.19,"d.leaf.emerge"] <- dat.18$leaf.emerge.first - dat.19$leaf.emerge.first
  quer.spp[ind.19,"d.flower.budburst"] <- dat.18$flower.budburst.first - dat.19$flower.budburst.first
  quer.spp[ind.19,"d.flower.open"] <- dat.18$flower.open.first - dat.19$flower.open.first
  
}
summary(quer.spp)

quer.stack <- stack(quer.spp[quer.spp$Year==2019,c("d.leaf.budburst", "d.leaf.emerge", "d.flower.budburst", "d.flower.open")])
quer.stack$Species <- quer.spp$Species[quer.spp$Year==2019]
summary(quer.stack)

ggplot(data=quer.stack) +
  facet_wrap(~ind, scales="free") +
  geom_histogram(aes(x=values)) +
  geom_vline(xintercept=0, linetype="dashed", color="red") +
  scale_x_continuous(name="Difference in Days (2018-2019)") +
  scale_y_continuous(expand=c(0,0)) +
  theme_bw()

write.csv(quer.spp, "../data/observations/Quercus_Summary_2018-2019_2019-07-02.csv", row.names=F)
#----------------------------
