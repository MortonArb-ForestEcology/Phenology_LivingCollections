# Setting up a script to pull some numbers on weather for the fall color report
library(ggplot2)
source("../../Phenology_Forecasting/scripts/met_download_GHCN.R")
source("../../Phenology_Forecasting/scripts/met_gapfill.R")

IDnew ="USC00115097" # 2007 - present
IDold ="USC00119221" # up to 2007
vars.want <- c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD")
dir.raw="../data_raw/meteorology/GHCN_raw/"
path.ghcn="../data_raw/meteorology/GHCN_extracted/"


download.ghcn(ID=IDnew, vars.in=vars.want, path.save=path.ghcn, dir.raw=dir.raw, gapfill=T, method="https")

download.ghcn(ID=IDold, vars.in=vars.want, path.save=path.ghcn, dir.raw=dir.raw, gapfill=T, method="https")


# ---------------------
datNew <- read.csv(file.path(path.ghcn, dir(path.ghcn, IDnew)))
datNew$DATE <- as.Date(datNew$DATE)
summary(datNew)

datOld <- read.csv(file.path(path.ghcn, dir(path.ghcn, IDold)))
datOld$DATE <- as.Date(datOld$DATE)
summary(datOld)

datAll <- rbind(datOld, datNew)
datAll$TMAX[datAll$flag.TMAX=="gapfill"] <- NA
datAll$TMIN[datAll$flag.TMIN=="gapfill"] <- NA
datAll$PRCP[datAll$flag.PRCP=="gapfill"] <- NA
summary(datAll)


datAll[datAll$YEAR==1992 & datAll$MONTH %in% c(4),]
datAll[datAll$YEAR==1992 & datAll$MONTH %in% c(5),]
datAll[datAll$YEAR==1992 & datAll$MONTH %in% c(6),]

# precipAprJun <- aggregate(PRCP ~ YEAR, data=datAll[datAll$MONTH %in% c(4:6),], FUN=sum, na.action=na.pass)
precipAprJun <- aggregate(PRCP ~ YEAR, data=datAll[datAll$MONTH %in% c(4:6),], FUN=sum, na.action=na.omit)
precipAprJun$PRCP[precipAprJun$PRCP==0] <- NA
summary(precipAprJun)
precipAprJun

precipAprJun[!is.na(precipAprJun$PRCP) & precipAprJun$PRCP<=precipAprJun$PRCP[precipAprJun$YEAR==2023],]


# precipJul <- aggregate(PRCP ~ YEAR, data=datAll[datAll$MONTH %in% c(7),], FUN=sum, na.action=na.pass)
precipJul <- aggregate(PRCP ~ YEAR, data=datAll[datAll$MONTH %in% c(7),], FUN=sum, na.action=na.omit)
precipJul$PRCP[precipJul$PRCP==0] <- NA
summary(precipJul)
precipJul

precipJul[!is.na(precipJul$PRCP) & precipJul$PRCP>=precipJul$PRCP[precipJul$YEAR==2023],]

png("../figures/2023 Preciptiation Apr-May-Jun.png", height=6, width=10, units="in", res=220)
ggplot(data=precipAprJun[precipAprJun$YEAR>1900,]) +
  ggtitle("April-Jun Precip") +
  geom_bar(aes(x=YEAR, y=PRCP), stat="identity", fill="gray50") +
  geom_bar(data=precipAprJun[precipAprJun$YEAR==2023,], aes(x=YEAR, y=PRCP), stat="identity", fill="orange2") +
  theme_bw()
dev.off()


png("../figures/2023 Preciptiation Jul.png", height=6, width=10, units="in", res=220)
ggplot(data=precipJul[precipJul$YEAR>1900,]) +
  ggtitle("July Precip") +
  geom_bar(aes(x=YEAR, y=PRCP), stat="identity", fill="gray50") +
  geom_bar(data=precipJul[precipJul$YEAR==2023,], aes(x=YEAR, y=PRCP), stat="identity", fill="blue") +
  theme_bw()
dev.off()
