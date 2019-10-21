# Anecdotal 'Forecast' for fall color 2019
library(ggplot2)
# ----------------------------------------------------------
# Q: How do conditions this year compare to other years?
# ----------------------------------------------------------
# -----------------------
# A: Lets start by looking at the Arboretum's station data!
# -----------------------
dat.met <- read.csv("/Volumes/GoogleDrive/My Drive/Arboretum Met Data/GHCN-Daily/MortonArb_GHCND-USC00115097_2007-04-01_2019-09-30.csv")
dat.met$DATE <- as.Date(dat.met$DATE)
dat.met$year <- lubridate::year(dat.met$DATE)
dat.met$yday <- lubridate::yday(dat.met$DATE)
summary(dat.met)

met.norm <- aggregate(dat.met[,c("PRCP", "SNOW", "TMAX", "TMIN")],
                      by=list(dat.met[,c("yday")]),
                      FUN=median, na.rm=T)
names(met.norm)[1] <- "yday"
summary(met.norm)

met.summary <- stack(met.norm[,c("PRCP", "SNOW", "TMAX", "TMIN")])
names(met.summary) <- c("val.mean", "ind")
met.summary$yday <- met.norm$yday
met.summary$lwr.25 <- stack(aggregate(dat.met[,c("PRCP", "SNOW", "TMAX", "TMIN")],
                                   by=list(dat.met[,c("yday")]),
                                   FUN=quantile, 0.25, na.rm=T)[,c("PRCP", "SNOW", "TMAX", "TMIN")])[,1]
met.summary$upr.75 <- stack(aggregate(dat.met[,c("PRCP", "SNOW", "TMAX", "TMIN")],
                                   by=list(dat.met[,c("yday")]),
                                   FUN=quantile, 0.75, na.rm=T)[,c("PRCP", "SNOW", "TMAX", "TMIN")])[,1]
met.summary$ind <- factor(met.summary$ind, levels=c("TMAX", "TMIN", "PRCP", "SNOW"))
summary(met.summary)

met.2019 <- stack(dat.met[dat.met$year==2019,c("PRCP", "SNOW", "TMAX", "TMIN")])
met.2019$yday <- dat.met[dat.met$year==2019,c("yday")]


yrs.mark <- data.frame(Label=c("Jan 1", "Apr 1", "Jul 1", "Oct 1"), 
                       Date=c("2019-01-01", "2019-04-01", "2019-07-01", "2019-10-01"))
yrs.mark$mark.yday <- lubridate::yday(yrs.mark$Date)
yrs.mark$mark.yday[1] <- yrs.mark$mark.yday[1]-365


png("/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/Reports/Weather_YearToDate_GHCN_2019-09-26.png", height=8, width=8, units="in", res=220)
ggplot(data=met.summary) +
  ggtitle("Arb Weather Data: 2019 vs. 2007-2019 median") +
  facet_grid(ind~., scales="free_y") +
  geom_ribbon(aes(x=yday, ymin=lwr.25, ymax=upr.75, fill="'normal'"), alpha=0.5) +
  geom_line(aes(x=yday, y=val.mean, color="'normal'")) +
  geom_line(data=met.2019, aes(x=yday, y=values, color="2019", fill="2019")) +
  # guides(fill=F)
  scale_fill_manual(name="year", values=c("black", "green3")) +
  scale_color_manual(name="year", values=c("black", "green3")) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=yrs.mark$mark.yday, labels = yrs.mark$Label) +
  theme_bw() +
  theme(legend.position="top")
dev.off()
# -----------------------

# -----------------------
# Lets also look at Daymet because it has PAR!
# Note: Can't get 2019 data (yet), but we can at least look at the relationship between precip & PAR
# -----------------------
library(ggplot2)
daymet <- read.csv("/Volumes/GoogleDrive/My Drive/Arboretum Met Data/Daymet/MortonArb-VisitorCenter/Daymet_MortonArb_1980-2018.csv")
summary(daymet)

plot(srad.Wm2 ~ log(prcp.mm), data=daymet[daymet$yday>=120 & daymet$yday<=210,])

# -----------------------

# ----------------------------------------------------------
