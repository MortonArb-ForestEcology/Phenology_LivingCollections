# A new script animating phenology change by geogrpahy 
library(ggplot2)
library(readr)
library(lubridate)
library(tidyverse)
library(gganimate)
library(dplyr)
library(transformr)
library(sf)
library(ggmap)


path.google <- "~/Google Drive/My Drive" # Mac
path.dat <- file.path(path.google,"/LivingCollections_Phenology/Data_Observations")
pathl <- file.path(path.google,"/LivingCollections_Phenology/Observing Lists")
#path.figs <- file.path(path.google, "LivingCollections_Phenology/Reports/2023_02_EndOfYear_Report/figures_2023_end")
# this is for google -># 
path.figs <- "~/Google Drive/My Drive/LivingCollections_Phenology/Reports/2023_02_End_Of_Year_Report/figures_2023_end"
if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")

# -----------------------------------
# 2023 pheno data
# -----------------------------------

acer23<- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Acer_2023_FINAL.csv"))
quercus23 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Quercus_2023_FINAL.csv"))
ulmus23 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Ulmus_2023_FINAL.csv"))
##binding 
df23<- rbind(ulmus23, quercus23, acer23)

#
#getting the correct date format
df23$yday <- lubridate::yday(df23$Date.Observed)
df23$Date <- as.Date(paste0("2023-", df23$yday), format="%Y-%j")

##werid 2027 value
summary(df23)

#Checking what is present in the collection column to see if it's been removed
unique(df23$Collection)
head(df23)

### full list data 
dfl<- read.csv(file.path(pathl,"ObservingList_AllCombined_2023.csv"))

dat.l <- dfl[, c("PlantID", "BgLatitude", "BgLongitude")]
head(dat.l)

dfll<- dat.l %>% 
  rename("PlantNumber"= "PlantID", "Latitude" = "BgLatitude", "Longitude" = "BgLongitude")
head(dfll)

# Merge the data frames based on the PlantNumber column
merged_df <- merge(df23, dfll, by = "PlantNumber")

# Select only the relevant columns (Latitude and Longitude) from dfll
result_df <- merged_df[, c("PlantNumber", "Latitude", "Longitude")]

# If you want to update df23 with the new Latitude and Longitude columns
df23 <- merge(df23, result_df, by = "PlantNumber", all.x = TRUE)

print(colnames(df23))

# Convert "Date" column to Date type
df23$Date <- as.Date(df23$Date)

dflc <- df23[df23$leaf.color.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.color.intensity", "Date", "yday","Longitude", "Latitude", "Collection")]
dflc <- dflc[!is.na(dflc$PlantNumber),]
summary(dflc)
head(dflc)


summary(dflc)


# Remove rows with missing longitude or latitude
dflc <- dflc[complete.cases(dflc[c("Longitude", "Latitude")]), ]
dflc <- na.omit(dflc)

dflc$Date <- as.Date(dflc$Date)
# Load required libraries
library(ggplot2)
library(gganimate)


# Convert "Date" column to Date type
dflc$Date <- as.Date(dflc$Date)

# Create an animated map with facet grid
anim <- ggplot(data=dflc)+
  facet_grid(Collection ~ ., scales = "free", space = "free") +
  geom_point(alpha=0.25, aes(x = Longitude, y = Latitude, group = PlantNumber, color = leaf.color.intensity, size = 1)) +
  scale_color_manual(values = c("<5%" = "blue", ">95%" = "red", "0%" = "green",
                                "25-49%" = "yellow", "5-24%" = "orange",
                                "50-74%" = "purple", "75-94%" = "pink")) +
  labs(title = "Leaf Color Intensity by Date",
       subtitle = "{closest_state}") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  transition_states(Date, transition_length = 2, state_length = 1) +
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, falloff = 'sine-in', exclude_phase = 'enter')

# Animate the map
anim <- animate(anim, nframes = 100, duration = 10, width = 800, height = 600)

# Display the animation
anim

###
# Create a list to store individual animated plots
animated_plots <- list()

# Iterate over unique collections and create animated plots
for (collection in unique(dflc$Collection)) {
  subset_data <- dflc[dflc$Collection == collection & dflc$leaf.color.intensity != "0%", ]
  
  anim <- ggplot(subset_data, aes(x = Longitude, y = Latitude, group = PlantNumber)) +
    geom_point(aes(color = leaf.color.intensity, size = 1)) +
    scale_color_manual(values = c("<5%" = "olivedrab2", ">95%" = "sienna3", "0%" = NA,
                                  "25-49%" = "yellow3", "5-24%" = "goldenrod2",
                                  "50-74%" = "darkorange", "75-94%" = "orangered3")) +
    labs(title = paste("Leaf Color Intensity by Date -", collection),
         subtitle = "{closest_state}",
         caption = "Source: Your Data Source") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    transition_states(Date, transition_length = 4, state_length = 1) +
    shadow_mark(1, size = 1, alpha = TRUE, wrap = TRUE, falloff = 'sine-in-out', exclude_phase = 'enter') +
    coord_cartesian(ylim = c(min(subset_data$Latitude), max(subset_data$Latitude)),
                    xlim = c(min(subset_data$Longitude), max(subset_data$Longitude)))
  
  animated_plots[[collection]] <- animate(anim, nframes = 100, duration = 30, width = 800, height = 600)
}

# Display the individual animated plots
for (i in seq_along(animated_plots)) {
  print(animated_plots[[i]])
}
for (i in seq_along(animated_plots)) {
  anim_save(file.path(path.figs, paste("animated_plot_", i, ".gif", sep="")), animated_plots[[i]])
}


####
ggplot(data=leaf.color) +
  facet_grid(Collection~ .,scales="free_y") +
  geom_freqpoly(alpha=0.25,aes(x=Date, color=as.factor(Year),)) +
  scale_x_date(date_labels="%b %d", date_breaks="1 month") + 
  scale_fill_manual(name="Year", values=c("2023"="red", "others"="gray"), guide=guide_legend(override.aes=list(fill=c("red")))) +
  scale_color_manual(name="Year", values=c("2023"="red", "others"="gray")) +
  theme_bw() +
  labs(title="Leaf Color Present", x="Day of Year") +


dev.off()


# Create an sf object for spatial data (latitude and longitude)
dflc_sf <- st_as_sf(dflc, coords = c("Longitude", "Latitude"), crs = 4326)

# Create an animated map
anim <- ggplot(dflc) +
  geom_point(aes(x = Longitude, y = Latitude, color = leaf.color.intensity, size = 3)) +
  scale_color_gradient(low = "green", high = "red") +
  labs(title = "Leaf Color Intensity by Date",
       subtitle = "{frame_time}",
       caption = "Source: Your Data Source") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  transition_states(Date, transition_length = 2, state_length = 1)+
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 

# Animate the map
anim <- animate(anim, nframes = 100, duration = 1, width = 800, height = 600)

# Display the animation
anim
# Save the animation
#anim_save("leaf_color_intensity_animation.gif", animation = anim, duration = 1, width = 800, height = 600)


