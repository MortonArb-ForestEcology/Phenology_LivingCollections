###Quick che ck  of uncleaned data from pgadmin 
# Install and load the RPostgreSQL package
library(RPostgreSQL)
library(lubridate)
setwd("~/Volumes/GoogleDrive/My DriveLivingCollections_Phenology/Figures/2023_Figures")
# Set up database connection categories found under the connection tab of the properties of the Arboretum Phenology server in PgAdmin4. 
db_host <- "164.92.83.213" #host name/address e  
db_port <- 5432 #port
db_name <- "arboretum" #data base name we are interested in querying, for this project, so far there is only one
db_user <- "arboretum" # user name
db_password <- "arboretum1234" # password for the user, we only have one user, and the password has been set to this 

# Create a connection to the database
con <- dbConnect(
  PostgreSQL(),
  host = db_host,
  port = db_port,
  dbname = db_name,
  user = db_user,
  password = db_password
)

# Check for successful connection with these two print statments
if (inherits(con, "PostgreSQLConnection")) 
  {
  cat("Connected to PostgreSQL database")
} else {
  cat("Failed to connect to PostgreSQL database")
}

#######query and manipulate the data here######

##putting the pgadmin data into a data frame
# List tables in the public schema
tables <- dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'")

# select the "FormSubmission" and put into the dat.all data fram, this is the table containing all the data entered from the app. 
dat.all <- dbGetQuery(con, 'SELECT * FROM public."FormSubmission"')

# Preview of columns in FormSubmission data frame dat.all
print(head(dat.all))

#### Identify problematic years###

#setting the values in  DateObserved column to a date
dat.all$DateObserved <- as.Date(dat.all$DateObserved)

# Check for entries that do not have an entry for "DateObserved" or a funky or weird date, by funky or weird
# I mean values that don't fit the as.Date format
nodate <- which(is.na(dat.all$DateObserved))

#if statement which prints rows with no or weird values for the Dateobserved column,
if (length(nodate) > 0) {
  cat("Rows with missing values in DateObserved:\n")
  print(dat.all[nodate, ])
} 
##quick range check to what may be outside of the range 
range(dat.all$DateObserved)

#narrowing down those entries which may be outside of the year of phenology monitoring. 
## setting the year to 2023, but this should be set to "Sys.Date" in the future.
badyr <- which(year(dat.all$DateObserved) != 2023)
by.dat <- dat.all[badyr, ]
# Print the years that do not match
print(by.dat)


##### Graphing phenophase intensity 
### Breaking leaf buds
dat.all$BreakingLeafBudsIntensity <- factor(
  dat.all$BreakingLeafBudsIntensity,
  levels = c("0", "<3", "3-10", "11-100", "101-1,000", "1,001-10,000", ">10,000"),
  ordered = TRUE
)

#Get unique genera
unique_genera <- unique(dat.all$Genus)


# Loop through each genus and create a separate plot
for (genus in unique_genera) {
  genus_data <- dat.all %>%
    filter(Genus == genus, BreakingLeafBudsObserved == "y")
  
  # Plotting using ggplot2
  p <- ggplot(genus_data, aes(x = DateObserved, y = Species, color = BreakingLeafBudsIntensity)) +
    geom_point(alpha = 0.5) +
    scale_color_manual(values = c("0" = "white", "<3" = "red", "3-10" = "orange", 
                                  "11-100" = "yellow", "101-1,000" = "green", 
                                  "1,001-10,000" = "blue", ">10,000" = "violet")) +
    labs(title = paste("Breaking Leaf Buds_Intensity -", genus), x = "Date Observed", y = "Species") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"))
  
 
  # Save the plot as a separate file (you can adjust the file format and name)
  ggsave(paste("Genus_", gsub(" ", "_", genus), "_Breaking_Leaf_Intensity_Plot.png", sep = ""), plot = p)
}
##Leaves
dat.all$LeafIntensity <- factor(
  dat.all$LeafIntensity,
  levels = c("0 %", "<5 %", "5-24 %", "24-49 %", "50-74 %", "75-94 %", ">95 %"),
  ordered = TRUE
)
head(dat.all)
#Get unique genera
unique_genera <- unique(dat.all$Genus)


# Loop through each genus and create a separate plot
for (genus in unique_genera) {
  genus_data <- dat.all %>%
    filter(Genus == genus, LeafObserved == "y")
  
  # Plotting using ggplot2
  p <- ggplot(genus_data, aes(x = DateObserved, y = Species, color = LeafIntensity)) +
    geom_point(alpha = 0.5) +
    scale_color_manual(values = c("0%" = "white", "<5%" = "red", "5-24 %" = "orange", 
                                  "24-49 %" = "yellow", "50-74 %" = "green", 
                                  "75-94 %" = "blue", ">95 %" = "violet")) +
    labs(title = paste("Leaf Intensity -", genus), x = "Date Observed", y = "Species") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"))
  
  
  # Save the plot as a separate file (you can adjust the file format and name)
  ggsave(paste("Genus_", gsub(" ", "_", genus), "_Leaf_Intensity_Plot.png", sep = ""), plot = p)
}
# Disconnect from the data base
dbDisconnect(con)
