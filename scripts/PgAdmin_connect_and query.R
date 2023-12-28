###Quick che ck  of uncleaned data from pgadmin 
# Install and load the RPostgreSQL package
library(RPostgreSQL)
library(lubridate)

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
badyr <- which(year(dat.all$DateObserved) != year(Sys.Date()))
by.dat <- dat.all[badyr, ]
# Print the years that do not match
print(by.dat)

##Separating out entries into 

# Disconnect from the data base
dbDisconnect(con)
