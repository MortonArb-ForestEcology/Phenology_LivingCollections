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

##query and manipulate the data here

# List tables in the public schema
tables <- dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'")
cat("Tables in the public schema:\n", tables$table_name, "\n\n")

# Query the "FormSubmission" table
form_submission_data <- dbGetQuery(con, 'SELECT * FROM public."FormSubmission"')

# Preview of FormSubmission data
cat("Preview of FormSubmission data:\n")
print(head(form_submission_data))
#renaming because I don't want to copy and paste "form_submission_data" every time. 
dat.all<- form_submission_data

# Identify problematic years
dat.all$DateObserved <- as.Date(dat.all$DateObserved)
yrs.bad <- which(year(dat.all$DateObserved) != year(Sys.Date()))
by.dat <- dat.all[yrs.bad, ]
# Print the years that do not match
print(by.dat)


# Disconnect from the data base
dbDisconnect(con)
