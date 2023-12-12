###Quick che ck  of uncleaned data from pgadmin 
# Install and load the RPostgreSQL package
library(RPostgreSQL)

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


# Disconnect from the data base
dbDisconnect(con)
