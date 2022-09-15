
# http://www.css.cornell.edu/faculty/dgr2/_static/files/R_html/ex_rgee.html
library(rgee)
ee_Initialize(user="crollinson@mortonarb.org")

######### Playing with the imagery
dataset <- ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_EVI')$filterDate('2017-01-01', '2017-12-31')
ee_print(dataset)

landsat <- dataset$select('EVI')
class(landsat)

ee_print(landsat)

evi <- landsat$select('EVI')$toBands()  
class(evi)

ee_print(evi)

region.ithaca <- ee$Geometry$Rectangle(
  coords = c(-76.7, 42.2, -76.2, 42.7),
  proj = "EPSG:4326",
  geodesic = FALSE
)

evi.r <- ee_as_raster(evi, 
                      region=region.ithaca,
                      via = "drive",
                      scale = 1000)




############# Workign with TerraClimate data
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")
print(terraclimate)

#Task: Filter this to the 2001 records, select only the precipitation bands, cast to an ee.Image, and rename the bands.
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>% # dataset
  ee$ImageCollection$filterDate("2001-01-01", "2002-01-01") %>%   # data range
  ee$ImageCollection$map(function(x) x$select("pr")) %>% # Select only precipitation bands
  ee$ImageCollection$toBands() %>% # from ImageCollection to Image
  ee$Image$rename(sprintf("%02d",1:12)) # rename the bands of an image
print(terraclimate)

bandNames <- terraclimate$bandNames()
cat("Band names: ",paste(bandNames$getInfo(),collapse=",")) 

b0proj <- terraclimate$select('01')$projection()
cat("Band 01 projection: ", paste(b0proj$getInfo(),"\n", collapse = " "))

b0scale <- terraclimate$select('01')$projection()$nominalScale()
cat("Band 01 Scale: ", paste(b0scale$getInfo(),"\n", collapse = " "))

metadata <- terraclimate$propertyNames()
cat("Metadata: ", paste(metadata$getInfo(),"\n",collapse = " "))


# Task: Extract monthly precipitation values from the Terraclimate ee$ImageCollection`.
library(sf)
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
summary(nc)
nc[order(nc$BIR74, decreasing=TRUE), c("NAME", "BIR74")]


ee_nc_rain <- ee_extract(x = terraclimate, y = nc["NAME"], sf = FALSE)
dim(ee_nc_rain)
