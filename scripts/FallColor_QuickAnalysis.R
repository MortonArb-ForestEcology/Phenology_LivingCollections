# Script to look at conditions going into the fall
# # - how does our weather compare to past years?
# # - when have we seen start/"peak" color in the past?
# # - maybe get/compare USA-NPN data for IL/IN/WI to look at the wave coming our way

# Looking at Arboretum data
# 1. Get all available Arboretum Data
# 2. Calculate 50% metric --> 50% leaves with color OR 50% canopy with leaves OR 25% & 25%
# 


# Trying to look at MODIS data from google earth engine
# https://cran.r-project.org/web/packages/rgee/vignettes/rgee01.html
library(rgee); library(reticulate)
# ee_install()
# ee_install(py_env = "rgee")
ee_check()
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive = FALSE, gcs = FALSE)
# ee_Initialize()
# earthengine_python <- Sys.getenv("EARTHENGINE_PYTHON", unset = NA)
# Sys.setenv(RETICULATE_PYTHON = earthengine_python)
# reticulate::py_config()
ee_get_earthengine_path()

srtm <- ee$Image("USGS/SRTMGL1_003")
