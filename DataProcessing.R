### Packages ###
library(dplyr)
### Project Functions ###
source("./APICalls.R")
source("./Locations.R")
source("./Climate.R")
source("./CensusData.R")

# Get biggest 100 metro areas and their populations from 1970 to 2010
metros <- getMetros(100) %>% getCounties() %>% getCensusPopulation()
# Principal cities, FIPS codes, and locations
metros <- getCities(metros) %>% fipsCorrelate() %>% latLongCorrelate()
# Calculate 40 mile box around each metro area
metros <- calculateRegion(metros, 40)
# Get climate stations
metros <- getStations(metros)