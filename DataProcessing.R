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
# Because the climate bureau API limits us to 10k requests/day
# Metros must be broken into batches of 204 (we're getting 49 years of data)
# This means it'll take ~3 days to get all the data
batch1 <- metros[1:204,]
batch2 <- metros[205:408,]
batch3 <- metros[409:420,]
# The following lines must be run one at a time, on different days:
# batch1Temps <- getTemperatures(batch1)
# batch2Temps <- getTemperatures(batch2)
# batch3Temps <- getTemperatures(batch3) -- Done
allTemps <- bind_rows(batch1Temps, batch2Temps, batch3Temps)