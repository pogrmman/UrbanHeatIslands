### Packages ###
library(dplyr)
### Project Functions ###
source("./APICalls.R")
source("./Locations.R")
source("./Climate.R")

# Get biggest 50 metro areas and their principal cities
metros <- getMetros(50) %>% getCities()
# Get FIPS codes and locations
metros <- fipsCorrelate(metros) %>% latLongCorrelate()
# Calculate 40 mile box around each metro area
metros <- calculateRegion(metros, 40)
# Get climate stations
metros <- getStations(metros)