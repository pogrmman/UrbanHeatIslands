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
# Global preserves results if interrupted.
temperatures <- data.frame(matrix(ncol=9, nrow=0)) 
colnames(temperatures) <- c("Year", "Month", "Day", "Station", "TempType",
                            "Attributes", "Temperature", "Decade", "Error")
# The following lines must be run one at a time, on different days:
# batch1Temps <- getTemperatures(batch1) -- Done
# batch2Temps <- getTemperatures(batch2) -- Done
# batch3Temps <- getTemperatures(batch3) -- Done
allTemps <- bind_rows(batch1Temps, batch2Temps, batch3Temps)

# Utility function to do calculations differently on different temp types
getVals <- function(type, temp, max, min, op) {
  if (type == "TMAX") {
    return(do.call(op, list(temp, max)))
  } else if (type == "TMIN") {
    return(do.call(op, list(temp, min)))
  }
}

# Calculate monthly average highs and lows and daily deviation by station
allTemps <- allTemps %>% group_by(Station, Month) %>%
  mutate(MeanMonthlyMax = mean(Temperature[TempType=="TMAX"], na.rm = TRUE),
         MeanMonthlyMin = mean(Temperature[TempType=="TMIN"], na.rm = TRUE),
         MeanMonthly = mean(Temperature, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(Deviation = mapply(function(type, temp, max, min) {
    getVals(type, temp, max, min, "-")
  }, TempType, Temperature, MeanMonthlyMax, MeanMonthlyMin))

# Standardize by month, using standard deviation of all stations
allTemps <- allTemps %>%  group_by(Month) %>%
    mutate(StdMaxDev = sqrt(mean(Deviation[TempType=="TMAX"]^2, 
                                 na.rm = TRUE)),
           StdMinDev = sqrt(mean(Deviation[TempType=="TMIN"]^2, 
                                 na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(StandardizedTemp = mapply(function(type, dev, max, min) {
      getVals(type, dev, max, min, "/")
    }, TempType, Deviation, StdMaxDev, StdMinDev))

# Calculate the average standardized temp by decade
allTemps <- allTemps %>% group_by(Station, Decade) %>%
  mutate(DecadeStdTemp = median(StandardizedTemp)) %>% ungroup()

# Calculate the average monthly temp by decade
allTemps <- allTemps %>% group_by(Station, Decade, Month) %>%
  mutate(DecadeMeanMonthlyMax = mean(Temperature[TempType=="TMAX"]),
         DecadeMeanMonthlyMin = mean(Temperature[TempType=="TMIN"]),
         DecadeMeanMonthly = mean(Temperature)) %>% ungroup()
