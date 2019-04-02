### Packages ####
library(dplyr)

# Utility function to do calculations differently on different temp types
getVals <- function(type, temp, max, min, op) {
  if (type == "TMAX") {
    return(do.call(op, list(temp, max)))
  } else if (type == "TMIN") {
    return(do.call(op, list(temp, min)))
  }
}

# Calculate monthly average highs and lows and daily deviation by station, then standardize
monthlyAvgs <- allTemps %>% group_by(Station, Month) %>%
  summarize(MeanMonthlyMax = mean(Temperature[TempType=="TMAX"], na.rm = TRUE),
            MeanMonthlyMin = mean(Temperature[TempType=="TMIN"], na.rm = TRUE),
            MeanMonthly = mean(Temperature, na.rm = TRUE))

dailyDeviations <- allTemps %>% left_join(monthlyAvgs, 
                                          by=c("Station", "Month")) %>%
  mutate(Deviation = mapply(function(type, temp, max, min) {
    getVals(type, temp, max, min, "-")
  }, TempType, Temperature, MeanMonthlyMax, MeanMonthlyMin)) %>%
  select(-Temperature) %>% group_by(Station, Month) %>%
  mutate(StandardizedTemp = mapply(function(type, dev, max, min) {
      getVals(type, dev, max, min, "/")
    }, TempType, Deviation, sd(Deviation[TempType=="TMAX"], na.rm=TRUE), 
                            sd(Deviation[TempType=="TMIN"], na.rm=TRUE))) %>% ungroup() %>%
  select(-Deviation)

# Calculate the average standardized monthly temps by decade
decadeStats <- dailyDeviations %>% group_by(Station, Decade, Month) %>%
  summarize(DecadeStdMonthlyMax = mean(StandardizedTemp[TempType=="TMAX"],
                                        na.rm = TRUE),
            DecadeStdMonthlyMin = mean(StandardizedTemp[TempType=="TMIN"],
                                       na.rm = TRUE),
            DecadeStdMonthly = mean(StandardizedTemp, na.rm = TRUE))

# Calculate decadal standardized temperatures
decadeAvgs <- decadeStats %>% group_by(Station, Decade) %>%
  summarize(StdMax = mean(DecadeStdMonthlyMax, na.rm = TRUE),
            StdMin = mean(DecadeStdMonthlyMin, na.rm = TRUE), 
            StdMean = mean(DecadeStdMonthly, na.rm = TRUE))

# Grab station information
stations <- dailyDeviations %>% select(Station, StationLat, StationLong,
                                       StationElev, MetroName, CityLat, 
                                       CityLong, StationDist, Population,
                                       Decade) %>% distinct()

# Correlate with decade info
decadeAvgs <- decadeAvgs %>% left_join(stations, by=c("Station", "Decade"))
decadeStats <- decadeStats %>% left_join(stations, by=c("Station", "Decade"))
monthlyAvgs <- decadeStats %>% left_join(stations, by=c("Station", "Decade"))