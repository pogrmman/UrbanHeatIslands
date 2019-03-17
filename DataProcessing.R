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

# Calculate monthly average highs and lows and daily deviation by station
monthlyAvgs <- allTemps %>% group_by(Station, Month) %>%
  summarize(MeanMonthlyMax = mean(Temperature[TempType=="TMAX"], na.rm = TRUE),
            MeanMonthlyMin = mean(Temperature[TempType=="TMIN"], na.rm = TRUE),
            MeanMonthly = mean(Temperature, na.rm = TRUE))

dailyDeviations <- allTemps %>% left_join(monthlyAvgs, 
                                          by=c("Station", "Month")) %>%
  mutate(Deviation = mapply(function(type, temp, max, min) {
    getVals(type, temp, max, min, "-")
  }, TempType, Temperature, MeanMonthlyMax, MeanMonthlyMin)) %>%
  select(-Temperature)

# Standardize by month, using standard deviation of all stations
# If we standardized by the standard deviation of each station, we lose
# information about how the variability of that station compares to other
# stations
standardMonthlyDeviations <- dailyDeviations %>%  group_by(Month) %>%
    summarize(StdMaxDev = sqrt(mean(Deviation[TempType=="TMAX"]^2, 
                                    na.rm = TRUE)),
              StdMinDev = sqrt(mean(Deviation[TempType=="TMIN"]^2,
                                    na.rm = TRUE)))

dailyDeviations <- dailyDeviations %>% left_join(standardMonthlyDeviations,
                                                 by=c("Month")) %>%
  mutate(StandardizedTemp = mapply(function(type, dev, max, min) {
      getVals(type, dev, max, min, "/")
    }, TempType, Deviation, StdMaxDev, StdMinDev)) %>%
  select(-Deviation)

# Calculate the average monthly temp by decade
decadeStats <- allTemps %>% group_by(Station, Decade, Month) %>%
  summarize(DecadeMeanMonthlyMax = mean(Temperature[TempType=="TMAX"],
                                        na.rm = TRUE),
            DecadeMeanMonthlyMin = mean(Temperature[TempType=="TMIN"],
                                        na.rm = TRUE),
            DecadeMeanMonthly = mean(Temperature,
                                     na.rm = TRUE))

# Calculate deviations in the decadal monthly temps from mean monthly temps
decadeStats <- decadeStats %>% left_join(monthlyAvgs,
                                         by=c("Station", "Month")) %>%
  mutate(DecadeMaxDeviation = DecadeMeanMonthlyMax - MeanMonthlyMax,
         DecadeMinDeviation = DecadeMeanMonthlyMin - MeanMonthlyMin,
         DecadeMeanDeviation = DecadeMeanMonthly - MeanMonthly)

decadeMonthlyStdDev <- decadeStats %>% group_by(Decade, Month) %>%
  summarize(DecadeMaxStdDev = sqrt(mean(DecadeMaxDeviation^2, 
                                        na.rm = TRUE)),
            DecadeMinStdDev = sqrt(mean(DecadeMinDeviation^2,
                                        na.rm = TRUE)),
            DecadeMeanStdDev = sqrt(mean(DecadeMeanDeviation^2,
                                         na.rm = TRUE)))

# Calculate standardized monthly temps by decade
decadeStats <- decadeStats %>% left_join(decadeMonthlyStdDev, 
                                         by=c("Decade", "Month")) %>%
  mutate(StdMaxMonthlyDecade = DecadeMaxDeviation / DecadeMaxStdDev,
         StdMinMonthlyDecade = DecadeMinDeviation / DecadeMinStdDev,
         StdMeanMonthlyDecade = DecadeMeanDeviation / DecadeMeanStdDev)

# Calculate decadal standardized temperatures
decadeAvgs <- decadeStats %>% group_by(Station, Decade) %>%
  summarize(StdMax = mean(StdMaxMonthlyDecade, na.rm = TRUE),
            StdMin = mean(StdMinMonthlyDecade, na.rm = TRUE), 
            StdMean = mean(StdMeanMonthlyDecade, na.rm = TRUE))
