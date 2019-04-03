### Packages ####
library(dplyr)
library(tidyr)

# Utility function to do calculations differently on different temp types
getVals <- function(type, temp, max, min, op) {
  if (type == "TMAX") {
    return(do.call(op, list(temp, max)))
  } else if (type == "TMIN") {
    return(do.call(op, list(temp, min)))
  }
}

# Calculate monthly average highs and lows by metro area
monthlyAvgs <- allTemps %>% group_by(MetroName, Month) %>%
  summarize(MeanMonthlyMax = mean(Temperature[TempType=="TMAX"], na.rm = TRUE),
            MeanMonthlyMin = mean(Temperature[TempType=="TMIN"], na.rm = TRUE),
            MeanMonthly = mean(Temperature, na.rm = TRUE))

# Calculate daily deviations from monthly averages and standardize by metro area
dailyDeviations <- allTemps %>% left_join(monthlyAvgs, 
                                          by=c("MetroName", "Month")) %>%
  mutate(Deviation = mapply(function(type, temp, max, min) {
    getVals(type, temp, max, min, "-")
  }, TempType, Temperature, MeanMonthlyMax, MeanMonthlyMin)) %>%
  select(-Temperature) %>% group_by(MetroName, Month) %>%
  mutate(StandardizedTemp = mapply(function(type, dev, max, min) {
      getVals(type, dev, max, min, "/")
    }, TempType, Deviation, sd(Deviation[TempType=="TMAX"], na.rm=TRUE), 
                            sd(Deviation[TempType=="TMIN"], na.rm=TRUE))) %>% ungroup() %>%
  select(-Deviation)

# Calculate the average standardized monthly temps by decade and make season column
decadeStats <- dailyDeviations %>% group_by(Station, Decade, Month) %>%
  summarize(DecadeStdMonthlyMax = mean(StandardizedTemp[TempType=="TMAX"],
                                        na.rm = TRUE),
            DecadeStdMonthlyMin = mean(StandardizedTemp[TempType=="TMIN"],
                                       na.rm = TRUE),
            DecadeStdMonthly = mean(StandardizedTemp, na.rm = TRUE)) %>%
  mutate(Season = mapply(function(month) {
    if (month == 12 | month == 1 | month == 2) {
      return("Winter")
    } else if (month == 3 | month == 4 | month == 5) {
      return("Spring")
    } else if (month == 6 | month == 7 | month == 8) {
      return("Summer")
    } else if (month == 9 | month == 10 | month == 11) {
      return("Fall")
    }}, Month)) %>% 
  mutate(Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter")))

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

# Get number of days in bottom/top 1% of temperatures
hottestDays <- dailyDeviations %>% filter(TempType == "TMAX") %>%
  group_by(MetroName) %>% filter(StandardizedTemp > quantile(StandardizedTemp, .99)) %>% ungroup() %>%
  group_by(Station, Decade) %>% summarize(HotDays = n())
coldestDays <- dailyDeviations %>% filter(TempType == "TMAX") %>%
  group_by(MetroName) %>% filter(StandardizedTemp < quantile(StandardizedTemp, .01)) %>% ungroup () %>%
  group_by(Station, Decade) %>% summarize(ColdDays = n())
hottestNights <- dailyDeviations %>% filter(TempType == "TMIN") %>%
  group_by(MetroName) %>% filter(StandardizedTemp > quantile(StandardizedTemp, .99)) %>% ungroup() %>%
  group_by(Station, Decade) %>% summarize(HotNights = n())
coldestNights <- dailyDeviations %>% filter(TempType == "TMIN") %>%
  group_by(MetroName) %>% filter(StandardizedTemp < quantile(StandardizedTemp, .01)) %>% ungroup () %>%
  group_by(Station, Decade) %>% summarize(ColdNights = n())

# Correlate with station info
extremes <- hottestDays %>% left_join(coldestDays, by=c("Station", "Decade")) %>%
  left_join(hottestNights, by=c("Station", "Decade")) %>% 
  left_join(coldestNights, by=c("Station", "Decade")) %>%
  left_join(stations, by=c("Station", "Decade")) %>%
  replace_na(list(HotDays = 0,
                  ColdDays = 0,
                  HotNights = 0,
                  ColdNights = 0)) %>% distinct()

# Remove unnecessary info
rm(coldestDays, coldestNights, hottestDays, hottestNights)