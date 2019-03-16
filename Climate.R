### Packages ####
library(dplyr)
library(tidyr)
library(stringr)
library(geosphere)
### Project Functions ###
source("./APICalls.R", local=TRUE)

### Functions ###
# Get lists of stations for each metro area
getStations <- function(metros) {
  print("Getting climate stations")
  metros <- metros %>% 
    mutate(StationList = mapply(function(s, w, n, e) {
      getClimateData(paste("stations?extent=", s, ",", w, ",", n, ",", e, 
                           "&enddate=1965-01-01",
                           "&startdate=2015-01-01",
                           "&sortfield=mindate",
                           "&datasetid=GHCND", sep=""))},
      SouthBound, WestBound, NorthBound, EastBound, SIMPLIFY=FALSE)) %>%
    select(-SouthBound, -WestBound, -NorthBound, -EastBound) %>%
    unnest () %>% filter(datacoverage > .5) %>% group_by(MetroName)
  # Calculate distance in km to each station from city center
  metros <- metros %>% mutate(Dist = distCosine(cbind(Longitude, Latitude),
                                              cbind(longitude, latitude))/1000)
  return(metros)
}

# Get climate data for each year for each station
getTemperatures <- function(metros) {
  # Get data for each metro area
  for (stationId in metros$id) {
    year <- 1966
    results <- getTempData(stationId, year)
    if (!(nrow(filter(temperatures, Station == stationId, Year == year)))) {
      results <- results %>% 
        mutate(value = value / 10,
               date = str_extract(date, "[0-9]{4}-[0-9]{2}-[0-9]{2}")) %>%
        separate(date, into=c("Year", "Month", "Day"), sep="-") %>% 
        mutate(Decade = floor(as.numeric(Year)/10 + .5) * 10) %>%
        mutate(Error = datatype == "ERROR") %>%
        rename(Station = station, TempType = datatype, Attributes = attributes,
               Temperature = value)
      temperatures <<- temperatures %>% bind_rows(results)
        
    }
    for (year in 1967:2015) {
      if (!(nrow(filter(temperatures, Station == stationId, Year == year)))) {
        results <- getTempData(stationId, year)
        results <- results %>% 
          mutate(value = value / 10,
                 date = str_extract(date, "[0-9]{4}-[0-9]{2}-[0-9]{2}")) %>%
          separate(date, into=c("Year", "Month", "Day"), sep="-") %>% 
          mutate(Decade = floor(as.numeric(Year)/10 + .5) * 10) %>%
          mutate(Error = datatype == "ERROR") %>%
          rename(Station = station, TempType = datatype, 
                 Attributes = attributes, Temperature = value)
        temperatures <<- temperatures %>% bind_rows(results)
      }
    }
  }
  # Exclude bad temperatures
  temperatures <<- temperatures %>% 
    separate(Attributes, into = c("MeasureFlag", "QualFlag", 
                                  "SourceFlag", "Time")) %>%
    filter(QualFlag == "") %>% select(-MeasureFlag, -QualFlag, -SourceFlag, 
                                      -Time)
  # Tidy data
  allData <- temperatures %>% left_join(metros, by=c(Station = "id")) %>%
    gather(key=PopYear, value=Population, Population1970, 
           Population1980, Population1990, Population2000, Population2010) %>% 
    mutate(PopYear = str_extract(PopYear, "[0-9]{4}")) %>% 
    filter(PopYear == Decade) %>%
    select(Year, Month, Day, Station, 
           latitude, longitude, elevation, 
           TempType, Temperature, MetroName,
           Latitude, Longitude, Dist, Population, Decade) %>%
    rename(StationLat = latitude, StationLong = longitude, 
           StationElev = elevation, CityLat = Latitude, CityLong = Longitude,
           StationDist = Dist)
  temperatures <<- temperatures[0,]
  return(allData)
}

# Regrab data for each failed one
redoErrors <- function(errList) {
  for (failed in errList) {
    temperatures <<- getTempData(failed$Station, failed$Year)
  }
}