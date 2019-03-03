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
  getData <- function(station) {
    year <- 1966
    url <- paste("data?datasetid=GHCND&startdate=", year, "-01-01&enddate=",
                 year, "-12-31&stationid=", station, 
                 "&datatypeid=TMAX,TMIN&limit=730", sep="")
    results <- getClimateData(url)
    for (year in 1967:2015) {
      url <- paste("data?datasetid=GHCND&startdate=", year, "-01-01&enddate=",
                   year, "-12-31&stationid=", station, 
                   "&datatypeid=TMAX,TMIN&limit=730", sep="")
      results <- results %>% bind_rows(getClimateData(url))
    }
    results <- results %>% mutate(value = value / 10,
                                  date = str_extract(date,
                                         "[0-9]{4}-[0-9]{2}-[0-9]{2}")) %>%
      separate(date, into=c("Year", "Month", "Day"), sep="-") %>% 
      mutate(Decade = floor(as.numeric(Year)/10) * 10)
    return(results)
  }
  # Get data for each metro area
  metros <- metros %>% 
    mutate(results = mapply(function(id) {getData(id)}, id, SIMPLIFY=FALSE))
  allData <- bind_rows(metros$results)
  metros <- select(metros, -results)
  # Tidy data
  allData <- allData %>% left_join(metros, by=c(station = "id")) %>%
    gather(key=PopYear, value=Population, Population1970, 
           Population1980, Population1990, Population2000, Population2010) %>% 
    mutate(PopYear = str_extract(PopYear, "[0-9]{4}")) %>% 
    filter(PopYear == Decade) %>%
    select(Year, Month, Day, station, latitude, longitude, elevation, datatype, 
           value, MetroName, Latitude, Longitude, Dist, Population, Decade) %>%
    rename(Station = station, StationLat = latitude, StationLong = longitude,
           StationElev = elevation, TempType = datatype, Temperature = value, 
           CityLat = Latitude, CityLong = Longitude, StationDist = Dist)
  return(allData)
}