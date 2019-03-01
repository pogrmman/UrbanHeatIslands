### Packages ####
library(dplyr)
library(tidyr)
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