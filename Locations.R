#### Packages ###
library(stringr)
library(dplyr)
library(fuzzyjoin)
### Project Functions ###
source("./APICalls.R")

### Functions ###
# Get population of all metro areas July 1, 2016
getMetros <- function(number) {
  print("Fetching list of metro areas")
  url <- paste("2016/pep/population?get=POP,GEONAME&for=metropolitan%20",
               "statistical%20area/micropolitan%20statistical%20area:*&DATE=9",
               sep="")
  metroPops <- getCensusData(url)
  # Cleanup data
  names <- metroPops[1,]
  metroPops <- data.frame(metroPops[-1,])
  colnames(metroPops) <- names
  # Convert POP from char factor to number
  metroPops$POP <- as.numeric(as.character(metroPops$POP))
  # Sort in descending order by population
  metroPops <- metroPops[order(metroPops$POP, decreasing=TRUE),]
  return(metroPops[1:number,])
}

# Extract Principal City and State
getCities <- function(metros) {
  print("Finding principal city")
  pat <- "(.*)[:space:](Metro Area)$"
  metros$GEONAME <- str_match(metros$GEONAME, pat)[,2]
  # Assume principal city and state are first listed in name
  pat <- paste("^([A-Z][a-z\\.]+(?:[/[:space:]](?:[A-Z][a-z\\.]+))*)",
               "[^,]*,[:space:]([A-Z]{2})", sep="")
  citiesStates <- str_match(metros$GEONAME, pat)[,2:3]
  metros$PrincipalCity <- citiesStates[,1]
  metros$StAbbr <- citiesStates[,2]
  return(metros)
}

# Find Place FIPS Code of Principal City
fipsCorrelate <- function(metros) {
  print("Looking up FIPS Codes")
  fipsCodes <- read.csv("./Data/all-geocodes-v2017.csv", 
                        fileEncoding="UTF-8-BOM")
  stateFips <- fipsCodes %>% filter(Summary.Level == 40) %>% 
    select(State.Code..FIPS., 
           Area.Name..including.legal.statistical.area.description.) %>%
    rename(State = Area.Name..including.legal.statistical.area.description.) %>%
    rename(StateFIPS = State.Code..FIPS.)
  stAbbrs <- read.csv("./Data/state-abbrs.csv", fileEncoding="UTF-8-BOM")
  metros <- metros %>% left_join(stAbbrs, by="StAbbr") %>%
    left_join(stateFips, by="State")
  placeFips <- fipsCodes %>% filter(Summary.Level == 162) %>%
    select(State.Code..FIPS., Place.Code..FIPS., 
           Area.Name..including.legal.statistical.area.description.) %>%
    rename(StateFIPS = State.Code..FIPS.) %>%
    rename(PlaceFIPS = Place.Code..FIPS.) %>%
    rename(Place = Area.Name..including.legal.statistical.area.description.)
  # Puerto Rico Cities are Classified as Counties
  prFips <- fipsCodes %>% filter(State.Code..FIPS. == 72) %>%
    filter(Summary.Level == 50) %>%
    select(State.Code..FIPS., County.Code..FIPS., 
           Area.Name..including.legal.statistical.area.description.) %>%
    rename(StateFIPS = State.Code..FIPS.) %>%
    rename(PlaceFIPS = County.Code..FIPS.) %>%
    rename(Place = Area.Name..including.legal.statistical.area.description.)
  placeFips <- placeFips %>% bind_rows(prFips)
  metros <- metros %>% mutate(PlaceRe = paste("^", PrincipalCity, sep="")) %>%
    mutate(StateCodeRe = paste("^", paste(StateFIPS, "$", sep=""), sep=""))
  metros <- placeFips %>% 
    regex_right_join(metros, by=c(StateFIPS = "StateCodeRe", 
                                 Place = "PlaceRe")) %>%
    select(-StateCodeRe, -PlaceRe, -StateFIPS.x, -DATE, -Place) %>%
    rename(StateFIPS = StateFIPS.y) %>%
    filter(!duplicated(PrincipalCity))
  return(metros)
}

# Find latitude/longitude of principal city
latLongCorrelate <- function(metros) {
  print("Looking up Latitude/Longtiude")
  locations <- read.csv("./Data/NationalFedCodes_20181201.txt", sep="|")
  # Cities use county codes in PR
  prLocations <- locations %>% filter(STATE_NUMERIC == 72) %>%
    # Only needed variables
    select(CENSUS_CODE, COUNTY_NUMERIC, CENSUS_CLASS_CODE, STATE_NUMERIC,
           COUNTY_SEQUENCE, PRIMARY_LATITUDE, PRIMARY_LONGITUDE) %>%
    # Only cities
    filter(COUNTY_SEQUENCE == 1, CENSUS_CLASS_CODE == "U5") %>%
    # Remove unneeded variables
    select(-CENSUS_CLASS_CODE, -COUNTY_SEQUENCE) %>%
    # Rename county code to place code
    mutate(CENSUS_CODE = as.numeric(as.character(COUNTY_NUMERIC))) %>%
    # Unselect county code
    select(-COUNTY_NUMERIC)
  locations <- locations %>% 
    # Use only variables we need
    select(CENSUS_CODE, CENSUS_CLASS_CODE, STATE_NUMERIC, 
           COUNTY_SEQUENCE, PRIMARY_LATITUDE, PRIMARY_LONGITUDE) %>%
    # Only cities and the primary county they're in
    filter(COUNTY_SEQUENCE == 1, substr(CENSUS_CLASS_CODE,1,1) == "C") %>%
    # Remove unnecessary variables
    select(-CENSUS_CLASS_CODE,-COUNTY_SEQUENCE) %>%
    # Convert CENSUS_CODE to numeric
    mutate(CENSUS_CODE = as.numeric(as.character(CENSUS_CODE))) %>%
    bind_rows(prLocations)
  metros <- metros %>% left_join(locations, by=c("StateFIPS" = "STATE_NUMERIC",
                                               "PlaceFIPS" = "CENSUS_CODE")) %>%
    rename(MetroCode = `metropolitan statistical area/micropolitan statistical area`) %>%
    rename(MetroName = GEONAME, Latitude = PRIMARY_LATITUDE, 
           Longitude = PRIMARY_LONGITUDE, Population = POP)
  return(metros)
}

# Calculate a box around each city
calculateRegion <- function(metros, size) {
  size <- size / 2
  # 1 degree of latitude ~ 69 miles (only varies ~1% from equator to poles)
  print("Calculating box around principal city")
  metros <- metros %>% mutate(SouthBound = round(Latitude - (size / 69), 5), 
                            NorthBound = round(Latitude + (size / 69), 5)) %>%
    # 1 degree of longitude ~ 69 miles at equator
    # miles per degree of longitude = cos(latitude) * 69
    mutate(WestBound = round(Longitude - (size / (cos(Latitude * (pi/180)) * 69)), 5),
           EastBound = round(Longitude + (size / (cos(Latitude * (pi/180)) * 69)), 5))
  return(metros)
}