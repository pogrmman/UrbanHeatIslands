library(stringr)
#install.packages("jsonlite")
#install.packages("httr")
#install.packages("dplyr")
#install.packages("fuzzyjoin")
library(jsonlite)
library(httr)
library(dplyr)
library(fuzzyjoin)

# Get data from the climate bureau API
getClimateData <- function(url) {
  authToken <- "XMsfwcICOajIgIeFdsYuYJnkxuWIrBKk"
  urlPrefix <- "https://www.ncdc.noaa.gov/cdo-web/api/v2/"
  url <- paste(urlPrefix, url, sep="")
  request <- GET(url, add_headers(token=authToken))
  json <- content(request, as="text")
  json <- fromJSON(json)
  # If it fails due to ratelimiting, try again after a second
  if (("status" %in% names(json)) & (json$status == "429")) {
    Sys.sleep(1)
    request <- GET(url, add_headers(token=authToken))
    json <- content(request, as="text")
    json <- fromJSON(json)
  }
  if ("results" %in% names(json)) {
    return(json$results)
  } else {
    return(json)
  }
}

# Get data from the census API
getCensusData <- function(url) {
  authToken <- "67671962d899413a75eed4bffb25e957458a750b"
  urlPrefix <- "https://api.census.gov/data/"
  urlPostfix <- paste("&key=", authToken, sep="")
  url <- paste(urlPrefix, url, urlPostfix, sep="")
  request <- GET(url)
  json <- content(request, as="text")
  return(fromJSON(json))
}

# Grab climate data of one type
getDataType <- function(data, type) {
  data <- data$results
  return(data[data$datatype == type,])
}

# Get population of all metro areas July 1, 2016
metroPops <- getCensusData("2016/pep/population?get=POP,GEONAME&for=metropolitan%20statistical%20area/micropolitan%20statistical%20area:*&DATE=9")
# Cleanup data
names <- metroPops[1,]
metroPops <- data.frame(metroPops[-1,])
colnames(metroPops) <- names
# Convert POP from char factor to number
metroPops$POP <- as.numeric(as.character(metroPops$POP))
# Sort in descending order by population
metroPops <- metroPops[order(metroPops$POP, decreasing=TRUE),]
# Get biggest 50
big50 <- metroPops[1:50,]

# Extract Principal City and State
pat <- "(.*)[:space:](Metro Area)$"
big50$GEONAME <- str_match(big50$GEONAME, pat)[,2]
# Assume principal city and state are first listed in name
pat <- "^([A-Z][a-z\\.]+(?:[/[:space:]](?:[A-Z][a-z\\.]+))*)[^,]*,[:space:]([A-Z]{2})"
citiesStates <- str_match(big50$GEONAME, pat)[,2:3]
big50$PrincipalCity <- citiesStates[,1]
big50$StAbbr <- citiesStates[,2]
rm(stAbbrs)

# Correlate to FIPS Codes
fipsCodes <- read.csv("./Data/all-geocodes-v2017.csv", 
                      fileEncoding="UTF-8-BOM")
stateFips <- fipsCodes %>% filter(Summary.Level == 40) %>% 
  select(State.Code..FIPS., 
         Area.Name..including.legal.statistical.area.description.) %>%
  rename(State = Area.Name..including.legal.statistical.area.description.) %>%
  rename(StateFIPS = State.Code..FIPS.)
stAbbrs <- read.csv("./Data/state-abbrs.csv", fileEncoding="UTF-8-BOM")
big50 <- big50 %>% left_join(stAbbrs, by="StAbbr") %>%
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
big50 <- big50 %>% mutate(PlaceRe = paste("^", PrincipalCity, sep="")) %>%
  mutate(StateCodeRe = paste("^", paste(StateFIPS, "$", sep=""), sep=""))
big50 <- placeFips %>% 
  regex_right_join(big50, by=c(StateFIPS = "StateCodeRe", 
                               Place = "PlaceRe")) %>%
  select(-StateCodeRe, -PlaceRe, -StateFIPS.x, -DATE, -Place) %>%
  rename(StateFIPS = StateFIPS.y) %>%
  filter(!duplicated(PrincipalCity))
rm(placeFips)
rm(prFips)
rm(fipsCodes)

# Correlate FIPS codes to Latitude/Longitude
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
big50 <- big50 %>% left_join(locations, by=c("StateFIPS" = "STATE_NUMERIC",
                                             "PlaceFIPS" = "CENSUS_CODE")) %>%
  rename(MetroCode = `metropolitan statistical area/micropolitan statistical area`) %>%
  rename(MetroName = GEONAME, Latitude = PRIMARY_LATITUDE, 
         Longitude = PRIMARY_LONGITUDE, Population = POP)
rm(locations)
rm(prLocations)

# Get box 20 miles on a side centered on each city
# 1 degree of latitude ~ 69 miles (only varies ~1% from equator to poles)
# 10 miles ~ .14493 degrees of latitude
big50 <- big50 %>% mutate(SouthBound = Latitude - .14493, 
                          NorthBound = Latitude + .14493) %>%
# 1 degree of longitude ~ 69 miles at equator
# miles per degree of longitude = cos(latitude) * 69
# 10 miles of longitude = 1/mpdl * 10
  mutate(WestBound = Longitude - (10 / (cos(Latitude * (pi/180)) * 69)),
         EastBound = Longitude + (10 / (cos(Latitude * (pi/180)) * 69)))

# Vectorize getClimateData function
getClimateDataVec <- Vectorize(getClimateData)
# Get lists of stations for each metro area
big50 <- big50 %>% 
  mutate(StationList = getClimateDataVec(paste("stations?extent=",
                                               SouthBound, WestBound,
                                               NorthBound, EastBound, sep="")))