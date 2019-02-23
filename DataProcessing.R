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
  return(fromJSON(json))
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

# Correlate to FIPS Codes
fipsCodes <- read.csv("./Data/all-geocodes-v2017.csv", fileEncoding="UTF-8-BOM")
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
  select(-StateCodeRe, -PlaceRe, -StateFIPS.x, -DATE) %>%
  rename(StateFIPS = StateFIPS.y) %>%
  filter(!duplicated(PrincipalCity))