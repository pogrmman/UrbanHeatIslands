### Packages ###
library(dplyr)

getCensusPopulation <- function(metros) {
  censusPops <- read.csv("./Data/nhgis0001_csv/nhgis0001_ts_nominal_county.csv")
  censusPops <- select(censusPops, STATE, COUNTY, STATEFP, COUNTYFP, 
                       26, 27, 28, 29, 30)
  names(censusPops) <- c("State", "County", "StateFIPS", "CountyFIPS", 
                         "Population1970", "Population1980", "Population1990", 
                         "Population2000", "Population2010")
  # Miami-Dade Co, FL changed its name and FIPS code
  metros <- metros %>% mutate(RealCountyCode = FIPS.County.Code)
  dadeCounty <- metros %>% 
    filter(FIPS.State.Code == 12 & FIPS.County.Code == 86) %>%
    mutate(FIPS.County.Code = 25)
  metros <- metros %>% bind_rows(dadeCounty) %>%
    # Remove Broomfield Co, CO -- it wasn't a county until 2001
    filter(!(FIPS.State.Code == 8 & FIPS.County.Code == 14)) %>%
    left_join(censusPops, by=c(FIPS.State.Code = "StateFIPS",
                               FIPS.County.Code = "CountyFIPS"))
  # Consolidate Miami-Dade Co and Dade Co
  dadeCounty <- metros %>% 
    filter(RealCountyCode == 86 & FIPS.State.Code == 12) %>%
    mutate(FIPS.County.Code = RealCountyCode) %>%
    mutate(County = "Miami-Dade County") %>%
    group_by(MetroName, MetroCode, FIPS.State.Code, 
             FIPS.County.Code, RealCountyCode, State, County) %>% 
    summarize_all(funs(.[!is.na(.)]))
  metros <- metros %>% 
    filter(!(RealCountyCode == 86 & FIPS.State.Code == 12)) %>%
    bind_rows(dadeCounty) %>% select(-RealCountyCode) %>%
    group_by(MetroName) %>% 
    summarize(Population1970 = sum(Population1970, na.rm=TRUE), 
              Population1980 = sum(Population1980, na.rm=TRUE), 
              Population1990 = sum(Population1990, na.rm=TRUE), 
              Population2000 = sum(Population2000, na.rm=TRUE), 
              Population2010 = sum(Population2010, na.rm=TRUE))
  return(metros)
}