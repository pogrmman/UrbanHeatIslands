### Packages ###
library(dplyr)
### Project Functions ###
source("./APICalls.R", local=TRUE)

getCensusPopulation <- function(metros, year) {
  if (year == "1990") {
    varname <- "P0010001"
  } else {
    varname <- "P001001"
  }
  if (year == "2010") {
    baseUrl <- paste(year, "/dec/", sep="")
  } else {
    baseUrl <- paste(year, "/", sep="")
  }
  baseUrl <- paste(baseUrl, "sf1?get=", varname, "&for=county:", sep="")
  columnCalled <- paste("Population", year, sep="")
  metros <- metros %>% 
    mutate(!!columnCalled := mapply(censusPop, FIPS.County.Code, 
                                    FIPS.State.Code, baseUrl))
  return(metros)
}