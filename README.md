# UrbanHeatIslands
Observing the urban heat island effect with R

## Introduction
Cities tend to be warmer than surrounding rural areas due to development and waste heat.
This is known an urban heat island. It can result in significantly warmer temperatures within 
cities. The difference can be quite large -- [one study measured Montreal as approximately 7.5 
degrees Celsius warmer than it's surroundings.](https://www.sciencedirect.com/science/article/pii/0004698173901406)

As might be expected, larger cities tend to have larger heat islands. The goal of this
is to explore how heat islands have changed as cities have grown by pulling together data 
from the US Climate Bureau and the US Census Bureau for some of the larger metro areas within
the United States from 1965 until 2015. By looking at climate data recorded both close to the
center of each metro area and futher away from each metro area, I'll see how the physical size
of the urban heat island has changed as well as its magnitude.

## Data File Descriptions and Sources
- `NationalFedCodes_20181201.txt` -- A pipe-seperated file of all GNIS-known places that formerly 
had a FIPS code assigned to them. Because FIPS *place* codes have been depreciated for all uses other than
the census, the [US Board on Geographic Names](https://geonames.usgs.gov/domestic/index.html) provides 
this file to help users convert to the newer system. Fortunately, this has the geographic locations builtin,
making it easy to convert from the Census Bureau FIPS codes to latitudes and longitudes without using GIS servers.
- `all-geocodes-v2017.csv` -- A comma-seperated file of all the Census Bureau FIPS codes, including places. Converted
from [excel files available on the US Census Bureau's website.](https://www.census.gov/geographies/reference-files/2017/demo/popest/2017-fips.html)
- `state-abbrs.csv` -- A comma-seperated file of states, territories, and their postal abbreviations. 
[From the US Postal Service.](https://pe.usps.com/text/pub28/28apb.htm)
