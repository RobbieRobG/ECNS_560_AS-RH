#libraries
library(tidyverse)
library(sf)
library(tmap)
library(units)

# load data
counties = st_read('data/california_counties/CaliforniaCounties.shp')|>
  st_make_valid()
"C:\Users\coole\Documents\GitHub\ECNS_560_AS-RH\1. Raw Data\US Shapefiles\cb_2018_us_county_500k\cb_2018_us_county_500k.shp"