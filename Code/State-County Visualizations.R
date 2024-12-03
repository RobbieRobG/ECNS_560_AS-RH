#libraries
library(tidyverse)
library(sf)
library(tmap)
library(units)

# load data
counties = st_read("1. Raw Data/US Shapefiles/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")|>
  st_make_valid()|>
  # remove us territories and outlying regions
  mutate(STATEFP = as.numeric(STATEFP)) |>
  filter(STATEFP <= 56)|>
  mutate 
  
tmap_mode('view')
qtm(counties)


# load in population data
popdata = readRDS("2. Merged Data/Intermediate Cleaning Datasets/popdata_00_23_clean.rds")