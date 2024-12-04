#libraries
library(tidyverse)
library(sf)
library(tmap)
library(units)
library(viridis)
library(magick)
library(purrr)

# load data
counties = st_read("1. Raw Data/US Shapefiles/cb_2018_us_county_20m/cb_2018_us_county_20m.shp")|>
  st_make_valid()|>
  # remove us territories and outlying regions
  mutate(STATEFP = as.numeric(STATEFP)) |>
  filter(STATEFP <= 56)|>
  filter(!STATEFP %in% c(2, 15)) |>
  mutate (FIPS = paste0(sprintf("%02d", STATEFP), sprintf("%03d", as.numeric(COUNTYFP))))
  
tmap_mode("plot")
#qtm(counties)


# load in population data and create retirement age proportion
popdata = readRDS("2. Merged Data/Intermediate Cleaning Datasets/popdata_00_23_clean.rds")
popdata = popdata|>
  mutate(RETIREMENTAGE_PROP = rowSums(
    cbind(AGE6569_PROP, AGE7074_PROP, AGE7579_PROP, AGE8084_PROP, AGE85PLUS_PROP)))|>
  mutate(RETIREMENTAGE_PROP = RETIREMENTAGE_PROP * 100)

# join counties with population data by FIPS
counties_pop = counties |>
  left_join(popdata, by = "FIPS")

# create consistant legend between heatmap years
breaks = c(0, 7, 14, 21, 28, 35, 100)
break_labels = c("0–7", "7–14", "14–21", "21–28", "28–35", "35+")


# TESTING HEATMAP CREATION ----------------------
# generate heatmap for each year
heatmaps = unique(popdata$YEAR) |>
  sort() |>
  map(function(year) {
    counties_year <- counties_pop |>
      filter(YEAR == !!year) # Ensure YEAR exists in counties_pop

    tm_shape(counties_year) +
      tm_polygons("RETIREMENTAGE_PROP",
                  title = "Retirement Age\nPopulation Density(%)",
                  palette = "plasma",
                  breaks = breaks,
                  legend.labels = break_labels,
                  border.alpha = .1,
                  lwd = 0.1) +
      tm_layout(
        title = paste("Population Heatmap:", year),
        title.position = c("center", "top"),
        title.size = 1.5,
        title.color = "black",
        legend.text.color = "black",
        legend.title.color = "black",
        title.fontface = "bold",
        legend.title.size = .9,
        legend.title.fontface = "bold",
        legend.text.size = 0.7,
        legend.text.fontface = "bold",
        legend.position = c("left", "bottom"),
        legend.format = list(text.align = "center"),
        legend.bg.color = "white",
        legend.bg.alpha = 0.9,
        legend.frame = TRUE,
        inner.margins = c(0.16, 0.05, 0.14, 0.05)
      )
  })


# save heatmaps

heatmaps_save = "Visualizations/population_heatmap_images/"
if (!dir.exists(heatmaps_save)) dir.create(heatmaps_save)

walk2(heatmaps, unique(popdata$YEAR), function(heatmap, year) {
  file_name = paste0(heatmaps_save, "heatmap_", year, ".png")
  tmap_save(heatmap, filename = file_name, width = 2400, height = 1800)
})



# HEATMAP ANIMATION THROUGH YEARS FAILED------------------
# Combine data for all years into a single dataset
all_years_map = counties_pop |>
  filter(YEAR %in% unique(popdata$YEAR)) # Ensure YEAR exists

# # Create the animation
# tmap_animation(
#   tm_shape(all_years_map) +
#     tm_polygons(
#       "RETIREMENTAGE_PROP",
#       title = "Retirement Age\nPopulation Density (%)",
#       palette = "plasma",
#       breaks = breaks,
#       legend.labels = break_labels,
#       border.alpha = 0.1,
#       lwd = 0.1
#     ) +
#     # tm_credits(
#     #   text = "Year: {YEAR}",            # Display the year dynamically
#     #   position = c("right", "top"),     # Position in the top-right corner
#     #   size = 1.5,                       # Text size
#     #   fontface = "bold",                # Bold font
#     #   col = "black"                     # Text color
#     # ) +
#     tm_layout(
#       title = paste0("Population Heatmap - Year:{YEAR}"),          # Static title for the animation
#       title.position = c("center", "top"),
#       title.size = 1.5,
#       title.color = "black",
#       title.fontface = "bold",
#       legend.text.color = "black",
#       legend.title.color = "black",
#       legend.title.size = 0.9,
#       legend.title.fontface = "bold",
#       legend.text.size = 0.7,
#       legend.text.fontface = "bold",
#       legend.position = c("left", "bottom"),
#       legend.format = list(text.align = "center"),
#       legend.bg.color = "white",
#       legend.bg.alpha = 0.9,
#       legend.frame = TRUE,
#       inner.margins = c(0.16, 0.05, 0.14, 0.05)
#     ),
#   filename = "Visualizations/population_heatmap_animation.gif", # Output file path
#   delay = 800, # Time between frames (milliseconds)
#   width = 800, # Width of the animation
#   height = 600, # Height of the animation
#   frames = length(unique(all_years_map$YEAR)), # Ensure one frame per year
#   loop = TRUE  # Enable looping
# )
# 
# 
# head(all_years_map)
# unique(all_years_map$YEAR)


# HEATMAP ANIMATION THROUGH YEARS 2nd ATTEMPT------------------
  # Path to heatmap images
image_files = list.files(heatmaps_save, pattern = "heatmap_.*\\.png", full.names = TRUE)
  # Read and combine images into a GIF
heatmap_gif = image_read(image_files) |>
  image_animate(fps = 1) |>
  image_write("Visualizations/population_heatmap_animation.gif")
