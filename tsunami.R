#https://geoportal.hawaii.gov/datasets/tsunami-evacuation-zones/explore?showTable=true

library(tidyverse)
library(sf)
library(here)
library(mapgl)
library(tidycensus)
#library(rsocrata)

evac_zones <- st_read(here("Tsunami_Evacuation_Zones.geojson"))



# 2) Simple style (semi-transparent fill + outline)
fill_col <- "#ef4444"# red
line_col <- "#7f1d1d"

mapboxgl(bounds = evac_zones) |>
  add_fill_layer(
    id = "tsunami_evac",
    source = evac_zones,
    fill_color = fill_col,
    fill_opacity = 0.55,
    tooltip = "mapname"
  ) |>
  add_line_layer(
    id = "tsunami_evac",
    source = evac_zones,
    line_color = line_col,
    line_width = 1
  ) |>
  add_legend(
    legend_title = "Tsunami Evacuation Zones",
    values = "Evacuation Area",
    colors = fill_col,
    type = "categorical"
  )



# Question 1: How many hawaii residents live in evacuation zones

# --- 1) Get ACS population with geometry (block groups) ---
bg <- get_acs(
  geography = "block group",
  variables = "B01003_001",   # total population
  state = "HI",
  year = 2023, survey = "acs5",
  geometry = TRUE, cache_table = TRUE
) |>
  st_transform(4326) |>
  st_make_valid() |>
  rename(pop = estimate) |>
  select(GEOID, pop)



block_groups <- bg |>
  mutate(blockgroup_area = st_area(geometry))

evac_zones <- evac_zones |>
  st_transform(4326) |>
  st_make_valid() |>
  mutate(evac_id = dplyr::row_number())



evac_zones <- evac_zones |>
  select(evac_id, island, zone_type, zone_desc)


intersections <- st_intersection(block_groups, evac_zones)


intersections <- intersections |>
  mutate(overlap_area = st_area(geometry))


intersections <- intersections |>
  mutate(weight = as.numeric(overlap_area / blockgroup_area))


intersections <- intersections |>
  mutate(pop_in_zone = pop * weight)


sum(intersections$pop_in_zone)


## Challenge 
# Use the Tsunami_Evacuation_All_Zones.geojson (https://geoportal.hawaii.gov/datasets/437b77f132ed416294b45bde6aef23f4_11/explore?location=20.546870%2C-157.491600%2C7.83) 
# 1. Using mapgl, make a map of colored zone types

all_evac_zones <- st_read(here("Tsunami_Evacuation_-_All_Zones.geojson"))

mapboxgl(bounds = all_evac_zones) |>
  add_fill_layer(
    id = "tsunami_evac_all",
    source = all_evac_zones,
    fill_color = match_expr(
      column = "zone_type",
      values = c("Tsunami Safe Zone", "Extreme Tsunami Evacuation Zone", "Tsunami Evacuation Zone"),
      stops = c("#1a9850", "#fee08b", "#d73027")
    ),
    fill_opacity = 0.55,
    tooltip = "zone_type"
  ) |>
  add_legend(
    legend_title = "Tsunami Evacuation Zones",
    values = c("Tsunami Safe Zone", "Extreme Tsunami Evacuation Zone", "Tsunami Evacuation Zone"),
    colors = map_colors,
    type = "categorical"
  )



# 2. Can you tell me how many people are in Tsunami Safe Zones vs Tsunami Evacuation Zones vs Extreme Tsunami Evacuation Zones

#split into 3 df

safe <- all_evac_zones[all_evac_zones$zone_type == "Tsunami Safe Zone", ]
extreme <- all_evac_zones[all_evac_zones$zone_type == "Extreme Tsunami Evacuation Zone", ]
evacuation <- all_evac_zones[all_evac_zones$zone_type == "Tsunami Evacuation Zone", ]

# safe 
safe <- safe |>
  st_transform(4326) |>
  st_make_valid() |>
  mutate(evac_id = dplyr::row_number())

safe <- safe |>
  select(objectid, island, zone_type, zone_desc) 

intersections <- st_intersection(block_groups, safe)

intersections <- intersections |>
  mutate(overlap_area = st_area(geometry))

intersections <- intersections |>
  mutate(weight = as.numeric(overlap_area / blockgroup_area))

intersections <- intersections |>
  mutate(pop_in_zone = pop * weight)

sum(intersections$pop_in_zone)

# 1124864 people


#extreme
extreme <- extreme |>
  st_transform(4326) |>
  st_make_valid() |>
  mutate(evac_id = dplyr::row_number())

extreme <- extreme |>
  select(objectid, island, zone_type, zone_desc) 

intersections <- st_intersection(block_groups, extreme)

intersections <- intersections |>
  mutate(overlap_area = st_area(geometry))

intersections <- intersections |>
  mutate(weight = as.numeric(overlap_area / blockgroup_area))

intersections <- intersections |>
  mutate(pop_in_zone = pop * weight)

sum(intersections$pop_in_zone)

# 174554.6 people


#evacuation
evacuation <- evacuation |>
  st_transform(4326) |>
  st_make_valid() |>
  mutate(evac_id = dplyr::row_number())

evacuation <- evacuation |>
  select(objectid, island, zone_type, zone_desc) 

intersections <- st_intersection(block_groups, evacuation)

intersections <- intersections |>
  mutate(overlap_area = st_area(geometry))

intersections <- intersections |>
  mutate(weight = as.numeric(overlap_area / blockgroup_area))

intersections <- intersections |>
  mutate(pop_in_zone = pop * weight)

sum(intersections$pop_in_zone)

# 130617.1 people



# 3. What is the key assumption our analysis makes? Is this acceptable? How can it be improved?

# This assumes that the population within the census tracts are evenly distributed geometrically.
