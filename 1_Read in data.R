# MNO Chain 
## 1 Read in of raw census data

library(tidyverse)
library(data.table)
library(sf)
library(raster)
library(furrr)
library(stars)
library(osc)

# Raw census 100 m² data read in
census.raw <- fread("Data/Census data Germany/csv_Bevoelkerung_100m_Gitter/Zensus_Bevoelkerung_100m-Gitter.csv")

# Dataframe with bounding box, tile id, and two versions of the population variable
census.de.100m <- census.raw %>% 
  dplyr::select(x = x_mp_100m, y = y_mp_100m, pop.raw = Einwohner) %>% 
  # filter(between(y, 2840000, 2850000), # 285
  #        between(x, 4400000, 4420000)) %>%
  filter(between(y, 2700000, 2900000), # 285
       between(x, 4400000, 4500000)) %>%
  mutate(internal.id = row_number()) %>%
  mutate(pop = case_when(pop.raw == "-1" | is.na(pop.raw) ~ sample(0:1, n(), replace = T),
                         pop.raw == 2 ~ sample(2:3, n(), replace = T),
                         TRUE ~ as.integer(pop.raw))) %>% 
  mutate(pop.raster = case_when(pop < 70 ~ 0,
                                pop >= 70 ~ 1)) %>% 
  dplyr::select(-pop.raw)

saveRDS(census.de.100m, "working objects/example data frame.rds")

# Raster brick object of the complete bounding box region
census.tile <- raster::rasterFromXYZ(census.de.100m, crs = st_crs(3035)$proj4string)

# Raster layer object with specified dichotomized variable for cca
census.tile.pop.raster <- raster::raster(census.tile, layer = 3) 

# CCA workflow, clustering 1 values (pop > 70) and define s
cities <- cca(census.tile.pop.raster, cell.class = 1, s = 20000000, unit = "m") 

# Adding classification results to census sf object and splitting it up for further parallelized workflow
census.classified.sf <- cities[["cluster"]] %>% 
  right_join(census.de.100m, by = c("long" = "x", "lat" = "y")) %>% 
  mutate(parts = ntile(internal.id, 50)) %>%
  group_by(parts) %>%
  group_split()

# Calculate the number of cores
no_cores <- availableCores() - 1
plan(multisession, workers = no_cores)

# Rasterizing the sf object with cluster results and transforming back to final sf object
census.classified.final.sf <- census.classified.sf %>% 
  future_map(~raster::rasterFromXYZ(., crs = st_crs(3035)$proj4string), .progress = T) %>% 
  future_map(~st_as_stars(.), .progress = T) %>% 
  future_map_dfr(~st_as_sf(., coords = c("long", "lat")), .progress = T) %>%
  st_transform(crs = 3035) %>% 
  group_by(cluster_id) %>% 
  mutate(cluster.tile.n = n()) %>% 
  ungroup() %>% 
  mutate(area.kind = case_when(pop.raster != 0 & cluster.tile.n > 100 ~ "Urban", # cluster with at least 100 tiles is urban
                               pop.raster != 0 & cluster.tile.n > 50 & cluster.tile.n <= 100 ~ "Suburban", # cluster with at least fifty and below 100 tiles is suburban
                               TRUE ~ "Rural")) %>%  # Remaining tiles are considered as rural
  dplyr::select(-parts)


saveRDS(census.classified.final.sf, "working objects/census.tile.final.rds")


######################################################

### Excurse: Checking the results of the cca (adjustment of parameters)
result <- census.tile.pop.raster * 0
result[cellFromXY(result, cities$cluster[, 1:2])] <- cities$cluster[, 3]
result.cca <- brick(census.tile.pop.raster2, result)

# Number of clusters and how many tiles does each cluster entail
result.summary <- cities[["cluster"]] %>% 
  group_by(cluster_id) %>% 
  summarise(n())

# Classifying tiles (pop.raster == 1) and transforming to sf object, will be used to join below
result.sf <- st_as_stars(result) %>% 
  st_as_sf() %>%
  st_transform(crs = 3035) %>% 
  group_by(pop.raster) %>% 
  mutate(pop.raster.n = n()) %>% 
  ungroup() %>% 
  mutate(area.kind = case_when(pop.raster =! 0 & pop.raster.n > 100 ~ "Urban", # cluster with at least 100 tiles is urban
                               pop.raster =! 0 & pop.raster.n > 50 & pop.raster.n <= 100 ~ "Suburban", # cluster with at least fifty and below 100 tiles is suburban
                               TRUE ~ NA_character_))  # Remaining tiles are considered as rural

