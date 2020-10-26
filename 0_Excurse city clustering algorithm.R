# MNO Chain 
## 0 City clustering algorithm excurse

library(tidyverse)
library(data.table)
library(sf)
library(raster)
library(stars)
library(osc)

census.raw <- fread("Data/Census data Germany/csv_Bevoelkerung_100m_Gitter/Zensus_Bevoelkerung_100m-Gitter.csv")

# Dataframe with bounding box, tile id, and two versions of the population variable
census.de.100m <- census.raw %>% 
  dplyr::select(x = x_mp_100m, y = y_mp_100m, pop.raw = Einwohner) %>% 
  filter(between(y, 2700000, 2900000), # 285
         between(x, 4400000, 4500000)) %>%
  mutate(internal.id = row_number()) %>%
  mutate(pop = case_when(pop.raw == "-1" | is.na(pop.raw) ~ sample(0:1, n(), replace = T),
                         pop.raw == 2 ~ sample(2:3, n(), replace = T),
                         TRUE ~ as.integer(pop.raw))) %>% 
  mutate(pop.raster = case_when(pop < 70 ~ 0,
                                pop >= 70 ~ 1)) %>%  # defining the pop threshold per tile
  dplyr::select(-pop.raw)


# creating the raster layer object with dichotomized pop.raster as focal variable 
census.de.100m.tile <- census.de.100m %>% 
  raster::rasterFromXYZ(crs = st_crs(3035)$proj4string) %>% 
  raster(layer = 3)

# Performing cca workflows
cities <- cca(census.de.100m.tile, cell.class = 1, s = 20000000, unit = "m")
result <- census.de.100m.tile*NA
result[cellFromXY(result, cities$cluster[,1:2])] <- cities$cluster[,3]

# Calculating the size of each cluster (how many tiles per cluster)
result.summary <- cities[["cluster"]] %>% 
  group_by(cluster_id) %>% 
  summarise(n())

# sf dataframe version of the clustered raster object and defining the area kind of all tiles which were introduced into the algorithm (pop.raster = 1)
result.sf <- st_as_stars(result) %>% 
  st_as_sf() %>%
  st_transform(crs = 3035) %>% 
  group_by(pop.raster) %>% 
  mutate(pop.raster.n = n()) %>% 
  ungroup() %>% 
  mutate(area.kind = case_when(pop.raster.n > 100 ~ "Urban", # more than 100 tiles equals to urban
                               pop.raster.n > 50 & pop.raster.n <= 100 ~ "Suburban", # between fifty and 100 equals to suburban
                               TRUE ~ "Rural")) # remaining are rural

# This plot is interesting as it shows all tiles that were introduced into the cca so having at least 70 people per tile. One can see a lot of towns and even some realistic suburban areas around urban areas
result.sf %>% 
  ggplot() + 
  geom_sf(aes(col = factor(area.kind)))

### Tony will look into the possibility to have more control over suburban tile definition, focusing on the outer parts of urban areas.









