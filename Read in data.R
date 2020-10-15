
## Read in of raw census data

library(tidyverse)
library(data.table)
library(sf)
library(raster)
library(furrr)
library(stars)

census.raw <- fread("Data/Census data Germany/csv_Bevoelkerung_100m_Gitter/Zensus_Bevoelkerung_100m-Gitter.csv")
census.de.100m <- census.raw %>% 
  dplyr::select(x = x_mp_100m, y = y_mp_100m, pop = Einwohner) %>% 
  # filter(between(y, 2840000, 2850000), # 285
  #        between(x, 4400000, 4420000)) %>%
  filter(between(y, 2700000, 2900000), # 285
       between(x, 4400000, 4500000)) %>%
  mutate(internal.id = row_number())


census.de.100m.tile <- census.de.100m %>% 
  mutate(parts = ntile(internal.id, 50)) %>%
  group_by(parts) %>%
  group_split()

# Calculate the number of cores
no_cores <- availableCores() - 1
plan(multisession, workers = no_cores)

census.tile.final <- census.de.100m.tile %>% 
  future_map(~raster::rasterFromXYZ(., crs = st_crs(3035)$proj4string), .progress = T) %>% 
  future_map(~st_as_stars(.)) %>% 
  future_map_dfr(~st_as_sf(.), .progress = T) %>%
  st_transform(crs = 3035) %>%
  dplyr::select(-parts)

saveRDS(census.tile.final, file = "working objects/census.tile.final.rds")

# plot sample
census.tile.final %>% 
  sample_n(100000) %>% 
  ggplot() +
  geom_sf(aes(color = pop))
