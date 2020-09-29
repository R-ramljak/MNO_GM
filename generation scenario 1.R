library(tidyverse)
library(data.table)
library(sf)
library(lubridate)
library(raster)

set.seed(762)

census.raw <- read_csv2("Data/Census data Germany/Zensus 1km.csv")
census.de.1km <- census.raw %>% 
  dplyr::select(x = x_mp_1km, y = y_mp_1km, tile.id = Gitter_ID_1km, pop = Einwohner) %>% 
  mutate(internal.id = row_number()) %>% 
  mutate(pop = case_when(pop == 1 ~ sample(1:250, n(), replace = T),
                         pop == 2 ~ sample(250:500, n(), replace = T),
                         pop == 3 ~ sample(500:2000, n(), replace = T),
                         pop == 4 ~ sample(2000:4000, n(), replace = T),
                         pop == 5 ~ sample(4000:8000, n(), replace = T),
                         pop == 6 ~ sample(8000:12000, n(), replace = T),
                         pop == "-1" ~ as.integer(1)))

census.de.1km.tile <- census.de.1km %>% 
  filter(between(y, 2800000, 2850000),
         between(x, 4500000, 4600000)) %>%
  # filter(between(y, 2600000, 2850000),
  #        between(x, 4300000, 4600000)) %>% 
  rasterFromXYZ(crs = st_crs(3035)$proj4string) %>% 
  rasterToPolygons() %>%
  st_as_sf() %>% 
  st_transform(crs = 3035) 


# Bounding box of focus area
focus.bb <- data.frame(maxlat = 4600000, minlat = 4300000,
                       maxlong = 2850000, minlong = 2650000)

# Focusing on the area within the bounding box (south bavaria)
census.de.1km %>% 
  sample_n(10000) %>% 
  ggplot() +
  geom_point(aes(x = x, y = y)) +
  geom_rect(data = focus.bb, aes(ymin = minlong, ymax = maxlong, 
                                 xmin = minlat, xmax = maxlat), 
            color = "red", fill = "transparent")
  

# Focus area
census.de.1km.tile %>% 
  ggplot() +
  geom_sf(aes(fill = pop))

# Histograms of population distribution
census.de.1km.tile %>% 
  ggplot() +
  geom_histogram(aes(pop))


# pop represents u in the paper

# define illumnation intensity sij for each tile 


## Generation of radio network ###
# Real data concerning cell towers in Germany
cell.tower.hilzen.raw <- read.csv("https://raw.githubusercontent.com/dahilzen/Mobilfunk-Scrape/master/Alle_Funkmasten_BRD.csv")
cell.tower.position <- cell.tower.hilzen.raw %>% 
  rename(tower.id = fID) %>% 
  filter(type == "Mobilfunk") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326)$proj4string) %>% 
  st_transform(crs = 3035) %>% 
  st_crop(xmin = 4500000, xmax = 4600000,
          ymin = 2800000, ymax = 2850000) # filtering based on boundary box


# mapping cell towers on census map
census.de.1km.tile %>% 
  ggplot() +
  geom_sf(aes(fill = pop)) +
  geom_sf(data = cell.tower.position, col = "red", size = 0.1)
# one can see that cell towers cluster, where it is more urban


# Defining the coverage area
# TO DO: create multiple cells per tower
radio.cells <- cell.tower.position %>% 
  # slice(rep(1:n(), each = 3)) %>% 
  mutate(universal.id = paste0("RC.", row_number())) # %>% 
  # group_by(tower.id) %>% 
  # mutate(tower.cell.id = paste0("T.RC.", row_number()))


# Option with Voronoi tesselation
# Coverage area is dataframe with radio cells in the rows and their geometries constituting their respective coverage area
# random.cell.vec <- sample(nrow(radio.cells))
# TO DO: create overlapping coverage areas
# TO DO: Implement signaling strength, currently uniform
coverage.areas <- radio.cells %>% 
  st_union() %>% # unite them
  st_voronoi() %>% # and perform the voronoi tessellation
  st_collection_extract(type = "POLYGON") %>% # select the polygons
  st_sf(crs = 3035) %>% 
  st_crop(xmin = 4500000, xmax = 4600000,
          ymin = 2800000, ymax = 2850000) %>% 
  st_join(radio.cells) %>% # & re-connect the data items
  st_set_agr("aggregate") %>%  # clean up
  mutate(signal.str = 1) # uniform
  
# Overlaying the coverage areas with the census data (Device to cell association)
coverage.areas %>% 
  ggplot() +
  geom_sf(data = census.de.1km.tile, aes(fill = pop)) +
  geom_sf(col = 'red', fill = NA) +
  ggtitle("Device-to-cell association", "Non-overlapping Voronoi Tesselation")
  
# Matrix representation of Device to cell association
  