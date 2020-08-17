library(tidyverse)
library(data.table)
library(sf)
library(lubridate)
library(raster)


# census.pop.de.raw <- fread("Data/Census data Germany/csv_Bevoelkerung_100m_Gitter/Zensus_Bevoelkerung_100m-Gitter.csv")
# # census.dem.de.raw <- fread("Data/Census data Germany/csv_Demographie_100m_Gitter/Bevoelkerung100M.csv", nrows = 1000000)
# 
# 
# pop.var.vec <- c("tile.100m", "x.mp.100m", "y.mp.100m", "pop")
# 
# 
# census.pop <- census.pop.de.raw %>% 
#   sample_n(100) %>% 
#   set_names(pop.var.vec) %>% 
#   mutate(tile.100m = as.character(tile.100m)) %>% 
#   mutate(pop.random = case_when(pop %in% c("-1", 0:10) ~ 1, # marking pop values which are random numbers
#                                 TRUE ~ 0)) %>%
#   mutate(pop = case_when(pop.random == 1 ~ sample(1:10, n(), replace = T), # random numbers for undisclosed pop values
#                          TRUE ~ pop)) %>%
#   # st_as_sf(coords = c("x.mp.100m", "y.mp.100m"), crs = st_crs(3035)$proj4string)
#   dplyr::select(x.mp.100m, y.mp.100m, everything()) %>% 
#   rasterFromXYZ(crs = st_crs(3035)$proj4string)


# download.file("https://tinyurl.com/ybtpkwxz", 
#               destfile = "census.zip", mode = "wb")
# unzip("census.zip") # unzip the files
census.de.1km.raw <- readr::read_csv2(list.files(pattern = "Gitter.csv"))
census.de.1km <- census.de.1km.raw %>% 
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
  rasterFromXYZ(crs = st_crs(3035)$proj4string) %>% 
  rasterToPolygons() %>%
  st_as_sf() %>% 
  st_transform(crs = 3035) 
# clump tiles together based on MSAs


# It should however be noted that the reliability of grid values of about 10 or less, as shown, is limited. 
# This is due to the risk of relative differences of over 50% occurring between the published values and the original values; 
# this risk is caused by the above-mentioned data perturbation method applied to ensure statistical confidentiality in accordance 
# with Article 16 of the Federal Statistics Law.
# Due to statistical confidentiality procedures, differences in totals may occur. 
# The total population (inhabitants) is not changed by the statistical confidentiality procedure. 
# Only grid cells containing one person are classified in the same way as grid cells without any persons, 
# and grid cells with two persons are treated like grid cells with three persons.
# Grid cells for which no persons are shown are not contained in the file. The fact that no persons are 
# shown for a specific grid cell may be due to the confidentiality procedure applied.


# bavaria: 10E-14E und 47N-50N

census.pop.by <- census.pop.de.raw %>% 
  filter()

census.pop %>% 
  ggplot() +
  geom_sf()


d <- census.pop.de.raw %>% 
  set_names(pop.var.vec) %>% 
  filter(!pop == "-1") %>% 
  ggplot() +
  geom_histogram(aes(pop))

# pop represents u in the paper

# define illumnation intensity sij for each tile 


census.pop.raster <- census.pop %>% 
  mutate(lat = sf::st_coordinates(.)[,1],
         lon = sf::st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  dplyr::select(lat, lon) %>% 
  rasterFromXYZ(crs = st_crs(3035)$proj4string)


# read in of mno data

# A cell is identified with four different values: mcc, net, area, cell.
# These tell us which country, which cellular network provider, the area within that country, 
# and which ID belongs to the cell, respectively.

# mno.raw <- fread("Data/Cell data/262.csv/262.csv")
# mno <- mno.raw %>% 
#   mutate(unique.id = paste0(mcc, net, area, cell, collapse = "-")) %>% 
#   filter(samples > 99) %>% 
#   mutate(cell.id = as.character(cell)) %>% 
#   mutate(c_date = floor_date(as_datetime(created), "day"), # antenna creation date
#          u_date = floor_date(as_datetime(updated), "day"))  # antenna last update date



# with this file we assume that row describes an antenna
cell.tower.hilzen.raw <- read.csv("https://raw.githubusercontent.com/dahilzen/Mobilfunk-Scrape/master/Alle_Funkmasten_BRD.csv")
cell.tower <- cell.tower.hilzen.raw %>% 
  filter(type == "Mobilfunk") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326)$proj4string) %>% 
  st_transform(crs = 3035)

cell.tower %>% 
  st_sample(100) %>% 
  ggplot() +
  geom_sf()


# Check in which tile a cell tower is located --> build in coverage radius
sji.df <- st_join(census.de.1km.tile, cell.tower, join = st_intersects)

sij.df.join <- sji.df %>% 
  dplyr::select(internal.id, cell.tower.id = fID, pop, pop.random) %>% 
  st_drop_geometry() %>% 
  left_join(census.de.1km, by = "internal.id") %>% 
  dplyr::select(tile.id = Gitter_ID_1km, cell.tower.id = fID, pop) %>% 
  pivot_wider(id_cols = tile.id, )
  

