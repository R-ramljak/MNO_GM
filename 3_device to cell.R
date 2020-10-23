# device to cell

library(tidyverse)
library(sf)
library(furrr)

census.de.100m.tile <- readRDS("working objects/census.tile.final.rds")

census.de.100m.tile.1 <- census.de.100m.tile %>% 
  rename(pop.raw = pop) %>% 
  mutate(pop = case_when(pop.raw == "-1" | is.na(pop.raw) ~ as.integer(1),
                         # pop.raw == "-1" ~ sample(0:1, n(), replace = T),
                         pop.raw == 2 ~ sample(2:3, n(), replace = T),
                         TRUE ~ as.integer(pop.raw)))

coverage.areas <- readRDS("working objects/coverage.areas.rds")

coverage.example <- coverage.areas %>% 
  filter(area.kind %in% c("Rural", "Suburban")) %>% 
  st_transform(crs = 3035)
  # st_drop_geometry() %>%
  # st_as_sf(coords = "antenna.centroid", crs = 3035)

signal_strength <- Vectorize(function(distance, radius, max.equal = 0.7, min.threshold = 0.2) {
  sij.calc <- 1 - distance / radius

  if (sij.calc < min.threshold) { # min.threshold (nu)
    sij <- as.numeric(0)
  } else if (sij.calc > max.equal) { # maximum sij where it doesnt make a difference anymore if closer
    sij <- as.numeric(1)
  } else {
    sij <- sij.calc
  }

  return(sij)
})

# # Calculate the number of cores
no_cores <- availableCores() - 1
plan(multisession, workers = no_cores)



dev.to.cell <- census.de.100m.tile.1 %>% 
  mutate(tile.centroid = st_centroid(geometry)) %>%
  st_join(coverage.example, left = F) %>%
  st_transform(crs = 3035) %>% 
  # st_drop_geometry() %>% 
  st_sf(sf_column_name = "antenna.centroid", crs = 3035) %>% 
  mutate(dist.sij = as.numeric(st_distance(tile.centroid, antenna.centroid, by_element = T))) %>% 
  # mutate(signal.sij = case_when(1 - (dist.sij / 15000) < min.threshold ~ 0,
  #                               1 - (dist.sij / 15000) > max.equal ~ 1,
  #                               TRUE ~ 1 - (dist.sij / 15000))) %>% 
  mutate(signal.sij = signal_strength(dist.sij, coverage.radius)) %>% # implement relevant signal strength formula
  group_by(internal.id) %>%
  mutate(weight.pij = case_when(is.nan(as.numeric(signal.sij / sum(signal.sij, na.rm = T))) ~ as.numeric(0),
                                TRUE ~ as.numeric(signal.sij / sum(signal.sij, na.rm = T)))) 


dev.to.cell.1 <- census.de.100m.tile.1 %>% 
  mutate(tile.centroid = st_centroid(geometry)) %>%
  st_join(coverage.example, left = F) %>%
  st_transform(crs = 3035) %>% 
  # st_drop_geometry() %>% 
  st_sf(sf_column_name = "antenna.centroid", crs = 3035) %>% 
  group_by(internal.id) %>%
  group_split() %>% 
  future_map(~mutate(., dist.sij = as.numeric(st_distance(tile.centroid, antenna.centroid, by_element = T))), .progress = T) %>% 
  future_map(~mutate(., signal.sij = signal_strength(dist.sij, coverage.radius)), .progress = T) %>% # implement relevant signal strength formula
  future_map_dfr(~mutate(., weight.pij = case_when(is.nan(as.numeric(signal.sij / sum(signal.sij, na.rm = T))) ~ as.numeric(0),
                                TRUE ~ as.numeric(signal.sij / sum(signal.sij, na.rm = T)))), .progress = T) 

saveRDS(dev.to.cell, file = "working objects/dev.to.cell.rural.rds")

# sparse matrix of device to cell
P.mat <- dev.to.cell %>% 
  dplyr::select(internal.id, antenna.ID, weight.pij) %>% 
  st_drop_geometry() %>% 
  pivot_wider(id_cols = antenna.ID, names_from = internal.id, 
              values_from = weight.pij) %>% 
  as.matrix() %>% 
  Matrix(sparse = T)

saveRDS(P.mat, file = "working objects/P.mat.rds")

# u-vector
U.vec <- census.de.1km.tile %>% 
  dplyr::select(internal.id, pop)

saveRDS(U.vec, file = "working objects/U.vec.rds")

# C-vector (total count of mobile phones of a tile stochastically assigned to a radio cell based on P --> here: weight.pij)
C.vec <- dev.to.cell %>% 
  dplyr::select(internal.id, antenna.ID, pop, weight.pij) %>% 
  st_drop_geometry() %>%
  split(.$internal.id) %>% 
  map(~sample(x = .$antenna.ID, mean(.$pop),
              replace = T, prob = .$weight.pij)) %>% 
  map(as_tibble, .id = "internal.id") %>% 
  map(~group_by(., value)) %>% 
  map(~summarise(., pop.count.rand = n(), .groups = "drop"))

C.vec.1 <- dev.to.cell %>% 
  dplyr::select(internal.id, antenna.ID, pop, weight.pij) %>% 
  st_drop_geometry() %>%
  split(.$internal.id) %>% 
  future_map(~sample(., x = .$antenna.ID, mean(.$pop),
              replace = T, prob = .$weight.pij)) %>% 
  future_map(as_tibble, .id = "internal.id") %>% 
  future_map(~group_by(., value)) %>% 
  future_map(~summarise(., pop.count.rand = n(), .groups = "drop"))

saveRDS(C.vec, file = "working objects/C.vec.rds")