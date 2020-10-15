# MNO Chain
## 2 Generating Radio cell network (layers and antennas; paramters adjustable)

library(tidyverse)
library(data.table)
library(sf)

# Initital object from file 1 Read In
census.de.100m.tile <- readRDS("working objects/census.tile.final.rds")

census.de.100m.tile.1 <- census.de.100m.tile %>% 
  mutate(pop.category = ntile(pop, 3)) %>% 
  mutate(pop.category = case_when(pop.category == 1 ~ "Rural",
                                  pop.category == 2 ~ "Suburban",
                                  pop.category == 3 ~ "Urban")) # THESE CATEGORIES NEED TO BE ADJUSTED BASED ON VALID DEFINTIONS


## Parameters

# Three region / layer types
type <- list("Rural" = c("Rural", "Suburban", "Urban"),
             "Suburban" = c("Suburban", "Urban"),
             "Urban" = c("Urban"))
layer.base <- list("Rural" = census.de.100m.tile.1, "Suburban" = census.de.100m.tile.1, "Urban" = census.de.100m.tile.1) %>% 
  map2(., type, ~filter(.x, pop.category %in% .y))


area.kind <- list("Rural" = "Rural", "Suburban" = "Suburban", "Urban" = "Urban")
cellsize <- list("Rural" = 20000, "Suburban" = 5000, "Urban" = 1000)
rotation.degree <- list("Rural" = 0, "Suburban" = 35, "Urban" = 70)
jitter <- list("Rural" = 5000, "Suburban" = 1000, "Urban" = 300)
coverage.centroid.dist <- list("Rural" = 10000, "Suburban" = 2000, "Urban" = 300)
coverage.radius <- c("Rural" = 15000, "Suburban" = 2500, "Urban" = 500)

# Focus area
bb.focus.vec <- c(xmin = 4400000, xmax = 4500000,
                  ymin = 2800000, ymax = 2900000)


# functions
rotation = function(a){
  r = a * pi / 180 #degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 

layer_network_generate = function(x, cellsize, rotation.degree, jitter = T, jitter.amount = NULL){
  layer.geo <- x %>% 
    st_make_grid(cellsize = cellsize, square = F, flat_topped = T) %>%  # different cell size (qm)
    st_geometry()
  
  if (jitter == T){
  layer.geo <- layer.geo %>%
    st_jitter(jitter.amount)
  }
  
  layer.centroid <- st_centroid(layer.geo)
  layer <- (layer.geo - layer.centroid) * rotation(rotation.degree) + layer.centroid # rotate by 35 degrees
  return(layer)
  
}


# Generate layers
layers <- pmap(list(layer.base, cellsize, rotation.degree, jitter), 
              ~layer_network_generate(x = ..1, cellsize = ..2, rotation.degree = ..3, jitter.amount = ..4)) %>% 
  set_names(c("Layer.1", "Layer.2", "Layer.3"))

saveRDS(layers, file = "working objects/radio cell layers.rds")
  
# plot(layers[[1]])
# plot(layers[[2]], add = TRUE, col = 'red')
# plot(layers[[3]], add = TRUE, col = 'green')


# Generate 3 antennas per tower and coverage areas
coverage.areas <- layers %>% 
  map(~st_coordinates(.)) %>% 
  map(~as_tibble(.)) %>% 
  map(~dplyr::select(., X, Y)) %>%
  map(~distinct(.)) %>%  # remove duplicate corners 
  map2(., c("RT", "ST", "UT"), ~mutate(.x, tower.ID = paste0(.y, 1:n()))) %>% 
  map(~slice(., rep(1:n(), each = 3))) %>% 
  map(~group_by(., tower.ID)) %>% 
  map(~mutate(., antenna.ID = paste(tower.ID, "A", 1:3, sep = "."))) %>% 
  map(~ungroup(.)) %>% 
  map(~mutate(., antenna.kind = str_sub(antenna.ID, -1))) %>%
  map2(., coverage.centroid.dist, ~mutate(.x, 
                                          X = case_when(antenna.kind == "1" ~ X - .y * 0.71,
                                                        antenna.kind == "2" ~ X + .y * 0.71,
                                                        antenna.kind == "3" ~ X - .y * 0.71),
                                          Y = case_when(antenna.kind == "1" ~ Y - .y * 0.71, # meter distance apart
                                                        antenna.kind == "2" ~ Y + .y * 0.71,
                                                        antenna.kind == "3" ~ Y + .y * 0.71))) %>% 
  map(~st_as_sf(., coords = c("X", "Y"))) %>% 
  map2(., coverage.radius, ~st_buffer(.x, .y)) %>% # radius coverage are per antenna
  map(~st_sf(., crs = 3035)) %>% 
  map(~st_crop(., bb.focus.vec)) %>% 
  map(~st_set_agr(., "aggregate")) %>% # clean up
  map(~mutate(., cell.centroid = st_centroid(geometry))) %>% 
  map2_dfr(., area.kind, ~mutate(., area.kind = .y))

saveRDS(coverage.areas, file = "working objects/coverage.areas.rds")


# coverage.areas %>% 
#   st_drop_geometry() %>% 
#   group_by(area.kind) %>% 
#   summarise(n.antenna = n())
       
