# MNO Chain
## 2 Generating Radio cell network (layers and antennas; paramters adjustable)

library(tidyverse)
library(data.table)
library(sf)

# Initital object from file 1 Read In
census.de.100m.tile <- readRDS("working objects/census.tile.final.rds")

census.de.100m.tile.1 <- census.de.100m.tile %>% 
  mutate(pop.category = case_when(pop == "-1" ~ "Rural", 
                                  pop <= 20 ~ "Rural", 
                                  pop >= 21 & pop <= 55 ~ "Suburban", 
                                  pop >= 56 ~ "Urban"))
## Parameters

# Three region / layer types
type <- list("Rural" = c("Rural", "Suburban", "Urban"),
             "Suburban" = c("Suburban", "Urban"),
             "Urban" = c("Urban"))
layer.base <- list("Rural" = census.de.100m.tile.1, "Suburban" = census.de.100m.tile.1, "Urban" = census.de.100m.tile.1) %>% 
  map2(., type, ~filter(.x, pop.category %in% .y))


area.kind <- list("Rural" = "Rural", "Suburban" = "Suburban", "Urban" = "Urban")
cellsize <- list("Rural" = 35000, "Suburban" = 5000, "Urban" = 700) # relation to radius (qm)
rotation.degree <- list("Rural" = 0, "Suburban" = 35, "Urban" = 70)
jitter <- list("Rural" = 10000, "Suburban" = 1000, "Urban" = 300)
coverage.centroid.dist <- list("Rural" = 15000, "Suburban" = 2500, "Urban" = 500) # same as radius
coverage.radius <- c("Rural" = 15000, "Suburban" = 2500, "Urban" = 500)

# Focus area
bb.focus.vec <- c(xmin = 4400000, xmax = 4500000,
                  ymin = 2700000, ymax = 2900000)


# functions
rotation = function(a){
  r = a * pi / 180 #degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 

layer_network_generate = function(x, cellsize, rotation.degree){
  layer.geo <- x %>% 
    st_make_grid(cellsize = cellsize, square = F, flat_topped = T) %>%  # different cell size (qm)
    st_geometry()
  
  layer.centroid <- st_centroid(layer.geo)
  layer <- (layer.geo - layer.centroid) * rotation(rotation.degree) + layer.centroid # rotate by 35 degrees
  return(layer)
  
}


# Generate layers
layers <- pmap(list(layer.base, cellsize, rotation.degree), 
              ~layer_network_generate(x = ..1, cellsize = ..2, rotation.degree = ..3)) %>% 
  set_names(c("Layer.1", "Layer.2", "Layer.3"))

saveRDS(layers, file = "working objects/radio cell layers.rds")
  
# plot(layers[[1]])
# plot(layers[[2]], add = TRUE, col = 'red')
# plot(layers[[3]], add = TRUE, col = 'green')


# Generate 3 antennas per tower and coverage areas
coverage.areas <- layers %>%
  map2(., jitter, ~st_jitter(st_centroid(.x), .y)) %>%
  map(~st_coordinates(.)) %>%
  map(~as_tibble(.)) %>%
  map(~dplyr::select(., X.tow = X, Y.tow = Y)) %>% 
  map2(., c("RT", "ST", "UT"), ~mutate(.x, tower.ID = paste0(.y, 1:n()))) %>%
  map(~slice(., rep(1:n(), each = 3))) %>%
  map(~group_by(., tower.ID)) %>%
  map(~mutate(., antenna.ID = paste(tower.ID, "A", 1:3, sep = "."))) %>%
  map(~ungroup(.)) %>%
  map(~mutate(., antenna.kind = str_sub(antenna.ID, -1))) %>%
  map2(., coverage.centroid.dist, ~mutate(.x,
                                          X.ant = case_when(antenna.kind == "1" ~ X.tow - .y * 0,
                                                            antenna.kind == "2" ~ X.tow + .y * 0.77,
                                                            antenna.kind == "3" ~ X.tow - .y * 0.77),
                                          Y.ant = case_when(antenna.kind == "1" ~ Y.tow - .y * 1, # meter distance apart
                                                            antenna.kind == "2" ~ Y.tow + .y * 0.77,
                                                            antenna.kind == "3" ~ Y.tow + .y * 0.77))) %>%
  map(~st_as_sf(., coords = c("X.ant", "Y.ant"))) %>%
  map2(., coverage.radius, ~st_buffer(.x, .y)) %>% # radius coverage are per antenna
  map(~st_sf(., crs = 3035)) %>%
  map(~st_crop(., bb.focus.vec)) %>%
  map(~st_set_agr(., "aggregate")) %>% # clean up
  map(~mutate(., cell.centroid = st_centroid(geometry))) %>%
  map2_dfr(., area.kind, ~mutate(., area.kind = .y))

saveRDS(coverage.areas, file = "working objects/coverage.areas.rds")


coverage.areas %>%
  st_drop_geometry() %>%
  group_by(area.kind) %>%
  summarise(n.antenna = n())

table(census.de.100m.tile.1$pop.category)       



r <- coverage.areas %>% 
  filter(area.kind == "Suburban")

r %>% 
  ggplot() +
  geom_jitter(aes(X.tow, Y.tow)) +
  # geom_label(aes(X.tow, Y.tow, label = point.pos, fill = factor(L2)))
  geom_text(aes(X.tow, Y.tow, label = antenna.ID, fill = factor(tower.ID))) +
  geom_sf()

f <- coverage.areas %>% 
  filter(area.kind == "Rural") %>%
  group_by(point.pos) %>% 
  summarise(n())


# # Generate 3 antennas per tower and coverage areas
# coverage.areas <- layers %>%
#   map2(., jitter, ~st_jitter(.x, .y)) %>%
#   map(~st_coordinates(.)) %>%
#   map(~as_tibble(.)) %>%
#   map(~dplyr::select(., X.tow = X, Y.tow = Y, L2)) %>%
#   map(~group_by(., L2)) %>%
#   map(~mutate(., point.pos = row_number())) %>%
#   map(~filter(., point.pos %in% c(2, 3))) %>%
#   map(~ungroup(.)) %>%
#   map2(., c("RT", "ST", "UT"), ~mutate(.x, tower.ID = paste0(.y, 1:n()))) %>%
#   map(~slice(., rep(1:n(), each = 3))) %>%
#   map(~group_by(., tower.ID)) %>%
#   map(~mutate(., antenna.ID = paste(tower.ID, "A", 1:3, sep = "."))) %>%
#   map(~ungroup(.)) %>%
#   map(~mutate(., antenna.kind = str_sub(antenna.ID, -1))) %>%
#   map2(., coverage.centroid.dist, ~mutate(.x,
#                                           X.ant = case_when(antenna.kind == "1" ~ X.tow - .y * 0,
#                                                             antenna.kind == "2" ~ X.tow + .y * 0.77,
#                                                             antenna.kind == "3" ~ X.tow - .y * 0.77),
#                                           Y.ant = case_when(antenna.kind == "1" ~ Y.tow - .y * 1, # meter distance apart
#                                                             antenna.kind == "2" ~ Y.tow + .y * 0.77,
#                                                             antenna.kind == "3" ~ Y.tow + .y * 0.77))) %>%
#   map(~st_as_sf(., coords = c("X.ant", "Y.ant"))) %>%
#   map2(., coverage.radius, ~st_buffer(.x, .y)) %>% # radius coverage are per antenna
#   map(~st_sf(., crs = 3035)) %>%
#   map(~st_crop(., bb.focus.vec)) %>%
#   map(~st_set_agr(., "aggregate")) %>% # clean up
#   map(~mutate(., cell.centroid = st_centroid(geometry))) %>%
#   map2_dfr(., area.kind, ~mutate(., area.kind = .y))


hex <- layers[[1]][1]
jit <- st_jitter(st_centroid(hex), 10000)

plot(hex)
plot(jit, add = T)
  map2(., jitter, ~st_jitter(.x, .y)) %>%
  map(~st_coordinates(st_centroid(.)))
