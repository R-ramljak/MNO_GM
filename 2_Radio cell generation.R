# MNO Chain
## 2 Generating Radio cell network (layers and antennas; paramters adjustable)

library(tidyverse)
library(sf)

# Initital object from file 1 Read In
census.de.100m.tile.1 <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/census.tile.final.rds")

## Parameters

# Three region / layer types
type <- list("Rural" = c("Rural", "Suburban", "Urban"),
             "Suburban" = c("Suburban", "Urban"),
             "Urban" = c("Urban"))
layer.base <- list("Rural" = census.de.100m.tile.1, "Suburban" = census.de.100m.tile.1, "Urban" = census.de.100m.tile.1) %>% 
  map2(., type, ~filter(.x, pop.area.kind %in% .y))


area.kind <- list("Rural" = "Rural", "Suburban" = "Suburban", "Urban" = "Urban")
tower.dist <- list("Rural" = 35000, "Suburban" = 8000, "Urban" = 1000) # relation to radius (qm)
rotation.degree <- list("Rural" = 0, "Suburban" = 35, "Urban" = 70)
jitter <- list("Rural" = 5000, "Suburban" = 1000, "Urban" = 400)
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

layer_network_generate = function(x, tower.dist, rotation.degree){
  layer.geo <- x %>% 
    st_make_grid(cellsize = tower.dist, square = F, flat_topped = T) %>%  # different cell size (qm)
    st_geometry()
  
  layer.centroid <- st_centroid(layer.geo)
  layer <- (layer.geo - layer.centroid) * rotation(rotation.degree) + layer.centroid # rotate by 35 degrees
  return(layer)
  
}


# Generate layers
layers <- pmap(list(layer.base, tower.dist, rotation.degree), 
              ~layer_network_generate(x = ..1, tower.dist = ..2, rotation.degree = ..3)) %>% 
  set_names(c("Layer.1", "Layer.2", "Layer.3"))

saveRDS(layers, file = "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/radio cell layers.rds")
  
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
                                          X.ant.help = case_when(antenna.kind == "1" ~ X.tow - .y * 0,
                                                            antenna.kind == "2" ~ X.tow + .y * 0.77,
                                                            antenna.kind == "3" ~ X.tow - .y * 0.77),
                                          Y.ant.help = case_when(antenna.kind == "1" ~ Y.tow - .y * 1, # meter distance apart
                                                            antenna.kind == "2" ~ Y.tow + .y * 0.77,
                                                            antenna.kind == "3" ~ Y.tow + .y * 0.77))) %>%
  # map(~mutate(., X.ant = X.ant.help,
  #             Y.ant = Y.ant.help)) %>% 
  map(~st_as_sf(., coords = c("X.ant.help", "Y.ant.help"))) %>%
  map(~mutate(., antenna.centroid = geometry)) %>% 
  map2(., coverage.radius, ~st_buffer(.x, .y)) %>% # radius coverage are per antenna
  map2(., coverage.radius, ~mutate(.x, coverage.radius = .y)) %>% # 
  map(~st_sf(., crs = 3035)) %>%
  map(~st_crop(., bb.focus.vec)) %>%
  map(~st_set_agr(., "aggregate")) %>% # clean up
  map2_dfr(., area.kind, ~mutate(., area.kind = .y))

saveRDS(coverage.areas, file = "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/coverage.areas.rds")


coverage.areas %>%
  st_drop_geometry() %>%
  group_by(area.kind) %>%
  summarise(n.antenna = n())

coverage.areas %>% 
  ggplot() +
  geom_sf(aes(col = area.kind), fill = NA) +
  facet_grid(cols = vars(area.kind)) +
  ggtitle("Fig 7: Coverage per layer", subtitle = "Signal density increases with increasing population density")



