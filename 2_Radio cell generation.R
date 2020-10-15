# MNO Chain
## 2 Generating Radio cell network

library(tidyverse)
library(data.table)
library(sf)
library(raster)
library(furrr)
library(stars)

# Initital object from file 1 Read In
census.de.100m.tile <- readRDS("working objects/census.tile.final.rds")

census.de.100m.tile.1 <- census.de.100m.tile %>% 
  mutate(pop.category = ntile(pop, 3)) %>% 
  mutate(pop.category = case_when(pop.category == 1 ~ "Rural",
                                  pop.category == 2 ~ "Suburban",
                                  pop.category == 3 ~ "Urban")) # THESE CATEGORIES NEED TO BE ADJUSTED BASED ON VALID DEFINTIONS



## layers
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


type <- list("Rural" = c("Rural", "Suburban", "Urban"),
             "Suburban" = c("Suburban", "Urban"),
             "Urban" = c("Urban"))
layer.base <- list("Rural" = census.de.100m.tile.1, "Suburban" = census.de.100m.tile.1, "Urban" = census.de.100m.tile.1) %>% 
  map2(., type, ~filter(.x, pop.category %in% .y))


cellsize <- list("Rural" = 20000, "Suburban" = 5000, "Urban" = 1000)
rotation.degree <- list("Rural" = 0, "Suburban" = 35, "Urban" = 70)
jitter <- list("Rural" = 5000, "Suburban" = 1000, "Urban" = 300)
coverage.radius <- c("Rural" = 15000, "Suburban" = 2500, "Urban" = 500)


layers <- pmap(list(layer.base, cellsize, rotation.degree, jitter), 
              ~layer_network_generate(x = ..1, cellsize = ..2, rotation.degree = ..3, jitter.amount = ..4)) %>% 
  set_names(c("Layer.1", "Layer.2", "Layer.3"))
  
# plot(layers[[1]])
# plot(layers[[2]], add = TRUE, col = 'red')
# plot(layers[[3]], add = TRUE, col = 'green')


 
layer.2.geo2 <- census.de.100m.tile.1 %>% 
  filter(pop.category %in% c("Suburban", "Urban")) %>% 
  st_make_grid(cellsize = 5000, square = F, flat_topped = T) %>% 
  st_geometry() %>% 
  st_jitter(1000)
layer.2.cntrd <- st_centroid(st_union(layer.2.geo))
layer.2 <- (layer.2.geo2 - layer.2.cntrd) * rotation(35) + layer.2.cntrd # rotate by 35 degrees

plot(layer.2)

coverage.areas <- layers.final %>% 
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
  map2(., ~mutate(., X = case_when(str_sub(antenna.ID, -1) == "1" ~ X - 0, # hier weitermachen
                                   str_sub(antenna.ID, -1) == "2" ~ X + 2211,
                                   str_sub(antenna.ID, -1) == "3" ~ X - 2211)) %>%
         mutate(Y = case_when(str_sub(antenna.ID, -1) == "1" ~ Y - 3000, # meter distance apart
                              str_sub(antenna.ID, -1) == "2" ~ Y + 2211,
                              str_sub(antenna.ID, -1) == "3" ~ Y + 2211)) %>%
         st_as_sf(coords = c("X", "Y")) %>% 
         st_buffer(4000) %>% # 4000 m radius coverage are per antenna
         st_sf(., crs = 3035) %>% 
         st_crop(bb.focus.vec) %>% 
         st_set_agr("aggregate") %>% # clean up
         mutate(cell.centroid = st_centroid(geometry)) %>% 
         mutate(area.kind = "Rural")
       
       
       antenna.1 <- layer.1 %>% 
         st_coordinates() %>% 
         as_tibble() %>% 
         dplyr::select(X, Y) %>%
         distinct() %>%  # remove duplicate corners 
         mutate(tower.ID = paste0("RT", 1:n())) %>% 
         slice(rep(1:n(), each = 3)) %>% 
         group_by(tower.ID) %>% 
         mutate(antenna.ID = paste(tower.ID, "A", 1:3, sep = ".")) %>% 
         ungroup() %>% 
         mutate(antenna.kind = str_sub(antenna.ID, -1)) %>% 
         mutate(X = case_when(str_sub(antenna.ID, -1) == "1" ~ X - 0,
                              str_sub(antenna.ID, -1) == "2" ~ X + 2211,
                              str_sub(antenna.ID, -1) == "3" ~ X - 2211)) %>%
         mutate(Y = case_when(str_sub(antenna.ID, -1) == "1" ~ Y - 3000, # meter distance apart
                              str_sub(antenna.ID, -1) == "2" ~ Y + 2211,
                              str_sub(antenna.ID, -1) == "3" ~ Y + 2211)) %>%
         st_as_sf(coords = c("X", "Y")) %>% 
         st_buffer(4000) %>% # 4000 m radius coverage are per antenna
         st_sf(., crs = 3035) %>% 
         st_crop(bb.focus.vec) %>% 
         st_set_agr("aggregate") %>% # clean up
         mutate(cell.centroid = st_centroid(geometry)) %>% 
         mutate(area.kind = "Rural")
       
       
       
       
       
       
       antenna.2 <- layer.2 %>% 
         st_coordinates() %>% 
         as_tibble() %>% 
         dplyr::select(X, Y) %>%
         distinct() %>%  # remove duplicate corners 
         mutate(tower.ID = paste0("ST", 1:n())) %>% 
         slice(rep(1:n(), each = 3)) %>% 
         group_by(tower.ID) %>% 
         mutate(antenna.ID = paste(tower.ID, "A", 1:3, sep = ".")) %>% 
         ungroup() %>% 
         mutate(antenna.kind = str_sub(antenna.ID, -1)) %>% 
         mutate(X = case_when(str_sub(antenna.ID, -1) == "1" ~ X - 0,
                              str_sub(antenna.ID, -1) == "2" ~ X + 1060,
                              str_sub(antenna.ID, -1) == "3" ~ X - 1060)) %>%
         mutate(Y = case_when(str_sub(antenna.ID, -1) == "1" ~ Y - 1500, # meter distance apart
                              str_sub(antenna.ID, -1) == "2" ~ Y + 1060,
                              str_sub(antenna.ID, -1) == "3" ~ Y + 1060)) %>%
         st_as_sf(coords = c("X", "Y")) %>% 
         st_buffer(2000) %>% # 6000 m radius coverage are per antenna
         st_sf(., crs = 3035) %>% 
         st_crop(bb.focus.vec) %>% 
         st_set_agr("aggregate") %>%  # clean up
         mutate(cell.centroid = st_centroid(geometry)) %>% 
         mutate(area.kind = "Suburban")
       
       
       
       antenna.3 <- layer.3 %>% 
         st_coordinates() %>% 
         as_tibble() %>% 
         dplyr::select(X, Y) %>%
         distinct() %>%  # remove duplicate corners 
         mutate(tower.ID = paste0("UT", 1:n())) %>% 
         slice(rep(1:n(), each = 3)) %>% 
         group_by(tower.ID) %>% 
         mutate(antenna.ID = paste(tower.ID, "A", 1:3, sep = ".")) %>% 
         ungroup() %>% 
         mutate(antenna.kind = str_sub(antenna.ID, -1)) %>% 
         mutate(X = case_when(str_sub(antenna.ID, -1) == "1" ~ X - 0,
                              str_sub(antenna.ID, -1) == "2" ~ X + 353,
                              str_sub(antenna.ID, -1) == "3" ~ X - 353)) %>%
         mutate(Y = case_when(str_sub(antenna.ID, -1) == "1" ~ Y - 500, # meter distance apart
                              str_sub(antenna.ID, -1) == "2" ~ Y + 353,
                              str_sub(antenna.ID, -1) == "3" ~ Y + 353)) %>%
         st_as_sf(coords = c("X", "Y")) %>% 
         st_buffer(500) %>% # 500 m radius coverage are per antenna
         st_sf(., crs = 3035) %>% 
         st_crop(bb.focus.vec) %>% 
         st_set_agr("aggregate") %>% # clean up
         mutate(cell.centroid = st_centroid(geometry)) %>% 
         mutate(area.kind = "Urban")
       
       coverage.areas <- bind_rows(antenna.1, antenna.2, antenna.3)
       
       coverage.areas %>% 
         st_drop_geometry() %>% 
         group_by(area.kind) %>% 
         summarise(n.antenna = n()) %>% 
         kbl() %>%
         kable_material(c("striped", "hover"))
       