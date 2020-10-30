# device to cell

library(tidyverse)
library(data.table)
library(sf)
library(furrr)
library(Matrix)

census.de.100m.tile <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/census.tile.final.rds") %>% 
  select(-c(cluster_id, pop.raster, cluster.tile.n))

coverage.areas <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/coverage.areas.rds")

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


dev.to.cell <- census.de.100m.tile %>% 
  mutate(tile.centroid = st_centroid(geometry)) %>%
  st_join(coverage.areas, left = F) %>%
  st_transform(crs = 3035) %>% 
  st_sf(sf_column_name = "antenna.centroid", crs = 3035) %>% 
  mutate(dist.sij = as.numeric(st_distance(tile.centroid, antenna.centroid, by_element = T))) %>% 
  mutate(signal.sij = signal_strength(dist.sij, coverage.radius)) %>% # implement relevant signal strength formula --> faster way of doing this maybe in a grouped way?
  group_by(internal.id) %>%
  mutate(weight.pij = case_when(is.nan(as.numeric(signal.sij / sum(signal.sij, na.rm = T))) ~ as.numeric(0),
                                TRUE ~ as.numeric(signal.sij / sum(signal.sij, na.rm = T)))) %>% 
  ungroup()


saveRDS(dev.to.cell, "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/dev.to.cell.rds")
dev.to.cell <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/dev.to.cell.rds")


C.vec.helper <- dev.to.cell %>% 
  dplyr::select(internal.id, antenna.ID, pop, weight.pij) %>% 
  st_drop_geometry()


C.vec.help.0 <- C.vec.helper %>% 
  filter(pop == 0)

C.vec.help.1 <- C.vec.helper %>% 
  filter(pop != 0 & weight.pij == 1)

# Calculate the number of cores
no_cores <- availableCores() - 1
plan(multisession, workers = no_cores)

# C.vec.help.rest <- C.vec.helper %>% 
#   filter(pop >= 1 & weight.pij > 0 & weight.pij < 1) %>% 
#   split(.$internal.id)
# saveRDS(C.vec.help.rest, "working objects/C.vec.rest.groups.rds")
C.vec.help.rest <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/C.vec.rest.groups.rds")

C.rest.final <- C.vec.help.rest %>% 
  future_map(~sample(., x = .$antenna.ID, mean(.$pop),
                     replace = T, prob = .$weight.pij), .progress = T) %>% 
  future_map(as_tibble, .id = "internal.id", .progress = T) %>% 
  future_map(~group_by(., value), .progress = T) %>% 
  future_map(~summarise(., pop.count.rand = n(), .groups = "drop"), .progress = T)

n_distinct(C.vec.help.rest$internal.id)

C.vec <- data.table(C.vec.helper) [, sample(x = C.vec.helper$antenna.ID, size = mean(C.vec.helper$pop),
                                            replace = T, prob = C.vec.helper$weight.pij), by = internal.id]


map(as_tibble, .id = "internal.id") %>% 
  map(~group_by(., value)) %>% 
  map(~summarise(., pop.count.rand = n(), .groups = "drop"))


# sparse matrix of device to cell
P.helper <- dev.to.cell %>% 
  dplyr::select(internal.id, antenna.ID, weight.pij) %>% 
  mutate_at(vars(c("internal.id", "antenna.ID")), factor) %>% 
  mutate(weight.pij = round(weight.pij, 4)) %>%
  st_drop_geometry()

P.mat <- sparseMatrix(i = as.numeric(P.helper$antenna.ID), 
                      j = as.numeric(P.helper$internal.id), 
                      x = P.helper$weight.pij,
                      dimnames = list(levels(P.helper$antenna.ID), levels(P.helper$internal.id)))


saveRDS(P.mat, file = "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/P.mat.rds")

P.mat <- readRDS("C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/P.mat.rds")


# C-vector (total count of mobile phones of a tile stochastically assigned to a radio cell based on P --> here: weight.pij)
C.vec.group <- dev.to.cell %>% 
  dplyr::select(internal.id, antenna.ID, pop, weight.pij) %>% 
  st_drop_geometry() %>%
  group_split(internal.id)

C.vec <- C.vec.group %>% 
  map(~sample(x = .$antenna.ID, mean(.$pop),
              replace = T, prob = .$weight.pij)) %>% 
  map(as_tibble, .id = "internal.id") %>% 
  map(~group_by(., value)) %>% 
  map(~summarise(., pop.count.rand = n(), .groups = "drop"))




r <- d %>% 
  bind_rows() %>% 
  pivot_wider(id_cols = internal.id, names_from = antenna.ID, values_from = weight.pij)

d <- dev.to.cell %>% 
  head(1000) %>% 
  dplyr::select(internal.id, antenna.ID, pop, weight.pij) %>% 
  st_drop_geometry() %>% 
  group_split(internal.id) %>% 
  map(~dplyr::select(., weight.pij)) %>% 
  map(deframe)

u <- dev.to.cell %>% 
  head(1000) %>% 
  dplyr::select(internal.id, pop) %>% 
  st_drop_geometry() %>% 
  filter(internal.id %in% names(d)) %>% 
  deframe()

antennas <- dev.to.cell %>% 
  head(1000) %>% 
  dplyr::select(antenna.ID, internal.id, pop, weight.pij) %>% 
  st_drop_geometry() %>% 
  group_by(internal.id) %>% 
  group_split() %>% 
  map(~select(., antenna.ID)) %>%
  map(deframe)

probb <- d %>% 
  pmap(list(antennas, u, d), ~sample(x = .x, size = .y, replace = T, prob = .z))

t <- r %>% 
  group_by(internal.id) %>% 
  filter(!sum(weight.pij) == 0) %>% 
  group_split()

e <- t %>% 
  map(~sample(x = .$antenna.ID, mean(.$pop),
              replace = T, prob = .$weight.pij)) %>% 
  map(as_tibble, .id = "internal.id") %>% 
  map(~group_by(., value)) %>% 
  map(~summarise(., pop.count.rand = n(), .groups = "drop"))



# C-vector (total count of mobile phones of a tile stochastically assigned to a radio cell based on P --> here: weight.pij)
C.vec.group <- dev.to.cell %>% 
  dplyr::select(internal.id, antenna.ID, pop, weight.pij) %>% 
  st_drop_geometry() %>%
  group_by(internal.id) %>% 
  group_split()

C.vec <- C.vec.group %>% 
  map(~sample(x = .$antenna.ID, mean(.$pop),
              replace = T, prob = .$weight.pij)) %>% 
  map(as_tibble, .id = "internal.id") %>% 
  map(~group_by(., value)) %>% 
  map(~summarise(., pop.count.rand = n(), .groups = "drop"))


# u-vector
U.vec <- census.de.1km.tile %>% 
  dplyr::select(internal.id, pop)

saveRDS(U.vec, file = "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/U.vec.rds")

# Calculate the number of cores
no_cores <- availableCores() - 1
plan(multisession, workers = no_cores)

C.vec.1 <- dev.to.cell %>% 
  dplyr::select(internal.id, antenna.ID, pop, weight.pij) %>% 
  st_drop_geometry() %>%
  group_by(internal.id) %>% 
  group_split() %>% 
  future_map(~sample(., x = .$antenna.ID, mean(.$pop),
              replace = T, prob = .$weight.pij), .progress = T) %>% 
  future_map(as_tibble, .id = "internal.id", .progress = T) %>% 
  future_map(~group_by(., value), .progress = T) %>% 
  future_map(~summarise(., pop.count.rand = n(), .groups = "drop"), .progress = T)

saveRDS(C.vec, file = "C:/Users/Marco/OneDrive - Universiteit Utrecht/MNO/working objects/C.vec.rds")