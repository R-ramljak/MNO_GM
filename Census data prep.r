library(tidyverse)
library(data.table)
library(sf)
library(raster)


census.pop.de.raw <- fread("Data/Census data Germany/csv_Bevoelkerung_100m_Gitter/Zensus_Bevoelkerung_100m-Gitter.csv", nrows = 10000)
census.dem.de.raw <- fread("Data/Census data Germany/csv_Demographie_100m_Gitter/Bevoelkerung100M.csv", nrows = 1000000)


pop.var.vec <- c("tile.100m", "x.mp.100m", "y.mp.100m", "pop")


census.pop <- census.pop.de.raw %>% 
  set_names(pop.var.vec) %>% 
  mutate(pop.random = case_when(pop %in% c("-1", 0:10) ~ 1, # marking pop values which are random numbers
                                TRUE ~ 0)) %>% 
  mutate(pop = case_when(pop.random == 1 ~ sample(1:10, n(), replace = T), # random numbers for undisclosed pop values
                         TRUE ~ pop)) %>% 
  st_as_sf(coords = c("x.mp.100m", "y.mp.100m"), crs = st_crs(3035)$proj4string)

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




