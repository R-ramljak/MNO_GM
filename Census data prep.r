library(tidyverse)
library(data.table)
library(sf)


census.pop.de.raw <- fread("Data/Census data Germany/csv_Bevoelkerung_100m_Gitter/Zensus_Bevoelkerung_100m-Gitter.csv", nrows = 100000)
census.dem.de <- fread("Data/Census data Germany/csv_Demographie_100m_Gitter/Bevoelkerung100M.csv", nrows = 1000)


pop.var.vec <- c("tile.100m", "x.cent", "y.cent", "pop")


census.pop <- census.pop.de %>% 
  set_names(pop.var.vec) %>% 
  mutate(pop.random = case_when(pop == "-1" ~ 1, # marking pop values which are random numbers
                                TRUE ~ 0)) %>% 
  mutate(pop = case_when(pop.random == 1 ~ sample(1:10, n(), replace = T), # random numbers for undisclosed pop values
                         TRUE ~ pop)) 


# pop represents u in the paper

# define illumnation intensity sij for each tile 

