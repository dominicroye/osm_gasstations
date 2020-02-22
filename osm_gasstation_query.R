####### Dr. Dominic Royé | @dr_xeo #########
####### dominic.roye@gmail.com     #########
############################################

##########                                                                   ###########
## The database of Open Street Maps: Map of the distribution of gas stations in Europe #
##########                                                                   ###########

# packages
library(tidyverse)
library(osmdata)
library(sf)

## functions for sf bind
bind_rows_sf <- function(...){
  sf_list <- rlang::dots_values(...)[[1]]
  
  sfg_list_column <- map(sf_list, st_geometry)
  sfg_list_column <- do.call(c, sfg_list_column)
  df <- map(sf_list, function(sf) st_set_geometry(sf, NULL)) %>% bind_rows
  
  sf_appended <- st_sf(data.frame(df, geom=sfg_list_column))
  
  return(sf_appended)
}


#bounding box, Example Iberian Peninsula
lon <- seq(-10, 4.9, .1) # longitude classes

m <- vector("list", length(lon))

for(i in 1:length(lon)){ m[[i]] <- c(lon[i], 30, lon[i]+.1, 46) }

#building the query for each long class
q <- map(m, function(x) {
             x %>% 
                opq(timeout = 60*60) %>%
                  add_osm_feature("amenity", "fuel")
         })

# query
pb <- progress_estimated(length(m)) #progress bar

# map over all queries with system sleep of 30 sec
gas <- map(q, function(x){ pb$tick()$print()
                            Sys.sleep(30)
                            osmdata_sf(x)})

# extract osm_points
gas <- map(gas, function(x) x$osm_points)

# bind all points
gas_final <- bind_rows_sf(gas)
