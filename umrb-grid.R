library(tidyverse)
library(sf)
library(magrittr)
library(mapview)
mapview::mapviewOptions(fgb = FALSE)

# Custom Albers parameterization from Alex Stum, 2021-10-20
umrb_grid_proj <-
  list(proj = "aea",
       lat_0 = 41.8865,
       lat_1 = 43.0,
       lat_2 = 47.8,
       lon_0 = -104.0487,
       units = "mi") %>%
  {paste0("+",names(.),"=",., collapse = " ")} %>%
  sf::st_crs()

edge <- 
  sqrt(500)

umrb_grid <-
  raster::raster(crs = umrb_grid_proj$input,
               resolution = c(edge,edge),
               xmn = 0-(19*edge),
               ymn = 0,
               xmx = (19*edge),
               ymx = 0+(23*edge)
) %>%
  raster::rasterToPolygons() %>%
  sf::st_as_sf()


mapview::mapview(umrb_grid) +
  mapview::mapview(sf::read_sf("data-raw/fwmesonetgrid19oct2021"))
