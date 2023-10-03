library(tidyverse)
library(magrittr)
library(sf)

httr::modify_url("https://api.synopticdata.com",
                 path = c("v2",
                          "stations",
                          "latest"),
                 query = list(token="d751b219bf9a4fd589f7304d76012fff",
                              output="geojson",
                              network="3023,3024,3025,3026,3027",
                              vars="air_temp",
                              fields="stid,name",
                              units="english",
                              obtimezone="local")) %>%
  sf::read_sf() %>%
  mapview::mapview(zcol = "air_temp")
