library(tidyverse)
library(magrittr)
library(sf)

httr::modify_url("https://api.synopticdata.com",
                 path = c("v2",
                          "stations",
                          "latest"),
                 query = list(token = "d751b219bf9a4fd589f7304d76012fff",
                              output = "geojson",
                              network = "3023,3024,3025,3026,3027",
                              fields = "stid,name",
                              vars = "soil_moisture",
                              units = "english",
                              obtimezone = "local",
                              sensorvars = "1"
                              )) %>%
  sf::read_sf() %>%
  mapview::mapview(zcol = "soil_moisture_1")


httr::GET("https://api.synopticdata.com",
          path = c("v2",
                   "variables"),
          query = list(token="d751b219bf9a4fd589f7304d76012fff")) %>%
  httr::content() %$%
  VARIABLES %>%
  purrr::map_chr(names) %>%
  sort()



httr::GET("https://api.synopticdata.com",
                 path = c("v2",
                          "stations",
                          "latest"),
                 query = list(token = "d751b219bf9a4fd589f7304d76012fff",
                              stid = "BRYM8",
                              vars = "soil_moisture",
                              sensorvars = "1",
                              output = "geojson"
                 )) %>%
  httr::content()

