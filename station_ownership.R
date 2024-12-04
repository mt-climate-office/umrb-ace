library(magrittr)
library(tidyverse)
library(arcgislayers)

con <- DBI::dbConnect(drv = RPostgres::Postgres(), 
                      host = "fcfc-mesonet-db2.cfc.umt.edu",
                      dbname = "mesonet",
                      user = "mesonet",
                      password = keyring::key_get("mesonet-db"))

stations <-
  dplyr::tbl(con, RPostgres::Id(schema = "data", table = "stations")) %>%
  dplyr::select(station, name, sub_network, status, longitude, latitude) %>%
  dplyr::collect() %>%
  dplyr::filter(!is.na(longitude)) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4326)

stations %>% 
  dplyr::group_by(sub_network, status) %>%
  sf::st_drop_geometry() %>%
  dplyr::count()

esri_stations <-
  prepare_spatial_filter(stations$geometry, predicate = "intersects", crs = 4326)

mt_cadastral <-
  arcgislayers::arc_open(
    "https://gisservicemt.gov/arcgis/rest/services/MSDI_Framework/Parcels/MapServer/0"
  )

parcels <-
  httr::modify_url("https://gisservicemt.gov/arcgis/rest/services/MSDI_Framework/Parcels/MapServer/0/query",
          query = list(where = "1=1",
                       geometry = esri_stations$geometry,
                       geometryType = esri_stations$geometryType,
                       spatialRel = esri_stations$spatialRel,
                       outFields = "PARCELID,PropertyID,PropType,OwnerName,CareOfTaxpayer,OwnerAddress1,OwnerAddress2,OwnerAddress3,OwnerCity,OwnerState,OwnerZipCode,AssessmentCode,LegalDescriptionShort,LevyDistrict",
                       returnGeometry = TRUE,
                       f = "geojson")) %>%
  sf::read_sf() %>%
  sf::st_make_valid()

sf::st_join(stations, parcels) %>%
  dplyr::select()

mapview::mapview(parcels) + mapview::mapview(stations)

  



