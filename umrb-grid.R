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

mt_grid <-
  raster::raster(crs = umrb_grid_proj$input,
                 resolution = c(edge,edge),
                 xmn = 0-(25*edge),
                 ymn = 0,
                 xmx = (19*edge),
                 ymx = 0+(24*edge)
  ) %>%
  raster::rasterToPolygons() %>%
  sf::st_as_sf() %>%
  sf::st_filter(
    mcor::mt_state_simple |>
      sf::st_transform(umrb_grid_proj)
    ) |>
  sf::st_transform(mcor::mt_state_plane) %T>%
  sf::write_sf("data/mt-ace-grid.shp") %>%
  sf::st_join(sf::read_sf("data/umrb-ace-grid.geojson") %>%
                      dplyr::select(cell = Cell) %>%
                      sf::st_transform(mcor::mt_state_plane) %>%
                      sf::st_centroid(),
            join = sf::st_contains) %>%
  sf::st_intersection(mcor::mt_state) %>%
  dplyr::mutate(cellArea = sf::st_area(geometry) %>%
                  units::set_units("miles^2")) %>%
  dplyr::filter(cellArea >= units::set_units(0.4*500, "miles^2")) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  dplyr::select(cell, cellArea) %T>%
  sf::write_sf("data/mt-ace-grid-clipped.geojson",
               delete_dsn = TRUE)

mapview::mapview(mt_grid, zcol = "cell", legend = FALSE)


mapview::mapview(umrb_grid) +
  mapview::mapview(mt_grid) + 
  mapview::mapview(sf::read_sf("data-raw/fwmesonetgrid19oct2021"))

library(sf)
library(magrittr)
sf::read_sf("/vsizip//Users/bocinsky/git/mt-climate-office/umrb-ace/data-raw/fwmesonetgrid19oct2021.zip") %>%
  dplyr::transmute(cell, state = State, included = grepl("^[[:upper:]]", cell)) %>%
  dplyr::mutate(cell_area = sf::st_area(geometry) %>%
                  units::set_units("mi^2")) %>%
  dplyr::select(cell, cell_area, included, state) %>%
  sf::write_sf("data/umrb-ace-grid.shp")
  
  



## Manual grid revisions
# Cells added to or removed from the network after the 2021 grid was drawn.
# Removed cells are lowercased in the shapefile, which marks them as not
# included, and dropped from the geojson. This section can be re-run safely.
grid_removed <- c("J-13")  # 2026-10-21: J-13 will not be filled
grid_added <- c("E-9")     # 2026-10-21: E-9 will get a site, despite being <40% in the UMRB

raw_grid <-
  sf::read_sf("data-raw/fwmesonetgrid19oct2021")

sf::read_sf("data/umrb-ace-grid.shp") %>%
  dplyr::mutate(cell = dplyr::if_else(toupper(cell) %in% grid_removed,
                                      tolower(cell),
                                      cell),
                included = grepl("^[[:upper:]]", cell)) %T>%
  sf::write_sf("data/umrb-ace-grid.shp",
               delete_dsn = TRUE) %>%
  invisible()

file.remove("data/umrb-ace-grid.zip")
zip("data/umrb-ace-grid.zip",
    files = list.files("data", pattern = "^umrb-ace-grid\\.(dbf|prj|shp|shx)$", full.names = TRUE),
    flags = "-j")

sf::read_sf("data/umrb-ace-grid.geojson") %>%
  dplyr::filter(!(toupper(Cell) %in% c(grid_removed, grid_added))) %>%
  dplyr::bind_rows(
    raw_grid %>%
      dplyr::filter(cell %in% grid_added) %>%
      dplyr::select(Cell = cell,
                    State) %>%
      sf::st_make_valid() %>%
      sf::st_transform(4326) %>%
      sf::st_cast("MULTIPOLYGON")
  ) %>%
  dplyr::arrange(Cell) %>%
  sf::write_sf("data/umrb-ace-grid.geojson",
               delete_dsn = TRUE)
