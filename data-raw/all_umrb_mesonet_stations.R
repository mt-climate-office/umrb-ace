library(magrittr)

umrb_grid_proj <-
  list(proj = "aea",
       lat_0 = 41.8865,
       lat_1 = 43.0,
       lat_2 = 47.8,
       lon_0 = -104.0487,
       units = "mi") %>%
  {paste0("+",names(.),"=",., collapse = " ")} %>%
  sf::st_crs()

umrb_grid <- sf::read_sf("data-raw/fwmesonetgrid19oct2021/") %>% 
  dplyr::filter(percentIn > 40) %>% 
  dplyr::select(cell) %>%
  sf::st_transform(umrb_grid_proj)


synoptic_url <- "https://api.synopticdata.com/v2/stations/metadata"
synoptic_token <- rstudioapi::askForPassword("Synoptic Token")

synoptic_to_df <- function(resp) {
  
  resp %>% 
    purrr::pluck("STATION") %>% 
    purrr::map(\(x) {
      x %>%
        purrr::map(~ {if (is.null(.x)) NA else .x}) %>%
        tibble::as_tibble() %>%
        janitor::clean_names() %>% 
        dplyr::select(station=stid, latitude, longitude) %>%
        dplyr::distinct()
    }) %>% 
    dplyr::bind_rows() 
  
}


get_ndsu_stations <- function() {
  url <- "https://ndawn.ndsu.nodak.edu/station-info.html"
  
  1:275 %>%
    purrr::map(\(x) {
      response <- httr::GET(
        url, query = list(station=x)
      )
      
      # Parse the content with rvest
      locs <- httr::content(response, as = "text") %>%
        # Pull out lines with lat and lon
        stringr::str_extract_all("(?<=[Latitude|Longitude]:</td><td>)-?\\d+\\.\\d+(?=&deg;)") %>%
        unlist()
      
      if (length(locs) == 0) {
        return(tibble::tibble())
      } else {
        locs %>% 
          as.numeric() %>%
          matrix(ncol = 2, byrow = TRUE) %>%
          tibble::as_tibble() %>%
          magrittr::set_names(c("latitude", "longitude")) %>%
          dplyr::mutate(station = x)
      }
    }) %>%
    dplyr::bind_rows()
}

stations_to_sf <- function(df) {
  df %>% 
    dplyr::mutate(
      station = as.character(station),
      latitude = round(as.numeric(latitude), 2),
      longitude = round(as.numeric(longitude), 2)
    ) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>%
    sf::st_transform(umrb_grid_proj)
}


get_all_stations <- function() {
  usace_stations <- httr::GET(
    synoptic_url,
    query = list(
      token = synoptic_token,
      network = "3023,3024,3025,3026,3027"
    )
  ) %>% 
    httr::content() %>%
    synoptic_to_df() %>% 
    stations_to_sf()
  
  mt_wyo_stations <- httr::GET(
    synoptic_url,
    query = list(
      token = synoptic_token,
      network = "3008,3006,233,3019,3002"
    )
  ) %>% 
    httr::content() %>%
    synoptic_to_df() %>%
    stations_to_sf()
  
  
  ndsu_stations <- get_ndsu_stations() %>% 
    stations_to_sf()
  
  sdsu_stations <- readr::read_csv("data-raw/sdsu_stations.csv") %>% 
    janitor::clean_names() %>% 
    stations_to_sf()
  
  
  unl_stations <- tibble::tribble(
    ~station, ~latitude, ~longitude,
    "harrison",42.664, -103.898,
    "whitney",42.742, -103.318,
    "marsland",42.356, -103.331,
    "scottsbluff",41.953, -103.702,
    "scottsbluff2",41.892, -103.681,
    "harrisburg",41.576, -103.732,
    "bushnell",41.048, -103.814,
    "hay",42.615, -102.909,
    "hemingford",42.409, -102.939,
    "alliance",42.184, -102.922,
    "alliance2",42.058, -102.894,
    "broadwater",41.693, -102.855,
    "sidney",41.232, -103.021,
    "rushville",42.712, -102.437,
    "gordon",42.648, -102.069,
    "ellsworth",42.279, -102.166,
    "oshkosh",41.493, -102.346,
    "merriman", 42.616, -101.725,
    "whitman", 42.082, -101.449,
    "arthur", 41.453, -101.713,
    "bigsprings",41.159, -101.995,
    "bigsprings2",41.021, -101.975,
    "grant",40.845, -101.649,
    "enders",40.318, -101.615,
    "hayes",40.318, -101.615,
    "dickens",40.833, -100.979,
    "northp",41.085, -100.775,
    "indianola",40.135, -100.479,
    "gothenburg",40.952, -100.194,
    "smithfield",40.569, -99.69,
    "lexington",40.726, -99.751,
    "merna",41.452, -99.78,
    "lonepine",42.244, -99.661,
    "naper",42.845, -99.259,
    "naper2",42.966, -99.139,
    "lynch",42.841, -98.504,
    "emmet",42.477, -98.764,
    "ord",41.624, -98.949,
    "overton",40.672, -99.477,
    "holdrege",40.505, -99.36,
    "axtell",40.53, -99.051,
    "kearney",40.718, -99.015,
    "guiderock",40.09, -98.285,
    "shelton",40.749, -98.76,
    "harvard",40.575, -98.139,
    "woodriver",40.76, -98.536,
    "alda",40.937, -98.526,
    "centralcity",41.113, -98.05,
    "york",40.871, -97.634,
    "wilber",40.475, -96.986,
    "rulo",40.022, -95.505,
    "firth",40.574, -96.612,
    "cook",40.47, -96.208,
    "nebraskacity",40.686, -95.909,
    "lincoln",40.83, -96.657,
    "walton",40.855, -96.606,
    "eagle",40.847, -96.467,
    "plattsmouth",40.977, -95.881,
    "memphis",41.146, -96.441,
    "memphis2",41.159, -96.414,
    "valparaiso",41.164, -96.871,
    "winslow",41.164, -96.871,
    "leigh",41.713, -97.264,
    "oakland",41.843, -96.541,
    "decatur",41.907, -96.263,
    "elgin",41.945, -98.183,
    "pierce",42.174, -97.546,
    "orchard",42.491, -98.261,
    "fordyce",42.75, -97.347,
    "fordyce2",42.749, -97.398,
  ) %>% 
    stations_to_sf()
  
  
  to_install <- umrb_grid %>%
    sf::st_join(usace_stations, join = sf::st_contains) %>% 
    sf::st_centroid() %>%
    dplyr::select(-cell) %>% 
    dplyr::filter(is.na(station)) %>%
    dplyr::mutate(station = tidyr::replace_na(station, "to_install"))
  
  stations <- 
    dplyr::bind_rows(
      usace_stations, to_install, unl_stations, ndsu_stations, mt_wyo_stations, sdsu_stations
    ) %>%
    dplyr::transmute(
      active = dplyr::case_when(
        station == "to_install" ~ "Future Installation",
        TRUE ~ "Operational"
      )
    )
}