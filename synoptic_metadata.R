library(magrittr)
library(ggplot2)

con <-
  DBI::dbConnect(RPostgres::Postgres(),
                 host = Sys.getenv("MESONET_HOSTNAME"),
                 dbname = Sys.getenv("MESONET_DBNAME"),
                 user = Sys.getenv("MESONET_USER"),
                 password = Sys.getenv("MESONET_PASSWORD")
  )

extract_depth <- function(element) {
  match <- stringr::str_extract(element, "_(\\d{4})$")
  return(stringr::str_remove(match, "_"))
}

get_station_sensor_metadata <- function(station, nwsli_id, ...) {
  dplyr::tbl(con, RPostgres::Id(schema = "data", table = "station_elements")) %>%
    dplyr::filter(
      station == !!station, 
      is.na(date_end),
      element != "door"
    ) %>% 
    dplyr::select(element, manufacturer, model) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      depth = extract_depth(element) %>% 
        as.numeric() %>% 
        magrittr::divide_by(100)
    )
}

get_all_station_sensor_metadata <- 
  function(start_date = lubridate::as_date("2000-01-01"), end_date = lubridate::today()) {
    elements <- dplyr::tbl(con, RPostgres::Id(schema = "data", table = "stations")) %>% 
      dplyr::filter(
        sub_network == "HydroMet",
        date_installed > start_date,
        date_installed < end_date,
        !is.na(nwsli_id)
      ) %>%
      dplyr::select(name, station, nwsli_id) %>%
      dplyr::collect() %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(
        info = purrr::pmap(list(station, nwsli_id), get_station_sensor_metadata)
      ) %>% 
      tidyr::unnest(cols = c(info))
    
    dplyr::tbl(con, RPostgres::Id(schema = "data", table = "elements")) %>% 
      dplyr::select(element, ace_name) %>% 
      dplyr::filter(element %in% elements$element) %>% 
      dplyr::collect() %>%
      dplyr::full_join(elements) %>% 
      dplyr::filter(!is.na(ace_name)) %>% 
      dplyr::select(
        `Station ID` = nwsli_id,
        `Station name` = name,
        Variable = ace_name,
        `Position/depth (AGL) (m)` = depth,
        `Sensor Mfg` = manufacturer,
        `Sensor Model` = model
      ) %>% 
      readr::write_delim("./sensor_metadata.txt", delim = "|")
  }


cover_description <- function() {
  
  dplyr::tbl(con, RPostgres::Id(schema = "data", table = "stations")) %>% 
    dplyr::filter(
      sub_network == "HydroMet",
      !is.na(date_installed),
      !is.na(nwsli_id)
    ) %>%
    dplyr::select(name, nwsli_id) %>%
    dplyr::collect() %>%
    dplyr::select(
      `Station ID` = nwsli_id,
      `Station name` = name
    ) %>% 
    dplyr::mutate(
      Slope = "A",
      Aspect = 0,
      `Cover Description Code` = NA
    ) %>%
    readr::write_delim("./site_metadata.txt", delim = "|")
  
}
