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


cover_description <- function(
    cover_file = "./Stations USDA Cover Codes 241202.csv",
    detail_file = "./nwsli_detail.txt"
) {
  
  cover <- readr::read_csv(cover_file, show_col_types=FALSE) %>%
    dplyr::select(station=station_ID, `Cover Description Code`=Cover_USDA)
  
  detail <- readr::read_delim(
    "./nwsli_detail.txt", 
    col_names = c("Station ID", "city", "distance", "azimuth")
  ) %>% 
    dplyr::select(`Station ID`, distance, azimuth)

  dplyr::tbl(con, RPostgres::Id(schema = "data", table = "stations")) %>% 
    dplyr::filter(
      sub_network == "HydroMet",
      !is.na(date_installed),
      !is.na(nwsli_id),
      nwsli_id != "RAPM8"
    ) %>%
    dplyr::collect() %>%
    dplyr::left_join(cover) %>%
    dplyr::select(station, name, nwsli_id, `Cover Description Code`) %>%
    dplyr::rename(
      `Station ID` = nwsli_id,
      `Station name` = name
    ) %>% 
    dplyr::mutate(
      Slope = "A",
      Aspect = 0,
      `Cover Description Code` = dplyr::case_when(
        `Station ID` == "BNGM8" ~ 3103,
        `Station ID` == "BSEM8" ~ 2233,
        `Station ID` == "MINM8" ~ 3103,
        `Station ID` == "MOBM8" ~ 3103,
        `Station ID` == "OHMM8" ~ 3103,
        `Station ID` == "TRNM8" ~ 3103,
        `Station ID` == "VLTM8" ~ 3102,
        `Station ID` == "WETM8" ~ 4402,
        TRUE ~ `Cover Description Code`
      )
    ) %>%
    dplyr::left_join(detail) %>%
    dplyr::mutate(distance = as.character(distance)) %>%
    tidyr::replace_na(list(distance="", azimuth="")) %>%
    dplyr::mutate(
      dir = paste0(distance, azimuth),
      `NWSLI Detail` = paste(`Station name`, dir, "Montana Mesonet") %>%
        gsub("\\s+", " ", .)
    ) %>%
    dplyr::select(-c(distance, azimuth, dir, station)) %>%
    readr::write_delim("./site_metadata.txt", delim = "|")
}


readr::read_csv("https://mesonet.climate.umt.edu/api/stations?type=csv", 
                show_col_types = FALSE) %>%
  dplyr::select(name, station, nwsli_id) %>%
  readr::write_csv("~/Downloads/mt_climate_offices_nwsliids.csv")
