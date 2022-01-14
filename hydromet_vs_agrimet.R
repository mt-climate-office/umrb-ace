library(tidyverse)
library(magrittr)
library(plotly)
library(crosstalk)

november_ppt <-
  httr::GET("https://mesonet.climate.umt.edu/api/v2/observations_new/",
            query = list(type = "csv",
                         units="SI",
                         stations=paste0(c("aceabsar","wsrabsaw",
                                           "acebozem","bozmtest",
                                           "acebroad","wsrbroad",
                                           "acecrowa","crowagen",
                                           "acehavre","havrenmt",
                                           "aceingom","sevnoner",
                                           "acemelvi","wsrmelvi",
                                           "acemiles","mdamiles",
                                           "acemocca","moccasin",
                                           "aceround","wrsround",
                                           "acesidne","sidneymt"
                         ),
                         collapse = ","),
                         elements=
                           paste0(
                             c(
                               "ppt"
                             ),
                             collapse = ","),
                         start_time="2021-11-21")) %>%
  httr::content()

november_ppt_processed <- 
  november_ppt %>%
  dplyr::left_join(readr::read_csv("https://mesonet.climate.umt.edu/api/v2/stations/?type=csv") %>%
                     dplyr::select(station, name) %>%
                     dplyr::arrange(name)) %>%
  dplyr::mutate(network = ifelse(stringr::str_detect(station, "ace"),"Hydromet","Agrimet")) %>%
  tidyr::pivot_longer(cols = `Precipitation [mm]`,
                      names_to = "variable") %>%
  # tidyr::separate(value,
  #                 into = c("value","units"),
  #                 sep = " ") %>%
  # dplyr::mutate(value = as.numeric(value)) %>%
  dplyr::group_by(network,
                  station,
                  name,
                  datetime = lubridate::ceiling_date(datetime, "15mins"),
                  variable) %>%
  dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = variable,
                     values_from = value) %>%
  dplyr::ungroup() %>%
  dplyr::select(Network = network,
                Location = name,
                Time = datetime,
                `Precipitation [mm]`) %>%
  tidyr::pivot_wider(names_from = Network,
                     values_from = `Precipitation [mm]`) %>%
  dplyr::arrange(Location, Time) %>%
  dplyr::mutate(Agrimet = ifelse(Location == "Sidney" & Agrimet == 278.868, NA, Agrimet)) %T>%
  readr::write_csv("compare_ppt.csv") %>%
  tidyr::pivot_longer(Agrimet:Hydromet,
                      names_to = "Network",
                      values_to = "Precipitation (mm)")

nov_ppt <- 
  november_ppt_processed %>%
  # dplyr::mutate(`Precipitation (mm)` = ifelse(`Precipitation (mm)`==0, 
  #                                             NA, 
  #                                             `Precipitation (mm)`)) %>%
  dplyr::mutate(Location = factor(Location, 
                                  levels = sort(unique(Location)), 
                                  ordered = TRUE)) %>%
  na.omit() %>%
  tidyr::pivot_wider(names_from = Network,
                     values_from = `Precipitation (mm)`) %>%
  highlight_key()

nov_ppt_filter <-
  filter_select(id = "id", 
                label = "Select a location", 
                sharedData = nov_ppt, 
                group = ~Location,
                multiple = FALSE)

bscols(nov_ppt_filter, 
         plot_ly(nov_ppt, 
                 x = ~Time, 
                 y = ~Agrimet, 
                 type = 'bar',
                 name = 'Agrimet') %>% 
           add_trace(y = ~Hydromet,
                     name = 'Hydromet') %>% 
           layout(yaxis = list(title = 'Precipitation (mm)'), 
                  barmode = 'group',
                  hovermode = "x",
                  hoverdistance = -1), 
         widths = 12) 

