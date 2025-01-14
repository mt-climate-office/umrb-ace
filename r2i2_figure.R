library(magrittr)
library(ggplot2)

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

states <-
  tigris::states(cb = TRUE) %>%
  dplyr::filter(!(STUSPS %in% c("PR","AK","HI","GU","MP", "AS","VI"))) %>%
  sf::st_transform(umrb_grid_proj)

umrb <- 
  sf::read_sf("data/umrb.geojson") %>%
  sf::st_transform(umrb_grid_proj) %>%
  rmapshaper::ms_simplify()

umrb_states <-
  states %>%
  dplyr::filter(STUSPS %in% c("MT", "WY", "ND", "SD", "NE")) %>%
  dplyr::select(NAME)

tcus <- 
  readr::read_csv("data-raw/tcus.csv") %>%
  dplyr::left_join(  tigris::places(cb = TRUE) %>%
                       sf::st_centroid()  %>%
                       dplyr::left_join(tigris::states() %>%
                                          sf::st_drop_geometry() %>%
                                          dplyr::select(STATEFP, State = STUSPS)) %>%
                       dplyr::transmute(Town = NAME,
                                        State)) %>%
  sf::st_as_sf() %>%
  sf::st_transform(umrb_grid_proj) %>%
  sf::st_intersection(umrb_states)


universities <- tibble::tribble(
  ~place, ~lat, ~lon,
  "um", 46.8619656963864, -113.98467560181538,
  "ndsu", 46.89774315419438, -96.80250322729643,
  "sdsu", 44.31911391577087, -96.78354850796151,
  "wy", 41.31463717100004, -105.56901693286915,
  "unl", 40.82015677983608, -96.70050968433904
) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  sf::st_transform(umrb_grid_proj)

get_image_rast <-
  function(x, url = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSShadedReliefOnly/MapServer/export"){
    
    umrb_buffer <- x  %>%
      sf::st_transform(umrb_grid_proj) %>%
      sf::st_buffer(20000)
    
    x <- rast(extent = terra::ext(umrb_buffer), crs = terra::crs(umrb_buffer), resolution = 1000)
    
    ext <- 
      x %>% 
      sf::st_bbox()
    
    
    deets <-
      httr::GET(url,
                query = list(bbox=ext %>% paste0(collapse = ","),
                             bboxSR=terra::crs(x, describe = TRUE)$code,
                             size = paste0(terra::ncol(x),",",terra::nrow(x)),
                             imageSR=terra::crs(x, describe = TRUE)$code,
                             f="json")) %>%
      httr::content()
    
    httr::modify_url(url,
                     query = list(bbox=ext %>% paste0(collapse = ","),
                                  bboxSR=terra::crs(x, describe = TRUE)$code,
                                  size = paste0(terra::ncol(x),",",terra::nrow(x)),
                                  imageSR=terra::crs(x, describe = TRUE)$code,
                                  f="image")) %>%
      terra::rast() %T>%
      terra::set.crs(terra::crs(x)) %T>%
      terra::set.ext(unlist(deets$extent[c("xmin","xmax","ymin","ymax")])) %>%
      magrittr::set_names("layer")
}


get_df <- function(x){
  out <- cbind(xyFromCell(x, seq_len(ncell(x))),
               tibble::tibble(ID = getValues(x))) %>%
    tibble::as_tibble()
  
  if(is.factor(x)){
    
    levels <- levels(x)[[1]] %>%
      dplyr::mutate_all(.funs = list(ordered)) %>%
      tibble::as_tibble()
    
    fact <- out$ID %>%
      ordered(levels = levels(levels$ID))
    
    out %<>%
      dplyr::mutate(ID = fact) %>%
      dplyr::left_join(levels)
  }
  
  return(out)
}


hill <- get_image_rast(umrb_states) %>%
  terra::project(umrb_grid_proj$wkt, method = "near") %>%
  terra::mask(
    terra::vect(
      sf::st_transform(umrb_states)
    )
  )

nlcd <- FedData::get_nlcd_annual(umrb_states, "umrb")

nlcd_out <- nlcd %>%
  purrr::pluck("rast") %>% 
  purrr::pluck(1) %>%
  terra::project(hill) %>%
  terra::classify(
    matrix(c(
      1, 31, 0,
      32, 43, 1,
      44, 72, 2,
      73, 74, 0,
      80, 82, 3,
      83, Inf, 0
    ), ncol=3, byrow=TRUE),
    include.lowest = TRUE
  ) %>%
  terra::subst( 
    from = c(0, 1, 2, 3),
    to = c(NA, "Forest", "Rangeland", "Cultivated Agriculture")
  ) %>%
  terra::mask(
    terra::vect(
      sf::st_transform(umrb_states)
    )
  )



to_install <- umrb_grid %>%
  sf::st_join(stations, join = sf::st_contains) %>% 
  sf::st_centroid() %>%
  dplyr::select(-cell) %>% 
  dplyr::filter(is.na(stid)) %>%
  dplyr::mutate(stid = tidyr::replace_na(stid, "to_install"))

stations <- dplyr::bind_rows(stations, to_install) %>%
  dplyr::mutate(
    active = dplyr::case_when(
      stid == "to_install" ~ "Future Installation",
      TRUE ~ "Operational"
    )
  ) %>% 
  dplyr::select(-stid)

p <- ggplot(sf::st_transform(umrb_states, umrb_grid_proj)) +
  geom_tile(data = nlcd_out %>%
                raster::raster() %>% 
                get_df() %>% 
                dplyr::filter(!is.na(value)),
              mapping = aes(x=x,
                            y=y,
                            fill=value),
            alpha = 0.6) + 
  geom_raster(data = hill %>%
                raster::raster() %>%
                get_df(),
              mapping = aes(x = x,
                            y = y,
                            alpha = ID),
              na.rm = TRUE) +
  scale_alpha(range = c(0.1, 0),
              na.value = 0,
              limits = c(0,255),
              guide = "none") +
  scale_fill_manual(
    values = c(
      "Forest" = "#4daf4a",        # Deep forest green
      "Rangeland" = "#ff7f00",     # Warm grassland yellow
      "Cultivated Agriculture" = "#ffff33"  # Agricultural brown
    ),
    name = "Land Cover",           # Legend title
    na.value = "white"            # NA values will be white
  ) +
  ggnewscale::new_scale_fill() + 
  geom_sf(data = umrb_states,
          fill = NA, 
          color="black") +
  geom_sf(data = umrb,
          aes(color = "Upper Missouri River Basin"),  # Add this aes mapping
          fill = "white",
          alpha = 0.2,
          size=2) +
  scale_color_manual(
    name = "",  # Empty name to avoid a legend title
    values = c("Upper Missouri River Basin" = "grey30"),
    guide = "none"
  ) +
  geom_sf(data = stations, mapping = aes(shape = active), size = 1.0, alpha = 1, color="black") +
  scale_shape_manual(
    values = c("Operational" = 20, "Future Installation" = 3),  # Green for active, red for inactive
    name = "Mesonet Stations"
  ) +
  ggnewscale::new_scale_fill() + 
  # TCU layer with shared legend for "Partners and Universities"
  geom_sf(data = tcus, aes(fill = "Tribal Colleges and Universities"), 
          color = "black", alpha = 1, size = 2.2, shape=23) +
  
  # Universities layer with shared legend for "Partners and Universities"
  geom_sf(data = universities, aes(fill = "State Climate Offices"), 
          shape = 24, size = 2.2, color = "black", alpha = 1) +
  
  scale_fill_manual(
    name = "Partners and Universities",  # Legend title
    values = c(
      "Tribal Colleges and Universities" = "blue",  # Blue for TCUs
      "State Climate Offices" = "gold"  # Gold for universities
    )
  ) +
  coord_sf(expand = TRUE,
           xlim = c(-1715671.1, 489292.2),
           ylim = c(1887033.0, 3059862.0)) +
  theme_void() + 
  theme(
    legend.title.position = "top",
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title.align = 0.5,
    legend.direction = "vertical",  # Makes items stack vertically within each group
    legend.box.just = "center",     # Centers the horizontal group arrangement
    legend.spacing = unit(0.5, "cm"),  # Adjust spacing between legend items
    legend.spacing.x = unit(1, "cm"),
    legend.title = element_text(face = "bold",
                                size = rel(0.8)),
    legend.text = element_text(size = rel(0.6))
  ) +
  # labs(title = "The RIDER Domain") +
  # ggplot2::scale_y_continuous(expand = expansion(mult = c(0.01,0.15))) +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  vjust = -1)
        )


gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

dev.off()
grid::grid.draw(gt) %>%
  ggsave(plot = .,
         filename = "r2i2_map.png",
         width = 5, 
         height = 3.5,
         device = ragg::agg_png,
         bg = "white",
         dpi = 1200
  )


synoptic_url <- "https://api.synopticdata.com/v2/stations/metadata"
synoptic_token <- rstudioapi::askForPassword("Synoptic Token")

resp <- httr::GET(
  synoptic_url,
  query = list(
    token = synoptic_token,
    network = "3023,3024,3025,3026,3027"
  )
) %>% 
  httr::content()

resp2 <- httr::GET(
  synoptic_url,
  query = list(
    token = synoptic_token,
    network = "3008,3006,233,3019,3002"
  )
) %>% 
  httr::content()


synoptic_to_sf <- function(resp) {
  resp %>% 
    purrr::pluck("STATION") %>% 
    purrr::map(\(x) {
      x %>%
        purrr::map(~ {if (is.null(.x)) NA else .x}) %>%
        tibble::as_tibble() %>%
        janitor::clean_names() %>% 
        dplyr::select(stid, latitude, longitude) %>%
        dplyr::distinct()
    }) %>% 
    dplyr::bind_rows() %>% 
    sf::st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>%
    sf::st_transform(umrb_grid_proj)
  
}

stations <- synoptic_to_sf(resp)

unl_stations <- tibble::tribble(
  ~station, ~lat, ~lon,
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
)

sdsu_stations <- readr::read_csv("data-raw/sdsu_stations.csv") %>% 
  janitor::clean_names()
  

ndsu_stations <- function() {
  url <- "https://ndawn.ndsu.nodak.edu/station-info.html?station=2"
  
  1:10 %>%
    purrr::map(\(x) {
      response <- httr::GET(
        url
      )
      
      # Parse the content with rvest
      html_content <- httr::content(response, as = "text") %>% rvest::read_html()
      
    })

}

html_content %>%
    html_node(css = "table#details") %>%  # CSS selector for table with id = "details"
    html_table()   

