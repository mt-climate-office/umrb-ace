library(magrittr)
library(ggplot2)
library(terra)
source("./data-raw/all_umrb_mesonet_stations.R")

states <-
  tigris::states(cb = TRUE) %>%
  dplyr::filter(!(STUSPS %in% c("PR","AK","HI","GU","MP", "AS","VI"))) %>%
  sf::st_transform("EPSG:5070")

umrb <- 
  sf::read_sf("data/umrb.geojson") %>%
  sf::st_transform("EPSG:5070") %>%
  rmapshaper::ms_simplify()

umrb_states <-
  states %>%
  dplyr::filter(STUSPS %in% c("MT", "WY", "ND", "SD", "NE")) %>%
  dplyr::select(NAME)

umrb_roi <- sf::st_union(umrb, umrb_states) %>% 
  sf::st_union()

stations <- get_all_stations() %>% 
  sf::st_transform("EPSG:5070") %>%
  sf::st_intersection(umrb_roi) 


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
  sf::st_transform("EPSG:5070") %>%
  sf::st_intersection(umrb_roi)


universities <- tibble::tribble(
  ~place, ~lat, ~lon,
  "um", 46.8619656963864, -113.98467560181538,
  "ndsu", 46.89774315419438, -96.80250322729643,
  "sdsu", 44.31911391577087, -96.78354850796151,
  "wy", 41.31463717100004, -105.56901693286915,
  "unl", 40.82015677983608, -96.70050968433904
) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  sf::st_transform("EPSG:5070")

nlcd <- FedData::get_nlcd_annual(umrb_roi, "umrb")

nlcd_out <- nlcd %>%
  purrr::pluck("rast") %>% 
  purrr::pluck(1) %>% 
  terra::aggregate(fact=1000/30, fun="modal") %>%
  terra::project("EPSG:5070") %>%
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
      umrb_roi
    )
  )

tribes <- tigris::native_areas() %>% 
  dplyr::select(NAME) %>%
  sf::st_transform("EPSG:5070") %>% 
  sf::st_intersection(umrb_roi) %>%
  sf::st_simplify() %>% 
  sf::st_cast("POLYGON") %>% 
  dplyr::mutate(area = sf::st_area(.)%>% as.numeric()) %>% 
  dplyr::filter(area > 1000000) %>% 
  dplyr::select(-area)

get_df <- function(x){
  out <- cbind(xyFromCell(x, seq_len(ncell(x))),
               tibble::tibble(ID = raster::getValues(x))) %>%
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




p <- ggplot(sf::st_transform(umrb_states, "EPSG:5070")) +
  geom_tile(data = nlcd_out %>%
                raster::raster() %>% 
                get_df() %>% 
                dplyr::filter(!is.na(value)),
              mapping = aes(x=x,
                            y=y,
                            fill=value),
            alpha = 0.6) + 
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
  geom_sf(data = umrb_states,
          fill = NA, 
          color="black",
          size=0) +
  geom_sf(
    data=tribes,
    fill = NA,
    color = "black",
    size = 3
  ) +
  geom_sf(data = umrb,
          aes(color = "Upper Missouri River Basin"),  # Add this aes mapping
          fill = "white",
          alpha = 0.2,
          color = "grey30",
          size=2) +
  geom_sf(data = stations, mapping = aes(shape = active), size = 0.75, alpha = 1, color="black") +
  scale_shape_manual(
    values = c("Operational" = 20, "Future Installation" = 3),  # Green for active, red for inactive
    name = "Mesonet Stations"
  ) +
  ggnewscale::new_scale_fill() + 
  # TCU layer with shared legend for "Partners and Universities"
  geom_sf(data = tcus, aes(fill = "Tribal Colleges\n& Universities"), 
          color = "black", alpha = 1, size = 2.2, shape=23) +
  
  # Universities layer with shared legend for "Partners and Universities"
  geom_sf(data = universities, aes(fill = "State Climate Offices,\nMesonets\n& Universities"), 
          shape = 24, size = 2.2, color = "black", alpha = 1) +
  
  scale_fill_manual(
    name = "Partners",  # Legend title
    values = c(
      "Tribal Colleges\n& Universities" = "blue",  # Blue for TCUs
      "State Climate Offices,\nMesonets\n& Universities" = "gold"  # Gold for universities
    )
  ) +
  coord_sf(expand = TRUE,
           xlim = c(-1715671.1, 489292.2),
           ylim = c(1887033.0, 3059862.0),
           crs = 5070) +
  theme_void() +
  theme(
    legend.position = "right", 
    legend.box.spacing = unit(0, "pt"),
    legend.margin=margin(0,0,0,0),    
    legend.spacing.y = unit(0.2, "cm"), 
    legend.spacing.x = unit(0.2, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(0.6, "cm"),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7, face="bold"),
    legend.box.margin = margin(20, 20, 0, -75)
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
