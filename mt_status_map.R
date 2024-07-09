library(tidyverse)
library(magrittr)
library(ggplot2)
library(mcor)
library(terra)


mt_station_status <-
  dplyr::left_join(
    readr::read_csv("https://mesonet.climate.umt.edu/api/v2/stations/?type=csv&active=false") %>%
      dplyr::select(station, name, sub_network, longitude, latitude),
    readr::read_csv("https://mesonet.climate.umt.edu/api/v2/stations/status?type=csv") %>%
      dplyr::select(station, status, ace_grid),
    by = c("station")
  ) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4326) %>%
  sf::st_transform(mcor::mt_state_plane) %>%
  dplyr::mutate(status = 
                  factor(status,
                         levels = c("pending",
                                    "active",
                                    "decommissioned",
                                    "inactive",
                                    "ground",
                                    "structures",
                                    "contracted",
                                    "future")),
                sub_network = 
                  factor(sub_network,
                         levels = c("HydroMet",
                                    "AgriMet"),
                         ordered = TRUE)
  ) %>%
  dplyr::filter(!(status %in% c("decommissioned")))

hill <- 
  mcor::mt_hillshade_500m %>%
  terra::rast()

grid <-
  sf::read_sf("data/umrb-ace-grid.geojson") %>%
  dplyr::filter(State == "MT") %>%
  sf::st_transform(mcor::mt_state_plane) %>%
  rmapshaper::ms_simplify() %>%
  dplyr::left_join(mt_station_status %>%
                     sf::st_drop_geometry() %>%
                     dplyr::select(Cell = ace_grid,
                                   status)) %>%
  dplyr::mutate(status = 
                  status %>%
                  tidyr::replace_na("future") %>%
                  forcats::fct_collapse(Operational = "active",
                                        `Install 2024` = c("ground", "inactive", "pending"),
                                        `Install 2025` = "contracted",
                                        Unassigned = "future"
                                        
                  ) %>%
                  factor(levels = c("Operational",
                                    "Install 2024",
                                    "Install 2025",
                                    "Unassigned"
                  ),
                  ordered = TRUE)
  )

mt_towns <-
  tribble(
    ~Name,        ~vjust, ~hjust,
    "Libby",        -0.3,    0.5,
    # "Whitefish",    -0.3,    0.0,
    "Kalispell",     1.3,    0.3,
    # "Polson",        1.3,    0.7,
    "Missoula",     -0.3,    0.5,
    "Cut Bank",     -0.3,    0.5,
    # "Hamilton",     -0.3,    0.1,
    "Butte",        -0.3,    0.5,
    "Dillon",       -0.3,    1.0,
    "Helena",       -0.3,    0.5,
    "Great Falls",  -0.3,    0.7,
    "Havre",        -0.3,    0.5,
    "Bozeman",      -0.3,    0.5,
    "Lewistown",    -0.3,    0.4,
    "Billings",     -0.3,    0.3,
    "Miles City",    1.3,    0.5,
    "Glendive",     -0.3,    0.7,
    "Wolf Point",   -0.3,    0.7,
    # "Gardiner",     -0.3,    0.3
  )


mt_towns <-
  sf::read_sf("/vsicurl/https://ftpgeoinfo.msl.mt.gov/Data/Spatial/MSDI/GeographicNames/MT_Names_gdb.zip",
              layer = "MT_Names") %>%
  dplyr::filter(Class == "Populated Place") %>%
  dplyr::right_join(mt_towns) %>%
  sf::st_transform(mcor::mt_state_plane)


"#0077BBFF"
"#33BBEEFF"
"#009988FF"
"#EE7733FF"
"#CC3311FF"
"#EE3377FF"
"#BBBBBBFF"

ggplot(mcor::mt_state_simple) +
  geom_sf(data = grid,
          mapping = aes(fill = status)) +
  scale_fill_manual(name = "UMRB Project\nStation Status",
                    values = c(
                      Operational = "#009988FF",
                      `Install 2024` = "#0077BBFF",
                      `Install 2025` = "#33BBEEFF",
                      Unassigned = "#EE7733FF"
                    )
  ) +
  geom_raster(data = terra::as.data.frame(hill, xy = TRUE),
              mapping = aes(x = x,
                            y = y,
                            alpha = layer),
              na.rm = TRUE) +
  scale_alpha(range = c(0.8, 0),
              na.value = 0,
              limits = c(0,255),
              guide = "none") +
  
  geom_sf(data = mcor::mt_counties_simple,
          fill = NA,
          color = "white",
          linewidth = 0.3) +
  geom_sf(data = mt_station_status %>%
            dplyr::arrange(dplyr::desc(sub_network)),
          mapping = aes(shape = sub_network,
                        size = sub_network),
          # shape = 21,
          fill = "black",
          color = "white",
  ) +
  scale_shape_manual(
    values = c(
      HydroMet = 21,
      AgriMet = 24
    ),
    name = "Subnetwork") +
  scale_size_manual(values = c(
    HydroMet = 1.5,
    AgriMet = 1
  ),
  name = "Subnetwork") +
  guides(
    shape = guide_legend(
      override.aes = list(size = 3),
      theme = ggplot2::theme(
        legend.title = element_text(face = "bold",
                                    size = rel(1.05))
      )
    )
  ) + 
  
  # geom_sf(data = mcor::mt_tribal_land,
  #         fill = NA,
  #         color = "black",
  #         linewidth = 1) +
  
  geom_sf(data = mcor::mt_state_simple,
          fill = NA,
          linewidth = 0.5) +
  # geom_sf(data = mt_towns,
  #         shape = 21,
  #         fill = "black",
  #         color = "white",
  # ) +
  # geom_sf_label(
  #   data = mt_towns,
  #   aes(label = Name,
  #       vjust = vjust,
  #       hjust = hjust),
  #   fontface = "bold",
  #   color = "black",
  #   fill = "white",
  #   size = 4,
  #   label.size = 0,
  #   alpha = 0.5,
  #   label.padding = unit(0.25, "lines"),
  #   # position = position_dodge(0.9),
  #   
  # ) +
  
  
  theme_void() +
  theme(
    # legend.direction = "horizontal",
    legend.position = "inside",
    legend.title = element_text(face = "bold",
                                size = rel(1.2)),
    legend.text = element_text(size = rel(1)),
    # legend.title.position = "top",
    # legend.text.position = "bottom",
    # legend.background = element_blank(),
    # legend.justification = c(0,1),
    # legend.key.width = unit(0.4, "in"),
    legend.justification = c(0, 0), 
    legend.position.inside = c(-0.16, 0.07),
    legend.background = ggplot2::element_blank(), 
    # legend.key.width = ggplot2::unit(0.15, "in"), 
    # legend.text = ggplot2::element_text(size = ggplot2::rel(1)),
    plot.title = element_text(hjust = 0.5, 
                              vjust = -1,
                              face = "bold",
                              size = rel(2)),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = ggplot2::margin(t = 0, 
                                  r = 0, 
                                  b = 0,
                                  l = 0.16, unit = "npc")
  )



ggsave("mt_status_map.png",
       width = 7.5, 
       height = 3.5,
       device = ragg::agg_png,
       bg = "white"
)


