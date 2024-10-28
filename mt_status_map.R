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
                                        `Summer 2025` = c("ground", "inactive", "pending", "structures"),
                                        `Summer 2026` = "contracted",
                                        `Future Stations` = "future"
                                        
                  ) %>%
                  factor(levels = c("Operational",
                                    "Summer 2025",
                                    "Summer 2026",
                                    "Future Stations"
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




p <-
  ggplot(mcor::mt_state_simple) +
  geom_sf(data = grid,
          mapping = aes(fill = status)) +
  scale_fill_manual(name = "Montana Mesonet\nStation Status",
                    values = c(
                      Operational = "#009988FF",
                      `Summer 2025` = "#0077BBFF",
                      `Summer 2026` = "#33BBEEFF",
                      `Future Stations` = "#EE7733FF"
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
  guides(fill = guide_legend(order = 1)) +
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
    legend.justification = c(0, 1), 
    legend.position.inside = c(-0.16, 0.9),
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
  ) +
  annotation_custom(
    "MCO_logo.png" %>%
      png::readPNG() %>%
      grid::rasterGrob(interpolate=TRUE,
                       # width = unit(0.6 * 3600/1325, "in"),
                       height = unit(0.75, "in"),
                       x = unit(-0.16, "npc"),
                       y = unit(0.28, "npc"),
                       just = "left"), 
    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  annotation_custom(
    "UM-Maroon.png" %>%
      png::readPNG() %>%
      grid::rasterGrob(interpolate=TRUE,
                       # width = unit(0.75, "in"),
                       height = unit(0.5, "in"),
                       x = unit(-0.16, "npc"),
                       y = unit(0.11, "npc"),
                       just = "left"), 
    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  annotation_custom(
    gridtext::textbox_grob(
      text = paste0("The Montana Mesonet is operated by the Montana Climate Office, 
      and is a service of the Montana Forest and Conservation Experiment Station, 
      WA Franke College of Forestry and Conservation, University of Montana.<br>
      Map produced on ", format(today(), "%B %d, %Y"), "."),
      x = unit(0.94, "npc"),
      y = unit(0.05, "npc"),
      width = unit(0.53, "npc"),
      hjust = 1,
      vjust = 0,
      halign = 1,
      valign = 1,
      gp = grid::gpar(fontface = "italic",
                      fontsize = 6)
    ),
    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

dev.off()
grid::grid.draw(gt) %>%
ggsave(plot = .,
       filename = "mt_status_map.png",
       width = 10, 
       height = 14/3,
       device = ragg::agg_png,
       bg = "white",
       dpi = 1200
)

# grid::grid.draw(gt) %>%
#   ggsave(plot = .,
#          filename = "mt_status_map.pdf",
#          width = 10, 
#          height = 14/3,
#          device = cairo_pdf,
#          bg = "white",
#          dpi = 1200
#   )

