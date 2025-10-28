library(tidyverse)
library(magrittr)
library(ggplot2)
library(mcor)
library(terra)


mt_station_status <- readr::read_csv(
  "https://mesonet.climate.umt.edu/api/v2/stations/?type=csv&active=false",
  show_col_types=FALSE
) %>%
  dplyr::filter(
    sub_network == "HydroMet",
    date_installed >= as.Date("2024-01-01")
  ) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 

tribes <- rmapshaper::ms_simplify(sf::read_sf("https://data.climate.umt.edu/mesonet/fgb/mt_tribes.fgb"))



hill <- 
  mcor::mt_hillshade_500m %>%
  terra::rast()

mt_towns <-
  tribble(
    ~Name,        ~vjust, ~hjust,
    "Libby",        -0.3,    0.5,
    # "Whitefish",    -0.3,    0.0,
    "Kalispell",     -0.3,    0.3,
    # "Polson",        1.3,    0.7,
    "Missoula",     1.3,    0.3,
    # "Cut Bank",     0.5,    -0.1,
    # "Hamilton",     -0.3,    0.1,
    "Butte",        -0.3,    0.5,
    "Dillon",       -0.3,    1.0,
    "Helena",       -0.3,    0.5,
    "Great Falls",  1.3,    0.7,
    "Havre",        0.5,    1.1,
    "Bozeman",      -0.3,    0.5,
    "Lewistown",    1.3,    0.2,
    "Billings",     -0.3,    0.3,
    "Miles City",    1.3,    0.5,
    "Glendive",     -0.3,    0.7,
    "Wolf Point",   1.3,    0.3,
    # "Gardiner",     -0.3,    0.3
  )


mt_towns <-
  sf::read_sf("/vsicurl/https://ftpgeoinfo.msl.mt.gov/Data/Spatial/MSDI/GeographicNames/MT_Names_gdb.zip",
              layer = "MT_Names") %>%
  dplyr::filter(Class == "Populated Place") %>%
  dplyr::right_join(mt_towns) %>%
  sf::st_transform(mcor::mt_state_plane)


p <-
  ggplot() +
  geom_sf(data = mcor::mt_state_simple,
          fill = "white",
          linewidth = 0) +
  geom_sf(data = tribes, 
          fill = "#70002e",
          linewidth = 0,
          alpha = 0.15,
          color = NA) + 
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
  geom_sf(data = mcor::mt_state_simple,
          fill = NA,
          linewidth = 0.5) +
  geom_sf(data = mt_station_status,
          shape = 24,
          size = 3,
          fill = "black",
          color = "white",
          linewidth = 1
  ) +
  geom_sf(data = mt_towns,
          shape = 21,
          fill = "black",
          color = "white",
  ) +
  geom_sf_label(
    data = mt_towns,
    aes(label = Name,
        vjust = vjust,
        hjust = hjust),
    fontface = "bold",
    color = "black",
    fill = "white",
    size = 4,
    label.size = 0,
    alpha = 0.5,
    label.padding = unit(0.25, "lines"),
    # position = position_dodge(0.9),

  ) +
  guides(
    fill = guide_legend(order = 1),
    shape = guide_legend(
      override.aes = list(size = 3),
      theme = ggplot2::theme(
        legend.title = element_text(face = "bold",
                                    size = rel(1.05))
      )
    )
  ) + 
  # ggrepel::geom_label_repel(
  #   data = mt_station_status,
  #   aes(label = name, geometry = geometry),
  #   stat = "sf_coordinates",
  #   min.segment.length = 0,
  #   size = 3,
  #   label.size = 0.05,
  #   max.time = 1, max.iter = 30000,
  #   alpha = 1,
  #   max.overlaps = 10,          # Reduce to show fewer overlapping labels
  #   force = 10,                # Increase repulsion between labels
  #   force_pull = 0.1,          # Pull labels slightly toward their points
  #   box.padding = 0.5,         # Increase padding around labels
  # )  +
  theme_void() +
  theme(
    legend.position = "inside",
    legend.title = element_text(face = "bold",
                                size = rel(1.2)),
    legend.text = element_text(size = rel(1)),
    legend.justification = c(0, 1), 
    legend.position.inside = c(-0.16, 0.9),
    legend.background = ggplot2::element_blank(), 
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
    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  annotation_custom(
    gridtext::textbox_grob(
      text = "2024 HydroMet<br>Station Installations",
      x = unit(0.17, "npc"),
      y = unit(0.39, "npc"),
      width = unit(0.40, "npc"),
      hjust = 1,
      vjust = 0,
      halign = 0.5,
      valign = 1,
      gp = grid::gpar(fontface = "bold",
                      fontsize = 18)
    ),
    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

dev.off()
grid::grid.draw(gt) %>%
  ggsave(plot = .,
         filename = "2024_hydromet_installs.png",
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

