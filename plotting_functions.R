
add_hillshade <- 
  function(){
    
    # Plot the hillshade using the "alpha hack"
    list(
      ggplot2::geom_raster(data = mcor::mt_hillshade_500m %>%
                             as.data.frame(xy = TRUE) %>%
                             dplyr::filter(!is.na(layer)),
                           mapping = aes(x = x,
                                         y = y,
                                         alpha = layer),
                           na.rm = TRUE,
                           inherit.aes = FALSE),
      scale_alpha(range = c(0.8, 0),
                  na.value = 0,
                  limits = c(0,255),
                  guide = "none")
    )
  }

add_climate_divisions <- function(){
  # Plot the climate division boundaries
  ggplot2::geom_sf(data = mcor::mt_climate_divisions_simple,
                   fill = NA,
                   color = "white",
                   size = 0.5,
                   inherit.aes = FALSE)
}

mtd_theme_map <- function(base_size = 6.5, 
                          base_family = ""){
  ggplot2::theme_bw(base_size = base_size, 
                    base_family = base_family) %+replace%
    ggplot2::theme(axis.line = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = 'transparent'),
                   panel.grid.minor = ggplot2::element_line(colour = 'transparent'),
                   legend.justification = c(0,1),
                   legend.position = c(0.41, 0.24),
                   legend.background = ggplot2::element_blank(),
                   legend.key.width = unit(8,"pt"),
                   legend.key.height = unit(8,"pt"),
                   legend.text = element_text(size = unit(8, "pt")),
                   legend.title = element_text(size = unit(10, "pt"), face = "bold", hjust = 0),
                   plot.background = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "npc")
    )
}

mtd_plot <- function(legend = TRUE){
  list(
    # add_hillshade(),
    # add_climate_divisions(),
    if(legend) ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.01))),
    if(legend) ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.16, 0.01))),
    ggplot2::labs(x = NULL, y = NULL),
    mtd_theme_map()
  )
}

expansion <- list(left = 0.01,
                  right = 0.01,
                  bottom = 0.16,
                  top = 0.01)

mt_plot_ratio <- 
  mcor::mt_state %>%
  sf::st_bbox()  %>%
  as.list() %$% {
    ((ymax - ymin) +
       ((ymax - ymin) * expansion$bottom) + 
       ((ymax - ymin) * expansion$top)) / 
      ((xmax - xmin) + 
         ((xmax - xmin) * expansion$right) + 
         ((xmax - xmin) * expansion$left))
  }

fig_width <- 6.5
fig_height <- fig_width * mt_plot_ratio
# fig_height <- 2.5
# fig_width <- fig_height / mt_plot_ratio

