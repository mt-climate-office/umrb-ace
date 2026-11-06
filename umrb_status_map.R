library(magrittr)
library(ggplot2)
library(terra)

# A UMRB-wide station status map in the style of mt_status_map.R.
# Grid cell status comes from the USACE Site Status Database.

status_database <- "data-raw/Site Status Database 30 AUG 2026.xlsx"

umrb_state_abbs <- c("MT", "WY", "ND", "SD", "NE")

# Custom Albers parameterization from Alex Stum, 2021-10-20
umrb_grid_proj <-
  list(proj = "aea",
       lat_0 = 41.8865,
       lat_1 = 43.0,
       lat_2 = 47.8,
       lon_0 = -104.0487,
       units = "m") %>%
  {paste0("+",names(.),"=",., collapse = " ")} %>%
  sf::st_crs()

status_levels <- c("Operational",
                   "In Progress",
                   "Future")

# Grid Cell IDs are inconsistently formatted in the database
# (e.g., "M-03", "M15", "M-3"); normalize to "M-3"
normalize_cell <- function(x){
  x %>%
    stringr::str_trim() %>%
    stringr::str_match("^([A-Za-z]+)[- ]*0*(\\d+)$") %>%
    {paste0(.[,2], "-", .[,3])}
}

site_status <-
  # suppress warnings about date cells in unused columns
  suppressWarnings(readxl::read_excel(status_database)) %>%
  # the station name column header changes between database versions
  dplyr::rename(Site = dplyr::starts_with("Site/Station Name")) %>%
  dplyr::filter(`Station Status` %in% c("Complete", "In-Progress", "Candidate")) %>%
  dplyr::transmute(Site,
                   Cell = normalize_cell(`Grid Cell ID`),
                   Network = Ownership,
                   status = dplyr::recode(`Station Status`,
                                          Complete = "Operational",
                                          `In-Progress` = "In Progress",
                                          # candidate sites are shown with unassigned cells as "Future"
                                          Candidate = "Future") %>%
                     factor(levels = status_levels,
                            ordered = TRUE),
                   Longitude,
                   Latitude) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4326) %>%
  sf::st_transform(umrb_grid_proj)

states <-
  tigris::states(cb = TRUE) %>%
  dplyr::filter(STUSPS %in% umrb_state_abbs) %>%
  sf::st_transform(umrb_grid_proj) %>%
  rmapshaper::ms_simplify()

counties <-
  tigris::counties(state = umrb_state_abbs,
                   cb = TRUE) %>%
  sf::st_transform(umrb_grid_proj) %>%
  rmapshaper::ms_simplify()

# Each cell takes the most advanced status of any station within it
grid <-
  sf::read_sf("data/umrb-ace-grid.geojson") %>%
  sf::st_transform(umrb_grid_proj) %>%
  rmapshaper::ms_simplify() %>%
  dplyr::left_join(site_status %>%
                     sf::st_drop_geometry() %>%
                     dplyr::group_by(Cell) %>%
                     dplyr::summarise(status = min(status)),
                   by = "Cell") %>%
  dplyr::mutate(status =
                  status %>%
                  as.character() %>%
                  tidyr::replace_na("Future") %>%
                  factor(levels = status_levels,
                         ordered = TRUE))

status_counts <-
  grid %>%
  sf::st_drop_geometry() %>%
  dplyr::count(status, .drop = FALSE) %$%
  magrittr::set_names(paste0(status, " (", n, ")"), status)

# Logo heights (in inches) are set by eye so that no logo dominates and
# small text (e.g., NDAWN's subtitle, CIRE's full name) stays legible.
read_logo <- function(file){
  img <- png::readPNG(file)
  # trim fully transparent borders
  if(dim(img)[3] == 4){
    opaque <- img[,,4] > 0
    img <- img[which(rowSums(opaque) > 0) %>% range() %>% {.[1]:.[2]},
               which(colSums(opaque) > 0) %>% range() %>% {.[1]:.[2]}, ,
               drop = FALSE]
  }
  img
}

# The five state mesonets, staggered through the open space left of the map.
# x and y are the logo centers, as a fraction of the full figure.
logo_scatter <- function(logos){
  purrr::pmap(list(purrr::map(logos$file, read_logo), logos$x, logos$y, logos$height),
              \(img, x, y, height){
                grid::rasterGrob(img,
                                 interpolate = TRUE,
                                 x = unit(x, "npc"),
                                 y = unit(y, "npc"),
                                 height = unit(height, "in"))
              }) %>%
    do.call(grid::gList, .) %>%
    grid::gTree(children = .)
}

# Funders and the University of Montana, in a strip below the map
logo_strip <- function(logos, gap = 0.45){
  imgs <- purrr::map(logos$file, read_logo)
  widths <- logos$height * purrr::map_dbl(imgs, ~ ncol(.x) / nrow(.x))
  lefts <- cumsum(c(0, head(widths, -1) + gap)) - (sum(widths) + gap * (length(widths) - 1)) / 2
  
  purrr::pmap(list(imgs, lefts, logos$height),
              \(img, left, height){
                grid::rasterGrob(img,
                                 interpolate = TRUE,
                                 x = unit(0.5, "npc") + unit(left, "in"),
                                 height = unit(height, "in"),
                                 just = "left")
              }) %>%
    do.call(grid::gList, .) %>%
    grid::gTree(children = .)
}

mesonet_logos <-
  tibble::tribble(
    ~file,                  ~x,    ~y,    ~height,
    "MCO_logo.png",         0.14,  0.60,  0.68,
    "logos/ndawn.png",      0.255, 0.485, 0.29,
    "logos/sd_mesonet.png", 0.125, 0.39,  0.42,
    "logos/ne_mesonet.png", 0.27,  0.28,  0.46,
    "logos/wy_mesonet.png", 0.125, 0.175, 0.9
  )

partner_logos <-
  tibble::tribble(
    ~file,             ~height,
    "logos/noaa.png",  0.55,
    "logos/usace.png", 0.5,
    "UM-Maroon.png",   0.32,
    "logos/cire.png",  0.7
  )

# USGS shaded relief, fetched to match the extent and resolution of x
get_image_rast <-
  function(x, url = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSShadedReliefOnly/MapServer/export"){
    query <- list(bbox = paste0(sf::st_bbox(x), collapse = ","),
                  bboxSR = terra::crs(x, describe = TRUE)$code,
                  size = paste0(terra::ncol(x), ",", terra::nrow(x)),
                  imageSR = terra::crs(x, describe = TRUE)$code)

    deets <-
      httr::GET(url, query = c(query, f = "json")) %>%
      httr::content()

    httr::modify_url(url, query = c(query, f = "image")) %>%
      terra::rast() %T>%
      terra::set.crs(terra::crs(x)) %T>%
      terra::set.ext(unlist(deets$extent[c("xmin","xmax","ymin","ymax")])) %>%
      magrittr::extract2(1) %>%
      magrittr::set_names("layer")
  }

hill <-
  states %>%
  sf::st_transform("EPSG:5070") %>%
  sf::st_buffer(20000) %>%
  terra::vect() %>%
  terra::rast(resolution = 1000) %>%
  get_image_rast() %>%
  terra::project(umrb_grid_proj$wkt, method = "near") %>%
  terra::mask(terra::vect(sf::st_union(sf::st_union(states), sf::st_union(grid))))

p <-
  ggplot(states) +
  geom_sf(data = grid,
          mapping = aes(fill = status)) +
  scale_fill_manual(name = "UMRB Mesonet\nStation Status",
                    values = c(
                      Operational = "#009988FF",
                      `In Progress` = "#0077BBFF",
                      Future = "#EE7733FF"
                    ),
                    labels = status_counts,
                    drop = FALSE
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
  geom_sf(data = counties,
          fill = NA,
          color = "white",
          linewidth = 0.15) +
  geom_sf(data = site_status %>%
            dplyr::filter(status == "Operational"),
          mapping = aes(shape = "Operational Station"),
          size = 1,
          fill = "black",
          color = "white",
          stroke = 0.3) +
  scale_shape_manual(values = c(`Operational Station` = 21),
                     name = NULL) +
  guides(
    fill = guide_legend(order = 1),
    shape = guide_legend(order = 2,
                         override.aes = list(size = 3))
  ) +
  geom_sf(data = states,
          fill = NA,
          linewidth = 0.5) +
  theme_void() +
  theme(
    legend.position = "inside",
    legend.title = element_text(face = "bold",
                                size = rel(1.5)),
    legend.text = element_text(size = rel(1)),
    legend.justification = c(0, 1),
    legend.position.inside = c(-0.2, 1),
    legend.background = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(t = 0.03,
                                  r = 0,
                                  b = 0,
                                  l = 0.2, unit = "npc")
  ) +
  annotation_custom(
    gridtext::textbox_grob(
      text = paste0("Station status from the UMRB Site Status Database (",
                    basename(status_database) %>%
                      stringr::str_extract("\\d+ \\w+ \\d{4}"),
                    ").<br>
      Map produced by the Montana Climate Office on ", format(Sys.Date(), "%B %d, %Y"), "."),
      x = unit(0.98, "npc"),
      y = unit(0, "npc"),
      width = unit(0.6, "npc"),
      hjust = 1,
      vjust = 0,
      halign = 1,
      valign = 1,
      gp = grid::gpar(fontface = "italic",
                      fontsize = 6)
    ),
    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  coord_sf(clip = "off")

gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

# Add the partner logo strip as its own row beneath the map
gt <-
  gt %>%
  gtable::gtable_add_rows(unit(0.85, "in"), pos = -1) %>%
  gtable::gtable_add_grob(logo_strip(partner_logos),
                          t = -1, l = 1, r = -1,
                          clip = "off",
                          name = "partner_logos") %>%
  gtable::gtable_add_grob(logo_scatter(mesonet_logos),
                          t = 1, b = -1, l = 1, r = -1,
                          clip = "off",
                          name = "mesonet_logos")

ggsave(plot = gt,
       filename = "umrb_status_map.png",
       width = 10,
       height = 6,
       device = ragg::agg_png,
       bg = "white",
       dpi = 1200
)

ggsave(plot = gt,
       filename = "umrb_status_map.pdf",
       width = 10,
       height = 6,
       device = cairo_pdf,
       bg = "white"
)
