library(magrittr)
library(ggplot2)
library(terra)

# A UMRB-wide station status map in the style of mt_status_map.R.
# Grid cell status comes from the USACE Site Status Database.
# A second map shows only the stations sited within American Indian
# reservations, with the count of stations in each reservation.

status_database <- "data-raw/Site Status Database 28 OCT 2026.xlsx"

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

# Federally recognized reservations (Census AIANNH TIGER/Line boundaries),
# clipped to the five states. Off-reservation trust lands (COMPTYP "T") are
# excluded; the cartographic boundary files merge them into the reservations,
# so the full TIGER/Line file is used to separate them.
reservations <-
  tigris::native_areas(cb = FALSE, year = 2024) %>%
  dplyr::filter(COMPTYP == "R") %>%
  sf::st_transform(umrb_grid_proj) %>%
  sf::st_make_valid() %>%
  sf::st_intersection(sf::st_union(states)) %>%
  rmapshaper::ms_simplify(keep_shapes = TRUE) %>%
  dplyr::mutate(short_name =
                  NAMELSAD %>%
                  stringr::str_remove(" (Indian )?Reservation$")) %>%
  dplyr::select(NAMELSAD, short_name)

# "Sited on tribal land" means inside a reservation boundary, regardless of
# the parcel ownership recorded in the database
tribal_sites <-
  site_status %>%
  sf::st_join(reservations, join = sf::st_intersects, left = FALSE)

reservation_labels <-
  reservations %>%
  dplyr::inner_join(tribal_sites %>%
                      sf::st_drop_geometry() %>%
                      dplyr::count(NAMELSAD),
                    by = "NAMELSAD") %>%
  sf::st_point_on_surface() %>%
  dplyr::mutate(label = paste0(short_name, " (", n, ")"))

# Tribal colleges and universities, placed at the Census place centroid of
# their town (as in data.R and r2i2_figure.R)
tcu_color <- "#882255"

tribal_colleges <-
  readr::read_csv("data-raw/tcus.csv", show_col_types = FALSE) %>%
  dplyr::filter(State %in% umrb_state_abbs) %>%
  dplyr::inner_join(tigris::places(state = umrb_state_abbs, cb = TRUE, year = 2024) %>%
                      dplyr::select(Town = NAME, State = STUSPS),
                    by = c("Town", "State")) %>%
  sf::st_as_sf() %>%
  sf::st_transform(umrb_grid_proj) %>%
  sf::st_centroid() %>%
  dplyr::mutate(label = stringr::str_wrap(`Tribal College`, width = 20))

# Shared label styling: reservations in boxes, colleges as colored text
map_labels <- function(labels){
  ggrepel::geom_label_repel(data = labels,
                            mapping = aes(label = label,
                                          geometry = geometry,
                                          colour = I(color),
                                          linewidth = I(box)),
                            stat = "sf_coordinates",
                            size = 2.1,
                            lineheight = 0.9,
                            label.padding = unit(0.12, "lines"),
                            fill = scales::alpha("white", 0.85),
                            segment.size = 0.25,
                            segment.colour = "black",
                            min.segment.length = 0,
                            box.padding = 0.4,
                            max.overlaps = Inf,
                            seed = 1,
                            show.legend = FALSE)
}

college_labels <-
  tribal_colleges %>%
  dplyr::transmute(label, color = tcu_color, box = 0)

college_layer <-
  geom_sf(data = tribal_colleges,
          mapping = aes(shape = "Tribal College or University"),
          size = 2,
          fill = tcu_color,
          color = "white",
          stroke = 0.3)

status_counts <-
  grid %>%
  sf::st_drop_geometry() %>%
  dplyr::count(status, .drop = FALSE) %$%
  magrittr::set_names(paste0(status, " (", n, ")"), status)

tribal_status_counts <-
  tribal_sites %>%
  sf::st_drop_geometry() %>%
  dplyr::count(status, .drop = FALSE) %$%
  magrittr::set_names(paste0(status, " (", n, ")"), status)

status_colors <- c(
  Operational = "#009988FF",
  `In Progress` = "#0077BBFF",
  Future = "#EE7733FF"
)

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

# Funders and the University of Montana, in a strip below the map.
# Logos are centered vertically in the strip unless `bottom` (inches above
# the bottom of the figure) is given.
logo_strip <- function(logos, gap = 0.45){
  imgs <- purrr::map(logos$file, read_logo)
  widths <- logos$height * purrr::map_dbl(imgs, ~ ncol(.x) / nrow(.x))
  lefts <- cumsum(c(0, head(widths, -1) + gap)) - (sum(widths) + gap * (length(widths) - 1)) / 2
  
  purrr::pmap(list(imgs, lefts, logos$height, logos$bottom),
              \(img, left, height, bottom){
                grid::rasterGrob(img,
                                 interpolate = TRUE,
                                 x = unit(0.5, "npc") + unit(left, "in"),
                                 y = if(is.na(bottom)) unit(0.5, "npc") else unit(bottom, "in"),
                                 height = unit(height, "in"),
                                 just = c("left", if(is.na(bottom)) "centre" else "bottom"))
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
    ~file,             ~height, ~bottom,
    "logos/noaa.png",  0.6,     NA,
    "logos/usace.png", 0.5,     NA,
    # CIRE is the tallest; sit it on the same line as USACE's bottom
    "logos/cire.png",  0.7,     0.175,
    "UM-Maroon.png",   0.42,    NA
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

hill_layers <- list(
  geom_raster(data = terra::as.data.frame(hill, xy = TRUE),
              mapping = aes(x = x,
                            y = y,
                            alpha = layer),
              na.rm = TRUE),
  scale_alpha(range = c(0.8, 0),
              na.value = 0,
              limits = c(0,255),
              guide = "none")
)

map_theme <-
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
  )

# `drop` lowers the credit (in inches) below the bottom of the map panel
credit <- function(extra = NULL, family = "", drop = 0){
  annotation_custom(
    gridtext::textbox_grob(
      text = paste0("Station status from the UMRB Site Status Database (",
                    basename(status_database) %>%
                      stringr::str_extract("\\d+ \\w+ \\d{4}"),
                    ").<br>
      Map produced by the Montana Climate Office on ", format(Sys.Date(), "%B %d, %Y"), ".", extra),
      x = unit(0.98, "npc"),
      y = unit(0, "npc") - unit(drop, "in"),
      width = unit(0.6, "npc"),
      hjust = 1,
      vjust = 0,
      halign = 1,
      valign = 1,
      gp = grid::gpar(fontface = "italic",
                      fontsize = 6,
                      fontfamily = family)
    ),
    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
}

p <-
  ggplot(states) +
  geom_sf(data = grid,
          mapping = aes(fill = status)) +
  scale_fill_manual(name = "UMRB Mesonet\nStation Status",
                    values = status_colors,
                    labels = status_counts,
                    drop = FALSE
  ) +
  hill_layers +
  # geom_sf(data = counties,
  #         fill = NA,
  #         color = "white",
  #         linewidth = 0.15) +
  geom_sf(data = reservations,
          fill = NA,
          color = "white",
          linewidth = 0.3) +
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
  map_theme +
  coord_sf(clip = "off")

# Tribal-lands version: the full grid muted, reservations highlighted,
# and every station within a reservation colored by status
p_tribal <-
  ggplot(states) +
  geom_sf(data = grid,
          fill = "grey85",
          color = "grey65",
          linewidth = 0.1) +
  geom_sf(data = reservations,
          mapping = aes(color = "Reservation"),
          fill = "#CCBB44",
          linewidth = 0.25) +
  scale_color_manual(values = c(Reservation = "white"),
                     labels = c(Reservation = "Reservation (station count)"),
                     name = NULL) +
  hill_layers +
  # geom_sf(data = counties,
  #         fill = NA,
  #         color = "white",
  #         linewidth = 0.15) +
  geom_sf(data = reservations,
          fill = NA,
          color = "white",
          linewidth = 0.3) +
  geom_sf(data = states,
          fill = NA,
          linewidth = 0.5) +
  geom_sf(data = tribal_sites %>%
            dplyr::arrange(dplyr::desc(status)),
          mapping = aes(fill = status),
          shape = 21,
          size = 1.6,
          color = "white",
          stroke = 0.3) +
  scale_fill_manual(name = "UMRB Mesonet\non Tribal Lands",
                    values = status_colors,
                    labels = tribal_status_counts,
                    drop = FALSE
  ) +
  college_layer +
  scale_shape_manual(values = c(`Tribal College or University` = 24),
                     name = NULL) +
  # station points are passed as empty labels so the labels avoid them
  map_labels(dplyr::bind_rows(reservation_labels %>%
                                dplyr::transmute(label, color = "black", box = 0.15),
                              college_labels,
                              tribal_sites %>%
                                dplyr::transmute(label = "", color = "black", box = 0))) +
  guides(
    fill = guide_legend(order = 1,
                        override.aes = list(size = 4)),
    color = guide_legend(order = 2),
    shape = guide_legend(order = 3,
                         override.aes = list(size = 3))
  ) +
  map_theme +
  # three legend groups; close them up so they clear the MCO logo
  theme(legend.spacing.y = unit(0, "pt")) +
  coord_sf(clip = "off")

# On this Mac, systemfonts (which ragg uses) cannot match bold or italic faces
# by family name and silently falls back to the regular face. Registering
# Helvetica's faces by file under an alias makes them reachable for the PNGs.
# ragg has no PDF device; the PDFs use grDevices::pdf, which writes the
# standard PDF Helvetica faces by name, so they keep the default family.
png_family <- "MCO Helvetica"
if(!(png_family %in% systemfonts::registry_fonts()$family)){
  helvetica <-
    systemfonts::system_fonts() %>%
    dplyr::filter(family == "Helvetica")
  face <- function(style){
    f <- dplyr::filter(helvetica, style == !!style)
    list(f$path[1], f$index[1])
  }
  systemfonts::register_font(png_family,
                             plain = face("Regular"),
                             bold = face("Bold"),
                             italic = face("Oblique"),
                             bolditalic = face("Bold Oblique"))
}

# The credit is added here, not in the plot definitions, because the
# textbox fixes its font family when it is created
build_map <- function(p, credit_extra = NULL, credit_drop = 0, family = ""){
  p <- p +
    credit(credit_extra, family = family, drop = credit_drop) +
    theme(text = element_text(family = family))

  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"

  # Add the partner logo strip as its own row beneath the map
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
}

save_map <- function(p, filename, credit_extra = NULL, credit_drop = 0){
  ggsave(plot = build_map(p, credit_extra, credit_drop, family = png_family),
         filename = paste0(filename, ".png"),
         width = 10,
         height = 6,
         device = ragg::agg_png,
         bg = "white",
         dpi = 1200
  )

  ggsave(plot = build_map(p, credit_extra, credit_drop),
         filename = paste0(filename, ".pdf"),
         width = 10,
         height = 6,
         device = grDevices::pdf,
         useDingbats = FALSE,
         bg = "white"
  )
}

save_map(p, "umrb_status_map")
save_map(p_tribal, "umrb_tribal_status_map",
         credit_extra = "<br>Reservations from the U.S. Census Bureau (2024).",
         # one line lower, so the three-line credit clears Nebraska
         credit_drop = 0.1)
