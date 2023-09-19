library(mapview)
library(magrittr)
library(raster)
library(ggplot2)
library(tidycensus)
library(terra)
mapviewOptions(fgb = FALSE)

umrb_grid_proj <-
  list(proj = "aea",
       lat_0 = 41.8865,
       lat_1 = 43.0,
       lat_2 = 47.8,
       lon_0 = -104.0487,
       units = "mi") %>%
  {paste0("+",names(.),"=",., collapse = " ")} %>%
  sf::st_crs()

get_umrb_status <- function(){
  # tmp <- tempfile(fileext = ".xlsx")
  # 
  # httr::GET(url = "https://dl.boxcloud.com/d/1/b1!Y5CMqqt3mOhS4jjvCCwD2AeQmhxM9Ohom794KK5Tv3Yal0Xo7Rm9u8XyjH8pNgyey2-PIAAw6BAIbN1gdXEWpLUCkiKDkRItrmNtZ6w-_LtjTZ_5r-VMH_Tmxigc_r-hHgUSypo23hZUA_Pmi5xNsKgoVmOh1TZY0mL_0mcaxuOY1wlYeEr9YT-6kSziddnV4VRftzDF8B34VEtyE_Zi2ROL9YsgB05TPM-p-WtKYKzYQdfFvnOfU9W5QnRyrZ1QTHKuN6512QzhpoTucVt_RNwLY7lNoolMFe4sd0hyMXX1jL7OfDqprFsbzk8pzN1v0NE_PCHFhs3xY-Q9c1yoHvt72YD0mOxez-JaZmCWW7iIOTUkja8JkWKhmHdVNN8oQGfFkwRLIGCEnKMm42aP3drXe2yDuxSMKbDNKo9YQdw227Dsv56iAljJyvl41lDgkWD7KX8Pm9pXgcUFFiBgJAReRFPWfny4TOqoMISWuk4s2skcp1LfnRydeeDDrbtL3VX11iLN-CXLE_GpZdPfvfqrseLiBgoZH08mi6SD04Jt75P-8wlCKCYepkGp80zk0Addl9Bg-w7Kh-BrS8JLEmoMf1ZiTK9U8eUGTGXrEPtmK1h0n07Mdn0SoT5u4ezz-U0LsowDupojxwv1CLcpJJMK0dQNkxDuNOew8K1xrdSxghc8mfl8gOltafC29cympLIzsh-bAnpSPfQ4O1y5QJuL9pNILF9-MmS1Wmfg_mvuZL6jhGZ86ofIh_7NV90GG9mMi4JFn0-RzXKQK979FNU4-YKrbBfYMCbzKLDRm6yWBVHUBgfvkCQ0K9XW3ljZBWYtXHuGVA1koeNURK-74gZULPLdnUTuXJacuefVAB_kG_t7ImC2mO-QatJHosUFOeBitH8pGO5S7wbRyt9cMI_-fGWgd4vQ29GNbTTEXPrXFV634jVm0UFrDycMXwSoscgT53_sFtMVxeQlIeVCJaOfKjMrbZ9SqKRbTs94YhKpxu9sot8rqxLHLXD_IiPLCJHfOiVUYECReukYHQMy3FWcN8r6DI3751B279DELqR18zxn0D5paq5f9zdUJXsoKxKE6WbYTtUkSs4AP_HvIqUVNDvDnwM3J8z6fEiznL-gWPhxihS1Vg7f3yvD8gursSh4XO54Xqr-NJ1Kl9S6PSb9J564UQ_MZ-ymkRBZVbYHkHcbOkmZ_r1VkHO8uk3iz6ZdeO0ueRMsfA9IGG-juTeLzE_xh5a0REVoaR6hrAZiW0XkhJWnmC8lJRhA3_JYYgPUtUOMuvaFNnkM-zp8V7Vh62Xh-ap1fkODg7dY5drvsbDdAyGWR4hXmkY0y7ycwYm7aAALs33n3wkTwycvDB7aBgGTeNVDTEboxb3wviWamcLBvyXW03j3J39zr2SRJ_FT8Ga6rnpPPsB1SMx4IZavWKI8mSj9tNYEyOd6K7zP84PD3Uh6GLjKI2iBk8srxhvEL1ETHgQP-6zsslE2YUsL_EEl1dWViymCtfaCryZn158gtIOm7siQpAgOXMc3ayuimfAW8H4YIc9qapiVqW5qqLTywfAJJjMeBK-u_oQTxw../download",
  #           httr::write_disk(tmp) )
  
  readxl::read_excel("data-raw/Site Status Database Jan 13 2023.xlsx") %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    dplyr::rename(`Site Type` = `Site Type\r\n(New, Existing, Existing - Moved > 100 ft)\r\n`) %>%
    dplyr::mutate(`Site Type` = ifelse(`Site Type` == "new","New",`Site Type`))
}

get_umrb_grid <- function(){
  # tmp <- tempfile(fileext = ".zip")
  # tmpdir <- tempfile()
  # 
  # httr::GET(url = "https://dl.boxcloud.com/d/1/b1!hwBHT8dIbZvqPy3P_X9q0FvGaXzAupcNO1iopFp95BlhYywdSIyftlByjVK9aFiTqndlQIMl4e--tPb7VCFQr7BbiKPOW0tdnbdGGQ0D8jN7dOtMCypsgNPGaOrFJ8jP-odNaFznkAbqGPdb2klQWORyBE3JaHovaIZVcmorOOQExd_ZdsTcPpj65DCTH6gNGmK5DCFs5atRNhw6c6RUZuYvnLCw7OtGBKLVxjESbgGmIWgVYh5zv7SFTx5_Gzscc9qJP0dOVnEyqcaQD_5b6iPdgw98RAJT0oIXgwa3gK8Mn2WTNf8Svj3mJiij_7_E1NPq0XcvYnGxkS3xAXYIv9_z_5zGhRc-B81ggVoLlYOsSNYaPC9RRkYgvxXhHGmgkhG2VcCx6Ot80GbD0iDYWzM7jcapKXHDFRflQ-kg9JmytF7OF7Jg_PBJZOd3qLM8XuNbuv1JSs2VqtYUCQ8D5Ze4zOVo4I6R3zE30YVeCH7vqywxPOTCEHJsG7HCmBlqzpWd1BltE2bor423NTSVTZ2mzZ2stOioU1E6QMHdCpEKrAyQ3mm5O6ZFIVhmlU4KBgG3SiRUrDVd_P-BmIUCvGV_ojNLx3xWCI9ZAI9bhtZpv5dfF7Wq6hK923DJNeq-k_HsrEpQ-JWjqnBsvvcWjP-4NOhNFYY4G3MJDgii-r31SuYBaIdy0H6h0y2wWdD91oNncl74TlCUh2hC2D883YReNWHfk6Kem-auwhDUkXPgFNuenPUsf2YMMQB9sNiQdIybv2sniDL9EQ9j_kaRWhYWVglWfY6pMoAoWLvn8DpEHIE3LnjseWpWoSM1PBq0zDTQogGjru2duErKUVthJODAqizQX-1MU4CdSAOjoJ__01yStQs-ZMnJiLGZEnOW5JHPgxPrIgpDW2A0AgtfuHGhsu9pKp7CpCXsb8YFt9eZxomqwbDl9FEN93SXVNEGQo_switUNpDJFmwkR66J-B6N1nVR2dPCNcSIQfqdytmIstoNW2Ff2NWENICSEN3ew21LBCpXD8UrlnqoENIgeCsV2TK08TF4TPdoOCTahxqH2i2EupxbDN13-hiWcyHc38MkQt8-ktnkedC11kAA8er6IClWcMt8KPiTVo3N4dJ-FJIxCHOTu93c4OhrMFvwfayqQrcQAXT_SVOxO0cafdcackZNtC0JTOg0pCZGSMpjuBsetp1Q6h2T9wULf5UXXjK716nV7_YloMkPSIXQEwp6lBoy-UnvrK-N84QDPAIzBmZ601JcDF6mXOkYYXLBI2SU5BGvHPJzxSDL8RVEX0y9kwhPFmvQLtjzxzAKps2YVuF6CIV34N6cMrjQ5pfseXbe7hBVIsjhRXJpXBYRBr1z8M6RGytemG0IwFSCv_V3t1-I6AMxju-NObFxPQfNqRqWgZ1Q/download",
  #           httr::write_disk(tmp))
  # 
  # unzip(tmp,
  #       exdir = tmpdir)
  # 
  # sf::read_sf(tmpdir)
  sf::read_sf("data-raw/fwmesonetgrid19oct2021/")
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

# sf::read_sf("data-raw/Mesonet_Grid_21May21/") %>%
#   dplyr::filter(Status != "Less than 40%") %>%
#   dplyr::select(Cell = cell, 
#                 State) %>%
#   sf::st_make_valid() %>%
#   sf::st_transform(4326) %>%
#   sf::write_sf("data/umrb-ace-grid.geojson")
# 
# sf::read_sf("data-raw/UMRB_outline.shp") %>%
#   dplyr::select() %>%
#   sf::st_make_valid() %>%
#   sf::st_transform(4326) %>%
#   sf::write_sf("data/umrb.geojson")

# httr::GET("https://dl.boxcloud.com/d/1/b1!if93txLCMXa5WiFxLIr7T6LHg7Q_U79gWKytJLXfIKeEG61Y4pq6PlTDaZfeHdf-PqNjWnlyfUQ4q5M-kiJ0ciZpkrxl-ibFjyWTioFUXKA-66lrZgvxQs-4RSfhjuRbOEWl58Ey0UGiGJ8BAMHOpsabGnrUYIrFPKcbXMLq6NFQBJ_BScOEfdcE0dmhV20fhXXekNM3ZOps6shW_-7vRhXpA8SmNQPTRfldKUz6A13S1FeCjISqHQFWalCLd_Na7Ah63m37IGZt08MqRhJpKXCOtQ9ZMLsyCEzVkgSg8_TMk79AJAOCZjovhiCEuSA797MjuXYsTPFBi4JaIDS_NOw2YHqmUMXRJ-7XBaWv6E7dOTtiPblLFnOdMlS7Nj4aIxCUJ5ITOQp1DTh4-2-BwFq94Rri3ECB5hrs0REb_vWfJ8J2tneYVQSFX2a6ZU-Zrar-vkbWj2ZJH5vRuyBC6rnP0T_AnFFJmeuK-FZfDq01JC4cRuZQC36Uxpi3Abpht-YK8Lmh6I4n1u6w3WngZXigyQQjMSygpqxdh3HJH9bmjvnLDYpBUEzEA3QpKopMazdUeNlSOpgxGxCAFj7qzBNAXWS7kzV_KNBTRM8j5bFMIlkAkrAMhNL62_dRemQ_MSZ1XjfZE9sYBAaO2mF5btpt315fg8v_EmDpKu6cMVYzlssy9aBfhdenvnKeQ9JcMRLyzU47REIAWRRwTdhDfCXqthK9L3iWmbz5dNgvVAi09rNzNkB33a7xgpHW_x0C5zvdwJggMtQPbDgo570Z0c0MowVzMCYTi0oeaRPGcLMKw2BaU-gyrQM1sOxtiq8sJyfielasFeZhxWK7UugXlM35HYbnyYDYSfBDFsNhLMSRxBLefIiySryqKdlkZvlQlBKhwRIWJ56mCmVormEBAhlgNKSG6y5w4LlRtNTclCi14tJ-G05vygl8RERmw0waAp-Tga-aCxe9vq-UB5eFtXLrzQN22t6cZWnR8sLLC-imzR4SfrGLYn0LW8iMc9PV0SffxV2r1rfq-zfBJidPQ5PdIRI50IBhg38OSMQLlOfzuvyH55YAZtTigsYqdLwVF2AJISPogsryTnxg6qOEdD_6EXk_kpkgbq6oja6NQZfx4nVsJ58syZSNuN_qzL9vfPSBa_JllapOmg3zNNWpBoApMosjPZfKKxLqkJSwYWMv7L2JE0xqHblWFslEH5YJw5XcaTFI32gjbkg3pYiL8h2mORHz5cHPUwBnYzEFHbmtAhvKvZ3bXEZqnXfVkFZmu-xcihLpYTHyCK8YpDt7xdek4Bt6QyqcfcfVay3nizCy2MBrdKAxB2WYwHKRU8vzcwf8XrqP4-Xow4eIjh4GFKb5MPtrs7AxgR8qZHlPdBZb91zsT-V8GXRUWQ../download",
#           httr::write_disk("data-raw/soils.zip"))
# 
# unzip(zipfile = "data-raw/soils.zip",
#       exdir = "data-raw")
# 
# sf::st_layers("data-raw/UPMO_final.gdb/")

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
  dplyr::filter(STUSPS %in% c("ID", "MT", "WY", "ND", "SD", "NE", "MN", "IA"))

# httr::GET("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/raster/PRISMA_SR_50M.zip",
#           httr::write_disk("data-raw/PRISMA_SR_50M.zip"))
# unzip("data-raw/PRISMA_SR_50M.zip",
#       exdir = "data-raw")


get_image_rast <-
  function(x, url = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSShadedReliefOnly/MapServer/export"){
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

umrb_buffer <- states  %>%
  sf::st_transform("EPSG:5070") %>%
  sf::st_buffer(20000)

umrb_rast <- rast(extent = terra::ext(umrb_buffer), crs = terra::crs(umrb_buffer), resolution = 1000)

hill <- get_image_rast(umrb_rast) %>%
  # terra::project(umrb_grid_proj$wkt) %>%
  terra::project(umrb_grid_proj$wkt, method = "near") %>%
  terra::mask(.,terra::vect(sf::st_transform(states, terra::crs(.))))




g <-
  ggplot(sf::st_transform(states, "EPSG:5070")) +
  geom_raster(data = hill %>%
                terra::project("EPSG:5070", method = "near") %>%
                raster::raster() %>%
                get_df(),
              mapping = aes(x = x,
                            y = y,
                            alpha = ID),
              na.rm = TRUE) +
  scale_alpha(range = c(0.8, 0),
              na.value = 0,
              limits = c(0,255),
              guide = "none") +
  geom_sf(data = states,
          fill = NA) +
  geom_sf(data = umrb,
          color = "#2b8cbe",
          fill = "white",
          alpha = 0.5) +
  theme_void(base_size = 24) + 
  coord_sf(expand = TRUE,
           xlim = c(-2356114, 2258200),
           ylim = c(269573.6, 3172568)) +
  labs(title = "The Upper Missouri River Basin") +
  # ggplot2::scale_y_continuous(expand = expansion(mult = c(0.01,0.15))) +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  vjust = 1),
        plot.subtitle = element_text(size = 12,
                                     hjust = 0.5,
                                     face = "bold",
                                     vjust = 3),
        legend.position = "top",
        legend.text = element_text(size = 14)
  )

ggsave("figures/umrb-overview.png",
       plot = g,
       width = 10,
       height = 5.15,
       bg = "transparent")



umrb_zoom_base <- function(){
  ggplot(umrb_states) +
    geom_raster(data = hill %>%
                  terra::crop(terra::vect(umrb_states), mask = TRUE) %>%
                  raster::raster() %>%
                  get_df(),
                mapping = aes(x = x,
                              y = y,
                              alpha = ID),
                na.rm = TRUE) +
    scale_alpha(range = c(0.8, 0),
                na.value = 0,
                limits = c(0,255),
                guide = "none") +
    
    geom_sf(data = umrb_states,
            fill = NA) +
    theme_void(base_size = 24) + 
    coord_sf(xlim = sf::st_bbox(umrb_states)[c("xmin","xmax")],
             ylim = sf::st_bbox(umrb_states)[c("ymin","ymax")],
             default = TRUE) +
    # ggplot2::scale_y_continuous(expand = expansion(mult = c(0.01,0.15))) +
    theme(plot.title = element_text(hjust = 0.5,
                                    face = "bold",
                                    vjust = 1),
          plot.subtitle = element_text(size = 18,
                                       hjust = 0.5,
                                    face = "bold",
                                    vjust = 3),
          legend.position = "top",
          legend.margin=margin(t = 0.085, unit='in'),
          legend.text = element_text(size = 14,
                                     margin = margin(r = 0.5, unit = "inch")),
          legend.spacing.x = unit(0, "in")
          )
}

g <-
  umrb_zoom_base() +
  geom_sf(data = umrb,
          color = "#2b8cbe",
          fill = "white",
          alpha = 0.5) +
  labs(title = "The Upper Missouri River Basin",
       subtitle = "")

ggsave("figures/umrb-zoom.png",
       plot = g,
       width = 10,
       height = 5.15,
       bg = "transparent")

sf::st_area(umrb) %>% units::set_units("mi^2")
#302,000 square miles

pop20_county <- get_decennial(geography = "county", 
                              variables = "P1_001N", 
                              year = 2020,
                              geometry = TRUE)
total_pop <- 
  get_decennial(geography = "county", 
                variables = "P1_001N", 
                year = 2020,
                geometry = TRUE) %>%
  dplyr::filter(sf::st_intersects(geometry,sf::st_transform(umrb,sf::st_crs(geometry)), sparse = FALSE)[,1]) %$%
  value %>%
  sum()
#2,974,820 people in 2020 Census

pop20_tribal <- 
  tigris::native_areas(cb = TRUE) %>%
  dplyr::mutate(GEOID = paste0(GEOID,"R")) %>%
  dplyr::left_join(
    get_decennial(
      geography = "american indian area/alaska native area (reservation or statistical entity only)", 
      variables = "P1_001N", 
      year = 2020
    ),
    by = "GEOID"
  ) %>%
  dplyr::filter(sf::st_intersects(geometry,sf::st_transform(umrb,sf::st_crs(geometry)), sparse = FALSE)[,1]) %>%
  dplyr::select(Name =  NAME.x,
                variable,
                value) %>%
  sf::st_transform(umrb_grid_proj) %>%
  sf::st_intersection(umrb_states) %>%
  dplyr::group_by(Name) %>%
  dplyr::summarise()

# 21 Tribal Reservations
sum(pop20_tribal$value, na.rm = TRUE)
# 145,802 Tribal members in 2020 US Census

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


g <-
  umrb_zoom_base() +
  geom_sf(data = umrb,
          color = "#2b8cbe",
          fill = "white",
          alpha = 0.5) +
  geom_sf(data = pop20_tribal,
          color = "#5D7A56",
          fill = "#5D7A56",
          alpha = 0.5
  ) +
  # geom_sf_text(data = tcus, 
  #              label="★", 
  #              size=3, 
  #              family = "HiraKakuPro-W3") +
  coord_sf(xlim = sf::st_bbox(umrb_states)[c("xmin","xmax")],
           ylim = sf::st_bbox(umrb_states)[c("ymin","ymax")]) +
  labs(title = "The Upper Missouri River Basin",
       subtitle = "Tribal Lands")

ggsave("figures/umrb-tribes.png",
       plot = g,
       width = 10,
       height = 5.15,
       bg = "transparent")


umrb_grid <- 
  get_umrb_grid() %>%
  dplyr::filter(Status != "Less than 40%") %>%
  dplyr::select(Cell = cell,
                State) %>%
  sf::st_make_valid() %>%
  sf::st_transform(umrb_grid_proj) %>%
  rmapshaper::ms_simplify() %>%
  dplyr::mutate(State = factor(State, 
                               levels = c("MT","ND","NE","SD","WY","TBD"),
                               ordered = TRUE))

umrb_dem_5500 <- 
  sf::read_sf("data-raw/umrb_dem_5500.shp") %>%
  sf::st_make_valid() %>%
  sf::st_transform(umrb_grid_proj) %>%
  rmapshaper::ms_simplify() %>%
  rmapshaper::ms_simplify() %>%
  sf::st_union() %>%
  sf::st_intersection(umrb)




g<-
  umrb_zoom_base() +
  geom_sf(data = umrb,
          color = "#2b8cbe",
          size = 0.25,
          fill = NA,
          alpha = 0.5) +
  geom_sf(data = umrb_dem_5500,
          color = "#2b8cbe",
          fill = "white",
          alpha = 0.5) +
  geom_sf(data = pop20_tribal,
          color = "#5D7A56",
          fill = "#5D7A56",
          alpha = 0.5
  ) +
  # geom_sf_text(data = tcus, 
  #              label="★", 
  #              size=3, 
  #              family = "HiraKakuPro-W3") +
  coord_sf(xlim = sf::st_bbox(umrb_states)[c("xmin","xmax")],
           ylim = sf::st_bbox(umrb_states)[c("ymin","ymax")]) +
  labs(title = "The Upper Missouri River Basin",
       subtitle = "Plains Below 5,500 ft.")

ggsave("figures/umrb-5500.png",
       plot = g,
       width = 10,
       height = 5.15,
       bg = "transparent")

g<-
  umrb_zoom_base() +
  geom_sf(data = umrb,
          color = "#2b8cbe",
          size = 0.25,
          fill = NA,
          alpha = 0.5) +
  geom_sf(data = pop20_tribal,
          color = "#5D7A56",
          fill = "#5D7A56",
          alpha = 0.3
  ) +
  # geom_sf_text(data = tcus, 
  #              label="★", 
  #              size=3, 
  #              family = "HiraKakuPro-W3") +
  geom_sf(data = umrb_grid,
          color = "gray50",
          fill = "white",
          alpha = 0.5) +
  coord_sf(xlim = sf::st_bbox(umrb_states)[c("xmin","xmax")],
           ylim = sf::st_bbox(umrb_states)[c("ymin","ymax")]) +
  labs(title = "The UMRB Monitoring Network",
       subtitle = "500 mi² Grid")

ggsave("figures/umrb-grid.png",
       plot = g,
       width = 10,
       height = 5.15,
       bg = "transparent")

# 540 grid cells = 540 stations, or roughtly 1 every 500 square miles

g<-
  umrb_zoom_base() +
  geom_sf(data = umrb,
          color = "#2b8cbe",
          size = 0.25,
          fill = NA,
          alpha = 0.5) +
  geom_sf(data = pop20_tribal,
          color = "#5D7A56",
          fill = "#5D7A56",
          alpha = 0.3
  ) +
  # geom_sf_text(data = tcus, 
  #              label="★", 
  #              size=3, 
  #              family = "HiraKakuPro-W3") +
  geom_sf(data = umrb_grid,
          mapping = aes(fill = State),
          color = "gray50",
          alpha = 0.5) +
  geom_sf_label(data = umrb_grid %>%
                 dplyr::group_by(State) %>%
                 dplyr::count() %>%
                 sf::st_drop_geometry() %>%
                 dplyr::inner_join(states, .,
                                   by = c("STUSPS" = "State")) %>%
                 sf::st_centroid() %>%
                 dplyr::mutate(count = paste0(NAME, ":\n", n, " stations")),
               mapping = aes(label = count),
               fontface = "bold",
               alpha = 0.5
               ) +
  scale_color_manual(values = c("MT" = "#00678a",
                                "ND" = "#c0affb",
                                "NE" = "#e6a176",
                                "SD" = "#984464",
                                "TBD" = "#5eccab",
                                "WY" = "#56641a")) +
  scale_fill_manual(values =  c("MT" = "#00678a",
                                "ND" = "#c0affb",
                                "NE" = "#e6a176",
                                "SD" = "#984464",
                                "TBD" = "#5eccab",
                                "WY" = "#56641a")) +
  coord_sf(xlim = sf::st_bbox(umrb_states)[c("xmin","xmax")],
           ylim = sf::st_bbox(umrb_states)[c("ymin","ymax")]) +
  guides(fill = "none",
         color = "none") +
  labs(title = "The UMRB Monitoring Network",
       subtitle = "Participating State Mesonets")

ggsave("figures/umrb-grid-states.png",
       plot = g,
       width = 10,
       height = 5.15,
       bg = "transparent")

umrb_grid %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(State) %>%
  dplyr::count(sort = TRUE)

# A tibble: 6 × 2
# # Groups:   State [6]
# State     n
# <chr> <int>
# 1 MT      205
# 2 SD      156
# 3 ND       79
# 4 WY       61
# 5 NE       35
# 6 TBD       4

site_status <-
  get_umrb_status() %>%
  sf::st_transform(umrb_grid_proj) %>%
  dplyr::select(Site, 
                `Grid Cell ID`,
                `Site Type`,
                `Project Year (PY)`) %>%
  dplyr::mutate(`Station Status` = forcats::fct_collapse(`Project Year (PY)`,
                                                         Operational = c("Pilot Year", "PY1", "PY2"),
                                                `Summer 2023` = c("PY3"),
                                                `Candidate 2024` = c("PY4"))) %>%
  dplyr::filter(!is.na(`Station Status`))

g <-
  umrb_zoom_base() +
  geom_sf(data = umrb,
          color = "#2b8cbe",
          size = 0.25,
          fill = NA,
          alpha = 0.5) +
  geom_sf(data = pop20_tribal,
          color = "#5D7A56",
          fill = "#5D7A56",
          alpha = 0.3
  ) +
  # geom_sf_text(data = tcus, 
  #              label="★", 
  #              size=3, 
  #              family = "HiraKakuPro-W3") +
  geom_sf(data = umrb_grid,
          color = "gray50",
          fill = "white",
          alpha = 0.5) +
  geom_sf(data = umrb_grid %>%
            dplyr::inner_join(site_status %>%
                                dplyr::filter(`Station Status` == "Operational") %>%
                                sf::st_drop_geometry() %>%
                                dplyr::rename(Cell = `Grid Cell ID`)),
          mapping = aes(fill = State),
          color = "gray50",
          size = 1.2,
          alpha = 0.5) +
  scale_color_manual(values = c("MT" = "#00678a",
                                "ND" = "#c0affb",
                                "NE" = "#e6a176",
                                "SD" = "#984464",
                                "TBD" = "#5eccab",
                                "WY" = "#56641a"),
                     guide = 'none') +
  scale_fill_manual(values =  c("MT" = "#00678a",
                                "ND" = "#c0affb",
                                "NE" = "#e6a176",
                                "SD" = "#984464",
                                "TBD" = "#5eccab",
                                "WY" = "#56641a"),
                    guide = 'none') +
  geom_sf(data = site_status %>%
            dplyr::filter(`Station Status` == "Operational"),
          aes(shape = `Station Status`),
          fill = "white") +
  scale_shape_manual(values = c("Operational" = 19,
                                "Summer 2023" = 21,
                                "Candidate 2024" = 3),
                     drop = FALSE,
                     name = NULL) +
  coord_sf(xlim = sf::st_bbox(umrb_states)[c("xmin","xmax")],
           ylim = sf::st_bbox(umrb_states)[c("ymin","ymax")]) +
  labs(title = "The UMRB Monitoring Network",
       subtitle = NULL) +
  geom_text(x = mean(sf::st_bbox(umrb_states)[c("xmin","xmax")]),
            y = sf::st_bbox(umrb_states)[c("ymax")],
            label = paste0(
              site_status %>%
                dplyr::filter(`Station Status` == "Operational") %>%
                nrow(),
              " Operational"
            ),
            fontface = "bold",
            size = 6)

ggsave("figures/umrb-grid-sites-2022.png",
       plot = g,
       width = 10,
       height = 5.15,
       bg = "transparent")


g <-
  umrb_zoom_base() +
  geom_sf(data = umrb,
          color = "#2b8cbe",
          size = 0.25,
          fill = NA,
          alpha = 0.5) +
  geom_sf(data = pop20_tribal,
          color = "#5D7A56",
          fill = "#5D7A56",
          alpha = 0.3
  ) +
  # geom_sf_text(data = tcus, 
  #              label="★", 
  #              size=3, 
  #              family = "HiraKakuPro-W3") +
  geom_sf(data = umrb_grid,
          color = "gray50",
          fill = "white",
          alpha = 0.5) +
  geom_sf(data = umrb_grid %>%
            dplyr::inner_join(site_status %>%
                                dplyr::filter(`Station Status` %in% c("Operational","Summer 2023")) %>%
                                sf::st_drop_geometry() %>%
                                dplyr::rename(Cell = `Grid Cell ID`)),
          mapping = aes(fill = State),
          color = "gray50",
          size = 1.2,
          alpha = 0.5) +
  scale_color_manual(values = c("MT" = "#00678a",
                                "ND" = "#c0affb",
                                "NE" = "#e6a176",
                                "SD" = "#984464",
                                "TBD" = "#5eccab",
                                "WY" = "#56641a"),
                     guide = 'none') +
  scale_fill_manual(values =  c("MT" = "#00678a",
                                "ND" = "#c0affb",
                                "NE" = "#e6a176",
                                "SD" = "#984464",
                                "TBD" = "#5eccab",
                                "WY" = "#56641a"),
                    guide = 'none') +
  geom_sf(data = site_status %>%
            dplyr::filter(`Station Status`  %in% c("Operational","Summer 2023")),
          aes(shape = `Station Status`),
          fill = "white") +
  scale_shape_manual(values = c("Operational" = 19,
                                "Summer 2023" = 21,
                                "Candidate 2024" = 3),
                     drop = FALSE,
                     name = NULL) +
  coord_sf(xlim = sf::st_bbox(umrb_states)[c("xmin","xmax")],
           ylim = sf::st_bbox(umrb_states)[c("ymin","ymax")]) +
  labs(title = "The UMRB Monitoring Network",
       subtitle = NULL) +
  geom_text(x = mean(sf::st_bbox(umrb_states)[c("xmin","xmax")]),
            y = sf::st_bbox(umrb_states)[c("ymax")],
            label = paste0(
              site_status %>%
                dplyr::filter(`Station Status` == "Summer 2023") %>%
                nrow(),
              " Summer 2023 Installs"
            ),
            fontface = "bold",
            size = 6)

ggsave("figures/umrb-grid-sites-2023.png",
       plot = g,
       width = 10,
       height = 5.15,
       bg = "transparent")


g <-
  umrb_zoom_base() +
  geom_sf(data = umrb,
          color = "#2b8cbe",
          size = 0.25,
          fill = NA,
          alpha = 0.5) +
  geom_sf(data = pop20_tribal,
          color = "#5D7A56",
          fill = "#5D7A56",
          alpha = 0.3
  ) +
  # geom_sf_text(data = tcus, 
  #              label="★", 
  #              size=3, 
  #              family = "HiraKakuPro-W3") +
  geom_sf(data = umrb_grid,
          color = "gray50",
          fill = "white",
          alpha = 0.5) +
  geom_sf(data = umrb_grid %>%
            dplyr::inner_join(site_status %>%
                                sf::st_drop_geometry() %>%
                                dplyr::rename(Cell = `Grid Cell ID`)),
          mapping = aes(fill = State),
          color = "gray50",
          size = 1.2,
          alpha = 0.5) +
  scale_color_manual(values = c("MT" = "#00678a",
                                "ND" = "#c0affb",
                                "NE" = "#e6a176",
                                "SD" = "#984464",
                                "TBD" = "#5eccab",
                                "WY" = "#56641a"),
                     guide = 'none') +
  scale_fill_manual(values =  c("MT" = "#00678a",
                                "ND" = "#c0affb",
                                "NE" = "#e6a176",
                                "SD" = "#984464",
                                "TBD" = "#5eccab",
                                "WY" = "#56641a"),
                    guide = 'none') +
  geom_sf(data = site_status,
          aes(shape = `Station Status`),
          fill = "white") +
  scale_shape_manual(values = c("Operational" = 19,
                                "Summer 2023" = 21,
                                "Candidate 2024" = 3),
                     drop = FALSE,
                     name = NULL) +
  coord_sf(xlim = sf::st_bbox(umrb_states)[c("xmin","xmax")],
           ylim = sf::st_bbox(umrb_states)[c("ymin","ymax")]) +
  labs(title = "The UMRB Monitoring Network",
       subtitle = NULL) +
  geom_text(x = mean(sf::st_bbox(umrb_states)[c("xmin","xmax")]),
            y = sf::st_bbox(umrb_states)[c("ymax")],
            label = paste0(
              site_status %>%
                dplyr::filter(`Station Status` == "Candidate 2024") %>%
                nrow(),
              " Candidate 2024 Installs"
            ),
            fontface = "bold",
            size = 6)

ggsave("figures/umrb-grid-sites-2024.png",
       plot = g,
       width = 10,
       height = 5.15,
       bg = "transparent")


umrb_grid_tribes <-
  umrb_grid %>%
  dplyr::filter(sf::st_intersects(geometry,sf::st_transform(pop20_tribal,
                                                            sf::st_crs(geometry)), 
                                  sparse = FALSE) %>%
                  rowSums() > 0)
  

g <-
  umrb_zoom_base() +
  geom_sf(data = umrb,
          color = "#2b8cbe",
          size = 0.25,
          fill = NA,
          alpha = 0.5) +
  geom_sf(data = pop20_tribal,
          color = "#5D7A56",
          fill = "#5D7A56",
          alpha = 0.3
  ) +
  geom_sf(data = umrb_grid,
          color = "gray50",
          fill = "white",
          alpha = 0.5) +
  geom_sf(data = umrb_grid_tribes,
          color = "#627D5A",
          fill = "#627D5A",
          size = 1.2,
          alpha = 0.5) +
  geom_sf(data = pop20_tribal,
          color = "#5D7A56",
          fill = NA) +
  # geom_sf_text(data = tcus, 
  #              label="★", 
  #              size=3, 
  #              family = "HiraKakuPro-W3") +
  theme(legend.position = 'none') +
  coord_sf(xlim = sf::st_bbox(umrb_states)[c("xmin","xmax")],
           ylim = sf::st_bbox(umrb_states)[c("ymin","ymax")]) +
  labs(title = "The UMRB Monitoring Network",
       subtitle = "174 Tribal Grid Cells")

ggsave("figures/umrb-grid-tribes.png",
       plot = g,
       width = 10,
       height = 5.15,
       bg = "transparent")

# 174 cells intersect Tribal land

g <-
  umrb_zoom_base() +
  geom_sf(data = umrb,
          color = "#2b8cbe",
          size = 0.25,
          fill = NA,
          alpha = 0.5) +
  
  geom_sf(data = umrb_grid,
          color = "gray50",
          fill = "white",
          alpha = 0.5) +
  geom_sf(data = umrb_grid_tribes %>%
            dplyr::filter(!(Cell %in% (umrb_grid %>%
                                         dplyr::inner_join(site_status %>%
                                                             sf::st_drop_geometry() %>%
                                                             dplyr::rename(Cell = `Grid Cell ID`)) %$%
                                         Cell))),
          color = "#627D5A",
          fill = "#627D5A",
          size = 1.2,
          alpha = 0.5) +
  geom_sf(data = umrb_grid_tribes,
          color = "#627D5A",
          fill = NA,
          size = 1.2,
          alpha = 0.5) +
geom_sf(data = pop20_tribal,
        color = "#5D7A56",
        fill = NA) +
  # geom_sf_text(data = tcus, 
  #              label="★", 
  #              size=3, 
  #              family = "HiraKakuPro-W3") +
  theme(legend.position = 'none') +
  coord_sf(xlim = sf::st_bbox(umrb_states)[c("xmin","xmax")],
           ylim = sf::st_bbox(umrb_states)[c("ymin","ymax")]) +
  labs(title = "The UMRB Monitoring Network",
       subtitle = "80 Tribal Grid Cells Remaining")

ggsave("figures/umrb-grid-tribes_remaining.png",
       plot = g,
       width = 10,
       height = 5.15,
       bg = "transparent")

# 80 potential cells to be filled on Tribal land


g <-
  umrb_zoom_base() +
  geom_sf(data = umrb,
          color = "#2b8cbe",
          size = 0.25,
          fill = NA,
          alpha = 0.5) +
  
  geom_sf(data = umrb_grid,
          color = "gray50",
          fill = "white",
          alpha = 0.5) +
  geom_sf(data = umrb_grid %>%
            dplyr::filter(!(Cell %in% (umrb_grid %>%
                                         dplyr::inner_join(site_status %>%
                                                             sf::st_drop_geometry() %>%
                                                             dplyr::rename(Cell = `Grid Cell ID`)) %$%
                                         Cell))),
          color = "#627D5A",
          fill = "#627D5A",
          size = 1.2,
          alpha = 0.5) +
  geom_sf(data = umrb_grid,
          color = "#627D5A",
          fill = NA,
          size = 1.2,
          alpha = 0.5) +
  geom_sf(data = pop20_tribal,
          color = "#5D7A56",
          fill = NA) +
  # geom_sf_text(data = tcus, 
  #              label="★", 
  #              size=3, 
  #              family = "HiraKakuPro-W3") +
  theme(legend.position = 'none') +
  coord_sf(xlim = sf::st_bbox(umrb_states)[c("xmin","xmax")],
           ylim = sf::st_bbox(umrb_states)[c("ymin","ymax")]) +
  labs(title = "The UMRB Monitoring Network",
       subtitle = "264 Grid Cells Remaining")

ggsave("figures/umrb-grid-all_remaining.png",
       plot = g,
       width = 10,
       height = 5.15,
       bg = "transparent")







## All mesonet sites
source("plotting_functions.R")

stations <- 
  "https://mesonet.climate.umt.edu/api/v2/stations" %>%
  httr::GET(query = list(type = "csv")) %>%
  httr::content() %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4326) %>%
  dplyr::mutate(`Sub-network` = factor(sub_network, 
                                       levels = c("HydroMet", "AgriMet"),
                                       ordered = TRUE))

ggplot() + 
  mtd_plot() +
  ggplot2::geom_sf(data = mcor::mt_counties_simple,
                   color = "black",
                   # alpha = 0.50,
                   fill = NA,
                   size = 0.5,
                   inherit.aes = FALSE) +
  add_hillshade() +
  ggplot2::geom_sf(data = mcor::mt_state_simple,
                   color = "black",
                   # alpha = 0.50,
                   fill = NA,
                   size = 1,
                   inherit.aes = FALSE) +
  ggplot2::geom_sf(data = stations,
                   mapping = aes(color = `Sub-network`,
                                 size = `Sub-network`)
  ) +
  scale_color_manual(values = c(HydroMet = rgb(211, 129, 46, maxColorValue = 255),
                                   AgriMet = rgb(29, 60, 52, maxColorValue = 255)),
                     aesthetics = c("colour", "fill")) +
  scale_size_manual(values = c(HydroMet = 2.5,  
                                   AgriMet = 1))

ggsave(file.path("figures","mco_mesonet.pdf") ,
       width = fig_width,
       height = fig_height,
       dpi = 600,
       bg = "transparent")

stations %>%
  dplyr::group_by(`Sub-network`) %>%
  dplyr::count()
# 105 stations
# 19 HydroMet
# 86 AgriMet

 



  



