library(mapview)
library(magrittr)
library(raster)
library(ggplot2)
library(tidycensus)
library(mcor)
library(terra)
mapviewOptions(fgb = FALSE)

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
  sf::st_transform(mcor::mt_state_plane)

umrb <- 
  sf::read_sf("data/umrb.geojson") %>%
  sf::st_transform(mcor::mt_state_plane) %>%
  rmapshaper::ms_simplify()


# httr::GET("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/raster/PRISMA_SR_50M.zip",
#           httr::write_disk("data-raw/PRISMA_SR_50M.zip"))
# unzip("data-raw/PRISMA_SR_50M.zip",
#       exdir = "data-raw")

hill <- 
  terra::rast("data-raw/PRISMA_SR_50M/PRIMSA_SR_50M.tif") %>%
  terra::crop(.,sf::st_transform(states, terra::crs(.))) %>%
  terra::project(mcor::mt_state_plane$wkt, method = "near") %>%
  terra::mask(.,terra::vect(sf::st_transform(states, terra::crs(.))))


g<-
  ggplot(states) +
  geom_raster(data = hill %>%
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
  theme_void() + 
  coord_sf(expand = FALSE)

ggsave("figures/umrb-overview.png",
       plot = g,
       width = 10.24,
       height = 7.68)


hill <- 
  mcor::mt_hillshade_500m %>%
  terra::rast()

g<-
  ggplot(mcor::mt_state_simple) +
  geom_raster(data = hill %>%
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
  theme_void() +
  coord_sf(xlim = sf::st_bbox(mcor::mt_state_simple)[c("xmin","xmax")],
           ylim = sf::st_bbox(mcor::mt_state_simple)[c("ymin","ymax")])




ggsave("figures/mt-zoom.png",
       plot = g,
       width = 10.24,
       height = 6.4
)

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
                value)

# 21 Tribal Reservations
sum(pop20_tribal$value, na.rm = TRUE)
# 145,802 Tribal members in 2020 US Census

g<-
  ggplot(mcor::mt_state_simple) +
  geom_raster(data = hill %>%
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
  geom_sf(data = pop20_tribal,
          color = "#5D7A56",
          fill = "#5D7A56",
          alpha = 0.5
  ) +
  theme_void() +
  coord_sf(xlim = sf::st_bbox(mcor::mt_state_simple)[c("xmin","xmax")],
           ylim = sf::st_bbox(mcor::mt_state_simple)[c("ymin","ymax")])

ggsave("figures/mt-tribes.png",
       plot = g,
       width = 10.24,
       height = 6.4
)

umrb_grid <- 
  get_umrb_grid() %>%
  dplyr::filter(Status != "Less than 40%") %>%
  dplyr::select(Cell = cell,
                State) %>%
  sf::st_make_valid() %>%
  sf::st_transform(mcor::mt_state_plane) %>%
  rmapshaper::ms_simplify()

umrb_dem_5500 <- 
  sf::read_sf("data-raw/umrb_dem_5500.shp") %>%
  sf::st_make_valid() %>%
  sf::st_transform(mcor::mt_state_plane) %>%
  rmapshaper::ms_simplify() %>%
  rmapshaper::ms_simplify() %>%
  sf::st_union() %>%
  sf::st_intersection(umrb)

ggplot(mcor::mt_counties_simple) +
  geom_sf() +
  geom_raster(data = mcor::mt_hillshade_500m %>%
                as.data.frame(xy = TRUE),
              mapping = aes(x = x,
                            y = y,
                            alpha = layer),
              na.rm = TRUE) +
  scale_alpha(range = c(0.8, 0),
              na.value = 0,
              limits = c(0,255),
              guide = "none") +
  mcor::mco_theme_map()



g<-
  ggplot(umrb) +
  geom_raster(data = hill %>%
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
  geom_sf(data = umrb_dem_5500,
          color = "#2b8cbe",
          fill = "white",
          alpha = 0.5) +
  # geom_sf(data = umrb_dem_5500,
  #         # mapping = aes(color = State,
  #         #               fill = State),
  #         alpha = 0.5) +
  theme_void() +
  coord_sf(xlim = sf::st_bbox(mcor::mt_state_simple)[c("xmin","xmax")],
           ylim = sf::st_bbox(mcor::mt_state_simple)[c("ymin","ymax")])

ggsave("figures/mt-5500.png",
       plot = g,
       width = 10.24,
       height = 6.4
)

g<-
  ggplot(umrb) +
  geom_raster(data = hill %>%
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
  geom_sf(data = umrb_grid,
          color = "#2b8cbe",
          fill = "white",
          alpha = 0.5) +
  theme_void() +
  coord_sf(xlim = sf::st_bbox(mcor::mt_state_simple)[c("xmin","xmax")],
           ylim = sf::st_bbox(mcor::mt_state_simple)[c("ymin","ymax")])

ggsave("figures/mt-grid.png",
       plot = g,
       width = 10.24,
       height = 6.4
)

# 540 grid cells = 540 stations, or roughtly 1 every 500 square miles

g<-
  ggplot(umrb) +
  geom_raster(data = hill %>%
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
  geom_sf(data = umrb_grid,
          mapping = aes(color = State,
                        fill = State),
          alpha = 0.5) +
  scale_color_manual(values = c("#00678a",
                                "#c0affb",
                                "#e6a176",
                                "#984464",
                                "#5eccab",
                                "#56641a")) +
  scale_fill_manual(values =  c("#00678a",
                                "#c0affb",
                                "#e6a176",
                                "#984464",
                                "#5eccab",
                                "#56641a")) +
  theme_void() +
  theme(legend.position = 'none') +
  coord_sf(xlim = sf::st_bbox(mcor::mt_state_simple)[c("xmin","xmax")],
           ylim = sf::st_bbox(mcor::mt_state_simple)[c("ymin","ymax")])

ggsave("figures/mt-grid-states.png",
       plot = g,
       width = 10.24,
       height = 6.4
)

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
  sf::st_transform(mcor::mt_state_plane) %>%
  dplyr::select(Site, 
                `Grid Cell ID`,
                `Site Type`,
                `Station Status`)

g <-
  ggplot(umrb) +
  geom_raster(data = hill %>%
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
  geom_sf(data = umrb_grid,
          color = "gray50",
          fill = "white",
          alpha = 0.5) +
  geom_sf(data = umrb_grid %>%
            dplyr::inner_join(site_status %>%
                                sf::st_drop_geometry() %>%
                                dplyr::rename(Cell = `Grid Cell ID`)),
          mapping = aes(color = State,
                        fill = State),
          size = 1.2,
          alpha = 0.5) +
  scale_color_manual(values = c("#00678a",
                                "#c0affb",
                                "#e6a176",
                                "#984464",
                                "#5eccab",
                                "#56641a")) +
  scale_fill_manual(values =  c("#00678a",
                                "#c0affb",
                                "#e6a176",
                                "#984464",
                                "#5eccab",
                                "#56641a")) +
  geom_sf(data = site_status,
          mapping = aes(shape = `Station Status`)) +
  theme_void() +
  theme(legend.position = 'none') +
  coord_sf(xlim = sf::st_bbox(mcor::mt_state_simple)[c("xmin","xmax")],
           ylim = sf::st_bbox(mcor::mt_state_simple)[c("ymin","ymax")])

ggsave("figures/mt-grid-sites.png",
       plot = g,
       width = 10.24,
       height = 6.4
)


umrb_grid_tribes <-
  umrb_grid %>%
  dplyr::filter(sf::st_intersects(geometry,sf::st_transform(pop20_tribal,sf::st_crs(geometry)), sparse = FALSE) %>%
                  rowSums() > 0)
  

g <-
  ggplot(umrb) +
  geom_raster(data = hill %>%
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
  geom_sf(data = umrb_grid,
          color = "gray50",
          fill = "white",
          alpha = 0.5) +
  geom_sf(data = umrb_grid_tribes,
          color = "#627D5A",
          fill = "#627D5A",
          size = 1.2,
          alpha = 0.5) +
  # scale_color_manual(values = c("#00678a",
  #                               "#c0affb",
  #                               "#e6a176",
  #                               "#984464",
  #                               "#5eccab",
  #                               "#56641a")) +
  # scale_fill_manual(values =  c("#00678a",
  #                               "#c0affb",
  #                               "#e6a176",
  #                               "#984464",
  #                               "#5eccab",
  #                               "#56641a")) +
  # geom_sf(data = site_status %>%
  #           dplyr::filter(`Station Status` %in% c("In-Progress","Complete"))) +
  geom_sf(data = pop20_tribal,
          fill = NA) +
  theme_void() +
  theme(legend.position = 'none') +
  coord_sf(xlim = sf::st_bbox(mcor::mt_state_simple)[c("xmin","xmax")],
           ylim = sf::st_bbox(mcor::mt_state_simple)[c("ymin","ymax")])

ggsave("figures/mt-grid-tribes.png",
       plot = g,
       width = 10.24,
       height = 6.4
)

# 174 cells intersect Tribal land

g <-
  ggplot(umrb) +
  geom_raster(data = hill %>%
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
  geom_sf(data = umrb_grid,
          color = "gray50",
          fill = "white",
          alpha = 0.5) +
  geom_sf(data = umrb_grid_tribes %>%
            dplyr::filter(!(Cell %in% (umrb_grid %>%
                                         dplyr::inner_join(site_status %>%
                                                             dplyr::filter(`Station Status` %in% c("In-Progress","Complete")) %>%
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
  # scale_color_manual(values = c("#00678a",
  #                               "#c0affb",
  #                               "#e6a176",
  #                               "#984464",
  #                               "#5eccab",
  #                               "#56641a")) +
  # scale_fill_manual(values =  c("#00678a",
  #                               "#c0affb",
  #                               "#e6a176",
  #                               "#984464",
  #                               "#5eccab",
#                               "#56641a")) +
# geom_sf(data = site_status %>%
#           dplyr::filter(`Station Status` %in% c("In-Progress","Complete"))) +
geom_sf(data = pop20_tribal,
        fill = NA) +
  theme_void() +
  theme(legend.position = 'none') +
  coord_sf(xlim = sf::st_bbox(mcor::mt_state_simple)[c("xmin","xmax")],
           ylim = sf::st_bbox(mcor::mt_state_simple)[c("ymin","ymax")])

ggsave("figures/mt-grid-tribes_remaining.png",
       plot = g,
       width = 10.24,
       height = 6.4
)

# 151 potential cells to be filled on Tribal land











