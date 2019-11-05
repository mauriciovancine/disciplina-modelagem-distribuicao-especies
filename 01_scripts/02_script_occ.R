# -------------------------------------------------------------------------
# occ - download and taxonomic, data, and spatial filter
# mauricio vancine - mauricio.vancine@gmail.com
# 06-11-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(CoordinateCleaner)
library(lubridate)
library(raster)
library(rgdal)
library(sf)
library(spocc)
library(taxize)
library(tidyverse)

# informations
# https://ropensci.org/
# https://ropensci.github.io/spocc/
# https://cloud.r-project.org/web/packages/spocc/index.html
# https://cloud.r-project.org/web/packages/spocc/vignettes/spocc_vignette.html
# https://ropensci.github.io/taxize/
# https://cloud.r-project.org/web/packages/taxize/index.html
# https://cloud.r-project.org/web/packages/taxize/vignettes/taxize_vignette.html
# https://ropensci.github.io/CoordinateCleaner/
# https://cloud.r-project.org/web/packages/CoordinateCleaner/index.html
# https://ropensci.github.io/CoordinateCleaner/articles/Tutorial_Cleaning_GBIF_data_with_CoordinateCleaner.html
# https://github.com/ropensci/rnaturalearth
# https://www.naturalearthdata.com/
# https://github.com/r-spatial/sf

# directory
path <- "/home/mude/data/disciplina-modelagem-distribuicao-especies"
setwd(path)
dir()

# download occurrences ----------------------------------------------------
# species list
sp <- c("Haddadus binotatus")
sp

# synonyms
syn <- taxize::synonyms(x = sp, db = "itis") %>% 
  taxize::synonyms_df()
syn

# combine
if(ncol(syn) > 4){sp_syn <- c(sp, syn$syn_name) %>% unique} else{sp_syn <- sp}
sp_syn

# bases for download
db <- c("gbif",       # Global Biodiversity Information Facility (https://www.gbif.org/)
        "ecoengine",  # Berkeley Initiative for Global Change Biology (https://ecoengine.berkeley.edu/)
        "inat",       # iNaturalist (https://www.inaturalist.org/)
        "vertnet",    # VertNet (http://vertnet.org/)
        "ebird",      # eBird (https://ebird.org/)
        "idigbio",    # Integrated Digitized Biocollections (https://www.idigbio.org/)
        "obis",       # Ocean Biogeographic Information System (www.iobis.org)
        "ala",        # Atlas of Living Australia (https://www.ala.org.au/)
        "bison")       # Biodiversity Information Serving Our Nation (https://bison.usgs.gov)
db

# occ download
occ <- spocc::occ(query = sp_syn, 
                  from = db, 
                  # ebirdopts = list(key = ""), # make key in https://ebird.org/api/keygen
                  has_coords = TRUE, 
                  limit = 1e6)
occ

# get data
occ_data <- occ %>%
  spocc::occ2df() %>% 
  dplyr::mutate(longitude = as.numeric(longitude),
                latitude = as.numeric(latitude),
                year = lubridate::year(date),
                base = prov) %>% 
  dplyr::select(name, longitude, latitude, year, base)
occ_data

# vector
occ_vector <- occ_data %>% 
  dplyr::mutate(lon = longitude, lat = latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_vector

# limit brazil
br <- rnaturalearth::ne_countries(country = "Brazil", scale = "small", returnclass = "sf")
br

# map
map_total <- tm_shape(br) +
  tm_polygons() +
  tm_shape(occ_vector) +
  tm_dots(shape = 21, size = .2, col = "steelblue", alpha = .7) +
  tm_graticules(lwd = .5) +
  tm_compass(type = "8star", size = 3, position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 500, 1000), text.size = .8)  +
  tm_add_legend(title = sp, type = "symbol", 
                labels = c(paste0("Total (", nrow(occ_data), ")")),
                col = c("steelblue"), shape = 21, size = 1) +
  tm_layout(legend.bg.color = TRUE, legend.frame = TRUE)
map_total

# directory
dir.create("02_dados/00_occ")
setwd("02_dados/00_occ")

# export
readr::write_csv(occ_data, paste0("00_occ_data_total_", sp, "_", Sys.Date(), ".csv"))
sf::write_sf(occ_vector, paste0("00_occ_data_total_", sp, "_", Sys.Date(), ".shp"))
tmap::tmap_save(map_total, paste0("00_occ_data_total_", sp, "_", Sys.Date(), ".png"))

# taxonomic filter --------------------------------------------------------
# gnr names
gnr <- taxize::gnr_resolve(sp_syn)
gnr

# adjust names
gnr_tax <- gnr %>% 
  dplyr::mutate(species = sp %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_")) %>% 
  dplyr::select(species, matched_name) %>%
  dplyr::bind_rows(tibble::tibble(species = sp %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"),
                                  matched_name = c(sp_syn, 
                                                   sp_syn %>% stringr::str_to_title(),
                                                   sp_syn %>% stringr::str_to_lower(),
                                                   sp_syn %>% stringr::str_to_upper()))) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(matched_name)
gnr_tax

# confer
occ_data %>%
  dplyr::select(name) %>% 
  table %>% 
  tibble::as_tibble()

# taxonomic filter
occ_data_tax <- dplyr::inner_join(occ_data, gnr_tax, c(name = "matched_name")) %>% 
  dplyr::arrange(name) %>% 
  dplyr::select(name, species, everything())
occ_data_tax

# confer
occ_data$name %>% table %>% tibble::as_tibble()
occ_data_tax$name %>% table %>% tibble::as_tibble()

# vector
occ_vector_tax <- occ_data_tax %>% 
  dplyr::mutate(lon = longitude, lat = latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_vector_tax

# map
map_filter_tax <- tm_shape(br) +
  tm_polygons() +
  tm_shape(occ_vector) +
  tm_dots(shape = 21, size = .2, col = "steelblue", alpha = .7) +
  tm_shape(occ_vector_tax) +
  tm_dots(shape = 21, size = .2, col = "red", alpha = .7) +
  tm_graticules(lwd = .5) +
  tm_compass(type = "8star", size = 3, position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 500, 1000), text.size = .8) +
  tm_add_legend(title = sp, type = "symbol", 
                labels = c(paste0("Total (", nrow(occ_data), ")"), 
                           paste0("Filtro - Taxonomia (", nrow(occ_data_tax), ")")),
                col = c("steelblue", "red"), shape = 21, size = 1) +
  tm_layout(legend.bg.color = TRUE, legend.frame = TRUE)
map_filter_tax

# export
readr::write_csv(occ_data_tax, paste0("01_occ_data_filter_tax_", sp, "_", Sys.Date(), ".csv"))
sf::write_sf(occ_vector_tax, paste0("01_occ_data_filter_tax_", sp, "_", Sys.Date(), ".shp"))
tmap::tmap_save(map_filter_tax, paste0("01_occ_data_filter_tax_", sp, "_", Sys.Date(), ".png"))

# date filter -------------------------------------------------------------
# verify
occ_data_tax$year %>% 
  table(useNA = "always")

# year > 1960 and < 2019
occ_data_tax_date <- occ_data_tax %>% 
  dplyr::filter(year > 1970, year <= 2019, !is.na(year)) %>% 
  dplyr::arrange(year)
occ_data_tax_date

# verify
occ_data_tax$year %>% table(useNA = "always")
occ_data_tax_date$year %>% table(useNA = "always")

hist_temporal <- ggplot() + 
  geom_histogram(data = occ_data, aes(year), color = "darkblue", fill = "steelblue", bins = 50) +
  geom_histogram(data = occ_data_tax_date, aes(year), color = "darkred", fill = "red", bins = 50, alpha = .7) +
  theme_bw()
hist_temporal

# vector
occ_vector_tax_date <- occ_data_tax_date %>% 
  dplyr::mutate(lon = longitude, lat = latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_vector_tax_date

# map
map_filter_tax_date <- tm_shape(br) +
  tm_polygons() +
  tm_shape(occ_vector) +
  tm_dots(shape = 21, size = .2, col = "steelblue", alpha = .7) +
  tm_shape(occ_vector_tax_date) +
  tm_dots(shape = 21, size = .2, col = "red", alpha = .7) +
  tm_graticules(lwd = .5) +
  tm_compass(type = "8star", size = 3, position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 500, 1000), text.size = .8) +
  tm_add_legend(title = "Legenda", type = "symbol", 
                labels = c(paste0("Total (", nrow(occ_data), ")"), 
                           paste0("Filtro - Data (", nrow(occ_data_tax_date), ")")),
                col = c("steelblue", "red"), shape = 21, size = 1) +
  tm_layout(legend.bg.color = TRUE, legend.frame = TRUE)
map_filter_tax_date

# export
readr::write_csv(occ_data_tax_date, paste0("02_occ_data_filter_tax_date_", sp, "_", Sys.Date(), ".csv"))
ggplot2::ggsave(paste0("02_occ_data_hist_year_", sp, "_", Sys.Date(), ".png"), hist_temporal)
sf::write_sf(occ_vector_tax_date, paste0("02_occ_data_filter_tax_date_", sp, "_", Sys.Date(), ".shp"))
tmap::tmap_save(map_filter_tax_date, paste0("02_occ_data_filter_tax_date_", sp, "_", Sys.Date(), ".png"))

# spatial filter ----------------------------------------------------------
# remove na
occ_data_na <- occ_data_tax_date %>% 
  tidyr::drop_na(longitude, latitude)
occ_data_na

# flag data
flags_spatial <- CoordinateCleaner::clean_coordinates(
  x = occ_data_na, 
  species = "species",
  lon = "longitude", 
  lat = "latitude",
  tests = c("capitals", # radius around capitals
            "centroids", # radius around country and province centroids
            "duplicates", # records from one species with identical coordinates
            "equal", # equal coordinates
            "gbif", # radius around GBIF headquarters
            "institutions", # radius around biodiversity institutions
            # "outliers", # records far away from all other records of this species
            "seas", # in the sea
            "urban", # within urban area
            "validity", # outside reference coordinate system
            "zeros" # plain zeros and lat = lon 
  )
)

# results
#' TRUE = clean coordinate entry 
#' FALSE = potentially problematic coordinate entries
flags_spatial %>% head
summary(flags_spatial)

# exclude records flagged by any test
occ_data_tax_date_spa <- occ_data_na %>% 
  dplyr::filter(flags_spatial$.summary == TRUE)
occ_data_tax_date_spa

# resume data
occ_data_na$species %>% table
occ_data_tax_date_spa$species %>% table

# vector
occ_vector_tax_date_spa <- occ_data_tax_date_spa %>% 
  dplyr::mutate(lon = longitude, lat = latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_vector_tax_date_spa

# map
map_filter_tax_date_spa <- tm_shape(br) +
  tm_polygons() +
  tm_shape(occ_vector) +
  tm_dots(shape = 21, size = .2, col = "steelblue") +
  tm_shape(occ_vector_tax_date_spa) +
  tm_dots(shape = 21, size = .2, col = "red") + 
  tm_graticules(lwd = .5) +
  tm_compass(type = "8star", size = 3, position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 500, 1000), text.size = .8) +
  tm_add_legend(title = "Legenda", type = "symbol", 
                labels = c(paste0("Total (", nrow(occ_data), ")"), 
                           paste0("Filtro - Espacial (", nrow(occ_data_tax_date_spa), ")")),
                col = c("steelblue", "red"), shape = 21, size = 1) +
  tm_layout(legend.bg.color = TRUE, legend.frame = TRUE)
map_filter_tax_date_spa

# export
readr::write_csv(occ_data_tax_date_spa, paste0("03_occ_data_filter_tax_data_espacial_", sp, "_", Sys.Date(), ".csv"))
sf::write_sf(occ_vector_tax_date_spa, paste0("03_occ_data_filter_tax_data_espacial_", sp, "_", Sys.Date(), ".shp"))
tmap::tmap_save(map_filter_tax_date_spa, paste0("03_occ_data_filter_tax_data_espacial_", sp, "_", Sys.Date(), ".png"))

# oppc --------------------------------------------------------------------
# directory
setwd(path)
setwd("02_dados/01_var/01_brazil_tif")
dir()

# import raster id
var_id <- raster::raster("wc20_brasil_bio01.tif")
var_id

var_id[!is.na(var_id)] <- raster::cellFromXY(var_id, raster::rasterToPoints(var_id)[, 1:2])

# map
tm_shape(var_id) +
  tm_raster(palette = "Greens") +
  tm_shape(br) +
  tm_borders() +
  tm_layout(legend.position = c("left", "bottom")) +
  tm_graticules(lwd = .5) +
  tm_compass(type = "8star", size = 3, position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 500, 1000), text.size = .8)

# oppc
occ_data_tax_date_spa_oppc <- occ_data_tax_date_spa %>% 
  dplyr::mutate(oppc = raster::extract(var_id, dplyr::select(., longitude, latitude))) %>% 
  dplyr::distinct(species, oppc, .keep_all = TRUE) %>% 
  dplyr::filter(!is.na(oppc)) %>% 
  dplyr::add_count(species) %>% 
  dplyr::arrange(species)
occ_data_tax_date_spa_oppc

# verify
table(occ_data_tax_date_spa$species)
table(occ_data_tax_date_spa_oppc$species)

# vector
occ_vector_tax_date_spa_oppc <- occ_data_tax_date_spa_oppc %>% 
  dplyr::mutate(lon = longitude, lat = latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_vector_tax_date_spa_oppc

# map
map_filter_tax_date_spa_oppc <- tm_shape(br) +
  tm_polygons() +
  tm_shape(occ_vector) +
  tm_dots(shape = 21, size = .2, col = "steelblue") +
  tm_shape(occ_vector_tax_date_spa_oppc) +
  tm_dots(shape = 21, size = .2, col = "red") + 
  tm_graticules(lwd = .5) +
  tm_compass(type = "8star", size = 3, position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 500, 1000), text.size = .8) +
  tm_add_legend(title = "Legenda", type = "symbol", 
                labels = c(paste0("Total (", nrow(occ_data), ")"), 
                           paste0("Filtro - OPPC (", nrow(occ_data_tax_date_spa_oppc), ")")), 
                col = c("steelblue", "red"), shape = 21, size = 1) +
  tm_layout(legend.bg.color = TRUE, legend.frame = TRUE)
map_filter_tax_date_spa_oppc

# directory
setwd(".."); setwd("..")
setwd("00_occ")

# export
readr::write_csv(occ_data_tax_date_spa_oppc, paste0("04_occ_data_filter_tax_data_espacial_oppc_", sp, "_", Sys.Date(), ".csv"))
readr::write_csv(occ_data_tax_date_spa_oppc %>% dplyr::select(species, longitude, latitude), 
                 paste0("04_occ_data_filter_tax_data_espacial_oppc_maxent_", sp, "_", Sys.Date(), ".csv"))
sf::write_sf(occ_vector_tax_date_spa_oppc, paste0("04_occ_data_filter_tax_data_espacial_oppc_", sp, "_", Sys.Date(), ".shp"))
tmap::tmap_save(map_filter_tax_date_spa_oppc, paste0("04_occ_data_filter_tax_data_espacial_oppc_", sp, "_", Sys.Date(), ".png"))

# verify filters ----------------------------------------------------------
occ_data %>% nrow()
occ_data_tax$species %>% table
occ_data_tax_date$species %>% table
occ_data_tax_date_spa$species %>% table
occ_data_tax_date_spa_oppc$species %>% table

# end ---------------------------------------------------------------------