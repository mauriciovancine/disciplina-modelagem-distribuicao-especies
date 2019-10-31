# -------------------------------------------------------------------------
# occ - download and taxonomic, data, and spatial filter
# mauricio vancine - mauricio.vancine@gmail.com
# 19-07-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(CoordinateCleaner)
library(landscapetools)
library(lubridate)
library(raster)
library(rgdal)
library(rnaturalearth)
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
path <- "/home/mude/data/gitlab/course-sdm"
setwd(path)
dir()

# download occurrences ----------------------------------------------------
# species
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

# limit brazil
br <- rnaturalearth::ne_countries(country = "Brazil", scale = "small", returnclass = "sf")
br

# map
ggplot() +
  geom_sf(data = br) +
  geom_point(data = occ_data, aes(x = longitude, y = latitude)) +
  theme_bw()

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

# map
ggplot() +
  geom_sf(data = br) +
  geom_point(data = occ_data, aes(x = longitude, y = latitude)) +
  geom_point(data = occ_data_tax, aes(x = longitude, y = latitude), color = "red") +
  theme_bw()

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

ggplot() + 
  geom_histogram(data = occ_data_tax_date, aes(year), color = "darkred", fill = "red", bins = 10, alpha = .5) +
  theme_bw()

# map
ggplot() +
  geom_sf(data = br) +
  geom_point(data = occ_data, aes(x = longitude, y = latitude)) +
  geom_point(data = occ_data_tax_date, aes(x = longitude, y = latitude), color = "red") +
  theme_bw()

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
            "outliers", # records far away from all other records of this species
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

# map
ggplot() +
  geom_sf(data = br) +
  geom_point(data = occ_data_na, aes(x = longitude, y = latitude)) +
  geom_point(data = occ_data_tax_date_spa, aes(x = longitude, y = latitude), color = "red") +
  theme_bw()

# oppc --------------------------------------------------------------------
# directory
setwd(path); setwd("03_var")

# import raster id
var_id <- raster::raster("wc20_brasil_res05g_bio03.tif")
var_id

var_id[!is.na(var_id)] <- raster::cellFromXY(var_id, raster::rasterToPoints(var_id)[, 1:2])
landscapetools::show_landscape(var_id) +
  geom_polygon(data = var_id %>% raster::rasterToPolygons() %>% fortify, 
               aes(x = long, y = lat, group = group), fill = NA, color = "black", size = .1) +
  theme(legend.position = "none")

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

# map
ggplot() +
  geom_sf(data = br) +
  geom_polygon(data = var_id %>% raster::rasterToPolygons() %>% fortify, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black", size = .2) +
  geom_point(data = occ_data_tax_date_spa, aes(x = longitude, y = latitude)) +
  geom_point(data = occ_data_tax_date_spa_oppc, aes(x = longitude, y = latitude), color = "red") +
  theme_bw()

# verify filters ----------------------------------------------------------
occ_data_tax$species %>% table
occ_data_tax_date$species %>% table
occ_data_tax_date_spa$species %>% table
occ_data_tax_date_spa_oppc$species %>% table

# export ------------------------------------------------------------------
# directory
setwd(path)
dir.create("02_occ")
setwd("02_occ")

# export
readr::write_csv(occ_data, paste0("occ_spocc_bruto_", lubridate::today(), ".csv"))
readr::write_csv(occ_data_tax_date_spa_oppc, paste0("occ_spocc_filtros_taxonomico_data_espatial_oppc.csv"))

# end ---------------------------------------------------------------------s