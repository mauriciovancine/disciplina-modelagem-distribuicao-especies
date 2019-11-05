# -------------------------------------------------------------------------
# var - download, adjust extention, adjust resolution
# mauricio vancine - mauricio.vancine@gmail.com
# 06-11-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(raster)
library(rgdal)
library(rnaturalearth)
library(tidyverse)

# informations
# https://ropensci.org/
# https://github.com/ropensci/rnaturalearth
# https://www.naturalearthdata.com/
# https://github.com/r-spatial/sf
# https://www.worldclim.org/

# directory
path <- "/home/mude/data/disciplina-modelagem-distribuicao-especies"
setwd(path)
dir()

# download ----------------------------------------------------------------
# directory
dir.create("02_dados"); dir.create("02_dados/01_var"); dir.create("02_dados/01_var/00_raw")
setwd("02_dados/01_var/00_raw")

# download
download.file(url = "http://biogeo.ucdavis.edu/data/worldclim/v2.0/tif/base/wc2.0_10m_bio.zip",
              destfile = "wc2.0_10m_bio.zip")

# unzip
unzip("wc2.0_10m_bio.zip")

# bioclimates
# BIO01 = Temperatura media anual
# BIO02 = Variacao da media diurna (media por mes (temp max - temp min))
# BIO03 = Isotermalidade (BIO02/BIO07) (* 100)
# BIO04 = Sazonalidade da temperatura (desvio padrao deviation *100)
# BIO05 = Temperatura maxima do mes mais quente
# BIO06 = Temperatura minima do mes mais frio
# BIO07 = Variacao da temperatura anual (BIO5-BIO6)
# BIO08 = Temperatura media do trimestre mais chuvoso
# BIO09 = Temperatura media do trimestre mais seco
# BIO10 = Temperatura media do trimestre mais quente
# BIO11 = Temperatura media do trimestre mais frio
# BIO12 = Precipitacao anual
# BIO13 = Precipitacao do mes mais chuvoso
# BIO14 = Precipitacao do mes mais seco
# BIO15 = Sazonalidade da precipitacao (coeficiente de variacao)
# BIO16 = Precipitacao do trimestre mais chuvoso
# BIO17 = Precipitacao do trimestre mais seco
# BIO18 = Precipitacao do trimestre mais quente
# BIO19 = Precipitacao do trimestre mais frio

# adjust extention --------------------------------------------------------
# limit
br <- rnaturalearth::ne_countries(country = "Brazil", scale = "small", returnclass = "sf")
br

# plot
tm_shape(br) +
  tm_polygons() +
  tm_graticules(lwd = .5) +
  tm_compass(type = "8star", size = 3, position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 500, 1000), text.size = .8)

# import bioclimates
# list files
tif <- dir(pattern = "tif$")
tif

# import
var <- raster::stack(tif)
var

# names
names(var)
names(var) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
names(var)
var

# plot
plot(var$bio01)

# adust extention
# crop = adjust extention
# mask = adjust to mask
var_br_crop <- raster::crop(x = var, y = br)
var_br_crop

# plot
tm_shape(var_br_crop$bio01) +
  tm_raster(palette = "-Blues") +
  tm_shape(br) +
  tm_borders() +
  tm_layout(legend.position = c("left", "bottom")) +
  tm_graticules(lwd = .5) +
  tm_compass(type = "8star", size = 3, position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 500, 1000), text.size = .8)

# mask = adjust to mask
var_br_crop_mask <- raster::mask(x = var_br_crop, mask = br)
var_br_crop_mask

# plot
tm_shape(var_br_crop_mask$bio01) +
  tm_raster(palette = "-Blues") +
  tm_shape(br) +
  tm_borders() +
  tm_layout(legend.position = c("left", "bottom")) +
  tm_graticules(lwd = .5) +
  tm_compass(type = "8star", size = 3, position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 500, 1000), text.size = .8)

# export
# directory
setwd("..")
dir.create("01_brazil_tif")
setwd("01_brazil_tif")

# tif
raster::writeRaster(x = var_br_crop_mask, 
                    filename = paste0("wc20_brasil_", names(var_br_crop_mask)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    overwrite = TRUE)

# directory
setwd("..")
dir.create("02_brazil_asc")
setwd("02_brazil_asc")

# asc
raster::writeRaster(x = var_br_crop_mask, 
                    filename = paste0("wc20_brasil_", names(var_br_crop_mask)), 
                    bylayer = TRUE, 
                    format = "ascii", 
                    overwrite = TRUE)

# end ---------------------------------------------------------------------