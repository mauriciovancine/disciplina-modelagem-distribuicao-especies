# -------------------------------------------------------------------------
# var - download, adjust extention, adjust resolution, and correlation
# mauricio vancine - mauricio.vancine@gmail.com
# 17-07-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(landscapetools)
library(psych)
library(raster)
library(rgdal)
library(rnaturalearth)
library(tidyverse)
library(wesanderson)

# informations
# https://ropensci.org/
# https://github.com/ropensci/rnaturalearth
# https://www.naturalearthdata.com/
# https://github.com/r-spatial/sf
# https://www.worldclim.org/

# directory
path <- "/home/mude/data/gitlab/course-sdm"
setwd(path)
dir()

# download ----------------------------------------------------------------
# directory
dir.create("03_var")
setwd("03_var")

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
ggplot() +
  geom_sf(data = br) +
  theme_bw()

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
var_br <- raster::crop(x = var, y = br) %>% 
  raster::mask(mask = br)
var_br

# plot
landscapetools::show_landscape(var_br$bio01)  +
  geom_polygon(data = var_br$bio01 %>% raster::rasterToPolygons() %>% fortify, 
               aes(x = long, y = lat, group = group), fill = NA, color = "black", size = .1) +
  theme(legend.position = "none")

# adjust resolution -------------------------------------------------------
# resolution
raster::res(var_br)[1]
raster::res(var_br)[1]/(30/3600) # ~km

# aggregation factor 
res_actual <- raster::res(var_br)[1]
res_actual

res_adjust <- 0.5
res_adjust

agg_fac <- res_adjust/res_actual
agg_fac

# aggregation
var_br_05 <- raster::aggregate(var_br, fact = agg_fac)
var_br_05

# new resolution
raster::res(var_br_05)[1]

# plot
landscapetools::show_landscape(var_br$bio01) +
  geom_polygon(data = var_br$bio01 %>% raster::rasterToPolygons() %>% fortify, 
               aes(x = long, y = lat, group = group), fill = NA, color = "black", size = .1) +
  theme(legend.position = "none")
landscapetools::show_landscape(var_br_05$bio01) +
  geom_polygon(data = var_br_05$bio01 %>% raster::rasterToPolygons() %>% fortify, 
               aes(x = long, y = lat, group = group), fill = NA, color = "black", size = .1) +
  theme(legend.position = "none")

# export
dir.create("00_var")
setwd("00_var")
raster::writeRaster(x = var_br_05, 
                    filename = paste0("wc20_brasil_res05g_", names(var_br)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    overwrite = TRUE)

# exclude
setwd("..")
dir(pattern = ".tif") %>% 
  unlink()

# correlation -------------------------------------------------------------
# directory
dir.create("01_correlation") 
setwd("01_correlation")
getwd()

# extract values
var_da <- var_br_05 %>% 
  raster::values() %>% 
  tibble::as_tibble() %>% 
  tidyr::drop_na()
var_da

# verify
head(var_da)
dim(var_da)

# correlation
cor_table <- corrr::correlate(var_da, method = "spearman") 
cor_table

# preparate table
cor_table_summary <- cor_table %>% 
  corrr::shave() %>%
  corrr::fashion()
cor_table_summary

# export
readr::write_csv(cor_table_summary, "correlacao.csv")


# select variables
# correlated variables
fi_06 <- cor_table %>% 
  corrr::as_matrix() %>% 
  caret::findCorrelation(cutoff = .6, names = TRUE, verbose = TRUE)
fi_06

# select
var_da_cor06 <- var_da %>% 
  dplyr::select(-fi_06)
var_da_cor06

# verify
var_da_cor06 %>% 
  corrr::correlate(method = "spearman") %>% 
  corrr::as_matrix() %>% 
  caret::findCorrelation(cutoff = .6, names = TRUE, verbose = TRUE)

# graphic
tiff("correlacao_plot.tiff", wi = 30, he = 25, un = "cm", res = 300, comp = "lzw")
pairs.panels(x = var_da_cor06 %>% dplyr::sample_n(1e3), 
             method = "spearman",
             pch = 20, 
             ellipses = FALSE, 
             density = FALSE, 
             stars = TRUE, 
             hist.col = "gray",
             digits = 2,
             rug = FALSE,
             breaks = 10,
             ci = TRUE)
dev.off()

# copy var not correlated -------------------------------------------------
# directory
setwd(path)
setwd("03_var/00_var")

# copy vars to 03_var
dir() %>% 
    grep(pattern = paste0(fi_06, collapse = "|"), invert = TRUE, value = TRUE) %>% 
    file.copy(., "/home/mude/data/gitlab/course-sdm/03_var")

# end ---------------------------------------------------------------------