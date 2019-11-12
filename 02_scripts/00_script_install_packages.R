# -------------------------------------------------------------------------
# install packages
# mauricio vancine - mauricio.vancine@gmail.com
# 06-11-2019
# -------------------------------------------------------------------------

# occurrences -------------------------------------------------------------
# manipulation and visualization
if(!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE)
if(!require(lubridate)) install.packages("lubridate", dependencies = TRUE)

# occurrences download
if(!require(spocc)) install.packages("spocc", dependencies = TRUE)

# clear - taxonomy
if(!require(taxize)) install.packages("taxize", dependencies = TRUE)

# clear - spatial
if(!require(CoordinateCleaner)) install.packages("CoordinateCleaner", dependencies = TRUE)
if(!require(sf)) install.packages("sf", dependencies = TRUE)

# variables ------------------------------------------------------
# manipulation and visualization
if(!require(raster)) install.packages("raster", dependencies = TRUE)
if(!require(rgdal)) install.packages("rgdal", dependencies = TRUE)
if(!require(tmap)) install.packages("tmap", dependencies = TRUE)

# limits
if(!require(rnaturalearth)) install.packages("rnaturalearth", dependencies = TRUE)

# end ---------------------------------------------------------------------