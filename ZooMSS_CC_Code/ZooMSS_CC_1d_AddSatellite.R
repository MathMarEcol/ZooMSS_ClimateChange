# library(raster)
# library(ncdf4)
# library(sf)
# library(tidyverse)
#
base_dir <- "/Users/jason/Nextcloud/MME1Data/Satellite/"

robCRS <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
latlonCRS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
new_grid <- raster(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90,
                   crs = latlonCRS)
# fsst <- list.files(paste0(base_dir, "tos"), pattern = paste0(ModelArray[m],"_",ExpArray[e]), full.names = TRUE)

fsst <- list.files(paste0(base_dir, "SST/Annual"), pattern = ".nc", full.names = TRUE)
ssst <- stack(fsst, varname = "sst")
msst <- raster::calc(ssst, mean, na.rm = TRUE)
msstr <- resample(msst, new_grid, method = "bilinear")
msstp <- rasterToPolygons(msstr, n = 8)
msst_sf <- st_as_sf(msstp, xy = TRUE)
msst_sf <- st_transform(msst_sf, crs = st_crs(robCRS)) # Convert to Robinson Projection


fchl <- list.files(paste0(base_dir, "Chlorophyll/Annual"), pattern = ".nc", full.names = TRUE)
schl <- stack(fchl, varname = "chlor_a")
mchl <- raster::calc(schl, mean, na.rm = TRUE)
mchlr <- resample(mchl, new_grid, method = "bilinear")
mchlp <- rasterToPolygons(mchlr, n = 8)
mchl_sf <- st_as_sf(mchlp, xy = TRUE)
mchl_sf <- st_transform(mchl_sf, crs = st_crs(robCRS)) # Convert to Robinson Projection

mchl_sf <- mchl_sf %>%
  mutate(layer = replace(layer, layer > 10, 10))


