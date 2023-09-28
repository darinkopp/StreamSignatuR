library(terra)
library(tmap)


source("R/functions.R")

# read in stream, set stream pixels to 1,  land pixels to 0

r <- rast("data/stream_upstr_basin.tif")
rf <- rast("data/StreamFlux.tif")

r2 <-StreamSignature(r)
r3 <-StreamSignature(r, rf)

p1 <- tm_shape(r2)+
  tm_raster(legend.show = F, palette = "-OrRd" )
p1

p2 <- tm_shape(r3)+
  tm_raster(legend.show = F, palette = "-OrRd" ) 
p2
