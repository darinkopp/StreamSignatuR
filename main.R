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

tmap::tmap_save(p2, file="figures/example.png", width=8, height=10.5, units="cm", dpi=300)


# we have two new functions:
#
# ss(Distance, a, b) - calculates stream signature as function of distance from stream
#
# ss_parameters() returns the parameters a and b for the function the 
#
# examples:

ss_parameters(method="Dispersal", print_coefficients=T)

ss_parameters(method="Dispersal", organism="Caddisflies", model=2, print_coefficients=T)
