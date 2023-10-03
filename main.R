library(terra)
library(tmap)
#
source("R/functions.R")

# ----------- Read input data -----------

# Load stream raster.
 r <- rast("data/stream_upstr_basin.tif")

# Load stream flux raster.
rf <- rast("data/StreamFlux.tif")

# Load fractional composition of benthic organism groups.
rc <- rast("data/Benthic_Composition.tif")

# ----------- Calculate Stream signatures --------------------

# Calculate stream signature using only stream raster.
r2 <-StreamSignature(r)
 
# Calculate stream signature using a flux raster.
r3 <-StreamSignature(r, rf)

# Calculate stream signature using a flux raster and a benthic composition raster.
r4 <-StreamSignature(r, rf, rc)
tm_shape(r4)+
  tm_raster(legend.show = F, palette = "-OrRd" ) 

# Benthic composition can also be supplied as a spatially-invariant list.
rclist <- list(E=0.25,P=0.25,T=0.25,C=0.25)
r5 <-StreamSignature(r, rf, rclist)

# ----------- Composition fractions --------------------

# The StreamSignature function gives a warning if the sums of organism group
# composition fractions are not equal to 1.0.
# By default, calculations will continue anyway.

# Using option to cause an error and stop processing if the sums of 
# organism group fractions are not equal to 1.0.

rclist <- list(E=0.25,P=0.25,T=0.25,C=0.27)
r6 <-StreamSignature(r, rf, rclist, options=list(check_composition_sums=TRUE))

print(head(r6))

# The function will only recognise benthic group fractions specified using
# the following names: A, C, E, P, T 

# A - All aquatic insects
# E - Mayflies
# P - Stoneflies
# T - Caddisflies
# C - Chironomids

rclist <- list(E=0.25,P=0.25,T=0.25,X=0.25)
r7 <-StreamSignature(r, rf, rclist)

# Note that in this case, the group "X" is not recognized.
# The weightings for the remaining 3 groups then become 0.33 
# 0.25 / (0.25 + 0.25 + 0.25)

