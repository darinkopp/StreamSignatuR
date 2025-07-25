# a landscape analysis of aquatic-terrestrial subsidies 
rm(list = ls())
library(fuzzyjoin)
library(terra)
library(stars)
library(tmap)
source("R/functions.R")
library(StreamCatTools)
library(nhdplusTools)
library(sf)

# EU Network
######

eunet <- read_sf("data/EU_Streams/reach_NETRACE_noce.shp")
eunet$flux <- 1.910
sub_comids <- eunet[,c("WIDTH_M","flux")]
colnames(sub_comids)[1:2]<-c("wettedwidth", "flux")
streamNet<-st_transform(sub_comids, crs=4326)

#####################

streamNet <- st_read("data/blackfootR_50.gpkg", layer = "streamNetwork")
Catchment <- st_read("data/blackfootR_50.gpkg", layer = "Catchment")

# creating flux raster 
######
#create raster template from watershed
bb <- st_bbox(streamNet)
tmpRas <- rast(xmin = bb$xmin - 0.01, xmax = bb$xmax + 0.01,
               ymin = bb$ymin - 0.01, ymax = bb$ymax + 0.01,
               nrows = 100, ncols = 100)
sqrt(cellSize(tmpRas,unit="km"))
v <- vect(streamNet)

# calculate stream length within raster cell
l <- rasterizeGeom(v, tmpRas, "length", "m")
w <- rasterize(v, tmpRas, field = "wettedwidth")
fx<-rasterize(v, tmpRas, field = "flux")
flux <- (l*w)*fx

# flux raster
flux[flux==0] <- NA

# stream raster --- this is redundant and may not be needed
# the flux raster is the stream raster... c
l[l > 0] <- 1
l[l ==0] <- NA
######################


#next is to replace the stream 
#raster with the flux raster... these no need 
#to supply 1 raster. Toggle switch is flux or SS

# add error for networks that exceed memory

#400mcg/d se is harmful -- should we truncate yearly emergence 
# to growing season or a min growing degree days to first freeze? 

StrSig <- StreamSignature(r = l, rf = flux, radius_m = 1000)
StrSig[StrSig==0] <- NA


#save stream signature raster
#writeRaster(StrSig, filename = paste0("figures/StreamSignature_EU.tif"))
writeRaster(StrSig, filename = paste0("figures/StreamSignature_Blackfoot.tif"),
                                      overwrite = T)


StrSig <- rast(paste0("figures/StreamSignature_EU.tif"))
StrSig <- rast(paste0("figures/StreamSignature_Blackfoot.tif"))
sqrt(terra::cellSize(StrSig, unit = "m"))


tmap_mode("view")
P2 <- tm_shape(StrSig, bbox = st_bbox(StrSig)) +
  tm_raster(col.scale = tm_scale_continuous(values = topo.colors(10, rev = T)), 
            col_alpha = 0.75,
            col.legend = tm_legend(title = "gDMyr"))+
   tm_shape(streamNet)+
   tm_lines(lwd=1.25, col = "blue")+
   tm_shape(Catchment)+
   tm_borders(lwd=1.5)+
  tm_layout(meta.margins = c(.2, 0, 0, 0),
            frame = FALSE)+
  tm_basemap("OpenTopoMap")

library("maptiles")
tmap_arrange(P1, P2, sync = T)
P2
tmap_mode("plot")
windows()
P2
search()
tmap_save(P2, filename = "Export_DM_BlackfootR.jpeg", 
          width=10,height=10, units = "in")
P2
###########################


######


# attempted flow accumulation raster
##########
d<-"C:/Users/Owner/Dropbox/AquaSync/StreamSignatuR/data/NHDPlusV21_PN_17_17a_FdrFac_01/NHDPlusPN/NHDPlus17/NHDPlusFdrFac17a/fac"
?raster
fac <- read_stars(d)
ws <- st_transform(ws, crs = st_crs(fac))
fac <- st_crop(fac,ws)
facSpatRaster<-as(fac,"SpatRaster")
net <- st_transform(UT_COMIDs,crs = st_crs(fac))
st_crs(fac)
st_res(fac)
90x=2000/90

facSpatRaster<-as(fac,"SpatRaster")


library(raster)
extent(ws)
st_crop(fac,ws)

facSpatRaster[facSpatRaster > 100] <- 1
facSpatRaster[facSpatRaster != 1] <- NA
extract(facSpatRaster,UT_COMIDs)
tmap_mode("view")
tm_shape(facSpatRaster)+
  tm_raster()+

tm_shape(fac)+
  tm_raster()+
tm_shape(fline)+
  tm_lines()
################################################


library(nhdplusTools)
source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))
start_COMID <- 11689302
# delineate stream network
############
UT_COMIDs <- get_UT(sample_flines, start_COMID, distance = 15)
UT_COMIDs <- dplyr::filter(sample_flines, COMID %in% UT_COMIDs)
UT_COMIDs$STREAM <- 1

#get catchments
Catchment <- get_nhdplus(comid = UT_COMIDs$COMID, 
                         realization = "catchment")

#flow accumulation grid

####################################

# Stream signature only works on unprojected rasters
# UT_COMIDs <- st_transform(UT_COMIDs, 26985)

# prepare raster
#######
v <- vect(UT_COMIDs)
r <- rast(v, nrows=100, ncols=100)
r = extend(x, ext(r) + 0.025)
x <- rasterizeGeom(v, r, "count")
x[x==0] <- NA
x[x > 0] <- 1
###########################

#create flux raster
#####





# needs to be a SpatRaster from Terra
# change organism compositon...
StrSig <- StreamSignature(r, rc=list(Tr=1), radius_m = 500)
StrSig[StrSig==0] <- NA

tmap_mode("view")
P2<-tm_shape(StrSig)+
  tm_raster()+
  tm_shape(UT_COMIDs)+
  tm_lines(lwd=1.25, col = "blue")
windows()
print(P2)

# Extras
######################
pts <- as.points(x)

tmap_mode("view")
s <- sf::st_as_sf(v)
p <- sf::st_as_sf(pts)
tm_shape(x)+
  tm_raster()+
  tm_shape(s)+
  tm_lines()+
  tm_shape(p)+
  tm_dots()
