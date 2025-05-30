# a landscape analysis of aquatic terrestrial subsidies 
rm(list = ls())
library(fuzzyjoin)
library(terra)
library(stars)
library(tmap)
source("R/functions.R")
library(remotes)
library(StreamCatTools)
library(nhdplusTools)

# add NEON sites for example -- get_comid 
# overloads API - only run once
##########
df <- read.csv("data/NEON-SiteMap-Table.csv") %>%
  filter(!stateCode %in% c("AK", "PR"))

comids <- sc_get_comid(df,
             xcoord = "longitude", 
             ycoord = "latitude", 
             crsys = 4269) %>%
  strsplit(",") %>%
  unlist()

df <- mutate(df, comids)%>%
  filter(terrain=="AQUATIC")
#####################################
#dont overwrite. identified order and whether
#streams were available

#write.csv(df,"data/NEON_Aquatic.csv")
df <- read.csv("data/NEON_Aquatic.csv")
df <- df[df$HAS_WIDTH&!is.na(df$HAS_WIDTH),]

# Download nhdplus files for network -- change 
# blackfoot river 24479247; Andrews LTER 23773411 

#for (site in (15:nrow(df))){
  # site <- 1 
  
  site_code <- df[site, "siteCode"]
  start_comid <- as.numeric(df[site, "comids"])
  # start_comid <- 24479247
  ###########
  flowline <- navigate_nldi(list(featureSource = "comid", 
                                 featureID = start_comid), 
                            mode = "upstreamTributaries", 
                            distance_km = 100)

  subset_file <- tempfile(fileext = ".gpkg")

  #subset_file <- file.path(paste0("data/neon/",site_code,".gpkg"))
  subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                           output_file = subset_file,
                           nhdplus_data = "download", 
                           flowline_only = FALSE,
                           return_data = TRUE, 
                           overwrite = TRUE)

  flowline <- subset$NHDFlowline_Network
  catchment <- subset$CatchmentSP
  waterbody <- subset$NHDWaterbody
  ########################################


# reduce network to 50km upstream of starting COMID
#########
sub_comids <- get_UT(flowline, start_comid, distance = 50)
sub_comids <- dplyr::filter(flowline, comid %in% sub_comids)

catch <- get_nhdplus(comid = sub_comids$comid, 
                     realization = "catchment")
ws <- st_union(catch)
#############################################

# attribute COMIDs with wetted widths -- there can be missing vals(!?),
# flux (gm2yr), or composition (EPTrC relative abundance) 
######
wwidth <- sub_comids$comid%>%
  as.character()%>%
  sc_get_data(metric = "WettedWidth",
              aoi = "other")
sub_comids <- merge(sub_comids, wwidth, by = "comid", all.x=T)

# used stream order to assign mean wetted width 
# for missing values. Need to investigate doyle for 
# model predictions

meanVals <- sub_comids %>%
  st_set_geometry(NULL) %>%
  group_by(streamorde) %>%
  summarize(meanWW = mean(wettedwidth, na.rm = T))

for (i in 1:nrow(meanVals)){
  # i<-1
  sub_comids[is.na(sub_comids$wettedwidth) &
               sub_comids$streamorde == meanVals$streamorde[i],
    "wettedwidth"] <- meanVals$meanWW[i]
}

# mgDM 1910; mgC 860; mgN 140; mgPUFA 20; mgP 10
sub_comids$flux <- 1.910
##################################

# need vector of flowlines, attributed with wetted width and flux
sub_comids <- sub_comids[,c("wettedwidth", "flux")]


library(sf)
list.files("data/EU_Streams/reach_NETRACE_noce.shp")
eunet <- read_sf("data/EU_Streams/reach_NETRACE_noce.shp")
eunet$flux <- 1.910
sub_comids <- eunet[,c("WIDTH_M","flux")]
colnames(sub_comids)[1:2]<-c("wettedwidth", "flux")
sub_comids<-st_transform(sub_comids, crs=4326)
#write_sf(sub_comids, data/b)
# creating flux raster 
######
#create raster template from watershed
#bb <- st_bbox(ws)
bb <- st_bbox(sub_comids)
tmpRas <- rast(xmin = bb$xmin - 0.01, xmax = bb$xmax + 0.01,
               ymin = bb$ymin - 0.01, ymax = bb$ymax + 0.01,
               nrows = 200, ncols = 200)

v <- vect(sub_comids)

# calculate stream length within raster cell
l <- rasterizeGeom(v, tmpRas, "length", "m")
w <- rasterize(v, tmpRas, field = "wettedwidth")
fx<-rasterize(v, tmpRas, field = "flux")
flux <- (l*w)*fx

# flux raster
flux[flux==0] <- NA

# stream raster 
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
StrSig[StrSig==0]<-NA

#save stream signature raster
writeRaster(StrSig, filename = paste0("figures/StreamSignature_EUNet.tif"))
windows()
plot(StrSig)



P1 <- tm_shape(sub_comids,bbox = st_bbox(StrSig))+
  tm_lines(lwd=1.25, col = "blue")+
  tm_shape(ws)+
  tm_borders(lwd=1.5)+
  tm_layout(meta.margins = c(.2, 0, 0, 0))

remove.packages("tmap")
install.packages("tmap")
plot(StrSig)
P2 <- tm_shape(StrSig, bbox = st_bbox(StrSig)) +
  tm_raster(col.scale = tm_scale_continuous(values = terrain.colors(9)), 
            col.legend = tm_legend(title = "gDMyr"))+
  tm_shape(sub_comids)+
  tm_lines(lwd=1.25, col = "blue")+
  tm_shape(ws)+
  tm_borders(lwd=1.5)+
  tm_layout(meta.margins = c(.2, 0, 0, 0))

tmap_arrange(P1, P2, sync = T)
P2
tmap_mode("view")
windows()
P2

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
