# Preprocessing -- Attribute Stream Network
# to create stream signautre raster 


# Starting with NEON sites for example
# attribute each site with a COMID -- be mindful 
# of API overload. Data are saved - Don't RUN. 
##########
# df <- read.csv("data/NEON-SiteMap-Table.csv") %>%
#   filter(!stateCode %in% c("AK", "PR"))
# 
# comids <- sc_get_comid(df,
#                        xcoord = "longitude", 
#                        ycoord = "latitude", 
#                        crsys = 4269) %>%
#   strsplit(",") %>%
#   unlist()
# 
# df <- mutate(df, comids)%>%
#   filter(terrain=="AQUATIC")
#####################################
#write.csv(df,"data/NEON_Aquatic.csv")


# df <- read.csv("data/NEON_Aquatic.csv")
# df <- df[df$HAS_WIDTH & !is.na(df$HAS_WIDTH),]

# Download nhdplus files for network 
# can 
# blackfoot river 24479247; Andrews LTER 23773411 

# can loop through COMID's to create 
#for (site in (15:nrow(df))){
# site <- 1 
# site_code <- df[site, "siteCode"]
# start_comid <- as.numeric(df[site, "comids"])

library(StreamCatTools)
library(nhdplusTools)
library(sf)
library(tidyverse)

start_comid <- 24479247
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

# need vector of flowlines, attributed with wetted width and flux
sub_comids <- sub_comids[,c("wettedwidth", "flux")]

st_write(sub_comids,"data/blackfootR_50.gpkg", layer="streamNetwork")
