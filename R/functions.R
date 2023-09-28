
# ---------- define stream signature function ------------------------

#' calculate stream signature as function of distance from stream
#' 
#' @param Distance distance in meters.
#' @param signature_model_options a list of options 
#' @returns a numeric signature value
#' @examples
#' ss(3.5)
#' ss(100)
#' 
ss <- function(Distance, signature_model_options) {
  # return stream signature as function of distance from stream
  # https://doi.org/10.6084/m9.figshare.c.3306405.v1
  
  log_ss <- -0.3716 - 0.318 + 0.3611 - 0.0949 -
    0.1161*log(Distance) - 0.1160 * log(Distance) + 
    0.0110*log(Distance)- 0.1456 * log(Distance)
  ss <- exp(log_ss)
  return(ss)
}



# ---------- main function ------------------------
#' calculate stream signatures for raster of ??
#' 
#' @param r raster
#' @param radius_m maximum distance for matching land and stream cells 
#' @param signature_model_options a list of options 
#' @returns raster of stream signatures of same dimensions as r
#' @examples
#' StreamSignature(r, 1000)
#' 
StreamSignature <- function(r, rf, radius_m=1000, signature_model_options=list()){
  require(terra)
  require(dplyr)
  require(tidyr)
  require(fuzzyjoin)
  
  # assign 0 values to replace NA (land cells)
  r[is.na(r)] <- 0
  
  # assign a value of 1 to all other cells (stream cells)
  r[r>0] <- 1
  
  if(missing(rf)){
    rf <- r
    values(rf)[,1] <- 1
  }
  # create a dataframe from the raster
  # starting with the cell coordinates
  xy <- terra::crds(r)
  dfRaster <- data.frame(xy) 
  
  # add the cell values to the dataframe
  val<- terra::values(r)[,1]
  val<-ifelse(is.na(val),0,val)
  dfRaster$val <- val
  
  flux <- terra::values(rf)[,1]
  dfRaster$flux <- flux
  
  
  # add the cell number to the dataframe
  cell <- terra::cellFromXY(r,xy)
  dfRaster$cell <- cell
  
  # get a subset of dataframe for land cells
  dfLand <- dfRaster %>%
    dplyr::filter(val==0) %>%
    dplyr::select(-val) %>%
    dplyr::select(-flux) %>%
    rename(cellLand=cell, lon=x, lat=y)
  
  # get a subset of dataframe for stream cells
  dfStream <- dfRaster %>%
    dplyr::filter(val==1) %>%
    dplyr::select(-val) %>%
    rename(cellStream=cell,lonS=x,latS=y)
  
  # use fuzzy join to find distances between land and stream cells
  # creates a dataframe with one row for each combination of land cell and stream cell
  # only cells with distance less than max_dist are "matched"
  df <- dfLand %>%
    fuzzyjoin::geo_full_join(dfStream, 
                             by=c("lon"="lonS","lat"="latS"), 
                             max_dist=radius_m/1000, 
                             unit="km", 
                             distance_col = "d_km")
  
  # calculate response for each combination of land cell and stream cell
  df <- df %>%
    mutate(s=ss(d_km*1000))
  
  # normalise the signatures so that the sum for each stream cell is 1.0
  df <- df %>%
    group_by(cellStream) %>%
    mutate(s_norm=s/(sum(s,na.rm=T))) %>%
    ungroup()
  
  
  # sum the response for each 
  df <- df %>%
    group_by(cellLand) %>%
    summarise(s=sum(s_norm*flux,na.rm=T),.groups="drop") 
  
  # join back to original data
  dfRaster <- dfRaster %>%
    left_join(df, by=c("cell"="cellLand")) 
  
  # replace missing values with zeros
  dfRaster <- dfRaster %>%
    mutate(s=ifelse(is.na(s),0,s))
  
  # replace the values in the raster with the calculated signature values
  r2 <- r
  terra::values(r2)[,1] <- dfRaster$s
  
  return(r2)
  
}
