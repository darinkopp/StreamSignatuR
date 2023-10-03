require(dplyr)
# ---------- define stream signature function ------------------------

#' calculate stream signature as function of distance from stream
#' 
#' @param Distance distance in meters.
#' @param a parameter in function log_s = a * distance + b
#' @param b parameter in function log_s = a * distance + b
#' @returns a numeric signature value
#' @examples
#' ss(3.5)
#' ss(100, a=-0.1456, b=-0.0949)
#' 
ss <- function(Distance, params=list(a=-0.3667, b=-0.4872)) {
  
  # return stream signature as function of distance from stream
  # https://doi.org/10.6084/m9.figshare.c.3306405.v1
  
  log_ss <- params$a*log(Distance) + params$b
  ss <- exp(log_ss)
  return(ss)
}


# ---------- get stream signature function parameters ----------------

#' return parameters for the stream signature function
#' based on stream characteristics, species, etc.
#' 
#' @param method "Food web", "Dispersal" (default) or "Predator abundance" 
#' @param organism "All aquatic insects", "Mayflies", "Stoneflies"
#                  "Caddisflies", "Chironomids", "Terrestrial predators"
#' @param productivity "Low" (default), "Medium" or "High"
#' @param streamorder integer from 1 to 7
#' @param geomorphology "Meandering", "Straight/Meandering" or "Straight"
#' @param banktype "Gradual", "Gradual/Steep" or "Steep"
#' @param model model type integer from 1 to 5 (default=1)
#' @param print_coefficients boolean - use for debugging to show values for individual coefficients 
#' @returns a list of 2 numeric values (a,b) for function log_s = a * distance + b
#' @examples 
#' ss_parameters(method="Food web")
#' ss_parameters(method="Dispersal", organism="Caddisflies", model=2)
#' @note character parameters are case insensitive
#' 
ss_parameters <- function(method="Dispersal",
                          organism="All aquatic insects",
                          productivity="Low",
                          streamorder=1,
                          geomorphology=NA_character_,
                          banktype=NA_character_,
                          model=1,
                          print_coefficients=F) {
  
  method <- tolower(method)
  organism <- tolower(organism)
  productivity <- tolower(productivity)
  streamorder <- ifelse(is.na(streamorder),1,streamorder)
  geomorphology <- tolower(geomorphology)
  banktype <- tolower(banktype)
  model <- ifelse(is.na(model),1,model)
  
  vect_beta0 = c(-0.3716, -0.2944, -0.3132, -0.5025, -0.4713)
  vect_beta1 = c(-0.1161, -0.1679, -0.117, -0.1161, -0.1155)
  
  # vector of coefficients for *Method* interactive term
  vect_a_method <- case_when(
    method == "food web" ~ c(0,0,0,0,0),
    method == "dispersal" ~ c(-0.116,0,-0.1149,-0.1162,-0.1164),
    method == "predator abundance" ~ c(0.0416,0,0.0437,0.0409,0.0387),
    .default = c(0,0,0,0,0))
  
  # vector of coefficients for *Method* additive term
  vect_b_method <- case_when(
    method == "food web" ~ c(0,0,0,0,0),
    method == "dispersal" ~ c(-0.3818,-0.5664,-0.3875,-0.3909,-0.3786),
    method == "predator abundance" ~ c(0.1265,0.192,0.1044,0.1326,0.1373),
    .default = c(0,0,0,0,0))
  
  # vector of coefficients for *Organism* interactive term
  vect_a_organism <- case_when(
    organism == "all aquatic insects" ~  c(0,0,0,0,0),
    organism == "mayflies" ~ c(-0.0868,-0.1613,-0.0868,-0.0834,-0.0855),
    organism == "stoneflies" ~ c(-0.1215,-0.1967,-0.1215,-0.118,-0.12),
    organism == "caddisflies" ~ c(0.011,-0.0618,0.0102,0.0122,0.0115),
    organism == "chironomids" ~ c(-0.0021,-0.0708,-0.0028,0.0004,-0.0021),
    organism == "terrestrial predators" ~ c(-0.1734,-0.1029,-0.1732,-0.1742,-0.1734),
    .default = c(0,0,0,0,0))
  
  # vector of coefficients for *Organism* additive term
  vect_b_organism <- case_when(
    organism == "all aquatic insects" ~ c(0,0,0,0,0),
    organism == "mayflies" ~ c(0.3858,0.5086,0.3561,0.4095,0.3689),
    organism == "stoneflies" ~ c(0.6835,0.8081,0.6702,0.7201,0.6651),
    organism == "caddisflies" ~ c(0.3611,0.4796,0.3365,0.4132,0.3514),
    organism == "chironomids" ~ c(0.9405,1.0554,0.9072,0.9603,0.9228),
    organism == "terrestrial predators" ~ c(-0.3113,-0.4107,-0.308,-0.312,-0.3152),
    .default = c(0,0,0,0,0))
  
  # vector of coefficients for *Productivity* interactive term
  vect_a_productivity <- case_when(
    productivity == "high" ~ c(0,0,0,0,0),
    productivity == "medium" ~ c(0.0203,0.0311,0.0209,0.0183,0.0198),
    productivity == "low" ~ c(-0.1456,-0.1336,-0.1449,-0.1455,-0.1466),
    .default = c(-0.1456,-0.1336,-0.1449,-0.1455,-0.1466))
  
  # vector of coefficients for *Productivity* additive term
  vect_b_productivity <- case_when(
    productivity == "high" ~ c(0,0,0,0,0),
    productivity == "medium" ~ c(-0.3246,-0.3389,-0.2981,-0.3775,-0.4089),
    productivity == "low" ~ c(-0.0949,-0.1205,-0.0886,-0.0988,-0.1312),
    .default = c(-0.0949,-0.1205,-0.0886,-0.0988,-0.1312))
  
  # vector of coefficients for *Stream order* additive term
  vect_b_streamorder <- case_when(
    streamorder > 6 ~ c(0,0,0,0.3426,0),
    streamorder > 4 ~ c(0,0,0,-0.105,0),
    streamorder > 2  ~ c(0,0,0,0.3121,0),
    .default = c(0,0,0,0,0))
  
  # vector of coefficients for *Geomorphology* additive term
  vect_b_geomorphology <- case_when(
    geomorphology == "meandering" ~ c(0,0,0,0,0),
    geomorphology == "straight/meandering" ~ c(0,0,-0.2085,0,0),
    geomorphology == "straight" ~ c(0,0,0,0,0.1417),
    .default = c(0,0,0,0,0))
  
  # vector of coefficients for *Bank type* additive term
  vect_b_banktype <- case_when(
    banktype == "gradual" ~ c(0,0,0,0,0),
    banktype == "gradual/steep" ~ c(0,0,-0.2085,0,0),
    banktype == "steep" ~ c(0,0,0.1106,0,0),
    .default = c(0,0,0,0,0))
  
  # for each term select a single value based on selected Model (1-5)
  beta0 <- vect_beta0[model]
  beta1 <- vect_beta1[model]
  a_method <- vect_a_method[model]
  a_organism <- vect_a_organism[model]
  a_productivity <- vect_a_productivity[model]
  b_method <- vect_b_method[model]
  b_organism <- vect_b_organism[model]
  b_productivity <- vect_b_productivity[model]
  b_streamorder <- vect_b_streamorder[model]
  b_geomorphology <- vect_b_geomorphology[model]
  b_banktype<- vect_b_banktype[model]
  
  msg <- ifelse(print_coefficients,
                .ss_coefficients(method, organism,
                                 productivity, streamorder, 
                                 geomorphology, banktype, model,
                                 beta0, beta1, a_method, a_organism, 
                                 a_productivity, b_method, b_organism,
                                 b_productivity, b_streamorder, 
                                 b_geomorphology,b_banktype),
                NA_character_)
  
  if(print_coefficients) message(msg)
  
  # calculate the combined interactive (a) and additive (b) terms 
  a <- beta1 + a_method + a_organism + a_productivity
  b <- beta0 + b_method + b_organism + b_productivity +
    b_streamorder + b_geomorphology + b_banktype
  
  return(list(a=a,b=b))
}


# ---------- main function ------------------------

#' calculate stream signatures for raster of stream cells
#'  
#' @param r raster
#' @param rf raster of flux values
#' @param rc species composition fractions
#'           can be supplied as a raster of organism group fractions
#'           or as a list of spatially invariant values
#' @param radius_m maximum distance for matching land and stream cells 
#' @param options a list of options 
#' @returns raster of stream signatures of same dimensions as r
#' @examples
#' StreamSignature(r, radius_m=1000)
#' StreamSignature(r, rc=list(E=0.25,P=0.25,Tr=0.25,C=0.25)
#' 
StreamSignature <- function(r, rf, rc=NULL, radius_m=1000, options=list()){
  require(terra)
  require(dplyr)
  require(tidyr)
  require(fuzzyjoin)
  
  # option to cause an error and stop processing if any of the 
  # sums of composition fractions are not equal to 1.0 
  check_composition_sums <- FALSE
  
  if("check_composition_sums" %in% names(options)){
    check_composition_sums <- options[["check_composition_sums"]]
  }

  org_list <- list(A="All aquatic insects",
                   E="Mayflies", 
                   P="Stoneflies",
                   T="Caddisflies",
                   C="Chironomids")
  
  # assign 0 values to replace NA (land cells)
  r[is.na(r)] <- 0
  
  # assign a value of 1 to all other cells (stream cells)
  r[r>0] <- 1
  
  if(missing(rf)){
    rf <- r
    values(rf)[,1] <- 1
  }
  # create a data frame from the raster
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
  
  # if organism fractional composition supplied
  if(!is.null(rc)){
    if(typeof(rc)=="S4"){
      # if organism composition is supplied as raster
      # then add composition to data frame
      
      name_match <- names(rc)[names(rc) %in% names(org_list)]
      
      if(length(name_match)<length(names(rc))){
        msg <- paste0("The composition raster contained ",
                      length(names(rc)), " layers: ",
                      paste0(names(rc),collapse=", "),"\n",
                      "Only the following layer(s) were recognized: ",
                      paste0(name_match,collapse=", "),"\n")
                      
        warning(msg, call.=F)
      }
      rc <- rc[[name_match]]
      comp <- terra::values(rc)
      dfRaster <- data.frame(dfRaster,comp)
      
      # get names of layers
      org_layers <- names(rc)
      
    }else if(typeof(rc)=="list"){
      # organism fractions supplied as a spatially invariant
      # e.g. rc = list(E=0.5, P=0.5)
      
      # take only items from the list of organism groups
      name_match <- names(rc)[names(rc) %in% names(org_list)]
      if(length(name_match)<length(names(rc))){
        msg <- paste0("The composition list contained ",
                      length(names(rc)), " values: ",
                      paste0(names(rc),collapse=", "),"\n",
                      "Only the following names were recognized: ",
                      paste0(name_match,collapse=", "),"\n")
        warning(msg, call.=F)
      }      
      rc <- rc[name_match]
      for(i in 1:length(rc)){
        dfRaster[,names(rc)[i]] <- rc[[i]]
      }
      org_layers <- names(rc)
    }else{
      stop ("Organism composition fractions must be supplied as a raster or a list")
    }
    
  }
  
  # get a subset of dataframe for land cells
  dfLand <- dfRaster %>%
    dplyr::filter(val==0) %>%
    dplyr::select(cellLand=cell, lon=x, lat=y)
  
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
  
  # if no organism information is supplied use the "All" 
  if(is.null(rc)){
    org_list <- org_list["A"]
    df <- df %>%
      mutate(OrgGroup="A", fraction=1, OrgGroupName="All aquatic insects")
  }else{
    # pivot longer so that there is one row per organism fraction
    df <- df %>%
      pivot_longer(cols=all_of(org_layers), names_to="OrgGroup", values_to="fraction")
    
    org_list <- list(E="Mayflies", P="Stoneflies", T="Caddisflies", C="Chironomids")
    df <- df %>%
      mutate(OrgGroupName=org_list[OrgGroup])
    
    df$OrgGroupName <- unlist(org_list[df$OrgGroup])
  }

  # check that organism fractions sum to 1.0
  res <- .check_composition_sums(df, stop_on_error=check_composition_sums)
  msg <- res[2]
  if(res[1]==T){
    msg <- paste0(msg, "Stopping processing and returning the dataframe of cell values\n")
    warning(msg, call.=F)
    return(df)
  }
  if(msg!="") warning(msg, call.=F)
  
  # calling ss_parameters for each row of the dataframe could be very slow
  # so we will retrieve them only for unique combinations of method, organism, etc.
  # right now we only have varying organism compositions
  # this can be expanded on for productivity, slope, others??
  ss_params <- lapply(org_list, ss_parameters, method="Dispersal")
  
  # from our list of unique parameter values, apply
  # the correct parameter pair to each row of df, based on the organism
  df$params <- ss_params[df$OrgGroup]
  
  # calculate weighted organism group contributions for each
  # unique combination of land cell, stream cell and organism group. 
  df <- df %>%
    rowwise() %>%
    mutate(s=fraction*ss(d_km*1000, params=params)/sum(fraction,na.rm=T)) %>%
    ungroup()
  
  
  # Calculate the sums of weighted organism contributions in 
  # each land/stream cell combination 
  df <- df %>%
    group_by(cellLand, cellStream, flux) %>%
    summarise(s=sum(s,na.rm=T),.groups="drop")
  
  # Idea for future development?
  # we could something here with summing separate contributions from each organism?
  
  # normalise the signatures so that the sum for each stream cell is 1.0
  df <- df %>%
    group_by(cellStream) %>%
    mutate(s_norm=s/(sum(s,na.rm=T))) %>%
    ungroup()
  
  # for each land cell, sum the response multiplied by flux from stream cells
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


# ---------------------------------------------------------------------
#' auxiliary function to print the individual coefficients used by
#' ss_parameters() function to give the "overall" a and b values 
#' used in ln(ss) = a*distance + b 
#' 
.ss_coefficients <- function(method, organism,
                             productivity, streamorder, 
                             geomorphology, banktype, model,
                             beta0, beta1, a_method, a_organism, 
                             a_productivity, b_method, b_organism,
                             b_productivity, b_streamorder,
                             b_geomorphology,b_banktype){
  s <- paste0("ss parameters (Model", model, ")\n")
  s <- paste0(s,"b0:             ",beta0,"\n")
  s <- paste0(s,"b1:             ",beta1,"\n")
  s <- paste0(s,"+method:        ",b_method," (", method ,")\n")
  s <- paste0(s,"+organism:      ",b_organism," (", organism ,")\n")
  s <- paste0(s,"+productivity:  ",b_productivity," (", productivity ,")\n")
  s <- paste0(s,"+stream order:  ",b_streamorder," (", streamorder ,")\n")
  s <- paste0(s,"+geomorphology: ",b_geomorphology," (", geomorphology ,")\n")
  s <- paste0(s,"+bank type:     ",b_banktype," (", banktype ,")\n")
  s <- paste0(s,"*method:        ",a_method," (", method ,")\n")
  s <- paste0(s,"*organism:      ",a_organism," (", organism ,")\n")
  s <- paste0(s,"*productivity:  ",a_productivity," (", productivity ,")\n")
  return(s)
}


# ---------------------------------------------------------------------
#' auxiliary function check if the sum of organism composition
#' fractions in each grid cell is equal to 1.0
#' Because of rounding errors at the 8th decimal
#' it is never a good idea to test "==" for 
#' floating point numbers so we need a tolerance value
#' 
.check_composition_sums<- function(df, tolerance=1e-7, stop_on_error=F){
  stop_processing <- T
  df <- df %>% 
    distinct(cellStream,OrgGroup,fraction) %>%
    filter(!is.na(cellStream)) %>%
    group_by(cellStream) %>%
    summarise(comp_sum=sum(fraction,na.rm=T),.groups="drop") %>%
    filter(comp_sum<(1-tolerance) | comp_sum>(1+tolerance))
  
  # count the cells where sum != 1.0
  n <- nrow(df)
  
  stop_processing <- ifelse(n==0,F,stop_processing)
  stop_processing <- ifelse(stop_on_error,stop_processing,F)

  msg <- paste0("For ", n,
                " river cells, the sum of organism composition fractions was not equal to 1.0\n")
  msg <- ifelse(n==0,"",msg)
  return(list(stop_processing,msg))
}
