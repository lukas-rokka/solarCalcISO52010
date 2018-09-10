#' @title Tidy version of the ISO 52010-1:2017 solar irradiance calculation
#' @description adds columns n_day and n_hour to .df
#' @param .df Data frame that holds the solar irradiance data
#' @param lat Latitude, in decimal degrees
#' @param lng Longitude, in decimal degrees
#' @param tz Time zone of the irradiance data, in hours. E.g. +1 for central European
#' (UTC+1) time zones. Use 0, if data is recorded in UTC time.
#' @param t_shift Decimal hours to shift timestamps with. E.g. timestamp 12:00 usually refer to the
#' solar irradaince during time interval 11:00 - 12:00, then use t_shift = 0.5. 
#' If the timestamp 12:00 refer to interval 11:45 - 12:15, use t_shift = 0.0. 
#' If the timestamp 12:00 refer to interval 11:45 - 12:00, use t_shift = 0.125.
#' If t_shift is NULL, it is estimated as (n_hour[2] - n_hour[1])/2. 
#' @param surfaceAzimuths Vector of surface azimuths, in degrees. Need to be same lenght as surfaceTilts
#' @param surfaceTilts Vector of surface tilts, in degrees. 90 for vertical, 0 for horizontal. 
#' @param albedo Constant solar reflectivity of the ground, 0.2 can be used if actual conditions are unknown. 
#' If NULL, the variable albedo from .df is used instead. 
#' @param interp_perez Set to 1 to interpolate from the Parez table (clearness index	and	brightness coefficients). 
#' Set to 0 to use binned values, this follow the procedure of the ISO 52010-1:2017 standard. Defaults to 0.
#' @param col_timestamp The name of the column in .df holding timestamp data 
#' @param col_G_dir The name of the column in .df holding direct normal (beam) irradance data 
#' @param col_G_dif The name of the column in .df holding diffuse horizontal irradance data
#' @param col_albedo The name of the column in .df holding albedo data.  
#' @return The inputed .df with additional columns of calculated solar irradiance
#' @export

tidyISO52010 <- function(
  .df, lat, lng, tz, t_shift = NULL, 
  surfaceAzimuths = c(180, 90, 0.0, -90), surfaceTilts = rep(90, length(surfaceAzimuths)),
  albedo = NULL, interp_perez = 0,
  col_timestamp = "timestamp", col_G_dir = "G_dir", col_G_dif = "G_dif", col_albedo = "albedo"
  ) {
  if (!inherits(.df, "data.frame")) stop("First argument, \".df\", need to inherit from data.frame class")
  if (!exists(col_timestamp, .df)) stop("Column  \"", col_timestamp, "\" doesn't exist in .df" )
  if (!exists(col_G_dir, .df)) stop("Column  \"", col_G_dir, "\" doesn't exist in .df" )
  if (!exists(col_G_dif, .df)) stop("Column  \"", col_G_dif, "\" doesn't exist in .df" )
  if (!inherits(.df[, col_timestamp][[1]], "POSIXt")) stop("Column \"", col_timestamp, "\" need to inherit from POSIXt class")
  if (!exists(col_timestamp, .df)) stop("Column  \"", col_timestamp, "\" doesn't exist in .df" )
  if (is.null(albedo) && !exists(col_albedo, .df)) stop("Either .df need an \"albedo\" column or the albedo need to be specified with the albedo argument")
  if (length(surfaceAzimuths)!=length(surfaceTilts)) stop("Arguments surfaceAzimuths and surfaceAzimuths need to be of same length ")
  
  .df <- as.data.frame(.df)
  .df <- add_dayOfYear_hourOfDay(.df)
  if (!is.null(albedo)) .df[ , col_albedo] <- albedo
  if (is.null(t_shift)) t_shift <- (.df$n_hour[2] - .df$n_hour[1]) / 2
  
  # converti degrees to radians
  surfaceAzimuths <- surfaceAzimuths*pi/180
  surfaceTilts<- surfaceTilts*pi/180
  
  res <- rcpp_ISO52010(
    lat=lat*pi/180, lng=lng*pi/180, tz=tz, t_shift=t_shift,
    surfaceAzimuths = surfaceAzimuths, surfaceTilts = surfaceTilts, # north, west, south, east
    n_day=.df$n_day, n_hour=.df$n_hour, 
    G_dir=as.vector(.df[, col_G_dir]), G_dif=as.vector(.df[, col_G_dif]), 
    albedo=as.vector(.df[, col_albedo]), interp_perez=interp_perez
  )
  
  .df$alpha_sol <- res[,1]
  N <- length(surfaceAzimuths)
  for (n in 1:N) {
    .df[ , paste0("I_tot_dir_s", n)] = res[,n+1]
    .df[ , paste0("I_tot_dif_s", n)] = res[,N+n+1]
  }
  .df
}

#' @title add helper variables
#' @description adds columns n_day and n_hour to .df
#' @param .df input data frame
#' @param col_timestamp name of the column holding timestamp information, defaults to "timestamp"
#' @return .df with columns n_day and n_hour added. Timestamp "2014-01-02 00:00" results in n_day = 1 & n_hour = 24.
#' n_day is the day of the year, an integer between 1-366
#' n_hour is the hour of the day, an decimal between 0.01667 - 24
add_dayOfYear_hourOfDay <- function(.df, col_timestamp="timestamp") {
  if (!exists(col_timestamp, .df)) stop("Column  \"", col_timestamp, "\" doesn't exist in .df")
  if (!inherits(.df[, col_timestamp][[1]], "POSIXt")) stop("Column \"", col_timestamp, "\" need to inherit from POSIXt class")
  timestamp <- (.df[ , col_timestamp]) - 1 
  .df$n_day = as.integer(format(timestamp, "%j"))
  .df$n_hour = as.numeric(format(timestamp, "%H")) + as.numeric(format(timestamp, "%M"))/60 + 1/60
  .df
}
