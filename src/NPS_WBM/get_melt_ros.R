#' get_melt_ros
#'
#' Calculates snowmelt at daily time steps incorporating rain-on-snow (ROS) acceleration.
#' Logic based on the USACE / SNOW-17 temperature-index wet melt formulations.
#' 
#' @param tmean A vector of daily mean temperatures (deg C).
#' @param jtemp The Jennings temperature threshold extracted from spatial parameters.
#' @param hock A base dry-melt factor of daily snowmelt (mm/deg C/day).
#' @param snow A time series vector of snowfall values (mm SWE).
#' @param rain A time series vector of rainfall values (mm).
#' @param sp.0 (optional) Initial snowpack value. Default is 0.
#' @export

get_melt_ros = function(tmean, jtemp, hock, snow, rain, sp.0=NULL){
  sp.0 <- ifelse(!is.null(sp.0), sp.0, 0)
  melt <- vector()
  
  # Calculate melt factor for day 1: adjust hock if it rains and it is warm enough to melt
  is_melting_1 <- tmean[1] >= (jtemp[1] - 3)
  hock_eff_1 <- ifelse(is_melting_1 && rain[1] > 0, hock + (0.0125 * rain[1]), hock)
  
  melt[1] <- ifelse(!is_melting_1 || sp.0 == 0, 0,
                    ifelse((tmean[1] - (jtemp[1] - 3)) * hock_eff_1 > sp.0, 
                           sp.0, (tmean[1] - (jtemp[1] - 3)) * hock_eff_1))
  
  snowpack <- vector()
  snowpack[1] <- sp.0 + snow[1] - melt[1]
  
  for(i in 2:length(tmean)){
    is_melting_i <- tmean[i] >= (jtemp[i] - 3)
    # Dynamically scale Hock up when rain falls concurrently with warm melting temps
    hock_eff_i <- ifelse(is_melting_i && rain[i] > 0, hock + (0.0125 * rain[i]), hock)
    
    melt[i] <- ifelse(!is_melting_i || snowpack[i-1] == 0, 0, 
                      ifelse((tmean[i] - (jtemp[i] - 3)) * hock_eff_i > snowpack[i-1], 
                             snowpack[i-1], (tmean[i] - (jtemp[i] - 3)) * hock_eff_i))
    
    snowpack[i] <- snowpack[i-1] + snow[i] - melt[i]
  }
  return(melt)
}
