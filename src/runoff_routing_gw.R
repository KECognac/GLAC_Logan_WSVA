#' Rainfall runoff routing function with added groundwater components.  
#' 
#' This function routes a timeseries of effective rainfall or lumped runoff 
#' using a recursive filter to partition runoff into slowflow (Xs) and 
#' quickflow (Xq). A fraction of Xs can also be routed as groundwater recharge 
#' using a loss factor applied to either slowflow only or the sum of quick- and
#' slowflow. There is also an option to add constant baseflow additions from a
#' water source not associated with runoff (i.e., fossil GW from springs).
#' 
#' This function is based on the IHACRES style expuh function in Hydromad with 
#' an addition of the GW loss term. 
#' 
#' Andrews F, Guillaume J, Vervoort W, Buzacott A (2021). hydromad: Hydrological 
#' Model Assessment and Development. http://hydromad.catchment.org/, 
#' https://github.com/josephguillaume/hydromad.
#'       
#' @param df: Dataframe containing timeseries data
#' @param runoff_col: Character string specifying column name for runoff
#' @param tau_q: Recession coefficient for quickflow (time unit of runoff)
#' @param tau_s: Recession coefficient for slowflow (time unit of runoff)
#' @param vs: Fraction of effective rainfall that goes to slowflow (-)
#' @param loss: Fraction of slow flow that goes to groundwater recharge (-)
#' @param baseflow: Constant baseflow value added to discharge representing "fossil GW" (default: 0)
#' @param Delay: applied to quickflow (default: 0)
#' @param return_components: Logical indicating return of separate components (default: FALSE)
#' @param slow_only: Logical indicating groundwater loss from slowflow only (default: FALSE)
#' 
#' @return Dataframe containing the total routed flow (Q_tot).
#'         If return_components is TRUE, also includes Q_slow, Q_quick, and Q_gw_recharge.

runoff_routing_gw <- function(df, tau_q, tau_s, vs, delay = 0, loss, 
                              baseflow = 0, 
                              runoff_col = "runoff_in_wbm",
                              return_components = FALSE,
                              slow_only = FALSE) {
  
  
  #df <- jemez_wbm
  #runoff_col <- "runoff_cfs_wbm"
  #tau_q <- 15
  #tau_s <- 40
  #vs <- .6
  #delay <- 0
  #loss <- .5
  #baseflow <- 0
  #slow_only <- FALSE
  #return_components <- FALSE
  
  stopifnot(all(c(tau_s, tau_q) >= 0))
  
  # Extract runoff data
  runoff <- df[runoff_col]
  
  # Calculate Parameters
  vq <- 1 - vs
  alpha_s <- exp(-1 / tau_s)
  alpha_q <- exp(-1 / tau_q)
  beta_s <- vs*(1 - alpha_s)
  beta_q <- vq*(1 - alpha_q)

  # Set delay to whole num if not
  delay <- round(delay, 0)
  
  # Initialize storage vectors
  Xs_0 <- 0 
  Xq_0 <- 0
  Xs <- Xq <- Xgw <- vector(mode = "numeric", length = nrow(df))
  
  # Slow flow 
  Xs[] <- stats::filter(beta_s * runoff, 
                         alpha_s, 
                         method = "recursive", 
                         init = Xs_0) 
  
  
  # Quick flow 
  Xq[] <- stats::filter(beta_q * runoff, 
                         alpha_q, 
                         method = "recursive", 
                         init = Xq_0)
  
  # Partition slowflow to groundwater (depending on options)
  if (slow_only == TRUE) {
    Xg <- loss*Xs
    Xs <- Xs - Xg
    
  } else if (slow_only == FALSE) {
    Xg <- loss * (Xq + Xs)
    Xq <- (1-loss) * Xq
    Xs <- (1-loss) * Xs
  }
  
  # Total Flow
  Q_tot <- Xs + Xq + baseflow
  
  # Create output dataframe
  df_ret <- data.frame(date = df$date, Q_tot)
  
  # Add separate components if requested
  if (return_components == TRUE) {
    df_ret <- df_ret %>% 
      bind_cols(Q_slow = Xs, Q_quick = Xq, Q_gw_recharge = Xg)
  }
  
  return(df_ret)
  
}
  
  
  
  