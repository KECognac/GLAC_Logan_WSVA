
getXYWatersheds <- function(sf = NULL, coordinates = NULL, crs = 4269, clip = FALSE, snap = TRUE){
  
  # Clip watershed to exact XY location
  if(clip == TRUE){
    
    if(is.null(sf)){
      
      # Create a data frame with a column named 'geometry'
      df <- tibble::tibble(long = coordinates[1],
                           lat = coordinates[2])
      
      aoi_raw <- sf::st_as_sf(df, coords = c("long", "lat"), crs = crs) 
      
    }
    
    if(is.null(coordinates)){
      
      aoi_raw <- sf
    }
    
    if(st_crs(aoi_raw)$epsg != 4326){
      
      aoi <- aoi_raw %>% st_transform(crs = 4326)
      
    } else {
      
      aoi <- aoi_raw
    }
    
    flowline <- aoi %>%
      nhdplusTools::get_nhdplus(AOI = ., t_srs = 4326)
    
    if(snap == TRUE){
      
      # "Snap" our site to the nearest NHD flowline feature
      nearest_points <- st_nearest_points(aoi, flowline)
      snapped_points_sf <- st_cast(nearest_points, "POINT")[2,]
      
      # Clip/split our catchment to only include the portion of the
      # catchment upstream of our site:
      better_termination <- get_split_catchment(snapped_points_sf, upstream = F)[2,]
      
    } else {
      
      better_termination <- get_split_catchment(st_cast(aoi))[2,]
      
    }
    
    # read in the complete NHD (in tabular form) to make for much more efficient nhd crawling. 
    # This data in tabular form doesn't exist anywhere online that I know of... -_-
    nhd <- readr::read_csv('data/all/nhd_flow_network.csv') 
    
    upstream <- nhdplusTools::get_UT(nhd, flowline$comid) %>% #upstream trace function in nhdplusTools
      tibble::as_tibble() %>%
      dplyr::rename(comid_list = value)  %>%
      dplyr::distinct(comid_list, .keep_all = TRUE)
    
    nhd_catch <-  upstream$comid_list %>%
      map(~nhdplusTools::get_nhdplus(comid = .,
                                     realization='catchment',
                                     t_srs = 4326)) %>%
      dplyr::bind_rows() %>%
      dplyr::distinct(featureid,.keep_all=TRUE) %>%
      dplyr::filter(featureid != flowline$comid) %>%
      dplyr::bind_rows(., better_termination) %>%
      #dplyr::group_by(featureid) %>%
      dplyr::summarize() %>%
      nngeo::st_remove_holes() 
    
    if(st_crs(nhd_catch) != st_crs(aoi_raw)){
      
      nhd_catch <- nhd_catch %>% st_transform(crs = st_crs(aoi_raw)$epsg)
      
    }
    
    return(nhd_catch)
    
    # Old approach, no clipping:
  } else {
    
    sf::sf_use_s2(FALSE)
    
    if(is.null(sf)){
      
      # Create a data frame with a column named 'geometry'
      df <- tibble::tibble(long = coordinates[1],
                           lat = coordinates[2])
      
      aoi <- sf::st_as_sf(df, coords = c("long", "lat"), crs = 4326)
    }
    
    if(is.null(coordinates)){
      
      aoi <- sf %>% sf::st_transform(4326)
      
    }
    
    aoi <- aoi %>%
      nhdplusTools::get_nhdplus(AOI = .)
    
    # Use the NHDPlus digital elevation model to find the nearest downslope  
    # NHD flowline for any point in space (here, our point of interest)
    # Not working anymore -
    # trace <- get_raindrop_trace(aoi, direction = "up") 
    #
    # "Snap" our site to the nearest NHD flowline feature
    # snap_point <- sf::st_sfc(sf::st_point(trace$intersection_point[[1]][1:2]),
    #                          crs=4326)
    #
    # Clip/split our catchment to only include the portion of the
    # catchment upstream of our site:
    # better_termination <- get_split_catchment(snap_point, upstream = F)[2,]
    
    # read in the complete NHD (in tabular form) to make for much more efficient nhd crawling. 
    # This data in tabular form doesn't exist anywhere online that I know of... -_-
    nhd <- readr::read_csv('data/all/nhd_flow_network.csv') 
    
    upstream <- nhdplusTools::get_UT(nhd, aoi$comid) %>% #upstream trace function in nhdplusTools
      tibble::as_tibble() %>%
      dplyr::rename(comid_list = value)  %>%
      dplyr::distinct(comid_list, .keep_all = TRUE)
    
    nhd_catch <-  upstream$comid_list %>%
      map(~nhdplusTools::get_nhdplus(comid = .,
                                     realization='catchment',
                                     t_srs = 4269)) %>%
      dplyr::bind_rows() %>%
      dplyr::distinct(featureid,.keep_all=TRUE) %>%
      dplyr::group_by(featureid) %>%
      dplyr::summarize()
    
    return(nhd_catch)
    
  }}
