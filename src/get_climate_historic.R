#' Download and tidy historic GridMET climate for points or polygons
#'
#' @description
#' Convenience wrapper around `climateR::getGridMET()` that fetches daily
#' GridMET variables for an `sf` object (points or polygons), harmonizes CRS,
#' and returns a tidy table with per-cell centroid coordinates (`x`, `y`), `date`,
#' precipitation (mm), and temperatures (°C). For polygons, you can choose an
#' **area-weighted** extraction that includes fractional cell overlap weights.
#'
#' @details
#' - **Geometry support:**  
#'   Accepts `POLYGON`, `MULTIPOLYGON`, or `POINT`. Polygons are processed cell-by-cell
#'   (optionally with overlap weights); points return the cell time series at the
#'   point location.
#'
#' - **Exact vs non-exact polygon mode:**  
#'   *`exact = TRUE`* → buffers the AOI by 4 km before download to ensure edge
#'   cells are included, then uses `terra::extract(..., weights = TRUE, exact = TRUE)`
#'   so the output includes an `area_weight` column (fractional overlap per cell).  
#'   *`exact = FALSE`* → crops/masks without weights; `area_weight` is set to `NA`.
#'
#' - **Returned grid (optional):**  
#'   If `return_grid = TRUE` (polygon input), the function creates
#'   **`aoi_gridmet_grid`** in the global environment: an `sf` polygon layer of the
#'   GridMET cells intersecting the AOI in the raster CRS.
#'
#' - **Variables & units:**  
#'   Designed for the trio `vars = c("tmmx","tmmn","pr")` (daily max/min air
#'   temperature in K; precipitation in mm). Temperatures are converted to °C via
#'   `tmax_C = tmmx - 273.15`, `tmin_C = tmmn - 273.15`, and `tmean_C` is their
#'   average. Precipitation is returned as `ppt_mm = pr`.
#'
#' - **Tagging / join key:**  
#'   The column given in `col_name` is temporarily renamed to `join_index`
#'   internally and restored in the output to its original name for joining.
#'
#' @param sf An `sf` object of AOIs: `POLYGON`/`MULTIPOLYGON` or `POINT`.
#' @param col_name Bare column name in `sf` used as an identifier; carried through
#'   to the output (restored from a temporary internal name).
#' @param start,end Character dates `"YYYY-MM-DD"` defining the inclusive time
#'   range to download. Defaults to `"1979-01-01"` and `"2023-12-31"`.
#' @param vars Character vector of GridMET variable names to request. Default
#'   `c("tmmx","tmmn","pr")`. Downstream calculations assume these are present.
#' @param exact Logical. If `TRUE`, perform weighted extraction over polygons
#'   (uses a 4 km buffer to ensure full cell capture and `terra` overlap weights).
#'   If `FALSE`, crop/mask without weights and only fully intersecting cells are 
#'   returned. Ignored for point inputs. Default `FALSE`.
#' @param return_grid Logical. If `TRUE` and `sf` is polygonal, writes an `sf`
#'   layer named `aoi_gridmet_grid` to the **global environment** containing the
#'   intersecting GridMET cell polygons in the raster CRS. Default `FALSE`.
get_climate_historic <- function(sf,
                                 col_name,
                                 start = "1979-01-01",
                                 end = "2023-12-31",
                                 vars = c("tmmx", "tmmn", "pr"),
                                 exact = FALSE,
                                 return_grid = FALSE) {
  
  sf <- sf %>%
    dplyr::rename("join_index" = {{col_name}})
  
  all_climate_data <- vector("list", length = nrow(sf))
  
  if(any(unique(sf::st_geometry_type(sf)) %in% c("POLYGON", "MULTIPOLYGON"))){
    
    for (i in 1:nrow(sf)) {
      
      aoi <- sf[i,]
      
      print(paste0('Downloading GridMET for ', aoi$join_index, "."))
      
      if(exact == TRUE){
        # download climate data using a 4km buffer to ensure full cell capture at aoi edges
        # getGridMET does NOT always grab all the grid cells that touch an AOI
        clim <- climateR::getGridMET(AOI = sf::st_buffer(aoi, 4000),
                                     varname = vars,
                                     startDate = start,
                                     endDate = end)
      } else if(exact == FALSE) {
        
        # download climate data NOT using a 4km buffer to ensure full cell capture at aoi edges
        # getGridMET does NOT always grab all the grid cells that touch an AOI
        clim <- climateR::getGridMET(AOI = aoi,
                                     varname = vars,
                                     startDate = start,
                                     endDate = end)
      }
      
      if(!is.data.frame(clim) | inherits(clim[[1]], "SpatRaster")){
        
        clim_crs <- terra::crs(clim[[1]])
        
        # transform AOI to match raster CRS if needed
        if(sf::st_crs(aoi) != clim_crs) {
          aoi_trans <- sf::st_transform(aoi, crs = clim_crs)
        } else {
          aoi_trans <- aoi
        }
        
        if(return_grid == TRUE){
          
          assign("aoi_gridmet_grid", 
                 terra::as.polygons(clim[[1]], values = FALSE, dissolve = FALSE, na.rm = TRUE) %>%
                   sf::st_as_sf() %>%
                   .[aoi_trans,], 
                 envir = .GlobalEnv)
          
        }
        
        if(exact == TRUE){
          
          all_climate_data[[i]] <- clim %>%
            purrr::map(~terra::extract(., terra::vect(aoi_trans), xy= TRUE, touches = TRUE, weights = TRUE)) %>%
            purrr::map_dfr(~ terra::as.data.frame(., xy = TRUE)) %>%
            data.table::data.table() %>%
            tidyr::pivot_longer(-c(x, y, ID, weight),
                                names_to = "var_temp",
                                values_to = "val") %>%
            tidyr::separate_wider_delim(var_temp, "_", names = c("var", "date")) %>%
            tidyr::drop_na(val) %>%
            dplyr::group_by(x, y, date) %>%
            tidyr::pivot_wider(names_from = "var", values_from = "val") %>%
            dplyr::mutate(date = as.Date(date),
                          ppt_mm = pr,
                          tmax_C = tmmx - 273.15,
                          tmin_C = tmmn - 273.15,
                          tmean_C = (tmax_C + tmin_C)/2,
                          join_index = aoi$join_index) %>%
            dplyr::rename(area_weight = weight) %>%
            dplyr::select(-c(ID, tmmx, tmmn, pr))
          
        } else if(exact == FALSE){
          
          all_climate_data[[i]] <- clim %>%
            purrr::map(~terra::crop(., terra::vect(aoi_trans), mask = TRUE)) %>%          
            purrr::map_dfr(~ terra::as.data.frame(., xy = TRUE)) %>%
            data.table::data.table() %>%
            tidyr::pivot_longer(-c(x, y),
                                names_to = "var_temp",
                                values_to = "val") %>%
            tidyr::separate_wider_delim(var_temp, "_", names = c("var", "date")) %>%
            tidyr::drop_na(val) %>%
            dplyr::group_by(x, y, date) %>%
            tidyr::pivot_wider(names_from = "var", values_from = "val") %>%
            dplyr::mutate(date = as.Date(date),
                          ppt_mm = pr,
                          tmax_C = tmmx - 273.15,
                          tmin_C = tmmn - 273.15,
                          tmean_C = (tmax_C + tmin_C)/2,
                          join_index = aoi$join_index,
                          area_weight = NA) %>%
            dplyr::select(-c(tmmx, tmmn, pr))
          
        }
        
      } else {
        
        # when the returned object is already a clean data.frame (e.g., small aoi of just one grid)
        
        all_climate_data[[i]] <- clim %>%
          data.table::data.table() %>%
          # since polygon grabbed a single grid, gridMET does not provide the coordinates
          # of the gridMET cell, so we fill in x and y with the coordinates
          # of the sf object:
          dplyr::mutate(x = sf::st_coordinates(sf::st_centroid(aoi))[[1]],
                        y =  sf::st_coordinates(sf::st_centroid(aoi))[[2]]) %>%
          # Then do all other cleaning steps done for polygon sf objects:
          dplyr::mutate(date = as.Date(date),
                        ppt_mm = pr,
                        tmax_C = tmmx - 273.15,
                        tmin_C = tmmn - 273.15,
                        tmean_C = (tmax_C + tmin_C)/2,
                        join_index = aoi$join_index,
                        # for a single grid cell fract of total aoi area divided 
                        # by 4km grid cell. Not really important tho since no 
                        # weighting really needed for these.
                        area_weight = ifelse(exact, NA, as.numeric(sf::st_area(aoi_trans)/4000000))) %>%
          dplyr::select(-c(tmmx, tmmn, pr))
        
      }
    }
    
    # combine all aoi outputs into one dataframe
    all_climate_data <- all_climate_data %>%
      dplyr::bind_rows()
    
    # rename the join_index column
    colnames(all_climate_data)[colnames(all_climate_data) == "join_index"] <- {{col_name}}
    
    return(all_climate_data)
    
  } else if(unique(sf::st_geometry_type(sf)) == "POINT"){
    
    for (i in 1:nrow(sf)) {
      
      aoi <- sf[i,]
      
      print(paste0('Downloading GridMET for ', aoi$join_index, "."))
      
      clim <- climateR::getGridMET(AOI = aoi,
                                   varname = vars,
                                   startDate = start,
                                   endDate = end)
      
      all_climate_data[[i]] <- clim %>%
        data.table::data.table() %>%
        # since point pulls from gridMET do not provide the coordinates
        # of the gridMET cell, we fill in x and y with the coordinates
        # of the sf object:
        dplyr::mutate(x = sf::st_coordinates(sf::st_centroid(aoi))[[1]],
                      y =  sf::st_coordinates(sf::st_centroid(aoi))[[2]]) %>%
        # Then do all other cleaning steps done for polygon sf objects:
        dplyr::mutate(date = as.Date(date),
                      ppt_mm = pr,
                      tmax_C = tmmx - 273.15,
                      tmin_C = tmmn - 273.15,
                      tmean_C = (tmax_C + tmin_C)/2,
                      join_index = aoi$join_index,
                      # For a point, this concept doesn't really apply
                      area_weight = NA) %>%
        dplyr::select(-c(tmmx, tmmn, pr))
      
    }
    
    all_climate_data <- all_climate_data %>%
      dplyr::bind_rows()
    
    # Rename the join_index column
    colnames(all_climate_data)[colnames(all_climate_data) == "join_index"] <- {{col_name}}
    
    return(all_climate_data)
    
  } else {stop("Your sf feature is neither a polygon nor point feature, or it needs to be made valid.")}
  
}