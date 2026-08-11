#' Retrieve and Process NPS Water Supply Database (WSD)
#'
#' This function loads the NPS Water Supply Database, performs spatial cleaning,
#' and allows for filtering by geography and park unit.
#'
#' @param conus Logical. If TRUE (default), filters out non-contiguous states and territories.
#' @param fill_empty_coords Logical. If TRUE (default), fills missing source coordinates 
#' with the respective park unit's centroid.
#' @param park Character vector. Optional list of four-letter park unit codes (e.g., c("ROMO", "GRCA")).
#' @param sf Logical. If TRUE (default), returns the data as an sf spatial object (CRS: 4326).
#' @param rapid Logical. If TRUE, applies "Rapid" assessment presets: forces CONUS, 
#'   fills missing coords, and filters to NPS-operated systems only (non-muncipal/hauled/tribal/concession)
#'
#' @return A data frame or sf object containing processed water supply system data.
#' @export
get_wsd <- function(conus = TRUE,
                    fill_empty_coords = FALSE,
                    park = NULL,
                    sf = TRUE,
                    rapid = FALSE) {
  
  
  if (rapid == TRUE) {
    # override these args for rapid
    fill_empty_coords <- TRUE
    sf <- TRUE
    conus <- TRUE
  }
  
  # --- Error Checking: File Existence ---
  table_path <- "data/Water_Supply_Systems/NPS_Water_Systems_Database_Joined.xlsx"
  
  if (!file.exists(table_path)) {
    stop(paste("WSD file not found at:", table_path, 
               "\nPlease check your working directory or data path."))
  }
  
  # --- Load and Clean Water Supply Database ---
  WSD_raw <- readxl::read_excel(table_path, 
                                sheet = 1, 
                                na = c("NA", "U"), 
                                skip = 1) %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      source_longitude = as.numeric(source_longitude),
      source_latitude = as.numeric(source_latitude)
    )
  
  if (rapid == TRUE) {
    WSD_raw <- WSD_raw %>%
      # Filter to CONUS & NPS-Operated Systems only
      dplyr::filter(!str_detect(source_type, 
                                regex("municipal|hauled|tribal|purchased",
                                            ignore_case = TRUE)))
  }
  
  # --- Handle Coordinate Filling ---
  # Moved this ABOVE drop_na so we actually have something to fill
  if (fill_empty_coords) {
    
    # Error checking for dependency
    if (!exists("getParkSystem")) {
      warning("Function 'getParkSystem()' not found. Skipping coordinate fill.")
    } else {
      parks <- getParkSystem()
      
      WSD_raw <- WSD_raw %>% 
        dplyr::mutate(
          coords_flag = ifelse(is.na(source_longitude) | is.na(source_latitude),
                               "Centroid", "Source Coordinates"))

      # filter missing coords to only join those (fixes GRSA issue)
      WSD_missing <- WSD_raw %>% 
        filter(coords_flag == "Centroid")
      
      # get associated park centroid and join
      park_centroids <- parks %>% 
        filter(UNIT_CODE %in% WSD_missing$park_unit) %>% 
        group_by(UNIT_CODE) %>%
        summarize() %>%
        sf::st_centroid() %>% 
        sf::st_transform(4326) %>% 
        dplyr::mutate(
          y_cent = sf::st_coordinates(.)[,2], 
          x_cent = sf::st_coordinates(.)[,1]
        ) %>%
        sf::st_drop_geometry() %>%
        dplyr::select(UNIT_CODE, x_cent, y_cent)
      
      WSD_missing <- WSD_missing %>%
        dplyr::left_join(park_centroids, by = c("park_unit" = "UNIT_CODE")) %>% 
        dplyr::mutate(
          source_longitude = ifelse(is.na(source_longitude), x_cent, source_longitude),
          source_latitude = ifelse(is.na(source_latitude), y_cent, source_latitude)
        ) %>%
        dplyr::select(-x_cent, -y_cent)
      
      # join to rows that had coords
      WSD_raw <- WSD_raw %>%
        filter(coords_flag == "Source Coordinates") %>% 
        bind_rows(WSD_missing)
       
    }
  }
  
  # --- Filters ---
  WSD <- WSD_raw
  
  if (!is.null(park)) {
    WSD <- WSD %>% dplyr::filter(park_unit %in% park)
  }
  
  if (conus) {
    # Territories/Oconus filters
    WSD <- WSD %>% 
      dplyr::filter(!state %in% c("AK", "AS", "HI", "MP", "PR", "VI", "GU")) 
  }
  
  # --- Return as Spatial or Flat ---
  if (sf) {
    # Final check: remove records that still have no coords after filling
    WSD <- WSD %>%
      tidyr::drop_na(source_longitude, source_latitude) %>%
      sf::st_as_sf(coords = c("source_longitude", "source_latitude"),
                   crs = 4326,
                   remove = FALSE)
  }
  
  return(WSD)
}

