#' Pull weather station data for parks
#'
#' This function imports SNOTEL and SCAN site data from the NRCS Air and Water
#' Database (AWDB) REST web service. (soilDB::fetchSCAN() no longer parses the
#' current service response, so data are requested from AWDB directly.)
#'
#' @param aoi An sf polygon/multipolygon object, preferably a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param dist The distance (in the same units as the aoi object) to buffer around park boundary
#' @param years The years to pull data for. Default set to "all" which pulls all available years for each site, but can be a string of years
#' @param timeseries either 'Daily' or 'Hourly
#' @param save Whether to save the resulting file (TRUE/FALSE)
#' @param path If `save == TRUE`, the file path to save to
#' @return An sf object of weather stations within given spatial boundary and attributes.
#'   Columns follow the former soilDB::fetchSCAN() layout: Site, Name, Date, value,
#'   sensor.id (e.g., "WTEQ.I", "PREC.I", "SNWD.I"), water_year.
getSNOTEL <- function(aoi,
                      dist = 0,
                      years = "all",
                      timeseries = "Daily",
                      save = FALSE,
                      path = NULL) {

    # 1. Buffer and Transform AOI
    aoi_buf <- st_buffer(aoi, dist = dist)
    aoi_wgs84 <- st_transform(aoi_buf, 4326)

    # 2. Filter sites quickly using WGS84
    data('SCAN_SNOTEL_metadata', package = 'soilDB')
    sites <- st_as_sf(SCAN_SNOTEL_metadata, coords = c("Longitude", "Latitude"), crs = 4326)

    site_aoi <- sites[aoi_wgs84, ] # Spatial Subset

    if (nrow(site_aoi) == 0) return(message("No sites found."))

    # 3. Determine Year Range
    # Use years[1] to check the first element of the input
    if (years[1] == "all") {
      start_yr <- min(lubridate::year(site_aoi$beginDate), na.rm = TRUE)
      end_yr   <- lubridate::year(Sys.Date())
    } else {
      # If it's a vector like 2010:2012, we just take the min and max
      start_yr <- min(as.numeric(years))
      end_yr   <- max(as.numeric(years))
    }

    # 4. Request data from the AWDB web service
    message(paste("Fetching data for", nrow(site_aoi), "sites from NRCS AWDB..."))

    # Large requests time out, so request one site at a time in 5-year chunks
    fetch_chunk <- function(triplet, yr0, yr1) {
      resp <- httr::RETRY(
        "GET", "https://wcc.sc.egov.usda.gov/awdbRestApi/services/v1/data",
        query = list(
          stationTriplets = triplet,
          elements        = "WTEQ,PREC,SNWD",
          duration        = toupper(timeseries),
          beginDate       = paste0(yr0, "-01-01"),
          endDate         = paste0(yr1, "-12-31")
        ),
        times = 4, pause_base = 5, quiet = TRUE
      )
      httr::stop_for_status(resp)
      jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                         simplifyVector = FALSE)
    }
    chunk_starts <- seq(start_yr, end_yr, by = 5)
    parsed <- purrr::map(site_aoi$stationTriplet, function(trip) {
      purrr::map(chunk_starts, ~ fetch_chunk(trip, .x, min(.x + 4, end_yr)))
    }) %>% purrr::flatten() %>% purrr::flatten()

    # 5. Flatten to one row per site, element, and date
    final_data <- purrr::map_dfr(parsed, function(st) {
      purrr::map_dfr(st$data, function(el) {
        if (length(el$values) == 0) return(NULL)
        tibble::tibble(
          Site      = sub(":.*", "", st$stationTriplet),
          Date      = as.Date(purrr::map_chr(el$values, "date")),
          value     = purrr::map_dbl(el$values, ~ if (is.null(.x$value)) NA_real_ else .x$value),
          sensor.id = paste0(el$stationElement$elementCode, ".I")
        )
      })
    }) %>%
      dplyr::mutate(water_year = calc_water_year(Date)) %>%
      dplyr::left_join(site_aoi %>%
                         dplyr::mutate(Site = as.character(Site),
                                       Longitude = sf::st_coordinates(.)[, 1],
                                       Latitude  = sf::st_coordinates(.)[, 2]) %>%
                         dplyr::select(Site, Name, State, Network, Elevation_ft, Latitude, Longitude),
                       by = "Site") %>%
      sf::st_as_sf()

    if (save) saveRDS(final_data, path)

    return(final_data)
}
