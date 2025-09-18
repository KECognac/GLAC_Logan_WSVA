#' Import park boundary shapefile
#'
#' This function pulls NPS park boundary spatial data from their ArcGIS REST services directory.
#' The URL is: https://services1.arcgis.com/fBc8EJBxQRMcHlei/ArcGIS/rest/services/NPS_Land_Resources_Division_Boundary_and_Tract_Data_Service/FeatureServer
#'
#' @param save Whether to save (TRUE) the resulting shapefile or not (FALSE)
#' @param path If `save = TRUE`, the file path to save the shapefile
#'
#' @return A spatial sf object for all park boundaries
getParkSystem <- function(save = FALSE, path = NULL) {
  base_url <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/NPS_Land_Resources_Division_Boundary_and_Tract_Data_Service/FeatureServer/2"

  # Add query parameters that ArcGIS REST services require
  full_url <- paste0(base_url, "/query?where=1=1&outFields=*&f=geojson")

  parks <- sf::st_read(full_url) %>%
    sf::st_make_valid()

  # Optionally save the data
  if (save && !is.null(path)) {
    sf::st_write(parks, path, delete_dsn = TRUE)
  }

  return(parks)
}
