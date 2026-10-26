# NPS Park Tiles basemap helpers (adapted from the MORA WSVA; shared with the GLAC WSVA).
# Sourced automatically by setup.R (all files in src/).

#' Public Park Tiles access token
#'
#' Uses the `NPS_TILES_TOKEN` environment variable if set; otherwise reads the
#' public token NPS publishes in its npmap.js library (search "atlas-user").
#' Cached for the session after the first lookup.
get_nps_tiles_token <- function(){
  tok <- Sys.getenv("NPS_TILES_TOKEN", unset = getOption("nps_tiles_token", ""))
  if (nzchar(tok)) return(tok)
  js <- paste(readLines("https://www.nps.gov/lib/npmap.js/4.2.1/npmap.min.js", warn = FALSE),
              collapse = "")
  tok <- regmatches(js, regexpr("pk\\.eyJ[A-Za-z0-9._-]+", js))
  if (length(tok) == 0) stop("Couldn't find the Park Tiles token in npmap.min.js; set NPS_TILES_TOKEN.")
  options(nps_tiles_token = tok)
  tok
}


#' Download the NPS Park Tiles basemap for an area
#'
#' Park Tiles is the National Park Service's own basemap
#' (https://www.nps.gov/maps/tools/park-tiles/). NPS serves it from its Atlas
#' tile server; the style IDs and public access token below come from NPS's
#' map library (npmap.js 4.2.1, the code behind the Park Tiles viewer). The
#' public access token is read from npmap.js at run time by
#' `get_nps_tiles_token()` rather than stored here, so it stays current if NPS
#' rotates it (and GitHub secret scanning doesn't flag it).
#'
#' @param aoi sf object to cover, e.g. from `getParkBoundary()`.
#' @param style `"standard"`, `"light"` or `"slate"`.
#' @param buffer_km Margin around `aoi`, in km.
#' @param zoom Tile zoom level, or `NULL` to let maptiles choose.
#'
#' @return A SpatRaster (RGB, Web Mercator) for `geom_nps_basemap()`.
get_nps_basemap <- function(aoi, style = "standard", buffer_km = 2, zoom = NULL){

  style_ids <- c(standard = "ck58pyquo009v01p99xebegr9",
                 light = "ck5cpia2u0auf01p9vbugvcpv",
                 slate = "ck5cpvc2e0avf01p9zaw4co8o")
  nps_token <- get_nps_tiles_token()

  # maptiles detects the image type from ".png" in the URL. The server rejects
  # extra query parameters, so ".png" goes in a URL fragment, which curl never
  # sends.
  provider <- maptiles::create_provider(
    name = paste0("nps_park_tiles_", style),
    url = paste0("https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/",
                 style_ids[[style]], "/tiles/256/{z}/{x}/{y}?access_token=", nps_token,
                 "#.png"),
    citation = "NPS Park Tiles; (c) Mapbox (c) OpenStreetMap contributors"
  )

  # Tiles come back in the CRS of `area`; Web Mercator is their native CRS, so
  # no resampling, and it matches coord_sf(crs = 3857) in the plot.
  area <- sf::st_buffer(sf::st_union(sf::st_transform(aoi, 4326)), buffer_km * 1000) %>%
    sf::st_transform(3857) %>%
    sf::st_as_sf()

  if (is.null(zoom)) {
    maptiles::get_tiles(area, provider = provider, crop = TRUE)
  } else {
    maptiles::get_tiles(area, provider = provider, zoom = zoom, crop = TRUE)
  }
}


#' ggplot layer for a basemap from `get_nps_basemap()`
#'
#' Draws the tiles as a background image, so other layers keep their own fill
#' and color scales. Add `coord_sf(crs = 3857)` to the plot, since the tiles
#' are in Web Mercator.
#'
#' @param tiles SpatRaster from `get_nps_basemap()`.
geom_nps_basemap <- function(tiles){

  rgb <- terra::as.array(tiles[[1:3]]) / 255
  e <- terra::ext(tiles)

  annotation_raster(grDevices::as.raster(rgb),
                    xmin = e$xmin, xmax = e$xmax, ymin = e$ymin, ymax = e$ymax)
}
