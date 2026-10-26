# Reusable 3D terrain figures (rayshader), generalized from the CEBR Blowhard
# Spring figure (Reports/3D_Map.Rmd). Sourced automatically by setup.R.
#
# Typical use:
#
#   layers <- list(
#     map3d_layer(recharge, "Recharge Area", "fill", color = "hotpink"),
#     map3d_layer(boundary, "Park Boundary", "line", color = "#1b7a4a", width = 6,
#                 callout = map3d_callout("Cedar Breaks", 0.25, 0.20)),
#     map3d_layer(springs, "Springs", "point", color = "hotpink", size_m = 75)
#   )
#   views <- list(basin = map3d_view(from = "NW", phi = 50))
#
#   # Quick preview in the plot pane (default; nothing saved)
#   render_map_3d(aoi = park_boundary, layers = layers, views = views)
#
#   # Final figure, saved to <out_dir>/<prefix>_basin.png
#   render_map_3d(aoi = park_boundary, layers = layers, views = views,
#                 preview = FALSE, out_dir = "data/park/CEBR/Figs", prefix = "cebr_3d")
#
# Each figure is the path-traced render with callouts, a scale bar, a north
# arrow, an elevation color bar, a boxed legend, and a notes strip. Positions on
# the image are fractions of its width and height from the top left.
#
# rayshader needs XQuartz on macOS (brew install --cask xquartz), and rendering
# off-screen needs options(rgl.useNULL = TRUE) set before rayshader loads.


# Layer and view constructors -------------------------------------------------

#' One layer to drape on the terrain
#'
#' @param data sf object (any CRS; reprojected to the DEM).
#' @param name Legend label, and the name callouts and `legend_drop` refer to.
#' @param type "fill" (polygon fill with outline), "line", "point" (circle
#'   marker) or "square" (square marker).
#' @param color Fill / line / marker color.
#' @param width Line width in texture pixels ("line", and "fill" outlines).
#' @param size_m Marker radius in metres ("point", "square").
#' @param outline Marker outline color, or halo color under "fill" outlines.
#' @param casing For "line": a color drawn wider underneath (e.g. "white"),
#'   or NULL for none.
#' @param lty For "line": line type ("solid", "dashed", "dotted", "longdash",
#'   or a hex dash pattern like "44"). With a casing, the gaps show the casing.
#' @param fill_alpha For "fill": fill opacity (0-1).
#' @param legend Show in the legend.
#' @param callout NULL, or a callout from `map3d_callout()`.
map3d_layer <- function(data, name, type = c("fill", "line", "point", "square"),
                        color, width = 3, size_m = 100, outline = "white",
                        casing = NULL, lty = "solid", fill_alpha = 0.3, legend = TRUE,
                        callout = NULL) {
  type <- match.arg(type)
  list(data = data, name = name, type = type, color = color, width = width,
       size_m = size_m, outline = outline, casing = casing, lty = lty,
       fill_alpha = fill_alpha, legend = legend, callout = callout)
}

#' A callout box with a leader line to the nearest part of its layer
#'
#' @param label Box text ("\n" for two lines).
#' @param x,y Box center (fractions of the image from the top left).
#' @param aim_x,aim_y Optional point the leader should aim toward when picking
#'   which part of a line or polygon to connect to (defaults to the box).
map3d_callout <- function(label, x, y, aim_x = NA, aim_y = NA) {
  list(label = label, x = x, y = y, aim_x = aim_x, aim_y = aim_y)
}

#' A camera view
#'
#' @param from Compass direction the camera sits on ("N", "NE", ... or a
#'   bearing in degrees). Ignored if `theta` is given.
#' @param theta rayshader rotation, if you'd rather set it directly
#'   (theta = 180 - bearing; 0 views north from the south).
#' @param phi Camera angle above the horizon (degrees). 90 is straight down.
#' @param zoom < 1 moves in.
#' @param fov Field of view (degrees). 0 is orthographic: no perspective, so
#'   the scale bar holds anywhere in the image along its own direction.
#' @param ... Per-view overrides of render_map_3d() overlay settings, e.g.
#'   `north_arrow = c(0.1, 0.7)`, `scalebar = list(shift = c(0, 0.05))`,
#'   `legend_corner`, `elev_bar`, or `callouts = list("Test Well" = c(0.7, 0.1))`
#'   to move callout boxes for this view.
map3d_view <- function(from = "S", theta = NULL, phi = 45, zoom = 0.85, fov = 20, ...) {
  if (is.null(theta)) {
    bearings <- c(N = 0, NNE = 22.5, NE = 45, ENE = 67.5, E = 90, ESE = 112.5,
                  SE = 135, SSE = 157.5, S = 180, SSW = 202.5, SW = 225,
                  WSW = 247.5, W = 270, WNW = 292.5, NW = 315, NNW = 337.5)
    b <- if (is.numeric(from)) from else bearings[[toupper(from)]]
    theta <- 180 - b
  }
  c(list(theta = theta, phi = phi, zoom = zoom, fov = fov), list(...))
}


# Main function ---------------------------------------------------------------

#' Render 3D terrain figures with draped layers
#'
#' @param aoi sf object the model should cover (buffered by `buffer_m`).
#'   Ignored when `dem` is given and `crop_to_aoi = FALSE`.
#' @param layers List of `map3d_layer()`s, drawn in order (later on top).
#' @param views Named list of `map3d_view()`s; one figure per view.
#' @param out_dir,prefix Where to save. NULL (default) saves nothing: the
#'   figure is only shown in the plot pane. With `out_dir`, each view is saved
#'   to `<out_dir>/<prefix>_<view>.png` (`_preview.png` for previews), plus
#'   `_raw` and `_callouts` stages for final renders.
#' @param dem Optional SpatRaster or path. If NULL, downloaded with elevatr at
#'   zoom `dem_z` and cached in `dem_cache` (if given).
#' @param crop_to_aoi Crop the DEM to `aoi` plus `buffer_m`.
#' @param buffer_m Margin around `aoi` (metres).
#' @param dem_z elevatr zoom (14 is ~10 m at mid-latitudes).
#' @param dem_cache Path to cache the downloaded DEM (GeoTIFF), or NULL.
#' @param dem_agg Aggregation factor for rendering (1 = none).
#' @param z_exag Vertical exaggeration.
#' @param drape "hillshade" (hypsometric tint + shading), "imagery" (Esri World
#'   Imagery) or an NPS Park Tiles style ("standard", "light", "slate").
#' @param texture_zoom Tile zoom for "imagery" / NPS drapes.
#' @param hypso_colors Low-to-high elevation palette for "hillshade".
#' @param preview TRUE (default) for a quick 16-sample half-size render, FALSE
#'   for the full-size path-traced figure. Either way it is shown in the plot
#'   pane, and saved only if `out_dir` is given.
#' @param width,height,samples Final image size (px) and path-tracer samples.
#' @param scalebar List: `km` (length), `edge` ("N", "S", "E", "W" edge of the
#'   model it runs along), `start` (fraction along that edge), `inset`
#'   (fraction in from it), `shift` (image-fraction nudge c(x, y)). NULL to omit.
#' @param north_arrow Arrow center c(x, y), or NULL to omit.
#' @param elev_bar List: `x` (center) and `y` (c(top, bottom)); NULL to omit.
#'   Only drawn with the "hillshade" drape.
#' @param legend_corner Bottom-right corner of the legend box c(x, y), or NULL
#'   to omit the legend.
#' @param legend_drop Layer names to leave out of the legend.
#' @param legend_order Layer names in the order to list them (others follow
#'   in draw order). NULL for draw order.
#' @param text_cex Text size for every label (cex at a 2400 px wide image).
#' @param notes Lines for the strip under the image (the vertical
#'   exaggeration note is added first). NULL for an automatic credit line;
#'   character(0) for no strip.
#' @param shadow_color,solid_color Ground shadow and model base colors.
#'
#' @return Invisibly, the paths of the figures (named by view); temp files
#'   when `out_dir` is NULL.
render_map_3d <- function(aoi, layers, views = list(main = map3d_view()),
                          out_dir = NULL, prefix = "map_3d",
                          dem = NULL, buffer_m = 2000, dem_z = 14,
                          dem_cache = NULL, crop_to_aoi = TRUE, dem_agg = 1,
                          z_exag = 2, drape = "hillshade", texture_zoom = 16,
                          hypso_colors = c("#b86b43", "#d9a56c", "#d1c29e",
                                           "#8da173", "#3b5c36", "white"),
                          preview = TRUE, width = 2200, height = 1800,
                          samples = 256,
                          scalebar = list(km = 5, edge = "S", start = 0.1,
                                          inset = 0.05, shift = c(0, 0)),
                          north_arrow = c(0.08, 0.65),
                          elev_bar = list(x = 0.90, y = c(0.38, 0.64)),
                          legend_corner = c(0.98, 0.98), legend_drop = character(0),
                          legend_order = NULL, text_cex = 3, notes = NULL,
                          shadow_color = "gray45", solid_color = "gray25") {
  
  # --- DEM, cropped and aggregated, in a projected metre CRS ---
  dem <- .m3d_dem(aoi, dem, buffer_m, dem_z, dem_cache, crop_to_aoi)
  if (dem_agg > 1) dem <- terra::aggregate(dem, fact = dem_agg, fun = "mean", na.rm = TRUE)
  elmat <- rayshader::raster_to_matrix(dem)
  cell <- terra::res(dem)[1]
  zscale <- cell / z_exag
  layers <- lapply(layers, function(l) {
    l$data <- sf::st_transform(l$data, terra::crs(dem)); l
  })
  
  # --- Texture with the layers draped on ---
  tex <- .m3d_texture(dem, elmat, drape, cell, texture_zoom, hypso_colors)
  ext <- sf::st_bbox(c(xmin = terra::xmin(dem), xmax = terra::xmax(dem),
                       ymin = terra::ymin(dem), ymax = terra::ymax(dem)),
                     crs = sf::st_crs(dem))
  for (l in layers) tex <- .m3d_drape_layer(tex, l, elmat, ext)
  
  if (is.null(notes)) {
    notes <- paste0("Elevation: ",
                    if (is.null(dem_cache) || !grepl("lidar", dem_cache, ignore.case = TRUE))
                      "USGS 3D Elevation Program (via elevatr). " else "",
                    if (drape == "imagery") "Imagery: Esri World Imagery."
                    else if (drape == "hillshade") "Shaded relief from the elevation model."
                    else "Basemap: NPS Park Tiles.")
  }
  notes <- if (length(notes)) c(paste0("Vertical exaggeration ", z_exag, "x."), notes)
  
  save <- !is.null(out_dir)
  if (save) dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_file <- function(vname, suffix) {
    if (save) file.path(out_dir, paste0(prefix, "_", vname, suffix, ".png"))
    else tempfile(fileext = ".png")
  }
  out_files <- c()
  
  for (vname in names(views)) {
    v <- views[[vname]]
    # per-view overrides
    sb <- utils::modifyList(scalebar %||% list(), v$scalebar %||% list())
    if (is.null(scalebar) && is.null(v$scalebar)) sb <- NULL
    na_xy <- v$north_arrow %||% north_arrow
    eb <- v$elev_bar %||% elev_bar
    lc <- v$legend_corner %||% legend_corner
    
    rgl::close3d()
    rayshader::plot_3d(tex, elmat, zscale = zscale,
                       solid = TRUE, soliddepth = "auto", solidcolor = solid_color,
                       solidlinecolor = solid_color,
                       shadow = TRUE, shadowdepth = "auto", shadowcolor = shadow_color,
                       background = "white", windowsize = c(width, height) / 2)
    rayshader::render_camera(theta = v$theta, phi = v$phi, zoom = v$zoom, fov = v$fov)
    
    # Image positions of everything drawn on top (uses the rgl camera)
    targets <- .m3d_callout_targets(layers, dem, zscale)
    boxes <- .m3d_callout_boxes(layers, v$callouts)
    north_px <- .m3d_project(.m3d_north_points(dem), dem, zscale)
    sb_px <- if (!is.null(sb)) {
      pts <- .m3d_scalebar_points(dem, sb)
      dplyr::bind_cols(sf::st_drop_geometry(pts), .m3d_project(pts, dem, zscale, flat = TRUE))
    }
    
    png <- if (preview) tempfile(fileext = ".png") else out_file(vname, "_raw")
    scale <- if (preview) 0.5 else 1
    rayshader::render_highquality(filename = png, samples = if (preview) 16 else samples,
                                  width = width * scale, height = height * scale,
                                  lightdirection = c(225, 45), lightaltitude = c(55, 60),
                                  lightintensity = c(700, 250),
                                  lightcolor = c("white", "white"),
                                  interactive = FALSE, preview = FALSE)
    fin <- out_file(vname, if (preview) "_preview" else "")
    stage <- if (preview) png else out_file(vname, "_callouts")
    
    .m3d_draw(png, stage, function(w, h, s) {
      .m3d_callouts(boxes, targets, w, h, s, text_cex)
      if (!is.null(sb_px)) .m3d_scalebar(sb_px, sb, w, h, s, text_cex)
      if (!is.null(na_xy)) .m3d_north_arrow(north_px, na_xy, w, h, s, text_cex)
      if (!is.null(eb) && drape == "hillshade")
        .m3d_elev_bar(range(elmat, na.rm = TRUE), eb, hypso_colors, w, h, s, text_cex)
      if (!is.null(lc)) .m3d_legend(layers, legend_drop, legend_order, lc, w, h, s, text_cex)
    })
    .m3d_notes(stage, fin, notes, text_cex)
    
    plot(magick::image_read(fin))
    out_files[vname] <- fin
  }
  invisible(out_files)
}


# DEM and texture -------------------------------------------------------------

.m3d_dem <- function(aoi, dem, buffer_m, z, cache, crop) {
  if (is.character(dem)) dem <- terra::rast(dem)
  if (is.null(dem)) {
    if (!is.null(cache) && file.exists(cache)) {
      dem <- terra::rast(cache)
    } else {
      area <- sf::st_buffer(sf::st_union(sf::st_transform(aoi, .m3d_utm(aoi))), buffer_m * 1.2)
      dem <- elevatr::get_elev_raster(sf::st_as_sf(area), z = z) %>% terra::rast()
      dem <- terra::project(dem, paste0("EPSG:", .m3d_utm(aoi)))
      if (!is.null(cache)) {
        dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
        terra::writeRaster(dem, cache, overwrite = TRUE)
      }
    }
  }
  if (terra::is.lonlat(dem)) dem <- terra::project(dem, paste0("EPSG:", .m3d_utm(aoi)))
  if (crop && !is.null(aoi)) {
    bb <- sf::st_transform(aoi, terra::crs(dem)) %>% sf::st_union() %>%
      sf::st_buffer(buffer_m) %>% sf::st_bbox()
    dem <- terra::crop(dem, terra::ext(bb[["xmin"]], bb[["xmax"]], bb[["ymin"]], bb[["ymax"]]))
  }
  if (terra::ncell(dem) == 0) stop("The DEM crop is empty; check the AOI and DEM CRS.")
  dem
}

# UTM zone (WGS84 EPSG code) for the center of an sf object
.m3d_utm <- function(x) {
  ll <- sf::st_coordinates(sf::st_centroid(sf::st_union(sf::st_transform(x, 4326))))
  (if (ll[2] >= 0) 32600 else 32700) + floor((ll[1] + 180) / 6) + 1
}

.m3d_texture <- function(dem, elmat, drape, zscale, zoom, hypso_colors) {
  shadow <- rayshader::ray_shade(elmat, zscale = zscale, sunaltitude = 35,
                                 sunangle = 225, multicore = TRUE)
  ambient <- rayshader::ambient_shade(elmat, zscale = zscale, multicore = TRUE)
  if (drape == "hillshade") {
    hypso <- grDevices::colorRampPalette(hypso_colors)(256)
    return(elmat %>%
             rayshader::height_shade(texture = hypso) %>%
             rayshader::add_shadow(rayshader::lamb_shade(elmat, zscale = zscale, sunangle = 225), 0.3) %>%
             rayshader::add_shadow(shadow, max_darken = 0.5) %>%
             rayshader::add_shadow(ambient, max_darken = 0.2))
  }
  aoi <- sf::st_as_sfc(sf::st_bbox(dem)) %>% sf::st_transform(4326)
  tiles <- if (drape == "imagery") {
    maptiles::get_tiles(aoi, provider = "Esri.WorldImagery", zoom = zoom, crop = TRUE)
  } else {
    get_nps_basemap(aoi, style = drape, buffer_km = 0, zoom = zoom)   # src/nps_basemap.R
  }
  arr <- terra::as.array(terra::project(tiles[[1:3]], dem, method = "bilinear")) / 255
  arr[is.na(arr)] <- 0
  pmin(pmax(arr, 0), 1) %>%
    rayshader::add_shadow(shadow, max_darken = 0.4) %>%
    rayshader::add_shadow(ambient, max_darken = 0.2)
}

.m3d_drape_layer <- function(tex, l, elmat, ext) {
  d <- l$data[!sf::st_is_empty(l$data), ]
  if (nrow(d) == 0) return(tex)
  line <- function(x, color, w, lty = "solid") rayshader::generate_line_overlay(
    x, extent = ext, heightmap = elmat, color = color, linewidth = w, lty = lty)
  if (l$type == "fill") {
    edge <- sf::st_cast(sf::st_boundary(sf::st_geometry(d)), "MULTILINESTRING")
    tex %>%
      rayshader::add_overlay(rayshader::generate_polygon_overlay(
        d, extent = ext, heightmap = elmat, palette = l$color, linecolor = NA),
        alphalayer = l$fill_alpha) %>%
      rayshader::add_overlay(line(edge, l$outline, l$width + 3)) %>%
      rayshader::add_overlay(line(edge, l$color, l$width))
  } else if (l$type == "line") {
    geom <- sf::st_geometry(d)
    if (any(sf::st_geometry_type(geom) %in% c("POLYGON", "MULTIPOLYGON")))
      geom <- sf::st_cast(sf::st_boundary(geom), "MULTILINESTRING")
    if (!is.null(l$casing)) tex <- rayshader::add_overlay(tex, line(geom, l$casing, l$width + 2))
    rayshader::add_overlay(tex, line(geom, l$color, l$width, l$lty %||% "solid"))
  } else {
    pts <- sf::st_centroid(sf::st_geometry(d))
    mark <- sf::st_buffer(pts, l$size_m,
                          endCapStyle = if (l$type == "square") "SQUARE" else "ROUND")
    rayshader::add_overlay(tex, rayshader::generate_polygon_overlay(
      sf::st_sf(geometry = mark), extent = ext, heightmap = elmat,
      palette = l$color, linecolor = l$outline, linewidth = 2))
  }
}


# Projection to the image -----------------------------------------------------

# Image positions (fractions from the top left) of sf points on the terrain,
# from rayshader's scene coordinates and the current rgl camera. `flat = TRUE`
# puts all points at their mean elevation (keeps the scale bar straight).
.m3d_project <- function(pts, dem, zscale, flat = FALSE) {
  xy <- sf::st_coordinates(pts)[, 1:2, drop = FALSE]
  cell <- terra::res(dem)[1]
  elev <- terra::extract(dem, xy)[, 1]
  if (flat) elev <- rep(mean(elev, na.rm = TRUE), length(elev))
  scene <- cbind((xy[, 1] - terra::xmin(dem)) / cell - terra::ncol(dem) / 2,
                 elev / zscale,
                 (terra::ymax(dem) - xy[, 2]) / cell - terra::nrow(dem) / 2)
  m <- rgl::par3d(c("modelMatrix", "projMatrix"))
  h <- cbind(scene, 1) %*% t(m$projMatrix %*% m$modelMatrix)
  data.frame(fx = (h[, 1] / h[, 4] + 1) / 2,
             fy = 1 - (h[, 2] / h[, 4] + 1) / 2)
}

# Candidate leader-line ends for each layer with a callout: the points
# themselves, or points every ~1/200 of the model width along lines and
# polygon outlines
.m3d_callout_targets <- function(layers, dem, zscale) {
  step <- (terra::xmax(dem) - terra::xmin(dem)) / 200
  e <- sf::st_as_sfc(sf::st_bbox(dem))
  purrr::map_dfr(Filter(function(l) !is.null(l$callout), layers), function(l) {
    g <- sf::st_geometry(l$data)
    pts <- if (l$type %in% c("point", "square") ||
               all(sf::st_geometry_type(g) %in% c("POINT", "MULTIPOINT"))) {
      sf::st_centroid(g)
    } else {
      if (any(sf::st_geometry_type(g) %in% c("POLYGON", "MULTIPOLYGON"))) g <- sf::st_boundary(g)
      p <- sf::st_line_sample(sf::st_cast(g, "LINESTRING", warn = FALSE), density = 1 / step)
      sf::st_cast(p[!sf::st_is_empty(p)], "POINT")
    }
    pts <- pts[lengths(sf::st_intersects(pts, e)) > 0]
    if (length(pts) == 0) return(NULL)
    dplyr::bind_cols(name = l$name, .m3d_project(pts, dem, zscale))
  })
}

.m3d_callout_boxes <- function(layers, overrides) {
  purrr::map_dfr(Filter(function(l) !is.null(l$callout), layers), function(l) {
    co <- l$callout
    xy <- overrides[[l$name]] %||% c(co$x, co$y)
    tibble::tibble(name = l$name, label = co$label, x = xy[1], y = xy[2],
                   aim_x = co$aim_x, aim_y = co$aim_y)
  })
}

.m3d_north_points <- function(dem) {
  e <- terra::ext(dem)
  cx <- (e$xmin + e$xmax) / 2; cy <- (e$ymin + e$ymax) / 2
  sf::st_as_sf(data.frame(x = c(cx, cx), y = c(cy, cy + 1000)),
               coords = c("x", "y"), crs = terra::crs(dem))
}

.m3d_scalebar_points <- function(dem, sb) {
  e <- terra::ext(dem)
  w <- e$xmax - e$xmin; h <- e$ymax - e$ymin
  d <- seq(0, sb$km * 1000, length.out = 5)
  start <- sb$start %||% 0.1; inset <- sb$inset %||% 0.05
  xy <- switch(sb$edge %||% "S",
               N = cbind(e$xmin + start * w + d, e$ymax - inset * h),
               S = cbind(e$xmin + start * w + d, e$ymin + inset * h),
               E = cbind(e$xmax - inset * w, e$ymin + start * h + d),
               W = cbind(e$xmin + inset * w, e$ymin + start * h + d))
  sf::st_as_sf(data.frame(km = d / 1000, x = xy[, 1], y = xy[, 2]),
               coords = c("x", "y"), crs = terra::crs(dem))
}


# Drawing on the rendered image -----------------------------------------------

# Opens `png_in` as a base-graphics device and calls fn(w, h, s), where s
# scales sizes to a 2400 px wide image
.m3d_draw <- function(png_in, png_out, fn) {
  img <- magick::image_read(png_in)
  info <- magick::image_info(img)
  img <- magick::image_draw(img)
  fn(info$width, info$height, info$width / 2400)
  dev.off()
  magick::image_write(img, png_out)
}

.m3d_callouts <- function(boxes, targets, w, h, s, text_cex) {
  cex <- text_cex * s
  for (i in seq_len(nrow(boxes))) {
    tg <- dplyr::filter(targets, name == boxes$name[i], !is.na(fx))
    if (nrow(tg) == 0) next
    bx <- boxes$x[i] * w; by <- boxes$y[i] * h
    tx <- tg$fx * w; ty <- tg$fy * h
    ax <- dplyr::coalesce(boxes$aim_x[i], boxes$x[i]) * w
    ay <- dplyr::coalesce(boxes$aim_y[i], boxes$y[i]) * h
    j <- which.min((tx - ax)^2 + (ty - ay)^2)
    # image_draw() flips the y axis, so text heights come back negative
    hw <- abs(strwidth(boxes$label[i], cex = cex, font = 2)) / 2 + 28 * s
    hh <- abs(strheight(boxes$label[i], cex = cex, font = 2)) / 2 + 20 * s
    sx <- min(max(tx[j], bx - hw), bx + hw)
    sy <- min(max(ty[j], by - hh), by + hh)
    segments(sx, sy, tx[j], ty[j], col = "black", lwd = 3.5 * s)
    points(tx[j], ty[j], pch = 21, bg = "black", col = "white", cex = 2.2 * s, lwd = 3 * s)
    rect(bx - hw, by - hh, bx + hw, by + hh, col = "white", border = "black", lwd = 4 * s)
    text(bx, by, boxes$label[i], cex = cex, font = 2, col = "gray15")
  }
}

.m3d_scalebar <- function(pts, sb, w, h, s, text_cex) {
  shift <- sb$shift %||% c(0, 0)
  px <- (pts$fx + shift[1]) * w; py <- (pts$fy + shift[2]) * h
  n_pts <- length(px)
  segments(px[1], py[1], px[n_pts], py[n_pts], col = "gray10", lwd = 26 * s, lend = 1)
  for (i in seq_len(n_pts - 1)) {
    segments(px[i], py[i], px[i + 1], py[i + 1],
             col = if (i %% 2 == 1) "gray10" else "white", lwd = 18 * s, lend = 1)
  }
  # labels on the upper side of the bar, with a white halo
  u <- c(px[n_pts] - px[1], py[n_pts] - py[1]); u <- u / sqrt(sum(u^2))
  nv <- c(-u[2], u[1]); if (nv[2] > 0) nv <- -nv
  idx <- c(1, ceiling(n_pts / 2), n_pts)
  lx <- px[idx] + nv[1] * 60 * s; ly <- py[idx] + nv[2] * 60 * s
  labels <- c(0, sb$km / 2, paste(sb$km, "km"))
  for (a in seq(0, 2 * pi, length.out = 13)) {
    text(lx + cos(a) * 4 * s, ly + sin(a) * 4 * s, labels, cex = text_cex * s,
         font = 2, col = "white")
  }
  text(lx, ly, labels, cex = text_cex * s, font = 2, col = "gray10")
}

.m3d_north_arrow <- function(npx, xy, w, h, s, text_cex) {
  v <- c((npx$fx[2] - npx$fx[1]) * w, (npx$fy[2] - npx$fy[1]) * h)
  v <- v / sqrt(sum(v^2))
  p <- c(-v[2], v[1])
  cx <- xy[1] * w; cy <- xy[2] * h
  L <- 110 * s; W <- 38 * s
  tip <- c(cx, cy) + v * L; tail <- c(cx, cy) - v * L * 0.55
  left <- tail + p * W; right <- tail - p * W
  polygon(c(tip[1], left[1], cx), c(tip[2], left[2], cy), col = "gray10", border = "gray10", lwd = 2 * s)
  polygon(c(tip[1], right[1], cx), c(tip[2], right[2], cy), col = "white", border = "gray10", lwd = 2 * s)
  lab <- tip + v * 42 * s
  text(lab[1], lab[2], "N", cex = 1.1 * text_cex * s, font = 2, col = "gray10")
}

.m3d_elev_bar <- function(range_m, eb, hypso_colors, w, h, s, text_cex) {
  cols <- grDevices::colorRampPalette(hypso_colors)(256)
  x0 <- eb$x * w - 22 * s; x1 <- eb$x * w + 22 * s
  ytop <- eb$y[1] * h; ybot <- eb$y[2] * h
  yy <- seq(ybot, ytop, length.out = length(cols) + 1)
  rng_ft <- range_m * 3.28084
  ticks <- pretty(rng_ft, n = 5); ticks <- ticks[ticks >= rng_ft[1] & ticks <= rng_ft[2]]
  rect(x0, yy[-length(yy)], x1, yy[-1], col = cols, border = NA)
  rect(x0, ytop, x1, ybot, border = "gray20", lwd = 2 * s)
  ty <- ybot + (ticks - rng_ft[1]) / diff(rng_ft) * (ytop - ybot)
  segments(x1, ty, x1 + 10 * s, ty, col = "gray20", lwd = 2 * s)
  text(x1 + 16 * s, ty, format(ticks, big.mark = ","), adj = c(0, 0.5),
       cex = text_cex * s, col = "gray15")
  text(eb$x * w + 50 * s, ytop - 55 * s, "Elevation (ft)", cex = text_cex * s,
       font = 2, col = "gray15")
}

.m3d_legend <- function(layers, drop, order, corner, w, h, s, text_cex) {
  items <- Filter(function(l) l$legend && !l$name %in% drop, layers)
  if (!is.null(order)) {
    nm <- vapply(items, `[[`, "", "name")
    items <- items[order(match(nm, order, nomatch = length(order) + 1))]
  }
  if (length(items) == 0) return(invisible())
  cex <- text_cex * s
  row_h <- 1.9 * abs(strheight("Mg", cex = cex, font = 2))
  sym_w <- 80 * s; gap <- 20 * s; pad <- 30 * s
  text_w <- max(abs(strwidth(vapply(items, `[[`, "", "name"), cex = cex, font = 2)))
  x1 <- corner[1] * w; y1 <- corner[2] * h
  x0 <- x1 - (pad + sym_w + gap + text_w + pad)
  y0 <- y1 - (2 * pad + length(items) * row_h)
  rect(x0, y0, x1, y1, col = "white", border = "gray20", lwd = 3 * s)
  for (k in seq_along(items)) {
    it <- items[[k]]
    y <- y0 + pad + (k - 0.5) * row_h
    xs <- x0 + pad; xm <- xs + sym_w / 2
    switch(it$type,
           fill   = rect(xs, y - 0.3 * row_h, xs + sym_w, y + 0.3 * row_h, col = it$color, border = NA),
           line   = {
        if (!is.null(it$casing)) segments(xs, y, xs + sym_w, y, col = it$casing, lwd = 10 * s, lend = 1)
        segments(xs, y, xs + sym_w, y, col = it$color, lwd = 7 * s, lty = it$lty %||% "solid", lend = 1)
      },
           point  = points(xm, y, pch = 21, bg = it$color, col = it$outline, cex = 3.5 * s, lwd = 2 * s),
           square = points(xm, y, pch = 22, bg = it$color, col = it$outline, cex = 3.5 * s, lwd = 2 * s))
    text(xs + sym_w + gap, y, it$name, adj = c(0, 0.5), cex = cex, font = 2, col = "gray15")
  }
}

# Notes strip appended under the image, wrapped to its width
.m3d_notes <- function(png_in, png_out, notes, text_cex) {
  img <- magick::image_read(png_in)
  if (length(notes) == 0) return(magick::image_write(img, png_out))
  w <- magick::image_info(img)$width; s <- w / 2400
  cex <- text_cex * s
  strip <- magick::image_blank(w, w, "white") %>% magick::image_draw()
  line_h <- 1.5 * abs(strheight("Mg", cex = cex))
  chars <- floor((w - 120 * s) / abs(strwidth("n", cex = cex)))
  lines <- unlist(lapply(notes, strwrap, width = chars))
  text(60 * s, 30 * s + (seq_along(lines) - 0.5) * line_h, lines,
       adj = c(0, 0.5), cex = cex, col = "gray25")
  dev.off()
  strip <- magick::image_crop(strip, paste0(w, "x", round(60 * s + length(lines) * line_h), "+0+0"))
  magick::image_append(c(img, strip), stack = TRUE) %>% magick::image_write(png_out)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
