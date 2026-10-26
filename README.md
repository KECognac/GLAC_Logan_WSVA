# GLAC_Logan_WSVA

Glacier National Park Water Supply Vulnerability Assessment (WSVA) for the Logan Pass water system

## Report status

**Draft, in review.** *Water for People: Water Supply Vulnerability Assessment for the Logan Pass Water System, Glacier National Park* (draft, 2026). A link to the published report will be added here once it is available on the NPS DataStore.

Prepared by Kristen Cognac, Kathryn Willi, Cibi Chinnasamy, Megan Sears (Radical Open Science Syndicate, Colorado State University), Caitlin Mothes (Geospatial Centroid, Colorado State University), Tyler Gilkerson (NPS Water Resources Division), and David Lawrence (NPS Climate Change Response Program).

## About the assessment

The Logan Pass Visitor Center and comfort station are supplied by an infiltration gallery in the bed of Logan Creek, just west of the Continental Divide. The gallery draws on a small (0.49 km²), snowmelt-dominated contributing area and feeds a 10,000-gallon storage tank. This assessment evaluates the vulnerability of that supply to projected mid-century climate change and growing visitor demand, rating exposure and sensitivity across water balance and inflow, drought, extreme events, system sensitivity, demand to supply, and storage to demand.

The analysis delineates the gallery's contributing area from USGS 3D Elevation Program 1 m lidar, runs the NPS Water Balance Model for that area with historical (GridMET) and 40 projected (MACA, CMIP5) climate datasets, and routes modeled runoff to gallery inflow with a calibrated two-store routing function. Projected inflow is compared with visitation-based demand and a daily tank simulation. Results focus on two contrasting climate futures: a "Hot Dry" future (HadGEM2-CC365, RCP 8.5) and a "Warm Wet" future (GFDL-ESM2M, RCP 4.5).

## Repository contents

- `Reports/GLAC_Quick_Analysis.Rmd`: prepares the data inputs, analyses, tables, and figures for the report
- `Reports/Glac_3d_map.rmd`: the 3D site figure of the Logan Pass area (contributing area, infiltration gallery, buildings, streams, Going-to-the-Sun Road, and the Continental Divide)
- `setup.R`: installs/loads required packages and sources every function in `src/`
- `src/`: the functions the report uses, including:
  - `NPS_WBM/`: the NPS Water Balance Model
  - `runoff_routing/`: the runoff routing function
  - `delineate_watershed.R`: whitebox watershed delineation from a lidar DEM
  - `map_3d.R`: `render_map_3d()`, a reusable 3D terrain figure function (see below)
  - `nps_basemap.R`: NPS Park Tiles basemaps for ggplot, mapview/leaflet, and the 3D figures

## Running the report

The report reads and writes data under a `data/` folder at the repository root, which is not tracked in git. On the project team's machines, `data/` is a symbolic link to the shared project data folder. The Rmd expects the following in `data/park/GLAC/`:

- `GLAC_data.RData`: the saved workspace, loaded at the start of the Rmd
- `clim_hist.rds`, `clim_fut.rds`, `aoi_gridmet_grid.rds`: GridMET and MACA climate data for the recharge area
- `logan_pass_supply_demand.csv`: daily supply and demand records kept by park staff since 2016
- `logan_infil.shp`: the infiltration gallery location
- `Visitation/GLAC.csv`: visitation data
- `Lidar/`: lidar DEM windows read from the USGS 3DEP 1 m cloud-optimized GeoTIFFs (created on first run if missing)

Running the Rmd also creates or updates:

- `data/park/GLAC/report_data/`: copies of downloaded inputs (NOAA, SNOTEL, NWIS, NHDPlus, WBD, NPS buildings, OpenStreetMap peaks and roads, the whitebox contributing area, and others). These are downloaded once and reused on later runs; delete a file, or set `refresh = TRUE` in `cache_download()`, to pull a fresh copy.
- `data/park/GLAC/tables/`: CSV files of the report tables and the statistics cited in the text
- `data/park/GLAC/Figs/`: report figures

The final chunk overwrites `data/park/GLAC/GLAC_data.RData` with the current workspace; skip that chunk if you don't intend to replace the saved workspace.

The NPS Park Tiles basemaps need a public Mapbox token. `get_nps_tiles_token()` reads it from the `NPS_TILES_TOKEN` environment variable if set, and otherwise from the NPS map library, so no token is stored in this repository.

## Key analysis settings

These are set once in the Rmd and used throughout:

- **Analysis periods:** 1980–2012 historical baseline and 2035–2064 future period (`hist_yrs`, `fut_yrs`). The water balance model is run from 1979, which serves as a spin-up year.
- **Contributing area:** 0.49 km², delineated with whitebox from 1 m lidar at the infiltration gallery
- **Snowmelt:** Hock melt coefficient of 3 (default 4), from a regional SNOTEL calibration
- **Inflow routing:** runoff scale factor `0.007739`, quickflow and slowflow time constants of 3 and 120 days, and slowflow fraction `vs = 0.7`, manually calibrated against the park's daily supply records (2016 onward). Low/high bounds use the parameter sets `params_low` and `params_high`.
- **Drought:** 6-month SPEI on a monthly time step, normalized to the 1980–2012 baseline
- **Demand:** per-visitor use rates from July–August records, applied to Logan Pass visitation (approximated as the sum of West Entrance, Saint Mary, and Camas visitors, the three entrances to Going-to-the-Sun Road)
- **Storage:** the existing 10,000-gallon tank and a 50,000-gallon alternative, simulated at a daily time step

## 3D site figure

`src/map_3d.R` builds a path-traced 3D terrain figure from an AOI and a list of sf layers, with callouts, a scale bar, a north arrow, an elevation color bar, a boxed legend, and a notes strip. `Reports/Glac_3d_map.rmd` is the Logan Pass example:

```r
layers <- list(
  map3d_layer(contributing_area, "Contributing Area", "fill", color = "hotpink"),
  map3d_layer(continental_divide, "Continental Divide", "line", color = "gray10",
              width = 8, casing = "white", lty = "dashed",
              callout = map3d_callout("Continental\nDivide", 0.10, 0.36))
)
views <- list(basin = map3d_view(from = "SE", phi = 50))

render_map_3d(aoi = contributing_area, layers = layers, views = views,
              dem = "data/park/GLAC/Lidar/lidar_DTM_site_fig_3m.tif", buffer_m = 1000)
```

`preview = TRUE` (the default) shows a quick render in the plot pane and saves nothing. Set `preview = FALSE` and give `out_dir` and `prefix` to write the final figure. rayshader needs XQuartz on macOS (`brew install --cask xquartz`).

## History

This code was moved from [nps_water_vulnerability](https://github.com/Water-Vulnerability-Analytics/nps_water_vulnerability) with its commit history preserved.
