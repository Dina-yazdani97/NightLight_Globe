# libraries we need
libs <- c(
  "tidyverse", "sf", "giscoR",
  "mapview", "terra", "terrainr",
  "magick"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# define projections
longlat_crs <- "+proj=longlat +datum=WGS84 +no_defs"
ortho_crs <- '+proj=ortho +lat_0=32.4279 +lon_0=53.688 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs'

# --- get world vector ---
get_flat_world_sf <- function() {
  world <- giscoR::gisco_get_countries(
    year = "2024",
    epsg = "4326",
    resolution = "10"
  ) %>%
    sf::st_transform(longlat_crs)
  
  world_vect <- terra::vect(world)
  return(world_vect)
}

world_vect <- get_flat_world_sf()

# --- get NASA data ---
get_nasa_data <- function() {
  ras <- terra::rast("/vsicurl/https://eoimages.gsfc.nasa.gov/images/imagerecords/144000/144898/BlackMarble_2016_01deg_geo.tif")
  rascrop <- terra::crop(x = ras, y = world_vect, snap = "in")
  ras_latlong <- terra::project(rascrop, longlat_crs)
  ras_ortho <- terra::project(ras_latlong, ortho_crs)
  return(ras_ortho)
}

ras_ortho <- get_nasa_data()
r <- ifel(is.na(ras_ortho), 0, ras_ortho)
plot(r)

# --- create graticule transformed to ortho projection ---
graticule_ortho <- sf::st_graticule(
  lat = seq(-80, 80, by = 15),
  lon = seq(-180, 180, by = 15)
) %>%
  sf::st_transform(ortho_crs)

grat_col <- alpha("white", 0.15)

# --- make nightlights globe ---
make_nighlights_globe <- function() {
  ggplot() +
    terrainr::geom_spatial_rgb(
      data = r,
      mapping = aes(x = x, y = y, r = red, g = green, b = blue)
    ) +
    # graticules
    geom_sf(
      data = graticule_ortho,
      color = grat_col,
      linewidth = 0.25
    ) +
    theme_void() +
    theme(
      plot.margin = unit(c(-1, -1, -1, -1), "lines"),
      plot.background = element_rect(fill = "black", color = NA),
      panel.background = element_rect(fill = "black", color = NA)
    )
}

globe <- make_nighlights_globe()

# save
ggsave(
  filename = "nightlight_globe.png",
  width = 7, height = 7.5, dpi = 600, device = "png", globe
)

# --- annotate map with magick ---
map <- magick::image_read("nightlight_globe.png")

clr <- "#FFFFBC"

# Title
map_title <- magick::image_annotate(
  map, "Earth at night",
  font = "Georgia",
  color = alpha(clr, .65), size = 150,
  gravity = "north",
  location = "+0+50"
)

# Caption
map_final <- magick::image_annotate(
  map_title,
  glue::glue("©2025 Dina Yazdani  |  Data: NASA Earth Observatory"),
  font = "Georgia",
  location = "+0+50",
  color = alpha(clr, .45), size = 80,
  gravity = "south"
)

magick::image_write(map_final, "nightlight_globe_annotated.png")
