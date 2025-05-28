library(sf)
library(dplyr)

# Leer puntos desde el GPKG (EPSG:5367, en metros)
fincas <- st_read("/home/cbrenes/GIS_RS/fincas.gpkg", layer = "datos_fincas__ubicacion_fincas")

# Extraer coordenadas
coords <- st_coordinates(fincas)
fincas <- fincas %>%
  mutate(x = coords[,1], y = coords[,2])

# Función para crear hexágono cerrado
crear_hexagono <- function(x, y, ha) {
  area_m2 <- ha * 10000
  r <- sqrt((2 * area_m2) / (3 * sqrt(3)))
  angulos <- seq(0, 2*pi, length.out = 7)[-7]
  coords <- lapply(angulos, function(a) c(x + r*cos(a), y + r*sin(a)))
  coords <- do.call(rbind, coords)
  coords <- rbind(coords, coords[1,])  # cerrar el polígono
  st_polygon(list(coords))
}

# Crear geometrías tipo POLYGON
hexagonos_geom <- st_sfc(lapply(1:nrow(fincas), function(i) {
  crear_hexagono(fincas$x[i], fincas$y[i], fincas$HA[i])
}), crs = 5367)

# Construir objeto sf con los hexágonos
hexagonos <- st_sf(
  Nombre = fincas$Nombre,
  ID = fincas$ID,
  HA_ref = fincas$HA,
  geometry = hexagonos_geom
)

# Calcular área real y agregarla como verificación (en hectáreas)
hexagonos$HA_calc <- as.numeric(st_area(hexagonos)) / 10000

# Exportar a GeoJSON
st_write(hexagonos, "/home/cbrenes/GIS_RS/hexagonos_fincas.geojson", driver = "GeoJSON", delete_dsn = TRUE)





