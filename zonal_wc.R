# Instalar y cargar librerías necesarias
install.packages("terra")
install.packages("sf")
library(terra)
library(sf)

# Cargar el límite de la región y verificar sistema de coordenadas
region_shp <- st_read("E:/UAC/TEMP/RACHEL/REGIONES_CLIMATICAS_IMN_FIX_GEO.shp")
crs_region <- st_crs(region_shp)

# Directorio de precipitación
precip_files <- list.files("E:/UAC/TEMP/RACHEL/PP", pattern = "\\.tif$", full.names = TRUE)
example_raster <- rast(precip_files[1])
crs_raster <- crs(example_raster)

# Reproyectar shapefile si no coincide con el raster
if (crs_region != crs_raster) {
  region_shp <- st_transform(region_shp, crs_raster)
}

# Procesar y cortar los rasters de precipitación, y calcular la suma acumulada
precip_rasters <- lapply(precip_files, function(file) {
  r <- rast(file)
  crop(r, region_shp) %>% mask(region_shp)
})
precip_stack <- rast(precip_rasters)
precip_sum <- sum(precip_stack)
writeRaster(precip_sum, "E:/UAC/TEMP/RACHEL/precip_sum.tif")

# Directorio de temperatura
temp_files <- list.files("E:/UAC/TEMP/RACHEL/TEMP", pattern = "\\.tif$", full.names = TRUE)
temp_rasters <- lapply(temp_files, function(file) {
  r <- rast(file)
  crop(r, region_shp) %>% mask(region_shp)
})
temp_stack <- rast(temp_rasters)
temp_mean <- mean(temp_stack)
writeRaster(temp_mean, "E:/UAC/TEMP/RACHEL/temp_mean.tif")

# Calcular estadísticas zonales para precipitación acumulada
precip_sum_stats_mean <- terra::extract(precip_sum, vect(region_shp), fun = mean, na.rm = TRUE)
precip_sum_stats_min <- terra::extract(precip_sum, vect(region_shp), fun = min, na.rm = TRUE)
precip_sum_stats_max <- terra::extract(precip_sum, vect(region_shp), fun = max, na.rm = TRUE)

# Combinar estadísticas zonales de precipitación acumulada
precip_zonal_stats <- data.frame(
  REGION = region_shp[["region"]],
  precip_sum_mean = precip_sum_stats_mean[,2],
  precip_sum_min = precip_sum_stats_min[,2],
  precip_sum_max = precip_sum_stats_max[,2]
)

# Calcular estadísticas zonales para temperatura promedio
temp_mean_stats <- terra::extract(temp_mean, vect(region_shp), fun = mean, na.rm = TRUE)
temp_min_stats <- terra::extract(temp_mean, vect(region_shp), fun = min, na.rm = TRUE)
temp_max_stats <- terra::extract(temp_mean, vect(region_shp), fun = max, na.rm = TRUE)

# Combinar estadísticas zonales de temperatura
temp_zonal_stats <- data.frame(
  REGION = region_shp[["region"]],
  temp_mean = temp_mean_stats[,2],
  temp_min = temp_min_stats[,2],
  temp_max = temp_max_stats[,2]
)

# Unir y guardar las estadísticas en un archivo CSV
zonal_stats <- merge(precip_zonal_stats, temp_zonal_stats, by = "region")
write.csv(zonal_stats, "E:/UAC/TEMP/RACHEL/zonal_statistics.csv", row.names = FALSE)



