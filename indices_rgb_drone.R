library(terra)
library(ggplot2)
library(viridis)  # Paleta de colores segura para daltónicos

# Definir la ruta de la imagen
ruta_img <- "/home/cbrenes/GIS_RS/ExG/platanar_clip.tif"

# Cargar la imagen como SpatRaster
img <- rast(ruta_img)

# Verificar las dimensiones y nombres actuales
print(img)
plot(img)

# Renombrar las bandas: según tus datos, el orden es Rojo (R), Verde (G), Azul (B)
names(img) <- c("R", "G", "B")

# Calcular índices de vegetación basados en RGB
GRVI <- (img$R - img$G) / (img$R + img$G)
RGBVI <- (img$G - (img$R * img$B)) / ((img$G * img$G) + (img$R + img$B))
GLI <- ((img$G - img$R) + (img$G - img$B)) / ((2 * img$G) + img$R + img$B)
VARI <- (img$G - img$R) / (img$G + img$R - img$B)
NGRDI <- (img$G - img$R) / (img$G + img$R)

plot(GLI)

# Lista de índices
indices <- list(GRVI = GRVI, RGBVI = RGBVI, GLI = GLI, VARI = VARI, NGRDI = NGRDI)
nombres_indices <- names(indices)

# Definir ruta de salida
ruta_salida <- "/home/cbrenes/GIS_RS/ExG/"

# Guardar los índices como archivos TIFF
for (i in seq_along(indices)) {
  writeRaster(indices[[i]], filename = paste0(ruta_salida, nombres_indices[i], ".tif"), overwrite = TRUE)
}

# Función para visualizar mapas con ggplot y paleta segura para daltónicos
plot_raster <- function(r, title) {
  r_df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colnames(r_df) <- c("x", "y", "value")
  
  ggplot(r_df) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    scale_fill_viridis(option = "plasma") +  # Paleta segura para daltónicos
    coord_equal() +
    labs(title = title, fill = "Valor") +
    theme_minimal()
}

# Visualizar la imagen original en RGB
plotRGB(img, r = 1, g = 2, b = 3, stretch = "lin", main = "Imagen RGB Original")

# Visualizar los índices
for (i in seq_along(indices)) {
  print(plot_raster(indices[[i]], paste("Índice:", nombres_indices[i])))
}
