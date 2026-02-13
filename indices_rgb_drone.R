library(terra)
library(ggplot2)
library(viridis)

ruta_img <- "/home/cbrenes/GIS/florencia/Va-411-11-2-2026-orthophoto.tif"
img <- rast(ruta_img)

# 1) Asegurar que sean 3 bandas (RGB) aunque venga con banda extra (alpha, mask, etc.)
nl <- nlyr(img)
if (nl < 3) stop("El archivo no tiene al menos 3 bandas (RGB). Tiene: ", nl)

# Si tiene más de 3, nos quedamos con las 3 primeras (lo más común en ortofotos)
if (nl > 3) {
  message("El raster tiene ", nl, " bandas. Usaré las 3 primeras como RGB.")
  img <- img[[1:3]]
}

# 2) Renombrar automáticamente a R,G,B (independiente del nombre original)
names(img) <- c("R", "G", "B")

# ---- Tu flujo sigue igual ----
GRVI  <- (img$R - img$G) / (img$R + img$G)
RGBVI <- (img$G - (img$R * img$B)) / ((img$G * img$G) + (img$R + img$B))
GLI   <- ((img$G - img$R) + (img$G - img$B)) / ((2 * img$G) + img$R + img$B)
VARI  <- (img$G - img$R) / (img$G + img$R - img$B)
NGRDI <- (img$G - img$R) / (img$G + img$R)

indices <- list(GRVI = GRVI, RGBVI = RGBVI, GLI = GLI, VARI = VARI, NGRDI = NGRDI)
nombres_indices <- names(indices)

ruta_salida <- "/home/cbrenes/GIS/florencia/"
for (i in seq_along(indices)) {
  writeRaster(indices[[i]], filename = file.path(ruta_salida, paste0(nombres_indices[i], ".tif")),
              overwrite = TRUE)
}

plot_raster <- function(r, title) {
  r_df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colnames(r_df) <- c("x", "y", "value")
  
  ggplot(r_df) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    scale_fill_viridis_c(option = "plasma") +  # continua
    coord_equal() +
    labs(title = title, fill = "Valor") +
    theme_minimal()
}


plotRGB(img, r = 1, g = 2, b = 3, stretch = "lin", main = "Imagen RGB Original")

for (i in seq_along(indices)) {
  print(plot_raster(indices[[i]], paste("Índice:", nombres_indices[i])))
}
  print(plot_raster(indices[[i]], paste("Índice:", nombres_indices[i])))
}
