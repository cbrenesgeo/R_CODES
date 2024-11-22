# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)
library(ggsankey)
library(readxl)
library(RColorBrewer)

# Ruta del archivo (ajusta la ruta según sea necesario)
file_path <- "E:/UAC/2024/refores/Mapas_Taller_1/bases_datos/amenazas_tipo_2_tipologia.xlsx"

# Leer los datos
data <- read_excel(file_path)

# Filtrar datos por el país y tipo de amenaza "Inundaciones"
data_filtered <- data %>%
  filter(F_Pais == "Belice", TIPO_AMENAZAS == "Inundaciones")

# Seleccionar y renombrar columnas (evitar caracteres especiales en los nombres)
sankey_data <- data_filtered %>%
  select(TIPO, TIPO_FRECUENCIA, TIPO_MAGNITUD, TIPO_FACT_CLIM, TIPO_FACT_NCLIM, TIPO_DANOS) %>%
  rename(
    Tipo = TIPO,
    Frecuencia = TIPO_FRECUENCIA,
    Magnitud = TIPO_MAGNITUD,
    FactoresClimaticos = TIPO_FACT_CLIM,
    FactoresNoClimaticos = TIPO_FACT_NCLIM,
    Danos = TIPO_DANOS
  )

# Contar conexiones para ordenar "Tipo" en función de la frecuencia
tipo_order <- sankey_data %>%
  count(Tipo, sort = TRUE) %>%
  pull(Tipo)

# Transformar los datos a formato largo para ggsankey y ordenar por frecuencia
sankey_long <- sankey_data %>%
  mutate(Tipo = factor(Tipo, levels = tipo_order)) %>%
  make_long(Tipo, Frecuencia, Magnitud, FactoresClimaticos, FactoresNoClimaticos, Danos)

# Generar colores aleatorios para cada categoría en la leyenda
set.seed(123) # Para reproducibilidad
num_categories <- length(unique(sankey_long$node))
colors <- sample(colorRampPalette(brewer.pal(9, "Set1"))(num_categories))

# Crear el gráfico Sankey con ajustes solicitados
ggplot(sankey_long, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node))) +
  geom_sankey(flow.alpha = 0.5, node.color = "black") +
  geom_sankey_text(aes(label = node), color = "black", size = 3, hjust = 0.5, vjust = 0.5) + # Etiquetas en negro en el centro de cada nodo
  scale_fill_manual(values = colors) + # Colores aleatorios para cada categoría
  theme_sankey(base_size = 12) +
  labs(title = "Relación tipología, factores y daños de inundaciones en sitio Belice") +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(), # Eliminar título del eje vertical
    axis.text.y = element_blank(), # Eliminar etiquetas del eje vertical
    axis.ticks.y = element_blank(), # Eliminar marcas del eje vertical
    axis.title.x = element_blank(), # Eliminar título del eje horizontal
    axis.text.x = element_text(size = 10, face = "bold"), # Ajustar texto del eje horizontal
    legend.position = "none" # Eliminar la leyenda
  ) +
  scale_x_discrete(
    labels = c("Tipo", "Frecuencia", "Magnitud", "Fac. climáticos", "Fac. no climáticos", "Daños") # Cambiar etiquetas del eje horizontal
  )
