#Análisis CUT

# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)
library(ggsankey)
library(readxl)
library(RColorBrewer)

# Ruta del archivo de Excel
file_path <- "E:/UAC/TEMP/matrix_example.xlsx" # Ajusta la ruta si es necesario

# Leer los datos desde el archivo Excel
data <- read_excel(file_path)

# Seleccionar y renombrar columnas relevantes
sankey_data <- data %>%
  select(`USO_1990`, `USO_2024`, HA) %>%
  rename(
    Uso1990 = `USO_1990`,
    Uso2024 = `USO_2024`,
    Area = HA
  )

# Transformar los datos a formato largo para ggsankey
sankey_long <- sankey_data %>%
  make_long(Uso1990, Uso2024, value = Area)

# Generar colores aleatorios para cada categoría en la leyenda
set.seed(123) # Para reproducibilidad
num_categories <- length(unique(sankey_long$node))
colors <- sample(colorRampPalette(brewer.pal(9, "Set1"))(num_categories))

# Crear el gráfico Sankey mostrando transiciones y su magnitud
ggplot(sankey_long, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), value = value)) +
  geom_sankey(flow.alpha = 0.5, node.color = "black") +
  geom_sankey_text(aes(label = node), color = "black", size = 3, hjust = 0.5, vjust = 0.5) + # Etiquetas en negro en el centro de cada nodo
  scale_fill_manual(values = colors) + # Colores aleatorios para cada categoría
  theme_sankey(base_size = 12) +
  labs(title = "Diagrama Sankey: Transición de Uso de la Tierra (1990-2024)",
       fill = "Cobertura de Tierra",
       x = "Año",
       y = "Hectáreas") +
  theme_minimal() +
  theme(
    legend.position = "bottom", # Ubicar la leyenda en la parte inferior
    legend.text = element_text(size = 8), # Tamaño de texto de la leyenda
    legend.title = element_text(size = 10, face = "bold")
  )
