# Instalar los paquetes necesarios si no los tienes
# install.packages("sf")

library(sf)
library(ggplot2)
library(dplyr)

# 1. Cargar el archivo de geometría de las regiones de Chile
chile_regions <- st_read("Regiones/Regional.shp")
chile_provincias <- st_read("Provincias/Provincias.shp")
chile_comunas <- st_read("Comunas/comunas.shp")

# 2. Cargar el DataFrame con la variable 'region' y otros datos
df <- read.csv("C:/Users/NTORRESH/Desktop/TAAA/ENE_2023.csv", sep=";")

colnames(df)[colnames(df) == "region"] <- "codregion"

# 3. Agrupar y contar encuestas por región en el DataFrame de encuestas (df)
encuestas_resumen <- df %>%
  group_by(codregion) %>%
  summarise(total_encuestas = n())  # Contar la cantidad de encuestas por región

# 4. Unir el resumen de encuestas con el objeto sf chile_regions usando la columna codregion
chile_mapa <- chile_regions %>%
  left_join(encuestas_resumen, by = "codregion")

# 5. Graficar el mapa de calor
g1 <- ggplot(data = chile_mapa) +
  geom_sf(aes(fill = total_encuestas), color = "black") +
  scale_fill_viridis_c(option = "C") +  # Cambia la escala de color según tu preferencia
  labs(title = "Mapa de Calor: Cantidad de Encuestas por Región", fill = "Total Encuestas") +
  theme_minimal()

