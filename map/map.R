library(shiny)
library(sf)
library(ggplot2)
library(dplyr)


chile_regiones <- st_read("~/github/tesis-portfolio/app/map/Regiones/Regional.shp")
chile_provincias <- st_read("~/github/tesis-portfolio/app/map/Provincias/Provincias.shp")
chile_comunas <- st_read("~/github/tesis-portfolio/app/map/Comunas/comunas.shp")

muestra <- read.csv('~/github/tesis-portfolio/app/datos/muestra.csv')

colnames(muestra)[colnames(muestra) == "COD_REG_RBD"] <- "codregion"
colnames(muestra)[colnames(muestra) == "COD_COM_RBD"] <- "cod_comuna"

reg_comuna <- chile_comunas %>%
  left_join(muestra, by = c("cod_comuna", "codregion"))



map_slep <- ggplot() +
  geom_sf(data = chile_regiones, fill = NA, color = "grey70") +
  geom_sf(data = chile_comunas, fill = NA, color = "grey85") +
  geom_sf(data = reg_comuna %>% filter(!is.na(AGNO)),
          aes(fill = Region)) +
  scale_fill_discrete(name = "Región") +
  guides(fill = guide_legend(override.aes = list(color = NA))) +
  theme_void() +
  theme(
    legend.position = c(0.95, 0.1),
    legend.key.size = unit(0.5, "lines"),
    legend.text = element_text(size = 7)
  )


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("slep", "Selecciona SLEP", choices = unique(muestra$NOMBRE_SLEP), selected = unique(muestra$NOMBRE_SLEP)[1])
    ),
    mainPanel(
      plotOutput("mapa")
    )
  )
)

server <- function(input, output, session) {
  output$mapa <- renderPlot({
    slep_comunas <- reg_comuna %>% filter(NOMBRE_SLEP == input$slep, !is.na(AGNO))
    slep_region_codes <- unique(slep_comunas$codregion)
    slep_regiones <- chile_regiones %>% filter(codregion %in% slep_region_codes)
    slep_comunas_outline <- chile_comunas %>% filter(codregion %in% slep_region_codes)
    ggplot() +
      geom_sf(data = slep_regiones, fill = NA, color = "grey70") +
      geom_sf(data = slep_comunas_outline, fill = NA, color = "grey85") +
      geom_sf(data = slep_comunas, aes(fill = Region)) +
      scale_fill_discrete(name = "Región") +
      guides(fill = guide_legend(override.aes = list(color = NA))) +
      theme_void() +
      theme(
        legend.position = c(0.95, 0.1),
        legend.key.size = unit(0.5, "lines"),
        legend.text = element_text(size = 7)
      ) +
      coord_sf(xlim = st_bbox(slep_regiones)[c("xmin", "xmax")],
               ylim = st_bbox(slep_regiones)[c("ymin", "ymax")])
  })
}

shinyApp(ui, server)
