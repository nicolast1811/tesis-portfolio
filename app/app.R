# Shiny Web App

library(shiny)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(plotly)
library(stringr)
library(DT)
library(sf)


# ---- Elementos estéticos ----
nord_aurora_colors <- list(
  background = "#FFFFFF",
  grid = "#4C566A",
  text = "#2e3440",
  accent1 = '#507297',
  accent2 = '#d08770',
  accent3 = "#A3BE8C",
  accent4 = "#B48EAD",
  accent5 = "#00C094",
  accent6 = '#bf616a',
  accent7 = '#ebcb8b'
)
colores_slep <- c(
  "ANDALIÉN SUR" = nord_aurora_colors$accent1,  
  "BARRANCAS" = nord_aurora_colors$accent2,     
  "CHINCHORRO" = nord_aurora_colors$accent3,    
  "COSTA ARAUCANÍA" = nord_aurora_colors$accent4,  
  "GABRIELA MISTRAL" = nord_aurora_colors$accent5, 
  "HUASCO" = nord_aurora_colors$accent6,        
  "PUERTO CORDILLERA" = nord_aurora_colors$accent7 
)
colores_nslep <- c(
  '1' = nord_aurora_colors$accent1,
  "2" = nord_aurora_colors$accent2,
  "3" = nord_aurora_colors$accent3,
  "4" = nord_aurora_colors$accent4,
  "5" = nord_aurora_colors$accent5,
  "6" = nord_aurora_colors$accent6,
  "7" = nord_aurora_colors$accent7
)
codigo_slep <- c('1' = "Andalién Sur", 
                 "2" = "Barrancas", 
                 "3" = "Chinchorro",
                 "4" = "Costa Araucanía",
                 "5" = "Gabriela Mistral",
                 "6" = "Huasco",
                 "7" = "Puerto Cordillera")
nord_palette <- c(nord_aurora_colors$accent1, 
                  nord_aurora_colors$accent2, 
                  nord_aurora_colors$accent3, 
                  nord_aurora_colors$accent4,
                  nord_aurora_colors$accent5,
                  nord_aurora_colors$accent6,
                  nord_aurora_colors$accent7) 

theme_nord_aurora <- function() {
  theme_minimal(base_family = "sans", base_size = 12) +
    theme(
      plot.background = element_rect(fill = nord_aurora_colors$background, color = NA),
      panel.background = element_rect(fill = nord_aurora_colors$background, color = NA),
      panel.grid.major = element_line(color = adjustcolor("gray60", alpha.f = 0.3), size = 0.3),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = nord_aurora_colors$text, size = 10),
      axis.title = element_text(color = nord_aurora_colors$text, face = "bold", size = 10),
      plot.title = element_text(color = nord_aurora_colors$text, face = "bold", size = 12, hjust = 0.5),
      legend.background = element_rect(fill = nord_aurora_colors$background, color = NA),
      legend.text = element_text(color = nord_aurora_colors$text, size = 10),
      legend.position = "bottom",
      axis.line = element_blank(),
      axis.ticks = element_blank()
    )
}

# ==============================================================================

# Carga de Datos
df_kmeans <- read.csv("datos/df_kmeans_cluster.csv")
genero_slep <- read.csv("datos/genero_slep.csv")
matricula_final <- read.csv("datos/matricula_final.csv")
matricula_time_slep <- read.csv("datos/matricula_time_slep.csv")
muestra <- read.csv("datos/muestra.csv")
resumen_cluster <- read.csv("datos/resumen_cluster.csv")
simce_long <- read.csv("datos/simce_long.csv")

# Tabla 1: Definición de muestra a trabajar ----
tabla_1 <- muestra %>%
  mutate(NOMBRE_SLEP = str_to_title(NOMBRE_SLEP)) %>% 
  count(NOMBRE_SLEP, sort = TRUE) %>%
  rename('Nombre SLEP' = NOMBRE_SLEP) %>%
  rename(`Nº Establecimientos` = n) %>%
  mutate(
    Región = case_when(
      `Nombre SLEP` == "Huasco" ~ "Atacama",
      `Nombre SLEP` == "Puerto Cordillera" ~ "Coquimbo",
      `Nombre SLEP` == "Barrancas" ~ "Metropolitana",
      `Nombre SLEP` == "Costa Araucanía" ~ "Araucanía",
      `Nombre SLEP` == "Chinchorro" ~ "Arica y Parinacota",
      `Nombre SLEP` == "Gabriela Mistral" ~ "Metropolitana",
      `Nombre SLEP` == "Andalién Sur" ~ "Bío Bío",
      TRUE ~ " "
    ),
    Comunas = case_when(
      `Nombre SLEP` == "Huasco" ~ "Alto del Carmen, Freirina, Huasco, Vallenar",
      `Nombre SLEP` == "Puerto Cordillera" ~ "Andacollo, Coquimbo",
      `Nombre SLEP` == "Barrancas" ~ "Cerro Navia, Lo Prado, Pudahuel",
      `Nombre SLEP` == "Costa Araucanía" ~ "Carahue, Nueva Imperial, Puerto Saavedra, Teodoro Schmidt, Toltén",
      `Nombre SLEP` == "Chinchorro" ~ "Arica, Camarones, General Lagos, Putre",
      `Nombre SLEP` == "Gabriela Mistral" ~ "La Granja, Macul, San Joaquín",
      `Nombre SLEP` == "Andalién Sur" ~ "Chiguayante, Concepción, Florida, Hualqui",
      TRUE ~ " "
    ),
    `Año Inicio de Funciones` = case_when(
      `Nombre SLEP` == "Huasco" ~ '2018',
      `Nombre SLEP` == "Puerto Cordillera" ~ '2018',
      `Nombre SLEP` == "Barrancas" ~ '2018',
      `Nombre SLEP` == "Costa Araucanía" ~ '2018',
      `Nombre SLEP` == "Chinchorro" ~ '2019',
      `Nombre SLEP` == "Gabriela Mistral" ~ '2019',
      `Nombre SLEP` == "Andalién Sur" ~ '2019',
      TRUE ~ " "
    )
  ) %>%
  relocate(`Año Inicio de Funciones`, .after = `Nombre SLEP`) %>%
  arrange(as.numeric(`Año Inicio de Funciones`))

chile_regiones <- st_read("datos/Regiones/Regional.shp")
chile_provincias <- st_read("datos/Provincias/Provincias.shp")
chile_comunas <- st_read("datos/Comunas/comunas.shp")

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

# Shiny APP

ui <- fluidPage(
  markdown('Esta es de mis primeras ShinyApps. Aquí aprovecharé de mostrarles los principales resultados de mi tesis de pregrado
     para obtener la Licenciatura en Administración Pública de la USACH.<br>Este es un punto fundamental de mi formación académica
     y marca un precedente al analizar datos públicos con herramientas de la Ciencia de Datos.'),
  p('Este gráfico permite visualizar la cantidad de alumnos matriculados en los SLEP,
     desde su constitución hasta 2024'),
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "sleps", 
        label = "Selecciona SLEP:",
        choices = unique(matricula_final$NOMBRE_SLEP_MUESTRA),
        selected = unique(matricula_final$NOMBRE_SLEP_MUESTRA),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE
        )
      )
    ),
    mainPanel(
      plotlyOutput("matriculaPlot"),
      DTOutput("tabla_detalles_slep"),
      plotOutput("mapa")
    )
  )
)

server <- function(input, output, session) {
  output$matriculaPlot <- renderPlotly({
    p <- matricula_final %>%
      filter(NOMBRE_SLEP_MUESTRA %in% input$sleps) %>%
      group_by(AGNO_TOTAL, NOMBRE_SLEP_MUESTRA) %>%
      summarise(matricula = sum(MAT_TOTAL, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(
        x = as.factor(AGNO_TOTAL),
        y = matricula,
        color = NOMBRE_SLEP_MUESTRA,
        group = NOMBRE_SLEP_MUESTRA,
        text = paste("SLEP", NOMBRE_SLEP_MUESTRA,
                     "<br>Año:", AGNO_TOTAL, 
                     "<br>Matrícula:", format(matricula, big.mark = ".", decimal.mark = ","))
      )) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_color_manual(values = colores_slep) +
      labs(x = "Año", y = "Matrícula total", color = "SLEP") +
      theme_nord_aurora() +
      theme(
        legend.position = "bottom",
        legend.justification = "center",
        legend.box.just = "center"
      )
    ggplotly(p, tooltip = c("text")) %>% 
      style(hoverinfo = "text")
  })
  
  output$tabla_detalles_slep <- DT::renderDT({
    tabla_1 %>% 
      filter(`Nombre SLEP` %in% str_to_title(input$sleps))
  }, rownames = FALSE, options = list(pageLength = 10, autoWidth = TRUE))
    output$mapa <- renderPlot({
    slep_comunas <- reg_comuna %>% filter(NOMBRE_SLEP == input$sleps, !is.na(AGNO))
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
    
    output$mapa <- renderPlot({
      slep_comunas <- reg_comuna %>% filter(NOMBRE_SLEP %in% input$sleps, !is.na(AGNO))
      slep_region_codes <- unique(slep_comunas$codregion)
      slep_regiones <- chile_regiones %>% filter(codregion %in% slep_region_codes)
      slep_comunas_outline <- chile_comunas %>% filter(codregion %in% slep_region_codes)
      
      seleccionados <- colores_slep[match(input$sleps, names(colores_slep))]
      
      ggplot() +
        geom_sf(data = slep_regiones, fill = NA, color = "grey70") +
        geom_sf(data = slep_comunas_outline, fill = NA, color = "grey85") +
        geom_sf(data = slep_comunas, aes(fill = NOMBRE_SLEP)) +
        scale_fill_manual(values = c("grey90", seleccionados)) +
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