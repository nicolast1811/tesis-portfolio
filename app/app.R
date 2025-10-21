library(shiny)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(plotly)
# ---- Elementos estéticos ----
# ---- CONFIGURACIONES DE LOS GRÁFICOS, TEMAS Y COLORES 
# ---- COLORES 
# Definir colores Nord Aurora
nord_aurora_colors <- list(
  background = "#FFFFFF",  # Blanco PDF
  grid = "#4C566A",        # Color de la cuadrícula
  text = "#2e3440",        # Texto oscuro
  accent1 = '#507297',     # Cada accent representa un SLEP en los gráficos comparativos
  accent2 = '#d08770',     # Y son utilizados en los gráficos no comparativos para no romper la estética general del doc
  accent3 = "#A3BE8C",     
  accent4 = "#B48EAD",
  accent5 = "#00C094",
  accent6 = '#bf616a',
  accent7 = '#ebcb8b'
)
# Colores de SLEP y dicionario con numero y nombre
# colores_slep con colores accent de nord_aurora_colors
colores_slep <- c(
  "ANDALIÉN SUR" = nord_aurora_colors$accent1,  
  "BARRANCAS" = nord_aurora_colors$accent2,     
  "CHINCHORRO" = nord_aurora_colors$accent3,    
  "COSTA ARAUCANÍA" = nord_aurora_colors$accent4,  
  "GABRIELA MISTRAL" = nord_aurora_colors$accent5, 
  "HUASCO" = nord_aurora_colors$accent6,        
  "PUERTO CORDILLERA" = nord_aurora_colors$accent7 
)

# También se dejó en numero, quizá no se utilice pero es bueno tenerlo
colores_nslep <- c(
  '1' = nord_aurora_colors$accent1,  # Azul oscuro
  "2" = nord_aurora_colors$accent2,  # Naranjo intenso
  "3" = nord_aurora_colors$accent3,  # Verde Aurora
  "4" = nord_aurora_colors$accent4,  # Morado Aurora
  "5" = nord_aurora_colors$accent5,  # Azul oscuro
  "6" = nord_aurora_colors$accent6,  # Naranjo intenso
  "7" = nord_aurora_colors$accent7   # Verde Aurora
)

# Código de BBDD de cada SLEP
codigo_slep <- c('1' = "Andalién Sur", 
                 "2" = "Barrancas", 
                 "3" = "Chinchorro",
                 "4" = "Costa Araucanía",
                 "5" = "Gabriela Mistral",
                 "6" = "Huasco",
                 "7" = "Puerto Cordillera")

# paleta de colores en formato vector, para acceder a ellos de manera más simple
nord_palette <- c(nord_aurora_colors$accent1, 
                  nord_aurora_colors$accent2, 
                  nord_aurora_colors$accent3, 
                  nord_aurora_colors$accent4,
                  nord_aurora_colors$accent5,
                  nord_aurora_colors$accent6,
                  nord_aurora_colors$accent7) 

# ---- TEMA ----
# Tema para nuestros gráficos, con fuente, configuraciones de grid, leyendas, colores que definimos anteriormente, etc.
theme_nord_aurora <- function() {
  theme_minimal(base_family = "sans", base_size = 12) +  # Reduce tamaño base
    theme(
      plot.background = element_rect(fill = nord_aurora_colors$background, color = NA),
      panel.background = element_rect(fill = nord_aurora_colors$background, color = NA),
      panel.grid.major = element_line(color = adjustcolor("gray60", alpha.f = 0.3), size = 0.3),  # Gris claro y más transparente
      panel.grid.minor = element_blank(),  # Ocultar la cuadrícula menor
      
      # Ajustes de tamaño para evitar texto gigante
      axis.text = element_text(color = nord_aurora_colors$text, size = 10),  # Tamaño más compacto
      axis.title = element_text(color = nord_aurora_colors$text, face = "bold", size = 10),  # Más pequeño pero resaltado
      plot.title = element_text(color = nord_aurora_colors$text, face = "bold", size = 12, hjust = 0.5),  # Centrado
      
      # Ajustes en la leyenda
      legend.background = element_rect(fill = nord_aurora_colors$background, color = NA),
      legend.text = element_text(color = nord_aurora_colors$text, size = 10),  # Reduce tamaño de la leyenda
      legend.position = "bottom",
      
      # Ocultar líneas de ejes y ticks
      axis.line = element_blank(),
      axis.ticks = element_blank()
    )
}

# ==============================================================================

df_kmeans <- read.csv("datos/df_kmeans_cluster.csv")
genero_slep <- read.csv("datos/genero_slep.csv")
matricula_final <- read.csv("datos/matricula_final.csv")
matricula_time_slep <- read.csv("datos/matricula_time_slep.csv")
muestra <- read.csv("datos/muestra.csv")
resumen_cluster <- read.csv("datos/resumen_cluster.csv")
simce_long <- read.csv("datos/simce_long.csv")

df_kmeans %>% count(cluster) %>% arrange(cluster)

g_matricula <- matricula_final %>%
  group_by(AGNO_TOTAL, NOMBRE_SLEP_MUESTRA) %>%
  summarise(matricula = sum(MAT_TOTAL, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = as.factor(AGNO_TOTAL), y = matricula, color = NOMBRE_SLEP_MUESTRA, group = NOMBRE_SLEP_MUESTRA)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Año", y = "Matrícula total", color = "SLEP") +
  theme_nord_aurora() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center"
  )

# Shiny Web App

ui <- fluidPage(
  markdown('Esta es de mis primeras ShinyApps. Aquí aprovecharé de mostrarles los principales resultados de mi tesis de pregrado
     para obtener la Licenciatura en Administración Pública de la USACH.Este es un punto fundamental de mi formación académica
     y marca un precedente al analizar datos públicos con herramientas de la Ciencia de Datos'),
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
      plotOutput("")
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
    # Acá se elige que en el tooltip aparezca solamente lo que escribimos
    ggplotly(p, tooltip = c("text")) %>% 
      style(hoverinfo = "text")
  })
  
}

shinyApp(ui, server)




