library(lubridate)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

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

# ---- Carga de Datos ----

docentes <- read.csv("datos/docentes_muestra.csv")

doc_state <- docentes %>%
  filter(!is.na(DOC_FEC_NAC) & DOC_FEC_NAC != 19000101) %>%
  mutate(
    DOC_FEC_NAC = as.Date(sprintf("%06d01", DOC_FEC_NAC), format = "%Y%m%d"),
    edad = interval(DOC_FEC_NAC, Sys.Date()) / years(1) %>% floor()
  ) %>%
  select(RBD, DOC_FEC_NAC, edad, SLEP, ANO_SERVICIO_SISTEMA, ANO_TITULACION_1, starts_with(c("HORAS_", "MEN")))

# Gráficos

# Histograma de edades
ggplot(doc_state, aes(x = edad)) +
  geom_histogram(binwidth = 5, color = nord_aurora_colors$grid, fill = nord_aurora_colors$accent1) +
  labs(
    title = "Distribución de las edades",
    x = "Edad",
    y = "Frecuencia"
  ) +
  theme_nord_aurora()

# Histograma con densidad
ggplot(doc_state, aes(x = edad)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, color = nord_aurora_colors$grid, fill = nord_aurora_colors$accent1) +
  geom_density(color = nord_aurora_colors$accent2, size = 1) +
  labs(title = "Distribución de las edades", x = "Edad", y = "Densidad") +
  theme_nord_aurora()

ggplot(doc_state, aes(x = ANO_SERVICIO_SISTEMA)) +
  geom_bar(fill = nord_aurora_colors$accent2, color = nord_aurora_colors$grid) +
  labs(
    title = "Años de Servicio en el Sistema",
    x = "Año de servicio",
    y = "Frecuencia"
  ) +
  theme_nord_aurora() +
  coord_flip()


# Pie chart de distribución de edades (en intervalos)

breaks  <- seq(20, 80, by = 10)                                   # 20,30,40,50,60,70,80
labels  <- paste0(seq(20,70,10), " a ", seq(29,79,10))            # "20 a 29", "30 a 39", ...

df_pie <- doc_state |>
  dplyr::mutate(
    grupo_edad = cut(
      edad,
      breaks = breaks,
      right = FALSE,               # [20,30)
      include.lowest = TRUE,
      labels = labels              # usa tus etiquetas personalizadas
    ),
    grupo_edad = forcats::fct_relevel(grupo_edad, labels)  # fija el orden de levels
  ) |>
  dplyr::filter(!is.na(grupo_edad)) |>
  dplyr::count(grupo_edad, name = "n")                      # no ordenar filas

fig <- plotly::plot_ly(
  data   = df_pie,
  labels = ~grupo_edad,
  values = ~n,
  type   = "pie",
  sort   = FALSE,                                           # desactiva reordenamiento
  marker = list(colors = unlist(nord_aurora_colors[4:9]))
) |>
  plotly::layout(
    title = "Distribución de las edades (intervalos de 10 años)",
    showlegend = TRUE,
    legend = list(traceorder = "normal"),                   # respeta el orden de levels
    paper_bgcolor = nord_aurora_colors$background
  )
fig

# Analizar las menciones de los docentes (qué están enseñando?)

menciones <- doc_state %>%
  select(starts_with("MEN_")) %>%
  summarise(across(everything(), ~sum(. == 1, na.rm = TRUE), .names = "n_{.col}")) %>%
  pivot_longer(everything(), names_to = "mencion", values_to = "n_docentes") %>%
  arrange(desc(n_docentes))




