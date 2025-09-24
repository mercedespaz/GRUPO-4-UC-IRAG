#Grafico requerimiento de internacion IRAG e IRAGE

#Categorias de las variables de interés
#Clasificación manual: IRAG, IRAG extendida, Caso invalidado por epidemiologia

#unique(data$CLASIFICACION_MANUAL)

#Clasificacion cuidado intensivo
#No, si y ""
#unique(data$CUIDADO_INTENSIVO)

#Agrupo data segun requerimiento de internacion
data_grafico_utip <- data %>% filter(CLASIFICACION_MANUAL != "Caso invalidado por epidemiología") %>% 
                                     mutate (CUIDADO_INTENSIVO = if_else(CUIDADO_INTENSIVO == "", "Sin dato", CUIDADO_INTENSIVO)) %>%
                                     group_by(CLASIFICACION_MANUAL,CUIDADO_INTENSIVO) %>% 
                                     summarise(casos = n(), .groups = "drop")


# Extraemos los valores de casos
valor_IRAG_extendida <- data_grafico_utip %>% filter(CUIDADO_INTENSIVO == "SI") %>%
  filter(CLASIFICACION_MANUAL == "IRAG extendida") %>%
  pull(casos)

valor_IRAG <- data_grafico_utip %>%
  filter(CLASIFICACION_MANUAL == "Infección respiratoria aguda grave (IRAG)") %>% filter(CUIDADO_INTENSIVO == "SI") %>%
  pull(casos)

# Calculamos los porcentajes

#CASOS DE IRAG E IRAGE POR SE

data_filtrada <- data %>% select(CLASIFICACION_MANUAL,SEPI_MIN_INTERNACION,ANIO_MIN_INTERNACION) %>%
  filter(CLASIFICACION_MANUAL != "Caso invalidado por epidemiología") %>% 
  group_by(CLASIFICACION_MANUAL,SEPI_MIN_INTERNACION, ANIO_MIN_INTERNACION) %>%
  summarise(casos_semana = n(),.groups = "drop") %>% 
  arrange(ANIO_MIN_INTERNACION) %>% 
  pivot_wider(names_from = CLASIFICACION_MANUAL,values_from = casos_semana)

casos_IRAGe <- sum(data_filtrada$`IRAG extendida`, na.rm = TRUE)
casos_IRAG <- sum(data_filtrada$`Infección respiratoria aguda grave (IRAG)`, na.rm = TRUE)

porc_IRAG_UTI <- round((valor_IRAG / casos_IRAG) * 100, 1)
porc_IRAGe_UTI <- round((valor_IRAG_extendida / casos_IRAGe) * 100, 1)


#Categorias tipo IRAG
categorias <- c("Infección respiratoria aguda grave (IRAG)", 
                "IRAG extendida")

#Grafico sin etiquetas estaticas

grafico_UTIP_2 <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_xAxis(categories = categorias) %>%
  hc_yAxis(title = list(text = "Cantidad de casos")) %>%
  hc_plotOptions(
    column = list(
      stacking = "percent",
      dataLabels = list(enabled = FALSE)
    )
  ) %>%
  hc_add_series(
    name = "Requirió UTIP",
    data = data_grafico_utip %>% 
      filter(CUIDADO_INTENSIVO == "SI",
             CLASIFICACION_MANUAL %in% categorias) %>% 
      arrange(match(CLASIFICACION_MANUAL, categorias)) %>% 
      pull(casos),
    color = "#feb24c"
  ) %>%
  hc_add_series(
    name = "No requirió UTIP",
    data = data_grafico_utip %>% 
      filter(CUIDADO_INTENSIVO == "NO",
             CLASIFICACION_MANUAL %in% categorias) %>% 
      arrange(match(CLASIFICACION_MANUAL, categorias)) %>% 
      pull(casos),
    color = "#1f77b4"
  ) %>% 
  hc_add_series(
    name = "Sin dato",
    data = data_grafico_utip %>% 
        filter(CUIDADO_INTENSIVO == "Sin dato",
               CLASIFICACION_MANUAL %in% categorias) %>% 
        arrange(match(CLASIFICACION_MANUAL, categorias)) %>% 
        pull(casos),
      color = "#A9A9A9"
    ) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_tooltip(
    pointFormat = "{series.name}: <b>{point.y}</b> casos<br/>Proporción: <b>{point.percentage:.1f}%</b>"
  ) %>%
  hc_title(text = "")


grafico_UTIP_2

