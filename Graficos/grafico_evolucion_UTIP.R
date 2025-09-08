#Grafico requerimiento de internacion IRAG e IRAGE

source("importacion_base.R")

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


#Categorias tipo IRAG
categorias <- c("Infección respiratoria aguda grave (IRAG)", 
                "IRAG extendida")

# Grafico
grafico_utip <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_xAxis(categories = categorias) %>%
  hc_yAxis(title = list(text = "Número de casos")) %>%
  hc_plotOptions(column = list(
    dataLabels = list(
      enabled = TRUE,
      formatter = JS(
        "function() { 
           return this.y + ' casos<br>' + Highcharts.numberFormat(this.percentage, 1) + '%';
         }")
    ),
    stacking = "percent"
  )) %>%
  hc_add_series(
    name = "Requirió UTIP",
    data = data_grafico_utip %>% 
      filter(CUIDADO_INTENSIVO == "SI", CLASIFICACION_MANUAL %in% categorias) %>%
      arrange(match(CLASIFICACION_MANUAL, categorias)) %>%
      pull(casos),
    color = "#feb24c"
  ) %>%
  hc_add_series(
    name = "No requirió UTIP",
    data = data_grafico_utip %>% 
      filter(CUIDADO_INTENSIVO == "NO", CLASIFICACION_MANUAL %in% categorias) %>%
      arrange(match(CLASIFICACION_MANUAL, categorias)) %>%
      pull(casos),
    color = "#3182bd"
  ) %>%
  hc_tooltip(pointFormat = "{series.name}: <b>{point.y}</b> casos<br/>Proporción: <b>{point.percentage:.1f}%</b>") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_title(text = "Requerimiento de UTIP en IRAG e IRAGe. Hospital Dr. Humberto J. Notti, Mendoza")

grafico_utip 


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
    color = "#3182bd"
  ) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_tooltip(
    pointFormat = "{series.name}: <b>{point.y}</b> casos<br/>Proporción: <b>{point.percentage:.1f}%</b>"
  ) %>%
  hc_title(text = "Requerimiento de UTIP en IRAG e IRAGE. Hospital Dr. Humberto J. Notti, Mendoza")


grafico_UTIP_2