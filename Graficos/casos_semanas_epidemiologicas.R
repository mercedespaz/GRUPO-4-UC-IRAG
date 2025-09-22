#DATOS DE IRAG E IRAGE POR SEMANA EPIDEMIOLOGICA

source("importacion_base_nominal.R")

# ===============================
# Valor único de ESTABLECIMIENTO_INTERNACION
# ===============================
establecimiento <- unique(data$ESTABLECIMIENTO_INTERNACION)

unique(data$ESTABLECIMIENTO_INTERNACION)


#CASOS DE IRAG E IRAGE POR SE

data_filtrada <- data %>% select(CLASIFICACION_MANUAL,SEPI_MIN_INTERNACION,ANIO_MIN_INTERNACION) %>%
                          filter(CLASIFICACION_MANUAL != "Caso invalidado por epidemiología") %>% 
                          group_by(CLASIFICACION_MANUAL,SEPI_MIN_INTERNACION, ANIO_MIN_INTERNACION) %>%
                          summarise(casos_semana = n(),.groups = "drop") %>% 
                          arrange(ANIO_MIN_INTERNACION) %>% 
                          pivot_wider(names_from = CLASIFICACION_MANUAL,values_from = casos_semana)

#SEPI LABEL PARA GRAFICO ORDENADA COMO FACTORES 
data_filtrada <- data_filtrada %>% arrange(ANIO_MIN_INTERNACION,SEPI_MIN_INTERNACION) %>%
                                   mutate(sepi_label = paste0(ANIO_MIN_INTERNACION, " - SE ", SEPI_MIN_INTERNACION),
                                          sepi_label = factor(sepi_label, levels = unique(paste(ANIO_MIN_INTERNACION, "- SE", SEPI_MIN_INTERNACION))))

casos_IRAGe <- sum(data_filtrada$`IRAG extendida`, na.rm = TRUE)
casos_IRAG <- sum(data_filtrada$`Infección respiratoria aguda grave (IRAG)`, na.rm = TRUE)
casos_totales <- sum(casos_IRAG + casos_IRAGe)

porc_IRAG <- round((casos_IRAG / casos_totales) * 100, 1)
porc_IRAGe <- round((casos_IRAGe / casos_totales) * 100, 1)

#Año de comienzo de la estrategia

anio_minimo <- min(data$ANIO_FECHA_INTER, na.rm = TRUE)

# SE del año de comienzo de la estrategia

semanas_anio_minimo <- data[data$ANIO_FECHA_INTER == anio_minimo, "SEPI_FECHA_INTER"]

semana_min_anio_minimo <- min(semanas_anio_minimo, na.rm = TRUE)

#Año del corte de análisis

anio_maximo <- max(data$ANIO_FECHA_MINIMA, na.rm = TRUE)

#SE del corte de análisis

semanas_anio_maximo <- data[data$ANIO_FECHA_MINIMA == anio_maximo, "SEPI_FECHA_MINIMA"]

semana_max_anio_minimo <- max(semanas_anio_maximo, na.rm = TRUE)

#GRAFICO INTERACTIVO POR SE POR IRAG E IRAGE
casos_se <- highchart()  %>% 
  hc_chart(type = "column")  %>% 
  hc_plotOptions(column = list(stacking = "normal",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(categories = data_filtrada$sepi_label,
           title = list(text = "Semana epidemiológica")) %>%
  hc_yAxis(title = list(text = "Número de casos"))  %>% 
  hc_exporting(enabled = T) %>%
  hc_add_series(name = "IRAG extendida", data = data_filtrada$"IRAG extendida", color = "#feb24c")  %>% 
  hc_add_series(name = "Infección respiratoria aguda grave (IRAG)", data = data_filtrada$"Infección respiratoria aguda grave (IRAG)", color = "#1f77b4") %>% 
  hc_title(text = "")

casos_se

