#GRAFICO DETERMINACIONES POSITIVAS POR SEMANA EPIDEMIOLOGICA

#Importo base
source("importacion_base.R")

#Selecciono columnas de interes para el analisis
data_virus <- data %>% select(SEPI_MIN_INTERNACION,ANIO_MIN_INTERNACION, INFLUENZA_FINAL,VSR_FINAL,COVID_19_FINAL)

#Vectores para la identificacion de resultados 
resultado_detectable_influenza <-c("Influenza A (sin subtipificar)","Influenza A H3N2","Influenza positivo-Sin Tipo",
                                   "Influenza B (sin linaje)","Influenza A H1N1")

resultado_detectable_VSR <- c("VSR A","VSR B","VSR")


resultado_detectable_covid <- ("Positivo")

#Creo variable con resultado final de determinacion 
data_virus<- data_virus %>% mutate (detectable_influenza = if_else(INFLUENZA_FINAL %in% resultado_detectable_influenza,1,0),
                                    detectable_VSR = if_else (VSR_FINAL %in% resultado_detectable_VSR,1,0),
                                    detectable_covid = if_else(COVID_19_FINAL == resultado_detectable_covid,1,0))


#Agrupo por año y semana y sumo la cantidad de determianciones segun tipo de virus
data_virus <- data_virus%>%
  group_by(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION) %>%
  summarise(casos_influenza = sum(detectable_influenza== 1, na.rm = TRUE),
            casos_vsr       = sum(detectable_VSR == 1, na.rm = TRUE),
            casos_covid     = sum(detectable_covid == 1, na.rm = TRUE),
            .groups = "drop") %>% 
  arrange(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION)

#Creo sepi_label para usar en el grafico
data_virus <-data_virus %>% mutate(
  sepi_label = paste(ANIO_MIN_INTERNACION, "-", SEPI_MIN_INTERNACION),
  sepi_label = factor(sepi_label, levels = unique(paste(ANIO_MIN_INTERNACION, "-", SEPI_MIN_INTERNACION))))


#Codigo gráfico interactivo

grafico_interactivo_virus <- highchart()  %>% 
  hc_chart(type = "column")  %>% 
  hc_xAxis(categories = data_virus$sepi_label,
           title = list(text = "Semanas epidemiológicas"))  %>% 
  hc_yAxis(title = list(text = "Cantidad de determinaciones"))  %>% 
  hc_plotOptions(
    column = list(stacking = "normal",
                  pointPadding = 0.1,   
                  groupPadding = 0.05,  
                  borderWidth = 0))  %>% 
  hc_add_series(name = "Influenza", data = data_virus$"casos_influenza", color = "#fdae6b")  %>% 
  hc_add_series(name = "VSR", data = data_virus$"casos_vsr", color = "#3182bd")  %>% 
  hc_add_series(name = "Sars-Cov-2", data = data_virus$"casos_covid", color = "#0207a4")  %>%
  hc_exporting(enabled = T) %>%
  hc_title(text = 
             "Casos confirmados de IRAG e IRAGe por resultado de laboratorio y semana epidemiológica.\n Desde SE23 2024-SE34 2025.\n Hospital Dr.Humberto J. Notti, Mendoza.")
             

#Visualizo grafico
grafico_interactivo_virus
