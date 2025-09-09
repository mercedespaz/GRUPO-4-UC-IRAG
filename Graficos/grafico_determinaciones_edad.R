#GRAFICO DE RESCATE VIRAL POR GRUPO ETARIO

#Importo base
source("importacion_base.R")
                                                                                                        
#Selecciono variables de interes para el analisis

data_virusedad <- data %>% select(EDAD_UC_IRAG,INFLUENZA_FINAL,VSR_FINAL,COVID_19_FINAL)

#Reclasifico grupos de edad 
data_virusedad <- data_virusedad %>% mutate(GRUPO_ETARIO= case_when(EDAD_UC_IRAG == "0 a 2 Meses" | EDAD_UC_IRAG == "3 a 5 Meses" ~ "Menor a 6 Meses",
                                                EDAD_UC_IRAG == "6 a 11 Meses"| EDAD_UC_IRAG == "12 a 23 Meses" ~ "6 a 23 Meses ",
                                                TRUE ~ EDAD_UC_IRAG ))  

#Establezco niveles para los grupos de edad creados
niveles_edades <- c("Menor a 6 Meses","6 a 23 Meses","02 a 04 Años", "05 a 09 Años","10 a 14 Años")

data_virusedad <- data_virusedad %>% filter(GRUPO_ETARIO %in% niveles_edades) %>%  # ya saca "Sin Especificar"
                                     mutate(GRUPO_ETARIO = factor(GRUPO_ETARIO, levels = niveles_edades)) %>%
                                     arrange(GRUPO_ETARIO)

#Vectores para la identificacion de resultados de determinaciones

resultado_detectable_influenza <-c("Influenza A (sin subtipificar)","Influenza A H3N2","Influenza positivo-Sin Tipo",
                                   "Influenza B (sin linaje)","Influenza A H1N1")

resultado_detectable_VSR <- c("VSR A","VSR B","VSR")


resultado_detectable_covid <- ("Positivo")

#Creo variable con resultado final de determinacion 
data_virusedad<- data_virusedad %>% mutate (detectable_influenza = if_else(INFLUENZA_FINAL %in% resultado_detectable_influenza,1,0),
                                    detectable_VSR = if_else (VSR_FINAL %in% resultado_detectable_VSR,1,0),
                                    detectable_covid = if_else(COVID_19_FINAL == resultado_detectable_covid,1,0))

#Agrupo por edad y sumo determinaciones
data_virusedad <- data_virusedad %>% group_by(GRUPO_ETARIO) %>%
                                     summarise (casos_influenza = sum(detectable_influenza == 1, na.rm = TRUE),
                                                casos_vsr = sum(detectable_VSR == 1, na.rm = TRUE),
                                                casos_covid = sum(detectable_covid == 1, na.rm = TRUE),
                                                .groups = "drop") %>%
                                      arrange(GRUPO_ETARIO)


#Grafico determinaciones por grupo de edad

grafico_interactivo_virusedad <- highchart()  %>% 
  hc_chart(type = "bar")  %>% 
  hc_xAxis(categories = data_virusedad$GRUPO_ETARIO,
           title = list(text = "Grupo Etario"))  %>% 
  hc_yAxis(title = list(text = "Número de determinaciones"))  %>% 
  hc_plotOptions(
    column = list(stacking = "normal",
                  pointPadding = 0.1,   
                  groupPadding = 0.05,  
                  borderWidth = 0))  %>% 
  hc_exporting(enabled = T) %>%
  hc_add_series(name = "Influenza", data = data_virusedad$"casos_influenza", color = "#fdae6b")  %>% 
  hc_add_series(name = "VSR", data = data_virusedad$"casos_vsr", color = "#3182bd")  %>% 
  hc_add_series(name = "SARS CoV-2", data = data_virusedad$"casos_covid", color = "#0207a4")  %>% 
  hc_title(text = "Determinaciones positivas de virus respiratorios por grupo etario en IRAG e IRAGe.\n Desde SE23 2024-SE34 2025.\n Hospital Dr.Humberto J. Notti, Mendoza.")            

grafico_interactivo_virusedad

