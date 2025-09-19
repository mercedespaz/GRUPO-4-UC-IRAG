library(tidyverse)
library(lubridate)  # viene con tidyverse
library(dplyr)

data <- read.csv2("notti.csv", sep = ";", encoding = "latin1")

# ===============================
# Valor único de ESTABLECIMIENTO_INTERNACION
# ===============================
establecimiento <- unique(data$ESTABLECIMIENTO_INTERNACION)

unique(data$ESTABLECIMIENTO_INTERNACION)

# ===============================
# Período de tiempo (columna FECHA_MINIMA)
# ===============================

data_filtrada <- data %>% select(CLASIFICACION_MANUAL,SEPI_MIN_INTERNACION,ANIO_MIN_INTERNACION) %>%
  filter(CLASIFICACION_MANUAL != "Caso invalidado por epidemiología") %>% 
  group_by(CLASIFICACION_MANUAL,SEPI_MIN_INTERNACION, ANIO_MIN_INTERNACION) %>%
  summarise(casos_semana = n(),.groups = "drop") %>% 
  arrange(ANIO_MIN_INTERNACION) %>% 
  pivot_wider(names_from = CLASIFICACION_MANUAL,values_from = casos_semana)

data_filtrada <- data_filtrada %>% arrange(ANIO_MIN_INTERNACION,SEPI_MIN_INTERNACION) %>%
  mutate(sepi_label = paste0(ANIO_MIN_INTERNACION, " - SE ", SEPI_MIN_INTERNACION),
         sepi_label = factor(sepi_label, levels = unique(paste(ANIO_MIN_INTERNACION, "- SE", SEPI_MIN_INTERNACION))))




# ===============================
# Frecuencias absolutas y relativas de IRAG e IRAGE
# ===============================

# porcentaje con 1 decimal

frecuencias_clasif <- data %>%
  count(`CLASIFICACION_MANUAL`) %>%
  mutate(prop = round(100 * n / sum(n), 1))  

# Para acceder a cada valor individual:
frec_IRAG <- frecuencias_clasif %>% filter(`CLASIFICACION_MANUAL` == "Infección respiratoria aguda grave (IRAG)") %>% pull(n)
frec_IRAG_EXT <- frecuencias_clasif %>% filter(`CLASIFICACION_MANUAL` == "IRAG extendida") %>% pull(n)

prop_IRAG <- frecuencias_clasif %>% filter(`CLASIFICACION_MANUAL` == "Infección respiratoria aguda grave (IRAG)") %>% pull(prop)
prop_IRAG_EXT <- frecuencias_clasif %>% filter(`CLASIFICACION_MANUAL` == "IRAG extendida") %>% pull(prop)

total_IRAG <- frecuencias_clasif %>% 
  filter(`CLASIFICACION_MANUAL` %in% c("Infección respiratoria aguda grave (IRAG)", 
                                       "IRAG extendida")) %>% 
  pull(n) %>% 
  sum()

# ====================================================
# Frecuencias absolutas y relativas de internaciones
# ====================================================

#ver (no tenemos el nro total de intenaciones o si?)

# ===========================================
# Total de positivos en las columnas virales
# ===========================================

resultado_detectable_influenza <-c("Influenza A (sin subtipificar)","Influenza A H3N2","Influenza positivo-Sin Tipo",
                                   "Influenza B (sin linaje)","Influenza A H1N1")

resultado_detectable_VSR <- c("VSR A","VSR B","VSR")


resultado_detectable_covid <- ("Positivo")

data<- data %>% mutate (detectable_influenza = if_else(INFLUENZA_FINAL %in% resultado_detectable_influenza,1,0),
                                    detectable_VSR = if_else (VSR_FINAL %in% resultado_detectable_VSR,1,0),
                                    detectable_covid = if_else(COVID_19_FINAL == resultado_detectable_covid,1,0))

casos_influenza <- sum(data$detectable_influenza, na.rm = TRUE)
casos_VSR       <- sum(data$detectable_VSR, na.rm = TRUE)
casos_covid     <- sum(data$detectable_covid, na.rm = TRUE)

total_detectables <- casos_influenza + casos_VSR + casos_covid

#Porcentajes

porc_influenza <- round((casos_influenza / total_detectables) * 100, 1)
porc_VSR       <- round((casos_VSR / total_detectables) * 100, 1)
porc_covid     <- round((casos_covid / total_detectables) * 100, 1)


total_casos <- nrow(data)

porc_casos     <- round((total_detectables / total_IRAG) * 100, 1)
