#cargar librerías

library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(readxl)
library(writexl)
library(readr)
library(gtable)
library(gt)
library(flextable)
library(reactable)
library(tibble)
library(tidyr)

#Importar la base de datos agrupada: corresponde a hoja de cálculo de drive 
#compartida por Vigilancia Epi Nación a los equipos de UC-IRAG. Base H. Notti

#Descargo base desde el drive en formato csv. Incorporo a data de mi R.project

data <- read_csv("data/UC IRAG - Carga Agrupada-Mendoza-Notti - HOSPITAL HUMBERTO J. NOTTI.-.csv")

#Identifico el nombre de las columanas para poder seleccionarlas

names(data)  

#Selecciono las columnas que voy a utilizar para estimar el indicador de datos agrupados
#que me interesa mostrar en el informe actual, menores de 15 años

df_agrupada <- data %>% 
  select(ANIO,SEMANA,NOMBREEVENTOAGRP,"0 a 2 m","3 a 5 m", "6 a 11 m","12 a 23 m",
         "2 a 4 años","5 a 9 años","10 a 14 años")

#Identifico que los valores de las variables de grupos estarios,
#están consideradas como categóricas en lugar de numéricas; cambio datos a variables numéricas

spec(data)

#convertir columnas categóricas a numéricas

df_agrupada_numerica <- df_agrupada %>% 
  mutate(across(c("0 a 2 m","3 a 5 m", "6 a 11 m","12 a 23 m",
                  "2 a 4 años","5 a 9 años","10 a 14 años"),as.numeric))


#Filtro las filas de "internados totales" 

internados_totales_por_SE <- df_agrupada_numerica %>% 
  filter(NOMBREEVENTOAGRP == "Pacientes internados por todas las causas")

#y agrego una columna con la suma de casos por SE?????

suma_internados_totales <- internados_totales_por_SE %>% 
  group_by(ANIO,SEMANA,"0 a 2 m","3 a 5 m", "6 a 11 m","12 a 23 m",
         "2 a 4 años","5 a 9 años","10 a 14 años") %>%
  summarise(int_totales_semana = n(),.groups = "drop")


# Calcular total por fila?????
# ej chat GPT

suma_internados_totales <- rowSums(df_agrupada_numerica[, c("0 a 2 m", "3 a 5 m", "6 a 11 m", "12 a 23 m",
                           "2 a 4 años", "5 a 9 años", "10 a 14 años")])

print(suma_internados_totales)


  

#Filtro las filas de "internados IRAG" y agrego una columna con suma de casos por SE

internados_IRAG_por_SE <- df_agrupada_numerica %>% 
  filter(NOMBREEVENTOAGRP == "Casos de IRAG entre los internados")



