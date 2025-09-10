#Cargar librerías
#Sugiero cargar solo las librerias a utilizar en el codigo, en este caso son
#dplyr,readr,tidyr

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

data <- read_csv("UC IRAG - Carga Agrupada-Mendoza-Notti - HOSPITAL HUMBERTO J. NOTTI.-.csv")

#Excluir primera fila del data frame (contiene descripcion de variables)
data <- data[-1, ]

#Identifico el nombre de las columanas para poder seleccionarlas

names(data)  

#Selecciono las columnas que voy a utilizar para estimar el indicador de datos agrupados
#que me interesa mostrar en el informe actual, menores de 15 años

df_agrupada <- data %>% select(ANIO,SEMANA,NOMBREEVENTOAGRP,"0 a 2 m","3 a 5 m", "6 a 11 m","12 a 23 m",
                                                             "2 a 4 años","5 a 9 años","10 a 14 años")

#Identifico que los valores de las variables de grupos estarios,
#están consideradas como categóricas en lugar de numéricas; cambio datos a variables numéricas

spec(data)

#Convertir columnas categóricas a numéricas
#Sugiero aplicar la funcion as. mueric al data frame creado anteriormente (df_agrupado). Sería así:

df_agrupada <- df_agrupada %>% mutate(across(c("0 a 2 m","3 a 5 m", "6 a 11 m","12 a 23 m",
                                      "2 a 4 años","5 a 9 años","10 a 14 años"),as.numeric))


#Usando esta opcion se genera un archivo más en el entorno de trabajo
df_agrupada_numerica <- df_agrupada %>% 
  mutate(across(c("0 a 2 m","3 a 5 m", "6 a 11 m","12 a 23 m",
                  "2 a 4 años","5 a 9 años","10 a 14 años"),as.numeric))

#Filtro las filas de "internados totales" 

internados_totales_por_SE <- df_agrupada_numerica %>% filter(NOMBREEVENTOAGRP == "Pacientes internados por todas las causas")

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


#Consideraciones para el analisis


#Selecciono clasificaciones de interés para el cálculo de porcentaje de ocipación de camas

#Paramatros temporales: excluir registros previos a la implementacion de la estrategia (SE 23-2024) 
# y posteriores al punto de corte del análisis (SE 34-2025)

df_agrupada<-df_agrupada %>% mutate (SEPI=paste0(ANIO,"-",SEMANA)) %>%
  relocate(SEPI,.after = SEMANA)

comienzo_estrategia <-"2024-23"
corte_analisis <-"2025-34" 

#Filtro segun criterios temporales
df_agrupada<-df_agrupada %>% filter(SEPI >= comienzo_estrategia & SEPI <= corte_analisis)

#La función unique me permite conocer los nombres de las categorías de la variable evento agrupado
unique(data$NOMBREEVENTOAGRP)


#Categorias de interés para la ocupación de camas por IRAG e IRAGe
internados<- c("Pacientes internados por todas las causas","Casos de IRAG entre los internados","Casos de IRAG extendida entre los internados")

df_agrupada<- df_agrupada %>% filter (NOMBREEVENTOAGRP %in% internados) %>%
  pivot_longer(5:ncol(df_agrupada),names_to = "grupo_edad",values_to ="casos_internados")



df_agrupada$casos_internados <- as.numeric(df_agrupada$casos_internados)

df_agrupada<-df_agrupada %>% group_by(SEPI,NOMBREEVENTOAGRP) %>%
  summarise(internados= sum(casos_internados),
            .groups = "drop")

#Calcular porcentaje de ocupacion por SE 
ocupacion_internacion <- df_agrupada %>%
  pivot_wider(
    names_from = NOMBREEVENTOAGRP,
    values_from = internados
  ) %>%
  mutate(
    pct_irag = round(`Casos de IRAG entre los internados` / `Pacientes internados por todas las causas` * 100,1),
    pct_irag_extendida =round(`Casos de IRAG extendida entre los internados` / `Pacientes internados por todas las causas` * 100,1)
  )


