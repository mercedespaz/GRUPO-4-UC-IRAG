#Cargar librerías

library(dplyr)
library(readr)
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


#CONSIDERACIONES PARA EL ANALISIS

#Seleccionar columnas de interes (ya lo hiciste)

#Filtrar clasificaciones de interés para el cálculo de porcentaje de ocupación de camas
# "Casos de IRAG entre los internados" y"Pacientes internados por todas las causas"  

#Paramatros temporales: filtrar registros previos a la implementacion de la estrategia (SE 23-2024) 
# y posteriores al punto de corte del análisis que están realizando (SE 34-2025)

#Se puede crear una variable que concatene el año y SE y filtrar a partir de allí

#Suma de internados por SE sugiero pivotear el dataframe hacia lo largo
#pasando a filas los grupos de edad 0-2 meses a 14 años, definir el nombre de la nueva columna y el argumento values
#Para poder sumarizar los internados, la columna values tiene que ser convertida a numeric.

#Agrupar y sumarizar para obtener internados por SE y nombre del evento: internados por todas las causas
#e internados por IRAG

#Para poder tener un dataframe que en cada columna tenga los internados por IRAG e internados totales
#podemos pivotear el dataframe creado anteriormente a formato wider donde los nombres de las
#columnas sean el nombre del evento agrupado y los valores el conteo del dataframe anterior






