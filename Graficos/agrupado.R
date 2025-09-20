#librerias 
library(readr)
library(tidyverse)
library(highcharter)


#Carga de la base de datos agrupados
UC_IRAG_Carga_Agrupada <- read_csv("UC IRAG - Carga Agrupada-Mendoza-Notti - HOSPITAL HUMBERTO J. NOTTI.-.csv")


#Elimino primeraa fila que no contiene datos
UC_IRAG_Carga_Agrupada <-UC_IRAG_Carga_Agrupada[-1,]

 
#unique(UC_IRAG_Carga_Agrupada_Mendoza_Notti_HOSPITAL_HUMBERTO_J_NOTTI_$NOMBREEVENTOAGRP)

#Crear el data frame de datos agrupados segun nuestras variables de interés

evento<-c("Pacientes internados por todas las causas", "Casos de IRAG entre los internados","Casos de IRAG extendida entre los internados")

UC_IRAG_Carga_Agrupada$ANIO<-as.numeric (UC_IRAG_Carga_Agrupada$ANIO)

UC_IRAG_Carga_Agrupada$SEMANA<-as.numeric (UC_IRAG_Carga_Agrupada$SEMANA)


dataagrupado <- UC_IRAG_Carga_Agrupada %>%
  # Filtrar columnas de interés
  select(ANIO, SEMANA, NOMBREEVENTOAGRP, `0 a 2 m`, `3 a 5 m`, `6 a 11 m`,
         `12 a 23 m`, `2 a 4 años`, `5 a 9 años`, `10 a 14 años`) %>%
  # Filtrar filas 
  filter(
    NOMBREEVENTOAGRP %in% evento &
      ((ANIO == 2024 & SEMANA >= 23) |
         (ANIO == 2025 & SEMANA <= 34))) %>%
  # Crear etiqueta de semana y año
  mutate(
    sepi_label = paste(ANIO, "- SE", SEMANA),
    sepi_label = factor(sepi_label, levels = unique(paste(ANIO, "- SE", SEMANA))))


# Se estructura a formato largo: pasar columnas de edad a una variable "grupo_etario"
# y guardar los valores en "casos_totales"

dataagrupado<-dataagrupado %>% pivot_longer(4:10, names_to = "grupo_etario",
                                            values_to = "casos_totales" )

#se transforma la variable casos totales a numérica
dataagrupado$casos_totales<-as.numeric(dataagrupado$casos_totales)
 
#se suman casos internados por evento y semana
dataagrupado<-dataagrupado %>%
  group_by(NOMBREEVENTOAGRP,sepi_label) %>%
  summarise(casosinternados=sum(casos_totales),
            .groups = ("drop"))

# se estructura a formato ancho,separando categorías de NOMBREEVENTOAGRP en columnas
dataagrupado<-dataagrupado %>% pivot_wider(names_from = NOMBREEVENTOAGRP,
                               values_from = casosinternados)

#se corre este comando para calcular la suma de IRAG e IRAGe
dataagrupado<- dataagrupado %>%
  mutate(IRAG_totales =(`Casos de IRAG entre los internados`+`Casos de IRAG extendida entre los internados`),
         Otras_internaciones =(`Pacientes internados por todas las causas`-`IRAG_totales`))

# se calcula el porcentaje de IRAG total (IRAG + IRAGe) sobre internaciones totales
dataagrupado<-dataagrupado %>% mutate(pct_irag = 
  round(`IRAG_totales` / `Pacientes internados por todas las causas` * 100,1))

#realizar grafico en highcharter: % IRAGtotal /internaciones totales por SE
Graficoagrupado <- highchart() %>%
  hc_xAxis(categories = dataagrupado$sepi_label,
           title = list(text = "Semana epidemiológica")) %>%
  hc_yAxis_multiples(
    list(title = list(text = "Número de internaciones"), 
         opposite = FALSE, 
         min = 0, 
         gridLineWidth = 1),  
    list(title = list(text = "% Internaciones por IRAG e IRAGe"), 
         opposite = TRUE, 
         min = 0, 
         max = 100,
         ceiling = 100,
         endOnTick = FALSE,
         gridLineWidth = 0)   
  ) %>%
  hc_plotOptions(
    column = list(
      stacking = "normal",
      pointPadding = 0.1,   
      groupPadding = 0.05,  
      borderWidth = 0,
      dataLabels = list(enabled = FALSE)
    ),
    line = list(
      dataLabels = list(enabled = FALSE)
    )
  ) %>%
  # Serie IRAG
  hc_add_series(name = "Internaciones por IRAG e IRAGe",
                type = "column",
                data = dataagrupado$IRAG_totales,
                color = "#fee391",
                yAxis = 0,
                stack = "internados") %>%
  hc_add_series(name = "Internaciones por otras causas",
                type = "column",
                data = dataagrupado$Otras_internaciones,
                color = "#DEB887",
                yAxis = 0,
                stack = "internados") %>%
  # Línea de porcentaje
  hc_add_series(name = "% Internaciones por IRAG e IRAGe",
                type = "line",
                data =dataagrupado$pct_irag,
                color = "#525252",
                yAxis = 1,
                tooltip = list(valueSuffix = "%"),
                dataLabels = list(enabled = FALSE)) %>%
  hc_exporting(enabled= TRUE) %>%
  hc_title(text = "")


Graficoagrupado


