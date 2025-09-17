#librerias 
library(readr)
library(readr)
library(tidyverse)
library(highcharter)

#carga de la base de datos agrupados
UC_IRAG_Carga_Agrupada_Mendoza_Notti_HOSPITAL_HUMBERTO_J_NOTTI_ <- read_csv("UC IRAG - Carga Agrupada-Mendoza-Notti - HOSPITAL HUMBERTO J. NOTTI.-.csv")
#View(UC_IRAG_Carga_Agrupada_Mendoza_Notti_HOSPITAL_HUMBERTO_J_NOTTI_)

#unique(UC_IRAG_Carga_Agrupada_Mendoza_Notti_HOSPITAL_HUMBERTO_J_NOTTI_$NOMBREEVENTOAGRP)

#crear el data frame de datos agrupados segun nuestras variables de interés

dataagrupado <- UC_IRAG_Carga_Agrupada_Mendoza_Notti_HOSPITAL_HUMBERTO_J_NOTTI_ %>%
  # Filtrar columnas y filas de interés
  select(ANIO, SEMANA, NOMBREEVENTOAGRP, 
         `0 a 2 m`, `3 a 5 m`, `6 a 11 m`,
         `12 a 23 m`, `2 a 4 años`, `5 a 9 años`, `10 a 14 años`) %>%
  filter(
    NOMBREEVENTOAGRP %in% c(
      "Pacientes internados por todas las causas",
      "Casos de IRAG entre los internados",
      "Casos de IRAG extendida entre los internados"
      
    ) &
      (
        (ANIO == 2024 & SEMANA >= 23) |
          (ANIO == 2025 & SEMANA <= 34)
      )
  ) %>%
  # Crear etiqueta de semana y año
  mutate(sepi_label = paste0(ANIO, " - SE ", SEMANA))

# se estructura a formato largo: pasar columnas de edad a una variable "grupo_etario"
# y guardar los valores en "casos_totales"

dataagrupado<-dataagrupado %>%
  pivot_longer(4:10, names_to = "grupo_etario",
                values_to = "casos_totales" )

#se transforma la variable casos totales a numérica
dataagrupado$casos_totales<-as.numeric(dataagrupado$casos_totales)
 
#se suman casos internados por evento y semana
dataagrupado<-dataagrupado %>%
  group_by(NOMBREEVENTOAGRP,sepi_label) %>%
  summarise(casosinternados=sum(casos_totales),
            .groups = ("drop"))

# se estructura a formato ancho,separando categorías de NOMBREEVENTOAGRP en columnas
dataagrupado<-dataagrupado %>%
  pivot_wider(names_from = NOMBREEVENTOAGRP,
               values_from = casosinternados)

#se corre este comando para calcular la suma de IRAG e IRAGe
dataagrupado<- dataagrupado %>%
  mutate(IRAG_totales =
           (`Casos de IRAG entre los internados`+`Casos de IRAG extendida entre los internados`)) 

# se calcula el porcentaje de IRAG total (IRAG + IRAGe) sobre internaciones totales
dataagrupado<- dataagrupado %>%
  mutate(pct_IRAG = (`IRAG_totales` / 
                       `Pacientes internados por todas las causas`) * 100)


#realizar grafico en highcharter: % IRAGtotal /internaciones totales por SE
Graficoagrupado <- highchart() %>%
  hc_xAxis(categories = dataagrupado$sepi_label,
           title = list(text = "Semana Epidemiológica")) %>%
  hc_yAxis_multiples(
    list(title = list(text = "Número de casos"), opposite = FALSE),
    list(title = list(text = "% IRAG e IRAGe sobre internados"), 
         opposite = TRUE, max = 100)
  ) %>%
  hc_plotOptions(
    column = list(
      stacking = "normal",             # <-- barras apiladas
      dataLabels = list(enabled = FALSE)
    ),
    line = list(
      dataLabels = list(enabled = TRUE, format = "{y}%")
    )
  ) %>%
  # Serie IRAG
  hc_add_series(name = "Casos de IRAG e IRAGe",
                type = "column",
                data = dataagrupado$`IRAG_totales`,
                color = "#fee391",
                yAxis = 0,
                stack = "internados") %>%
  # Serie otros internados (se calcula automáticamente = total - IRAG)
  hc_add_series(name = "Internaciones totales",
                type = "column",
                data = dataagrupado$`Pacientes internados por todas las causas` - dataagrupado$`IRAG_totales`,
                color = "#cc4c02",
                yAxis = 0,
                stack = "internados") %>%
  # Línea de porcentaje
  hc_add_series(name = "% IRAG e IRAGe",
                type = "line",
                data = round(dataagrupado$pct_IRAG, 1),
                color = "#525252",
                yAxis = 1,
                tooltip = list(valueSuffix = "%"),
                dataLabels = list(enabled = TRUE, format = "{y}%")) %>%
  hc_title(text = "Porcentaje de IRAG sobre el total de internaciones
           \nSE23 2024 - SE34 2025\nHospital Dr.H.Notti, Mendoza")

Graficoagrupado


