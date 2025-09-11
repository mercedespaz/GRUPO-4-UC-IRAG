#porcentaje de madres vacunadas en pacientes menores de 6 meses 
library(highcharter)
library(dplyr)

#Crear variable GRUPOETARIO
#La base original no tiene la categoria menor a 6 meses, construirla para luego poder filtrar
#por ese criterio

#armado de la base de datos
data_vacuna<- data%>% 
  select(VAC_VSR,EDAD_UC_IRAG) %>%
  mutate(GRUPO_ETARIO= case_when(EDAD_UC_IRAG == "0 a 2 Meses" | EDAD_UC_IRAG == "3 a 5 Meses" ~ "Menor a 6 Meses",
                                              EDAD_UC_IRAG == "6 a 11 Meses"| EDAD_UC_IRAG == "12 a 23 Meses" ~ "6 a 23 Meses ",
                                              TRUE ~ EDAD_UC_IRAG ))%>%
  filter(GRUPO_ETARIO=="Menor a 6 Meses")

#crear variable para vacuna SI en caso que la haya recibido y NO en defecto
data_vacuna <- data_vacuna %>%
  mutate(detectable_vacuna = case_when(
    `VAC_VSR` == "SE 32" |
      `VAC_VSR` == "SE 33"|
      `VAC_VSR` == "SE 34"|
      `VAC_VSR` == "SE 35"|
      `VAC_VSR` == "SE 36"
      ~ 1,
    
    `VAC_VSR` == "MADRE NO VACUNADA" |
      `VAC_VSR` == "SIN DATO"  ~ 0,
    # Si ninguna de las condiciones anteriores se cumple, asignar NA
    TRUE ~ NA
  ))

# realizar conteo de ctos SI y ctos NO
data_vacuna_tabla <- data_vacuna %>%
  filter(!is.na(detectable_vacuna)) %>%  # opcional, quitar NA
  count(detectable_vacuna) %>%
  rename(
    Vacuna = detectable_vacuna,
    Frecuencia = n
  )

#Comentar los prints para que no se renderizen en el quarto
#data_vacuna_tabla
 
# Calcular frecuencias
totales <- sum(data_vacuna_tabla$Frecuencia)

# Categorías
categorias <- c("SI VACUNA MATERNA", "NO VACUNA MATERNA")

# Datos con porcentaje
valores <- data_vacuna_tabla %>%
  filter(Vacuna %in% c("1","0")) %>%
  mutate(
    Grupo = ifelse(Vacuna == "1", "SI VACUNA MATERNA", "NO VACUNA MATERNA"),
    Porcentaje = round(Frecuencia / totales * 100, 1)
  )

# Gráfico horizontal de barras con Highchart
Grafico_vacuna <- highchart() %>%
  hc_chart(type = "bar") %>%
  hc_xAxis(
    categories = categorias, #c("Vacunación"), # una sola categoría
    title = list(text = NULL),
    labels = list(enabled = FALSE) #no muestra etiqueta en el eje x
  ) %>%
  hc_yAxis(
    title = list(text = "Cantidad de madres"),
    max = max(valores$Frecuencia) * 1.2
  ) %>%
  hc_plotOptions(
    bar = list(
      dataLabels = list(
        enabled = TRUE,
        formatter = JS(
          paste0(
            "function() {",
            "  var total = ", totales, ";",
            "  var pct = (this.y / total * 100).toFixed(1);",
            "  return this.y + ' casos - ' + pct + '%';",
            "}"
          )
        )
      )
    )
  ) %>%
  # Serie 1: SI vacunación materna
  hc_add_series(
    name = "SI VACUNA MATERNA",
    data = list(valores$Frecuencia[valores$Grupo == "SI VACUNA MATERNA"]),
    color = "#feb24c"
  ) %>%
  # Serie 2: NO vacunación materna
  hc_add_series(
    name = "NO VACUNA MATERNA",
    data = list(valores$Frecuencia[valores$Grupo == "NO VACUNA MATERNA"]),
    color = "#3182bd"
  ) %>%
  hc_title(
    text = "Vacunación materna VSR en grupo etario menor a 6 meses
    con IRAG e IRAGe - Hospital Dr. H. Notti"
  )

Grafico_vacuna
