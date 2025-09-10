#library(gt)
#library(dplyr)
#library(stringr)

# Variables originales en la base
vars <- c("BAJO_PESO_NACIMIENTO","ASMA","CARDIOPATIA_CONGENITA",
          "DESNUTRICION","BRONQUIOLITIS_PREVIA","ENF_NEUROLOGICA_CRONICA",
          "OBESIDAD","PREMATURIDAD","S_DOWN","OTRAS_COMORBILIDADES")

#  nombres más legibles
vars_title <- vars %>% 
  str_to_lower() %>%              # paso a minúscula
  str_replace_all("_", " ") %>%   # guión bajo → espacio
  str_to_title()                  # primera letra en mayúscula

# Selección, recodificación y renombrado
data_co <- notti %>% 
  select(all_of(vars)) %>% 
  mutate(across(everything(), ~ ifelse(. == 9, 0, .))) %>% 
  setNames(vars_title)


#sumar cada columna
totales_comorbilidad <- colSums(data_co, na.rm = TRUE)

# armar dataframe con total y porcentaje
totales_comorbilidad_df <- data.frame(
  Comorbilidad = names(totales_comorbilidad),
  Total = as.vector(totales_comorbilidad)
) %>%
  mutate(Porcentaje = round(100 * Total / nrow(data_co), 1)) %>%
  arrange(desc(Total))

#tabla con gt

graficocomorbilidad <- totales_comorbilidad_df %>% 
  gt() %>%
  tab_header(
    title = "Comorbilidades notificadas en IRAG e IRAGe 
    \n -Hospital Dr.H.Notti",
    ) %>%
  fmt_number(columns = "Porcentaje", decimals = 1) %>%
  cols_label(
    Comorbilidad = "Comorbilidad",
    Total = "Frecuencia",
    Porcentaje = "%"
  ) %>%
  # Nota al pie de la tabla
  tab_source_note(
    source_note = md("Fuente de datos: Sistema Nacional de Vigilancia de la Salud")
  ) %>%
  # Nota al pie en cabecera
  tab_footnote(
    footnote = "Comorbilidades notificadas en el SNVS",
    locations = cells_column_labels(columns = Comorbilidad)
  ) %>%
  # Aplicar colores a la columna Total
  data_color(
    columns = Total,
    colors = scales::col_numeric(
      palette = c("#deebf7", "#3182bd"),   # de verde muy claro a rojo oscuro
      domain = range(totales_comorbilidad_df$Total)
    )
  ) %>%
  # Fuente y estilo
  opt_table_font(
    font = "Helvetica", 
    weight = "normal"
  ) %>%
  # Alinear cabeceras de columnas 1 y 2 a la izquierda
  tab_style(
    style = cell_text(align = "left"), 
    locations = cells_column_labels(columns = c(Comorbilidad, Total))
  )

graficocomorbilidad













