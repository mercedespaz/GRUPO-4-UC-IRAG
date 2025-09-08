library(dplyr)
library(leaflet)
library(geoAr)
library(readr)

EFECTORES_MENDOZA <- read_csv("Mapa/EFECTORES MENDOZA.csv")
EFECTORES_MENDOZA <- EFECTORES_MENDOZA %>% mutate(NIVEL_COMPL = case_when(`NIVEL COMPL` == "NIVEL III" ~ "ALTA COMPLEJIDAD CON TERAPIA INTENSIVA",
                                                                          TRUE ~ "SIN CLASIFICAR"))
# Separar coordenadas en latitud y longitud
EFECTORES_MENDOZA <- EFECTORES_MENDOZA %>%
  mutate(
    lat = as.numeric(sub(",.*", "", Coordenadas)),
    lon = as.numeric(sub(".*,", "", Coordenadas))
  )

# Crear mapa base
mapa_efectores <- leaflet() %>%
  addArgTiles()

# Agregar hospitales (azules)
mapa_efectores <- mapa_efectores %>%
  addCircleMarkers(
    data = EFECTORES_MENDOZA %>% dplyr::filter(`tipo de establecimiento` == "HOSPITAL"),
    ~lon, ~lat,
    popup = ~paste0("<b>Nombre:</b> ", Nombre, "<br>",
                    "<b>Código SISA:</b> ", `Código SISA EFECTOR`, "<br>",
                    "<b>Nivel Complejidad:</b> ", NIVEL_COMPL, "<br>",
                    "<b>Zona Sanitaria:</b> ", `zona sanitaria`, "<br>",
                    "<b>Localidad:</b> ", LOCALIDAD, "<br>",
                    "<b>Tipo de Establecimiento:</b> ", `tipo de establecimiento`, "<br>",
                    "<b>Estrategia:</b> ", Estrategia),
    color = "blue",
    radius = 5,
    fillOpacity = 0.7
  )

# Agregar los establecimientos centinela (cruces rojas)
mapa_efectores <- mapa_efectores %>%
  addAwesomeMarkers(
    data = EFECTORES_MENDOZA %>% dplyr::filter(Estrategia == "CENTINELA"),
    ~lon, ~lat,
    popup = ~paste0("<b>Nombre:</b> ", Nombre, "<br>",
                    "<b>Código SISA:</b> ", `Código SISA EFECTOR`, "<br>",
                    "<b>Nivel Complejidad:</b> ", NIVEL_COMPL, "<br>",
                    "<b>Zona Sanitaria:</b> ", `zona sanitaria`, "<br>",
                    "<b>Localidad:</b> ", LOCALIDAD, "<br>",
                    "<b>Tipo de Establecimiento:</b> ", `tipo de establecimiento`, "<br>",
                    "<b>Estrategia:</b> ", Estrategia),
    icon = awesomeIcons(
      icon = "plus",
      library = "fa",
      markerColor = "red"
    )
  )

# Agregar leyenda
# Leyenda personalizada con HTML
# Leyenda personalizada con HTML y cruz negra
custom_legend <- htmltools::HTML('
<div style="background:white; padding: 10px; border-radius: 5px; box-shadow: 2px 2px 6px rgba(0,0,0,0.3); font-size: 14px;">
  <b>Tipos de Establecimientos</b><br>
  <i style="background:blue; width: 12px; height: 12px; display:inline-block; border-radius:50%; margin-right:6px;"></i> Hospital<br>
  <i class="fa fa-plus" style="color:black; background:#cc0415; border-radius: 50%; padding: 2px 5px; margin-right:6px;"></i> Unidad Centinela de IRAG
</div>
')

mapa_efectores <- mapa_efectores %>%
  addControl(custom_legend, position = "bottomright")

mapa_efectores