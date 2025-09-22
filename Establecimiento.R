# ===============================
# Valor único de ESTABLECIMIENTO_INTERNACION
# ===============================
establecimiento <- unique(data$ESTABLECIMIENTO_INTERNACION)

print(establecimiento)

establecimiento <- str_to_title(establecimiento)

establecimiento <- str_replace_all(establecimiento, "-", " ")


