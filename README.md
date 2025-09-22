# GRUPO-4-UC-IRAG
# **Repositorio para el trabajo en la automatización de reportes epidemiológicos: UC-IRAG Hospital Dr. Humberto J. Notti**

Este repositorio contiene el proyecto de análisis automatizado de datos derivados de la Estrategia de Vigilancia en Unidades Centinela (UC-IRAG) de Infección Respiratoria Aguda Grave (IRAG) e Infección Respiratoria Aguda Grave extendida (IRAGe) en pacientes pediátricos internados del Hospital Dr. Humberto J. Notti de Mendoza. El objetivo principal es la organización, procesamiento y análisis automatizado de la información nominal y agrupada disponible, contribuyendo a evidenciar la importancia de esta estrategia de vigilancia,  aportando información de calidad y robustez para la toma de decisiones sanitarias y brindando información epidemiológica oportuna para los equipos de salud.

## **Contenido del repositorio:**

+ Proyecto en R Studio, denominado: “UC-IRAG-Grupo 4.Rproj”
+ Archivo Quarto (.qmd): “EJEMPLO.qmd”
+ README.md: documento que describe el contenido del repositorio en GitHub
+ Documento de salida en formato HTML: “EJEMPLO.html”
+ Plan de análisis
+ Scripts en R
+ Bases de datos
+ Otros materiales: logos, estilos.css, algoritmos.

### **Archivo Quarto (EJEMPLO.qmd):**

Incluye el encabezado con la estructura YAML, donde se definen:

**Título del informe:**'UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS GRAVES (UC-IRAG): HOSPITAL "DR. HUMBERTO J. NOTTI", MENDOZA'

**Autoría:** Celeste Guerrero, Gonzalo Crombas, Julia Lozano, Lia Bosio, Mara Russo, María Elena Dattero.

**Fecha de publicación**.

**Formato de salida (HTML).**

**Opciones globales de estilo.**

#### **Contiene:**

+ Índice del documento (table of contents).
+ Texto enriquecido
+ Chunks de código en R:
+ Scripts de carga y procesamiento de datos.
+ Generación de tablas y gráficos.
+ Configuración de qué se muestra y qué se oculta en el informe final.

### **Scripts en R (.R)**

Archivos separados con funciones de limpieza, procesamiento y análisis exploratorio.

Se invocan desde el archivo Quarto para mantener ordenado el flujo de trabajo.

### **Base de datos**

La base nominal utilizada no está integrada al repositorio por cuestiones de confidencialidad.

Para trabajar en el proyecto en RStudio se debe incorporar la base localmente.

El código del proyecto incluye instrucciones de importación de dicha base (importación_base.R).

### **Plan de análisis**

Documento que guía las decisiones metodológicas.

Define las variables a estudiar, los grupos etarios, las métricas de análisis y los métodos estadísticos.

Sirve como referencia para interpretar cada elemento generado en el proyecto.

### **Scripts principales**

**1. Mapa de efectores (`mapa_efectores.R`)**  
  
  Script en R que:  
  
  Importa la base `EFECTORES MENDOZA.csv`.  
  
  Clasifica el nivel de complejidad de cada efector.  
  
  Separa coordenadas en latitud y longitud.  
  
  Genera un mapa interactivo con Leaflet:  
  + Hospitales (círculos azules).  
  + Unidades centinelas IRAG (cruces rojas).  
  + Incluye leyenda personalizada en HTML.

**2. Casos IRAG e IRAGe por Semana Epidemiológica (`casos_se.R`)**  
 
 Script en R que:  
 
 Importa la base nominal mediante `importacion_base.R`.  
 
 Filtra los casos válidos (excluyendo “Caso invalidado por epidemiología”).  
 
 Agrupa los casos por clasificación, semana epidemiológica y año.  
 
 Reorganiza los datos en formato ancho para su análisis.  
 
 Genera un gráfico interactivo apilado con **Highcharter**, mostrando:  
 
  + IRAG extendida (naranja) e Infección respiratoria aguda grave (IRAGe) (azul).  
 
 El resultado es un gráfico de columnas apiladas que permite visualizar la evolución de los casos desde SE23/2024 hasta SE34/2025 en el Hospital Dr. Humberto Notti.  

**3. Internaciones por IRAG e IRAGe en relación al total de internaciones (`Graficoagrupado.R`)**  
 
 Script en R que:  
 
 Importa la base de datos agrupada de hospitalizaciones.  
 
 Filtra semanas epidemiológicas de interés (desde SE23/2024 hasta SE34/2025).
 
 Reestructura los datos en formato largo y ancho para análisis por grupo etario y evento.  
 
 Calcula:  
 + Total de internaciones por IRAG (IRAG + IRAG extendida).  
 + Otras internaciones (todas las causas menos IRAG).  
 + Porcentaje de internaciones por IRAG sobre el total.  

Genera un gráfico interactivo con **Highcharter** que combina:  
 + Columnas apiladas (IRAG/IRAGe vs. otras causas).  
 + Línea con el porcentaje de IRAG sobre el total.  
 
 El resultado permite visualizar tanto la carga absoluta como la proporción de IRAG en las internaciones hospitalarias.  

**4. Determinaciones positivas por virus y semana epidemiológica (`grafico_interactivo_virus.R`)**

  Script en R que:  
  
  Importa la base nominal y selecciona columnas de interés (semana, año y resultados de laboratorio).  
  
  Recodifica variables para identificar resultados positivos de **Influenza**, **Virus Sincitial Respiratorio (VSR)** y **SARS-CoV-2**.  
  
  Agrupa por semana epidemiológica y año, sumando la cantidad de casos positivos para cada virus.  
  
  Genera un gráfico interactivo con Highcharter:  
   + Columnas apiladas con los casos semanales de Influenza, VSR y SARS-CoV-2.  
  
  Permite visualizar la distribución temporal de los virus respiratorios en internaciones hospitalarias. El resultado ofrece una visión clara de la dinámica de circulación viral entre  SE23/2024 y SE34/2025.  

**5. Determinaciones positivas de virus respiratorios  por grupo etario (`grafico_interactivo_virusedad.R`)**

 Script en R que:
 
 Carga de la base de datos (importacion_base.R).
 
 Selección de variables de interés: edad y resultados virales.
 
 Reclasificación de grupos etarios.
 
 Identificación de resultados positivos para cada virus.
 
 Agrupamiento y conteo de casos por grupo etario.
 
 Creación del gráfico interactivo con highcharter, mostrando barras apiladas por virus y habilitando exportación.
 
 El gráfico permite visualizar rápidamente la distribución de virus respiratorios según la edad y comparar su prevalencia entre grupos etarios.

**6. Comorbilidades notificadas  (tabla_comorbilidad.R)**
 
 Script en R que:
 
 Selecciona las 10 comorbilidades más frecuentes en la cohorte.
 
 Recodifica valores y renombra columnas con nombres legibles.
 
 Calcula frecuencia y porcentaje de cada comorbilidad.
 
 Genera una tabla interactiva con gt, con colores escalados, notas de fuente (SNVS 2.0) y formato optimizado para presentación.
 
 Permite visualizar rápidamente las comorbilidades más frecuentes en pacientes con IRAG/IRAGe entre SE23 2024 y SE34 2025 en el Hospital Dr. Humberto J. Notti.

**7. Porcentaje de madres vacunadas contra VSR en pacientes menores de 6 meses (grafico_vacuna.R)**
 
 Script en R que:
 
 Selecciona pacientes menores de 6 meses y la variable de vacunación materna contra VSR.
 
 Crea una variable binaria que indica si la madre recibió la vacuna o no.
 
 Calcula la frecuencia y el porcentaje de pacientes con antecedentes de vacunación materna.
 
 Genera un gráfico interactivo de barras horizontales con highcharter:
 
+ Una barra para “Con antecedentes de vacuna materna” y otra para “Sin antecedentes de vacuna materna”.
 Colores diferenciados y tooltip con conteo de casos.

Se habilita la exportación del gráfico.

El resultado permite visualizar claramente la cobertura de vacunación materna en menores de 6 meses entre SE23/2024 y SE34/2025 en el Hospital Dr. Humberto J. Notti, Mendoza.
















