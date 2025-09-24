
# **Repositorio para el trabajo en la automatización de reportes epidemiológicos: UC-IRAG Hospital Dr. Humberto J. Notti**

Este repositorio contiene el proyecto de análisis automatizado de datos derivados de la Estrategia de Vigilancia en Unidades Centinela (UC-IRAG) de Infección Respiratoria Aguda Grave (IRAG) e Infección Respiratoria Aguda Grave extendida (IRAGe) en pacientes pediátricos internados del Hospital Dr. Humberto J. Notti de Mendoza. 

## **Objetivo**
+ Automatizar el procesamiento y análisis de la información nominal y agrupada disponible.
+ Brindar información epidemiológica oportuna para los equipos de salud.

# **Requisitos previos**

Para poder reproducir el análisis aquí propuesto es necesario instalar previamente (en caso de no tenerlas),las siguientes dependencias:
+ R. Puede descargarse [aquí](https://cran.r-project.org/) 
+ RStudio. Puede descargarse [aquí](https://posit.co/download/rstudio-desktop/)
+ Quarto. Puede desargarse [aquí](https://quarto.org/). Las versiones recientes de RStudio ya incluyen Quarto.

# **Pasos para utilizar el repositorio y generar un reporte automatizado**

+ Ingresar al repositorio de [Github](https://github.com/mercedespaz/GRUPO-4-UC-IRAG)
+ Para descargar el material del repositorio, hacer click en el botón verde "Code", luego hacer click en "Download ZIP".
+ Descomprimir el archivo zip en la computadora de trabajo.

**Todos los archivos deben estar guardados en una misma carpeta**

# **Contenido y estructura del repositorio:**

+ Carpeta "Data y plantillas" : Contiene las bases de datos que sirven de input para los scripts y archivos que configuran el estilo del reporte (.css, imágenes y logos).
+ Carpeta "Scripts": contiene los archivos. R para el procesamiento y visualización de datos.
+ Carpeta "Documentación adicional" : Contiene el plan de análisis.
+ Proyecto en R Studio, denominado: “UC-IRAG-Grupo 4.Rproj”
+ Archivo Quarto (.qmd): “Reporte UC-IRAG Notti.qmd”
+ Documento de salida en formato HTML: “Reporte UC-IRAG Notti.html”
+ README.md: documento que describe el contenido del repositorio en GitHub

## **Descripción del contenido**

**1-Carpeta "Data y plantillas"**

Contiene las bases de datos que sirven de input para el reporte epidemiológico:

+ Base nominal de IRAG e IRAGe. Por tratarse de datos sensibles, esta base no se incluye en el repositorio remoto del proyecto.

+ Base agrupada de internaciones por semana epidemiológica, utilizada como input para el script `Graficoagrupado.R`.

+ Efectores Mendoza: Contiene datos de identificación y coordenadas geográficas de los establecimientos centinela de la provincia. Esta base funciona como input del script `mapa_efectores.R`.

+ Archivo .css: Define el estilo (tipografía, colores, alineación de texto) del reporte epidemiológico.

+ Imágenes .png

**2- Carpeta "Scripts"**

Contiene los scripts presentados a continuación:

+ `librerias.R`: Contiene las librerías que se utilizarán para procesar los datos.

+ `importacion_base_nominal.R`: Carga en el entorno de trabajo la base de datos nominal de eventos. 

+ `Establecimiento.R`: Define el nombre del establecimiento en el que funciona la UC-IRAG y sobre el que se hará el análisis. Puede modificarse manualmente.

+ `mapa_efectores.R`: Mediante la ejecución de este código, se obtiene una mapa interactivo en el que se georreferencian las unidades centinela de IRAG e IRAGe de la provincia de Mendoza.  
 
+ `casos_se.R`: Código para obtener una curva epidemiológica en la que se reperesentan los casos de IRAG e IRAGe por SE. Para realizar este gráfico, se excluyen los "Casos invalidados por epidemiología"

+ `grafico_interactivo_virus.R`: Código para obtener una curva de determinaciones positivas por semana epidemiológica para los siguientes virus: Influenza, Virus Sincial Respiratorio (VSR) Y Sars- Cov -2. 
Permite visualizar la distribución temporal de los virus respiratorios en internaciones hospitalarias.
 
+ `grafico_interactivo_virusedad.R`: Código para obtener un gráfico de barras horizontales en el que se muestra la distribución por grupo etario de las determinaciones positivas para los virus mencionados anteriormente. El gráfico permite visualizar rápidamente la distribución de virus respiratorios según grupo de edad.

+ `tabla_comorbilidad.R`: Código para generar una tabla en la que se representan las 10 comorbilidades más frecuentemente notificadas en el SNVS 2.0. Para su mejor legibilidad, se recodifica la escritura de las comorbilidades.

+ `grafico_evolucion_UTIP.R`: Código para generar un gráfico de barras en el que se muestra el % de requirimiento de UTIP en casos de IRAG e IRAGe. 

+ `vacunaVSR.R`:Código para obtener un gráfico de barras en el que se representa los antecedentes de vacunación materna contra VSR en los casos notificados de IRAG e IRAGe en menores de 6 meses.

+ `Graficoagrupado.R`: Este código utiliza como input la base de datos agrupada de hospitalizaciones y genera un gráfico de doble eje. 
En este gráfico se presentan con columnas apiladas las internaciones por IRAG y otras causas por semana epidemiológica y con un gráfico de línea el porcentaje de internaciones por IRAG sobre todas las causas.
El resultado permite visualizar tanto la carga absoluta como la proporción de IRAG en las internaciones hospitalarias.  


**3- Carpeta "Documentación adicional"**

Contiene el "Plan de análisis", documento que guía las decisiones metodológicas. En el mismo, se definen las fuentes de datos, las variables incluidas en el análisis y el análisis estadístico. 
Sirve como referencia para interpretar cada elemento generado en el proyecto.

**4- Reporte "UC-IRAG Notti.qmd"**

Este archivo genera un reporte automatizado que contiene código (chunks),texto,tablas,gráficos y mapa. Es el producto final que se obtiene luego de realizar todo el procesamiento de los datos disponibles.


# **Generación y exportación del reporte automatizado**

Para obtener un reporte epidemiológico o actualizarlo la versión vigente se proponen los siguientes pasos:

1- Copiar la base nominal de eventos respiratorios y la base agrupada de internaciones a la carpeta "Datos y plantillas"

2- Abrir el proyecto de R "UC_IRAG_GRUPO4.Rproj".

3- Una vez abierto el proyecto, abrir el archivo "UC-IRAG Notti.qmd".

4- Hacer click en "Render" (flecha horizontal azul) en RStudio.

5- El documento obtenido se guarda como "Reporte UC-IRAG Notti.html".










 






















