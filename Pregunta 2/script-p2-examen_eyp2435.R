# Librerias
library(rio)

# Carga de datos
datos = rio::import("Casen 2017.dta")
View(datos)

nombres = colnames(datos)
nombres == "numper"

# Objetivos

# La Encuesta Casen se plantea dos objetivos fundamentales: 
#  ---Conocer periódicamente la situación de los hogares y de la población,
#  especialmente de aquélla en situación de pobreza y de aquellos grupos
#  definidos como prioritarios por la política social (infancia,juventud
#  , adultos mayores, mujeres, pueblos indígenas, personas en situación 
#  de discapacidad,   nacidos fuera de chile, entre otros),
#  principalmente con relación a aspectos demográficos, de educación, 
#  salud, vivienda, 
#  trabajo e ingresos. En particular, estimar la magnitud de la pobreza 
#  y la
#  distribución del ingreso; identificar carencias y demandas de la   
#  población en las áreas señaladas; y evaluar las distintas brechas que
#  separan a los diferentes segmentos sociales y ámbitos territoriales.



# POBREZA - GRUPOS PRIORITARIOS respecto a DEMOGRAFÍA, EDUCACIÓN, SALUD, VIVIENDA, TRABAJO e INGRESOS



#  ----Estimar la cobertura, la focalización y la distribución del gasto   fiscal de los
#  principales programas sociales de alcance nacional entre los hogares,
# según su nivel de ingreso, para evaluar el impacto de este gasto en el
#  ingreso de los hogares y en la distribución del mismo.



# Cobestura y focalizacion de los gastos fiscales de programas sosciales nacionales según ingreso hogares y su distribución


# Descripción de variables:

# pag 15 del documento?




