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



# Dentro de las variables más importantes tenemos las presentes en el objetivo que detallan la 
# demografía (Urbano o Rural, región o modulo REGIDTRO HOGARES), 
# educacion (nivel educacional o modulo de EDUCACION), 
# salud (s12. ¿A qué sistema previsional de salud pertenece usted? o de modulo SALUD), 
# vivienda (v1. ¿Cuál es el tipo de vivienda que ocupa el entrevistado? o modulo VIVIENDA Y ENTORNO), 
#que trabajo desempeña(Ch1. Chequeo de situación ocupacional. o Modulo ingreso) e 
# ingresos(ytot Ingreso Total, y totcor Ingreso total corregido).
# 
# De esto tenemos que analizar la 
# pobreza (Situación de pobreza por ingresos)
# y los grupos prioritarios (edad, r3. Pueblos indígenas, ¿pertenece usted o es descendiente de alguno de ellos?, y20e. Mes pasado Subsidio a la discapacidad mental?, r1a. ¿Cuál es la nacionalidad?)


# FORMA BONITA PARA EL TEXTO

# Dentro de las variables de interés podemos contar con las variables que buscamos conocer del onjetivo de la encuesta. Por lo tanto las variables de interés son los ingresos ganados y grupos prioritarios (estos grupo se encuentran en diversas variables de la base de datos). Estas variables de interés también nos importa conocer su relación  con la demografía, educación, salud, vivienda, trabajo e ingresos que tienen esas personas. por lo tanto estos aspectos también serán considerados como variables de interés.




