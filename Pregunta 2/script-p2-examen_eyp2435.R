# Librerias
library(rio)
library(tidyverse)
library(tidyr)
library(corrplot)

# Carga de datos
datos = rio::import("Casen 2017.dta")
View(datos)

nombres = colnames(datos)

#######
# Objetivos

# POBREZA - GRUPOS PRIORITARIOS respecto a DEMOGRAFÍA, EDUCACIÓN, SALUD, VIVIENDA, TRABAJO e INGRESOS


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

# GRUPOS PRIOTITARIOS (Ch4. Chequeo de situación de dependencia)


# FORMA BONITA PARA EL TEXTO

# Dentro de las variables de interés podemos contar con las variables que buscamos conocer del onjetivo de la encuesta. Por lo tanto las variables de interés son los ingresos ganados y grupos prioritarios (estos grupo se encuentran en diversas variables de la base de datos). Estas variables de interés también nos importa conocer su relación  con la demografía, educación, salud, vivienda, trabajo e ingresos que tienen esas personas. por lo tanto estos aspectos también serán considerados como variables de interés.











# Variable de pobreza (Y):
# pobreza_multi_4d    -    pobreza_multi_5d    -    pobreza

# Modulo Educación   43 -> 101
# Modulo Trabajo    102 -> 150
# Modulo Ingresos   151 -> 304
# Modulo Salud      305 -> 307
# Modulo Vivienda   500 -> 573

# Identificación de pobreza  767 y 784 -> 804

datos1 = datos %>% mutate(SEXO = case_when(
  sexo == 1 ~ "Hombre",
  sexo == 2 ~ "Mujer"
)) 
datos1 = datos1 %>% mutate(POBRE = case_when(
  pobreza_multi_5d == 0 ~ "No pobre",
  pobreza_multi_5d == 1 ~ "Pobre"
))

datos1 = datos1 %>% drop_na(POBRE)


datos1 %>% ggplot(aes(x = POBRE, y = ytot)) +
  geom_boxplot(fill = "#05cce5") +
  theme_bw() +
  labs(title = "Esperanza de vida según región",
       x = "Sexo",
       y = "ingresos totales")

colores = c("red", "blue")

plot(datos1$s12, 
     datos1$ytot, 
     xlim = c(0, 10),
     col = colores[factor(datos1$POBRE)],
     pch = 19)

