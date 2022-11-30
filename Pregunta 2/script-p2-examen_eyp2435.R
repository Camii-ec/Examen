# Librerias
library(rio)
library(tidyverse)
library(tidyr)
library(corrplot)
library(ggforce)
library(scales)
library(raster)
library(rworldxtra)
library(sf)

# Carga de datos
datos = rio::import(file.choose())
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


# Demografía --------------------------------------------------------------

datos %>% filter(pobreza%in% c(1,2)) %>% 
  ggplot(aes(as.factor(region), fill = as.factor(pobreza))) + 
  geom_bar(position = "dodge")

rm <- chilemapas::mapa_comunas %>% 
  filter(codigo_region == 13)

names(rm)[1] <- "comuna"

rm %>% ggplot() +
  geom_sf(aes(geometry = geometry))


# Variables interesantes educación ----------------------------------------------------

educacion <- datos %>% 
  dplyr::select(-c(folio:hogar, expr:fecha_año)) %>% 
  filter(region == "13") %>% 
  dplyr::select(comuna, e1:e0) 


##e1: sabe leer y escribir (1-sí, 2-lee, 3-escribe, 4-ninguno)
##e6a: nivel educacional más alto o actual(1-ninguno, 2-sala cuna, 3-jardín, 4-kinder, 5-educación diferencial, 6-primaria (sistema antiguo), 7-básica, 8-humanidades (sistema antiguo), 9-media, 10-técnica(s.a), 11-tp, 12-técnico incompleto, 13-técnico, 14-profesional incompleto, 15-profesional, 16-postgrado incompleto, 17-postgrado)

educacion <- educacion[is.na(educacion$e1)==FALSE,] %>% 
  dplyr::select(comuna, e1, e6a) %>% 
  filter(e1 != 9, e6a != 99) 
# Eliminamos personas que no responden/no saben

cuenta.e1 <- educacion %>% count(comuna, e1)
personas.comuna <- educacion %>% count(comuna) %>% 
  mutate(comuna = as.character(comuna))

e1.1 <- cuenta.e1 %>% 
  filter(e1 == 1) %>% 
  mutate(comuna = as.character(comuna)) %>% 
  dplyr::select(comuna, n)

e1.1 <- left_join(e1.1, personas.comuna, by = "comuna")

e1.1 <- e1.1 %>% mutate(prop.e1 = n.x/n.y) %>% 
  dplyr::select(comuna, prop.e1)

e1.2 <- educacion %>% 
  filter(e1 != 1) %>% 
  mutate(comuna = as.character(comuna)) %>% 
  count(comuna) 

e1.2 <- left_join(e1.2, personas.comuna, by = "comuna")
e1.2 <- e1.2 %>% mutate(prop.analfabetismo = n.x/n.y) %>% 
  dplyr::select(comuna, prop.analfabetismo)

# Proporción de personas que leen por comuna
left_join(rm, e1.1, by = "comuna") %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop.e1*100))

# Proporción de personas que presentan algún grado de analfabetismo por comuna
left_join(rm, e1.2, by = "comuna") %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop.analfabetismo*100))

e6a <- educacion %>% 
  mutate(comuna=as.character(comuna), e6a = as.factor(e6a), e1 = as.factor(e1))

pro.o.mas <- e6a %>% 
  filter(e6a %in% c("15", "16, 17")) %>% 
  count(comuna)

pro.o.mas <- left_join(pro.o.mas, personas.comuna, by = "comuna") %>% 
  mutate(prop = n.x/n.y) %>% 
  dplyr::select(comuna, prop)

# Proporción de personas con nivel de educación profesional o superior según comuna
left_join(rm, pro.o.mas, by = "comuna") %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop*100))

## Comuna y nivel de educación podrían ser interesantes para ver qué pasa con el nivel de ingresos o pobreza

# Relación con variables de trabajo ------------------------------------------

trabajo <- datos %>% 
  dplyr::select(-c(folio:hogar, expr:fecha_año)) %>% 
  filter(region == "13") %>% 
  dplyr::select(comuna, e1, e6a, ch1, y1, ch2, y6, o6, o8) %>% 
  mutate(comuna = as.character(comuna), e6a = as.factor(e6a))

## chi1: 1-asalariado, 2-emprendedor, 3-inactivo o cesante 4-muy chico
## y1: sueldo líquido mes pasado
## ch2: 1-tiene otra pega, 2-no tiene nada más
## y6: sueldo segunda pega
## o6: buscó trabajo o intentó iniciar algo propio en el último mes (1-sí, 2-no)
## o8: cuántas semanas buscó o ha buscado pega (999-no sabe/no responde)

# Fuerza laboral por comuna (no dejaron en blanco o1)
p.comuna <- trabajo[is.na(trabajo$o1)==FALSE & is.na(trabajo$e1)==FALSE,] %>% 
  filter(e1!=9, e6a != 99) %>% 
  count(comuna)

asalariados <- trabajo[is.na(trabajo$e1)==FALSE, 1:7] %>% 
  filter(ch1 == 1, e1 !=9, e6a !=99) %>% 
  mutate(sueldo_total = ifelse(is.na(y6)==FALSE, y1+y6, y1),
         trabajos = ifelse(ch2==1, 2, 1)) %>% 
  dplyr::select(-c(ch1,ch2,y1,y6)) 

asalariados %>% ggplot(aes(e6a)) +
  geom_bar()

asalariados %>% ggplot(aes(e6a, sueldo_total)) + 
  geom_boxplot() 

sueldos <- asalariados %>% group_by(e6a) %>% 
  summarise(media = mean(sueldo_total), mediana = median(sueldo_total))

sueldos %>% ggplot(aes(e6a, mediana)) +
  geom_col()

sueldos %>% ggplot(aes(e6a, media)) +
  geom_col()


  