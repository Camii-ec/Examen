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

rm <- chilemapas::mapa_comunas %>% 
  filter(codigo_region == 13)

chile <- chilemapas::generar_regiones()
names(chile)[1] <- "region"

names(rm)[1] <- "comuna"

rm %>% ggplot() +
  geom_sf(aes(geometry = geometry))


# Variabless útiles -------------------------------------------------------

datos1 <- datos[,c("folio",
                   "region",
                   "comuna",
                   "numper", #personas en el hogar sin contar a la nana
                   "asiste", #¿asiste?
                   "esc", #escolaridad
                   "educ", #nivel educacional
                   "depen", #dependencia educacional
                   "activ", #condición de actividad con respecto al trabajo
                   "indmat", #cómo está la casa
                   "indsan", #saneamiento
                   "iae", #allegado externo
                   "iai", #allegado interno
                   "hacinamiento",
                   "ytotcorh", #ingreso total del hogar corregido
                   "pobreza", #pobreza por ingresos
                   "pobreza_multi_4d", #pobreza 4-dimensional
                   "pobreza_multi_5d", #pobreza 5-dimensional
                   "dau", #decil autónomo nacional
                   "dautr")] #decil autónomo regional


# Pobreza por región ------------------------------------------------------

datos2 <- datos1[is.na(datos$pobreza)==FALSE,] %>% 
  mutate(region = as.character(region),
         comuna = as.character(comuna)) 
  
datos2$region <- recode(datos2$region, "1" = "01", "2" = "02", "3" = "03", 
                        "4" = "04", "5" = "05", "6" = "06", "7" = "07", 
                        "8" = "08", "9" = "09")

personas <- datos2 %>% count(region)

p.region <- datos2 %>% count(region, pobreza) 

p.region <- left_join(p.region, personas, by = "region") %>% 
  mutate(prop = n.x/n.y*100) %>% 
  dplyr::select(region, pobreza, prop)

# Pobres extremos 
left_join(chile, p.region[which(p.region$pobreza==1),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))

# Pobres no extremos
left_join(chile, p.region[which(p.region$pobreza==2),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))

# Millonarios
left_join(chile, p.region[which(p.region$pobreza==3),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))


# Índice de actividad por región ------------------------------------------

datos3 <- datos1[is.na(datos$activ)==FALSE,] %>% 
  mutate(region = as.character(region),
         comuna = as.character(comuna)) 

datos3$region <- recode(datos3$region, "1" = "01", "2" = "02", "3" = "03", 
                        "4" = "04", "5" = "05", "6" = "06", "7" = "07", 
                        "8" = "08", "9" = "09")

personas <- datos3 %>% count(region)

p.region <- datos3 %>% count(region, activ) 

p.region <- left_join(p.region, personas, by = "region") %>% 
  mutate(prop = n.x/n.y*100) %>% 
  dplyr::select(region, activ, prop)

# Ocupados 
left_join(chile, p.region[which(p.region$activ==1),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))

# Desocupados
left_join(chile, p.region[which(p.region$activ==2),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))

# Inactivos
left_join(chile, p.region[which(p.region$activ==3),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))


# Ingresos por región -----------------------------------------------------

datos4 <- datos1 %>% distinct(folio, .keep_all = TRUE) %>% 
  mutate(region = as.character(region),
         comuna = as.character(comuna)) 

datos4$region <- recode(datos4$region, "1" = "01", "2" = "02", "3" = "03", 
                        "4" = "04", "5" = "05", "6" = "06", "7" = "07", 
                        "8" = "08", "9" = "09")

p.region <- datos4 %>% group_by(region) %>% 
  summarise(media = mean(ytotcorh), mediana = median(ytotcorh))

# Ingreso medio del hogar
left_join(chile, p.region) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = media)) + 
  coord_sf(xlim = c(-77, -65))

# Mediana del ingreso por hogar
left_join(chile, p.region) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = mediana)) + 
  coord_sf(xlim = c(-77, -65))

## ¿Qué pasa en la rm? ----

datos4rm <- datos4 %>% 
  filter(region=="13")

p.rm <- datos4rm %>% group_by(comuna) %>% 
  summarise(media = mean(ytotcorh), mediana = median(ytotcorh))

# Ingreso medio del hogar
left_join(rm, p.rm) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = media))

# Mediana del ingreso por hogar
left_join(rm, p.rm) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = mediana))

# Nivel educacional por región --------------------------------------------

datos5 <- datos1[is.na(datos$educ)==FALSE,] %>% 
  mutate(region = as.character(region),
         comuna = as.character(comuna)) %>% 
  filter(educ != 99)

datos5$region <- recode(datos5$region, "1" = "01", "2" = "02", "3" = "03", 
                        "4" = "04", "5" = "05", "6" = "06", "7" = "07", 
                        "8" = "08", "9" = "09")

personas <- datos5 %>% count(region)

p.region <- datos5 %>% count(region, educ) 

p.region <- left_join(p.region, personas, by = "region") %>% 
  mutate(prop = n.x/n.y*100) %>% 
  dplyr::select(region, educ, prop)

# Sin educación 
left_join(chile, p.region[which(p.region$educ==0),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))

# Básica incompleta
left_join(chile, p.region[which(p.region$educ==1),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))

# Básica completa
left_join(chile, p.region[which(p.region$educ==2),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))

# M. humanista incompleta
left_join(chile, p.region[which(p.region$educ==3),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))

# M. TP incompleta
left_join(chile, p.region[which(p.region$educ==4),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))

# M. humanista completa
left_join(chile, p.region[which(p.region$educ==5),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))

# M. TP completa
left_join(chile, p.region[which(p.region$educ==6),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))

# Técnico incompleto
left_join(chile, p.region[which(p.region$educ==7),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))

# Técnico completo
left_join(chile, p.region[which(p.region$educ==8),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))

# Profesional incompleto 
left_join(chile, p.region[which(p.region$educ==9),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))

# Postgrado incompleto
left_join(chile, p.region[which(p.region$educ==10),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))

# Profesional completo
left_join(chile, p.region[which(p.region$educ==11),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))

# Postgrado completo 
left_join(chile, p.region[which(p.region$educ==12),]) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65))


# Análisis factorial ------------------------------------------------------

library(psych)
library(GPArotation)

datitos <- datos[,c("numper", #personas en el hogar sin contar a la nana
                   "esc", #escolaridad
                   "educ", #nivel educacional
                   "depen", #dependencia educacional
                   "activ", #condición de actividad con respecto al trabajo
                   "indmat", #cómo está la casa
                   "hacinamiento",
                   "pobreza", #pobreza por ingresos
                   "pobreza_multi_4d", #pobreza 4-dimensional
                   "pobreza_multi_5d", #pobreza 5-dimensional
                   "dau" #decil autónomo nacional
                   )]

datazos <- na.omit(datitos)

CorPears <- cor(datazos)

corrplot::corrplot(CorPears)

mod_Pears1 <- factanal(factors = 5, covmat = CorPears, n.obs = nrow(datazos),
                       rotation = "bifactorT")

mod_P <- fa(r = CorPears,
            nfactors = 5,
            rotate = "none",
            n.obs = nrow(datazos),
            fm = "ml")

lambdas <- eigen(CorPears)$values[1]
e <- eigen(CorPears)$vectors[,1]

L <- sqrt(lambdas)*e
l2 <- colSums(L^2)
psi <- diag(rep(1,10)-L^2)

S <- L%*%t(L)+psi

corrplot::corrplot(S)

