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

rm <- chilemapas::mapa_comunas %>% 
  filter(codigo_region == 13)

chile <- chilemapas::generar_regiones()
names(chile)[1] <- "region"

names(rm)[1] <- "comuna"

rm %>% ggplot() +
  geom_sf(aes(geometry = geometry))


# Variabless útiles -------------------------------------------------------

datos1 <- datos[,c("region",
                   "comuna",
                   "numper", #personas en el hogar sin contar a la nana
                   "esc", #escolaridad
                   "educ", #nivel educacional
                   "depen", #dependencia educacional
                   "activ", #condición de actividad con respecto al trabajo
                   "indmat", #cómo está la casa
                   "indsan", #saneamiento
                   "iae", #allegado externo
                   "iai", #allegado interno
                   "hacinamiento"
                   )] #decil autónomo regional

save(datos1, file = "Pregunta 2/DatosP2.RData")


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

# Intento número 1000 -----------------------------------------------------

load("Pregunta 2/DatosP2.RData")

datos1[is.na(datos1)] <- 0

sigma <- cov(scale(datos1))

valp <- eigen(sigma)$values
vecp <- eigen(sigma)$vectors

plot(valp/sum(valp))

#test para 8 componentes
f <- sum(valp[9:length(valp)])/sum(valp)
var.f <- 2*(0.25/sum(diag(sigma)))^2*sum(valp[1:8]^2) + 
  2*(0.75/sum(diag(sigma)))^2*sum(valp[9:length(valp)]^2)

sqrt(length(valp)-1)*abs(f-0.25) < qnorm(0.975)*sqrt(var.f)
#rechazamos H0, podemos usar 8 componentes 

valp <- eigen(sigma)$values[1:8]
vecp <- eigen(sigma)$vectors[,1:8]

prop.var <- valp/sum(valp)

plot(prop.var)

cumsum(prop.var)

correlaciones <- function(valp, vecp, sigma){
  r <- matrix(1, nrow=12)
  for(i in 1:8){
    r <- cbind(r, vecp[,i]*sqrt(valp[i]/sigma[i,i]))
  }
  
  return(r[,-1])
}

r <- correlaciones(valp, vecp, sigma)
rowSums(r^2)
