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
library(ggrepel)
library(vcd)

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
                   "hacinamiento",
                   "pobreza",
                   "ypchautcor"
)] #decil autónomo regional

save(datos1, file = "DatosP2.RData")


# Pobreza por región ------------------------------------------------------

datos2 <- datos1 %>% 
  mutate(region = as.character(region),
         comuna = as.character(comuna)) 

datos2$pobreza[is.na(datos2$pobreza)] = 0

datos2$region <- recode(datos2$region, "1" = "01", "2" = "02", "3" = "03", 
                        "4" = "04", "5" = "05", "6" = "06", "7" = "07", 
                        "8" = "08", "9" = "09")

personas <- datos2 %>% count(region)

p.region <- datos2 %>% count(region, pobreza) 

p.region <- left_join(p.region, personas, by = "region") %>% 
  mutate(prop = n.x/n.y*100) %>% 
  dplyr::select(region, pobreza, prop)


# Pobres extremos 
left_join(chile, p.region[which(p.region$pobreza == 1),]) %>%
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  col = "black", size = 3, max.overlaps = 22) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "green", high = "brown", na.value = NA)

# Pobres no extremos
left_join(chile, p.region[which(p.region$pobreza==2),]) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  col = "black", size = 3, max.overlaps = 22) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "green", high = "brown", na.value = NA)


# Millonarios
left_join(chile, p.region[which(p.region$pobreza==3),]) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  col = "black", size = 3, max.overlaps = 22) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "green", high = "brown", na.value = NA)


# Índice de actividad por región ------------------------------------------

datos3 <- datos1 %>% 
  mutate(region = as.character(region),
         comuna = as.character(comuna)) 

datos3$activ[is.na(datos3$activ)] = 0

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
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "blue", high = "orange", na.value = NA)

# Desocupados
left_join(chile, p.region[which(p.region$activ==2),]) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "orange", high = "blue", na.value = NA)

# Inactivos
left_join(chile, p.region[which(p.region$activ==3),]) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA)


# Ingresos por región -----------------------------------------------------

datos4 <- datos1 %>% 
  mutate(region = as.character(region),
         comuna = as.character(comuna)) 

datos4$region <- recode(datos4$region, "1" = "01", "2" = "02", "3" = "03", 
                        "4" = "04", "5" = "05", "6" = "06", "7" = "07", 
                        "8" = "08", "9" = "09")

p.region <- datos4 %>% group_by(region) %>% 
  summarise(media = mean(ypchautcor), mediana = median(ypchautcor))

# Ingreso medio del hogar
left_join(chile, p.region) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = media)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "white", high = "darkgreen", na.value = NA)

# Mediana del ingreso por hogar
left_join(chile, p.region) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = mediana)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "white", high = "darkgreen", na.value = NA)

## ¿Qué pasa en la rm? ----

datos4rm <- datos4 %>% 
  filter(region=="13")

p.rm <- datos4rm %>% group_by(comuna) %>% 
  summarise(media = mean(ypchautcor), mediana = median(ypchautcor))

# Ingreso medio del hogar
left_join(rm, p.rm) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = media)) + 
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "white", high = "darkgreen", na.value = NA)

# Mediana del ingreso por hogar
left_join(rm, p.rm) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = mediana)) + 
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "white", high = "darkgreen", na.value = NA)

# Nivel educacional por región --------------------------------------------

datos5 <- datos1 %>% 
  mutate(region = as.character(region),
         comuna = as.character(comuna)) %>% 
  filter(educ != 99)

datos5$educ[is.na(datos5$educ)] = 0

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
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "blue", high = "darkorange", na.value = NA)


# Básica incompleta
left_join(chile, p.region[which(p.region$educ==1),]) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "blue", high = "darkorange", na.value = NA)

# Básica completa
left_join(chile, p.region[which(p.region$educ==2),]) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(high = "blue", low = "darkorange", na.value = NA)

# M. humanista incompleta
left_join(chile, p.region[which(p.region$educ==3),]) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "blue", high = "darkorange", na.value = NA)

# M. TP incompleta
left_join(chile, p.region[which(p.region$educ==4),]) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "blue", high = "darkorange", na.value = NA)

# M. humanista completa
left_join(chile, p.region[which(p.region$educ==5),]) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(high = "blue", low = "darkorange", na.value = NA)

# M. TP completa
left_join(chile, p.region[which(p.region$educ==6),]) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(high = "blue", low = "darkorange", na.value = NA)

# Técnico incompleto
left_join(chile, p.region[which(p.region$educ==7),]) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "blue", high = "darkorange", na.value = NA)

# Técnico completo
left_join(chile, p.region[which(p.region$educ==8),]) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(high = "blue", low = "darkorange", na.value = NA)

# Profesional incompleto 
left_join(chile, p.region[which(p.region$educ==9),]) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "blue", high = "darkorange", na.value = NA)

# Postgrado incompleto
left_join(chile, p.region[which(p.region$educ==10),]) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "blue", high = "darkorange", na.value = NA)

# Profesional completo
left_join(chile, p.region[which(p.region$educ==11),]) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(high = "blue", low = "darkorange", na.value = NA)

# Postgrado completo 
left_join(chile, p.region[which(p.region$educ==12),]) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         coords_x = map_dbl(coords, 1), 
         coords_y = map_dbl(coords, 2)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  coord_sf(xlim = c(-77, -65)) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = region),
                  max.overlaps = 49) +
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(high = "blue", low = "darkorange", na.value = NA)


# analisis region metropolitana -------------------------------------------
p.com <- datos5 %>% filter(region == 13) %>% count(comuna, educ) 

personas <- datos3 %>% filter(region == 13) %>% count(comuna)

p.com <- left_join(p.com, personas, by = "comuna") %>% 
  mutate(prop = n.x/n.y*100) %>% 
  dplyr::select(comuna, educ, prop)

p.com = p.com %>% filter(educ == 12)

left_join(rm, p.com) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) + 
  theme(axis.text.x = element_blank(), # Eliminar ejes
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradient(low = "darkorange", high = "blue", na.value = NA)


# Intento número 1000 -----------------------------------------------------

load("DatosP2.RData")

datos1[is.na(datos1)] <- 0


## Para balancear la muestra, consideramos a 20 mil personas no pobres al azar y 20 mil personas pobres (extremas y no extremas) al azar.
muestra <- function(datos){
  pobres <- which(datos$pobreza %in% c(1,2))
  nopobres <- which(datos$pobreza==3)
  
  n1 <- sample(pobres, 20000)
  n2 <- sample(nopobres, 20000)
  n <- sort(c(n1,n2))
  
  datitos <- datos[n,]
  
  return(datitos)
}

datazo <- muestra(datos1)

ingreso <- datazo[,14]
pobreza <- recode(datazo[,13], `1` = "1", `2` = "1", `3` = "2")
salmon <- datazo[,-c(13,14)]

sigma <- cor(scale(salmon))

corrplot::corrplot(sigma)


valp <- eigen(sigma)$values
vecp <- eigen(sigma)$vectors

prop.var <- valp/sum(valp)

cbind(Factores = 1:12,
      Varianzas = prop.var) %>%
  as.data.frame() %>%
  ggplot()+
  theme_bw() +
  geom_point(aes(Factores, Varianzas)) +
  scale_x_continuous(breaks = c(1:12),
                     labels = 1:12)

## Queremos explicar entre un 70% y un 90% de la varianza, así que vemos con cuántos componentes logramos eso
cbind(Factores = 1:12,
      Varianzas = cumsum(prop.var)) %>%
  as.data.frame() %>%
  ggplot()+
  theme_bw() +
  geom_point(aes(Factores, Varianzas)) +
  scale_x_continuous(breaks = c(1:12),
                     labels = 1:12) +
  geom_hline(yintercept = 0.70, col = "red", lwd = 1) +
  geom_hline(yintercept = 0.90, col = "red", lwd = 1) +
  geom_segment(x = 8, y = 0,
               xend = 8, yend = cumsum(prop.var)[8],
               color = "blue",
               linetype = 2,
               lwd = 1) +
  geom_segment(x = 0, y = cumsum(prop.var)[8],
               xend = 8, yend = cumsum(prop.var)[8],
               color = "blue",
               linetype = 2,
               lwd = 1)+
  geom_text(x = 8.5, 
            y = cumsum(prop.var)[8], 
            label = round(cumsum(prop.var)[8], 2),
            col = "blue", size = 2)

# Así, nos quedamos únicamente con 8
valp <- eigen(sigma)$values[1:8]
vecp <- eigen(sigma)$vectors[,1:8]

prop.var <- valp/sum(valp)

# varianza de clada factor
cbind(Factores = 1:8,
      Varianzas = prop.var) %>%
  as.data.frame() %>%
  ggplot()+
  theme_bw() +
  geom_point(aes(Factores, Varianzas)) +
  scale_x_continuous(breaks = c(1:8),
                     labels = 1:8)

# varianza acumulada de cada factor
cbind(Factores = 1:8,
      Varianzas = cumsum(prop.var)) %>%
  as.data.frame() %>%
  ggplot()+
  theme_bw() +
  geom_point(aes(Factores, Varianzas)) +
  scale_x_continuous(breaks = c(1:8),
                     labels = 1:8)

correlaciones <- function(valp, vecp, sigma){
  r <- matrix(1, nrow=12)
  for(i in 1:8){
    r <- cbind(r, vecp[,i]*sqrt(valp[i]/sigma[i,i]))
  }
  
  return(r[,-1])
}

r <- correlaciones(valp, vecp, sigma)
rownames(r) <- colnames(datos1)[-c(13,14)]
colnames(r) <- paste0("r", 1:8)

data.frame(r) %>% ggplot(aes(x=1:12)) +
  geom_line(aes(y=r1, color = "r1"), size = 1.5) +
  geom_line(aes(y=r2, color = "r2"), size = 1.5) +
  geom_line(aes(y=r3, color = "r3"), size = 1.5) +
  geom_line(aes(y=r4, color = "r4"), size = 1.5) +
  geom_line(aes(y=r5, color = "r5"), size = 1.5) +
  geom_line(aes(y=r6, color = "r6"), size = 1.5) +
  geom_line(aes(y=r7, color = "r7"), size = 1.5) +
  geom_line(aes(y=r8, color = "r8"), size = 1.5) +
  theme_bw() +
  labs(title = "Correlaciones de las componentes principales con las variables",
       color = "Componentes") +
  scale_color_manual(values = rainbow(8)) +
  scale_x_continuous(breaks = c(1:12),
                     labels = 1:12)

## Varianza explicada para cada variable
var_exp = as.data.frame(rowSums(r^2))
var_exp = var_exp %>%
  mutate(variable = rownames(var_exp))
colnames(var_exp) = c("suma", "var")

var_exp %>%
  ggplot(aes(x = var, y = suma))+
  geom_bar(stat = "identity", position = "stack", width=0.5, fill = "purple") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

a <- as.matrix(salmon)%*%vecp
suma <- rowSums(a)

plot(suma)

b <- data.frame(a, pobreza)

c = data.frame(b) %>%
  mutate(pobreza = case_when(
    pobreza == "1" ~ 1,
    pobreza == "2" ~ 0
  ))
names(c)


# Regresiones -------------------------------------------------------------


# Regresión logística
modelo_glm <- glm(pobreza ~ ., 
                  data = c,
                  family = "binomial")
primodenoruega <- summary(modelo_glm)[["coefficients"]]

int_conf = confint(modelo_glm)

predicciones <- ifelse(test = modelo_glm$fitted.values > 0.5, yes = 1, no = 0)
matriz_confusion <- table(modelo_glm$model$pobreza, predicciones,
                          dnn = c("observaciones", "predicciones"))
mosaic(matriz_confusion, shade = TRUE, colorize = TRUE,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

# Regresión lineal sobre ingresos
d = data.frame(cbind(a, ingreso))

modelo_lm = lm(ingreso ~.,
               data = d)
pasos = step(object = modelo_lm, direction = "both", trace = 1)

# Generamos otros modelos posibles
{
  modelo_lm1 = lm(ingreso ~ .-V1,
                  data = d)
  modelo_lm2 = lm(ingreso ~ .-V2,
                  data = d)
  modelo_lm3 = lm(ingreso ~ .-V3,
                  data = d)
  modelo_lm4 = lm(ingreso ~ .-V4,
                  data = d)
  modelo_lm5 = lm(ingreso ~ .-V5,
                  data = d)
  modelo_lm6 = lm(ingreso ~ .-V6,
                  data = d)
  modelo_lm7 = lm(ingreso ~ .-V7,
                  data = d)
  modelo_lm8 = lm(ingreso ~ .-V8,
                  data = d)
}


AICS = c(AIC(modelo_lm),
         AIC(modelo_lm1),
         AIC(modelo_lm2),
         AIC(modelo_lm3),
         AIC(modelo_lm4),
         AIC(modelo_lm5),
         AIC(modelo_lm6),
         AIC(modelo_lm7),
         AIC(modelo_lm8))

compara = cbind(modelo = c("Modelo completo", 
                           "Modelo -primer comp",
                           "Modelo -segundo comp",
                           "Modelo -tercero comp",
                           "Modelo -cuarto comp",
                           "Modelo -quinto comp",
                           "Modelo -sexto comp",
                           "Modelo -septimo comp",
                           "Modelo -octavo comp"), 
                aic = round(AICS, 0)) %>%
  as.data.frame()

compara$modelo = factor(compara$modelo, 
                        levels = c("Modelo completo", 
                                   "Modelo -primer comp",
                                   "Modelo -segundo comp",
                                   "Modelo -tercero comp",
                                   "Modelo -cuarto comp",
                                   "Modelo -quinto comp",
                                   "Modelo -sexto comp",
                                   "Modelo -septimo comp",
                                   "Modelo -octavo comp"))

compara %>%
  ggplot()+
  geom_bar(aes(x = modelo, y = aic), stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# Se muestra que usar los 8 componentes generan un mayor AIC, y comparamos los coeficientes con los del modelo inicial, vemos que son los mismos
pasos$coefficients
modelo_lm$coefficients



summary(modelo_lm)
confint(modelo_lm)


ggplot(data = d, aes(x = V8, y = modelo_lm$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick") +
  geom_hline(yintercept = 0) +
  theme_bw()

qqnorm(modelo_lm$residuals)
qqline(modelo_lm$residuals, col = "red", lwd = 2)

