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

datos1 <- datos[,c("folio",
                   "region",
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
                   "ytotcorh", #ingreso total del hogar corregido
                   "pobreza", #pobreza por ingresos
                   "pobreza_multi_4d", #pobreza 4-dimensional
                   "pobreza_multi_5d" #pobreza 5-dimensional
                   )] #decil autónomo regional


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

# Regresión ---------------------------------------------------------------

str(muerte)
names(muerte)

muerte <- na.omit(datos1)

muerte$region <- as.factor(muerte$region)
muerte$comuna <- as.factor(muerte$comuna)
muerte$esc <- as.factor(muerte$esc)
muerte$educ <- as.factor(muerte$educ)
muerte$depen <- as.factor(muerte$depen)
muerte$activ <- as.factor(muerte$activ)
muerte$indmat <- as.factor(muerte$indmat)
muerte$indsan <- as.factor(muerte$indsan)
muerte$hacinamiento <- as.factor(muerte$hacinamiento)
muerte$pobreza <- as.factor(muerte$pobreza)
muerte$pobreza_multi_4d <- as.factor(muerte$pobreza_multi_4d)
muerte$pobreza_multi_5d <- as.factor(muerte$pobreza_multi_5d)
muerte$dau <- as.factor(muerte$dau)
muerte$dautr <- as.factor(muerte$dautr)
muerte$iai <- as.factor(muerte$iai)
muerte$iae <- as.factor(muerte$iae)

muerte$numper <- as.numeric(muerte$numper)
muerte$ytotcorh <- as.numeric(muerte$ytotcorh)

destruccion <- fastDummies::dummy_columns(muerte)[,-c(1:3, 5:14, 16:20)]

## Regresión con múltiples respuestas ----

respuesta.region <- function(datos){
  y <- 1:500
  aux <- datos %>% group_by(region)
  
  for(reg in 1:16){
    y <- cbind(y, as.numeric(datos$ytotcorh[which(aux$region == as.character(reg))]))
  }
  
  return(y[,-1])
}

muestra <- function(datos){
  datitos <- matrix(1, nrow = 1, ncol = ncol(datos))
  datos <- datos %>% group_by(region)
  
  for(reg in 1:16){
    aux <- datos %>% filter(region==as.character(reg)) %>% 
      as.matrix()
    n <- sample(1:nrow(aux), 500)
    datitos <- rbind(datitos, aux[n,])
  }
  
  names(datitos) <- names(datos)
  
  return(datitos[-1,])
}

panico <- muestra(muerte)[,-1]

Y <- respuesta.region(data.frame(panico))

agonia <- data.frame(panico[,-13])
agonia$numper <- as.numeric(agonia$numper)

m.diseño <- function(datos){
  x <- list()
  
  for(reg in 1:16){
    aux <- datos %>% 
      filter(region == as.character(reg)) %>% 
      dplyr::select(-region) %>% 
      fastDummies::dummy_columns(remove_first_dummy = TRUE) %>% 
      dplyr::select(-c(comuna, esc:pobreza_multi_5d)) %>% 
      as.matrix()
    
    x[[reg]] <- aux
  }
  
  return(x)
}

X <- m.diseño(agonia)

correlaciones <- function(x, y){
  for(i in 1:16){
    cor <- cor(cbind(x[[i]],Y[,]))
  }
}

regresiones <- function(y, x, step = FALSE){
  modelos <- list(NA)
  for(i in 1:16){
    aux <- x[[i]]
    modelos[[i]] <- lm(y[,i] ~ aux)
  }
  
  if(step == TRUE){
    for(i in 1:16){
      modelos[[i]] <- step(lm(y[,i] ~ aux))
    }
  }
  
  return(modelos)
}

desesperacion <- regresiones(Y, X)

summary(desesperacion[[1]])

jlo <- regresiones(Y, X, step = TRUE)
summary(jlo[[1]])


# Análisis factorial -----------------------------------------------

library(GPArotation)

datitos <- datos %>% 
  dplyr::select(region, comuna, r1a, educ, depen, 
         indsan, iae, iai, hacinamiento, calglobviv, ypchautcor) %>% 
  na.omit()

X <- datitos$ypchautcor

datitos <- scale(datitos[,-11])

cor <- cor(datitos)
corrplot::corrplot(cor)

mod_Pears1 <- factanal(factors = 3, covmat = cor, n.obs = nrow(datitos))

datazos <- datos %>% 
  dplyr::select(region, comuna, hh_d_asis:hh_d_seg) %>% 
  na.omit()

cor <- cor(scale(datazos))
corrplot::corrplot(cor)

mod_Pears2 <- factanal(factors = 3, covmat = cor, n.obs = nrow(datazos))


yanomas <- datos %>% 
  dplyr::select(calglobviv, hh_d_habitab:hh_d_estado, 
                hh_d_entorno:hh_d_medio, ypchautcor) %>% 
  na.omit()
X <- yanomas$ypchautcor
yanomas <- yanomas[,-8]

cor <- cor(yanomas)
corrplot::corrplot(cor)

mod_Pears3 <- factanal(factors = 3, covmat = cor, n.obs = nrow(yanomas))



# Educación ---------------------------------------------------------------

educacion <- datos %>% 
  dplyr::select( e1:e0, -c(e4_esp, e5a_esp, e13b1_esp, e13b2_esp, e7, 
                           e9nom, e9dir, e9com, e18a))


mayores15 <- educacion[is.na(educacion$e1)==FALSE,]  %>% 
  dplyr::select(-e4, -e16, -c(e14pbu:e14pbm)) 

mayores15[is.na(mayores15)] <- 0 ##En las casillas, 0="no aplica"

cor(mayores15)
corrplot(cor(mayores15))

# Eliminamos las correlaciones más bajas
mayores15 <- mayores15 %>% 
  dplyr::select(-c(e1, e2, e5a:e6b, e9com_c_cod, e9com_p_cod, e13b1, e13b2, e19_2, e0,
                   e18b, e7_subarea:e8, e14bu:e14bpc, e15b)) %>% 
  scale()

corrplot(cor(mayores15))

factanal(mayores15, factors = 2, rotation = "none")

menores15 <- educacion[is.na(educacion$e1)==TRUE,] %>% 
  dplyr::select(-e1, -e2, -c(e7_subarea:e8), -e9rbd_sup, -e9sup_g,
                -e12e, -c(e17:e19_2))

menores15[is.na(menores15)] <- 0

cor.men <- cor(menores15)
corrplot(cor(menores15))

factanal(menores15, factors = 3, rotation = "none")


# Componentes principales -------------------------------------------------

educacion <- datos %>% 
  dplyr::select( e1:e0, -c(e4_esp, e5a_esp, e13b1_esp, e13b2_esp, e7, 
                           e9nom, e9dir, e9com))

educacion[is.na(educacion)] <- 0

corrplot(cor(educacion))

sigma <- cor(educacion)

valp <- eigen(sigma)$values
vecp <- eigen(sigma)$vectors

plot(valp/sum(valp)) #

#test para 10 componentes

f <- sum(valp[11:length(valp)])/sum(valp)
var.f <- 2*(0.25/sum(diag(sigma)))^2*sum(valp[1:10]^2) + 
  2*(0.75/sum(diag(sigma)))^2*sum(valp[11:length(valp)]^2)
  
sqrt(length(educacion)-1)*abs(f-0.25) < qnorm(0.95)*sqrt(var.f)

# Rechazamos H0, pedemos ignorar las últimas componentes

valp <- eigen(sigma)$values[1:10]
vecp <- eigen(sigma)$vectors[,1:10]

prop.var <- valp/sum(valp)  


# Intento número 1000 -----------------------------------------------------

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
  r <- matrix(1, nrow=17)
  for(i in 1:8){
    r <- cbind(r, vecp[,i]*sqrt(valp[i]/sigma[i,i]))
  }
  
  return(r[,-1])
}

r <- correlaciones(valp, vecp, sigma)
rowSums(r^2)
