library(tidyverse)

datos <- rio::import("Pregunta 1/UNData.csv")


# Análisis exploratorio ---------------------------------------------------

## Correlaciones entre las variables continuas ----
GGally::ggpairs(datos[,-c(1:3)])

## Dos correlaciones más altas

datos %>% ggplot(aes(lifeExpF, pctUrban)) +
  geom_point(color = "#e55605") + 
  theme_bw() +
  labs(title = "Relación entre esperanza de vida y porcentaje de población urbana",
       x = "Expectativa de vida",
       y = "Población urbana")

datos %>% ggplot(aes(lifeExpF, fertility)) +
  geom_point(color = "#e55605") + 
  theme_bw() +
  labs(title = "Relación entre esperanza de vida y tasa de natalidad",
       x = "Expectativa de vida",
       y = "Fertilidad")

## Fertilidad ----

## Por grupo
datos %>% ggplot(aes(group, fertility)) +
  geom_boxplot(fill = "#05cce5") +
  theme_bw() +
  labs(title = "Tasa de natalidad según grupo",
       x = "Grupo",
       y = "Fertilidad")

## Por región
datos %>% ggplot(aes(region, fertility)) +
  geom_boxplot(fill = "#05cce5") +
  theme_bw() +
  labs(title = "Tasa de natalidad según región",
       x = "Región",
       y = "Fertilidad")

## ¿Localidades del atlántico norte?
datos %>% filter(region == "NorthAtlantic")

## Expectativa de vida ----

## Por grupo
datos %>% ggplot(aes(group, lifeExpF)) +
  geom_boxplot(fill = "#05cce5") +
  theme_bw() +
  labs(title = "Esperanza de vida según grupo",
                     x = "Grupo",
                     y = "Expectativa")

## Por región
datos %>% ggplot(aes(region, lifeExpF)) +
  geom_boxplot(fill = "#05cce5") +
  theme_bw() +
  labs(title = "Esperanza de vida según región",
       x = "Región",
       y = "Expectativa")

## Producto nacional bruto por persona ----

## Por grupo
datos %>% ggplot(aes(group, ppgdp)) +
  geom_boxplot(fill = "#05cce5") +
  theme_bw() +
  labs(title = "Producto nacional bruto por persona según grupo",
       x = "Grupo",
       y = "PPGDP")

## Por región
datos %>% ggplot(aes(region, ppgdp)) +
  geom_boxplot(fill = "#05cce5") +
  theme_bw() +
  labs(title = "Producto nacional bruto por persona según región",
       x = "Región",
       y = "PPGDP")

## Porcentaje de población urbana ----

## Por grupo
datos %>% ggplot(aes(group, pctUrban)) +
  geom_boxplot(fill = "#05cce5") +
  theme_bw() +
  labs(title = "Porcentaje de población urbana según grupo",
       x = "Grupo",
       y = "Porcentaje")

## Por región
datos %>% ggplot(aes(region, pctUrban)) +
  geom_boxplot(fill = "#05cce5") +
  theme_bw() +
  labs(title = "Porcentaje de población urbana según región",
       x = "Región",
       y = "Porcentaje")

## Correlaciones por grupo ----

datos %>% filter(group == "africa") %>% 
  select(-1,-3) %>% 
  GGally::ggpairs()

datos %>% filter(group == "oecd") %>% 
  select(-1,-3) %>% 
  GGally::ggpairs()

datos %>% filter(group == "other") %>% 
  select(-1,-3) %>% 
  GGally::ggpairs()


# Efecto de las otras variables -------------------------------------------

modelito <- lm(cbind(fertility, lifeExpF) ~ ., datos[,-1])
summary(modelito)
summary(manova(modelito), test = "Pillai")
summary(manova(modelito), test = "Wilks")
summary(manova(modelito), test = "Hotelling-Lawley")
summary(manova(modelito), test = "Roy")

modelito2 <- lm(cbind(fertility, lifeExpF) ~ ., datos[,-c(1,3)])
summary(modelito2)
summary(manova(modelito2), test = "Pillai")
summary(manova(modelito2), test = "Wilks")
summary(manova(modelito2), test = "Hotelling-Lawley")
summary(manova(modelito2), test = "Roy")


datos1 <- datos %>% filter(region != "NorthAtlantic")

modelito3 <- lm(cbind(fertility, lifeExpF) ~ ., datos1[,-c(1,3)])
summary(modelito3)
summary(manova(modelito3), test = "Pillai")
summary(manova(modelito3), test = "Wilks")
summary(manova(modelito3), test = "Hotelling-Lawley")
summary(manova(modelito3), test = "Roy")

# Verificar que el modelo es adecuado

summary_mod <- summary(modelito)
summary_mod$`Response fertility`$r.squared
summary_mod$`Response fertility`$adj.r.squared

summary_mod$`Response lifeExpF`$r.squared
summary_mod$`Response lifeExpF`$adj.r.squared

# ¿Mejor modelo?

update_y.formula <- function(variables, fm) {
  as.formula(paste0(variables, " ~ ", paste(all.vars(fm)[-1], collapse=" + ")))
}

Y <- c("fertility", "lifeExpF")

step1 <- function(y, orig_fm){
  fm <- update_y.formula(y, orig_fm)
  step(lm(fm, data=datos[,-1]))
}

fm <- fertility ~ region + group + ppgdp + pctUrban

modelitopro <- lapply(Y, step1, orig_fm=fm)
modelitopro

lapply(modelitopro, function(x) summary(x)$coefficients)

residuos <- cbind(as.numeric(lapply(modelitopro, function(x) summary(x)$residuals)[[1]]),
                  as.numeric(lapply(modelitopro, function(x) summary(x)$residuals)[[2]]))

sigma.gorro <- 1/nrow(datos) * t(residuos) %*% residuos

x <- fastDummies::dummy_columns(datos[,-c(1,4,6)], remove_first_dummy = TRUE)
x <- cbind(1, x[,5:13], x[,3]) %>% 
  as.matrix()
y <- cbind(datos$fertility, datos$lifeExpF)

b1 <- lapply(modelo2.0, function(x) summary(x)$coefficients)[[1]][,1]
b2 <- lapply(modelo2.0, function(x) summary(x)$coefficients)[[2]][,1]

B1 <- c(b1[1:8], rep(0, 3), b1[9])
B2 <- c(b2[1], rep(0,7), b2[2:5])

betas <- cbind(B1, B2)

sigma.colita <- 1/nrow(datos) * t(y-x%*%betas[1:11,])%*%(y-x%*%betas[1:11,])

G <- nrow(datos) * sigma.gorro
H <- nrow(datos) * (sigma.colita-sigma.gorro)

pillai <- sum(diag(H%*%solve(H+G)))

(199-2)/(2*199-2)*pillai^2

qf(pillai, 10, 189)

qf(0.95,10,189)

lapply(modelo2.0, function(x) anova(x))
