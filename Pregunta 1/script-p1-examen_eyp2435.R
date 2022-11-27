library(tidyverse)

datos <- rio::import("Pregunta 1/UNData.csv")


# Análisis exploratorio ---------------------------------------------------

## Correlaciones entre las variables continuas 
GGally::ggpairs(datos[,-c(1:3)])

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
