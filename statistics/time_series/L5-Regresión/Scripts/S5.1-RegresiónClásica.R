library(tidyverse)
library(forecast)
library(gridExtra)
library(broom)

# Vamos a hacer una función que aplique tema a nuestros gráficos
ggtema <- function(graf = NULL) {
  graph <- graf +
    theme_classic() +
    theme(
      plot.title = element_text(color = "#0099F8",
                                size = 17,
                                face = "bold"),
      plot.subtitle = element_text(color = "#969696", size = 13, face = "italic"),
      axis.title = element_text(color = "#969696",
                                size = 10,
                                face = "bold"),
      axis.text = element_text(color = "#969696", size = 10),
      axis.line = element_line(color = "#969696")
    )
  
  return(graph)
}

# Directorio donde se encuentra este cuaderno
setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L5-Regresión/Scripts")

# Leer los datos
aus <- read.csv("../data/aus_production.csv")
aus <- aus %>% na.omit()
head(aus)

# Convertimos en serie de tiempo
aus.ts <- ts(aus[,2:6], 
          st = 1956, 
          fr = 4)

# Datos de entrenamiento
aus.train <- window(aus.ts, 
                    start = c(1956,1),
                    end = c(1999,4))

# Datos de prueba
aus.test <- as.data.frame(window(aus.ts, 
                                 start = c(2000,1)))

# Gráficos de series incluidas en el dataset
autoplot(aus.train, facet=TRUE) +
  xlab("Año") + 
  ylab("Producción") + 
  ggtitle("Producciones de diversos artículos en Australia")

## Modelos de regresión
# Primer modelo de regresión
reg_1 <- tslm(Beer ~ Electricity,
              aus.train)
summary(reg_1)

# Segundo modelo de regresión (Añadir una variable)
reg_2 <- tslm(Beer ~ Electricity + Tobacco,
              aus.train)
summary(reg_2)

# Tercer modelo de regresión (Añadir una variable)
reg_3 <- tslm(Beer ~ Electricity + Tobacco + Bricks,
              aus.train)
summary(reg_3)

# Cuarto modelo de regresión (Modelo Saturado)
reg_4 <- tslm(Beer ~ Tobacco + Bricks + Cement + Electricity, 
              aus.train)
summary(reg_4)

# GRÁFICOS
# Ajuste primer modelo
plt_reg1 <- autoplot(aus.train[,"Beer"], series="Datos") +  
  autolayer(fitted(reg_1), series="Ajuste") + 
  labs(title = "Producción Trimestral de Cerveza",
       subtitle = "Ajuste Regresión Lineal - Modelo 1",
       y = "Megalitros", x = "Año") 
plt_reg1 <- ggtema(plt_reg1)

# Ajuste segundo modelo
plt_reg2 <- autoplot(aus.train[,"Beer"], series="Datos") +  
  autolayer(fitted(reg_2), series="Ajuste") + 
  labs(title = "Producción Trimestral de Cerveza",
       subtitle = "Ajuste Regresión Lineal - Modelo 2",
       y = "Megalitros", x = "Año") 
plt_reg2 <- ggtema(plt_reg2)

# Ajuste tercer modelo
plt_reg3 <- autoplot(aus.train[,"Beer"], series="Datos") +  
  autolayer(fitted(reg_3), series="Ajuste") + 
  labs(title = "Producción Trimestral de Cerveza",
       subtitle = "Ajuste Regresión Lineal - Modelo 3",
       y = "Megalitros", x = "Año") 
plt_reg3 <- ggtema(plt_reg3)

# Ajuste cuarto modelo
plt_reg4 <- autoplot(aus.train[,"Beer"], series="Datos") +  
  autolayer(fitted(reg_4), series="Ajuste") + 
  labs(title = "Producción Trimestral de Cerveza",
       subtitle = "Ajuste Regresión Lineal - Modelo 4",
       y = "Megalitros", x = "Año") 
plt_reg4 <- ggtema(plt_reg4)

# Mostrar los gráficos
grid.arrange(plt_reg1,plt_reg2,
             plt_reg3,plt_reg4, 
             nrow = 2, 
             ncol = 2)

## RESIDUALES
checkresiduals(reg_1)
checkresiduals(reg_2)
checkresiduals(reg_3)
checkresiduals(reg_4)

### Criterios de información
glance(reg_1) %>%
  dplyr::select(r.squared, adj.r.squared, AIC, BIC)

glance(reg_2) %>%
  dplyr::select(r.squared, adj.r.squared, AIC, BIC)

glance(reg_3) %>%
  dplyr::select(r.squared, adj.r.squared, AIC, BIC)

glance(reg_4) %>%
  dplyr::select(r.squared, adj.r.squared, AIC, BIC)

# ¿Hacemos algún cambio? Ajusta un modelo que te haga sentido
reg_final <- tslm(Beer ~ Bricks + Cement,
                  aus.train)
summary(reg_final)
  
# Elabora un gráfico del modelo (similar a alguno de las líneas 74-105)
plt_reg_final <- autoplot(aus.train[,"Beer"], series="Datos") +  
  autolayer(fitted(reg_final), series="Ajuste") + 
  labs(title = "Producción Trimestral de Cerveza",
       subtitle = "Ajuste Regresión Lineal - Modelo Final",
       y = "Megalitros", x = "Año") 
plt_reg_final <- ggtema(plt_reg_final)
plt_reg_final

# Elabora un gráfico de análisis de residuales (similar a alguno de las líneas 114-117)
checkresiduals(reg_final)
  
# Elabora un análisis de los criterios de bondad de ajuste (similar a alguno de las líneas 120-130)
glance(reg_final) %>%
  dplyr::select(r.squared, adj.r.squared, AIC, BIC)

  
# Predicciones con base en el modelo elegido
predic <- forecast(reg_final, newdata = aus.test)
predic

# Gráfico final!
plt_reg_final <- autoplot(aus.ts[,"Beer"], series="Datos", color = "darkturquoise", alpha = 0.7) +  
  autolayer(fitted(reg_final), series="Ajuste", color = "coral", alpha = 0.5) + 
  autolayer(predic$mean, series="Predicción", color = "red", lwd = 1, alpha = 0.7) + 
  labs(title = "Producción Trimestral de Cerveza",
       subtitle = "Ajuste Regresión Lineal + Predicción",
       y = "Megalitros", x = "Año") 
plt_reg_final <- ggtema(plt_reg_final)
plt_reg_final

### TEMA APARTE!
# ¿Y si lo vemos como una serie de tiempo?
# Revisemos una regresión incluyendo componentes de ST
reg_ts <- tslm(Beer ~ trend + season + Bricks + Cement, 
                  aus.train)
summary(reg_ts)
checkresiduals(reg_ts)

# QQ - Plot para revisar normalidad de residuales
res.ts <- resid(reg_ts)
qqnorm(res.ts)
qqline(res.ts) 

# Criterios de información
glance(reg_ts) %>%
  dplyr::select(r.squared, adj.r.squared, AIC, BIC)

# Predicciones con base en el modelo
predic <- forecast(reg_ts, newdata = aus.test)
predic

# Gráfico con ajuste y predicciones
plt_reg_ts <- autoplot(aus.ts[,"Beer"], series="Datos", color = "darkturquoise", alpha = 0.7) +  
  autolayer(fitted(reg_ts), series="Ajuste", color = "coral", alpha = 0.5) + 
  autolayer(predic$mean, series="Predicción", color = "red", lwd = 1, alpha = 0.7) + 
  labs(title = "Producción Trimestral de Cerveza",
       subtitle = "Ajuste Regresión Lineal con componentes de ST + Predicción",
       y = "Megalitros", x = "Año") 
plt_reg_ts <- ggtema(plt_reg_ts)
plt_reg_ts
