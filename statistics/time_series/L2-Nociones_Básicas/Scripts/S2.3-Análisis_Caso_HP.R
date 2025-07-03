library(tidyverse)

# Get working directory para saber en dónde estamos
getwd()
setwd("C:/Users/emanu/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L2-Nociones_Básicas/Scripts")

hpStock <- read.table("../data/HP.txt", header = T)

## ¿Cómo se ven sus gráficos?
par(mfrow = c(2,2))
plot(as.ts(hpStock$Price))
acf(as.ts(hpStock$Price))
pacf(as.ts(hpStock$Price))
# ¿A qué modelo se parece?

# Nueva columna: Diferencia de los tiempos t y t-1
hpStock$difPrice <- c(0, diff(hpStock$Price))

## ¿Cómo se ven sus gráficos?
par(mfrow = c(2,2))
plot(as.ts(hpStock$difPrice))
acf(as.ts(hpStock$difPrice))
pacf(as.ts(hpStock$difPrice))
# ¿A qué modelo se parece?
