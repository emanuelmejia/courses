library(tidyverse)
library(forecast)

# Coloca la ubicación de este cuaderno
setwd("C:/Users/emanu/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L3-ETSDA/Scripts")

# Leer los datos del cierre de precio de acción de HP
hpStock <- read.table("../data/HP.txt", header = T)
head(hpStock)

# Armar un objeto ts de frecuencia diaria
hpStock <- ts(hpStock, frequency = 7)

# Elabora un gráfico de series de tiempo
# del precio de la acción
# TU CÓDIGO AQUÍ

# Número de pasos para los pronósticos = 60
steps = 60

# Dataframe para guardar los valores pronosticados
forecasts <- data.frame(
  Periodo = 1:steps
)

### Utiliza el método del Promedio (Mean)
### Para elaborar un pronóstico 60 pasos adelante
# TU CÓDIGO AQUÍ

# Agrega las predicciones del método Promedio
# al DF "forecasts"
# TU CÓDIGO AQUÍ

# Elabora un gráfico que muestre la predicción
# Bajo el método de Promedio
# TU CÓDIGO AQUÍ

### Utiliza el método Naive
# Para elaborar un pronóstico 60 pasos adelante
# TU CÓDIGO AQUÍ

# Agrega las predicciones del método Naive
# al DF "forecasts"
# TU CÓDIGO AQUÍ

# Elabora un gráfico que muestre la predicción
# Bajo el método Naive
# TU CÓDIGO AQUÍ

### Utiliza el método Seasonal Naive
# Para elaborar un pronóstico 60 pasos adelante
# TU CÓDIGO AQUÍ

# Agrega las predicciones del método Seasonal Naive
# al DF "forecasts"
# TU CÓDIGO AQUÍ

# Elabora un gráfico que muestre la predicción
# Bajo el método Seasonal Naive
# TU CÓDIGO AQUÍ

### Utiliza el método Drift
# Para elaborar un pronóstico 60 pasos adelante
# TU CÓDIGO AQUÍ

# Agrega las predicciones del método Seasonal Drift
# al DF "forecasts"
# TU CÓDIGO AQUÍ

# Elabora un gráfico que muestre la predicción
# Bajo el método Drift
# TU CÓDIGO AQUÍ

#### Muestra el data.frame de "forecasts"
# TU CÓDIGO AQUÍ

# Elabora un gráfico que muestre todas
# las predicciones juntas
# TU CÓDIGO AQUÍ