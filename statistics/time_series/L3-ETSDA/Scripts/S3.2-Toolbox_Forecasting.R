library(tidyverse)
library(forecast)

# Ubicación de este cuaderno
setwd("C:/Users/emanu/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L3-ETSDA/Scripts")

# Leer los datos
aus <- read.csv("../data/aus_production.csv")
head(aus)

# Armar un objeto ts
ausBrick <- ts(
  aus$Bricks %>% na.omit(), # OJO: ausBrick únicamente tiene datos hasta 2005
  st = 1956, 
  fr = 4) 

plot(ausBrick,
     main = "Producción de Ladrillos en Australia",
     ylab = "Ladrillos")

# Número de pasos para los pronósticos
steps = 20

# Dataframe para guardar los valores pronosticados
forecasts <- data.frame(
  Periodo = 1:steps
)

### Método del Promedio (Mean)
### Promedio de observaciones
Prom <- meanf(ausBrick, 
              h = steps)
Prom

# Agregamos la predicción a nuestro DF
forecasts$Promedio <- as.vector(Prom$mean)

# Gráfico de predicción Promedio
plot(
  meanf(ausBrick, h = steps), 
  ylim = c(200,700),
  main = "Método Promedio"
  )

### Método Naive
### Último dato observado
Naive <- naive(ausBrick, 
              h = steps)
Naive

# Agregamos la predicción a nuestro DF
forecasts$Naive <- as.vector(Naive$mean)

# Gráfico de predicción Naive
plot(
  naive(ausBrick, h = steps), 
  ylim = c(200,700),
  main = "Método Naive"
  )

### Método Seasonal Naive:
### Último dato observado c/ estacionalidad
SNaive <- snaive(ausBrick, 
               h = steps)
SNaive

# Agregamos la predicción a nuestro DF
forecasts$SNaive <- as.vector(SNaive$mean)

# Gráfico de predicción S-Naive
plot(
  snaive(ausBrick,h = steps), 
  ylim = c(200,700),
  main = "Método Naive Estacional"
  )

### Método Naive Drift
### Naive con Incremento/Decremento
Drift <- rwf(ausBrick,
             h = steps, 
             drift = TRUE)
Drift

# Agregamos la predicción a nuestro DF
forecasts$Drift <- as.vector(Drift$mean)

# Gráfico de predicción Drift
plot(
  rwf(ausBrick,h = steps, drift = TRUE), 
  ylim = c(200,700),
  main = "Método Drift"
  )

#### Veamos la tabla de los pronósticos
view(forecasts)

#### Veamos todos juntos graficados!
autoplot(ausBrick) +
  autolayer(meanf(ausBrick, h = steps),
            series = "Promedio", PI = FALSE) +
  autolayer(naive(ausBrick, h = steps),
            series = "Naive", PI = FALSE) +
  autolayer(snaive(ausBrick, h = steps),
            series = "Naive Estacional", PI = FALSE) +
  autolayer(rwf(ausBrick,h = steps, drift = TRUE),
            series = "Drift", PI = FALSE) + 
  labs(title = "Producción de Ladrillos",
       subtitle = "Australia 1956 - 2005",
       x = "Año",
       y = "Ladrillos") +
  theme_minimal() +
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
