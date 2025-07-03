library(tidyverse)

# Cargamos datos
pas <- AirPassengers
plot(pas)

# Función para descomponer la serie
pas.decom <- decompose(AirPassengers, type="mult")

# Gráfico de la descomposición
plot(pas.decom)
# ¿El componente aleatorio se ve aleatorio?

# Extraer componente de tendencia y estacionalidad
tendPas <- pas.decom$trend
estPas <- pas.decom$seasonal

# Gráfico de los componentes
ts.plot(cbind(pas, tendPas, tendPas * estPas), 
        lty=1:3,
        lwd = c(2,1.5,1.5),
        col = c("gray","orange","blue")
        )
legend("topleft", 
       legend=c("Serie", "Tendencia", "Tendencia + Estacion"),
       col=c("gray","orange","blue"), lty=1:3)

# Probemos con otra serie

# Cargar los datos
setwd("C:/Users/emanu/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L3-ETSDA/Scripts")
calGlobal <- read.table("../data/global.dat") # OJO! No tiene header

# Data wrangling
calGlobal <- calGlobal%>% pivot_longer(
  cols= starts_with("V"),
  names_to = "t",
  values_to="temperature"
)
# Convertir en ts
calGlobal.st <- ts(calGlobal$temperature, 
                   st = c(1856,1), 
                   end = c(2005,12),
                   fr = 12)
# Obtener una ventana
calGlobal.st <- window(calGlobal.st,
                       start = c(1901,1),
                       end = c(2005,12))

# Función para descomponer la serie
calGlobal.decom <- decompose(calGlobal.st)

# Gráfico de la descomposición
plot(calGlobal.decom)
# ¿El componente aleatorio se ve aleatorio?

# Extraer componente de tendencia y estacionalidad
tendCal <- calGlobal.decom$trend
estCal <- calGlobal.decom$seasonal

# Gráfico de los componentes
ts.plot(cbind(calGlobal.st, tendCal, tendCal+estCal), 
        lty=1:3,
        lwd = c(1,1.5,1.5),
        col = c("gray","orange","blue")
)
legend("topleft", 
       legend=c("Serie", "Tendencia", "Tendencia + Estacion"),
       col=c("gray","orange","blue"), lty=1:3)
