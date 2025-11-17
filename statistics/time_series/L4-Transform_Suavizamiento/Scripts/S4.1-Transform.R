library(tidyverse)
library(forecast)

setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L4-Transform_Suavizamiento/Scripts")

### Transformación por calendario

# Leemos desde web (el csv también se encuentra en la carpeta)
www <- "https://raw.githubusercontent.com/ricardoscr/UW-Data-Science-Certificate/master/02-Methods/CADairyProduction.csv"

# Guardamos en una variable la lectura
prod <- read.csv(www, header = T)
head(prod)

# Convertir en ts
leche <- ts(prod$Milk.Prod, start=1995, freq=12)

# Gráfico Inicial
plot(leche, 
     main='Serie de tiempo Producción Leche', 
     ylab='Producción de Leche')

# Cálculo de promedio diario
tab.leche <- cbind(Mensual = leche,
                   PromDiario = leche/monthdays(leche))

# Gráfico comparativo
autoplot(tab.leche, facet=TRUE) +
  xlab("Año") + 
  ylab("Leche") + 
  ggtitle("Producción de Leche")

### Transformación por población
econ <- read.csv("../data/global_economy.csv", header = T)
head(econ)

# Seleccionamos los datos únicamente de Qatar
econQat <- econ[econ$unique_id == "Qatar",]
# Convertir en ts
econQat <- ts(econQat, start=1960, freq=1)
# Cálculo per cápita
tab.qat <- cbind(PIB = econQat[,"GDP"],
                 PIBxCapita = econQat[,"GDP"]/econQat[,"Population"])

# Gráfico comparativo
autoplot(tab.qat, facet=TRUE) +
  xlab("Año") + 
  ylab("PIB") + 
  ggtitle("PIB Anual - Qatar")

### Transformación por inflación
ret <- read.csv("../data/aus_retail.csv", header = T)
head(ret)

# Observando un solo tipo de industria
retIndustry <- ret[ret$Industry == "Newspaper and book retailing",]
# Convertimos la fecha en tipo fecha
retIndustry$Month <- ymd(retIndustry$Month)
# Agrupamos sumando por año
turnAus <- retIndustry %>% 
  group_by(ds = lubridate::year(ymd(Month))) %>% 
  summarize(Turnover = sum(Turnover)) %>% 
  as.data.frame()

# Obtenemos INPC de Australia desde otro csv
cpiAus <- econ[econ$unique_id == "Australia", c("ds", "CPI")]
# Unimos ambas tablas por fecha
econAus <- merge(turnAus, cpiAus, by = "ds")
# Convertimos en serie de tiempo, únicamente columnas 2 y 3
econAus <- ts(econAus[,2:3], start=1982, freq=1)

# Ajuste inflacionario
tab.fact <- cbind(Factur = econAus[,"Turnover"],
                  FacturAjust = econAus[,"Turnover"]/econAus[,"CPI"]*100)

# Gráfico comparativo
autoplot(tab.fact, facet=TRUE) +
  xlab("Año") + 
  ylab("Facturación") + 
  ggtitle("Facturación Retail Libros y Periódicos")

### Transformaciones logarítmicas

# Cargamos datos de pasajeros
pas <- as.vector(AirPassengers)

# Creamos tabla con datos originales y transformados
tab.pas <- as.data.frame(
  cbind(Pasajeros = pas,
        logPasajeros = log(pas))
  )

# Convertimos en serie de tiempo
tab.pas <- ts(tab.pas, start = 1949, frequency = 12)

# Gráfico comparativo
autoplot(tab.pas, facet=TRUE) +
  xlab("Año") + 
  ylab("Pasajeros") + 
  ggtitle("Reservaciones de Pasajeros en Vuelos")

## Cálculo de lambda automática en BoxCox
lambda <- BoxCox.lambda(pas)

# Tabla con transf logarítmica y BoxCox con lambda calculada
tab.boxpas <- as.data.frame(
  cbind(logPasajeros = BoxCox(pas, 0),
        boxcoxPasajeros = BoxCox(pas, lambda))
)

# Convertimos en serie de tiempo
ts.boxpas <- ts(tab.boxpas, start = 1949, frequency = 12)

# Gráfico comparativo entre log y BoxCox (lambda)
autoplot(ts.boxpas, facet=TRUE) +
  xlab("Año") + 
  ylab("Pasajeros") + 
  ggtitle("Reservaciones de Pasajeros en Vuelos")

# Nuevo set de datos precio de huevo
huevos <- read.csv("../data/eggs.csv", header = T)
# Queremos únicamente la segunda columna
huevos <- huevos[,2]
head(huevos)
# Convertimos en serie de tiempo
huevos.ts <- ts(
  huevos, 
  st = 1900) 

#Creamos lambda
lambda <- BoxCox.lambda(huevos)

# Gráfico inicial
plot(huevos.ts)

# Método drift con transformación log
predbox <- rwf(huevos.ts, drift = TRUE, lambda = 0,
               h = 50)
# Método drift con transformación boxcox
predboxajust <- rwf(huevos.ts, drift = TRUE, lambda = lambda,
                    h = 50)
# Método drift con transformación boxcox ajuste sesgo
predboxajustbias <- rwf(huevos.ts, drift = TRUE, lambda = lambda,
               h = 50, biasadj = TRUE)

# Viendo resultados de ambos pronósticos
autoplot(huevos.ts) +
  autolayer(predbox, series = "Predicción usando ln", PI = FALSE)+
  autolayer(predboxajust, series = "Predicción usando BoxCox(lambda)", PI = FALSE)+
  autolayer(predboxajustbias, series = "Ajuste por sesgo", PI = FALSE) +
  guides(colour = guide_legend(title = "Método"))
