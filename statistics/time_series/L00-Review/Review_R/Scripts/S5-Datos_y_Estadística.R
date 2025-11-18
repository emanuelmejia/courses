# La librería dplyr se especializa en hacer consultas/extraer
# info de tablas y bases de datos

library(dplyr)
View(starwars)

# ¿Cuál es el nombre del personaje más alto

summary(starwars$height)

?NA

MAX <- max(starwars$height[!is.na(starwars$height)])
MAX

# Opción 2.

MAX <- max(starwars$height,
           na.rm = TRUE) # Para indicar que ignore los NA's
MAX

# Para extraer el nombre del personaje
# Opción 1

starwars$name[starwars$height == MAX & !is.na(starwars$height)]

# Opción 2

na.omit(starwars$name[starwars$height == MAX])[1] # na.omit devuelve un vector raro, por eso se selecciona la primera entrada

# Hay que contar los NA's de la columna de alturas

# Opción 1
summary(starwars$height)["NA's"]

# Opción 2
sum(is.na(starwars$height)) # Cuenta los "TRUE"

# ¿Cuál es el nombre del personaje con menor masa

library(magrittr)
minmas <-  starwars$mass %>% na.omit() %>% min() # Notación magrittr

starwars$name [starwars$mass == minmas & !is.na(starwars$mass)]

# ¿Cuál es el planeta natal de más personajes y quiénes son dichos personajes

planetas <- table(starwars$homeworld)
planetas

popular <- which.max(planetas) # índice de la entrada más grande
popular # esto no es la respuesta, esto es el índice (posición) de la respuesta

# Esto no es la respuesta
planetas[popular] # de planetas selecciona la posición que corresponde al máximo

# Esto sí es la respuesta
names(popular) # Opción 1
planetas[popular] %>% names() # Opción 2

# Segunda parte ¿Cuáles son esos personajes?

nombres <- c(na.omit(starwars$name[starwars$homeworld == names(popular)])) # Vector quitando NA's
nombres

# ¿Cómo pondríamos "junta" esta información

# Opción 1 - Chafa
paste(names(popular),"es el planeta natal de",nombres)

# Opción 2 - Práctica

df <- data.frame(Planeta = names(popular), # cuidado con esto
                 Nombre = nombres)
View (df)

# Recordemos que el dataframe necesita dos vectores de la misma longitud
# Aquí "Planeta" lo corrió 11 veces para que tuviera el mismo tamaño

# OJO!!!  R NO ES UN MANEJADOR DE BASES DE DATOS!!!
# Lo ideal es que el procesamiento se haga en otro sistema (SQL, Excel)


# Correlación y regresión ---------------------------------------


# ¿Las variables peso y altura están correlacionadas? Muestra su gráfico
# ¿Hay datos atípicos? ¿Cuáles? Elimina los datos atípicos y muestra el gráfico de peso contra altura.
# ¿Cómo se ve afectada la correlación? realiza un modelo de regresión lineal simple e interpreta
# Comprueba los supuestos del modelo.

# Para poder hacer esto, vamos a extraer la información

base <- dplyr::starwars[,c("name","height","mass")] %>% na.omit() # Mucho cuidado con los NA
base

# Vamos a ver la correlación entre las variables

cor(base[,2:3]) # Muestra la matriz de correlaciones
cov(base[,2:3]) # Matriz de varianzas y covarianzas
var(base$height) # Recordemos que la diagonal de la matriz anterior son las varianzas. Esto es una muestra.

library(corrplot) # librería de gráficos de correlaciones

base[,2:3] %>% cor() %>% corrplot(method = "square")

# Vamos a graficar peso vs altura

# Opción 1

plot(x = base$height,
     y = base$mass)  

# Opción 2

colors() # Muestra los colores que hay en R

# Otra opción con adicionales

plot(base[,2:3],                    # Coordenadas
     col = c("orangered1","grey50"),# De qué color (puede ser más de uno e incluso ponerle "colors()")
     pch = 18,                      # Tipo de punto que se va a utilizar
     main = "Altura VS Peso",       # Título del gráfico
     xlab = "Altura",               # Nombre del eje x
     ylab = "Peso")                 # Nombre del eje y

# Podemos notar aquí un dato atípico a simple vista que nos puede estar afectando.

id <- identify(base[,2:3],          # Coordenadas
               labels = base$name)  # Los nombres

# Después de correr esta línea se pone un punto rojo en la parte superior
# de la consola (cuadrante II). Después le damos click al punto que queremos identificar
# en el cuadrante IV (en el gráfico) y le damos click en finish
# En el cuadrante IV esquina superior derecha.
# Puedes darle click a varios puntos antes de darle finish y mostrará todos

id # Tiene que ser después de seleccionar en el gráfico con la instrucción anterior
# Dará como resultado el índice del punto seleccionado

# Ya podremos meter "id" como índice para seleccionar dentro de "base"
base[id,]
base$name[id]

# Guardaremos en datos todo lo que está en base menos el renglón "id"
# Dato atípico
datos <- base[-id,]

# Corremos nuevamente sobre "datos" y veremos que hay una correlación fuerte
datos[,2:3] %>% cor() %>% corrplot(method = "square")
cor(datos[,2:3])

# Gráfico de nuestros datos

plot(datos[,2:3],                   # Coordenadas
     col = "grey50",                # De qué color (puede ser más de uno e incluso ponerle "colors()")
     pch = 18,                      # Tipo de punto que se va a utilizar
     main = "Altura VS Peso",       # Título del gráfico
     xlab = "Altura",               # Nombre del eje x
     ylab = "Peso")                 # Nombre del eje y

# abline pinta sobre el gráfico actual una recta del tipo y = bx + a

abline(a = 0, b = 1, col = "gray", lwd = 2) # "lwd" es el grosor de la línea
abline(a = 0, b = 1/2, col = "gray50", lwd = 2)

# Ajusta un modelo de regresión lineal (simple)

?lm # Ajusta un modelo lineal

# Y : Peso (variable respuesta)
# X : Altura (variable explicativa)

# Busco encontrar coeficientes b0 y b1 en Y = b1*X + b0
# Que mejor se ajusten a mis datos

reg <- lm(datos$mass~datos$height) # "~" Lo que está en masa en función de lo que está en altura
reg # Intercept es b0 y pendiente (trae el título original) es b1

# summary a una regresión para ver muchos datos 
# verificar asteriscos de coeficientes (basados en p-value)
summary(reg) 

# vamos a extraer los coeficientes
coeficientes <- coef(reg)

# Vamos a graficarla con estos coeficientes en rojo
abline(a = coeficientes[1], b = coeficientes[2], col = "blue", lwd = 5)

# Intentemos ajustar un polinomio de grado 3
# Y = b0 + b1*X + b2*X^2 + b3*X^3

datos$alt2 <- datos$height^2
datos$alt3 <- datos$height^3
View(datos)

# Regresión "múltiple"
fit2 <- lm(data = datos,mass~height+alt2+alt3)
summary(fit2) # Parece que no lo está estimando muy bien

coef3 <- coef(fit2)

f3 <- function(x){
  # b0 + b1*X + b2*X^2 + b3*X^3
  coef3[1] + coef3[2]*x + coef3[3]*x^2 + coef3[4]*x^3
}

# Para graficar la función. No olvidar ponerle Add para mostrarlo ahí mismo
curve(f3,from = 0 , to = 300, col = "orange", lwd = 2, add = TRUE) 
