# Presentación y analisis de la información (Parte 2) By: Alejandro Zavala

# Definicion de problema
# 
# Tabla para datos no agrupados

# Los siguientes datos denotan el resultado de 26 pilas escogidas al azar, el tiempo de duración en horas a cierto tratamiento (proceso) 

rm(list = ls())
library(ggplot2)

data_freq <- data.frame( "x_i" = c(4,2,11,8,9), "f_i" = c(2,3,10,4,7))
data_freq

g <- ggplot(data = data_freq, mapping = aes(x=x_i, y=f_i))
g <- g + geom_bar(stat = "identity",fill="cornflowerblue") + labs( x = "Hora de duración", y = "Frecuencia",title = "Tabla de frecuencias para la duración de pilas en horas")
g

# Ordenando el dataframe por las clases
data_freq <- data_freq[order(data_freq$x_i), ]
data_freq

# Agregando frecuencia acumulada
data_freq$`Frecuencia Acumulada` <- cumsum(data_freq$f_i)
data_freq

# Agregando columnas extras para datos
data_freq$`XiFi` <- data_freq$x_i *data_freq$f_i
data_freq

# n (tamaño de la muestra)

n_tot <- sum(data_freq$f_i);n_tot 
# Equivalente a data_freq$`Frecuencia Acumulada`[length(data_freq$`Frecuencia Acumulada`)]

# Suma de las Xi * Fi
sum_XiFi <- sum(data_freq$`XiFi`);sum_XiFi

mu_estimada <- sum_XiFi/n_tot
print(paste("La media estimada es: ",mu_estimada))

# Columnas para la varianza

data_freq$`Xi-mu` <- data_freq$x_i - mu_estimada
data_freq$`(Xi-mu)^2` <- data_freq$`Xi-mu`^2
data_freq$`(Xi-mu)^2 * Fi` <- data_freq$`(Xi-mu)^2` * data_freq$f_i
data_freq

sum_ximu2Fi <- sum(data_freq$`(Xi-mu)^2 * Fi`)
sigma_2 <- sum_ximu2Fi/(n_tot - 1)
print(paste("La varianza estimada es: ",sigma_2))

sigma_1 <- sqrt(sigma_2)
print(paste("La desviacion estimada es: ",sigma_1))


# Coeficiente de variacion

coef_variacion <- (sigma_1/mu_estimada)*100
print(paste("El coeficiente de variacion es: ",coef_variacion))

