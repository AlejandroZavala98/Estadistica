# Presentación y analisis de la información (Parte 1) By: Alejandro Zavala

# Definicion de problema
# 
# Los datos siguientes se tomaron de una muestra, los cuales indican el numero de hijos en ciertas
# familias


rm(list = ls())

library(ggplot2)

datos <- c(3,2,0,1,2,3,4,5,6,5,4,3,2,1,0,7,4,2,5,4,1,3,4,6,5,3,4,5,5)
datos

n_tot <- length(datos)
print(paste("El tamaño de la muestra es:",n_tot))

# Ordenando datos de menor a mayor

datos_sort <- sort(datos)
datos_sort

# ¿Cuantas familias tienen mas de 5 hijos (Expreselo ademas en porcentaje) ?

datos_mayor5 <- datos_sort[datos_sort>5]
n_datos_mayor5 <- length(datos_mayor5)
print(paste("El numero de familias que tiene mas de 5 hijos es:",n_datos_mayor5))

porcent_n_datos_mayor5 <- n_datos_mayor5/n_tot
print(paste("El numero de familias que tiene mas de 5 hijos en porcentaje es:",porcent_n_datos_mayor5))

# Dataframe de frecuencias del numero de hijos que tienen por familia

data_frecuencia <- data.frame()
for(num_hijos in unique(datos_sort))
{
  data_filter <- datos_sort[datos_sort == num_hijos]
  n_filter <- length(data_filter)
  porcent_n_filter <- round((n_filter/n_tot) * 100,2);
  
  filter_iesimo <- c(num_hijos,n_filter,porcent_n_filter)
  data_frecuencia <- rbind(data_frecuencia,filter_iesimo)
}
colnames(data_frecuencia) <- c("Numero_de_hijos","Familias_numero","Porcentaje (%)")
data_frecuencia

# Grafica de frecuencia 
g <- ggplot(data = data_frecuencia, mapping = aes(x=Numero_de_hijos, y=Familias_numero))
g <- g + geom_bar(stat = "identity",fill="darkgreen") + labs(x = "Número de hijos", y = "Frecuencia obtenida", title = "Frecuencia de números de hijos")
g

# Tabla de frecuencia transpuesta

df_freq <- data_frecuencia[,1:2]
colnames(df_freq) <- c("x_i","f(x_i)")
t_freq <- t(df_freq)
t_freq




