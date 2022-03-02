# Tabla de frecuencias con estadisticos para datos agrupados Por: Alejandro Zavala

# Los siguientes datos representan el costo de la energía eléctrica durante julio del año en curso,
# de departamentos con dos recámaras en la colonia las águilas

rm(list = ls())

library(readxl)
library(ggplot2)


df_data <- read_excel("C:/Users/Alejandro Zavala/Zavala_Programas/Repositorios_Git/Estadistica/Distribucion_De_Frecuencias/Insumos/Datos_EnergiaElectrica.xlsx")

# Descripcion de los datos
names(df_data)[2]

# Informacion de una columna

data_to <- df_data$Datos

data_to

# Determinando el numero de clases para la construcción de frecuencias

n_tot <- length(data_to); paste("El número total de la muestra es: ",n_tot)

clases_Sturge <- function(n_data)
{
  1 + 3.33 * log10(n_data)
}

k_sturge <- round(clases_Sturge(n_tot)); paste("El numero de clases por regla de Sturges es:",k_sturge)

max_data <- max(data_to);paste("El dato máximo es: ",max_data)

min_data <- min(data_to);paste("El dato mínimo es: ",min_data)

rango_data <- max_data - min_data; paste("El rango es: ",rango_data)

size_clase <- round(rango_data/k_sturge); paste("El tamaño de la clase es: ",size_clase)

# Armando nuestra base de tabla de frecuencias

ic_min <- c()
ic_max <- c()
ic_com <- c()
xi_data <- c()

t_beg <- min_data
t_end <- max_data

while(t_beg < t_end) 
{
  t_beg_size <- t_beg + size_clase
  ic_min <- c(ic_min,t_beg)
  ic_max <- c(ic_max,t_beg_size)
  ic_com <- c(ic_com,(paste(t_beg,"-",t_beg_size)))
  xi_data <- c(xi_data,t_beg + (t_beg_size-t_beg)/2)
  t_beg <- t_beg + size_clase
}

data_agrupada <- data.frame("IC_min" = ic_min,
                            "IC_max" = ic_max,
                            "IC" = ic_com,
                            "Marc(x_i)" = xi_data)

data_agrupada
n_agrupada <- dim(data_agrupada)[1]

fi_data <- rep(0,n_agrupada)

for(valor in data_to)
{
  for (index in 1:n_agrupada)
  {
    if(valor <= data_agrupada$IC_max[index])
    {
      fi_data[index] <- fi_data[index] + 1
      break
    }
  }
}

#Visualizando la tabla de frecuencias creado
data_agrupada$`frec(f_i)` <- fi_data
data_agrupada

#Visualizando las frecuencias por clase
hist_data <- ggplot(mapping = aes(x = data_agrupada$Marc.x_i.,y = data_agrupada$`frec(f_i)`)) + geom_bar(stat = "identity",fill="darkcyan", color="white") ;
hist_data <- hist_data + labs( x="Marca de la clase",y = "Frecuencia",title = "Histograma de los datos agrupados");
hist_data

# Calculando el promedio

data_agrupada$`xi*fi` <- data_agrupada$Marc.x_i.*data_agrupada$`frec(f_i)`
data_agrupada

n_data <- sum(data_agrupada$`frec(f_i)`) # Que debe coincidir con la muestra inicial

if(n_data != n_tot)
{
  print("Ocurrio un error")
}

prom_data <- sum(data_agrupada$`xi*fi` / n_data); 

paste("El promedio calculado en la tabla de frecuencia es: ",prom_data)
paste("El promedio calculado desde el inicio de la tabla es: ",mean(data_to))

# Calculando la mediana

data_agrupada$`frecAcu(F_i)` <- cumsum(data_agrupada$`frec(f_i)`)
data_agrupada

func_par <- function(n)
{
  if(n%%2==0) #es par
  {
    search_n <- n/2
  }
  else{
    search_n <- (n + 1)/2
  }
  return(search_n)
}

n_sch <- func_par(n_data)
paste("El n a buscar para mediana es:",n_sch)

found_med <- function(df,n_search)
{
  if (n_search %in% df$`frecAcu(F_i)`)
  {
    data_med <- df[df$`frecAcu(F_i)` == n_search,]$IC_max
  }
  else
  {
    for(i in 1:(dim(df)[1]))
    {
      if (n_search < df$`frecAcu(F_i)`[i])
      {
        paste("El valor de i es: ",i)
        break
      }
    }
    li <- df$IC_min[i]
    facu_i1 <- df$`frecAcu(F_i)`[i-1]
    fi <- df$`frec(f_i)`[i]
    ai <- df$IC_max[i]-df$IC_min[i]
    paste("Limite inferior: ",li)
    paste("Frecuencia i: ",fi)
    paste("Frecuencia acumulada i-1: ",facu_i1)
    paste("a_i: ",ai)
    data_med <- li + (((n_search - facu_i1)/fi)* ai)
  
  }
  return(data_med)
}
med_data <- found_med(data_agrupada,n_sch)
paste("La mediana encontrada en la tabla de frecuencia es: ",med_data)
paste("La mediana encontrada desde el inicio de la tabla es: ",median(data_to))

# Calculando la moda

found_moda <- function(df)
{
  facu_max <- max(df$`frec(f_i)`)
  paste("La frecuencia mas alta es: ",facu_max)
  index_max <- which(data_agrupada$`frec(f_i)` == facu_max)[1]
  li <- df$IC_min[index_max]
  fi <- df$`frec(f_i)`[index_max]
  fip1 <- df$`frec(f_i)`[index_max+1]
  fil1 <- df$`frec(f_i)`[index_max-1]
  ai <- df$IC_max[index_max]-df$IC_min[index_max]
  paste("Limite inferior: ",li)
  paste("Frecuencia i: ",fi)
  paste("Frecuencia i-1: ",fil1)
  paste("Frecuencia i+1: ",fip1)
  paste("a_i: ",ai)
  data_mod <- li + (((fi-fil1)/((fi-fil1)+(fi-fip1)))*ai)
  return(data_mod)
}

mode_funct <- function(x) 
{
  return(as.numeric(names(which.max(table(x)))))
}

moda_data <- found_moda(data_agrupada)
paste("La moda encontrada es: ",moda_data)
paste("La moda encontrada desde el inicio de la tabla es: ",mode_funct(data_to))

# Calculando la varianza

data_agrupada$`xi-x^-` <- data_agrupada$Marc.x_i. - prom_data
data_agrupada

data_agrupada$`xi-x^-^2` <- data_agrupada$`xi-x^-`^2
data_agrupada

data_agrupada$`xi-x^-^2*fi` <- data_agrupada$`xi-x^-^2` * data_agrupada$`frec(f_i)`
data_agrupada

var_data <- sum(data_agrupada$`xi-x^-^2*fi`)/(n_data-1)
sd_data <- sqrt(var_data)

paste("La varianza de datos agrupados es: ",var_data,"y su desviacion es: ",sd_data)
paste("La varianza sin datos agrupados es: ",var(data_to))

# Coeficiente de variación
coef_var <- sd_data*100/prom_data
paste("El coeficiente de variacion es:",coef_var)

# Definiendo quantiles, deciles y percentiles

quar_per_dec <- function(df,valor)
{
  
  if (valor %in% df$`frecAcu(F_i)`)
  {
    found_til <- df[df$`frecAcu(F_i)` == valor,]$IC_max
  }
  else
  {
    for(i in 1:(dim(df)[1]))
    {
      if (valor < df$`frecAcu(F_i)`[i])
        {
        print(paste("El valor de i es: ",i))
        break
        }
    }
    ampli <- df$IC_max[i] - df$IC_min[i]
    fac_i <- df$`frecAcu(F_i)`[i]
    l_i <- df$IC_min[i] 
    if (i == 1)
    {
      fac_i1 <- 0
    }
    else
    {
      fac_i1 <- df$`frecAcu(F_i)`[i-1]
    }
    found_til <- l_i + ( ((valor - fac_i1 )/(fac_i-fac_i1)) * (ampli))
  }
  return(found_til)
}

input_data <- function(cal_data,df)
{
  til_data <- "error"
  n_dim <- sum(df$`frec(f_i)`)
  print("Calcularemos quantiles, deciles y percentiles")
  type_c <- readline(prompt="Ingrese \"p\" para percentil, \"d\" para decil o \"q\" para quantiles: ")
  if (type_c == "p")
  {
    calc_t <- readline(prompt="Ingrese el percentil deseado (1 al 100) ")
    if(as.integer(calc_t) %in% 1:100)
    {
      val_data <- (as.integer(calc_t)*n_dim)/100
      print(paste("El valor para calcular la medida deseada es: ",val_data))
      til_data <- quar_per_dec(df,val_data)
    }
    else
    {
      print("Error no se puede calcular percentil")
    }
  }
  else if (type_c == "d")
  {
    calc_t <- readline(prompt="Ingrese el decil deseado (1 al 10) ")
    if(as.integer(calc_t) %in% 1:10)
    {
      val_data <- (as.integer(calc_t)*n_dim)/10
      print(paste("El valor para calcular la medida deseada es: ",val_data))
      til_data <- quar_per_dec(df,val_data)
    }
    else
    {
      print("Error no se puede calcular percentil")
    }
  }
  else if (type_c == "q")
  {
    calc_t <- readline(prompt="Ingrese el percentil deseado (1 al 3) ")
    if(as.integer(calc_t) %in% 1:3)
    {
      val_data <- (as.integer(calc_t)*n_dim)/4
      print(paste("El valor para calcular la medida deseada es: ",val_data))
      til_data <- quar_per_dec(df,val_data)
    }
    else
    {
      print("Error no se puede calcular percentil")
    }
  }
  else
  {
    print("No seleccionaste una opción correcta")
  }
  return(til_data)
}

input_data(quar_per_dec,data_agrupada) # Ingresar datos para verificar

# Summary

df_resumen <- data.frame("Promedio"=prom_data,
                         "Varianza"=var_data,
                         "Desviación Estándar"=sd_data,
                         "Coeficiente de variación"=coef_var,
                         "Mediana"=med_data,
                         "Moda" = moda_data)
df_resumen

df_frecuencias <- data_agrupada[,c("IC_min","IC_max","IC","Marc.x_i.","frec(f_i)","frecAcu(F_i)")]
df_frecuencias


write.csv(df_frecuencias, file="Tabla de frecuencias.csv", row.names = F)
write.csv(df_resumen, file="Medidas encontradas.csv", row.names = F)
