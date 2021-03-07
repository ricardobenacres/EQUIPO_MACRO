  #Trabajamos con los precios diferenciados

library(lubridate)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(plyr)
library(forecast)
library(stats)
library(tseries)
library(performance)
library(quantmod)
library(lmtest)
library(moments)
library(dynlm)
library(fpp2)
library(readxl)
library(mlr)

# DATOS DE INVESTING.COM
# Dólares por barril
View(PWTI)

# Lectura y limpieza de datos
PWTI <- read_csv("Precios WTI_2.csv")
PWTI$Fechas     <- parse_date_time(PWTI$Date, "mdy")
# 1607 entradas
precios <- PWTI$Price
which(precios %in% min(precios))       #en el renglón 221 está el precio negativo
precios[221] <- .01                    #cambiando el valor negativo por .01
min(precios)                      
PWTI$Price = precios

#Realizando nuestra serie de tiempo
#Confirmamos que eliminamos el precio negativo
precio.ts=ts(PWTI$Price,start=2015,frequency=365)
precio.ts[]      <- rev(precio.ts) 
plot(precio.ts,main="Precio del petróleo",col="blue") 

#Diferenciando
diflogprecios.ts=diff(log(precio.ts))
plot(diflogprecios.ts)

  #Checando estacionariedad
  adf.test(diflogprecios.ts, k = 30) #Es estacionaria nuestra serie
  
  #Toca observar nuestra autocorrelación y autocorrelación parcial.
  
  
  ##autocorrelación y autocorrelación parcial, esto nos va a ayuda para saber
  #cuántos autoregresivos vamos a utilizar en nuestro modelo ARIMA
  plot(diflogprecios.ts,type="o",lty="dashed",col="red",main="Serie de Tiempo")
  
      #A continuación en las gráficas, lo que tenemos que observar son las líneas
      #que se salen de la autocorrelación normal y parcial, para determinar el número
      #de medias móviles y de medias móviles y de autoregresivos (respectivamente)
  
  par(mfrow=c(2,1),mar=c(4,4,4,1)+.1)
  acf(diflogprecios.ts) #número de medias móviles: 2 media móvil observada
  pacf(diflogprecios.ts) ##autocorrelación, número de autoregresivos, tenemos 5
  
  #Para que el rezago coincida con la frecuencia:
  acf(ts(diflogprecios.ts,frequency=1), lag.max = 535) #535 es 1/3 del total de la muestra 
  pacf(ts(diflogprecios.ts,frequency=1), lag.max = 535)
  

#Comenzando ahora con nuestras autoregresiones
modelo1<-dynlm(precio.ts~L(precio.ts),data=precio.ts) #L=un rezago
summary(modelo1)

  #Podemos observar claramente que el primer periodo es sigificativo. 
  #Ahora procede realizar 30 rezagos porque nuestra serie es diaria.

modelo2<-dynlm(precio.ts~L(precio.ts,1:30),data=precio.ts) #L=treinta rezagos
summary(modelo2)

  #La mayoría de los periodos anteriores no son significativos. Por lo tanto,
  #después de analizar el número de autoregresivos y el número de medias móviles,
  #realizacmos nuestro modelo ARMA.

#hacemos ahora nuestro ARIMA, con nuestra serie de tiempo original!!!!
#c(autoregresivos,diferencias,medias movil)
modeloARMA=arima(precio.ts,order=c(5,1,2)) #lo hacemos con la serie de tiempo inicial
modeloARMA
tsdiag(modeloARMA) 








#=====================================================
  #NO NECESARIO?

  #LJUNG BOX: mayor a .05 que se ve en la gráfica de tsdiag

  Box.test(residuals(modeloARMA),type="Ljung-Box") #>.05 y sí hay ruido blanco

  #CONFIRMANDO : RUIDO BLANCO media cero, var constante y errores no correlacionados

  #Observando gráficamente el modelo

error=residuals(modeloARMA)
par(mfrow=c(1,1))
plot(error) #media=0 de los errores
jarque.bera.test(error) #>0.05 por lo tanto sí son normales








