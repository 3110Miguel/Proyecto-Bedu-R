library(quantmod)
library(forecast)
library(tseries)

getwd()
setwd("C:/Users/Miguel Angel/Documents/R/BEDU/Proyecto")

#Objetivo:
#El objetivo es el análisis, modelado y predicción de la serie temporal del precio de Bitcoin

#Análisis:

#Datos de Yahoo Finance
bit <- getSymbols("BTC-USD", src = "yahoo", from = "2014-10-01", to = "2021-01-1", periodicity = "monthly") 
eth <- getSymbols("ETH-USD", src = "yahoo", from = "2015-09-01", to = "2021-01-1", periodicity = "monthly") 
ltc <- getSymbols("LTC-USD", src = "yahoo", from = "2014-10-01", to = "2021-01-1", periodicity = "monthly") 

#Gráficos historicos
chartSeries(`BTC-USD`, TA=NULL) 
chartSeries(`BTC-USD`, subset = "last 12 months") #Gráfico últimos 12 meses
chartSeries(`ETH-USD`, TA=NULL) 
chartSeries(`ETH-USD`, subset = "last 12 months") #Gráfico últimos 12 meses
chartSeries(`LTC-USD`, TA=NULL) 
chartSeries(`LTC-USD`, subset = "last 12 months") #Gráfico últimos 12 meses

#Convertir a Data Frame
bit2 <- as.data.frame(`BTC-USD`)
eth <- as.data.frame(`ETH-USD`)
ltc <- as.data.frame(`LTC-USD`)

#Convertir a Serie de Tiempo
btc.ts <- ts(bit2$`BTC-USD.Adjusted`, start = c(2014,10), frequency = 12)
eth.ts <- ts(eth$`ETH-USD.Adjusted`, start = c(2015,9), frequency = 12)
ltc.ts <- ts(ltc$`LTC-USD.Adjusted`, start = c(2014,10), frequency = 12)

#Gráfica 3 principales criptomonedas (Una sola gráfica)
plot(cbind(btc.ts, eth.ts, ltc.ts), plot.type = "single", col = 1:3,
     main = "Precio principales criptomonedas", 
     xlab = "Año",
     sub = "2014-2020")
legend("topleft", colnames(cbind(btc.ts, eth.ts, ltc.ts)), col = 1:3, lty = 1)

#Gráficas 3 pirncipales criptomonedas
plot(cbind(btc.ts, eth.ts, ltc.ts),
     main = "Precio principales criptomonedas", 
     xlab = "Año",
     sub = "2014-2020")
#Se puede observar que las monedas tienen un comportamiento similar a través del tiempo.
#Las tres tuvieron una subida significante en 2017. Y han superado su máximo historico en los últimos meses. Excepto LTC.
#¿Qué relación existe entre las tres criptomonedas? ¿Existe relación o casualidad en su comportamiento?
#Hipótesis: ¿Continurá bitcoin a la alza o tendrá una correción semejante a la del año 2018?

#Gráfica Bitcoin
plot(btc.ts, type = "l", ylab = "Precio (USD)", xlab = "Año",   #Cambiar type = "o" para indicar los meses
     main = "Precio histórico BTC",
     sub = "1-10-2014 a 1-1-2020")

boxplot(btc.ts ~ cycle(btc.ts),
        xlab = "Boxplot de valores estacionales",
        sub = "Precio  2014-2020",
        main = "Precios históricos de BTC")
#En esta gráfica observamos que los meses 1 y 12 tienen valores atípicos.
#Son estos meses donde el precio a subido de manera exponencial.
#En el mes 3 tenemos el menor IQR, lo que indica una menor dispersión de datos.
#Por lo tanto, es en marzo donde existe menos volatilidad en el precio.
#Por otra parte, todos cuentan con mínimos no atípicos semejante.

#Tendencia (regresión lineal) 
ns.btc <- window(btc.ts, start = c(2018,10), end = c(2021, 1))       #Ventana para escoger fecha
nt.btc <- time(ns.btc)
plot(ns.btc, xlab = "Año", ylab = "Precio (USD)", main = "Precios históricos de BTC",
     sub = "2014-2020"); abline(reg = lm (ns.btc~nt.btc))


#Descomposicón de la serie de tiempo

#Modelo aditivo
DA.BTC <- decompose((btc.ts))
plot(DA.BTC, xlab = "Años",
     sub = "Descomposición de los datos del precio de BTC")
#Se aprecia que no tenemos una varianza constante, por lo cual, el modelo aditivo no es de utilidad.

#Componentes
Tendencia <- DA.BTC$trend
Estacionalidad <- DA.BTC$seasonal
Aleatorio <- DA.BTC$random
#plot(Estacionalidad)
ts.plot(cbind(Tendencia, Tendencia + Estacionalidad), 
        xlab = "Años", main = "Datos precio histórico BTC", 
        ylab = "Precio $USD", lty = 1:2,
        sub = "Tendencia con efectos estacionales aditivos sobrepuestos")


#Modelo multiplicativo
DM.BTC <- decompose(btc.ts, type = "mult")
plot(DM.BTC, xlab = "Años", 
     sub = "Descomposición de los datos del precio del BTC")
#Componentes
Trend <- DM.BTC$trend
Seasonal <- DM.BTC$seasonal
Random <- DM.BTC$random

ts.plot(cbind(Trend, Trend*Seasonal), xlab = "Años", main = "Datos precio histórico BTC", 
        ylab = "Precio $USD", lty = 1:2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")

seasonplot(btc.ts, col = rainbow(12), year.labels = TRUE)  #Gráfica de estacionalidad
#Se mantiene una media y varianza no constantes

#Modelado:

#Función de autocorrelación
acf(btc.ts)
#La serie no es estacionaria en media, hay muchos valores fuera de los rangos (líneas punteadas)

ndiffs(btc.ts)  #Saber cuantas diferencias necesito #2

#Diferenciación 1
seriedif <- diff(btc.ts)
plot(seriedif)
#Se puede observar que comienza a obtener estacionalidad, la media está en cero. Los datos están alrededor de ésta.

acf(seriedif) #
ndiffs(seriedif) #Falta una diferenciación

#Diferenciación 2
#seriedif2.1 <- diff(btc.ts, differences = 2)
#plot(seriedif2.1)

seriedif2 <- diff(seriedif) #Los datos rondan al rededor de la media cero
plot(seriedif2)

acf(seriedif2) #
ndiffs(seriedif2) #Ya no hacen falta diferencias
#Hay estacionalidad, se puede llevar a cabo el modelo Arima

#
layout(1:2)
plot(seriedif2.1)
plot(seriedif2)
dev.off()

#Análisis visual de las gráficas de autocorrelación
par(mfrow =c(2,2), mar = c(4,4,4,1)+.1)
plot(btc.ts, type = "o", ylab = "Precio (USD)", xlab = "Año",   #Cambiar type = "o" para indicar los meses
     main = "Precio histórico BTC")
acf(btc.ts, main = "Serie no estacionaria")
plot(seriedif2)
acf(seriedif2, main = "Serie estacionaria")

#Prueba de Dickey-Fuller
adf.test(seriedif2, alternative = "stationary") #debemos tener un p-value menor a 0.05
#adf.test(seriedif2.1, alternative = "stationary") #debemos tener un p-value menor a 0.05

#Función de autocorrelación y autocorrelación parcial
layout(1:2)
acf(seriedif2)    #Número de medias móviles
pacf(seriedif2)   #Número de autorgresivos
acf(ts(seriedif2, frequency = 1))
pacf(ts(seriedif2, frequency = 1))
#Tenemos una media móvil, un autoregresivo y dos diferencias (1,2,1)
dev.off()

modelo1 <- arima(btc.ts, order = c(1,2,1)) #modelo arima con 1 autoregresivo, 2 diferencias y 1 media móvil
modelo1 #Coeficientes para (1,2,1)

tsdiag(modelo1) #diagnóstico
#Errores estandarizados deben parecer al ruido blanco.
#Valores p son mayores a 0.05

Box.test(residuals(modelo1), type = "Ljung-Box")
#Prueba Ljung-Box si p-value > 0.05 hay ruido blanco. Por lo tanto, el modelo de ajusta bien.
#Ruido blanco significa media = 0 y varianza constante.

error <- residuals(modelo1)
plot(error)
#Media igual a cero.

#Predicción:

#Predicción 1
pronostico <- forecast::forecast(modelo1, h = 12) #12 meses
pronostico  #Confianza del 80 y 95%
plot(pronostico)
#class(pronostico)

#Predicción 2
#Prueba Arima II
arima1<- Arima(btc.ts, order=c(0,1,2), seasonal=list(order=c(0,1,1),period=12))
arima2<- Arima(btc.ts, order=c(1,1,0), seasonal=list(order=c(2,1,0),period=12))
arima3<- Arima(btc.ts, order=c(1,1,2), seasonal=list(order=c(2,1,1),period=12))
arima4<- Arima(btc.ts, order=c(1,1,1), seasonal=list(order=c(2,1,1),period=12))
arima5<- Arima(btc.ts, order=c(1,1,2), seasonal=list(order=c(1,1,1),period=12))
arima6<- Arima(btc.ts, order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12))
arima7<- Arima(btc.ts, order=c(1,1,0), seasonal=list(order=c(1,1,0),period=12))

AIC(arima1,arima2,arima3,arima4,arima5,arima6,arima7)
BIC(arima1,arima2,arima3,arima4,arima5,arima6,arima7)
#Arima 2. Test menor valor, por ende, más significativo

autoplot(acf(arima2$residuals, plot = FALSE))
autoplot(pacf(arima2$residuals, plot = FALSE))

#Auto arima
auto.arima(btc.ts, stepwise = FALSE, approximation = FALSE)

forecast1 <- forecast(arima2, level = c(95), h = 12)
autoplot(forecast1)
class(forecast1)

x <- as.data.frame(forecast1)
write.table(x, file = "Predicción.csv", sep = ",", 
            row.names = TRUE, col.names = TRUE)

y <- read.table(file = "Predicción.csv", header = TRUE, sep = ",")


#En ambas predicciones el precio del bitcoin se mantiene a la alza.
