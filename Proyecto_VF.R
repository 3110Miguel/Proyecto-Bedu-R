library(quantmod)
library(forecast)
library(tseries)

getwd()
setwd("C:/Users/Miguel Angel/Documents/R/BEDU/Proyecto")

#Objetivo:
#El objetivo es el an�lisis, modelado y predicci�n de la serie temporal del precio de Bitcoin

#An�lisis:

#Datos de Yahoo Finance
bit <- getSymbols("BTC-USD", src = "yahoo", from = "2014-10-01", to = "2021-01-1", periodicity = "monthly") 
eth <- getSymbols("ETH-USD", src = "yahoo", from = "2015-09-01", to = "2021-01-1", periodicity = "monthly") 
ltc <- getSymbols("LTC-USD", src = "yahoo", from = "2014-10-01", to = "2021-01-1", periodicity = "monthly") 

#Gr�ficos historicos
chartSeries(`BTC-USD`, TA=NULL) 
chartSeries(`BTC-USD`, subset = "last 12 months") #Gr�fico �ltimos 12 meses
chartSeries(`ETH-USD`, TA=NULL) 
chartSeries(`ETH-USD`, subset = "last 12 months") #Gr�fico �ltimos 12 meses
chartSeries(`LTC-USD`, TA=NULL) 
chartSeries(`LTC-USD`, subset = "last 12 months") #Gr�fico �ltimos 12 meses

#Convertir a Data Frame
bit2 <- as.data.frame(`BTC-USD`)
eth <- as.data.frame(`ETH-USD`)
ltc <- as.data.frame(`LTC-USD`)

#Convertir a Serie de Tiempo
btc.ts <- ts(bit2$`BTC-USD.Adjusted`, start = c(2014,10), frequency = 12)
eth.ts <- ts(eth$`ETH-USD.Adjusted`, start = c(2015,9), frequency = 12)
ltc.ts <- ts(ltc$`LTC-USD.Adjusted`, start = c(2014,10), frequency = 12)

#Gr�fica 3 principales criptomonedas (Una sola gr�fica)
plot(cbind(btc.ts, eth.ts, ltc.ts), plot.type = "single", col = 1:3,
     main = "Precio principales criptomonedas", 
     xlab = "A�o",
     sub = "2014-2020")
legend("topleft", colnames(cbind(btc.ts, eth.ts, ltc.ts)), col = 1:3, lty = 1)

#Gr�ficas 3 pirncipales criptomonedas
plot(cbind(btc.ts, eth.ts, ltc.ts),
     main = "Precio principales criptomonedas", 
     xlab = "A�o",
     sub = "2014-2020")
#Se puede observar que las monedas tienen un comportamiento similar a trav�s del tiempo.
#Las tres tuvieron una subida significante en 2017. Y han superado su m�ximo historico en los �ltimos meses. Excepto LTC.
#�Qu� relaci�n existe entre las tres criptomonedas? �Existe relaci�n o casualidad en su comportamiento?
#Hip�tesis: �Continur� bitcoin a la alza o tendr� una correci�n semejante a la del a�o 2018?

#Gr�fica Bitcoin
plot(btc.ts, type = "l", ylab = "Precio (USD)", xlab = "A�o",   #Cambiar type = "o" para indicar los meses
     main = "Precio hist�rico BTC",
     sub = "1-10-2014 a 1-1-2020")

boxplot(btc.ts ~ cycle(btc.ts),
        xlab = "Boxplot de valores estacionales",
        sub = "Precio  2014-2020",
        main = "Precios hist�ricos de BTC")
#En esta gr�fica observamos que los meses 1 y 12 tienen valores at�picos.
#Son estos meses donde el precio a subido de manera exponencial.
#En el mes 3 tenemos el menor IQR, lo que indica una menor dispersi�n de datos.
#Por lo tanto, es en marzo donde existe menos volatilidad en el precio.
#Por otra parte, todos cuentan con m�nimos no at�picos semejante.

#Tendencia (regresi�n lineal) 
ns.btc <- window(btc.ts, start = c(2018,10), end = c(2021, 1))       #Ventana para escoger fecha
nt.btc <- time(ns.btc)
plot(ns.btc, xlab = "A�o", ylab = "Precio (USD)", main = "Precios hist�ricos de BTC",
     sub = "2014-2020"); abline(reg = lm (ns.btc~nt.btc))


#Descomposic�n de la serie de tiempo

#Modelo aditivo
DA.BTC <- decompose((btc.ts))
plot(DA.BTC, xlab = "A�os",
     sub = "Descomposici�n de los datos del precio de BTC")
#Se aprecia que no tenemos una varianza constante, por lo cual, el modelo aditivo no es de utilidad.

#Componentes
Tendencia <- DA.BTC$trend
Estacionalidad <- DA.BTC$seasonal
Aleatorio <- DA.BTC$random
#plot(Estacionalidad)
ts.plot(cbind(Tendencia, Tendencia + Estacionalidad), 
        xlab = "A�os", main = "Datos precio hist�rico BTC", 
        ylab = "Precio $USD", lty = 1:2,
        sub = "Tendencia con efectos estacionales aditivos sobrepuestos")


#Modelo multiplicativo
DM.BTC <- decompose(btc.ts, type = "mult")
plot(DM.BTC, xlab = "A�os", 
     sub = "Descomposici�n de los datos del precio del BTC")
#Componentes
Trend <- DM.BTC$trend
Seasonal <- DM.BTC$seasonal
Random <- DM.BTC$random

ts.plot(cbind(Trend, Trend*Seasonal), xlab = "A�os", main = "Datos precio hist�rico BTC", 
        ylab = "Precio $USD", lty = 1:2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")

seasonplot(btc.ts, col = rainbow(12), year.labels = TRUE)  #Gr�fica de estacionalidad
#Se mantiene una media y varianza no constantes

#Modelado:

#Funci�n de autocorrelaci�n
acf(btc.ts)
#La serie no es estacionaria en media, hay muchos valores fuera de los rangos (l�neas punteadas)

ndiffs(btc.ts)  #Saber cuantas diferencias necesito #2

#Diferenciaci�n 1
seriedif <- diff(btc.ts)
plot(seriedif)
#Se puede observar que comienza a obtener estacionalidad, la media est� en cero. Los datos est�n alrededor de �sta.

acf(seriedif) #
ndiffs(seriedif) #Falta una diferenciaci�n

#Diferenciaci�n 2
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

#An�lisis visual de las gr�ficas de autocorrelaci�n
par(mfrow =c(2,2), mar = c(4,4,4,1)+.1)
plot(btc.ts, type = "o", ylab = "Precio (USD)", xlab = "A�o",   #Cambiar type = "o" para indicar los meses
     main = "Precio hist�rico BTC")
acf(btc.ts, main = "Serie no estacionaria")
plot(seriedif2)
acf(seriedif2, main = "Serie estacionaria")

#Prueba de Dickey-Fuller
adf.test(seriedif2, alternative = "stationary") #debemos tener un p-value menor a 0.05
#adf.test(seriedif2.1, alternative = "stationary") #debemos tener un p-value menor a 0.05

#Funci�n de autocorrelaci�n y autocorrelaci�n parcial
layout(1:2)
acf(seriedif2)    #N�mero de medias m�viles
pacf(seriedif2)   #N�mero de autorgresivos
acf(ts(seriedif2, frequency = 1))
pacf(ts(seriedif2, frequency = 1))
#Tenemos una media m�vil, un autoregresivo y dos diferencias (1,2,1)
dev.off()

modelo1 <- arima(btc.ts, order = c(1,2,1)) #modelo arima con 1 autoregresivo, 2 diferencias y 1 media m�vil
modelo1 #Coeficientes para (1,2,1)

tsdiag(modelo1) #diagn�stico
#Errores estandarizados deben parecer al ruido blanco.
#Valores p son mayores a 0.05

Box.test(residuals(modelo1), type = "Ljung-Box")
#Prueba Ljung-Box si p-value > 0.05 hay ruido blanco. Por lo tanto, el modelo de ajusta bien.
#Ruido blanco significa media = 0 y varianza constante.

error <- residuals(modelo1)
plot(error)
#Media igual a cero.

#Predicci�n:

#Predicci�n 1
pronostico <- forecast::forecast(modelo1, h = 12) #12 meses
pronostico  #Confianza del 80 y 95%
plot(pronostico)
#class(pronostico)

#Predicci�n 2
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
#Arima 2. Test menor valor, por ende, m�s significativo

autoplot(acf(arima2$residuals, plot = FALSE))
autoplot(pacf(arima2$residuals, plot = FALSE))

#Auto arima
auto.arima(btc.ts, stepwise = FALSE, approximation = FALSE)

forecast1 <- forecast(arima2, level = c(95), h = 12)
autoplot(forecast1)
class(forecast1)

x <- as.data.frame(forecast1)
write.table(x, file = "Predicci�n.csv", sep = ",", 
            row.names = TRUE, col.names = TRUE)

y <- read.table(file = "Predicci�n.csv", header = TRUE, sep = ",")


#En ambas predicciones el precio del bitcoin se mantiene a la alza.
