library(ggplot2)
library(dplyr)
require(corrplot)
require(psych)
require(GGally)
require(car)
library(lmtest)

getwd()
setwd("C:/Users/Miguel Angel/Documents/R/BEDU/Proyecto")

df_BTC <- read.csv("C:/Users/Miguel Angel/Documents/R/BEDU/Proyecto/BTC-USD.csv")
df_ETH <- read.csv("C:/Users/Miguel Angel/Documents/R/BEDU/Proyecto/ETH-USD.csv")
df_LTC <- read.csv("C:/Users/Miguel Angel/Documents/R/BEDU/Proyecto/LTC-USD.csv")


#Limpieza Data Frame

df_BTC <- select(df_BTC, Date, Open, Close)                                     # Fecha, Abre, Cierra
df_BTC <- rename(df_BTC, Fecha = Date, Apertura_BTC = Open, Cierre_BTC = Close)
df_BTC <- mutate(df_BTC, Fecha = as.Date(Fecha, "%Y-%m-%d"))
df_BTC <- mutate(df_BTC, Apertura_BTC = Apertura_BTC*19.97)                     #Precio del dolar al 24-01-2021
df_BTC <- mutate(df_BTC, Cierre_BTC = Cierre_BTC*19.97)                         #Precio del dolar al 24-01-2021
#Grupo <- sample(c('Bitcoin'), size = 2316, replace = TRUE)
#df_BTC <- cbind(df_BTC,Grupo)
#df_BTC <- mutate(df_BTC, Grupo = factor(Grupo), Grupo = factor(Grupo))
View(df_BTC)

df_ETH <- select(df_ETH, Date, Open, Close)                                     # Fecha, Abre, Cierra
df_ETH <- rename(df_ETH, Fecha = Date, Apertura_ETH = Open, Cierre_ETH = Close)
df_ETH <- mutate(df_ETH, Fecha = as.Date(Fecha, "%Y-%m-%d"))
df_ETH <- mutate(df_ETH, Apertura_ETH = Apertura_ETH*19.97)                     #Precio del dolar al 18-01-2021
df_ETH <- mutate(df_ETH, Cierre_ETH = Cierre_ETH*19.97)                         #Precio del dolar al 18-01-2021
#Grupo <- sample(c('Ethereum'), size = 1991, replace = TRUE)
#df_ETH <- cbind(df_ETH,Grupo)
#df_ETH <- mutate(df_ETH, Grupo = factor(Grupo), Grupo = factor(Grupo))
View(df_ETH)

df_LTC <- select(df_LTC, Date, Open, Close)                                     # Fecha, Abre, Cierra
df_LTC <- rename(df_LTC, Fecha = Date, Apertura_LTC = Open, Cierre_LTC = Close)
df_LTC <- mutate(df_LTC, Fecha = as.Date(Fecha, "%Y-%m-%d"))
df_LTC <- mutate(df_LTC, Apertura_LTC = Apertura_LTC*19.97)                     #Precio del dolar al 18-01-2021
df_LTC <- mutate(df_LTC, Cierre_LTC = Cierre_LTC*19.97)                         #Precio del dolar al 18-01-2021
#Grupo <- sample(c('Litecoin'), size = 2316, replace = TRUE)
#df_LTC <- cbind(df_LTC,Grupo)
#df_LTC <- mutate(df_LTC, Grupo = factor(Grupo), Grupo = factor(Grupo))
View(df_LTC)

#Gráfica BTC
#ggplot(df_BTC, aes(x=Fecha, y = Precio_Final_BTC, )) + 
  geom_point() +
  ggtitle('Historico BTC') +
  xlab('Fecha')+
  ylab('Precio')
#Gráfica ETH
#ggplot(df_ETH, aes(x=Fecha, y = Precio_Final_ETH, )) + 
  geom_point() +   
  ggtitle('Historico ETH') +
  xlab('Fecha')+
  ylab('Precio')
#Gráfica LTC
#ggplot(df_LTC, aes(x=Fecha, y = Precio_Final_LTC, )) + 
  geom_point() +
  ggtitle('Historico LTC') +
  xlab('Fecha')+
  ylab('Precio')

#Unir df's
#df1 <- merge(df_BTC, df_ETH, by = 'Fecha')
#str(df1)
#df2 <-merge(df1,df_LTC, by = 'Fecha')
#str(df2)
#View(df2)

df <- Reduce(merge, list(df_BTC,df_ETH,df_LTC))
str(df)
#View(df)

#Correlación
attach(df)
#plot(df2)
pairs(df)                         #Matriz con gráficos de dispersión de todas las variables del set de datos


# Matriz de correlación entre predictores
round(cor(subset(df, select = -Fecha), method = "pearson"), digits = 2)

corrplot(round(cor(subset(df, select = -Fecha)), digits = 3), type = "lower")

# Distribución de densidad de las variables cuantitativas del modelo
#multi.hist(x = select(df, -Fecha), dcol = c("blue", "red"), 
           dlty = c("dotted", "solid"), main = "" )

#Matriz dispersión, distribución y correlación
ggpairs(select(df, - Fecha), lower = list(continuous = "smooth"), 
        diag = list(continuous = "bar"), axisLabels = "none")

#Modelo líneal
ml <- lm(Cierre_BTC ~ .-Fecha, data = df)
summary(ml)

#Selección de predictores
#Calidad del modelo
step(ml, direction = "both", trace = 1)

#Intervalos de confianza
confint(ml)

#Validación
par(mfrow=c(2,2))
plot(ml)
dev.off()

# Detección y visualización de observaciones influyentes
influencePlot(ml)

# Gráfico residuos estudentizados frente a valores ajustados por el modelo
ggplot(data = df, aes(x = predict(ml), 
                        y = abs(rstudent(ml))))+
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed")+
  # se identifican en rojo las observaciones con residuos estandarizados absolutos > 3
  geom_point(aes(color = ifelse(abs(rstudent(ml)) > 3, "red", "black")))+
  scale_color_identity()+
  labs(title = "Distribución de los residuos estudentizados", 
       x = "Predicción modelo", 
       y = "Residuos estudentizados")+
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

# Detección de los residuos estudentizados > 3 considerados como outliers
which(rstudent(ml) > 3)

outlierTest(ml)

# Test de hipótesis para el análisis de normalidad de los residuos
shapiro.test(ml$residuals)

# Test de contraste de homocedasticidad Breusch-Pagan
bptest(ml)

corrplot(cor(select(df, Cierre_BTC, Apertura_BTC, Apertura_ETH, Apertura_LTC, Cierre_ETH, 
                    Cierre_LTC)), method = "number", type = "lower")

# Factores de inflación de la varianza
vif(ml)

#Pruebas correlación
ggplot(df, aes(x=Apertura_ETH, y = Cierre_BTC, )) + 
  geom_point() +
  ggtitle('Relación BTC-ETH') +
  xlab('ETH')+
  ylab('BTC')
(RBE <- ggplot(df, aes(x = Apertura_BTC, y = Cierre_BTC)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = T))  # modelo lineal, cambia el parametro `se`, este hace referencia al intervalo de confianza

ggplot(df2, aes(x=Precio_Final_LTC, y = Precio_Final_BTC, )) + 
  geom_point() +
  ggtitle('Relación BTC-LTC') +
  xlab('LTC')+
  ylab('BTC')
(RBE <- ggplot(df2, aes(x = Precio_Final_LTC, y = Precio_Final_BTC)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = T))

ggplot(df2, aes(x=Precio_Final_LTC, y = Precio_Final_ETH, )) + 
  geom_point() +
  ggtitle('Relación ETH-LTC') +
  xlab('LTC')+
  ylab('ETH')
(RBE <- ggplot(df2, aes(x = Precio_Final_LTC, y = Precio_Final_ETH)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = T))

#Correlación
round(cor(df2),2) #Redondeo de dos decimales

cor(x=df2$Precio_Final_ETH,
    y=df2$Precio_Final_BTC)
cor(x=df2$Precio_Final_ETH,
    y=df2$Precio_Final_BTC,
    method= 'spearman')

cor(x=df2$Precio_Final_LTC,
    y=df2$Precio_Final_BTC)
cor(x=df2$Precio_Final_LTC,
    y=df2$Precio_Final_BTC,
    method='spearman')

cor(x=df2$Precio_Final_LTC,
    y=df2$Precio_Final_ETH)
cor(x=df2$Precio_Final_LTC,
    y=df2$Precio_Final_ETH,
    method='spearman')

cor(df2) #quitar todos los datos no numéricos


x <- ts(df_BTC, start = 2014, frequency = 365)
plot(x)
x
y <- ts(df2, start= 2015, frequency = 1)
y

######



head(df2, 4); tail(df2, 4); dim(df2)


