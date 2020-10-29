library(forecast)
library(smooth)
#### Proceso para estimar 16 17 18 y 19 de marzo  #######
#se realiza el modelo hasta el 28 de febrero y pronostico 1 al 15 de marzo
fallecidos=read.table('fallecidos2014-2020.csv',header = T,sep = ',')
fallecidos$ï..fecha=as.Date(fallecidos$ï..fecha,'%m/%d/%Y')

#fijar los datos hasta el 28 de febrero de 2020
fallecidos[2249,]
fallecidos.corte=fallecidos[1:2249,]

#Convertir los datos de 2020 a time series
ecu.ts<-ts(fallecidos.corte$Count, start = c(2014,1), frequency = 365)

#ajustar modelo de suavizado exponencial
modelo1=forecast(ecu.ts,robust = T)
summary(modelo1)
prediccion.modelo1=forecast(modelo1,h=15)
prediccion.modelo1=as.numeric(prediccion.modelo1$mean)
reales=fallecidos[2250:2264,]$Count
errorSuav=sqrt(sum((prediccion.modelo1-reales)^2)/15) #17.44346

#ETS ANN
ann=ets(ecu.ts,model = 'ANN', allow.multiplicative.trend = allow.multiplicative.trend)
error_ann <- sqrt(sum((reales-forecast(ann,h=15)$mean[1:15])^2)/15)#12.09731 

#ETS AAN
aan=ets(ecu.ts,model = 'AAN', allow.multiplicative.trend = allow.multiplicative.trend)
error_aan <- sqrt(sum((reales-forecast(aan,h=15)$mean[1:15])^2)/15)#12.09392

#ETS MNN
mnn=ets(ecu.ts,model = 'MNN', allow.multiplicative.trend = allow.multiplicative.trend)
error_mnn <- sqrt(sum((reales-forecast(mnn,h=15)$mean[1:15])^2)/15)#11.40092

#ETS AAA
aaa=ets(ecu.ts,model = 'AAA', allow.multiplicative.trend = allow.multiplicative.trend)
error_aaa <- sqrt(sum((reales-forecast(aaa,h=15)$mean[1:15])^2)/15)#ERROR

#ETS ANA
ana=ets(ecu.ts,model = 'ANA', allow.multiplicative.trend = allow.multiplicative.trend)
error_ana <- sqrt(sum((reales-forecast(ana,h=15)$mean[1:15])^2)/15)#ERROR 

#ETS MAN
man=ets(ecu.ts,model = 'MAN', allow.multiplicative.trend = allow.multiplicative.trend)
error_man <- sqrt(sum((reales-forecast(man,h=15)$mean[1:15])^2)/15)#11.79285

#ARIMA
arima1=arima(ecu.ts,c(1,1,1))    #Modelo sugerido por el autoarima
#prediccion del 1 al 15 de marzo
prediccion.modelo1=forecast(arima1,h=15)
prediccion.modelo1=as.numeric(prediccion.modelo1$mean)
errorarima=sqrt(sum((prediccion.modelo1-reales)^2)/15)#11.54464

##Splines
smooth=smooth.spline(1:2249,fallecidos.corte$Count,df=25,all.knots=T)
#Predicciones del 1 al 15 de marzo
predict(smooth,2250:2264)
errorspli=sqrt(sum((predict(smooth,2250:2264)$y-reales)^2)/15)#[1] 12.42046

## media movil simple
mediamovil1=sma(ecu.ts,10)
#Predicciones del 1 al 15 de marzo
predict(mediamovil1,h=15)$mean[1:15]
errormed=sqrt(sum((predict(mediamovil1,h=15)$mean[1:15]-reales)^2)/15)#15.4087

# se ajusta hasta el 15 de marzo con mejor modelo ETS MNN
fallecidos[2264:2268,]
fallecidos.corte=fallecidos[1:2264,]
ecu.ts<-ts(fallecidos.corte$Count, start = c(2014,1), frequency = 365)
#predecir 16 17 18 19 de marzo
predict(mnn,4)



#### Proceso para estimar 13,14,15,16 17 18 y 19 de marzo
#se realiza el modelo hasta el 28 de febrero y pronostico 1 al 12 de marzo
fallecidos=read.table('fallecidos2014-2020.csv',header = T,sep = ',')
fallecidos$ï..fecha=as.Date(fallecidos$ï..fecha,'%m/%d/%Y')

#fijar los datos hasta el 28 de febrero de 2020
fallecidos[2249,]
fallecidos.corte=fallecidos[1:2249,]

#Convertir los datos de 2020 a time series
ecu.ts<-ts(fallecidos.corte$Count, start = c(2014,1), frequency = 365)

#Se ajustan los modelos para predecir los 12 primeros dias de marzo
#ajustar modelo de suavizado exponencial
modelo1=forecast(ecu.ts,robust = T)
prediccion.modelo1=forecast(modelo1,h=12,robust = T)
prediccion.modelo1=as.numeric(prediccion.modelo1$mean)
reales=fallecidos[2250:2261,]$Count
errorSuav=sqrt(sum((prediccion.modelo1-reales)^2)/12) #16.19176

#ETS ANN
ann=ets(ecu.ts,model = 'ANN', allow.multiplicative.trend = allow.multiplicative.trend)
error_ann <- sqrt(sum((reales-forecast(ann,h=12)$mean[1:12])^2)/12)#10.36825 

#ETS AAN
aan=ets(ecu.ts,model = 'AAN', allow.multiplicative.trend = allow.multiplicative.trend)
error_aan <- sqrt(sum((reales-forecast(aan,h=12)$mean[1:12])^2)/12)#10.36257

#ETS MNN
mnn=ets(ecu.ts,model = 'MNN', allow.multiplicative.trend = allow.multiplicative.trend)
error_mnn <- sqrt(sum((reales-forecast(mnn,h=12)$mean[1:12])^2)/12)#9.664209

#ETS MAN
man=ets(ecu.ts,model = 'MAN', allow.multiplicative.trend = allow.multiplicative.trend)
error_man <- sqrt(sum((reales-forecast(man,h=12)$mean[1:12])^2)/12)#9.939576

#ARIMA
arima1=arima(ecu.ts,c(1,1,1))#aca tambien dió el mismo orden con el autoarima 
#prediccion del 1 al 12 de marzo
prediccion.modelo1=forecast(arima1,h=12)
prediccion.modelo1=as.numeric(prediccion.modelo1$mean)
errorarima=sqrt(sum((prediccion.modelo1-reales)^2)/12)#9.831572

##Splines
smooth=smooth.spline(1:2249,fallecidos.corte$Count,df=25,all.knots=T)
#Predicciones del 1 al 12 de marzo
predict(smooth,2250:2261)
errorspli=sqrt(sum((predict(smooth,2250:2261)$y-reales)^2)/12)#10.49726


## media movil simple
mediamovil1=sma(ecu.ts,10)
#Predicciones del 1 al 12 de marzo
predict(mediamovil1,h=12)$mean[1:12]
errormed=sqrt(sum((predict(mediamovil1,h=12)$mean[1:12]-reales)^2)/12)#13.83225


# se ajusta hasta el 12 de marzo con mejor modelo ETS MNN
fallecidos[2264:2261,]
fallecidos.corte=fallecidos[1:2261,]
ecu.ts<-ts(fallecidos.corte$Count, start = c(2014,1), frequency = 365)
#ETS MNN
mnn=ets(ecu.ts,model = 'MNN', allow.multiplicative.trend = T)
predict(mnn,7)

#Debido a que se observó una disminución de fallecidos en los últimos días de diciembre del 2019
#y primero de enero 2020 da un exceso muy alto en los primeros dias del 2020
# por lo que se decide ajustar los días mencionados
###del 24 dic 2019 al 1 ene 2020 200

#fijar 1 de enero 2019 hasta 23 de dic 2019
fallecidos[2182,]
fallecidos.corte=fallecidos[1826:2182,]
ecu.ts<-ts(fallecidos.corte$Count, start = c(2014,1), frequency = 365)
mnn=ets(ecu.ts,model = 'MNN', allow.multiplicative.trend = allow.multiplicative.trend)
forecast(mnn,h=9)
