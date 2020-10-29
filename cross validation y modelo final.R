library(forecast)
library(smooth)

fallecidos=read.table('fallecidos2014-2020.csv',header = T,sep = ',')
fallecidos$ï..fecha=as.Date(fallecidos$ï..fecha,'%m/%d/%Y')


#cambiando los valores reportados por los de splines

fallecidos$Count[2265]=211
fallecidos$Count[2266]=212
fallecidos$Count[2267]=212
fallecidos$Count[2268]=212

fallecidos[2265:2268,]


fallecidos[2190,]

fallecidos[2205,]


i <- 2205   #### empezar con los primeros 6 años

error_suavizado <- c()
error_splines <- c()
error_med_movil <- c()
error_holtwinter <- c()


while(i < 2265){
  ts <- ts(fallecidos[1:i,]$Count, start=c(2014, 1), frequency=365)
  
  #suavizado exponencial
  suavizado=forecast(ts,robust = T)
  error_suavizado=rbind(error_suavizado,(sum((forecast(suavizado,h= 15,robust=T)$mean[1:15]-fallecidos[(i+1):(i+15),]$Count)^2)))
  
  #Splines
  smooth=smooth.spline(1:i,fallecidos[1:i,]$Count,df=25,all.knots=T)
  error_splines=rbind(error_splines,(sum((predict(smooth,(i+1):(i+15))$y-fallecidos[(i+1):(i+15),]$Count)^2)))
  
  #Medias moviles
  mediamovil1=sma(ts,10)
  predict(mediamovil1,h=15)$mean[1:15]
  error_med_movil=rbind(error_med_movil,(sum((predict(mediamovil1,h=15)$mean[1:15]-fallecidos[(i+1):(i+15),]$Count)^2)))
  
  #Holt winters
  modelo.HW=HoltWinters(ts)
  error_holtwinter=rbind(error_holtwinter,(sum((predict(modelo.HW,15)-fallecidos[(i+1):(i+15),]$Count)^2)))
  
  #Farrington
  #fallecidos.DisProg <- create.disProg(week = 1:i,
                                       #observed = fallecidos[1:i,]$Count,
                                       #state = rep(0,i),
                                       #start = c(2014, 1),freq = 365)
  #modelo.alg.farrington=algo.farrington(fallecidos.DisProg)
  #modelo.alg.farrington$upperbound
  #plot(modelo.alg.farrington)
  #1000+1197
  #x=modelo.alg.farrington$control$pd
  
  
  
  
  i = i + 15
}


mean(error_suavizado)
mean(error_splines)
mean(error_med_movil)
mean(error_holtwinter)

