#leemos el fichero
library(readr)
datos <- read_csv("datos.csv")
peso= na.omit(datos$Peso)
altura=na.omit(datos$Altura)
###########################

#ejercicio 1
b=covarianza(altura,peso)/varianza(altura)
a=mean(peso)-b*mean(altura)
plot(altura,peso,xlab = "altura",ylab = "peso")
abline(a,b)
##covarianza
covarianza <- function(x,y){
  n=length(x)
  covarianza1 = (sum(x*y))/n-(mean(x)*mean(y))
  return(covarianza1)
}
#variancia
varianza<-function(x){
  n=length(x)
  varianza1= sum(x**2)/n-(mean(x)**2)
  return(varianza1)
}
#ejercicio 2
regresion= lm(peso ~ altura)
plot(altura,peso,xlab = "altura",ylab = "peso")
abline(regresion)
#ejercicio3
library(ggplot2)
cuadrodedatos= data.frame(altura,peso)
ggplot(cuadrodedatos,aes(x = cuadrodedatos$altura,y = cuadrodedatos$peso))+geom_point(color= "blue")+geom_line(color="green",aes(y = predict(regresion)))
#ejercicio4
library(readxl)
cardata <- read_excel("cardata.xlsx")
View(cardata)
consumo=as.numeric(cardata$mpg)
potencia=as.numeric(cardata$horsepower)
aceleracion=as.numeric(cardata$accel)
peso=as.numeric(cardata$weight)
precio=as.numeric(cardata$price)

regresion= lm(potencia ~ consumo)
plot(consumo,potencia,xlab = "consumo",ylab = "potencia")
abline(regresion)
cor(consumo,potencia,use = "na.or.complete")

regresion= lm(aceleracion ~ consumo)
plot(consumo,aceleracion,xlab = "consumo",ylab = "aceleracion")
abline(regresion)
cor(consumo,aceleracion,use = "na.or.complete")

regresion= lm(peso ~ consumo)
plot(consumo,peso,xlab = "consumo",ylab = "peso")
abline(regresion)

cor(consumo,peso,use = "na.or.complete")

regresion= lm(precio ~ consumo)
plot(consumo,precio,xlab = "consumo",ylab = "precio")
abline(regresion)
cor(consumo,precio,use = "na.or.complete")
#ejercio 5
#la recta que mejor se ajusta es la del peso con el consumo porque su indice correlacion es el que mas se ajusta a menos 
#uno -0,82

