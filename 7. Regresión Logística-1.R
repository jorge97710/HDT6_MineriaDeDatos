#library(e1071)

#install.packages("dummy)

library(caret)

library(dummy)

#Modelo de Regresión logística

porcentaje<-0.7

prueba <- perros5[,3:15]

prueba[14] <- perros5[24]

datos<-prueba

set.seed(123)


datos$y<- datos$AdoptionSpeed

#datos<-cbind(datos,dummy(datos,verbose = T))



corte <- sample(nrow(datos),nrow(datos)*porcentaje)

train<-datos[corte,]

test<-datos[-corte,]



modelo<-glm(4~, data = train[,c(1:15,8)],family = binomial(), maxit=100)
modelo<-glm(AdoptionSpeed_4~., data = train[,c(1:5,8)],family = binomial(), maxit=100)



#-------------------------------------------------

# Regresión Logistica 

#-------------------------------------------------



##Modelo con todas las variables

pred<-predict(modelo,newdata = test[,1:4], type = "response")

prediccion<-ifelse(pred>=0.5,1,0)

confusionMatrix(as.factor(test$AdoptionSpeed),as.factor(prediccion))
#tutorial http://www.postdata-statistics.com/IntroEstadistica/Tutoriales/Tutorial-13.pdf
#prueba 2
colX = 1
if(colX == 1){
  X = train[ , 14] 
  Y = train[ , 1] 
} else {
  X = train[ , 1] 
  Y = train[ , 14] 
}
datos = data.frame(X, Y)

colores = c()
colores[datos$Y == 0] = "grey"
colores[datos$Y == 1] = "black"
plot(datos$X, datos$Y, pch = 21, bg = colores, cex=1.3,font=2,
     xlab = "Indice de adopciones", ylab = "AdoptionSpeed", font.lab=2, )
legend("left", c("High Adoption Speed", "Low adoption"), pch = 21,
       pt.bg = c("grey", "black")) 
box(lwd=3)
Ysimul01 = ifelse(X > 2, yes = 0, no = 1)
datosSimul01 = data.frame(X, Y = Ysimul01)

colores = c()
colores[datosSimul01$Y == 0] = "grey"
colores[datosSimul01$Y == 1] = "black" 
plot(datosSimul01$X, datosSimul01$Y, pch = 21, bg = colores, cex=1.3,font=2, 
     xlab = "Indice de adopciones", ylab = "AdoptionSpeed", font.lab=2, )
legend("left", c("High Adoption Speed", "Low adoption"), pch = 21,
       pt.bg = c("grey", "black")) 
box(lwd=3) 
set.seed(2015)
Ysimul03 = sample(0:1, size = length(X), replace = TRUE)
datosSimul03 = data.frame(X, Y = Ysimul03)
colores = c() 
colores[datosSimul03$Y == 0] = "grey" 
colores[datosSimul03$Y == 1] = "black" 
plot(datosSimul03$X, datosSimul03$Y, pch = 21, bg = colores, cex=1.3,font=2, 
     xlab = "Indice de adopciones", ylab = "AdoptionSpeed", font.lab=2, )
legend("left", c("High Adoption Speed", "Low adoption"), pch = 21,
       pt.bg = c("grey", "black")) 
box(lwd=3) 
#agrupar valores para estimar las probabilidades
range(X)
a = -4
b = 0
numClases = 40 
(u = seq(from=a, to=b, length.out=(numClases + 1)))
(marcasClase = (u + ((b - a) / (2 * numClases)))[1:numClases])
clases = cut(X, breaks=u, include.lowest=TRUE)
head(clases, 10)
head(X, 10)
(claseElegida = levels(clases)[18])
X[clases == claseElegida]
(cualesSon = which(clases == claseElegida))
X[cualesSon]
Y[cualesSon]
(sumaYporClases = tapply(Y, INDEX=clases, FUN=sum))
(probabilidades = sumaYporClases / table(clases))
points(marcasClase, probabilidades, col="red", pch=2, lwd=2)
probab2Odds = log(probabilidades[probsAcotadas]/(1-probabilidades[probsAcotadas]))
(probsAcotadas = (probabilidades < 1) & (probabilidades > 0))
(Oddslm = lm(probab2Odds ~ marcasClase[probsAcotadas]))

curvaLogisticaMinCuad = function(x){
  b0 = Oddslm$coefficients[1] 
  b1 = Oddslm$coefficients[2] 
  return(exp(b0 + b1 * x)/(1 + exp(b0 + b1 * x))) 
}


points(marcasClase, probabilidades, col="red", pch=2, lwd=2) 
curve(curvaLogisticaMinCuad, from = -10, to = 10, col="blue", lwd="2", add=TRUE, lty="dotted")
