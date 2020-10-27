rm(list=ls()) 
#sE IMPORTA LA BASE DE DATOS
setwd("C:/Users/arelv/Documents/Github/primerparcial/")
luzsep <-read.csv("EXAMEN1.csv")


#INSTALAR LOS PAQUETES Y BIBLIOTECAS NECESARIOS
install.packages("plm")
install.packages("ggplot2")
install.packages("gplots")
library("ggplot2")
library("plm")

#La idea del trabajo es observar cuales son aquellas variables que influyen  para 
#poder observar la luminucidad del territorio mexicano, que proviene del satélite de VIIRS(NASA), 
#dentro de ellas se puede encontrar la nubosidad, día que se registro, municipio, etc. 

#la variable dependiente es el nivel de luminucidad=luz
#clave= Clave del municipio 
#t= tiempo 
#cve_ent 
#cve_mun        
#nom_mun      
#luz= Nivel de iluminacion    
#x0       
#x1
#x2       
#x3    
#x4       
#x5 
#mes 


#A)ESTABLECER LA BASE DE DATOS COMO PANEL 
panel<-pdata.frame(luzsep,index=c("clave","dia"))
head(panel)


#SE PUEDE HACER UNA REVISIÓN RÁPIDA DE LAS CARACTERÍSTICAS DE LAS VARIABLES Y DE 
#ESA MANERA OBSERVAR QUE TIPO DE VADRIABLES SE USAN
dim.data.frame(panel)
str(panel)

#B)Realizar una tabla de estadIsticas descriptivas de las variables 
summary.data.frame(panel[,c("luz" , "x0" , "x1" , "x2" , "x3" , "x4", "x5","mes")])




#se establace que el número total de municipios es 2458 ciendo un número constante,
#de tiempo son 61 días y el número total de datos observados para conocer la luminocidad en México
#es de 149938




#C)Realizar regresion pooled-OLS 
pooledOLS <- plm(luz ~ x0 + x1 + x2 + x3 + x4 + x5 + mes, data=luzsep)
summary(pooledOLS)


#En esta regresión se puede observar que las variables X1,X2,x3 y la variable mes son significativas
#lo que significa que por cada unidad que la luminocidad e y se pueda observar el día del mes influye 
#negativamente en 2.68 unididades, x4 también tiene una relación negativa más alta de 8.2 unidades 
#sin embargo no tiene significancia en la luminucidad del país, posiblemente la razón de esta situación
#se debe a que esta variable funciona como Dummy y los datos se repiten para el conjunto de municipios de
#para cada estado; por otro lado la variable que más afecta a la variable dependiente es x2, probablemente
#se relaciona directamente con el clima, ya sea lluvia o nubes.




#D)Determinar si la regresion es de efectos fijos o aleatorios 
#PARA EFECTOS FIJOS
EFECTOSFIJOS<-plm(luz ~ x0 + x1 + x2 + x3 + x4 + x5 + mes, data=luzsep, index=c("clave", "dia"), model="within")
summary(EFECTOSFIJOS)


EFECTOSALEATORIOS<-plm(luz ~ x0 + x1 + x2 + x3 + x4 + x5 + mes, data=luzsep, index=c("clave", "dia"), model="random")
summary(EFECTOSALEATORIOS)

#El resultado de estas regresiones, refleja que la variable x5 no es sinificativa en niguno de los dos casos, sin embargo
#si tienen un pvalue aceptable para aceptar los resultados, al igual que el resultado de bondad de ajuste con un valor 
#de Adj. R-Squared 0.98123 a 0.9871

#Se conprueba entonces cual de los dos modelos es el que mï¿½s funciona para entender el comportamiento de las variables
phtest(EFECTOSFIJOS, EFECTOSALEATORIOS)
#El resultado de esta operación, nos permite observar que uno de los modelos es inconsistente, a primera vista pareciera que
#el modelo que pudiera funcionar de mejor manera es el de efectos aleatorios por la significancia de las variables, 
#sin embargo  por la naturaleza del modelo de las variables y el número de datos lo más adecuado es usar los efectos fijos, 
#ya que la prueba de ht da como un resultado un p-value muy bajo, por lo que se rechaza la hipótesis de que el mejor modelo 
#para explicar sea de efectos aleatorios; otra explicación puede ser que el comportamiento o cambios en las variables de tiempo 
#que se conocen (día1, día2...o mes 1,mes2 ) son un término constante para cada municipio o estado del país.

#Bajo esta perspectiva, lo más adecuado es comprobar si existe heterocedasticida para después corregirla.



#E)Probar si la base de datos tiene heteroscedasticidad, si la tiene, corregirla 
#PRUEBA DE HETEROCEDASTICIDAD
library(lmtest)
bptest(EFECTOSFIJOS)
plot(y = panel$luz, x =panel$dia)
#Con base en el P-value y de manera grafica se puede observar que existe heterocedasticidad
#Lo que se puede observar con respecto a la heterocedasticidad, es una relación directa entre 
#la luminucidad y los dï¿½as que trasncurren, es decir, puede que en los días donde se oberva que 
#hay más concentración de datos es porque hubo menos nubes, el satelite pudo capturar mejor las
#imagenes, entre otros aspectos. 


# Para corregir HTCD 
VEF<- vcovHC(EFECTOSFIJOS, type="HC0")
summary(VEF)



#¿Qué otras maneras de presentar las variables se te ocurren para que la Adj:R2 aumente de valor?
#Se pueden trabajar algunas variables explicativas con las que se tuvo problemas de significancia
#lo que pudo interferir en la bondad de ajuste, en ese sentido, lo más adecuado es trabajar con logaritmos
#y observar que sucede con este dato
#La idea es conocer que sucede con las variable x0,x4,x5 ya que conforme se fueron haciendo las regresiones
#se pudo observar que son las que menos significancia tienen, incluso con estos cambias podríamos confirmar
#la importancia dentro del modelo para explicar la luminucidad en el país

panel$x4log <- log(panel$x4)
panel$x5log <- log(panel$x5)
panel$x0log <- log(panel$x0)
panel$luzlog <- log(panel$luz)


reglogx0 <- plm(luz ~ x0log + x1 + x2 + x3 + x4 + x5 + mes, data=panel)
summary(reglogx0)

reglogx4 <- plm(luz ~ x0 + x1 + x2 + x3 + x4log + x5 + mes, data=panel)
summary(reglogx4)

reglogx5 <- plm(luz ~ x0 + x1 + x2 + x3 + x4 + x5log + mes, data=panel)
summary(reglogx5)

#se observa que aunque estas variable se moodificaron, no tuvieron efecto contundente en AdjR2
#lo que se esperaba es que esta variable se hiciera significativa, lo que significa
#que estos datos no influyen en la luminucidad de los municipios como la variable x1 o x2 que 
#incluso las podrï¿½amos establecer como nï¿½mero de habitantes por municipio y la densidad poblacional
#respectivamente.


#Serï¿½a conveniente establecer una nueva regresión donde se establezacan logaritmos en las variables
#mencionadas al mismo tiempo e incluso la variable dependiente
regluz<- plm(luz ~ x0log + x1 + x2 + x3 + x4log + x5log + mes, data=panel)
summary(regluz)

#se observa de nueva cuenta que los cambios de estas variables no tuvieron efecto mayor para Adjr2
#De hecho esta medida tuvo un valor de 0.98123 que explica que de las variables que si son significativas,
#son lo suficientemente buenas y tienen alta capacidad de ajuste es decir que tiene un gran poder explicativo, 
#ya que se acerca a la unidad. Esto podría ser explicado de mejor manera si se supiera de manera clara a que se refiere 
#cada variable ya que de esta manera se entendería el propio comportamiento de cada una de ellas.
#Así mismo se reconoce que convertir las variables a logaritmos no es la mejor opción y/o se tiene que buscar otra manera
#para conocer a plenitud el comportamiento de esas variables.



