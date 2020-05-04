#install.packages("rapportools")
#install.packages("DescTools")
#install.packages("normtest")
#install.packages("nortest")
#install.packages("car")
#install.packages("ggplot2")
#install.packages("descr")


#DIRECTORIO DE TRABAJO: Establecemos nuestro directorio de trabajo
setwd("D:/Dropbox/Cursos pregrado/Métodos Cuantitativos_2020_I/talleres/ANOVA")
  
#CARGANDO LOS DATOS
#Abrimos la librería para leer archivos en Excel 
library(readxl)

#Abrimos la base de datos de imacecs
imacec_sa <- read_excel("imacec_sa.xlsx")

#Visualizamos la base de datos (se nos abrirá una ventana mostrando la base de datos)
View(imacec_sa)

#######################################################
#REVISANDO LOS DATOS
#Solicitamos a R una descripción de la base de datos con la función str():
str(imacec_sa)

#Si indicamos el nombre del archivo R nos despliega en pantalla directamente las primeras filas de datos
imacec_sa

#Ahora adjuntamos la base de datos. Esto sirve para que cuando nombremos una variable de la base R la reconozca directamente
attach(imacec_sa)

#También podemos pedirle a R que nos muestre las variables que hay en la base usando el comando names()
names(imacec_sa)

#Le consultamos a R por el tipo de variables que tenemos usando el comando class() 
class(imacec) 
class(month)
class(si_ratio)

#Le solicitamos a R las estadísticas sumarias de la base de datos
summary(imacec_sa)

#También le podemos pedir las estadísticas sumarias de una variable específica
summary(imacec)
summary(si_ratio)

#Otra alternativa para estadísticas sumarias es usaR la librería descr, que permite también hacer tablas cruzadas:
library(descr)
descr(imacec_sa)

#Vamos a generar ahora una nueva variable a la base de datos para comprobar que es si_ratio
imacec_sa$si_checking<-(imacec/imacec_d12)*100


########################################################
#ANÁLISIS GRÁFICO

#Histograma
hist(imacec_sa$si_ratio, col = "steelblue", breaks = 50)


#DENSIDAD
# Computamos el gráfico de densidad
dens <- density(imacec_sa$si_ratio)
# Generamos el gráfico de densidad
plot(dens, frame = FALSE, col = "steelblue", 
     main = "Densidad del del componente SI del imacec") 
#Lo coloreamos con el comando polygon()
polygon(dens, col = "steelblue")

#QQ-Plot para comparar normalidad
qqnorm(imacec_sa$si_ratio)
qqline(imacec_sa$si_ratio, col = "steelblue", si_ratiod = 2)
#Incorporamos un intervalo de confianza al 95% en el QQ-Plot
library(car)
qqPlot(imacec_sa$si_ratio)

#BOXPLOT: Comparamos el SI_ratio del imacec según el nivel de escolaridad en gráficos de cajas
#Pero antes generamos la variable mes para que R la reconozca como un factor

imacec_sa$month<- factor(imacec_sa$month, levels = c('enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio', 'julio','agosto', 'septiembre', 'octubre', 'noviembre', 'diciembre'))

#Ahora vamos a cambiar las etiquetas de los meses por etiquetqs más cortas
levels(imacec_sa$month)
levels(imacec_sa$month) <- c("ene", "feb", "mar", "abr", "may", "jun", "jul","ago", "sep", "oct", "nov", "dic")
levels(imacec_sa$month)

#Finalmente hacemos el boxplot
boxplot(si_ratio~month, data = imacec_sa, frame = FALSE, col = "steelblue")
#Inertamos una línea horizontal en la media de cada mes
abline(h=mean(imacec_sa$si_ratio),lty=3,col=4)
summary(si_ratio)

#Otra forma de hacer gráfico de boxplot es con la librería ggplot
library(ggplot2)
ggplot(data = imacec_sa, aes(x = month, y = si_ratio, colour = month)) +
  geom_boxplot() +
  geom_point() +
  geom_hline(yintercept=mean(imacec_sa$si_ratio),lty=5, col = "darkred") +
  theme_bw() +
  theme(legend.position = "none") 

#########################################################
#ANOVA DE UN FACTOR
#Implementamos la prueba ANOVA de un factor
modelo = lm(imacec_sa$si_ratio~imacec_sa$month)
anova(modelo)

#Gráficos de homocedasticidad y normalidad
par(mfrow = c(1,2), mar=c(5,5,2,1))
for (i in 1:2) {
  plot(modelo, which = i)
}
par(mfrow=c(1,1))

#Otra manera de hacerlo es la siguiente
anova_month=aov(si_ratio~month)
summary(anova_month)

#Test de Tukey
TukeyHSD(anova_month)

#Test de scheffé
require("DescTools")
ScheffeTest(anova_month)

#Test de Bonferrroni
pairwise.t.test(imacec_sa$si_ratio, imacec_sa$month, p.adjust.method = "bonferroni")






#####################################
#PRUEBAS DE NRMALIDAD

library(nortest) 

#Kolmogorov-Smirnov-Lilliefors
lillie.test(imacec)
lillie.test(si_ratio)

library(normtest)

#Test de Asimetría 
skewness.norm.test(imacec)
skewness.norm.test(si_ratio)

#Test de Curtosis
kurtosis.norm.test(imacec)
kurtosis.norm.test(si_ratio)

#Jarque-Bera
jb.norm.test(imacec)

######################################
#PRUEBAS DE HOMOGENIDAD DE VARIANZA

#Test de Bartlett
bartlett.test(imacec_sa$si_ratio~imacec_sa$month)

#Test de Levene
library(car)
leveneTest(imacec_sa$si_ratio~imacec_sa$month)

######################################
#CONTRASTE DE KRUSKAL-WALLIS
kruskal.test(imacec_sa$si_ratio~imacec_sa$month)


