#######################################################################################
##################ESTE ES UN SCRIPT BÁSICO PARA ESTIMAR ANOVA##########################
#######################################################################################

#En este ejemplo queremos saber si el factor estacional incide en el comportamiento del si_ratio del imacec#

#DEFINIMOS EL DIRECTORIO DE TRABAJO:

setwd("D:/Dropbox/Cursos pregrado/Métodos Cuantitativos_2020_I/talleres/ANOVA")

#ABRIMOS LA BAS DE DATOS
library(readxl)
imacec_sa <- read_excel("D:/Dropbox/Cursos pregrado/Métodos Cuantitativos_2020_I/talleres/ANOVA/imacec_sa.xlsx")
View(imacec_sa)

#Ahora adjuntamos la base de datos. Esto sirve para que cuando nombremos una variable de la base R la reconozca directamente
attach(imacec_sa)

#Le solicitamos a R las estadísticas sumarias de la base de datos
summary(imacec_sa)

########################################################################################
#IMPLEMENTAOS LA PRUEBA DE ANOVA DE UN FACTOR:
anova_month<-aov(si_ratio~month)
summary(anova_month)

########################################################################################

#Hacemos un gráfico de caja (boxplot) para ver como se ubican los distintos grupos de tratamiento del factor
boxplot(si_ratio~month)

#Vemos que laas cajas de los gráficos aparecen en desorden. Vamos a generar un factor para que R reconozca el orden:
month<- factor(month, levels = c('enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio', 'julio','agosto', 'septiembre', 'octubre', 'noviembre', 'diciembre'))

#Ahora vamos a cambiar las etiquetas de los meses por etiquetqs más cortas
levels(month)
levels(month) <- c("ene", "feb", "mar", "abr", "may", "jun", "jul","ago", "sep", "oct", "nov", "dic")
levels(month)

#Ahora volvemos a correr el boxplot pEro ya ordenado:
boxplot(si_ratio~month)
#Coloreamos las cajas en azul ténue:
boxplot(si_ratio~month, col = "steelblue")
#Le quitamos el marco al gráfico y coloriamaos en naranjo:
boxplot(si_ratio~month, frame = FALSE, col = "orange")
#Djémolo ahora con el marco del gráfico e insertamos una línea horizontal en la media global
boxplot(si_ratio~month, col = "steelblue")
abline(h=mean(imacec_sa$si_ratio),lty=3,col="red")
#Comprobemos que es la media:
summary(si_ratio)

#Hacemos las pruebas de normalidad:

#####################################
#PRUEBAS DE NORMALIDAD

#Primero tenemos que cargar dos librerías "nortest" y "normtest"

#Ahora implementamos los test:

#Kolmogorov-Smirnov-Lilliefors (con la librería "nortest")
library(nortest) 
lillie.test(si_ratio)

#Test de Asimetría (con la librería "normtest")
library(normtest)
skewness.norm.test(si_ratio)

#Test de Curtosis
kurtosis.norm.test(si_ratio)

#Jarque-Bera
jb.norm.test(si_ratio)

######################################
#PRUEBAS DE HOMOGENIDAD DE VARIANZA

#Test de Bartlett
bartlett.test(si_ratio~month)

#Test de Levene (Ojo:Primero cargamos la librería "car")
library(car)
leveneTest(si_ratio~month)

######################################
#CONTRASTE DE KRUSKAL-WALLIS
kruskal.test(si_ratio~month)
