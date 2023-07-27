# TAREA MINERÍA DE DATOS Y MODELIZACIÓN PREDICTIVA
# ALEJANDRO LEMA FERNÁNDEZ
# Entrega 15 dic 2022

setwd("C:/Users/alex_/Desktop/MÁSTER BD/5.3 Tarea Minería de Datos")

source("FuncionesRosa.R")
library(questionr) # freq(), descibre()
library(psych) # skew() (AtipicosAMissing() la usa)
library(car) # recode() (ojo, también hay una en dplyr)
library(corrplot) # corrplot()
library(caret) # createDataPartition() # train()
library(ggplot2)
library(lmSupport) # modelEffectSizes()
library(unmarked)
library(VGAM)
library(stats) # alias(), step()

## PARTE 2: REGRESIÓN LINEAL ---------------------------------------------------

# 7. Transformaciones y relaciones con la varObj &
# 8. Relaciones entre variabs y varObj

# Parto de los datos depurados
datos<-readRDS("datosEconDep_discretizados") ### para hacerlo con los datos DISCRETIZADOS, ejecutar desde línea 296
varObjCont<-datos$varObjCont
input<-datos[,-(1:2)]

# Importancia de las VARIABS ORIG:
par(mar=c(9.5, 4.1, 4.1, 2.1))
graficoVcramer(input,varObjCont) # (falla si hay alguna variab cont tiene menos de 6 valores diferentes)
# no parece que podamos descargar definitivamente ninguna variable

# #  Corr de variabs cont ORIGINALES entre sí y con la varObjCont:
# plot_1 <- graficoCorrelacion(varObjCont,input) ### tarda varios minutos!!!
# ggsave(plot_1, "graficoCorrelacionCont.png", "C:/Users/alex_/Desktop/MÁSTER BD/5.3 Tarea Minería de Datos/Gráficos")
# 
# plot_2 <- corrplot(cor(cbind(varObjCont,Filter(is.numeric, input)), use="pairwise", method="pearson"),
#                    method = "ellipse", type = "upper")
# ggsave(plot_2, "corrplotCont.png", "C:/Users/alex_/Desktop/MÁSTER BD/5.3 Tarea Minería de Datos/Gráficos")

# corr alta entre:
# - Salarios con Salario_4 (normal, si los otros % son altos, el % restante debe ser bajo, pues el total está limitado a 100%)
# - Edades (ídem)
# - DeudaSaldada y DeudaFinanciada:
cor(input$DeudaFinanc, input$DeudaSaldada) # corr = -0.74 (alta)
# y demás corr... ver valores en graficoCorrelacion
# también podemos verlo visualmente:
scatterplot(input$DeudaFinanc, input$DeudaSaldada)
scatterplot(input$Edad_18_65, input$Mayor_65)

# (relaciones...)

# COLINEALIDAD: ________________________________________________________________
# Antes de pasar a transf, interacc, y mét selecc variabs, hicimos un primer escritinio de modelo lineal con todas las variabs originales, y 
# encontramos que las variabs categ Distrito y CCAA con colineales, ie aportan la misma información, y el modelo no calcula los paráms de una de ellas:
todo <- data.frame(input,varObjCont) # sin variables transformadas (seguimos el enunciado)

# cor(Filter(is.numeric, todo))

#Obtengo la partición
set.seed(123456)
trainIndex <- createDataPartition(todo$varObjCont, p=0.8, list=FALSE)
data_train <- todo[trainIndex,]
data_test <- todo[-trainIndex,]

# Construyo un modelo preliminar con todas* las variables (*recuerda que ya hemos quitado Salario_1 y Menor_18)
modelo1 <- lm(varObjCont~.,data=data_train)
summary(modelo1)

# el modelo no calcula paráms para las categorías de CCAA --> Resulta que CCAA y Distritos nos dan la misma info. Tenemos que decidir cuál quitar.

alias(modelo1) # encuentra dependencia lineal entre variables de un modelo lineal
# nos devuelve una matriz donde en el eje y están las categorías de la variable
# que ha devuelto el problema de colinealidad (CCAA), y la matriz vale 0 o 1 en función
# de si hay o no dep lineal. Vemos que cada distrito tiene dep lineal con la CCAA
# a la que pertenece.
# Luego CCAA y Distrito son "linealmente dependientes". Pero qué significa que 
# dos variables categóricas sean linealmente dependientes? Pues que sus efectos
# a la hora de predecir la varObj son dependientes. Y por tanto, debemos eliminar
# una de las dos del modelo.
# Para decidir cuál:
# 1) miramos su importancia (gráfico v cramer que vimos)
# 2) como tienen importancias muy parecidas, podríamos eliminar cualquiera.
# Si bien Distrito tiene un valor de V Cramer ligeramente superior (es más imp),
# es mejor eliminar la que tiene más categorías, pues así simplificamos el modelo.
# Eliminamos Distrito y mantenemos CCAA:
# ______________________________________________________________________________
input <- subset(input, select=-Distrito) # importante!
datos <- subset(datos, select=-Distrito)


## Añadimos TRANSFORMACIONES:
# Busco las mejores transformaciones para las variables continuas con respesto a la VarObjCont
input_transf <- cbind(input,Transf_Auto(Filter(is.numeric, input),varObjCont))

summary(input_transf)

corrplot(cor(cbind(varObjCont,Filter(is.numeric, input_transf)), use="pairwise", method="pearson"),
         method = "ellipse", type = "upper")

## las variab orig y sus transf tipo "x" son linealmente dependientes, luego no podemos meter ambas en el modelo, pues su corr = 1
# - quitar: Salario_2, Salario_3, Habit_Vivienda:
input_transf <- subset(input_transf, select=-c(Salario_2, Salario_3, Habit_Vivienda))

datos <- data.frame(input_transf,varObjCont) # añadimos las tranfs a nuestro df de datos

# (relaciones...)
par(mar=c(9.5, 4.1, 4.1, 2.1))
graficoVcramer(input_transf,varObjCont)

# También podemos ver cor y scatterplot entre variabs entre sí, y también entre cada variab y transf, y la varObj, para ver si hay relación o no...
# DUDA: ver relaciones entre variabs, qué quiere que miremos exactamente??? @@@@@

# una alta corr entre variables puede indicar que existe una interacción significativa entre ellas (útil si hacemos selecc MANUAL de variabs). Pero lo haremos con méts selecc ;)

cor(input_transf$Salario_4, input_transf$sqrtxSalario_4) # 0.99 --> no eliminamos por ahora
scatterplot(input_transf$Salario_4, input_transf$sqrtxSalario_4)

cor(input_transf$Cambio_Deuda, input_transf$raiz4Cambio_Deuda) # 0.975 --> ídem


## AÑADIMOS INTERACCIONES:
# (incl transf, pues haremos mét selecc variabs directamente con todo - la profe lo hizo por separado para ir poco a poco)

### nota: formulaInteracciones solo crea interacciones cont:categ y categ:categ (las interacc cont:cont quedan fuera del temario)
# para que formulaInteracciones detecte los tipo "factor", es necesario introducirle datos que sean un dataframe, y no una table "tbl_df".
# por eso es importante haber definido los datos con datos <- data.frame(...)

posObj <- which(colnames(datos) == "varObjCont")
formIntT <- formulaInteracciones(datos, posicion=posObj)

# y ahora ya por fin:

# 9. REGRESIÓN LINEAL ##########################################################
# 9.1 Métodos Selección Variables (CLÁSICA)

# Partición en train y test
set.seed(12345678)
trainIndex <- createDataPartition(datos$varObjCont, p=0.8, list=FALSE)
data_train <- datos[trainIndex,]
data_test <- datos[-trainIndex,]

null <- lm(varObjCont~1,data=data_train) # modelo mínimo (observar que el mod mín es el mismo, estemos o no considerando transf y/o interacc)
fullIntT <- lm(formIntT, data=data_train) # modelo máximo

# usar step() en las 6 combinaciones posibles
# 1. forward AIC
modForAIC_transInt <- step(null, scope=list(lower=null, upper=fullIntT), direction="forward")
summary(modForAIC_transInt)
Rsq(modForAIC_transInt,"varObjCont",data_test) # 
modForAIC_transInt$rank # 

# 2. backwards AIC
modBackAIC_transInt <- step(fullIntT, scope=list(lower=null, upper=fullIntT), direction="backward")
summary(modBackAIC_transInt)
Rsq(modBackAIC_transInt,"varObjCont",data_test) #
modBackAIC_transInt$rank #

# 3. stepwise AIC
modBothAIC_transInt <- step(null, scope=list(lower=null, upper=fullIntT), direction="both")
summary(modBothAIC_transInt)
Rsq(modBothAIC_transInt,"varObjCont",data_test) #
modBothAIC_transInt$rank # 

# 4. forward BIC
modForBIC_transInt <- step(null, scope=list(lower=null, upper=fullIntT), direction="forward",k=log(nrow(data_train)))
summary(modForBIC_transInt)
Rsq(modForBIC_transInt,"varObjCont",data_test) # 
modForBIC_transInt$rank # 

# 5. backwards BIC
modBackBIC_transInt <- step(fullIntT, scope=list(lower=null, upper=fullIntT), direction="backward",k=log(nrow(data_train)))
summary(modBackBIC_transInt)
Rsq(modBackBIC_transInt,"varObjCont",data_test) #
modBackBIC_transInt$rank #

# 6. stepwise BIC
modBothBIC_transInt <- step(null, scope=list(lower=null, upper=fullIntT), direction="both",k=log(nrow(data_train)))
summary(modBothBIC_transInt)
Rsq(modBothBIC_transInt,"varObjCont",data_test) # 
modBothBIC_transInt$rank # 

# ------------------------------------------------------------------------------
# VALIDACIÓN CRUZADA repetida (6 modelos selecc clásica)
# nota: modelos debe tener este aspecto: modelos <- c("FEV~Height","FEV~Height+Age", "etc")
# aplicando la función "formula" a los 6 modelos, extraemos sus expresionmes en ese formato ;)

total<-c()
modelos<-sapply(list(modForAIC_transInt,
                     modBackAIC_transInt,
                     modBothAIC_transInt,
                     modForBIC_transInt,
                     modBackBIC_transInt,
                     modBothBIC_transInt),formula)
# modelos<-sapply(list(modForAIC_transInt,modBothAIC_transInt,modForBIC_transInt,modBothBIC_transInt),formula)

for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(modelos[[i]]), data = datos,
             method = "lm",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      returnResamp="all")
  )
  total<-rbind(total,cbind(vcr$resample[,1:2],modelo=rep(paste("Modelo",i),
                                                         nrow(vcr$resample))))
}
boxplot(Rsquared~modelo,data=total,main="R-Square")

total_aux <- total[which(total$modelo != 'Modelo 2'), ]
boxplot(Rsquared~modelo,data=total_aux,main="R-Square")

## DECIDIR EL MEJOR MODELO:
# Nos quedamos con el modelo que:
# - media de R2 más alta (mejor modelo)
# - menor variabilidad (sd) de los R2 (mayor robustez)
# - menor número de parámetros (mayor sencillez)

# vemos valores de la media de R2 y su sd:
aggregate(Rsquared~modelo, data = total, mean)
aggregate(Rsquared~modelo, data = total, sd)

# vemos cuántos paráms tienen. Otra forma: length(coef(modForAIC_transInt)))
modForAIC_transInt$rank
modBackAIC_transInt$rank
modBothAIC_transInt$rank
modForBIC_transInt$rank
modBackBIC_transInt$rank
modBothBIC_transInt$rank

aux <- (formula(modForBIC_transInt) == formula(modBothBIC_transInt)) # TRUE, son el mismo modelo

# El mejor modelo de selecc clásica es: (luego compararemos este modelo con los 3 mejores obtenidos por selecc aleatoria)
( form_clasica <- paste("varObjCont ~", formula(modBothBIC_transInt)[3]) )

save.image("C:/Users/alex_/Desktop/MÁSTER BD/5.3 Tarea Minería de Datos/2. RLineal.RData")  ### save.image ;)

#  ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# 9.2 SELECCIÓN ALEATORIA
## Seleccion aleatoria (se coge una submuestra de los datos de entrenamiento)

# explicación código: df[sample(x = 1:nrow, size = prop*nrow, replace = TRUE), ] coge una muestra de tamaño=size, y la coge del elemento=x, que
# en nuestro caso es el vector 1,2,3,...,nrow. Con esto, conseguimos una lista aleatoria con el 70% de índices --> df[sample, ] y listo ;)
# notar que vamos directamente a orig+tranfs+interacc, y buscamo directamente el criterio BIC (penaliza muchos paráms, y aquí tenemos muchas variabs)
# (además, con el criterio AIC, calcular 100 modelos tardaría bastante tiempo, así que para acortar la tarea, hacemos solo BIC stepwise)

rep<-100
prop<-0.7 # se realiza con el 70% de los datos de entrenamiento por velocidad. El resultado es el mismo.
modelosGenerados<-c()
for (i in 1:rep){
  set.seed(12345+i)
  subsample<-data_train[sample(1:nrow(data_train),prop*nrow(data_train),replace = T), ]
  full<-lm(formIntT,data=subsample)
  null<-lm(varObjCont~1,data=subsample)
  modeloAux<-step(null,scope=list(lower=null,upper=full),direction="both",trace=0, k=log(nrow(subsample))) # trace = 0 para que no se muestren los modelos según los va calculando
  modelosGenerados<-c(modelosGenerados,paste(sort(unlist(strsplit(as.character(formula(modeloAux))[3]," [+] "))),collapse = "+"))
}
freq(modelosGenerados,sort="dec")

### save.image ;)
save.image("C:/Users/alex_/Desktop/MÁSTER BD/5.3 Tarea Minería de Datos/2. RLineal.RData")

## De las 100 repeticiones, las 3 que más se repiten son:
form_aleat_1 <- "varObjCont ~ CCAA+DeudaSaldada+Hosteleria+Hosteleria:xHabit_Vivienda+logxVivienda_Grande+raiz4Locales+xHabit_Vivienda+xSalario_3"
form_aleat_2 <- "varObjCont ~ CCAA+DeudaSaldada+Hosteleria+Hosteleria:DeudaSaldada+Hosteleria:xSalario_3+logxVivienda_Grande+raiz4Locales+xHabit_Vivienda+xSalario_3"
form_aleat_3 <- "varObjCont ~ CCAA+DeudaSaldada+expxDeudaSaldada+Hosteleria+Hosteleria:xSalario_3+logxVivienda_Grande+raiz4Locales+xHabit_Vivienda+xSalario_3"

## Comparación final, tomo el ganador de la selecc clásica y los nuevos candidatos de la selecc aleatoria:
total2<-c()
modelos2<-c(form_aleat_1,
            form_aleat_2,
            form_aleat_3,form_clasica)

for (i in 1:length(modelos2)){
  set.seed(1712)
  vcr<-train(as.formula(modelos2[[i]]), data = datos,
             method = "lm",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      returnResamp="all")
  )
  total2<-rbind(total2,cbind(vcr$resample[,1:2],modelo=rep(paste("Modelo",i),
                                                           nrow(vcr$resample))))
}

boxplot(Rsquared~modelo,data=total2,main="R-Square")

total_aux_2 <- total2[which((total2$modelo != 'Modelo 2') & (total2$modelo != 'Modelo 5')), ]
boxplot(Rsquared~modelo,data=total_aux_2,main="R-Square")

aggregate(Rsquared~modelo, data = total2, mean) 
aggregate(Rsquared~modelo, data = total2, sd) 

# Comparar los modelos:
summary(modBothBIC_transInt) # el de la selecc clásica

mod_aleat_1 <- lm(form_aleat_1, data=data_train)
summary(mod_aleat_1)

mod_aleat_2 <- lm(form_aleat_2, data=data_train)
summary(mod_aleat_2)

mod_aleat_3 <- lm(form_aleat_3, data=data_train)
summary(mod_aleat_3)

mod_aleat_1$rank
mod_aleat_2$rank
mod_aleat_3$rank
modBothBIC_transInt$rank

### MODELO GANADOR:
### MODELO 1

(form_aleat_1) # "varObjCont ~ CCAA+DeudaSaldada+Hosteleria+Hosteleria:xHabit_Vivienda+logxVivienda_Grande+raiz4Locales+xHabit_Vivienda+xSalario_3"

# ------------------------------------------------------------------------------
## REAGRUPAR A POSTERIORI CATEGORÍAS POCO SIGNIFICATIVAS:
summary(mod_aleat_1)
# no tiene variabs poco significativas, por lo que no hay que reagrupar nada :)
# (en la R.logística sí que tuvimos que reagrupar, ver ahí un ejemplo!)


# INTERPRETACIÓN DE LOS PARÁMETROS, calidad del modelo, etc.

### save.image ;)
save.image("C:/Users/alex_/Desktop/MÁSTER BD/5.3 Tarea Minería de Datos/2. RLineal.RData")
