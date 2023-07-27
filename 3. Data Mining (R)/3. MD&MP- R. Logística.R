# TAREA MINERÍA DE DATOS Y MODELIZACIÓN PREDICTIVA
# ALEJANDRO LEMA FERNÁNDEZ
# Entrega 15 dic 2022

setwd("C:/Users/alex_/Desktop/MÁSTER BD/5.3 Tarea Minería de Datos")

source("FuncionesRosa.R")

library(questionr) # freq(), descibre()
library(psych) # skew() (AtipicosAMissing() la usa)
library(car) # recode() (ojo, también hay una en dplyr)
library(corrplot) # corrplot()
library(caret) # createDataPartition() # train() # confusionMatrix
library(ggplot2)
library(lmSupport) # modelEffectSizes()
library(unmarked)
library(VGAM) # DUDA: Warning: package ‘VGAM’ was built under R version 4.1.3
library(stats) # alias(), step()

library(pROC) # roc() # DUDA: Warning: package ‘VGAM’ was built under R version 4.1.3

## PARTE 3: REGRESIÓN LOGÍSTICA ------------------------------------------------

# 7. Transformaciones y relaciones con la varObj &
# 8. Relaciones entre variabs y varObj

# Parto de los datos depurados
datos<-readRDS("datosEconDep_discretizados")
varObjBin<-datos$varObjBin
input<-datos[,-(1:3)] # Observar que quitamos también la variable Distrito, por colinealidad con CCAA (en la depuración no la habíamos quitado)

summary(input)

# Importancia de las VARIABS ORIG:
# par(mar=c(9.5, 4.1, 4.1, 2.1))
graficoVcramer(input,varObjBin) # Falla si hay alguna variable cuantitativa con menos de 6 valores diferentes

# - Efecto de variabs CATEG sobre varObjBin:
mosaico_targetbinaria(input$CCAA,varObjBin,"CCAA") # sí influye: distritas CCAA tienen distinta proporción de 0's y 1's
mosaico_targetbinaria(input$Tamano,varObjBin,"Tamaño")
mosaico_targetbinaria(input$Actividad_Municipio,varObjBin,"Actividad Municipio")

mosaico_targetbinaria(input$Ganaderia,varObjBin,"Ganaderia")
mosaico_targetbinaria(input$Hosteleria,varObjBin,"Hosteleria")
mosaico_targetbinaria(input$Comercios,varObjBin,"Comercios")
mosaico_targetbinaria(input$Agricultura,varObjBin,"Agricultura")

barras_targetbinaria(input$CCAA,varObjBin,"CCAA") # sí influye: distritas CCAA tienen distinta proporción de 0's y 1's
barras_targetbinaria(input$Tamano,varObjBin,"Tamaño")
barras_targetbinaria(input$Actividad_Municipio,varObjBin,"Actividad Municipio")

# - Efecto de variabs CONT sobre varObjBin:
boxplot_targetbinaria(input$Salario_2,varObjBin,"Salario_2")
boxplot_targetbinaria(input$Salario_3,varObjBin,"Salario_3")
boxplot_targetbinaria(input$Salario_3,varObjBin,"Salario_3")
boxplot_targetbinaria(input$Habit_Vivienda,varObjBin,"Habit_Vivienda")
boxplot_targetbinaria(input$Empresario,varObjBin,"Empresario")
boxplot_targetbinaria(input$DeudaSaldada,varObjBin,"DeudaSaldada")
# etc
# Ojo que unas variables pueden ser relevantes para una varObjBin, y no serlo para la varObjBin (y viceversa!)

# También se puede ver con histogramas en lugar de con boxplot
hist_targetbinaria(input$Salario_3,varObjBin,"Salario_3")

# (relaciones...)


# ______________________________________________________________________________
# # modelo con todas las variabs orig --> un primer ejemplo antes de mét selecc variabs :)
# aux <- data.frame(input,varObjBin)
# set.seed(123456)
# trainIndex <- createDataPartition(aux$varObjBin, p=0.8, list=FALSE)
# data_train <- aux[trainIndex,]
# data_test <- aux[-trainIndex,]
# 
# 
# modeloInicial <- glm(varObjBin~.,data=data_train, family=binomial)
# summary(modeloInicial)
# pseudoR2(modeloInicial,data_train,"varObjBin")
# pseudoR2(modeloInicial,data_test,"varObjBin")
# modeloInicial$rank
# ______________________________________________________________________________

### Añadimos TRANSFORMACIONES:
# Busco las mejores transformaciones para las variables continuas con respesto a la varObjBin
input_transf <- cbind(input, Transf_Auto(Filter(is.numeric, input), varObjBin))
summary(input_transf) # curioso, salen todas transformaciones tipo "x" (*)

## (*) las variab orig y sus transf tipo "x" son linealmente dependientes, luego no podemos meter ambas en el modelo, pues su corr = 1
# - quitar: todas las variabs ORIG CONTINUAS
input_transf <- subset(input_transf, select=-c(Salario_2,Salario_3,Salario_4,Habit_Vivienda,Empresario,DeudaSaldada,DeudaFinanc,Cambio_Deuda,
                                               Edad_18_65,Mayor_65,Vivienda_Grande,Ganaderia,Locales,prop_missings))

datos <- data.frame(input_transf,varObjBin) # añadimos las tranfs a nuestro df de datos

# (relaciones...) (Notar que no podemos meter varObjBin porque es categ y la corr es para cosas continuas)

corrplot(cor(cbind(Filter(is.numeric, input_transf)), use="pairwise", method="pearson"), # diff con R.Lineal: hemos quitado varObjBin, porque no es continua, no tiene sentido correlación (ver V cramer!)
         method = "ellipse", type = "upper")

# corr altas entre cont:
cor(input_transf$Edad_18_65, input_transf$xMayor_65) # -0.75 es alta, pero no eliminamos por ahora
scatterplot(input_transf$Edad_18_65, input_transf$xMayor_65)
# etc.

par(mar=c(9.5, 4.1, 4.1, 2.1))
graficoVcramer(input_transf,varObjBin)


## AÑADIMOS INTERACCIONES:
# (incl transf, pues haremos mét selecc variabs directamente con todo - la profe lo hizo por separado para ir poco a poco)

### nota: formulaInteracciones solo crea interacciones cont:categ y categ:categ (las interacc cont:cont quedan fuera del temario)
# para que formulaInteracciones detecte los tipo "factor", es necesario introducirle datos que sean un dataframe, y no una table "tbl_df".
# por eso es importante haber definido los datos con datos <- data.frame(...)

posObj <- which(colnames(datos) == "varObjBin")
formIntT <- formulaInteracciones(datos, posicion=posObj)

# y ahora ya por fin:

# 9. REGRESIÓN LOGÍSTICA #######################################################
# 9.1 Métodos Selección Variables (CLÁSICA)

# Partición en train y test
set.seed(12345678)
trainIndex <- createDataPartition(datos$varObjBin, p=0.8, list=FALSE)
data_train <- datos[trainIndex,]
data_test <- datos[-trainIndex,]

null <- glm(varObjBin~1,data=data_train,family=binomial) # modelo mínimo (observar que el mod mín es el mismo, estemos o no considerando transf y/o interacc)
fullIntT <- glm(formIntT,data=data_train,family=binomial) # modelo máximo

# usar step() en las 6 combinaciones posibles
# 1. forward AIC
modForAIC_transInt <- step(null, scope=list(lower=null, upper=fullIntT), direction="forward")
summary(modForAIC_transInt)
pseudoR2(modForAIC_transInt,data_train,"varObjBin")
pseudoR2(modForAIC_transInt,data_test,"varObjBin") # (si cambia mucho, mala robustez del modelo)
modForAIC_transInt$rank

# 2. backwards AIC
modBackAIC_transInt <- step(fullIntT, scope=list(lower=null, upper=fullIntT), direction="backward")

# 3. stepwise AIC
modBothAIC_transInt <- step(null, scope=list(lower=null, upper=fullIntT), direction="both")

# 4. forward BIC
modForBIC_transInt <- step(null, scope=list(lower=null, upper=fullIntT), direction="forward",k=log(nrow(data_train)))

# 5. backwards BIC
modBackBIC_transInt <- step(fullIntT, scope=list(lower=null, upper=fullIntT), direction="backward",k=log(nrow(data_train)))

# 6. stepwise BIC
modBothBIC_transInt <- step(null, scope=list(lower=null, upper=fullIntT), direction="both",k=log(nrow(data_train)))

(formula(modForBIC_transInt) == formula(modBothBIC_transInt)) # sale el mismo que el 4.forward BIC :D

# ------------------------------------------------------------------------------
# Antes de pasar a VALIDACIÓN CRUZADA, veamos un ejemplo didáctico de cómo se analizaría la calidad de un modelo en Regresión Logística:

# ## Análisis de uno de los modelos: ROC, ptoCorte y confusionMatrix
# # ejemplo de ploteo de curva ROC:
# curvaRoc <- roc(data_test$varObjBin, predict(modBothBIC_transInt,data_test,type = "response"), direction="<")
# plot(curvaRoc)
# # con su punto de corte y matriz de confusión (este código es la función sensEspCorte de funcionesRosa.R)
# ptoCorte <- 0.5
# probs <-predict(modBothBIC_transInt,data_test,type="response")
# cm <- confusionMatrix(data=factor(ifelse(probs>ptoCorte,1,0)),
#                     reference=data_test[,"varObjBin"],positive="1")
# 
# cm$table # matriz de confusión (molaría sacarla gráficamente, pero no he tenido tiempo de hacerlo)
# # O bien, directamente la función de la profe (solo que no nos devuelve cm$table --> podría modificarla para que sí lo hiciera)
# sensEspCorte(modBothBIC_transInt,data_test,"varObjBin",ptoCorte=0.5,evento="1") # cambiando el ptoCorte, podemos probar otros

# ------------------------------------------------------------------------------
# VALIDACIÓN CRUZADA repetida (6 modelos selecc clásica)
# nota: modelos debe tener este aspecto: modelos <- c("FEV~Height","FEV~Height+Age", "etc")
# aplicando la función "formula" a los 6 modelos, extraemos sus expresionmes en ese formato ;)

auxVarObj<-datos$varObjBin
datos$varObjBin<-make.names(datos$varObjBin) #formateo varObjBin a X0 y X1 para que funcione el codigo
total<-c()
modelos<-sapply(list(modForAIC_transInt,
                     modBackAIC_transInt,
                     modBothAIC_transInt,
                     modForBIC_transInt,
                     modBackBIC_transInt,
                     modBothBIC_transInt),formula)
# modelos<-sapply(list(modForAIC_transInt,
#                      modBothAIC_transInt,
#                      modForBIC_transInt,
#                      modBothBIC_transInt),formula)
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(modelos[[i]]), data = datos,
             method = "glm", family="binomial",metric = "ROC", # se añado family y metric por R.logíst
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      summaryFunction=twoClassSummary, # se añado summaryFunction por R.logíst
                                      classProbs=TRUE,returnResamp="all") # se añade ClassProbs por R.logíst
  )
  total<-rbind(total,data.frame(roc=vcr$resample[,1],modelo=rep(paste("Modelo",i),
                                                                nrow(vcr$resample))))
}
# no olvidar: recupero la variable objetivo en su formato
datos$varObjBin<-auxVarObj

## DECIDIR EL MEJOR MODELO:
# Nos quedamos con el modelo que:
# - media de área bajo curva ROC más alta (mejor modelo)
# - menor variabilidad (sd) de ese área ROC (mayor robustez)
# - menor número de parámetros (mayor sencillez)

# Vemos valores de área bajo la curva ROC y su sd:
boxplot(roc~modelo,data=total,main="Área bajo la curva ROC")

total_aux <- total[which((total$modelo != 'Modelo 2') & (total$modelo != 'Modelo 5')), ]
boxplot(roc~modelo,data=total_aux,main="Área bajo la curva ROC")

aggregate(roc~modelo, data = total, mean) 
aggregate(roc~modelo, data = total, sd)

# vemos cuántos paráms tienen. Otra forma: length(coef(modForAIC_transInt)))
modForAIC_transInt$rank
modBackAIC_transInt$rank
modBothAIC_transInt$rank
modForBIC_transInt$rank
modBackBIC_transInt$rank
modBothBIC_transInt$rank

aux <- (formula(modForBIC_transInt) == formula(modBothBIC_transInt)) # Si TRUE, son el mismo modelo

# El mejor modelo es:
form_clasica <- formula(modBothBIC_transInt) # (luego compararemos este modelo con el mejor obtenido por selecc aleatoria)

mejor_clásica <- modBothBIC_transInt # esta redefinición es simplemnte para generalizar el código de ptoCorte


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
  set.seed(12345+i) # para que cada iteracción sea distinta, hay que cambiar la semilla para que el reparto de subsample sea diferente cada vez
  subsample<-data_train[sample(1:nrow(data_train),prop*nrow(data_train),replace = T), ]
  full<-glm(formIntT,data=subsample,family=binomial)
  null<-glm(varObjBin~1,data=subsample,family=binomial)
  modeloAux<-step(null,scope=list(lower=null,upper=full),direction="both",trace=0, k=log(nrow(subsample))) # trace = 0 para que no se muestren los modelos según los va calculando
  modelosGenerados<-c(modelosGenerados,paste(sort(unlist(strsplit(as.character(formula(modeloAux))[3]," [+] "))),collapse = "+"))
}
freq(modelosGenerados,sort="dec")

### save.image ;)
save.image("C:/Users/alex_/Desktop/MÁSTER BD/5.3 Tarea Minería de Datos/3. Rlogistica.RData")

## De las 100 repeticiones, las 3 que más se repiten son:
form_aleat_1 <- "varObjBin ~ CCAA+Hosteleria+Hosteleria:xDeudaSaldada+Hosteleria:xVivienda_Grande+xDeudaSaldada+xHabit_Vivienda+xSalario_3+xVivienda_Grande"
form_aleat_2 <- "varObjBin ~ CCAA+Comercios+Comercios:xDeudaSaldada+Comercios:xVivienda_Grande+xDeudaSaldada+xHabit_Vivienda+xSalario_3+xVivienda_Grande"
form_aleat_3 <- "varObjBin ~ CCAA+Hosteleria+Hosteleria:xDeudaSaldada+Hosteleria:xVivienda_Grande+xDeudaSaldada+xHabit_Vivienda+xLocales+xSalario_3+xVivienda_Grande"
form_aleat_4 <- "varObjBin ~ CCAA+Hosteleria+Hosteleria:xVivienda_Grande+xDeudaSaldada+xHabit_Vivienda+xSalario_3+xVivienda_Grande"
form_aleat_5 <- "varObjBin ~ CCAA+Hosteleria+Hosteleria:xVivienda_Grande+xDeudaSaldada+xHabit_Vivienda+xSalario_4+xVivienda_Grande"

## Comparación final (VRC), tomo el ganador de la selecc clásica y los 3 nuevos candidatos
auxVarObj<-datos$varObjBin
datos$varObjBin<-make.names(datos$varObjBin) # formateo la variable objetivo para que funcione el codigo
total2<-c()
modelos2<-c(form_aleat_1,
            form_aleat_2,
            form_aleat_3,
            form_aleat_4,
            form_aleat_5,form_clasica)

for (i in 1:length(modelos2)){
  set.seed(1712)
  vcr<-train(as.formula(modelos2[[i]]), data = datos,
             method = "glm", family="binomial",metric = "ROC", # se añado family y metric
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      summaryFunction=twoClassSummary, # se añado summaryFunction
                                      classProbs=TRUE,returnResamp="all") # se añade ClassProbs
  )
  total2<-rbind(total,data.frame(roc=vcr$resample[,1],modelo=rep(paste("Modelo",i),
                                                                nrow(vcr$resample))))
}
# no olvidar: recupero la variable objetivo en su formato
datos$varObjBin<-auxVarObj

# Análisis:
boxplot(roc~modelo,data=total2,main="Área bajo la curva ROC")

total_aux_2 <- total2[which((total2$modelo != 'Modelo 2') & (total2$modelo != 'Modelo 5')), ]
boxplot(roc~modelo,data=total_aux_2,main="Área bajo la curva ROC")

aggregate(roc~modelo, data = total2, mean) 
aggregate(roc~modelo, data = total2, sd)

# Comparar los modelos:
mod_aleat_1 <- glm(form_aleat_1, data=data_train, family=binomial)
summary(mod_aleat_1)

mod_aleat_2 <- glm(form_aleat_2, data=data_train, family=binomial)
summary(mod_aleat_2)

mod_aleat_3 <- glm(form_aleat_3, data=data_train, family=binomial)
summary(mod_aleat_3)

mod_aleat_4 <- glm(form_aleat_4, data=data_train, family=binomial)
summary(mod_aleat_4)

mod_aleat_5 <- glm(form_aleat_5, data=data_train, family=binomial)
summary(mod_aleat_5)

summary(modBothBIC_transInt) # el de la selecc clásica

mod_aleat_1$rank
mod_aleat_2$rank
mod_aleat_3$rank
mod_aleat_4$rank
mod_aleat_5$rank
modBothBIC_transInt$rank

                                  ### MODELO GANADOR:
                                  ### MODELO 4

(form_aleat_4) # "varObjBin ~ CCAA+Hosteleria+Hosteleria:xVivienda_Grande+xDeudaSaldada+xHabit_Vivienda+xSalario_3+xVivienda_Grande"

# ------------------------------------------------------------------------------
## REAGRUPAR A POSTERIORI CATEGORÍAS POCO SIGNIFICATIVAS:
# Con summary, vemos que algunas de las categorías* de CCAA no son significativas. Esto nos da una pista de que podemos reagruparlas
# con otras CCAAs para reducir el número de parámetros del modelo: siguiendo el criterio de proximidad geográfica, reagrupamos:
# Baleares* con ComValenciana, Galicia* con Asturias, y Navarra* con PaísVasco. 

# PERO NO CAMBIAMOS LA FÓRMULA DEL MODELO!
# Solo recalculamos los valores de los parámetros, con un data_train modificado y con glm:

data_train_recode <- data_train
data_train_recode$CCAA <- recode(data_train_recode$CCAA,
                                "c('Galicia', 'Asturias')='Galicia&Asturias';
                                 c('Baleares', 'ComValenciana')='Valencia&Baleares';
                                 c('Navarra', 'PaísVasco')='Navarra&PaísVasco'")
modelo_ganador <- glm(form_aleat_4, # usamos la fórmula del modelo ganador
                      data = data_train_recode,
                      family=binomial) 

summary(modelo_ganador)
modelo_ganador$rank

# también cambiarlo en data_test, para calcular luego el ptoCorte
data_test_recode <- data_test
data_test_recode$CCAA <- recode(data_test_recode$CCAA,
                                 "c('Galicia', 'Asturias')='Galicia&Asturias';
                                 c('Baleares', 'ComValenciana')='Valencia&Baleares';
                                 c('Navarra', 'PaísVasco')='Navarra&PaísVasco'")

# ..............................................................................
# PUNTO CORTE

# (se optimiza el ptoCorte para el modelo óptimo elegido, no para todos!)

## generamos una rejilla de puntos de corte
posiblesCortes<-seq(0,1,0.01)
rejilla<-data.frame(t(rbind(posiblesCortes,sapply(posiblesCortes,
  function(x) sensEspCorte(modelo_ganador,data_test_recode,"varObjBin",x,"1")))))
# t() devuelve la traspuesta

# Analizar y seleccionar el ptoCorte óptimo:
rejilla$Youden<-rejilla$Sensitivity+rejilla$Specificity-1 # ind Youden para cada ptoCorte
plot(rejilla$posiblesCortes,rejilla$Youden)
plot(rejilla$posiblesCortes,rejilla$Accuracy)

rejilla$posiblesCortes[which.max(rejilla$Youden)]
rejilla$posiblesCortes[which.max(rejilla$Accuracy)]

#El resultado es 0.28 para youden y 0.42 para Accuracy. Los comparamos:
sensEspCorte(modelo_ganador,data_test_recode,"varObjBin",0.28,"1")
sensEspCorte(modelo_ganador,data_test_recode,"varObjBin",0.42,"1")



### INTERPRETACIÓN DE LOS PARÁMETROS, calidad del modelo, etc ------------------

# Vemos las variables más importantes del modelo ganador
impVariablesLog(modelo_ganador,"varObjBin")

# Vemos los coeficientes del modelo ganador
coef(modelo_ganador)
summary(modelo_ganador)

# Evaluamos la estabilidad del modelo a partir de las diferencias en train y test:
pseudoR2(modelo_ganador,data_train,"varObjBin")
pseudoR2(modelo_ganador,data_test,"varObjBin")
# es poca la diferencia, por lo que el modelo se puede considerar robusto

# también es poca la diferencia en el área bajo la curva roc y para el punto de corte:
roc(data_train$varObjBin, predict(modelo_ganador,data_train_recode,type = "response")) # data_train
roc(data_test$varObjBin, predict(modelo_ganador,data_test_recode,type = "response")) # data_test

sensEspCorte(modelo_ganador,data_train_recode,"varObjBin",ptoCorte=0.65,evento="1") # data_train
sensEspCorte(modelo_ganador,data_test_recode,"varObjBin",ptoCorte=0.65,evento="1") # data_test


# El punto de corte debe seleccionarse a partir del conjunto de datos test, lo que ofrecerá resultados más realistas.

save.image("C:/Users/alex_/Desktop/MÁSTER BD/5.3 Tarea Minería de Datos/3. Rlogistica.RData")  ### save.image ;)
