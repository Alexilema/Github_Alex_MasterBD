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
# library(ggplot2)
library(lmSupport) # modelEffectSizes()
# library(unmarked)
# library(VGAM) # DUDA: Warning: package ‘VGAM’ was built under R version 4.1.3
library(stats) # alias(), step()

## PARTE 1. DEPURACIÓN DE LOS DATOS --------------------------------------------

library("readxl")
datos <- read_excel("DatosImpuestos_Tarea.xlsx")

# eliminar cols que no usaremos:
colnames(datos)
# Basura col num 5
# Vehiculo 6
# Vivienda 7
# Basura_Cuali 8
# Vehiculo_Cuali 9

datos <- subset(datos, select=-c(Basura,Vehiculo,Vivienda,Basura_Cuali,Vehiculo_Cuali))
# datos <- datos[ ,-c(5,6,7,8,9) # otra forma

summary(datos)

# Comprobamos el tipo de variable asignado a cada una:
str(datos)

## Categóricas:
# Distrito col 1 (aunque tiene 52 categorías... analizar más adelante)
# CCAA col 2
# ActEconom_Cuali col 4
# Actividad_Municipio col 22
# Tamano col 23

# ------------------------------------------------------------------------------
## POSIBLES ERRORES EN LOS DATOS:
# - NAs --> Cambio_Deuda, Vivienda_Grande, Ganaderia, Hosteleria, Comercios y Agricultura
# - Vivienda_Grande debe ser int --> está OK
# - CCAA debe ser una de las 19 reales --> está OK
# - Empresario es un %, no puede tener valores negativos
# - DeudaSaldada es un %, no puede ser mayor que 100
# - En las edades, hay algún max = 100, pero me parece raro que haya 100% de menores de edad en algún distrito
# - Salario... comprobar que los 4 salarios sumen 100%
# - Tamano tiene valores "?"
# - Locales tiene valores 99999 --> eliminar (max locales sin eso es 4759)
# - En Ganaderia, Hosteleria, Comercios y Agricultura, hay muchos ceros (¿falta de info?), y los máximos son muy altos --> atípicos
# - No parece haber errores de escritura en las variables categóricas :)
# - las variables que suman 100% son linealmente dependientes --> eliminar una de ellas

# head(sort(datos$Vivienda_Grande,decreasing=TRUE),n=200)
# Vivienda_Grande int, pero por ahora no categ (luego quizás por intervalos; y también atípicos)

# head(sort(datos$Ganaderia,decreasing=TRUE),n=20)
# head(sort(datos$Hosteleria,decreasing=TRUE),n=20)
# head(sort(datos$Comercios,decreasing=TRUE),n=20)
# head(sort(datos$Agricultura,decreasing=TRUE),n=20)
# head(sort(datos$Locales,decreasing=TRUE),n=200)

# Cambio las categóricas a tipo factor:
# (apply con la función factor, que es como as.factor)
datos[,c('Distrito','CCAA','ActEconom_Cuali','Actividad_Municipio','Tamano')] <-
  lapply(datos[ ,c('Distrito','CCAA','ActEconom_Cuali','Actividad_Municipio','Tamano')], factor)

str(datos) # se ha corregido

# Más herramientas de análisis (las usaremos de nuevo más adelante, en la parte de discretizar variabs)
# Cuento el número de valores diferentes para las variables numéricas:
sapply(Filter(is.numeric, datos), function(x) length(unique(x)))

# Para otros estadísticos:
describe(Filter(is.numeric, datos)) # min, max, nº de NAs, nº de unique values

# -----------------------------------------------------------------------------
# REAGRUPAR CATEGORÍAS

# Ver el reparto de las categorías de las variables categóricas:
freq(datos$Distrito) # los distritos 51 y 51 solo aparecen 1 vez --> combinarlos en 1 único distrito (incluso si lo conociéramos, combinar con distrito correspondiente a Cádiz o así)
freq(datos$CCAA) # Ceuta y Melilla solo aparecen 1 vez --> las juntamos a Andalucía (por criterio de proximidad)
freq(datos$Actividad_Municipio) # nota: agricultura y ganadería aparecen muy poco --> combinar en Agric&Ganad
freq(datos$Tamano) # todo bien (por ahora)

# a) categs poco representadas:
# Agrupamos Ceuta (51) y Melilla (52) con Andalucia (criterio de proximidad)
# Y sus dos distritos los agrupamos como 1 solo (tolo lo que sea reducir categs simplifica el modelo)
datos$CCAA <- recode(datos$CCAA, "c('Ceuta', 'Melilla','Andalucía')='Andalucía'")
datos$Distrito <- recode(datos$Distrito, "c('51', '52')='51'")

# Actividad_Municipo: Agricultura y Ganaderia están muy poco representadas, incluso uniéndolas entre sí. Así que las unimos con Otros:
datos$Actividad_Municipio <- recode(datos$Actividad_Municipio, "c('Agricultura', 'Ganaderia', 'Otros')='Otros_Activ'")

      ## ACTUALIZACIÓN (11 dic)
      # Reagrupamos también Comercios y Hostelería, porque por separado no son significativas:
      datos$Actividad_Municipio <- recode(datos$Actividad_Municipio, "c('Comercios','Hosteleria')='Hostel&Comerc'")


# b) categs poco significativas:
# Esto no se hace aquí. Se hace después de escoger el modelo ganador!!! Ver ejemplo en mi informe entregado en Regresión Logística ;)


# -----------------------------------------------------------------------------
# 4. Corrección de errores

# eliminar missings de Tamano:
datos$Tamano <- recode.na(datos$Tamano,"?") # Recoded 92 values

# eliminar valores erróneos (99999) en Locales:
# usamos replace(x, list of index, values)
datos$Locales <- replace(datos$Locales, which(datos$Locales==99999), NA)

# Valores fuera de rango (0 - 100 por ser porcentajes) --> pasarlos a NA:
datos$Empresario <- replace(datos$Empresario, which(datos$Empresario < 0), NA)
datos$DeudaSaldada <- replace(datos$DeudaSaldada, which(datos$DeudaSaldada > 100), NA)

# Verificar variables que deben sumar 100%:
# - En las edades, hay algún max = 100, pero me parece raro que haya 100% de menores de edad en algún distrito
#   --> comprobar que suma de las 3 edades no excede el 100% en cada distrito
# - Salario... comprobar que los 4 salarios sumen 100% en cada distrito

# Edades:
# usamos rowSums
datos$TotalAge <- rowSums(cbind(datos$Menor_18,datos$Edad_18_65,datos$Mayor_65),na.rm=TRUE)
freq(datos$TotalAge)
# vemos valores de 99.999%, 100% y 100.001%, los consideramos correctos
# pero también hay valores de 0% --> pasamos las 3 col de edades correspondientes a NAs:
datos$Menor_18 <- replace(datos$Menor_18, which(datos$TotalAge == 0), NA)
datos$Edad_18_65 <- replace(datos$Edad_18_65, which(datos$TotalAge == 0), NA)
datos$Mayor_65 <- replace(datos$Mayor_65, which(datos$TotalAge == 0), NA)

# Comprobamos (ahora hay que poner na.rm=FALSE para que reescriba la col correctamente)
datos$TotalAge <- rowSums(cbind(datos$Menor_18,datos$Edad_18_65,datos$Mayor_65),na.rm=FALSE)
freq(datos$TotalAge) # comprobamos que ahora no hay ceros, sino NAs
# análisis --> 446 NAs x 3 cols, son muchos, igual no es buena idea imputarlos... aunque son el 5,4% de cada col. Visto así, no es tanto.

# me adelanto: al final los vamos a imputar con la media, porque además, vamos a eliminar una de las 3 variables de edad, ya que no se pueden
# meter variables linealmente dependientes en el modelo. Entonces, eliminamos por ej MAyor_65 y luego imputamos los NAs en las otras por la media 
# Nota: si imputáramos las 3 variabs con la media, en muchos casos no sumarían 100%, sería mucha casualidad. Pero esto ya no va a ser un problema
# porque estamos ignorando Mayor_65 e imputando solamente las otras dos ;)

# Salarios:
datos$TotalSalary <- rowSums(cbind(datos$Salario_1,datos$Salario_2,datos$Salario_3,datos$Salario_4),na.rm=TRUE)
summary(datos$TotalSalary) # problema, no son 100%

length(which(datos$TotalSalary < 99.9 | datos$TotalSalary > 100.1)) # casi todos tienen este problema...
# no hay menores de 99.99. Los mayores de 100 pero aprox 100 los damos por buenos. Pero vemos que el max es 155%... los valores que no sumen aprox
# 100 los quitaremos. Por ej, podemos tolerar hasta 110%:
length(which(datos$TotalSalary > 110)) # 19 valores es más razonable. Los pasamos a NAs:

datos$Salario_1 <- replace(datos$Salario_1, which(datos$TotalSalary > 110), NA)
datos$Salario_2 <- replace(datos$Salario_2, which(datos$TotalSalary > 110), NA)
datos$Salario_3 <- replace(datos$Salario_3, which(datos$TotalSalary > 110), NA)
datos$Salario_4 <- replace(datos$Salario_4, which(datos$TotalSalary > 110), NA)

# comprobación de que hemos eliminado los correctos:
datos$TotalSalary <- rowSums(cbind(datos$Salario_1,datos$Salario_2,datos$Salario_3,datos$Salario_4),na.rm=TRUE)
length(which(datos$TotalSalary < 110)) # ahora ya sale con 8100 casos. Los 19 restantes son los NAs ;) 
summary(datos$Salario_4) # 19 NAs

# descartamos las cols auxiliares que creamos para analizar los datos:
datos <- subset(datos, select=-c(TotalSalary,TotalAge))

# ------------------------------------------------------------------------------
# ELIMINAR VARIABLES linealmente DEPENDIENTES
# las variables que suman 100% son linealmente dependientes. Por tanto, hay que eliminar una de ellas, tanto en Edades como en Salarios.

# Da igual cual sea, pero como estamos hablando de predecir impagos de impuestos, quitamos los Menor_18 que no pagan esos impuestos así que ya
# sabemos que no es significativo. En Salarios no está tan claro, podríamos argumentar varias cosas. Vamos a optar por eliminar Salario_1, porque sus
# valores oscilan entre 0% y 9,96%, siendo el que menos variabilidad muestra, y por tanto, de cara a futuras predicciones, cabría esperar que futuras
# observaciones también tendrían valores más similares entre ellas en Salario_1 que en los otros, lo que hace que Salario_1 sea la variable Salario
# menos relevante (¿no?). No estoy del todo seguro de esto, pero como al final da igual cuál de los 4 quitar, pues no le voy a dar más vueltas :)

# eliminar Menores_18 y Salario_1:
datos <- subset(datos, select=-c(Salario_1,Menor_18))

# ------------------------------------------------------------------------------
# 5. Detección, análisis y tratamiento de MISSINGS (incluye ATÍPICOS)

#Indico la variableObj, el ID y las Input (los atípicos y los missings se gestionan sólo en las variables input)
varObjCont <- datos$ActEconom
varObjBin <- datos$ActEconom_Cuali
input <- as.data.frame(datos[,-(3:4)]) # para el tratam de missings, no selecciono las variables objetivo
# row.names(input) <- datos$ID # Se considera la variable identificadora
# (en nuestro caso, no había variable identificadora)

# Veamos si hay relación entre los missings de las variables (antes de hacer atipicosAmissing!!):
corrplot(cor(is.na(input[colnames(input)[ colSums(is.na(input)) > 0 ]])), method = "ellipse", type = "upper")

## explicación DENTRO A FUERA:
# is.na(input) # nos cambia CADA valor del df por TRUE or FALSE, dependiendo de si era NA o no
# colSums(is.na(input)) # es como un sum() que se aplica a cada columna, es decir, hace la suma, para cada columna, de algo.
#                       # al meterle una condición lógica como argumento, colSums cuenta el número de NA's por columna
# colnames(input)[ colSums(is.na(input)) > 0 ] # nos quedamos con las cols que sí tienen NA's
# input(...) de todo eso --> seleccionamos solo esas columnas, para hacer el gráfico de correlación solo entre ellas
# y les aplicamos is.na() otra vez
# cor(x,y) mide la correlación o dependencia lineal entre dos vectores
## cor(x) mide la corr entre las columnas de x, supongo... DUDA
# xxx <- cor(is.na(input[colnames(input)[ colSums(is.na(input)) > 0 ]]))
# show(xxx)
## valores cercanos a cero es que no hay correlación. Cercamos a 1 o -1 indica mucha corr. Cada variable consigo misma evidentemente da corr=1


### ATÍPICOS:
# antes de seguir, tenemos que identificar los atípicos, para pasarlos a NAs:
# Cuento el porcentaje de atípicos de cada variable
sapply( Filter(is.numeric, input), function(x) atipicosAmissing(x)[[2]] ) / nrow(input)

## Explicación del código:
# Filter(f, x) nos devuelve un vector con solamente los elementos de x que devuelven TRUE cuando se les aplica la función lógica f
# nrow(x) = número de filas del vector o dt. Dividimos entre esto para que sea el % (entre 0 y 1)
# atipicosAmissing(x) nos devuelve una lista con 2 cosas: el dt x con los atípicos cambiados a NAs, y el número de atípicos por variable
# por eso seleccionamos solo la 2ª lista: [[2]]
# diviendo entre el número de filas, tendremos la proporción de atípicos (entre 0 y 1)
#
# Salen porcentajes de atípicos muy bajos, pero es normal porque 1) si no fueran
# bajos, no serían atípicos y 2) hay muchos datos, entonces no es extraño que,
# en proporción, los atípicos sean pocos.

# NOTA: las variabs de DeudaSaldada, DeudaFinanc y Cambio_Deuda tienen algunos valores que serían identificados como atípicos. No obstante, como son todo valores en el
# rango correcto de 0 y 100% (o sin límites para Cambio_Deuda), pensamos que es mejor no tratar como atípicos esos valores:
# Modifico los atípicos como missings:
input[ ,c("Salario_2","Salario_3","Salario_4","Habit_Vivienda","Empresario","Edad_18_65","Mayor_65","Vivienda_Grande","Ganaderia","Hosteleria","Comercios",
          "Agricultura","Locales")] <- sapply(Filter(is.numeric, subset(input,select=-c(DeudaSaldada,DeudaFinanc,Cambio_Deuda))),function(x) atipicosAmissing(x)[[1]])

## para tratar todas las variabs cont con atípicos:
# input[ ,as.vector(which(sapply(input, class)=="numeric"))] <- sapply(Filter(is.numeric, input),function(x) atipicosAmissing(x)[[1]])

## Explicación del código:
# todas las filas, y las col que son numéricas (OJO: en variables categ no eliminamos los NAs, pues pueden ser una categoría más)
# con sapply(input,class) le preguntamos qué clase tiene cada columna
# con which == numeric, elegimos las variables numéricas
# as.vector porque estamos haciento input[,c(index)], es decir, estamos eligiendo las columnas de input, y R nos pide decirle un vector con
# los índices de las columnas a seleccionar, por eso pasamos los índices obtenidos a as.vector ;)
## esas columnas que hemos seleccionado, las rellenamos con lo mismo que arriba, pero ahora con el [[1]] porque ahora sí reemplazamos los atípicos or NA's

summary(datos)
# Vemos que mete NAs en casi todas las variables
# en ganaderia, hosteleria, comercios, agricultura y locales, mete MUCHOS NAs... --> discretizar y crear categ "Desconocido"


# Busco si existe algún patrón en los missings, que me pueda ayudar a entenderlos
# (esta parte nos da un poco igual, pues el conj datos no es nuestro, no tenemos el poder de corregir la forma en que se ha obtenido)
corrplot(cor(is.na(input[colnames(input)[ colSums(is.na(input)) > 0 ]])), method = "ellipse", type = "upper")
# vemos de nuevo el corr de missings, tras el tratamiento de atípicos, y sacamos conclusiones --> (((...) ver el informe de la tarea))


#Creamos nueva col con la proporción de missings por variable y observación
input$prop_missings <- apply(is.na(input),1,mean) # dim=1 indica que se aplica por filas, por observación.
freq(input$prop_missings)
# la mayoría de observ no tienen missings :D
# si hubiera alguna fila con una proporc de missings muy alta, podríamos eliminarla o algo...
# En freq() vemos el max = 0.45, luego no eliminamos ninguna fila (ninguna es mayor que 0.5)

(prop_missingsVars <- apply(is.na(input),2,mean)) # dim=2 indica que se aplica por col, por variables.
freq(prop_missingsVars)
# Si alguna muy alta, indicaría que no estamos obteniendo esa info de forma consistente
# pero no es el caso: max = 0.126

# #elimino las observaciones y las variables con más de la mitad de datos missings (No hay ninguna, no ejecuto este código)
# input <- subset(input, prop_missings< 0.5, select=names(prop_missingsVars)[prop_missingsVars<0.5])
# varObjBin <- varObjBin[input$prop_missings<0.5] #Actualizar las observaciones de las variables objetivo
# varObjCont <- varObjCont[input$prop_missings<0.5]

# Recategorizo categóricas con "suficientes" observaciones missings
prop_missingsVars
# hay 4 variables con más de 10% de missings: Agricultura, Ganadería, Comercios y Vivienda_Grande
# Los missings se podrían considerar una categoría más. Pero en este caso (script SIN DISCRETIZAR), consideramos que es mejor imputarlos,
# para no tener que categorizar variabs cont (pues aumentaría demasiado el nº paráms del modelo).

# ------------------------------------------------------------------------------
# 6. Imputaciones

  # A. Imputar variabs categ:
# - razonar para cada variable si tiene más sentido usar moda o aleatorio
# - Solo tiene NAs la variab categ Tamano
# a) Por un lado, cabe argumentar que es mejor usar la moda, pues la mayoría son Pequeño, así que es más probable que acertemos
# b) Por otro lado, cabe pensar que podría ser todo lo contrario y que todos los NAs fueran en realidad distritos (tan) Pequeños que apenas recaban
#    bien su información, y por esto estaban como NAs... Haremos aleatorio para confurdirnos "sin sesgo" en caso de confundirnos
input$Tamano <- ImputacionCuali(input$Tamano,"aleatorio")

# nota: si fuera el caso, podríamos imputar todas a la vez con el mismo método:
# input[,as.vector(which(sapply(input, class)=="factor"))] <- sapply(Filter(is.factor, input), function(x) ImputacionCuali(x,"aleatorio"))

# A veces se cambia el tipo de factor a character al imputar, así que hay que indicarle que es factor
str(input) # están como tipo character
input[,as.vector(which(sapply(input, class)=="character"))] <- lapply(input[,as.vector(which(sapply(input, class)=="character"))], factor)
str(input) # ahora como factor ;)


  # B. Imputar variabs cont:
# Imputo todas las cuantitativas, seleccionar el tipo de imputación: media, mediana o aleatorio
# - ojo, si la variab cont muestra mucha asimetría, es más correcto imputar con la mediana que con la media
# - y si la desv tip es alta (en comparación con la media), es mejor usar aleatorio (*si std_dev es alta por pocos valores muy grandes, mejor usar la media/mediana)
# - para distribuciones más cercanas a una normal, usamos la media

describe(Filter(is.numeric, input)) # min, max, nº de NAs, nº de unique values

# también lo podemos ver gráficamente: 
hist(input$Salario_3) # media
hist(input$Salario_2) # aleatorio
hist(input$Locales) # mediana

# - usar media para: Salario_3, Habit_Vivienda, Empresario*, DeudaSaldada, Cambio_Deuda*, Edad_18_65, Mayor_65
# - usar aleatorio para: Salario_2, Salario_4, DeudaFinanc, Locales
# - usar mediana para: Vivienda_Grande y Locales (**)
  # NOTA: imputamos todas las variabs, aunque no hubiéramos buscado atípicos en ellas, porque podrían venir con NAs del dataset original

input[ ,c('Salario_3','Habit_Vivienda','Empresario','DeudaSaldada','Cambio_Deuda','Edad_18_65','Mayor_65')] <- 
  sapply(input[ ,c('Salario_3','Habit_Vivienda','Empresario','DeudaSaldada','Cambio_Deuda','Edad_18_65','Mayor_65')], function(x) ImputacionCuant(x,"media"))

input[ ,c('Salario_2','Salario_4','DeudaFinanc','Locales')] <-
  sapply(input[ ,c('Salario_2','Salario_4','DeudaFinanc','Locales')], function(x) ImputacionCuant(x,"aleatorio"))

input[ ,c('Vivienda_Grande','Locales')] <- sapply(input[ ,c('Vivienda_Grande','Locales')], function(x) ImputacionCuant(x,"mediana"))

# nota: si fuera el caso, podríamos imputar todas a la vez con el mismo método:
# input[,as.vector(which(sapply(input, class)=="numeric"))] <- sapply(Filter(is.numeric, input), function(x) ImputacionCuant(x,"mediana"))
# ImputacionCuant solo modifica los valores que son NA (ver funcionesRosa)

summary(input) # comprobar que ya no hay missings (excepto en Ganaderia, Hosteleria, Comercios y Agricultura, ver abajo)
               # OJO que a veces imputar por mét aleatorio puede dejar NAs --> se soluciona imputando de nuevo

### IMPORTANTE:
# (**) Observar que no hemos imputado (por ahora) las variabs: Ganaderia, Hosteleria, Comercios y Agricultura. Esto es porque vamos a probar dos cosas:
    # (opción 1) imputarlas (por la mediana) y hacer la REGRESIÓN con estas variabs CONTINUAS
    # (opción 2) no imputarlas, sino discretizarlas, y que los valores NAs procedentes de los atípicos formarán parte de la categoría 'Desconocido'


### LOS MEJORES RESULTADOS SE CONSIGUEN CON LA OPCIÓN 2, pues al discretizar, la variable Hosteleria pasa a ser significativa


                                    # --> COMENTAR UNA DE LAS 2 OPCIONES
                                    #         EXPORTAR LOS DATOS
                                    # Y PASAR AL SCRIPT DE REGRESIÓN <--


# # OPCIÓN 1 =====================================================================
# input[ ,c('Ganaderia','Hosteleria','Comercios','Agricultura')] <-
#   sapply(input[ ,c('Ganaderia','Hosteleria','Comercios','Agricultura')], function(x) ImputacionCuant(x,"mediana"))
# 
# summary(input) # comprobar que ya no hay missings
# 
# # GUARDAMOS los datos:
# ### Guardamos la imagen de R para seguir trabajando otro día ;)
# save.image("C:/Users/alex_/Desktop/MÁSTER BD/5.3 Tarea Minería de Datos/1. Depuración_sin discretizar.RData")
# 
# # Una vez finalizado este proceso, se puede considerar que los datos están depurados. Los guardamos
# saveRDS(cbind(varObjBin,varObjCont,input),"datosEconDep_sinDiscretizar")


# OPCIÓN 2: discretizar ========================================================
# pensar en qué categorías vamos a dicretizar las variabs:

# a) Agricultura, Ganaderia y Comercios
summary(input) # tienen mean baja y median = 0 --> ¿imputar por la mediana?
freq(input$Agricultura) # ACTUALIZACIÓN: podríamos dejarla como cont, pero no es significativa --> probar a discretizar
# b) Vivienda_Grande
freq(input$Vivienda_Grande) # no recomendable categorizar (la mediana no es cero, es una variable cont no anómala) --> imputar

# Recategorizar variabs cont con mediana = 0:
# - Hosteleria
# - Comercios
# - Agricultura
# *** Con Ganaderia, hemos probado a dejarla continua, porque sin discretizar ninguna variab, salía significativa
# Pero hemos visto que al discretizar las otras, ya no es signif. Así que al final da un poco igual si la discretiz o no...

aux <- input$Hosteleria[which(input$Hosteleria!=0)]
summary(aux) # 2º cuartil(median)=68, 3º=122, mean=88, max=260 --> podemos hacer valorNormal y valorAlto separando por 122 (por probar...)
aux <- input$Comercios[which(input$Comercios!=0)]
summary(aux) # ídem
aux <- input$Agricultura[which(input$Agricultura!=0)]
summary(aux) # ídem
aux <- input$Ganaderia[which(input$Ganaderia!=0)]
summary(aux) # en Ganaderia, no tiene sentido diferenciar entre valorNormal y Alto --> hacemos Cero, Desc y valor

# # Recode:
# input$Hosteleria <- recode(input$Hosteleria,"NA='Desconocido';0='Cero'; 1:122='valorNormal'; 123:260='valorAlto'",as.factor = T)
# input$Comercios <- recode(input$Comercios,"NA='Desconocido';0='Cero'; 4:73='valorNormal'; 74:160='valorAlto'",as.factor = T)
# input$Agricultura <- recode(input$Agricultura,"NA='Desconocido';0='Cero'; 4:48='valorNormal'; 49:100='valorAlto'",as.factor = T)
# input$Ganaderia <- recode(input$Ganaderia,"NA='Desconocido';0='Cero'; 4:56='valor'",as.factor = T)

# Finalmente, hemos visto que no aporta significancia separar los valores no nulos en dos categorías, así que los agrupamos en una sola:
# probar con 3 categs: Cero, Desconocido y Valor:
input$Hosteleria <- recode(input$Hosteleria,"NA='Desconocido';0='Cero'; 1:260='Valor'",as.factor = T)
input$Comercios <- recode(input$Comercios,"NA='Desconocido';0='Cero'; 4:160='Valor'",as.factor = T)
input$Agricultura <- recode(input$Agricultura,"NA='Desconocido';0='Cero'; 4:100='Valor'",as.factor = T)
input$Ganaderia <- recode(input$Ganaderia,"NA='Desconocido';0='Cero'; 4:56='valor'",as.factor = T)

# GUARDAMOS los datos:
### Guardamos la imagen de R para seguir trabajando otro día ;)
save.image("C:/Users/alex_/Desktop/MÁSTER BD/5.3 Tarea Minería de Datos/1. Depuración_Discretizados.RData")  ### save.image ;)

# Una vez finalizado este proceso, se puede considerar que los datos están depurados. Los guardamos
saveRDS(cbind(varObjBin,varObjCont,input),"datosEconDep_discretizados")

