########################################################################
#                      TRABAJO DE FIN DE MÁSTER                        #
#             Máster Universitario en Estadística Aplicada             #
########################################################################
#              ESTIMACIÓN EN ENCUESTAS NO PROBABILÍSTICAS              #  
#              CON MECANISMOS DE SELECCIÓN NO ALEATORIOS               #
########################################################################
#              Presentado por: José Juan García Rodríguez              #
########################################################################
#  Tutorizado por:                                                     #
#  - María del Mar Rueda García                                        #
#  - Ramón Ferri García                                                #
########################################################################
#  - Universidad de Granada.                                           #
#  - Facultad de Ciencias.                                             #
#  - Departamento de Estadística e Investigación operativa.            #
#  - Curso académico 2024-2025                                         #
########################################################################

# Antes de comenzar, limpiamos el entorno de trabajo y situamos el 
# directorio de trabajo en la carpeta actual, en la que se ubican 
# tanto el presente script como el archivo "Población.RData", que 
# incluye los datos sobre la población con la que se trabaja.
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))





#############################################################
# 1) SIMULACIÓN DE LA POBLACIÓN Y OBTENCIÓN DE LAS MUESTRAS #
#############################################################

# CARGA DE LOS DATOS DE LA POBLACIÓN

# Tras haber simulado la población siguiendo el procedimiento marcado 
# por Ferri-García y Rueda (2018), los resultados se encuentran 
# guardados en el archivo "Población.RData", que debe ubicarse en el 
# mismo directorio que este script. Comenzamos cargando dichos datos:
load("Población.RData")

# Los datos sobre las variables de interés para el estudio en la 
# población se encuentran en el data frame "sim_poblacion", salvo 
# la variable indicadora de los individuos con acceso a Internet, 
# que se encuentra en el vector "pob_int".

# Además, "pop_size" es el tamaño poblacional seleccionado para simular
# la población (en este caso, 10000), y se tienen dos vectores "vec" y 
# "x", que son auxiliares de la simulación de la población.

# La descripción de la base de datos se encuentra en la memoria del 
# trabajo.



# ANÁLISIS EXPLORATORIO DE LOS DATOS

# Con la función "head" podemos ver la cabecera del data frame que 
# incluye los datos de la población, y con la función "str" podemos 
# ver la estructura de las variables del mismo.
head(sim_poblacion)
str(sim_poblacion)

# Convertimos las variables categóricas en factores.
sim_poblacion$vote<-as.factor(sim_poblacion$vote)
sim_poblacion$education<-as.factor(sim_poblacion$education)
sim_poblacion$sex<-as.factor(sim_poblacion$sex)
sim_poblacion$language<-as.factor(sim_poblacion$language)

str(sim_poblacion)


# Realizamos un resumen descriptivo de los datos. 
summary(sim_poblacion)



# TAMAÑO POBLACIONAL Y TAMAÑOS MUESTRALES

# Definimos variables que incluyan el tamaño poblacional y los tamaños 
# de las muestras probabilística y no probabilística.
N<-length(sim_poblacion[,1]) #Tamaño poblacional
n_p<-100 #Tamaño de la muestra probabilística
n_np<-100 #Tamaño de la muestra no probabilísitca



# VARIABLE OBJETIVO Y VARIABLES AUXILIARES

# Definimos un vector poblacional con los valores de la variable 
# objetivo o variable de interés para nuestro estudio.

# En este caso, se trata de la variable indicadora de votantes del 
# Partido Pirata, que, como se comenta en la memoria, resultará en 
# un mecanismo de selección NMAR. 
y<-as.numeric(sim_poblacion[,1]=="pir") 

# También se utilizará la variable indicadora de votantes del Partido 
# Pensionista, que resultará en un mecanismo de selección MAR. Para 
# aplicarle las técnicas implementadas, basta sustituir "pir" por 
# "pens" en la línea de código previa y ejecutar todo el script de
# nuevo.

# Definimos además un data frame que incluye las covariables o 
# variables auxiliares que se utilizarán en el estudio. Dicho data 
# frame tendrá por columnas estas covariables y por filas a los 
# individuos de la población. Se trata de todas las variables del 
# data frame "sim_poblacion", salvo la primera, que se corresponde
# con la variable objetivo del estudio.
X<-sim_poblacion[,-1]



# OBTENCIÓN DE LAS MUESTRAS

# Para la obtención de las muestras, se utiliza un muestreo 
# estratificado en la variable "language", para garantizar que 
# la proporción de individuos no nativos en la muestra sea igual
# que la de la población. Esto se hace para evitar muestras que 
# sólo incluyan a individuos nativos, dada la reducida proporción
# de individuos no nativos.

# Se utiliza la siguiente función para realizar dicho muestreo 
# estratificado.
estrat<-function(n, indices_pob, variable){
  
  # "variable" es la variable estratificadora. Suponemos que se han
  # introducido sus valores en la población completa, y calculamos 
  # los tamaños de cada uno de los estratos (Nh).
  # Calculamos también el tamaño de la muestra que se va a extraer 
  # de cada estrato para que la proporción de individuos con cada 
  # valor de la variable estratificadora sea la misma en la muestra 
  # completa y en la población.
  
  estratos<-levels(variable) #Posibles valores de la variable 
                             #estratificadora
  
  N<-length(variable) #Tamaño poblacional
  Nh<-numeric() #Vector con los tamaños poblacionales de cada estrato
  nh<-numeric() #Vector con los tamaños muestrales de cada estrado
  
  for(i in 1:length(estratos)){
    Nh[i]<-sum(variable==estratos[i])
    nh[i]<-round(Nh[i]/N*n)
  }
  
  # Ahora, nos quedamos con los valores de la variable estratificadora 
  # en los individuos de la población de la que se va a extraer la 
  # muestra (serán la población completa en el muestreo probabilístico 
  # y la población con acceso a Internet en el muestreo no 
  # probabilístico).
  variable<-variable[indices_pob]
  
  indices<-numeric() #Vector de índices de los individuos de la muestra
  for(i in 1:length(estratos)){
    indices<-c(indices, sample(indices_pob[variable==estratos[i]], 
                               nh[i], replace=FALSE))
  }
  
  pesos<-rep(Nh/nh,nh) #Vector de pesos de los individuos de la muestra
  return(list(indices=indices, pesos=pesos))
  
}


# La siguiente función obtiene una muestra probabilística y una 
# muestra no probabilística de la población, siendo la primera una 
# muestra estratificada de toda la población; y la segunda, una 
# muestra con el mismo diseño muestral, pero a partir de la 
# subpoblación con acceso a Internet (esto es justamente lo que
# hace que el muestreo sea no probabilístico).

muestreo<-function(n_p, n_np){
  
  #MUESTRA PROBABILÍSTICA
  muestra_p<-estrat(n=n_p, indices_pob=1:N, 
                    variable=sim_poblacion$language)
  indices_p<-muestra_p$indices #Índices i \in s^P
  d_p<-muestra_p$pesos #Pesos del muestreo probabilístico
  
  #MUESTRA NO PROBABILÍSTICA
  muestra_np<-estrat(n=n_np, indices_pob=which(pob_int==1), 
                    variable=sim_poblacion$language)
  indices_np<-muestra_np$indices #Índices i \in s^{NP}
  d_np<-rep(1,n_np) #Pesos del muestreo no probabilístico
  
  return(list(indices_p=indices_p, d_p=d_p, 
              indices_np=indices_np, d_np=d_np))
  
}


# Obtenemos las muestras mediante la función de muestreo 
# definida previamente.
realizacion<-muestreo(n_p=n_p, n_np=n_np)
indices_p<-realizacion$indices_p
indices_np<-realizacion$indices_np
d_p<-realizacion$d_p
d_np<-realizacion$d_np

# Guardamos en nuevos vectores la información de las variables 
# en las muestras.
ys_p<-y[indices_p]
ys_np<-y[indices_np]

Xs_p<-X[indices_p,]
Xs_np<-X[indices_np,]



# PARÁMETRO DE INTERÉS - VALOR REAL Y ESTIMADOR

# El parámetro de interés es la proporción de votantes del
# partido elegido.
# Su valor real es:
v.real<-sum(y)/N
v.real

# Además, se define la siguiente función, que devuelve el 
# estimador de dicha proporción dada la información "y" de 
# la variable de interés en una muestra y el vector de pesos
# correspondiente a los individuos de la misma.
# Concretamente, se está implementando el estimador de Hájek, 
# que se utilizará para probar todas las técnicas estudiadas.
estimador<-function(y,pesos){
  est<-sum(pesos*y)/sum(pesos)
  return(est)
}



# FUNCIÓN QUE TRANSFORMA TODAS LAS VARIABLES EN NUMÉRICAS
# Para aplicar correctamente algunas de las técnicas, necesitaremos
# que todas las variables sean numéricas. Por ello, definimos la 
# siguiente función, que identifica las variables no numéricas del
# data frame y aprovecha la función disjunctive del paquete sampling
# para obtener las variables indicadoras correspondientes y, 
# posteriormente, eliminar las variables no numéricas.

# El argumento "indep" permite decidir si se utilizan todas las 
# variables indicadoras (indep=FALSE), lo cual será necesario, por 
# ejemplo, para la calibración; o si sólo se usan un subconjunto 
# linealmente independiente de dichas variables (indep=TRUE), lo 
# cual será necesario, por ejemplo, para el PSA de Chen, Li y Wu (2019).

#install.packages("sampling")
library(sampling)

variables_numericas<-function(X, indep=TRUE){
  
  for(i in 1:ncol(X)){
    if(!is.numeric(X[,i])){
      num<-disjunctive(X[,i])
      
      if(indep==TRUE){
        aux<-data.frame(num[,-ncol(num)])
        colnames(aux)<-paste(colnames(X)[i], 1:ncol(aux), sep=".")
        X<-cbind(X, aux)
        
      } else{
        aux<-data.frame(num)
        colnames(aux)<-paste(colnames(X)[i], 1:ncol(aux), sep=".")
        X<-cbind(X, aux)
    
      }
    }
  }
  
  X<-X[,sapply(X, is.numeric)]
  return(X)
  
}





####################################################
# 2) TÉCNICAS PARA MITIGAR EL SESGO EN EL CASO MAR #
####################################################

# En este apartado se implementan todas las técnicas de reducción del 
# sesgo descritas en el Capítulo 2 de la memoria del trabajo.

######################################
# 2.1) TÉCNICAS BASADAS EN EL DISEÑO #
######################################

######################
# 2.1.1) CALIBRACIÓN #
######################

# a) CALIBRACIÓN USUAL

# La función calib del paquete sampling devuelve un vector con los 
# g-pesos del estimador de calibración. Los multiplicamos por los 
# pesos del muestreo para obtener los pesos del estimador de 
# calibración.

# Esta función requiere que todas las variables sean numéricas, 
# por lo que transformamos cada variable categórica en un conjunto 
# de variables indicadoras, una por cada modalidad de la variable.

calibracion<-function(Xs, ys, d, total){
  
  Xs<-variables_numericas(Xs, indep=FALSE)
  omega<-calib(Xs=Xs, d=d, total=totales, method="linear")*d
  
  est.cal<-estimador(y=ys, pesos=omega)

  return(list(est.cal=est.cal,d.cal=omega))
}

totales<-colSums(variables_numericas(X, indep=FALSE))
calibracion(Xs=Xs_np, ys=ys_np, d=d_np, total=totales)$est.cal


# b) CALIBRACIÓN CUANDO NO SE CONOCEN LOS TOTALES POBLACIONALES

# En caso de no conocer los totales poblacionales de las covariables,
# los estimábamos mediante el estimador de Horvitz-Thompson a partir 
# de la muestra probabilística. Tras esto, aplicamos la calibración 
# en la muestra no probabilística.

calibracion2<-function(Xs_np, ys_np, Xs_p, d_p, d_np){
  
  Xs_p<-variables_numericas(Xs_p, indep=FALSE)
  totales.est<-colSums(d_p*Xs_p)
  
  cal.2<-calibracion(Xs=Xs_np, ys=ys_np, d=d_np, total=totales.est)
  return(cal.2)
}

calibracion2(Xs_p=Xs_p, d_p=d_p, Xs_np=Xs_np, 
             ys_np=ys_np, d_np=d_np)$est.cal





##############
# 2.1.2) PSA #
##############

# a) PSA DE LEE Y VALLIANT (2009)
PSA.Lee<-function(ys_np, Xs_np, d_np, Xs_p, d_p, n_clases=5){
  
  n_p<-nrow(Xs_p)
  n_np<-nrow(Xs_np)
  
  # Obtenemos la muestra combinada.
  muestra_comb<-rbind(Xs_np, Xs_p)
  
  # Definimos la variable indicadora de pertenencia a la muestra no 
  # probabilística en la muestra combinada.
  delta<-rep(c(1,0),c(n_np,n_p))
  
  # Ajustamos un modelo de regresión logística para estimar las 
  # propensiones de pertenencia a la muestra no probabilística.
  formula<-as.formula(paste("delta~",paste(colnames(Xs_p),
                                           collapse="+"),sep=""))
  reg_logit<-glm(formula, data=muestra_comb, family="binomial")
  
  pi_np<-predict(object=reg_logit, newdata=muestra_comb, 
                 type="response")
  
  # Dividimos la muestra combinada en "n_clases" clases atendiendo
  # a la propensión de cada individuo. Con el siguiente código 
  # obtenemos un vector que indica la clase a la que pertenece 
  # cada individuo de la muestra combinada.
  cortes<-cut(1:length(pi_np), n_clases, labels=FALSE)
  
  clases<-vector()
  for(i in 1:n_clases){
    clases[order(pi_np)[cortes==i]]<-i
  }
  
  # Obtenemos vectores que indican las clases a las que pertenecen los
  # individuos de cada una de las muestras.
  clases_np<-clases[1:n_np]
  clases_p<-clases[n_np+1:n_p]
  
  # Calculamos los factores de ajuste asociados a cada clase.
  fc<-sapply(1:n_clases, function(i){
    (sum(d_p[clases_p==i])/sum(d_p))/(sum(d_np[clases_np==i])/sum(d_np))
  })
  
  # Finalmente, calculamos los pesos del PSA de Lee y Valliant (2009).
  d.PSA<-fc[clases_np]*d_np
  
  return(d.PSA)
  
}



# b) PSA DE VALLIANT Y DEVER (2011)
PSA.Valliant<-function(ys_np, Xs_np, d_np, Xs_p, n_clases=5){
  
  n_p<-nrow(Xs_p)
  n_np<-nrow(Xs_np)
  
  # Obtenemos la muestra combinada.
  muestra_comb<-rbind(Xs_np, Xs_p)
  
  # Definimos la variable indicadora de pertenencia a la muestra no 
  # probabilística en la muestra combinada.
  delta<-rep(c(1,0),c(n_np,n_p))
  
  # Ajustamos un modelo de regresión logística para estimar las
  # propensiones de pertenencia a la muestra no probabilística.
  formula<-as.formula(paste("delta~",paste(colnames(Xs_p),
                                           collapse="+"),sep=""))
  reg_logit<-glm(formula, data=muestra_comb, family="binomial")
  
  pi_np<-predict(object=reg_logit, newdata=muestra_comb, 
                 type="response")
  
  # Dividimos la muestra combinada en n_clases clases atendiendo
  # a la propensión de cada individuo. Con el siguiente código 
  # obtenemos un vector que indica la clase a la que pertenece 
  # cada individuo de la muestra combinada.
  cortes<-cut(1:length(pi_np), n_clases, labels=FALSE)
  
  clases<-vector()
  for(i in 1:n_clases){
    clases[order(pi_np)[cortes==i]]<-i
  }
  
  # Obtenemos vectores que indican las clases a las que pertenecen los
  # individuos de cada una de las muestras.
  clases_np<-clases[1:n_np]
  clases_p<-clases[n_np+1:n_p]
  
  # Calculamos la media de las estimaciones de las propensiones de los 
  # individuos de cada clase.
  pi_c<-as.vector(by(pi_np, clases, mean))
  
  # Finalmente, calculamos los pesos del PSA de Valliant y Dever (2011).
  d.PSA<-d_np/pi_c[clases_np]
  
  return(d.PSA)
  
}  



# c) PSA DE SCHONLAU Y COUPER (2017)
PSA.Scholau<-function(ys_np, Xs_np, Xs_p){
  
  n_p<-nrow(Xs_p)
  n_np<-nrow(Xs_np)
  
  # Obtenemos la muestra combinada.
  muestra_comb<-rbind(Xs_np, Xs_p)
  
  # Definimos la variable indicadora de pertenencia a la muestra no 
  # probabilística en la muestra combinada.
  delta<-rep(c(1,0),c(n_np,n_p))
  
  # Ajustamos un modelo de regresión logística para estimar las
  # propensiones de pertenencia a la muestra no probabilística.
  formula<-as.formula(paste("delta~",paste(colnames(Xs_p),
                                           collapse="+"),sep=""))
  reg_logit<-glm(formula, data=muestra_comb, family="binomial")
  
  pi_np<-predict(object=reg_logit, newdata=muestra_comb, 
                 type="response")
  
  # Finalmente, calculamos los pesos del PSA de Scholau y Couper (2011).
  d.PSA<-(1-pi_np[1:n_np])/pi_np[1:n_np]
  
  return(d.PSA)
  
}  



# d) PSA DE CHEN, LI Y WU (2019)

# MÉTODO ORIGINAL DE CHEN, LI Y WU (2019)

# Necesitaremos una norma en R^n para establecer el criterio de parada.
# Utilizaremos la norma usual.
norma<-function(x){
  sum(x*x)^0.5
}

# Para calcular la inversa de la matriz que aparece en este método,
# utilizaremos la función pinv del paquete pracma, que implementa 
# la matriz pseudoinversa de Moore-Penrose (generalización de la 
# matriz inversa).
#install.packages("pracma")
library(pracma)

# También definimos previamente la función que da el método iterativo
# de Newton-Raphson que aparece en el PSA de Chen, Li y Wu (2019):
f.Chen<-function(theta, X1_np, X1_p, d_p, tol=10^(-3)){
  
  n_p<-length(X1_p[,1])
  n_variables<-length(X1_p[1,])
  
  matriz<-matrix(0, nrow=n_variables, ncol=n_variables)
  exponente<-numeric()
  for(k in 1:n_p){
    xk<-as.matrix(X1_p[k,])
    exponente[k]<-as.numeric(xk%*%theta)

    matriz<-matriz+d_p[k]*exp(exponente[k])/((1+exp(exponente[k]))^2)*t(xk)%*%xk
  }
  
  vect<-colSums(X1_np)-colSums(d_p*exp(exponente)/(1+exp(exponente))*X1_p)
  
  theta1<-as.vector(theta + pinv(matriz)%*%vect)

  if(norma(theta-theta1)<tol){
    return(theta1)
  } else{
    f.Chen(theta=theta1, X1_np=X1_np, X1_p=X1_p, d_p=d_p)
  }
}


PSA.Chen1<-function(ys_np, Xs_np, Xs_p, d_p){
  
  # En primer lugar, transformamos las variables categóricas en
  # variables indicadoras (numéricas). Estas deben ser linealmente
  # independientes para que exista la matriz inversa del método 
  # iterativo de Newton-Raphson, por lo que se consideran todas 
  # las variables indicadoras de de la matriz disyuntiva asociada
  # a cada variable categórica menos la última, que se puede obtener
  # como 1 menos la suma de todas las demás.
  Xs_p<-variables_numericas(Xs_p, indep=TRUE)
  Xs_np<-variables_numericas(Xs_np, indep=TRUE)
  
  # Añadimos una columna de unos, que se corresponderá con el 
  # término independiente del modelo de regresión.
  X1_np<-cbind(1,Xs_np)
  X1_p<-cbind(1,Xs_p)
  
  n_np<-nrow(X1_np)
  n_variables<-ncol(X1_p)
  
  # Estimamos los valores de los parámetros mediante el método de 
  # Newton-Raphson.
  semilla<-rep(0,n_variables)
  theta.est<-f.Chen(theta=semilla, X1_np=X1_np, X1_p=X1_p, d_p=d_p)

  # Estimamos las propensiones sustituyendo las estimaciones de los
  # parámetros en el modelo logístico.
  exponente<-numeric()
  pi_np<-numeric()
  for(j in 1:n_np){
    exponente<-as.numeric(as.matrix(X1_np[j,])%*%theta.est)
    pi_np[j]<-exp(exponente)/(1+exp(exponente))
  }
  
  # Finalmente, calculamos los pesos del PSA de Chen, Li y Wu (2019).
  d.PSA<-1/pi_np
  
  return(d.PSA)
  
}



# APROXIMACIÓN DE FERRI-GARCÍA ET AL. (2024).
PSA.Chen2<-function(ys_np, Xs_np, d_np, Xs_p, d_p){
  
  n_p<-nrow(Xs_p)
  n_np<-nrow(Xs_np)
  
  # Obtenemos la muestra combinada.
  muestra_comb<-rbind(Xs_np, Xs_p)
  
  # Definimos la variable indicadora de pertenencia a la muestra no 
  # probabilística en la muestra combinada.
  delta<-rep(c(1,0),c(n_np,n_p))
  
  # Ajustamos un modelo de regresión logística ponderada por los
  # pesos de los individuos de ambas muestras para estimar las 
  # propensiones de pertenencia a la muestra no probabilística.
  formula<-as.formula(paste("delta~",paste(colnames(Xs_p),
                                           collapse="+"),sep=""))
  reg_logit<-glm(formula, data=muestra_comb, family="binomial", 
                 weights=c(d_np, d_p))
  
  pi_np<-predict(object=reg_logit, newdata=Xs_np, type="response")
  
  # Finalmente, calculamos los pesos de la aproximación del PSA
  # de Ferri-García et al. (2024).
  d.PSA<-1/pi_np
  
  return(d.PSA)
  
}



# FUNCIÓN DEL PSA GLOBAL (INCLUYENDO TODAS LAS VARIANTES)
PSA<-function(ys_np, Xs_np, d_np, Xs_p, d_p, n_clases=5, method="lee"){
  
  if(method=="lee"){
    
    d.PSA<-PSA.Lee(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np,
                       Xs_p=Xs_p, d_p=d_p, n_clases=n_clases)
    
  } else if(method=="valliant"){
    
    d.PSA<-PSA.Valliant(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np,
                        Xs_p=Xs_p, n_clases=n_clases)
    
  } else if(method=="schonlau"){
    
    d.PSA<-PSA.Scholau(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p)
    
  } else if(method=="chen1"){
    
    d.PSA<-PSA.Chen1(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, d_p=d_p)
    
  } else if(method=="chen2"){
    
    d.PSA<-PSA.Chen2(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
                     Xs_p=Xs_p, d_p=d_p)
    
  } else{
    
    stop("El método introducido no es válido.")
  }

  est.PSA<-estimador(y=ys_np, pesos=d.PSA)
  return(list(est.PSA=est.PSA, d.PSA=d.PSA))
  
}

PSA(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, d_p=d_p,
    n_clases=5, method="lee")$est.PSA

PSA(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p,
    n_clases=5, method="valliant")$est.PSA

PSA(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, method="schonlau")$est.PSA

PSA(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, 
    d_p=d_p, method="chen1")$est.PSA

PSA(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, d_p=d_p, 
    method="chen2")$est.PSA



# PSA+CALIBRACIÓN 
PSA.cal<-function(ys_np, Xs_np, d_np, Xs_p, d_p,
                  n_clases=5, method="lee", total){
  
  # Obtenemos los pesos del PSA obtenidos mediante el método 
  # correspondiente.
  d.PSA<-PSA(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
             Xs_p=Xs_p, d_p=d_p, n_clases=5, method=method)$d.PSA
  
  # Hacemos la calibración utilizando los pesos del PSA como 
  # pesos de partida.
  if(missing(total)){
    est<-calibracion2(Xs_p=Xs_p, d_p=d_p, Xs_np=Xs_np, ys_np=ys_np, 
                      d_np=d.PSA)$est.cal
  } else{
    est<-calibracion(Xs=Xs_np, ys=ys_np, d=d.PSA, total=total)$est.cal
  }
  
  return(est)
  
}

totales<-colSums(variables_numericas(X, indep=FALSE))

PSA.cal(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, d_p=d_p, 
        n_clases=5, method="lee", total=totales)
PSA.cal(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, d_p=d_p, 
        n_clases=5, method="valliant", total=totales)
PSA.cal(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, d_p=d_p, 
        n_clases=5, method="schonlau", total=totales)
PSA.cal(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, d_p=d_p, 
        n_clases=5, method="chen1", total=totales)
PSA.cal(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, d_p=d_p, 
        n_clases=5, method="chen2", total=totales)





###############################
# 2.1.3) CUASI-ALEATORIZACIÓN #
###############################

QA<-function(ys_np, Xs_np, d_np, Xs_p, d_p, N){
  
  n_p<-nrow(Xs_p)
  n_np<-nrow(Xs_np)
  
  # Obtenemos la muestra combinada.
  muestra_comb<-rbind(Xs_np, Xs_p)
  
  # Estimamos el cociente 1/P(delta_i^P=1/x_i) mediante una 
  # regresión de Poisson.
  formula1<-as.formula(paste("d_p~",paste(colnames(Xs_p),
                                          collapse="+"),sep=""))
  reg1<-glm(formula1, data=Xs_p, family="poisson")
  
  aux1<-predict(object=reg1, newdata=Xs_np, type="response")
  
  
  # Estimamos P(Z_i=1/x_i).

  # Definimos la variable indicadora de pertenencia a la muestra no 
  # probabilística en la muestra combinada.
  delta<-rep(c(1,0),c(n_np,n_p))
  
  # Ajustamos un modelo de regresión logística para estimar las
  # propensiones de pertenencia a la muestra no probabilística.
  formula2<-as.formula(paste("delta~",paste(colnames(Xs_p),
                                            collapse="+"),sep=""))
  reg2<-glm(formula2, data=muestra_comb, family="binomial")
  
  aux2<-predict(object=reg2, newdata=Xs_np, type="response")
  
  
  # Con esto, tenemos valores proporcionales a los pesos de la 
  # Cuasi-Aleatorización.
  pesos.prop<-as.vector(aux1*(1-aux2)/aux2)
  
  # Imponiendo que los pesos sumen el total poblacional, obtenemos
  # los pesos de la Cuasi-Aleatorización.
  lambda<-N/sum(pesos.prop)
  d.QA<-lambda*pesos.prop
  
  # Finalmente, calculamos el estimador del parámetro de interés
  # con estos pesos.
  est.QA<-estimador(y=ys_np, pesos=d.QA)
  
  return(list(est.QA=est.QA, d.QA=d.QA))
  
}

QA(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, d_p=d_p, N=N)$est.QA





###########################
# 2.1.4) KERNEL WEIGHTING #
###########################

# Para implementar el Kernel-Weighting, se utiliza la función 
# "kw.wt" del paquete "KWML", por lo que comenzamos instalando 
# y cargando el paquete.

#install.packages("KWML")
library(KWML)

KW<-function(ys_np, Xs_np, Xs_p, d_p){
  
  n_p<-nrow(Xs_p)
  n_np<-nrow(Xs_np)
  
  # Obtenemos la muestra combinada.
  muestra_comb<-rbind(Xs_np, Xs_p)
  
  # Definimos la variable indicadora de pertenencia a la muestra no 
  # probabilística en la muestra combinada.
  delta<-rep(c(1,0),c(n_np,n_p))
  
  # Ajustamos un modelo de regresión logística para estimar las 
  # propensiones de pertenencia a la muestra no probabilística.
  formula<-as.formula(paste("delta~",paste(colnames(Xs_p),
                                           collapse="+"),sep=""))
  reg_logit<-glm(formula, data=muestra_comb, family="binomial")
  
  pi_np<-predict(object=reg_logit, newdata=muestra_comb, 
                 type="response")
  
  # Utilizamos la función "kw.wt" del paquete "KWML" para obtener 
  # los pesos del Kernel-Weighting.
  d.KW<-kw.wt(p_score.c=pi_np[1:n_np], 
              p_score.s=pi_np[(n_np+1):(n_np+n_p)],
              svy.wt=d_p, krn="triang")$pswt
  
  # Finalmente, calculamos el estimador del parámetro de interés
  # con estos pesos.
  est.KW<-estimador(y=ys_np, pesos=d.KW)
  return(list(est.KW=est.KW, d.KW=d.KW))
}

KW(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, d_p=d_p)$est.KW





######################################
# 2.2) TÉCNICAS BASADAS EN EL MODELO #
######################################

###############################
# 2.2.1) STATISTICAL MATCHING #
###############################

# En primer lugar, definimos una función para obtener las 
# predicciones de los valores de la variable de interés en 
# la muestra probabilística a partir de la información de 
# esta variable en la muestra no probabilística y de la
# información de un conjunto de covariables en ambas muestras.

# Dicha función permite hacer las predicciones mediante una
# regresión lineal ("lineal"), mediante una regresión logística
# ("logit") (pueden ser ponderadas si se introduce un vector de
# pesos en el argumento "weights"), mediante el algoritmo del 
# vecino más próximo ("neighbour1") y mediante el algoritmo de 
# los k vecinos más próximos ("neighbour2").

# Para estos dos últimos se utiliza la función "matching" del paquete 
# "NonProbEst", que ya los implementa de forma optimizada. Comenzamos 
# instalando y cargando dicho paquete.

#install.packages("NonProbEst")
library(NonProbEst)

# La función "predicciones" no sólo nos servirá para el Statistical 
# Matching, sino que la utilizaremos también más adelante para otros
# métodos como los estimadores modelo-asistido, modelo-basado y 
# modelo-calibrado, los estimadores doblemente robustos y las 
# adaptaciones de las técnicas descritas para el caso NMAR.

predicciones<-function(ys_np, Xs_np, Xs_p, weights, method="lineal"){
  
  if(method=="lineal"){ # REGRESIÓN LINEAL
    
    # Ajustamos un modelo de regresión lineal para estimar los valores
    # de la variable objetivo en la muestra probabilística. Utilizamos 
    # la información de la muestra no probabilística como datos de
    # entrenamiento.
    formula<-as.formula(paste("ys_np~",paste(colnames(Xs_np),
                                             collapse="+"),sep=""))

    if(missing(weights)){ #Si se incluye un vector de pesos, se hace
                          #una regresión lineal ponderada.
      reg_lineal<-lm(formula, data=cbind(ys_np,Xs_np))
      
    } else{
      
      reg_lineal<-lm(formula, data=cbind(ys_np,Xs_np), weights=weights)
      
    }
    
    y_gorro<-predict(object=reg_lineal, newdata=Xs_p, type="response")
    return(y_gorro)
    
  } else if(method=="logit"){ # REGRESIÓN LOGÍSTICA
    
    formula<-as.formula(paste("ys_np~",paste(colnames(Xs_np),
                                             collapse="+"), sep=""))
    
    if(missing(weights)){ #Si se incluye un vector de pesos, se hace una 
                          #regresión lineal ponderada.
      reg_logit<-glm(formula, data=cbind(ys_np,Xs_np), 
                     family="binomial")
    } else{
      reg_logit<-glm(formula, data=cbind(ys_np,Xs_np), 
                     family="binomial", weights=weights)
    }
    
    y_gorro<-predict(object=reg_logit, newdata=Xs_p, type="response")
    return(y_gorro)
    
  } else if(method=="neighbour1"){ # VECINO MÁS PRÓXIMO
    
    y_gorro<-matching(convenience_sample=cbind(Xs_np,y=ys_np), 
                      reference_sample=Xs_p, estimated_var="y",
                      covariates=colnames(Xs_np), algorithm="kknn",k=1)
    return(y_gorro)
    
  } else if(method=="neighbour2"){ # K VECINOS MÁS PRÓXIMOS
    
    y_gorro<-matching(convenience_sample=cbind(Xs_np,y=ys_np), 
                      reference_sample=Xs_p, estimated_var="y",
                      covariates=colnames(Xs_np), algorithm="kknn")
    return(y_gorro)
    
  } else{
    stop("El método introducido no es válido.")
  }
  
}


# Ahora sí, definimos una función que implementa el Statistical
# Matching y lo probamos con los cuatro métodos que permite la 
# función "predicciones" (sin contar las regresiones ponderadas).

statistical<-function(ys_np, Xs_np, Xs_p, d_p, 
                      weights, method="lineal"){
  
  if(missing(weights)){
    y_gorro<-predicciones(ys_np=ys_np, Xs_np=Xs_np, 
                          Xs_p=Xs_p, method=method)
  } else{
    y_gorro<-predicciones(ys_np=ys_np, Xs_np=Xs_np, 
                          Xs_p=Xs_p, weights=weights, method=method)
  }
  
  est.statistical<-estimador(y=y_gorro, pesos=d_p)
  return(est.statistical)
}

statistical(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, 
            d_p=d_p, method="lineal")
statistical(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, 
            d_p=d_p, method="logit")
statistical(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, 
            d_p=d_p, method="neighbour1")
statistical(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, 
            d_p=d_p, method="neighbour2")





##################################
# 2.2.2) ESTIMADORES MA, MB Y MC #
##################################

# Definimos una función que calcula los estimadores modelo-asistido, 
# modelo-basado y modelo-calibrado. Se elige cuál calcular a través 
# del argumento "method".

MABC<-function(ys_np, Xs_np, d_np, indices_np, X, method="MB"){
  
  N<-length(X[,1])
  
  y_gorro<-predicciones(ys_np=ys_np, Xs_np=Xs_np, 
                        Xs_p=X, method="lineal")

  if(method=="MA"){ #Estimador Modelo-Asistido
    
    est.MA<-(sum(y_gorro) + sum(d_np*(ys_np-y_gorro[indices_np])))/N
    return(est.MA)
    
  } else if(method=="MB"){ #Estimador Modelo-Basado
    
    est.MB<-(sum(ys_np)+sum(y_gorro[-indices_np]))/N
    return(est.MB)
    
  } else if(method=="MC1"){ #Estimador Modelo-Calibrado
    
    omega<-calib(Xs=y_gorro[indices_np], d=d_np, total=sum(y_gorro), 
                 method="linear")*d_np
    est.MC1<-sum(omega*ys_np)/N
    return(est.MC1)
    
  } else if(method=="MC2"){ #Estimador Modelo-Calibrado (incluyendo 
                            #la ecuación de calibración que impone 
                            #que la suma de los pesos coincide con
                            #el tamaño poblacional).
    
    omega<-calib(Xs=cbind(1,y_gorro[indices_np]), d=d_np, 
                 total=c(N,sum(y_gorro)), method="linear")*d_np
    est.MC2<-sum(omega*ys_np)/N
    return(est.MC2)
    
  } else{
    
    stop("El método introducido no es válido.")
    
  }
  
}

MABC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, indices_np=indices_np,
     X=X, method="MA")
MABC(ys_np=ys_np, Xs_np=Xs_np, indices_np=indices_np, X=X, 
     method="MB")
MABC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, indices_np=indices_np, 
     X=X, method="MC1")
MABC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, indices_np=indices_np, 
     X=X, method="MC2")





########################################
# 2.3) ESTIMADORES DOBLEMENTE ROBUSTOS #
########################################

# La función "DR" calcula los estimadores DR1 y DR2, según si se 
# incluye el total poblacional N como argumento o no.

DR<-function(ys_np, Xs_np, Xs_p, d_p, N){
  
  y_gorro_np<-predicciones(ys_np=ys_np, Xs_np=Xs_np, 
                           Xs_p=Xs_np, method="lineal")
  y_gorro_p<-predicciones(ys_np=ys_np, Xs_np=Xs_np, 
                          Xs_p=Xs_p, method="lineal")
  
  d.PSA<-d.PSA<-PSA(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, 
                    d_p=d_p, method="chen1")$d.PSA
  
  if(missing(N)){
    est.DR<-sum(d_p*y_gorro_p)/sum(d_p) + sum(d.PSA*(ys_np-y_gorro_np))/sum(d.PSA)
  } else{
    est.DR<-sum(d_p*y_gorro_p)/N + sum(d.PSA*(ys_np-y_gorro_np))/N
  }
  
    return(est.DR)
  
}

DR(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, d_p=d_p, N=N)
DR(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, d_p=d_p)



# PSA+STATISTICAL MATCHING
PSA.stat<-function(ys_np, Xs_np, d_np, Xs_p, d_p, method.PSA="lee"){
  
  d.PSA<-PSA(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np,
             Xs_p=Xs_p, d_p=d_p, method=method.PSA)$d.PSA
  
  est<-statistical(ys_np=ys_np, Xs_np=Xs_np, 
                   Xs_p=Xs_p, d_p=d_p, weights=d.PSA, method="lineal")
  
  return(est)
  
}

PSA.stat(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
         Xs_p=Xs_p, d_p=d_p, method.PSA="lee")
PSA.stat(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
         Xs_p=Xs_p, d_p=d_p, method.PSA="valliant")
PSA.stat(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
         Xs_p=Xs_p, d_p=d_p, method.PSA="schonlau")
PSA.stat(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
         Xs_p=Xs_p, d_p=d_p, method.PSA="chen1")
PSA.stat(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
         Xs_p=Xs_p, d_p=d_p, method.PSA="chen2")





#####################################################
# 3) TÉCNICAS PARA MITIGAR EL SESGO EN EL CASO NMAR #
#####################################################

######################################################
# 3.1) ADAPTACIÓN DEL PSA DE BURAKAUSKAITE Y CIGINAS #
######################################################

# Definimos previamente la función que da el método iterativo de 
# Newton-Raphson que aparece en la adaptación del PSA de Burakauskaite
# y Ciginas (2023).

f.BYC<-function(theta, X1_np, X1_p, tol=10^(-3), omega){
  
  X1_c<-rbind(X1_np,X1_p)
  
  n_np<-length(X1_np[,1])
  n_p<-length(X1_p[,1])
  n_c<-length(X1_c[,1])
  
  n_variables<-length(X1_p[1,])
  
  omega_np<-omega[1:n_np]
  
  
  matriz<-matrix(0, nrow=n_variables, ncol=n_variables)
  exponente<-numeric()
  for(i in 1:n_c){
    xi<-as.matrix(X1_c[i,])
    exponente[i]<-as.numeric(xi%*%theta)
    
    matriz<-matriz+omega[i]*exp(exponente[i])/((1+exp(exponente[i]))^2)*t(xi)%*%xi
  }
  
  vect<-colSums(omega_np*X1_np)-colSums(omega*exp(exponente)/(1+exp(exponente))*X1_c)
  
  theta1<-as.vector(theta + pinv(matriz)%*%vect)
  
  if(norma(theta-theta1)<tol){
    return(theta1)
  } else{
    f.BYC(theta1, X1_np=X1_np, X1_p=X1_p, omega=omega)
  }
}

# Definimos una función que, utilizando la función anterior, obtiene 
# los pesos del método de Burakauskaite y Ciginas (2023).

pesos.BYC<-function(ys_np, Xs_np, d_np, ys_p, Xs_p, d_p){
  
  # En primer lugar, transformamos las variables categóricas en
  # variables indicadoras (numéricas).
  Xs_p<-variables_numericas(Xs_p, indep=TRUE)
  Xs_np<-variables_numericas(Xs_np, indep=TRUE)
  
  
  # Obtenemos la muestra combinada.
  Xs_c<-rbind(Xs_np, Xs_p)
  
  # Hallamos los pesos de calibración a partir de la muestra combinada.
  totales.est<-colSums(d_p*Xs_p)
  
  omega<-calib(Xs=Xs_c, d=c(d_np, d_p), total=totales.est, 
               method="linear")*c(d_np,d_p)
  
  
  # Añadimos una columna de unos, que se corresponderá con el 
  # término independiente del modelo de regresión.
  X1_np<-cbind(1,Xs_np,y=ys_np)
  X1_p<-cbind(1,Xs_p,y=ys_p)

  n_np<-length(Xs_np[,1])
  n_variables<-length(X1_p[1,])
  
  
  # Estimamos los valores de los parámetros mediante el método de 
  # Newton-Raphson.
  semilla<-rep(0,n_variables)
  theta.est<-f.BYC(theta=semilla, X1_np=X1_np, X1_p=X1_p, omega=omega)
  
  # Estimamos las propensiones sustituyendo las estimaciones de los
  # parámetros en el modelo logístico.
  exponente<-numeric()
  pi_np<-numeric()
  for(j in 1:n_np){
    exponente<-as.numeric(as.matrix(X1_np[j,])%*%theta.est)
    pi_np[j]<-exp(exponente)/(1+exp(exponente))
  }
  
  # Calculamos los pesos del PSA de Chen, Li y Wu (2019) y el
  # correspondiente estimador de la media poblacional.
  d.BYC<-1/pi_np
  
  return(d.BYC)
  
}


# Finalmente, definimos una función que implementa las cuatro 
# variantes propuestas del método de Burakauskaite y Ciginas, 
# utilizando las funciones anteriores y las del PSA.

BYC<-function(ys_np, Xs_np, d_np, ys_p, Xs_p, d_p, method="3"){
  
  if(method=="1"){ # a) Conocemos los yk, sin calibrar
    
    d.BYC<-PSA(ys_np=ys_np, Xs_np=cbind(Xs_np,ys_np), 
               Xs_p=cbind(Xs_p,ys_p), d_p=d_p, method="chen1")$d.PSA
    
  } else if(method=="2"){ # b) Conocemos los yk, calibrando
    
    d.BYC<-pesos.BYC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np,
                     ys_p=ys_p, Xs_p=Xs_p, d_p=d_p)
    
  } else if(method=="3"){ # c) No conocemos los yk, sin calibrar
    
    # Comenzamos prediciendo los valores de yk en la muestra 
    # probabilística.
    y_gorro<-predicciones(ys_np=ys_np, Xs_np=Xs_np, 
                          Xs_p=Xs_p, method="lineal")
    
    # En este caso, en vez de usar el PSA de Chen, Li y Wu (2019), 
    # se utiliza la aproximación de Ferri-García et al. (2024). 
    # Esto se debe a que, al predecir los valores de la variable 
    # de interés mediante una regresión lineal, estos son linealmente 
    # dependientes con las covariables, lo que hace que la matriz 
    # del método iterativo de Newton-Raphson no sea invertible y, 
    # por tanto, no se pueda aplicar esta versión del PSA. Sin embargo,
    # por construcción, la aproximación no presenta este problema.
    
    d.BYC<-PSA(ys_np=ys_np, Xs_np=cbind(Xs_np,ys_np), d_np=d_np, 
               Xs_p=cbind(Xs_p,ys_np=y_gorro), d_p=d_p, method="chen2")$d.PSA

    
  } else if(method=="4"){ # d) No conocemos los yk, calibrando
    
    # Comenzamos prediciendo los valores de yk en la muestra 
    # probabilística.
    y_gorro<-predicciones(ys_np=ys_np, Xs_np=Xs_np, 
                          Xs_p=Xs_p, method="lineal")
    
    d.BYC<-pesos.BYC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np,
                     ys_p=y_gorro, Xs_p=Xs_p, d_p=d_p)
    
  } else{
    stop("El método introducido no es válido.")
  }
  
  est.BYC<-estimador(y=ys_np, pesos=d.BYC)
  return(list(est.BYC=est.BYC, d.BYC=d.BYC))
}


BYC(ys_np=ys_np, Xs_np=Xs_np, ys_p=ys_p, Xs_p=Xs_p, 
    d_p=d_p, method="1")$est.BYC
BYC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, ys_p=ys_p, 
    Xs_p=Xs_p, d_p=d_p, method="2")$est.BYC
BYC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, 
    d_p=d_p, method="3")$est.BYC
BYC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, 
    d_p=d_p, method="4")$est.BYC









##########################################
# 3.3) ADAPTACIÓN DEL MÉTODO DE BEAUMONT #
#      PARA PALIAR LA FALTA DE RESPUESTA #
##########################################

# Empezamos definiendo una función que ejecute cada una de las 
# iteraciones del método de Beaumont, a la que llamaremos de 
# forma recursiva con la función que implementa el método completo.
iteracion.Beaumont<-function(ys_np, Xs_np, d_np, Xs_p, d_p){
  
  # Comenzamos prediciendo los valores de yk en la muestra 
  # probabilística, pero ahora lo hacemos entrenando un modelo
  # de regresión ponderado por los pesos d_np.
  y_gorro<-predicciones(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, 
                        weights=d_np, method="lineal")
  
  
  # Utilizamos BYC3 para calcular los pesos de la siguiente iteración
  # (con la aproximación de Ferri-García et al. (2024)).
  d.Beaumont<-PSA(ys_np=ys_np, Xs_np=cbind(Xs_np,y=ys_np), 
                  d_np=d_np, Xs_p=cbind(Xs_p,y=y_gorro), 
                  d_p=d_p, method="chen2")$d.PSA
  
  return(d.Beaumont)
  
}


# Definimos la función que implementa el método de Beaumont completo.
Beaumont<-function(ys_np, Xs_np, d_np, Xs_p, d_p, tol=10^(-3), aux=Inf){
  
  # Establecemos un contador de iteraciones.
  cont<-3
  
  # El criterio de parada será el momento en el que se alcance un 
  # mínimo en las normas de las diferencias de los pesos obtenidos 
  # en dos iteraciones consecutivas. Para intentar evitar mínimos 
  # locales, se exige que, tras el mínimo, al menos cinco iteraciones
  # den valores crecientes para dicha norma. Por ello, definimos otro
  # contador para estas iteraciones.
  creciente<-0
  
  # Además, para evitar fallos por pesos demasiado grandes (que 
  # pueden surgir porque se descontrole su crecimiento después 
  # del mínimo, pero antes de poder comprobar las cinco iteraciones 
  # exigidas), se incluye una variable booleana para parar las
  # iteraciones en tales casos.
  parar=FALSE
  
  # Vamos a ir guardando los pesos de todas las iteraciones en una 
  # lista, y las estimaciones del parámetro correspondientes en un 
  # vector. Definimos dicha lista y dicho vector y calculamos las 
  # primeras iteraciones.

  pesos<-list()
  pesos[[1]]<-d_np
  pesos[[2]]<-iteracion.Beaumont(ys_np=ys_np, Xs_np=Xs_np, 
                                 d_np=d_np, Xs_p=Xs_p, d_p=d_p)
  pesos[[3]]<-iteracion.Beaumont(ys_np=ys_np, Xs_np=Xs_np, 
                                 d_np=pesos[[2]], Xs_p=Xs_p, d_p=d_p)
  
  estim<-numeric()
  estim[1]<-estimador(y=ys_np, pesos=pesos[[1]])
  estim[2]<-estimador(y=ys_np, pesos=pesos[[2]])
  estim[3]<-estimador(y=ys_np, pesos=pesos[[3]])
  
  dist.nueva<-norma(pesos[[cont-1]]-pesos[[cont-2]])
  
  # Implementamos el algoritmo iterativo de Beaumont. 
  while(creciente<5 & parar==FALSE){
    
    dist.vieja<-dist.nueva
    dist.nueva<-norma(pesos[[cont]]-pesos[[cont-1]])
    
    # Si la distancia entre los pesos nuevos y los antiguos ha 
    # aumentado mucho, cambiamos el valor de la variable booleana
    # que fuerza el fin del algoritmo iterativo.
    if(dist.nueva>10^10){
      parar=TRUE
    } else{
      # Si la distancia entre los pesos nuevos y los antiguos 
      # decrece, mantenemos el contador de crecimiento a cero
      # y, en caso contrario, lo aumentamos una unidad.
      if(dist.nueva<dist.vieja){
        creciente<-0
      } else{
        creciente<-creciente+1
      }
    
      # Contamos esta iteración y guardamos los pesos y la estimación 
      # correspondiente.
      cont<-cont+1
      pesos[[cont]]<-iteracion.Beaumont(ys_np=ys_np, Xs_np=Xs_np, 
                                        d_np=pesos[[cont-1]], 
                                        Xs_p=Xs_p, d_p=d_p)
    
      estim[cont]<-estimador(y=ys_np, pesos=pesos[[cont]])

    }
  }
  
  return(list(est.Beaumont=estim[cont-creciente],
              d.Beaumont=pesos[[cont-creciente]]))
  
}

Beaumont(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
         Xs_p=Xs_p, d_p=d_p)$est.Beaumont






###################
# 4) SIMULACIÓN 1 #
###################

# En esta sección vamos a repetir varias veces el proceso de tomar 
# una muestra no probabilística y otra probabilística para calcular
# a partir de ellas los distintos estimadores descritos en los 
# apartados previos.

# Con ello, conseguiremos una serie de observaciones de los 
# estimadores y, dado que conocemos el valor real del parámetro,
# podremos comparar el sesgo que se ha producido al estimarlo 
# mediante cada estimador, la varianza de cada uno de ellos y 
# el Error Cuadrático Medio cometido. Así, podremos ver qué 
# método da mejores resultados a la hora de reducir el sesgo 
# de selección en cada caso.

# Indicamos el número de veces que vamos a repetir el proceso
# en la simulación.
repeticiones<-1000

# Introducimos también los tamaños muestrales para la simulación.
n_p<-100
n_np<-100


# Comenzamos con la repetición del proceso en un bucle "for". 
# Antes de entrar en él, definimos un data frame en el que iremos
# guardando los resultados de cada ejecución del bucle. Además, 
# guardamos el tiempo del sistema al inicio de la simulación para
# poder calcular el tiempo que ha tardado el proceso.
t1<-Sys.time()
obs<-data.frame(Observaciones=1:repeticiones)

for(i in 1:repeticiones){
  
  # Obtenemos las muestras mediante la función de muestreo 
  # definida previamente.
  realizacion<-muestreo(n_p=n_p, n_np=n_np)
  indices_p<-realizacion$indices_p
  indices_np<-realizacion$indices_np
  d_p<-realizacion$d_p
  d_np<-realizacion$d_np
  
  # Guardamos en nuevos vectores la información de las variables 
  # en las muestras.
  ys_p<-y[indices_p]
  ys_np<-y[indices_np]
  
  Xs_p<-X[indices_p,]
  Xs_np<-X[indices_np,]
  
  totales<-colSums(variables_numericas(X, indep=FALSE))
  
  # 1.1) ESTIMADOR DE HAJEK (a partir de la muestra probabilística)
  # Lo utilizaremos para comparar los resultados obtenidos cuando 
  # se dispone de la información de la variable de interés en la 
  # muestra probabilística# con los resultados que se obtienen a 
  # partir de las técnicas estudiadas en los casos en los que no
  # se dispone de dicha información. 
  # Además, podremos ver si se gana algo al utilizar la información
  # de la variable de interés en ambas muestras en BYC1 y BYC2.
  obs$Hajek.P[i]<-estimador(y=ys_p, pesos=d_p)
  
  # 1.2) ESTIMADOR DE HAJEK (a partir de la muestra no probabilística)
  # Al comparar los resultados obtenidos al utilizar este estimador 
  # con los obtenidos al aplicar cualquiera de las técnicas estudiadas,
  # podremos ver si estas son efectivas para reducir el sesgo de 
  # selección.
  obs$Hajek.NP[i]<-estimador(y=ys_np, pesos=d_np)
  
  
  # 2.1.1) CALIBRACIÓN
  obs$Calib[i]<-calibracion(Xs=Xs_np, ys=ys_np, 
                            d=d_np, total=totales)$est.cal
  obs$Calib2[i]<-calibracion2(Xs_p=Xs_p, d_p=d_p, Xs_np=Xs_np,
                              ys_np=ys_np, d_np=d_np)$est.cal

  # 2.1.2) PSA
  obs$`PSA-LV`[i]<-PSA(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, 
                       d_p=d_p, n_clases=5, method="lee")$est.PSA
  obs$`PSA-VD`[i]<-PSA(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p,
                       d_p=d_p, n_clases=5, method="valliant")$est.PSA
  obs$`PSA-SC`[i]<-PSA(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, 
                       method="schonlau")$est.PSA
  obs$`PSA-CLW1`[i]<-PSA(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, d_p=d_p, 
                         method="chen1")$est.PSA
  obs$`PSA-CLW2`[i]<-PSA(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, 
                         d_p=d_p, method="chen2")$est.PSA
  
  # PSA+CALIBRACIÓN
  obs$`PSA-LV+Calib`[i]<-PSA.cal(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
                                 Xs_p=Xs_p, d_p=d_p, n_clases=5, 
                                 method="lee", total=totales)
  obs$`PSA-VD+Calib`[i]<-PSA.cal(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
                                 Xs_p=Xs_p, d_p=d_p, n_clases=5, 
                                 method="valliant", total=totales)
  obs$`PSA-SC+Calib`[i]<-PSA.cal(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
                                 Xs_p=Xs_p, d_p=d_p, n_clases=5, 
                                 method="schonlau", total=totales)
  obs$`PSA-CLW1+Calib`[i]<-PSA.cal(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
                                   Xs_p=Xs_p, d_p=d_p, n_clases=5, 
                                   method="chen1", total=totales)
  obs$`PSA-CLW2+Calib`[i]<-PSA.cal(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
                                   Xs_p=Xs_p, d_p=d_p, n_clases=5, 
                                   method="chen2", total=totales)
  
  # 2.1.3) CUASI-ALEATORIZACIÓN
  obs$QA[i]<-QA(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
                                 Xs_p=Xs_p, d_p=d_p, N=N)$est.QA
  
  # 2.1.4) KERNEL WEIGHTING
  obs$KW[i]<-KW(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, d_p=d_p)$est.KW

  
  # 2.2.1) STATISTICAL MATCHING
  obs$`SM-L`[i]<-statistical(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, 
                             d_p=d_p, method="lineal")
  obs$`SM-LOG`[i]<-statistical(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, 
                               d_p=d_p, method="logit")
  obs$`SM-1NN`[i]<-statistical(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, 
                               d_p=d_p, method="neighbour1")
  obs$`SM-kNN`[i]<-statistical(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, 
                               d_p=d_p, method="neighbour2")
  
  # 2.2.2) MODELO-ASISTIDO, MODELO-BASADO Y MODELO-CALIBRADO
  obs$MA[i]<-MABC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
                  indices_np=indices_np, X=X, method="MA")
  obs$MB[i]<-MABC(ys_np=ys_np, Xs_np=Xs_np, indices_np=indices_np, 
                  X=X, method="MB")
  obs$MC1[i]<-MABC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
                   indices_np=indices_np, X=X, method="MC1")
  obs$MC2[i]<-MABC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
                   indices_np=indices_np, X=X, method="MC2")
  
  
  # 2.3) ESTIMADORES DOBLEMENTE ROBUSTOS
  obs$DR1[i]<-DR(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, d_p=d_p, N=N)
  obs$DR2[i]<-DR(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, d_p=d_p)
  
  
  # PSA+STATISTICAL MATCHING
  obs$`PSA-LV+SM`[i]<-PSA.stat(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
                               Xs_p=Xs_p, d_p=d_p, method.PSA="lee")
  obs$`PSA-VD+SM`[i]<-PSA.stat(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
                               Xs_p=Xs_p, d_p=d_p, method.PSA="valliant")
  obs$`PSA-SC+SM`[i]<-PSA.stat(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
                               Xs_p=Xs_p, d_p=d_p, method.PSA="schonlau")
  obs$`PSA-CLW1+SM`[i]<-PSA.stat(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
                                 Xs_p=Xs_p, d_p=d_p, method.PSA="chen1")
  obs$`PSA-CLW2+SM`[i]<-PSA.stat(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
                                 Xs_p=Xs_p, d_p=d_p, method.PSA="chen2")
  
  
  # 3.1) ADAPTACIÓN DEL PSA DE BURAKAUSKAITE Y CIGINAS
  obs$BYC1[i]<-BYC(ys_np=ys_np, Xs_np=Xs_np, ys_p=ys_p, Xs_p=Xs_p, 
                   d_p=d_p, method="1")$est.BYC
  obs$BYC2[i]<-BYC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, ys_p=ys_p, 
                   Xs_p=Xs_p, d_p=d_p, method="2")$est.BYC
  obs$BYC3[i]<-BYC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, 
                   d_p=d_p, method="3")$est.BYC 
  obs$BYC4[i]<-BYC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, 
                   d_p=d_p, method="4")$est.BYC
  
  
  # 3.3) ADAPTACIÓN DEL MÉTODO DE BEAUMONT
  obs$Beaumont[i]<-Beaumont(ys_np=ys_np, Xs_np=Xs_np, 
                            d_np=d_np, Xs_p=Xs_p, d_p=d_p)$est.Beaumont
  
  # Cada vez que se termina de ejecutar una iteración, se muestra
  # por pantalla el tiempo de ejecución requerido hasta el momento.
  # Con ello, el usuario que ejecute el código puede comprobar que 
  # este se está ejecutando bien y, dado que todas las iteraciones 
  # requieren un tiempo de ejecución similar, puede estimar el 
  # tiempo restante hasta completar la simulación.
  print(paste("Tiempo hasta la repetición ",i,":", sep=""))
  print(Sys.time()-t1)
  
}

# Al finalizar la simulación, se guarda el tiempo del sistema y se
# le resta el tiempo de inicio para ver cuánto ha tardado en ejecutarse 
# la simulación completa.
t2<-Sys.time()
t2-t1



# Para presentar los resultados del estudio de simulación, como 
# conocemos el valor real del parámetro, vamos a calcular el sesgo 
# que se ha producido, la varianza del estimador y el Error Cuadrático
# Medio.

# Definimos una función para calcular el Error Cuadrático Medio 
# cometido.
f.ECM<-function(observaciones,v.real){
  sum((observaciones-v.real)^2)/length(observaciones)
}

# Calculamos las medidas mencionadas.
Esperanza<-as.numeric(lapply(obs[-1],mean))
Sesgo<-Esperanza-v.real
Varianza<-as.numeric(lapply(obs[-1],var))
ECM<-c(as.numeric(lapply(obs[-1],f.ECM,v.real=v.real)))

# Finalmente, guardamos estos resultados en un data frame y lo
# mostramos.
Resultados<-data.frame(Sesgo,Varianza,ECM, row.names=names(obs[-1]))
View(Resultados)



# Para obtener una tabla en formato LaTeX, utilizaremos la 
# función "xtable" del paquete "xtable" como sigue:
#install.packages("xtable")
library(xtable)
xtable(Resultados, digits=6, display=c("s","f","e","e"))



# REPRESENTACIONES GRÁFICAS DE LOS RESULTADOS

# Para ayudar a la interpretación de los resultados de la 
# simulación, estos se representan mediante gráficos de tipo
# boxplot. Como son muchos, se separan en varios gráficos por 
# subgrupos de estimadores. Además, en todos los gráficos se 
# incluye una línea horizontal de color azul, correspondiente
# al valor real del parámetro de interés.

# 1) Comparación entre las variantes del método de calibración.
boxplot(obs[c("Hajek.P","Hajek.NP","Calib","Calib2")])
abline(h=v.real, lwd=3, col="blue")

# 2) Comparación entre las variantes del PSA.
boxplot(obs[c("Hajek.P","Hajek.NP","PSA-LV","PSA-VD","PSA-SC",
              "PSA-CLW1","PSA-CLW2")])
abline(h=v.real, lwd=3, col="blue")

# 3) Comparación entre las variantes del PSA+Calibración.
boxplot(obs[c("Hajek.P","Hajek.NP","Calib","PSA-LV","PSA-LV+Calib",
              "PSA-VD","PSA-VD+Calib","PSA-SC","PSA-SC+Calib",
              "PSA-CLW1","PSA-CLW1+Calib","PSA-CLW2","PSA-CLW2+Calib")])
abline(h=v.real, lwd=3, col="blue")

# 4) Comparación entre las variantes del PSA+SM.
boxplot(obs[c("Hajek.P","Hajek.NP","SM-L","PSA-LV","PSA-LV+SM",
              "PSA-VD","PSA-VD+SM","PSA-SC","PSA-SC+SM","PSA-CLW1",
              "PSA-CLW1+SM","PSA-CLW2","PSA-CLW2+SM")])
abline(h=v.real, lwd=3, col="blue")

# 5) Resultados de las técnicas basadas en el diseño.
boxplot(obs[c("Hajek.P","Hajek.NP","Calib","PSA-CLW1","QA","KW")])
abline(h=v.real, lwd=3, col="blue")

# 6) Resultados de las técnicas basadas en el modelo.
boxplot(obs[c("Hajek.P","Hajek.NP","SM-L","SM-LOG",
              "SM-1NN","SM-kNN","MA","MB","MC1","MC2")])
abline(h=v.real, lwd=3, col="blue")

# 7) Resultados de las técnicas para el caso MAR.
boxplot(obs[c("Hajek.P","Hajek.NP","Calib","PSA-CLW1",
              "QA","KW","SM-L","MB","DR1","DR2")])
abline(h=v.real, lwd=3, col="blue")

# 8) Resultados de las técnicas para el caso NMAR.
boxplot(obs[c("Hajek.P","Hajek.NP","BYC1","BYC2",
              "BYC3","BYC4","Beaumont")])
abline(h=v.real, lwd=3, col="blue")

# 9) Resumen global de resultados (Simulación 1).
boxplot(obs[c("Hajek.P","Hajek.NP","Calib","PSA-CLW1",
              "QA","KW","SM-L","MB","DR1","BYC1",
              "BYC3","Beaumont")])
abline(h=v.real, lwd=3, col="blue")







#############################################
# 5) SIMULACIÓN 2 - MÉTODO MIXTO DE MARELLA #
#############################################

# Como el método de Marella requiere que las muestras se solapen, 
# tenemos que tratarlo aparte. Para lograr solapamiento de las 
# muestras, vamos a aumentar el tamaño muestral con respecto al
# tamaño poblacional.

# Consideramos los siguientes tamaños muestrales:
n_p<-2500 #Tamaño de la muestra probabilística.
n_np<-2500 #Tamaño de la muestra no probabilística.

# Al aumentar tanto el tamaño de muestra con respecto al total 
# poblacional también solventamos el problema de que obtengamos
# muestras que sólo incluyan a individuos nativos, pues ahora la
# probabilidad de que no haya ningún no nativo en la muestra es 
# muy baja.

# Además, si intentamos utilizar la función "muestreo" definida 
# anteriormente, nos encontramos con que nos da error, pues al 
# forzar que la proporción de individuos no nativos en la muestra
# no probabilística sea igual que en la población objetivo, nos 
# encontramos con que no hay tantos individuos no nativos en la 
# subpoblación con acceso a Internet como para que esto se cumpla. 

# Por ello, vamos a definir una nueva función de muestreo, 
# "muestreo2", que devolverá, simplemente, un muestreo aleatorio
# simple de la población como muestra probabilística y un muestreo
# aleatorio simple de la subpoblación con acceso a Internet como 
# muestra no probabilística.

muestreo2<-function(n_p, n_np){
  
  #MUESTRA PROBABILÍSTICA
  indices_p<-sample(1:N, size=n_p, replace=FALSE)
  d_p<-rep(N/n_p,n_p) #Pesos del muestreo probabilístico
  
  #MUESTRA NO PROBABILÍSTICA
  indices_np<-sample(which(pob_int==1), size=n_np, replace=FALSE) 
  d_np<-rep(1,n_np) #Pesos del muestreo no probabilístico
  
  return(list(indices_p=indices_p, d_p=d_p, 
              indices_np=indices_np, d_np=d_np))
  
}

# Obtenemos las muestras mediante esta nueva función de muestreo.
realizacion<-muestreo2(n_p=n_p, n_np=n_np)
indices_p<-realizacion$indices_p
indices_np<-realizacion$indices_np
d_p<-realizacion$d_p
d_np<-realizacion$d_np

# Guardamos en nuevos vectores la información de las variables
# en las muestras.
ys_p<-y[indices_p]
ys_np<-y[indices_np]

Xs_p<-X[indices_p,]
Xs_np<-X[indices_np,]



###########################
# MÉTODO MIXTO DE MARELLA #
###########################

# Ahora, definimos una función en la que se implementa el método 
# mixto de Marella. Esta función llama a la función "predicciones" 
# para predecir los valores de la variable de interés en la muestra
# probabilística, luego podremos utilizar los distintos métodos de 
# predicción allí implementados.
# Concretamente, utilizaremos el algorimto del vecino más próximo y 
# el algoritmo de los k vecinos más próximos.

Marella<-function(ys_np, Xs_np, d_np, Xs_p, d_p, method="neighbour1"){
  
  # Comenzamos prediciendo los valores de yk en la muestra 
  # probabilística
  y_gorro<-predicciones(ys_np=ys_np, Xs_np=Xs_np, 
                        Xs_p=Xs_p, method=method)

  # Definimos la variable indicadora de pertenencia a la muestra no 
  # probabilística en la muestra combinada.
  n_p<-length(d_p)
  delta<-rep(0,n_p)
  delta[indices_p %in% indices_np]<-1
  
  # Ajustamos un modelo de regresión logística para estimar las 
  # propensiones de pertenencia a la muestra no probabilística.
  formula<-as.formula(paste("delta~",
                            paste(colnames(cbind(Xs_p,y=y_gorro)),
                                           collapse="+"),sep=""))
  reg_logit<-glm(formula, data=cbind(Xs_p,y=y_gorro), family="binomial") 
  
  pi_np<-predict(object=reg_logit, newdata=cbind(Xs_np,y=ys_np), 
                 type="response")

  d.Marella<-1/pi_np
  
  est.Marella<-estimador(y=ys_np, pesos=d.Marella)
  return(list(est.Marella=est.Marella, 
              d.Marella=d.Marella, prueba=y_gorro))
  
}

Marella(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, d_p=d_p, 
        method="neighbour1")$est.Marella
Marella(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, d_p=d_p, 
        method="neighbour2")$est.Marella





################
# SIMULACIÓN 2 #
################

# De nuevo, vamos a repetir varias veces el proceso de tomar una
# muestra no probabilística y otra probabilística para calcular a
# partir de ellas el estimador del método mixto de Marella y los 
# estimadores más relevantes entre los descritos previamente. No 
# se incluyen todos debido al hecho de que, al considerar muestras
# tan grandes, se requieren muchos más recursos y, en consecuencia,
# un tiempo de ejecución mucho mayor.

# Con ello, conseguiremos una serie de observaciones de los 
# estimadores y, dado que conocemos el valor real del parámetro,
# podremos comparar el sesgo que se ha producido al estimarlo mediante
# cada estimador, la varianza de cada uno de ellos y el ECM cometido.
# Así, podremos ver qué método da mejores resultados a la hora de 
# reducir el sesgo de selección en cada caso.

# Indicamos el número de veces que vamos a repetir el proceso en 
# la simulación.
repeticiones<-500

# Introducimos también los tamaños muestrales para la simulación.
n_p<-2500
n_np<-2500

# Comenzamos con la repetición del proceso en un bucle "for". Antes 
# de entrar en él, definimos un data frame en el que iremos guardando 
# los resultados de cada ejecución del bucle. Además, guardamos el 
# tiempo del sistema al inicio de la simulación para poder calcular
# el tiempo que ha tardado el proceso.
t1<-Sys.time()
obs<-data.frame(Observaciones=1:repeticiones)

for(i in 1:repeticiones){
  
  # Obtenemos las muestras mediante la nueva función de muestreo.
  realizacion<-muestreo2(n_p=n_p, n_np=n_np)
  indices_p<-realizacion$indices_p
  indices_np<-realizacion$indices_np
  d_p<-realizacion$d_p
  d_np<-realizacion$d_np
  
  # Guardamos en nuevos vectores la información de las variables 
  # en las muestras.
  ys_p<-y[indices_p]
  ys_np<-y[indices_np]
  
  Xs_p<-X[indices_p,]
  Xs_np<-X[indices_np,]
  
  totales<-colSums(variables_numericas(X, indep=FALSE))
  
  # 1.1) ESTIMADOR DE HAJEK (a partir de la muestra probabilística)
  # Lo utilizaremos para comparar los resultados obtenidos cuando
  # se dispone de la información de la variable de interés en la 
  # muestra probabilística con los resultados que se obtienen a 
  # partir de las técnicas estudiadas en los casos en los que no 
  # se dispone de dicha información. 
  # Además, podremos ver si se gana algo al utilizar la información 
  # de la variable de interés en ambas muestras en BYC1 y BYC2.
  obs$Hajek.P[i]<-estimador(y=ys_p, pesos=d_p)
  
  # 1.2) ESTIMADOR DE HAJEK (a partir de la muestra no probabilística)
  # Al comparar los resultados obtenidos al utilizar este estimador 
  # con los obtenidos al aplicar cualquiera de las técnicas estudiadas,
  # podremos ver si estas son efectivas para reducir el sesgo de 
  # selección.
  obs$Hajek.NP[i]<-estimador(y=ys_np, pesos=d_np)
  
  
  
  # 2.1.1) CALIBRACIÓN
  obs$Calib[i]<-calibracion(Xs=Xs_np, ys=ys_np, 
                            d=d_np, total=totales)$est.cal
  #obs$Calib2[i]<-calibracion2(Xs_p=Xs_p, d_p=d_p, Xs_np=Xs_np,
  #                            ys_np=ys_np, d_np=d_np)$est.cal
  
  # 2.1.2) PSA
  #obs$`PSA-LV`[i]<-PSA(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, 
  #                     d_p=d_p, n_clases=5, method="lee")$est.PSA
  #obs$`PSA-VD`[i]<-PSA(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p,
  #                     d_p=d_p, n_clases=5, method="valliant")$est.PSA
  #obs$`PSA-SC`[i]<-PSA(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, 
  #                     method="schonlau")$est.PSA
  obs$`PSA-CLW1`[i]<-PSA(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, d_p=d_p, 
                         method="chen1")$est.PSA
  #obs$`PSA-CLW2`[i]<-PSA(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, 
  #                       d_p=d_p, method="chen2")$est.PSA
  
  # PSA+CALIBRACIÓN
  #obs$`PSA-LV+Calib`[i]<-PSA.cal(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
  #                               Xs_p=Xs_p, d_p=d_p, n_clases=5, 
  #                               method="lee", total=totales)
  #obs$`PSA-VD+Calib`[i]<-PSA.cal(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
  #                               Xs_p=Xs_p, d_p=d_p, n_clases=5, 
  #                               method="valliant", total=totales)
  #obs$`PSA-SC+Calib`[i]<-PSA.cal(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
  #                               Xs_p=Xs_p, d_p=d_p, n_clases=5, 
  #                               method="schonlau", total=totales)
  #obs$`PSA-CLW1+Calib`[i]<-PSA.cal(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
  #                                 Xs_p=Xs_p, d_p=d_p, n_clases=5, 
  #                                 method="chen1", total=totales)
  #obs$`PSA-CLW2+Calib`[i]<-PSA.cal(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
  #                                 Xs_p=Xs_p, d_p=d_p, n_clases=5, 
  #                                 method="chen2", total=totales)
  
  # 2.1.3) CUASI-ALEATORIZACIÓN
  #obs$QA[i]<-QA(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
  #              Xs_p=Xs_p, d_p=d_p, N=N)$est.QA
  
  # 2.1.4) KERNEL WEIGHTING
  #obs$KW[i]<-KW(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, d_p=d_p)$est.KW
  
  
  # 2.2.1) STATISTICAL MATCHING
  obs$`SM-L`[i]<-statistical(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, 
                             d_p=d_p, method="lineal")
  #obs$`SM-LOG`[i]<-statistical(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, 
  #                             d_p=d_p, method="logit")
  #obs$`SM-1NN`[i]<-statistical(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, 
  #                             d_p=d_p, method="neighbour1")
  #obs$`SM-kNN`[i]<-statistical(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, 
  #                             d_p=d_p, method="neighbour2")
  
  # 2.2.2) MODELO-ASISTIDO, MODELO-BASADO Y MODELO-CALIBRADO
  #obs$MA[i]<-MABC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
  #                indices_np=indices_np, X=X, method="MA")
  #obs$MB[i]<-MABC(ys_np=ys_np, Xs_np=Xs_np, indices_np=indices_np, 
  #                X=X, method="MB")
  #obs$MC1[i]<-MABC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
  #                 indices_np=indices_np, X=X, method="MC1")
  #obs$MC2[i]<-MABC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
  #                 indices_np=indices_np, X=X, method="MC2")
  
  
  # 2.3) ESTIMADORES DOBLEMENTE ROBUSTOS
  obs$DR1[i]<-DR(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, d_p=d_p, N=N)
  #obs$DR2[i]<-DR(ys_np=ys_np, Xs_np=Xs_np, Xs_p=Xs_p, d_p=d_p)
  
  
  # PSA+STATISTICAL MATCHING
  #obs$`PSA-LV+SM`[i]<-PSA.stat(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
  #                             Xs_p=Xs_p, d_p=d_p, method.PSA="lee")
  #obs$`PSA-VD+SM`[i]<-PSA.stat(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
  #                             Xs_p=Xs_p, d_p=d_p, method.PSA="valliant")
  #obs$`PSA-SC+SM`[i]<-PSA.stat(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
  #                             Xs_p=Xs_p, d_p=d_p, method.PSA="schonlau")
  #obs$`PSA-CLW1+SM`[i]<-PSA.stat(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
  #                               Xs_p=Xs_p, d_p=d_p, method.PSA="chen1")
  #obs$`PSA-CLW2+SM`[i]<-PSA.stat(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, 
  #                               Xs_p=Xs_p, d_p=d_p, method.PSA="chen2")
  
  
  # 3.1) ADAPTACIÓN DEL PSA DE BURAKAUSKAITE Y CIGINAS
  obs$BYC1[i]<-BYC(ys_np=ys_np, Xs_np=Xs_np, ys_p=ys_p, Xs_p=Xs_p, 
                   d_p=d_p, method="1")$est.BYC
  #obs$BYC2[i]<-BYC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, ys_p=ys_p, 
  #                 Xs_p=Xs_p, d_p=d_p, method="2")$est.BYC
  obs$BYC3[i]<-BYC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, 
                   d_p=d_p, method="3")$est.BYC 
  #obs$BYC4[i]<-BYC(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, 
  #                 d_p=d_p, method="4")$est.BYC
  
 
  # 3.2) MÉTODO MIXTO DE MARELLA
  obs$Marella1[i]<-Marella(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p,
                           d_p=d_p, method="neighbour1")$est.Marella
  obs$Marella2[i]<-Marella(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np, Xs_p=Xs_p, 
                           d_p=d_p, method="neighbour2")$est.Marella
  
  # 3.3) ADAPTACIÓN DEL MÉTODO DE BEAUMONT
  # obs$Beaumont[i]<-Beaumont(ys_np=ys_np, Xs_np=Xs_np, d_np=d_np,
  #                           Xs_p=Xs_p, d_p=d_p)$est.Beaumont
  
  
  # Cada vez que se termina de ejecutar una iteración, se muestra
  # por pantalla el tiempo de ejecución requerido hasta el momento. 
  # Con ello, el usuario que ejecute el código puede comprobar que 
  # este se está ejecutando bien y, dado que todas las iteraciones 
  # requieren un tiempo de ejecución similar, puede estimar el tiempo
  # restante hasta completar la simulación.
  print(paste("Tiempo hasta la repetición ",i,":", sep=""))
  print(Sys.time()-t1)
  
}

# Al finalizar la simulación, se guarda el tiempo del sistema y se
# le resta el tiempo de inicio para ver cuánto ha tardado en ejecutarse
# la simulación completa.
t2<-Sys.time()
t2-t1



# Para presentar los resultados del estudio de simulación, como 
# conocemos el valor real del parámetro, vamos a calcular el sesgo 
# que se ha producido, la varianza del estimador y el ECM. 

# Definimos una función para calcular el Error Cuadrático Medio 
# cometido.
f.ECM<-function(observaciones,v.real){
  sum((observaciones-v.real)^2)/length(observaciones)
}

# Calculamos las medidas mencionadas.
Esperanza<-as.numeric(lapply(obs[-1],mean))
Sesgo<-Esperanza-v.real
Varianza<-as.numeric(lapply(obs[-1],var))
ECM<-c(as.numeric(lapply(obs[-1],f.ECM,v.real=v.real)))

# Finalmente, guardamos estos resultados en un data frame 
# y lo mostramos.
Resultados<-data.frame(Sesgo,Varianza,ECM, row.names=names(obs[-1]))
View(Resultados)



# Para obtener una tabla en formato LaTeX, utilizaremos la 
# función "xtable" del paquete "xtable" como sigue:
#install.packages("xtable")
library(xtable)
xtable(Resultados, digits=6, display=c("s","f","e","e"))



# REPRESENTACIÓN GRÁFICA DE LOS RESULTADOS

# Para ayudar a la interpretación de los resultados de la 
# simulación, estos se representan mediante un gráfico de tipo 
# boxplot. Cabe destacar que este incluye una línea horizontal
# de color azul, correspondiente al valor real del parámetro de 
# interés.
boxplot(obs[-1])
abline(h=v.real, lwd=3, col="blue")





