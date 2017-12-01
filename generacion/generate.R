source("scripts/load.R")

#Crea un vector de 0's y 1's
generarador.zeros_ones<-function(N = 100, factor=FALSE){
  vector<-NULL
  for(i in 1:N){
    if (sample(0:1,1))
      vector[i]<-1
    else
      vector[i]<-0
  }
  if(factor)
    vector<-factor(vector, levels=c("si","no"))
  return (vector)    
}

generarador.generar<-function(N = 100){

  #Definiendo variable sexo
  sexo<- sex(N, x=c("Hombre","Mujer"))
  
  #Definiendo edades con probailidades dispersas
  edad<-c()
  for(i in 1:N){
    num<-sample(1:8,1)
    if(num <6)
      edad[i]<-sample(14:25, 1)
    else if(num >=6 & num <8)
      edad[i]<-sample(26:45, 1)
    else
      edad[i]<-sample(55:65, 1)
  }
  #Definiendo vector de depresion
  depresion<-generarador.zeros_ones(N)
  
  #Definiendo vector de intentos previos de suicidio
  intentosPrevios<-generarador.zeros_ones(N)
  
  #Definiendo vector de consumo de alcohol
  alcohol<-generarador.zeros_ones(N)
  
  #Definiendo ausencia de pensamiento racional
  ausPensRac<-generarador.zeros_ones(N)
  
  #Definiendo ausencia de apoyo emocional
  ausApoSoc <-generarador.zeros_ones(N)
  
  #Definiendo vector sobre si ya ha estado planeando el suicidio
  planeacion <-generarador.zeros_ones(N)
  
  #Definiendo vector con antecedentes de problema salud
  proSalud <-generarador.zeros_ones(N)
  
  #Definiendo vector con pareja
  pareja <-generarador.zeros_ones(N)
  
  datos<-data.frame(sexo, edad,depresion,intentosPrevios,alcohol,ausPensRac,ausApoSoc,planeacion,proSalud,pareja)
 
  #Definiendo la puntuación asociada a los factores
  unidos<-unite(datos,col="cantidad",3:10,sep=" ", remove = T) %>% select(cantidad)
  puntuacion<-str_count(as.vector(unidos$cantidad),"1") + as.integer(datos$sexo == "Hombre") + as.integer(datos$edad <=20 | datos$edad>=45)
  puntuacion<-puntuacion*rep(1,10)
  
  datos$puntuacion <- puntuacion
  #Definiendo el dataframe
  
  write.csv2(datos,file="salida/output.csv")
}


generarador.generar(500)


