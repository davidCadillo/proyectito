source("scripts/load.R")

N<-100

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
depresion<-zeros_ones(N)

#Definiendo vector de intentos previos de suicidio
intentosPrevios<-zeros_ones(N)

#Definiendo vector de consumo de alcohol
alcohol<-zeros_ones(N)

#Definiendo ausencia de pensamiento racional
ausPensRac<-zeros_ones(N)

#Definiendo ausencia de apoyo emocional
ausApoSoc <-zeros_ones(N)

#Definiendo vector sobre si ya ha estado planeando el suicidio
planeacion <-zeros_ones(N)

#Definiendo vector con antecedentes de problema salud
proSalud <-zeros_ones(N)

#Definiendo vector con pareja
pareja <-zeros_ones(N)

#Definiendo el dataframe
datos<-data.frame(sexo, edad,depresion,intentosPrevios,alcohol,ausPensRac,ausApoSoc,planeacion,proSalud,pareja)

calcular<-function(df, pesos=rep(1,10), num_riesgo=c(2,4,6,10),
                   riesgo=c("Sin riesgo","Riesgo bajo","Riesgo medio","Riesgo alto")){
  unidos<-unite(datos,col="cantidad",3:10,sep=" ", remove = T) %>% select("cantidad")
  puntos<-str_count(as.vector(unidos$cantidad),"si") + as.integer(datos$sexo == "Hombre") + as.integer(datos$edad <=20 | datos$edad>=45)
   
  puntos<-puntos*pesos
  return (puntos)
}

guardar<-function(df){
  write.csv2(df,file="salida/output.csv")
}

guardar(datos)

datos$puntuacion <- calcular(datos)
View(datos)




