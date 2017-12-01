###############################################################################
# Crear una clase que permite generar factores de suicidio
#
##############################################################################

GeneradorFactores<- setClass(
  "GeneradorFactores",
  
  #Se define los campos que contiene la clase
  slots = c(
    N="numeric",
    year="numeric",
    data="dataframe"
  ),
  
  #Funciona como constructor de la clase
  prototype = list(
    N=200,
    year=1960
  ),
  
  #Función de ámbito privado útil para generar 0's y 1's que indican ausencia de factor.
  print<-function(object){
    return (object@year + 30)
  }
  
  
)


