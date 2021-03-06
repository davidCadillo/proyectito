library(lazyLoadR)
checkPackages(c("wakefield","dplyr","tidyr","stringr"))


zeros_ones<-function(q=100, factor=FALSE){
  vector<-NULL
  for(i in 1:q){
    if (sample(0:1,1))
      vector[i]<-1
    else
      vector[i]<-0
  }
  if(factor)
    vector<-factor(vector, levels=c("si","no"))
  return (vector)    
}
