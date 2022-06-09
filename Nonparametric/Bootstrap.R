library(tidyverse)

bootstrap <- function(samp, nrep=1000, func, ...){
  emp_dis <- vector(mode='double', length=nrep)
  
  emp_dis <- map_dbl(emp_dis, function(x){
    func(sample(samp, size=length(samp), replace=TRUE),...)
  })
  
  return(emp_dis)
}
