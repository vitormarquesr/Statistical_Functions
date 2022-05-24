library(tidyverse)

levene_test <- function(df){
  colnames(df) <- c("Values", "Group")
  
  
  df_n <- df %>% group_by(Group) %>% 
    mutate(Median=median(Values),
           Abs_Deviation=abs(Values-Median)) %>%
    ungroup()
  
  Overall_Mean <- mean(df_n$Abs_Deviation)
  
  print(df_n)
  
  cat("-----------------------\n")
  df_s <- df_n %>% group_by(Group) %>%
    summarise(n=n(), Mean_Group=mean(Abs_Deviation),
              GSQRes = sum((Abs_Deviation - Mean_Group)^2),
              GSQTrat=n*(Mean_Group-Overall_Mean)^2)
  
  print(df_s)
  cat("-----------------------\n")
  
  N <- sum(df_s$n)
  k <- nrow(df_s)
  SQT <- sum((df_n$Abs_Deviation - Overall_Mean)^2)
  SQTrat <- sum(df_s$GSQTrat)
  SQRes <- sum(df_s$GSQRes)
  
  cat("[+]N = ", N, "\n")
  cat("[+]K = ", k, "\n")
  cat("[+]SQT = ", SQT, "\n")
  cat("[+]SQTrat = ", SQTrat, "\n")
  cat("[+]SQRes = ", SQRes, "\n")
  
  f <- (SQTrat/(k-1))/(SQRes/(N-k))
  
  cat("[+]F statistic = ", f, "\n")
  
}
