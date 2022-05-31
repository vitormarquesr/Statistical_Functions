library(tidyverse)

spearman <- function(df, p=0.95){
  colnames(df) <- c("X", "Y")
  
  df_n <- df %>%
    arrange(X) %>%
    mutate(Posts_X=rank(X, ties.method='average'),
           Posts_Y=rank(Y, ties.method='average')) %>%
    select(X, Posts_X, Y, Posts_Y)
  
  print(df_n)
  cat("--------------------------\n")
  
  n <- nrow(df)
  Sx2 <- sum(df_n$Posts_X^2)
  Sy2 <- sum(df_n$Posts_Y^2)
  Sxy <- sum(df_n$Posts_X*df_n$Posts_Y)
  
  ax <- n*((n+1)/2)^2
  rs <- (Sxy - ax)/(((Sx2-ax)^0.5)*((Sy2-ax)^0.5))
  
  cat("[+]n = ", n, "\n")
  cat("[+]sum(r^2) =", Sx2, "\n")
  cat("[+]sum(s^2) =", Sy2, "\n")
  cat("[+]sum(s*r) =", Sxy, "\n")
  cat("\n")
  cat("Test statistics and Spearman correlation coefficient:\n")
  cat("[+]rs = ", rs, "\n")
  
  cat("\n")
  if(any(duplicated(df$X) || duplicated(df$Y))){
    cat("[!]There are ties. Should use assintotic aproximation.\n")
  }else{
    cat("[!]There are no ties.\n")
  }
  
  
  cat("[+]Critical point: \n")
  wp <- qnorm(p)/sqrt(n-1)
  cat("[+]W(",100*p,"%) =", wp, "\n")
}

