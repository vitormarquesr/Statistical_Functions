library(tidyverse)

kendall <- function(df, p=0.95){
  colnames(df) <- c("X", "Y")
  
  df_n <- df %>%
    arrange(X)
  nc <- 0
  nd <- 0
  
  ties <- FALSE
  n <- nrow(df_n)
  
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      if (df_n$X[i] == df_n$X[j]){
        ties <- TRUE
        next
      }
      if (df_n$Y[i] == df_n$Y[j]){
        ties <- TRUE
        nc <- nc + 0.5
        nd <- nd + 0.5
      }else if(df_n$Y[i] < df_n$Y[j]){
        nc <- nc + 1
      }else{
        nd <- nd + 1
      }
    }
  }
  cat("[+]n =", n, "\n")
  cat("[+]nc = ", nc, "\n")
  cat("[+]nd = ", nd, "\n")
  rk <- (nc - nd)/(nc + nd)
  cat("[+]rk =", rk, "\n")
  t <- nc - nd
  cat("[+]T =",t, "\n")
  cat("\n")
  if (ties){
      cat("[!]There are ties. Should use assintotic aproximation.\n")
    }else{
      cat("[!]There are no ties.\n")
  }
  cat("\n")
  wp <- qnorm(p)*sqrt(n*(n-1)*(2*n+5)/18)
  cat("[+]Critical point: \n")
  cat("[+]W(",100*p,"%) =", wp, "\n")
}
