library(tidyverse)

kruskal_wallis <- function(df){
  df_t <- as.list(df)
  k <- nlevels(factor(df_t[[2]]))
  df_n <- df %>% 
    mutate(Posts=rank(df_t[[1]], ties.method='average')) %>%
    arrange(df_t[[2]])
  
  print(df_n)
  
  df_s <- df_n %>% 
    group_by(df_n[[2]]) %>%
    summarise(n=n(), S=sum(Posts), `S^2/n`=S^2/n)
  colnames(df_s)[1] <- "Group"
  
  Sk <- sum(df_s[,4])
  N <- sum(df_s$n)
  Sr <- sum(df_n$Posts^2)
  C <- N*((N+1)^2)/4
  
  t <- (N-1)*(Sk - C)/(Sr - C)
  
  cat("------------------------\n")
  print(df_s)
  
  if (any(duplicated(df_t[[1]]))){
    cat("------------------------\n")
    cat("[!]Ties present.\n")
  }
  
  cat("------------------------\n")
  cat("[+]k = ", k, "\n")
  cat("[+]N = ", N, "\n")
  cat("[+]Sk = ", Sk, "\n")
  cat("[+]Sr = ", Sr, "\n")
  cat("[+]C = ", C, "\n")
  cat("\n")
  cat("[*]Test statistics\n")
  cat("[*]T = ", t)
  invisible(t)
}


