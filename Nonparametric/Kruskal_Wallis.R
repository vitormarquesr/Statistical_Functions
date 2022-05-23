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

krusk_posthoc_multiple_comparison <- function(df, conf=0.05){
  df_t <- as.list(df)
  k <- nlevels(factor(df_t[[2]]))
  
  df_n <- df %>% 
    mutate(Posts=rank(df_t[[1]], ties.method='average')) %>%
    arrange(df_t[[2]])
  
  df_s <- df_n %>% 
    group_by(df_n[[2]]) %>%
    summarise(n=n(), S=sum(Posts), `S^2/n`=S^2/n)
  colnames(df_s)[1] <- "Group"
  
  Sk <- sum(df_s[,4])
  N <- sum(df_s$n)
  Sr <- sum(df_n$Posts^2)
  C <- N*((N+1)^2)/4
  
  t <- (N-1)*(Sk - C)/(Sr - C)
  
  S2 <- (sum(df_n$Posts^2) - N*(N+1)^2/4)/(N-1)
  
  aux <- qt(conf/2,df=N-k,lower.tail=FALSE)*sqrt(S2*(N-1-t)/(N-k))
  
  for (i in 1:(k-1)){
    for (j in (i+1):k){
      cat("-----------------------------\n")
      cat(df_s[[1]][i], "and", df_s[[1]][j], "\n")
      lstq <- aux*sqrt(1/df_s$n[i]+1/df_s$n[j])
      estt <- abs(df_s$S[i]/df_s$n[i] - df_s$S[j]/df_s$n[j])
      if (estt > lstq){
        cat("[!]", estt, ">", lstq, "\n")
        cat("[!]Evidence of difference between the two populations\n")
      }else{
        cat("[+]", estt, "<=", lstq, "\n")
      }
    }
  }
}

