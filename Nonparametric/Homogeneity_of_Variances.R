library(tidyverse)

homogeneity_of_variances <- function(df){
  colnames(df) <- c("Values", "Group")
  k <- nlevels(factor(df$Group))
  
  
  df_n <- df %>% group_by(Group) %>%
    mutate(Mean=mean(Values),Abs_Deviation=abs(Values-Mean)) %>%
    ungroup() %>% 
    mutate(Posts=rank(Abs_Deviation, ties.method='average')) %>%
    arrange(Group)
  
  print(df_n)
  
  df_s <- df_n %>% 
    group_by(Group) %>%
    summarise(n=n(), S=sum(Posts^2), `S^2/n`=S^2/n)
  
  Sk <- sum(df_s[[4]])
  N <- sum(df_s$n)
  Sr <- sum(df_n$Posts^4)
  C <- ((sum(df_n$Posts^2))^2)/N
  
  t <- (N-1)*(Sk - C)/(Sr - C)
  
  cat("------------------------\n")
  print(df_s)
  
  if (any(duplicated(df$Values))){
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

multiple_comparisons_homogeneity_of_variances <- function(df, conf=0.05){
  colnames(df) <- c("Values", "Group")
  k <- nlevels(factor(df$Group))
  df_n <- df %>% group_by(Group) %>%
    mutate(Mean=mean(Values),Abs_Deviation=abs(Values-Mean)) %>%
    ungroup() %>% 
    mutate(Posts=rank(Abs_Deviation, ties.method='average')) %>%
    arrange(Group)
  
  df_s <- df_n %>% 
    group_by(Group) %>%
    summarise(n=n(), S=sum(Posts^2), `S^2/n`=S^2/n)
  
  Sk <- sum(df_s[[4]])
  N <- sum(df_s$n)
  Sr <- sum(df_n$Posts^4)
  C <- ((sum(df_n$Posts^2))^2)/N
  
  t <- (N-1)*(Sk - C)/(Sr - C)
  
  
  
  Sb <- sum(df_n$Posts^2)/N
  D2 <- (sum(df_n$Posts^4)-N*Sb^2)/(N-1)
  
  cat("[+]N = ", N, "\n")
  cat("[+]k = ", k, "\n")
  cat("[+]T = ", t, "\n")
  cat("[+]Sb = ",Sb, "\n")
  cat("[+]D2 = ", D2, "\n")
  
  cat("----------------------------\n")
  
  aux <- qt(conf/2,df=N-k,lower.tail=FALSE)*sqrt(D2*(N-1-t)/(N-k))
  
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


