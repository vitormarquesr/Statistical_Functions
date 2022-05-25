library(tidyverse)

friedman <- function(df){
  
  cat("Friedman's test for more than two dependent samples\n")
  cat("---------------------------------\n")
  
  colnames(df) <- c("Block", "Treatment", "Values")
  
  b <- nlevels(factor(df$Block))
  k <- nlevels(factor(df$Treatment))
  
  df_n <- df %>% group_by(Block) %>%
    mutate(Posts=rank(Values, ties.method='average')) %>%
    ungroup()
  
  ord <- vector("integer", length=2*k)
  ord1 <- 2:(k+1)
  ord2 <- (k+2):(2*k+1)

  v <- c()
  for (i in 1:k){
    v <- append(v, c(ord1[i], ord2[i]))
  }
  
  df_n %>% 
    pivot_wider(names_from = 'Treatment', values_from=c('Values', 'Posts')) %>%
    select(1, v) %>%
    print()
  
  cat("---------------------------------\n")
  
  
  df_s <- df_n %>% group_by(Treatment) %>%
    summarise(S=sum(Posts), S2=S^2)
  
  print(df_s)
  
  cat("---------------------------------\n")
  C <- (b*k*(k+1)^2)/4
  Sr <- sum(df_n$Posts^2)
  St <- sum(df_s$S2)/b
  
  cat("[+]b = ", b, "\n")
  cat("[+]k = ", k, "\n")
  
  cat("[+]C = ", C, "\n")
  cat("[+]Sr = ", Sr, "\n")
  cat("[+]St = ", St, "\n")
  
  t <- b*(k-1)*(St-C)/(Sr - C)
  cat("[+]Test statistics T = ", t, "\n")

}

friedman_posthoc_multiple_comparison <- function(df, conf=0.05){
  colnames(df) <- c("Block", "Treatment", "Values")
  
  b <- nlevels(factor(df$Block))
  k <- nlevels(factor(df$Treatment))
  
  df_n <- df %>% group_by(Block) %>%
    mutate(Posts=rank(Values, ties.method='average')) %>%
    ungroup()
  
  df_s <- df_n %>% group_by(Treatment) %>%
    summarise(S=sum(Posts), S2=S^2)
  
  
  Sr <- sum(df_n$Posts^2)

  Saux <- sum(df_s$S2)

  lstq <- qt(conf/2, df=(b-1)*(k-1), lower.tail=FALSE)*sqrt(2*(b*Sr-Saux)/((b-1)*(k-1))) 
  
  for (i in 1:(k-1)){
    for (j in (i+1):k){
      cat("-----------------------------\n")
      cat(df_s[[1]][i], "and", df_s[[1]][j], "\n")
      estt <- abs(df_s$S[i] - df_s$S[j])
      if (estt > lstq){
        cat("[!]", estt, ">", lstq, "\n")
        cat("[!]Evidence of difference between the two populations\n")
      }else{
        cat("[+]", estt, "<=", lstq, "\n")
      }
    }
  }
}

