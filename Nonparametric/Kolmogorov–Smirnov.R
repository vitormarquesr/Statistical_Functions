library(tidyverse)

kolmogorov <- function(g1, pF, ...){
  df <- tibble(x=sort(unique(g1)),
               n = length(g1))
  f <- function(el, g1){
    sum(g1 <= el)/length(g1)
  }
  df <- df %>% 
    mutate(Fx = pF(x, ...),
           Sx = map_dbl(x, f, g1),
           dFS1 = Fx - Sx,
           dFS2 = Fx - dplyr::lag(Sx, n = 1,
                                  default = 0))
  
  print(str_c('T = ',max(abs(c(df$dFS1, df$dFS2)))))
  return(df)
}

smirnov <- function(g1, g2){
  tot <- sort(unique(c(g1, g2)))
  
  g1t <- sort(unique(g1))
  ps1 <- rep(NA, length(tot))
  ps1[match(g1t, tot)] <- g1t
  
  g2t <- sort(unique(g2))
  ps2 <- rep(NA, length(tot))
  ps2[match(g2t, tot)] <- g2t
  
  df <- tibble(Obs = tot,
               G1 = ps1,
               n1 = length(g1),
               G2 = ps2,
               n2 = length(g2))
  f <- function(el, g1){
    sum(g1 <= el)/length(g1)
  }
  
  df <- df %>% 
      mutate(S1 = map_dbl(tot,
                          f, g1),
             S2 = map_dbl(tot,
                          f, g2),
             dS = S1 - S2)
  print(str_c('T = ',max(abs(df$dS))))
  print(str_c('T+ = ',max(df$dS)))
  print(str_c('T- = ',max(-df$dS)))
  return(df)
}
