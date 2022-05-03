library(tidyverse)

mann_whitney <- function(s1, s2){
  t <- sort(unique(c(s1, s2)))
  posts <- unique(rank(sort(c(s1,s2)), ties.method = 'average'))
  
  
  s1_n <- vector('double', length=length(t))
  s1_n[] <- NA
  
  p1 <- vector('integer', length=length(t))
  p1[] <- NA
  
  s1_n[match(s1, t)] <- s1
  p1[!is.na(s1_n)] <- posts[!is.na(s1_n)]
  
  s2_n <- vector('double', length=length(t))
  s2_n[] <- NA
  
  p2 <- vector('integer', length=length(t))
  p2[] <- NA
  
  s2_n[match(s2, t)] <- s2
  p2[!is.na(s2_n)] <- posts[!is.na(s2_n)]

  df <- tibble(A1=s1_n,A2=s2_n, 
               f1=map_int(A1, function(x){sum(x==s1)}),
               f2=map_int(A2, function(x){sum(x==s2)}),
               P1=p1,
               P2=p2)
  print(df)
  print("-----------------------------")
  N1 = length(s1)
  N2 = length(s2)
  S1 = sum(df$P1*df$f1, na.rm=TRUE)
  S2 = sum(df$P2*df$f2, na.rm=TRUE)
  W1 = S1 - N1*(N1+1)/2
  W2 = S2 - N2*(N2+1)/2
  print(str_c("[*]N1 = ", N1))
  print(str_c("[*]S1 = ", S1))
  print(str_c("[*]W1 = ", W1))
  tie_1 = any(df$f1[!is.na(df$f1)]>1)
  print(str_c("[+]Ties within sample 1: ", tie_1))
  print('-----')
  print(str_c("[*]N2 = ", N2))
  print(str_c("[*]S2 = ", S2))
  print(str_c("[*]W2 = ", W2))
  tie_2 = any(df$f2[!is.na(df$f2)]>1)
  print(str_c("[+]Ties within sample 2: ", tie_2))
  print('-----')
  tie_a = any(!is.na(df$f1*df$f2))
  print(str_c("[+]Ties across samples: ", tie_a))
  
  tie_t = any(tie_1 || tie_2 || tie_a)
  print(str_c("[!]Ties: ", tie_t))
  
  if(tie_t){
    print("---------------------------------")
    print("[!]Since there are ties it is recommended to use the assintotic distribution")
  }
  
  print("---------------------------------")
  print("Assintotic Parameters")
  
  EW = N1*N2/2
  VarW = N1*N2*(N1+N2+1)/12
  
  print(str_c("[+]E(W) = ", EW))
  print(str_c("[+]Var(W) = ", VarW))
  
  invisible(df)
}

walsh_means_independent <- function(ge, gc, leav=0){
  n1 = length(ge)
  n2 = length(gc)
  M <- matrix(0, nrow=n1, ncol=n2)
  ge <- sort(ge)
  gc <- sort(gc)
  val <- vector("double", length = n1*n2)
  rownames(M) <- ge
  colnames(M) <- gc
  con <- 1
  
  for (i in 1:n1){
    for (j in 1:n2){
      de = ge[i] - gc[j]
      M[i,j] <- de
      val[con] <- de
      con <- con + 1
    }
  }
  val <- sort(val)
  print("[*]Walsh-Means")
  print(M)
  print("-------------------------")
  print("[*]Walsh-Means ordered in ascending order")
  
  print(val)
  
  print("--------------------------")
  pla <- (n1*n2)/2
  if (near(pla, as.integer(pla))){
    psmed <- val[pla]
  }else{
    psmed <- (val[as.integer(pla)] + val[as.integer(pla)+1])/2
  }
  print(str_c("[*]N1 = ", n1))
  print(str_c("[*]N2 = ", n2))
  print(str_c("[*]Hodges-Lehman median estimator = ", psmed))
  if (leav > 0){
    print("----------------------------------")
    print(str_c('[*]IC: ', '[', val[leav],';', val[(length(val)-leav+1)],']'))
  }
  invisible(M)
}

