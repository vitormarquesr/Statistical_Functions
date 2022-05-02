library(tidyverse)

wilcox_sum_posts_paired <- function(s1, s2, mu=0){
  s <- (s1 - s2)
  
  d <- s-mu
  sel <- d == 0
  remov <- sum(sel)
  
  d <- d[!(sel)]
  s <- s[!(sel)]
  
  s1_r <- s1[sel]
  s2_r <- s2[sel]
  
  s1 <- s1[!(sel)]
  s2 <- s2[!(sel)]
  
  df <- tibble(S1=s1[order(abs(d))],
               S2=s2[order(abs(d))],
              Samp = s[order(abs(d))],
               D = d[order(abs(d))],
               Posts=rank(abs(D)),
               Signal=sign(D),
               Signed_Posts=Signal*Posts)
  Pp <- df$Posts[df$Signal==1]
  Np <- df$Posts[df$Signal==-1]
  
  dup_p <- any(duplicated(Pp))
  dup_n <- any(duplicated(Np))
  
  dup_int <- any((Pp %in% Np))
  
  Sp <- sum(Pp)
  Sn <- sum(Np)
  
  print(df)
  
  emp <- (dup_p || dup_n || dup_int)
  
  if (remov){
    print("----------------------------------")
    print(str_c("[!]Removed ",remov, 
                " exact zeros"))
    print(str_c("[!]S1: ", s1_r))
    print(str_c("[!]S2: ", s2_r))
  }
  print("----------------------------------")
  
  print(str_c("[*] N = ", length(s)))
  print(str_c("[*] S+ = ",Sp, " ", "| Ties within +: ", dup_p))
  print(str_c("[*] S- = ", Sn, " ", "| Ties within -: ", dup_n))
  print(str_c("[*] Ties between + and -: ", dup_int))
  print(str_c("[!] Ties: ", emp))
  
  print("----------------------------------")
  
  if (emp){
    print('[!]Since there are ties. It is recommended to use the assintotic test')
    E <- sum(df$Posts)/2
    V <- sum(df$Posts^2)/4
    print(str_c('[*]E(S) = ', E))
    print(str_c('[*]Var(S) = ', V))
  }
  invisible(df)
}

walsh_means_paired <- function(s1, s2, leav=0){
  s <- (s1 - s2)
  
  n <- length(s)
  s <- sort(s)
  M <- matrix(0, nrow=n, ncol=n)
  rownames(M) <- s
  colnames(M) <- s
  
  val <- vector(mode="double", length=n*(n+1)/2)
  cont <- 1
  for (j in 1:n){
    for (i in 1:n){
      if (i > j){
        next
      }
      med <- (s[i]+s[j])/2
      M[i,j] <- med
      val[cont] <- med
      cont <- cont + 1
    }
  }
  val <- sort(val)
  print("[*]Walsh-Means")
  print(M)
  
  print("----------------------------------")
  
  print("[*]Walsh-Means ordered in ascending order")
  print(val)
  
  print("----------------------------------")
  
  print(str_c('[*]Hodges-Lehman median estimator = ', 
              median(val)))
  if (leav > 0){
    print("----------------------------------")
    print(str_c('[*]IC: ', '[', val[leav],';', val[(length(val)-leav+1)],']'))
  }
  
  invisible(M)
}



