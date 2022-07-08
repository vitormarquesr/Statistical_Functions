permutation.test <- function(g1,g2,func=mean,k=1000, 
                         alternative='two.sided'){
  est <- func(g1) - func(g2)
  tod <- c(g1,g2)
  per <- vector(mode='double',length=k)
  for (i in 1:k){
    ream <- sample(tod, size=length(tod), replace=FALSE)
    g1t <- ream[1:length(g1)]
    g2t <- ream[(length(g1)+1):length(tod)]
    per[i] <- func(g1t) - func(g2t)
  }
  
  hist(per)
  abline(v=est)
  
  cat("[+]Test statistics: ", est, "\n")
  if(alternative == 'greater'){
    # Ha: func(g1) > func(g2)
    pval <- mean(per >= est)
  }else if(alternative == 'less'){
    # Ha: func(g1) < func(g2)
    pval <- mean(per <= est)
  }else{
    pval <- 2*mean(per >= abs(est))
  }
  cat("[+]p-value: ", pval, "\n")
}
