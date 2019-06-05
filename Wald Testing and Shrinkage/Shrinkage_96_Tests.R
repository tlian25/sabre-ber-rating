#---------------------------------------------------------------
# Wald Statistic/P-Value Function

wald.func <- function(B,V,R,df){
  wald.stat <- t(R%*%B)%*%inv(R%*%V%*%t(R))%*%(R%*%B)
  wald.pvalue <- pchisq(wald.stat,df,lower.tail = FALSE)
  w <- cbind(wald.stat,wald.pvalue)
  return(w)
}



#---------------------------------------------------------------
# Wald Null Hypothesis Test (All coefficients = 0 in independent tests)
# Hard-coded to cluster on data table dt for variable Half_Inning

null.wald.test <- function(reg){
  
  n <- as.numeric(length(reg$coefficients))
  w <- matrix(0,n,2,byrow = T)
  colnames(w) <- c("Wald.Stat","Wald.p.Value")
  
  B <- reg$coefficients
  V <- cluster.vcov(reg,cluster=dt$Half_Inning)
  
  df<- 0
  for(i in 1:n){
    R <- matrix(0,1,n,byrow = T)
    R[1,i] <- 1
    w[i,] <- wald.func(B,V,R,df)
  }
  
  return(w)
}




#---------------------------------------------------------------
# Shrinkage Estimation Function

shrinkage <- function(reg) {
  reg_wald <- null.wald.test(reg)
  
  #Number of coefficients
  k <- as.numeric(length(reg$coefficients))
  
  ##Create Variance and Inverse Variance vector
  vv <- matrix(0,k,1,byrow = T)
  #ivv <- matrix(0,k,1,byrow = T)
  for (i in 1:k) {
    vv[i,1] <- (vcov(reg)[i,i])  
  #  ivv[i,1] <- 1/(vcov(reg)[i,i])
  }
  
  #Calculate average inverse variances
  #sum_var_inv <- sum(ivv)
  #avg_var_inv <- sum(ivv)*(1/(k-1))
  
  #Create Delta Matrix
  delta <- matrix(0,k,1,byrow = T)
  for (i in 1:k) {
    for (j in 1:k) {  
      delta[i,1] <- delta[i,1] + (vv[i,1]/vv[j,1])
    }
    delta[i,1] <- delta[i,1] - k
  }
  
  
  #Create Alpha Matrix
  alpha <- matrix(0,k,1,byrow = T)
  for (i in 1:k) {
    alpha[i,1] <- (reg_wald[i,1]-(k-1))/(reg_wald[i,1]+delta[i,1])
  }
  
  #Create output Matrix
  coefs <- data.frame(reg$coefficients)
  shrunk <- cbind((coefs*alpha),alpha,coefs)
  colnames(shrunk) <- c("new","alpha","old")
  
  return(shrunk)
}