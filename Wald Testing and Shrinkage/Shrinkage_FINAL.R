#---------------------------------------------------------------
# Wald Statistic/P-Value Function

wald.func <- function(B,V,R,df){
  wald.stat <- t(R%*%B)%*%solve(R%*%V%*%t(R))%*%(R%*%B)
  wald.pvalue <- pchisq(wald.stat,df,lower.tail = FALSE)
  w <- cbind(wald.stat,wald.pvalue)
  return(w)
}



#---------------------------------------------------------------
# Wald Null State Hypothesis Test (Each state equals each other state)
# Treats the 4 classes (position_ before main effect and the 3
# interactions of FIP, park_factor, league) as separate
# Hard-coded to cluster on data table dt for variable Half_Inning

null.wald.test <- function(reg, covar){
  
  n <- as.numeric(length(reg$coefficients))
  m <- ((n-4)/4)
  w <- matrix(0,1,2,byrow = T)
  colnames(w) <- c("Wald.Stat","Wald.p.Value")
  
  B <- reg$coefficients
  #V <- cluster.vcov(reg,cluster=dt$Half_Inning)
  V <- covar
  R <- matrix(0,(n-4),n,byrow = T)
  df <- (n-4)
  
  for(i in 1:4){
    for(j in 1:m){
      R[j+(23*(i-1)),j+(24*(i-1))] <- 1
      R[j+(23*(i-1)),j+(24*(i-1))+1] <- -1
    }
  }
  
  w[1,] <- wald.func(B,V,R,df)
  
  return(w)
}




#---------------------------------------------------------------
# Shrinkage Estimation Function

shrinkage <- function(reg, covar) {
  reg_wald <- null.wald.test(reg, covar)
  
  #Number of coefficients
  k <- as.numeric(length(reg$coefficients))
  
  ##Create Variance vector
  vv <- matrix(0,k,1,byrow = T)
  for (i in 1:k) {
    vv[i,1] <- (covar[i,i])  
  }
  
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
    alpha[i,1] <- (reg_wald[1,1]-(k-1))/(reg_wald[1,1]+delta[i,1])
  }
  
  #Create output Matrix
  coefs <- data.frame(reg$coefficients)
  shrunk <- cbind((coefs*alpha),alpha,coefs)
  colnames(shrunk) <- c("new","alpha","old")
  
  vl.shrink = reg$coefficients * alpha[,1]
  names(vl.shrink) = names(reg$coefficients) %>%
    sapply(function(s) gsub("position_before", "", s)) %>%
    sapply(function(s) gsub(":league", "", s)) %>%
    sapply(function(s) if_else(grepl("*]$", s), paste0(s, "AL"), s))
  
  # Add base states to NL adjustments for total NL effect
  vl.shrink[25:48] = vl.shrink[1:24] + vl.shrink[25:48]
  # Add values of 0 for end inning states
  vl.shrink = c(vl.shrink, "end inningAL" = 0, "end inningNL" = 0, "end inning" = 0, 
                "end inning:park_factor" = 0, "end inning:FIP" = 0)
  
  return(list("wald" = reg_wald, 
              "coef.shrink" = shrunk, 
              "vl.shrink" = vl.shrink))
  
  
}