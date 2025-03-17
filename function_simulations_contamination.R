
#### functions contamination patient #####

worst_contamination_H0 <- function(lambdaB, lambdaA ,n1, n2, ka,kb){
  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= kb) 
  B$event <- 1
  
  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= ka) 
  A$event <- 1
  
  data=bind_rows(A,B)
  
  
  
  p <- seq(0,0.95,0.01) 
  #we vary the proportion of patients who will be bad contaminants until the conclusions change
  pval <- c()
  

  q1 <- qweibull(0.25, 1, lambdaB)
  
  data$new_time <- runif(n1+n2,0,q1) 
  #the new patient time is drawn between 0 and the first quartile of basic event times = patients who die more quickly
  

  
  for(i in 1:length(p)) { 
    
    data$bin <- 0
    data$bin[data$trt=="B"] <- rbinom(n2,1,p[i]) 
    
    
    data$time[data$bin ==1] <-data$new_time[data$bin==1] 
    
    
    sdf <- survdiff(Surv(time, event) ~ trt=="A", data)
    pval[i]<- 1 - pchisq(sdf$chisq, length(sdf$n) - 1)
    


    if (pval[i] < 0.05) { 
      break  #Exit the loop if the p-value is greater than 0.05
    }
  }
  
  pval_below_0.05 <- p[i] 
  
  return(pval_below_0.05)
  
}

worst_contamination_H1 <- function(lambdaB, lambdaA ,n1, n2, ka,kb){
  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= kb) 
  B$event <- 1
  
  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= ka) 
  A$event <- 1
  
  data=bind_rows(A,B)
  
  
  
  p <- seq(0,0.95,0.01) 
  #we vary the proportion of patients who will be bad contaminants until the conclusions change
  pval <- c()
  
  
  q1 <- qweibull(0.25, 1, lambdaA)
  
  data$new_time <- runif(n1+n2,0,q1) 
  #the new patient time is drawn between 0 and the first quartile of basic event times = patients who die more quickly
  

  
  for(i in 1:length(p)) { 
    
    data$bin <- 0
    data$bin[data$trt=="A"] <- rbinom(n2,1,p[i]) 
    
    
    data$time[data$bin ==1] <-data$new_time[data$bin==1] 
    
    
    sdf <- survdiff(Surv(time, event) ~ trt=="A", data)
    pval[i]<- 1 - pchisq(sdf$chisq, length(sdf$n) - 1)
    
    
    if (pval[i] > 0.05) { 
      break  #Exit the loop if the p-value is less than 0.05
    }
    
   
  }
  
  pval_below_0.05 <- p[i] 
  
  return(pval_below_0.05)
  
}

best_contamination_H0 <- function(lambdaB, lambdaA ,n1, n2, ka,kb){
  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= kb) 
  B$event <- 1
  
  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= ka) 
  A$event <- 1
  
  data=bind_rows(A,B)
  
  
  
  p <- seq(0,0.95,0.01) 
  #we vary the proportion of patients who will be bad contaminants until the conclusions change
  pval <- c()
  
  
  q3 <- qweibull(0.75, 1, lambdaA)
  
  data$new_time <- runif(n1+n2,q3, max(data$time))
  
  #the new patient time is drawn between Q3 and the last quartile of basic event times
  
  
  
  for(i in 1:length(p)) { 
    
    data$bin <- 0
    data$bin[data$trt=="A"] <- rbinom(n2,1,p[i]) 
    
    
    data$time[data$bin ==1] <-data$new_time[data$bin==1] 
    
    
    sdf <- survdiff(Surv(time, event) ~ trt=="A", data)
    pval[i]<- 1 - pchisq(sdf$chisq, length(sdf$n) - 1)
    
    
    if (pval[i] < 0.05) { 
      break  #Exit the loop if the p-value is less than 0.05
    }
    
    
 
  }
  
  pval_below_0.05 <- p[i] 
  
  return(pval_below_0.05)
  
}

best_contamination_H1 <- function(lambdaB, lambdaA ,n1, n2, ka,kb){
  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= kb) 
  B$event <- 1
  
  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= ka) 
  A$event <- 1
  
  data=bind_rows(A,B)
  
  
  
  p <- seq(0,0.95,0.01) 
  #we vary the proportion of patients who will be bad contaminants until the conclusions change
  pval <- c()
  
  
  q3 <- qweibull(0.75, 1, lambdaB)
  
  data$new_time <- runif(n1+n2,q3, max(data$time))
  
  #the new patient time is drawn between Q3 and the last quartile of basic event times
  
  
  
  for(i in 1:length(p)) { 
    
    data$bin <- 0
    data$bin[data$trt=="B"] <- rbinom(n2,1,p[i]) 
    
    
    data$time[data$bin ==1] <-data$new_time[data$bin==1] 
    
    
    sdf <- survdiff(Surv(time, event) ~ trt=="A", data)
    pval[i]<- 1 - pchisq(sdf$chisq, length(sdf$n) - 1)
    
    
    if (pval[i] > 0.05) { 
      break  #Exit the loop if the p-value is less than 0.05
    }
    
 
  }
  
  pval_below_0.05 <- p[i] 
  
  return(pval_below_0.05)
  
}




