##### functions uncensored  data #########

sim_uncensored_data_sifi_original <- function(lambdaB, lambdaA ,n1, n2, ka,kb, direction, operation){
  
  #groupe experimental  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= kb) /30.5
  B$event = 1
  
  #groupe control
  
  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= ka)  /30.5
  A$event = 1
  
  
  data=bind_rows(A,B)
  
   sifi_res <- sifi(sv_data = data[,c(2,3,1)],direction =direction, operation =operation, agnostic = T)

   
  return(sifi_res)
  
}

sim_uncensored_data_sifi_remove <- function(lambdaB, lambdaA ,n1, n2, ka,kb, direction){
  
  #groupe experimental  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= kb) /30.5
  B$event = 1
  
  #groupe control
  
  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= ka)  /30.5
  A$event = 1
  
  
  data=bind_rows(A,B)
  

  sifi_res <-  sifi_remove(sv_data = data[,c(2,3,1)],direction =direction, agnostic = T)
  
  return(sifi_res)
  
}

sim_uncensored_data_sifi_random<- function(lambdaB, lambdaA ,n1, n2, ka,kb, operation){
  
  #groupe experimental  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= kb) /30.5
  B$event = 1
  
  #groupe control
  
  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= ka)  /30.5
  A$event = 1
  
  
  data=bind_rows(A,B)
  
  
  sifi_res <- sifi_random(sv_data = data[,c(2,3,1)], operation = operation, agnostic = T)
  
  return(sifi_res)
  
}

sim_uncensored_data_sifi_random_remove<- function(lambdaB, lambdaA ,n1, n2, ka,kb){
  
  #groupe experimental  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= kb) /30.5
  B$event = 1
  
  #groupe control
  
  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= ka)  /30.5
  A$event = 1
  
  
  data=bind_rows(A,B)
  
  
  sifi_res <- sifi_random_remove(sv_data =  data[,c(2,3,1)], agnostic = T)
  
  return(sifi_res)
  
}


##### truncature censored data ######

## find the tracking end date of control the desired censoring percentage 

# lambdaA = 370 
# lambdaB = 210 
# 10 % censored : n = 147
#40 % censored : n = 220
#60% censored : n = 330
#80% censored : n= 660
# 
# set.seed(145)
# tolerance = 0.1
# #pourcentage_censure = c(0.1,0,4,0.6,0.8)#all percentage you want : here 10%, 40%, 60 and 80%
# pourcentage_censure <- 0.8 #CHANGE the value 
# #Inisalization of value
# truncation_time <- NULL #the date of control the desired censoring percentage
# simulated_censoring_percentage <- 1
# #n_simulations <- c(147,220,330,660) 
# n_simulations <- 660 #CHANGE the value 
# #Find truncation_time
# for (i in 1:n_simulations) {
#   #Times survival
#   survival_times <- rweibull(n_simulations, scale = (lambdaA + lambdaB)/2, shape = 1)
# 
#   #determine the end date of monitoring based on the quantile
#   truncation_time <- quantile(survival_times, 1 -pourcentage_censure)
# 
#   #Calculate the censoring percentage
#   simulated_censoring_percentage <- sum(survival_times >= truncation_time) / length(survival_times)
# 
#   #Verified if the censoring percentage is the desired one
#   if (abs(simulated_censoring_percentage -pourcentage_censure) < tolerance) {
#     break
#   }
# }
# 
# cat("End date of follow up to obtain ", pourcentage_censure * 100,
#     "% uniformly distributed censors :", truncation_time, "month\n")

#438 ; 178 ; 94 ; 41 under H0
#606 ; 246 ; 129 ; 57 under H1 

###### functions censored data ######

sim_censored_data_sifi_original <- function(lambdaB, lambdaA ,n1, n2, ka,kb, truncation_time, direction,operation){
  
  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= kb) 
  B$censure <-runif(n1, min = 0, max = truncation_time)     
  B$event <- ifelse(B$time >= B$censure, 1, 0)
  B$event <- ifelse(B$time>=truncation_time,0,1)
  B$time <- pmin(B$time, B$censure)
  

  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= ka) 
  A$censure <-runif(n2, min = 0, max = truncation_time)      
  A$event <- ifelse(A$time >= A$censure, 1, 0)
  A$event <- ifelse(A$time>=truncation_time,0,1)
  A$time <- pmin(A$time, A$censure)
  
  data=bind_rows(A,B)
  
  sifi_res <- sifi(sv_data = data[,c(2,4,1)],direction =direction, operation = operation, agnostic = T)
  
  #nb_censure <- sum(data$event==0)
return(sifi_res)
  

}

sim_censored_data_sifi_remove <- function(lambdaB, lambdaA ,n1, n2, ka,kb, truncation_time, direction){
  
  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= kb) 
  B$censure <-runif(n1, min = 0, max = truncation_time)    
  B$event <- ifelse(B$time >= B$censure, 1, 0)
  B$event <- ifelse(B$time>=truncation_time,0,1)
  B$time <- pmin(B$time, B$censure)
  
  
  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= ka)
  A$censure <-runif(n2, min = 0, max = truncation_time)     
  A$event <- ifelse(A$time >= A$censure, 1, 0)
  A$event <- ifelse(A$time>=truncation_time,0,1)
  A$time <- pmin(A$time, A$censure)
  
  data=bind_rows(A,B)
  

  sifi_res <-  sifi_remove(sv_data = data[,c(2,4,1)],direction =direction, agnostic = T)
  
  
 
  # nb_censure <- sum(data$event==0)
  # return(list(sifi_res, nb_censure))
  return(sifi_res)
  
}

sim_censored_data_sifi_random <- function(lambdaB, lambdaA ,n1, n2, ka,kb, truncation_time,operation){
  
  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= kb)  
  B$censure <-runif(n1, min = 0, max = truncation_time)     
  B$event <- ifelse(B$time >= B$censure, 1, 0)
  B$event <- ifelse(B$time>=truncation_time,0,1)
  B$time <- pmin(B$time, B$censure)
  
  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= ka) 
  A$censure <-runif(n2, min = 0, max = truncation_time)     
  A$event <- ifelse(A$time >= A$censure, 1, 0)
  A$event <- ifelse(A$time>=truncation_time,0,1)
  A$time <- pmin(A$time, A$censure)
  
  data=bind_rows(A,B)
  

  sifi_res <- sifi_random(sv_data = data[,c(2,4,1)], operation = operation, agnostic = T)

  # nb_censure <- sum(data$event==0) #count the percentage of censored
  # return(list(sifi_res, nb_censure))
  
  return(sifi_res)

  
}

sim_censored_data_sifi_random_remove <- function(lambdaB, lambdaA ,n1, n2, ka,kb, truncation_time){
  
  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= kb)  
  B$censure <-runif(n1, min = 0, max = truncation_time)    
  B$event <- ifelse(B$time >= B$censure, 1, 0)
  B$event <- ifelse(B$time>=truncation_time,0,1)
  B$time <- pmin(B$time, B$censure)
  

  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= ka) 
  A$censure <-runif(n2, min = 0, max = truncation_time)    
  A$event <- ifelse(A$time >= A$censure, 1, 0)
  A$event <- ifelse(A$time>=truncation_time,0,1)
  A$time <- pmin(A$time, A$censure)
  
  data=bind_rows(A,B)
  
 
  sifi_res <- sifi_random_remove(sv_data =  data[,c(2,4,1)], agnostic = T)
  
  #   nb_censure <- sum(data$event==0)
  # return(list(sifi_res, nb_censure))
  # 
  
  return(sifi_res)

}




