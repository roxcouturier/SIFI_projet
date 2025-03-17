##### under H0 #####

getwd()


directory_path <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table 2/H0"
file_names <- list.files(directory_path, pattern = "\\.csv$")


for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}

results <- list()

#Loop to process each file
for (file in file_names) {
  # Lire le fichier
  data <- read.csv(file)
  
  #Convert transpose
  data <- t(data)
  data <- as.data.frame(data)
  

  #check type 1 error (5%)
  
  nombre_test_sig <- round((sum(data$V1 >= 0, na.rm = TRUE)) / 100, 1)
  
  
  #Keep just SIFI for  non significant test 
  data <- data$V1[data$V1<0]
  
  n <- rep(132,length(data))
  
  prop <- round(abs(sum(data,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(data, na.rm=T) 
  q1 <- quantile(data, na.rm=T, 0.25) 
  q3 <- quantile(data, na.rm=T, 0.75) 
  mean <- round(mean(data, na.rm=T),4)
  sd <- round(sd(data,na.rm=T),4)
  
  
  
  #Store results in list
  results[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd)
}



##### under H1 #####

directory_path <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table 2/H1"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}


#setwd("~./Tables/Table 2/H1")

# Créer une liste pour stocker les résultats de chaque fichier
results_H1 <- list()

#Loop to process each file
for (file in file_names) {
  # Lire le fichier
  data <- read.csv(file)
  
  #Convert transpose
  data <- t(data)
  data <- as.data.frame(data)
  
  
  #check type 1 error (5%)
  
  nombre_test_sig <- round((sum(data$V1 >= 0, na.rm = TRUE)) / 100, 1)

  
  
  #Keep just SIFI for  significant test 
  data <- data$V1[data$V1>0]
  
  n <- rep(132,length(data))
  
  prop <- round(abs(sum(data,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(data, na.rm=T) 
  q1 <- quantile(data, na.rm=T, 0.25) 
  q3 <- quantile(data, na.rm=T, 0.75) 
  mean <- round(mean(data, na.rm=T),4) 
  sd <- round(sd(data,na.rm=T),4)
  
  
  
  #Store results in list
  results_H1[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd)
}



#### table 

Table <- data.frame(matrix(vector(), 24, 8, 
                            dimnames=list(c(),c("Methods","RHO", "Median","Q1","Q3", "Mean","SD", "Prop-SIFI"))),
                     stringsAsFactors = F)

Table$Methods <- c(" Best Clone","H0 1a  - Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete",
                   "Best Clone","H1 1a - Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete",
                   "Best Clone"," H1 1b - Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete",
                   "Best Clone","H1 1c - Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete")



##RHO 

 premiers_elements <- sapply(results, function(x) x[1])
 premiers_elements <- unlist(premiers_elements)
 
 premiers_elements_H1 <- sapply(results_H1, function(x) x[1])
 premiers_elements_H1 <- unlist(premiers_elements_H1)

##median 
 
 median_H0 <- sapply(results, function(x) x[3])
 median_H0 <- unlist(median_H0)
 
 median_H1 <- sapply(results_H1, function(x) x[3])
 median_H1 <- unlist(median_H1)
 
 ##Q1 
 
 q1_H0 <- sapply(results, function(x) x[4])
 q1_H0 <- unlist(q1_H0)
 
 q1_H1 <- sapply(results_H1, function(x) x[4])
 q1_H1 <- unlist(q1_H1)
 
 ##Q3 
 
 q3_H0 <- sapply(results, function(x) x[5])
 q3_H0 <- unlist(q3_H0)
 
 q3_H1 <- sapply(results_H1, function(x) x[5])
 q3_H1 <- unlist(q3_H1)
 
 ##mean 
 
 mean_H0 <- sapply(results, function(x) x[6])
 mean_H0 <- unlist(mean_H0)
 
 mean_H1 <- sapply(results_H1, function(x) x[6])
 mean_H1 <- unlist(mean_H1)
 
 ##szd 
 
sd_H0 <- sapply(results, function(x) x[7])
sd_H0 <- unlist(sd_H0)
 
sd_H1 <- sapply(results_H1, function(x) x[7])
sd_H1 <- unlist(sd_H1)


##prop

prop_H0 <- sapply(results, function(x) x[2])
prop_H0 <- unlist(prop_H0)

prop_H1 <- sapply(results_H1, function(x) x[2])
prop_H1 <- unlist(prop_H1)


Table$RHO <- c(premiers_elements ,premiers_elements_H1 )
Table$Median <- c(median_H0,median_H1)
Table$Q1 <- c(q1_H0,q1_H1)
Table$Q3 <- c(q3_H0,q3_H1)
Table$Mean <- c(mean_H0,mean_H1)
Table$SD <- c(sd_H0,sd_H1)
Table$Prop.SIFI <- c(prop_H0,prop_H1)

Table[22,] <- c("Worst Clone",100,48,40,56,47.4,10.523,0.3247)


#### cox hazard ratio : 

sim_uncensored_data_cox <- function(lambdaB, lambdaA ,n1, n2, ka,kb){
  
  #groupe experimental  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= kb) /30.5
  B$event = 1
  
  #groupe control
  
  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= ka)  /30.5
  A$event = 1
  
  
  data=bind_rows(A,B)
  
  cox <- coxph(Surv(time,event)~ relevel(factor(trt), ref = "B"),data=data)
  beta <- cox$coefficients

  
  return(beta)
  
}

# #sim_uncensored_data_cox_zph <- function(lambdaB, lambdaA ,n1, n2, ka,kb){
#   
#   #groupe experimental  
#   B <- data.frame(trt = rep("B", n1))
#   B$time <- rweibull(n1, scale = lambdaB , shape= kb) /30.5
#   B$event = 1
#   
#   #groupe control
#   
#   A <- data.frame(trt=rep("A",n2))
#   A$time <- rweibull(n2, scale = lambdaA , shape= ka)  /30.5
#   A$event = 1
#   
#   
#   data=bind_rows(A,B)
#   
#   cox <- coxph(Surv(time,event)~ relevel(factor(trt), ref = "B"),data=data)
#   zph <- cox.zph(cox)$table[6]
#   
#   return(zph)
#   
# }


sim_uncensored_data_cox_pval <- function(lambdaB, lambdaA ,n1, n2, ka,kb){
  
  #groupe experimental  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= kb) /30.5
  B$event = 1
  
  #groupe control
  
  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= ka)  /30.5
  A$event = 1
  
  
  data=bind_rows(A,B)
  
  cox <- coxph(Surv(time,event)~ relevel(factor(trt), ref = "B"),data=data)
  p_value <- summary(cox)$coefficients[5]  
  
  
  return(p_value)
  
}

#beta
set.seed(145) 
sim_cox_H0_1a_uncensored<- replicate(10000,sim_uncensored_data_cox(210,210 ,66, 66, 1,1))
HR_H0a <- exp(mean(sim_cox_H0_1a_uncensored)) #1.00

#power  
set.seed(145)  
sim_pvals <- replicate(10000, sim_uncensored_data_cox_pval(210, 210, 66, 66, 1, 1))
power <- mean(sim_pvals > 0.05)  
print(power)  #0.9486


set.seed(145)
sim_cox_H1_1a_uncensored<- replicate(10000,sim_uncensored_data_cox(210,370 ,66, 66, 1,1))
HR_H1a <-exp(mean(sim_cox_H1_1a_uncensored,na.rm=T)) #0.56
#power  
set.seed(145)  
sim_pvals <- replicate(10000, sim_uncensored_data_cox_pval(210,370 ,66, 66, 1,1))
power <- mean(sim_pvals < 0.05)  
print(power)   ##0.8877

set.seed(145)
sim_cox_H1_1b_uncensored<- replicate(10000,sim_uncensored_data_cox(210,370 ,66, 66, 0.7,0.7))
HR_H1b <-exp(mean(sim_cox_H1_1b_uncensored)) #0.67
#power  
set.seed(145)  
sim_pvals <- replicate(10000, sim_uncensored_data_cox_pval(210,370 ,66, 66, 0.7,0.7))
power <- mean(sim_pvals < 0.05)  
print(power)   #0.6054


set.seed(145)
sim_cox_H1_1c_uncensored<- replicate(10000,sim_uncensored_data_cox(210,370 ,66, 66, 2,2))
HR_H1c <- exp(mean(sim_cox_H1_1c_uncensored)) #0.32
#power  
set.seed(145)  
sim_pvals <- replicate(10000, sim_uncensored_data_cox_pval(210,370 ,66, 66, 2,2))
power <- mean(sim_pvals < 0.05)  
print(power)  #1


# Réorganiser le dataframe pour obtenir l'ordre souhaité
Table <- Table[c(
  2, 1, 3,  
  5, 4, 6, 
  8,7,9,
  11, 10, 12,
  14, 13, 15,
  17, 16, 18,
  20, 19, 21, 
  23, 22, 24  
), ]


Table$HR <- ""
Table$HR[c(1, 7, 13, 19)] <- c(HR_H0a, HR_H1a, HR_H1b, HR_H1c)


write.csv(Table, file = "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table 2/Table2.csv")

