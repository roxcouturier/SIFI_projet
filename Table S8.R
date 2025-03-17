
##### scenario 2a #####


directory_path <- "./Tables/Table S6/2a"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}

results_2a <- list()


#setwd("~./Tables/Table S6/2a")
#Loop to process each file
for (file in file_names) {
  # Lire le fichier
  data <- read.csv(file)
  

  #Convert transpose
  data <- t(data)
  data <- as.data.frame(data)
  
  data$censure <- data$V5
  
  #check type 1 error (5%)
  
  nombre_test_sig <- sum(data$V1 >= 0, na.rm = TRUE)
  
  sifi_not_reached <- sum(is.na(data$V1))
  
  nombre_test_sig <- round((sifi_not_reached + nombre_test_sig) / 100,1)  
  
  #Keep just SIFI for   significant test 
  sifi <- data$V1[data$V1>0]
  
  
  n <- rep(660,length(sifi))
  censure <- data$censure[data$V1>0]
  
  prop <- round(abs(sum(sifi,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(sifi, na.rm=T) 
  q1 <- quantile(sifi, na.rm=T, 0.25) 
  q3 <- quantile(sifi, na.rm=T, 0.75) 
  mean <- round(mean(sifi, na.rm=T),4)
  sd <- round(sd(sifi,na.rm=T),4)
  prop_cens <- round(abs(sum(sifi,na.rm=T) / sum(censure,na.rm=T)),4)
  
  
  #Store results in list
  results_2a[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd, prop_cens = prop_cens)
}



#### scenario 2b #####



directory_path <- "./Tables/Table S6/2b"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}

results_2b <- list()

#setwd("~./Tables/Table S6/2b")

#Loop to process each file
for (file in file_names) {
  # Lire le fichier
  data <- read.csv(file)
  
  
  #Convert transpose
  data <- t(data)
  data <- as.data.frame(data)
  
  data$censure <- data$V5
  
  #check type 1 error (5%)
  
  nombre_test_sig <- round((sum(data$V1 >= 0, na.rm = TRUE)) / 100, 1)
  
  
  #Keep just SIFI for   significant test 
  sifi <- data$V1[data$V1>0]
  
  
  n <- rep(660,length(sifi))
  censure <- data$censure[data$V1>0]
  
  prop <- round(abs(sum(sifi,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(sifi, na.rm=T) 
  q1 <- quantile(sifi, na.rm=T, 0.25) 
  q3 <- quantile(sifi, na.rm=T, 0.75) 
  mean <- round(mean(sifi, na.rm=T),4)
  sd <- round(sd(sifi,na.rm=T),4)
  prop_cens <- round(abs(sum(sifi,na.rm=T) / sum(censure,na.rm=T)),4)
  
  
  #Store results in list
  results_2b[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd, prop_cens = prop_cens)
}



#### table #######

Table <- data.frame(matrix(vector(), 12, 9, 
                           dimnames=list(c(),c("Methods","RHO", "Median","Q1","Q3", "Mean","SD", "Prop-SIFI","PropC"))),
                    stringsAsFactors = F)

Table$Methods <- c("H1 2a  - Best Clone","Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete",
                   "H1 2b  - Best Clone","Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete")



##RHO 

H1_2a <- unlist(sapply(results_2a, function(x) x[1]))
H1_2b <- unlist(sapply(results_2b, function(x) x[1]))



##median 
median_2a <- unlist(sapply(results_2a, function(x) x[3]))
median_2b <- unlist(sapply(results_2b, function(x) x[3]))
##Q1 
q1_2a<- unlist(sapply(results_2a, function(x) x[4]))
q1_2b <- unlist(sapply(results_2b, function(x) x[4]))
##Q3 
q3_2a <- unlist(sapply(results_2a, function(x) x[5]))
q3_2b<- unlist(sapply(results_2b, function(x) x[5]))
#mean 
mean_2a <- unlist(sapply(results_2a, function(x) x[6]))
mean_2b <- unlist(sapply(results_2b, function(x) x[6]))
##szd 
sd_2a<- unlist(sapply(results_2a, function(x) x[7]))
sd_2b <- unlist(sapply(results_2b, function(x) x[7]))
##prop

prop_2a <- unlist(sapply(results_2a, function(x) x[2]))
prop_2b <- unlist(sapply(results_2b, function(x) x[2]))


##prop_censure

propC_2a <- unlist(sapply(results_2a, function(x) x[8]))
propC_2b <- unlist(sapply(results_2b, function(x) x[8]))

Table$RHO <- c(H1_2a,H1_2b )
Table$Median <- c(median_2a,median_2b)
Table$Q1 <- c(q1_2a,q1_2b)
Table$Q3 <- c(q3_2a,q3_2b)
Table$Mean <- c(mean_2a,mean_2b)
Table$SD <- c(sd_2a,sd_2b)
Table$Prop.SIFI <- c(prop_2a,prop_2b)
Table$PropC <- c(propC_2a,propC_2b)

write.csv(Table, "./TableS6.csv")











