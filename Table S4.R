
#### under H1 2a #####

directory_path <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S4/data 2a"
file_names <- list.files(directory_path, pattern = "\\.csv$")

#setwd("~./Tables/Table S4/data 2a")
for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}

results_2a <- list()

#Loop to process each file
for (file in file_names) {
  data <- read.csv(file)
  
  #Convert transpose
  data <- t(data)
  data <- as.data.frame(data)
  
  
  #check type 1 error (5%)
  
  nombre_test_sig <- sum(data$V1 >= 0, na.rm = TRUE)
  
  sifi_not_reached <- sum(is.na(data$V1))
  
  nombre_test_sig <- round((sifi_not_reached + nombre_test_sig) / 100,1)
  
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
  results_2a[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd)
}




##### under H1 2b  #####

directory_path <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S4/data 2b"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}


#setwd("~./Tables/Table S4/data 2b")

results_2b <- list()

#Loop to process each file
for (file in file_names) {
  # Lire le fichier
  data <- read.csv(file)
  
  #Convert transpose
  data <- t(data)
  data <- as.data.frame(data)
  
  
  #check type 1 error (5%)
  
  nombre_test_sig <- sum(data$V1 >= 0, na.rm = TRUE)
  
  sifi_not_reached <- sum(is.na(data$V1))
  
  nombre_test_sig <- round((sifi_not_reached + nombre_test_sig) / 100,1)
  
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
  results_2b[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd)
}




#### table #######

Table <- data.frame(matrix(vector(), 12, 8, 
                           dimnames=list(c(),c("Methods","RHO", "Median","Q1","Q3", "Mean","SD", "Prop-SIFI"))),
                    stringsAsFactors = F)

Table$Methods <- c("Best Clone","H1 2a  - Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete",
                   "Best Clone","H1 2b  - Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete")



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


Table$RHO <- c(H1_2a,H1_2b )
Table$Median <- c(median_2a,median_2b)
Table$Q1 <- c(q1_2a,q1_2b)
Table$Q3 <- c(q3_2a,q3_2b)
Table$Mean <- c(mean_2a,mean_2b)
Table$SD <- c(sd_2a,sd_2b)
Table$Prop.SIFI <- c(prop_2a,prop_2b)

Table <- Table[c(
  2, 1, 3,  
  5, 4, 6, 
  8,7,9,
  11, 10, 12 
), ]



write.csv(Table, "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S4/TableS4.csv")











