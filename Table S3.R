##### under H0 #####

#####  n=40 ######
directory_path <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S3/SIFI_n_40_640/40"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}


results_40 <- list()


#Loop to process each file
for (file in file_names) {
  # Lire le fichier
  data <- read.csv(file)
  
  #Convert transpose
  data <- t(data)
  data <- as.data.frame(data)
  
  
  #check type 1 error (5%)
  
  nombre_test_sig <- round((sum(as.numeric(data$V4) <= 0.05, na.rm = TRUE))/100, 1)
  
  
  #Keep just SIFI for  non significant test 
  data <- data$V1[data$V1<0]
  
  sifi_not_reached <- sum(is.na(data))/100
  
  
  n <- rep(40,length(data)-sifi_not_reached*100)
  
  prop <- round(abs(sum(data,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(data, na.rm=T) 
  q1 <- quantile(data, na.rm=T, 0.25) 
  q3 <- quantile(data, na.rm=T, 0.75) 
  mean <- round(mean(data, na.rm=T),4)
  sd <- round(sd(data,na.rm=T),4)
  
  
  
  #Store results in list
  results_40[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd)
}


#####  n=80 ######

directory_path <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S3/SIFI_n_40_640/80"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}


results_80 <- list()

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
  
  sifi_not_reached <- sum(is.na(data))/100
  
  
  n <- rep(80,length(data)-sifi_not_reached*100)
  
  prop <- round(abs(sum(data,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(data, na.rm=T) 
  q1 <- quantile(data, na.rm=T, 0.25) 
  q3 <- quantile(data, na.rm=T, 0.75) 
  mean <- round(mean(data, na.rm=T),4)
  sd <- round(sd(data,na.rm=T),4)
  
  
  
  #Store results in list
  results_80[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd)
}



#####  n=160 ######
directory_path <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S3/SIFI_n_40_640/160"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}


results_160 <- list()

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
  
  sifi_not_reached <- sum(is.na(data))/100
  
  
  n <- rep(160,length(data)-sifi_not_reached*100)

  
  prop <- round(abs(sum(data,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(data, na.rm=T) 
  q1 <- quantile(data, na.rm=T, 0.25) 
  q3 <- quantile(data, na.rm=T, 0.75) 
  mean <- round(mean(data, na.rm=T),4)
  sd <- round(sd(data,na.rm=T),4)
  
  
  
  #Store results in list
  results_160[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd)
}




#####  n=640 ######
directory_path <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S3/SIFI_n_40_640/640"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}

#setwd("~./Tables/Table S3/SIFI_n_40_640/640")

results_640 <- list()

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
  
  sifi_not_reached <- sum(is.na(data))/100

  n <- rep(640,length(data)-sifi_not_reached*100)  
  
  prop <- round(abs(sum(data,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(data, na.rm=T) 
  q1 <- quantile(data, na.rm=T, 0.25) 
  q3 <- quantile(data, na.rm=T, 0.75) 
  mean <- round(mean(data, na.rm=T),4)
  sd <- round(sd(data,na.rm=T),4)
  
  
  
  #Store results in list
  results_640[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd)
}


#### table #######

Table <- data.frame(matrix(vector(), 24, 8, 
                           dimnames=list(c(),c("Methods","RHO", "Median","Q1","Q3", "Mean","SD", "Prop-SIFI"))),
                    stringsAsFactors = F)

Table$Methods <- c("Best Clone","H0 40  - Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete",
                   "Best Clone","H0 80  - Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete",
                   "Best Clone","H0 160 - Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete",
                   "Best Clone","H0 640 - Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete")



##RHO 

H0_40 <- unlist(sapply(results_40, function(x) x[1]))
H0_80 <- unlist(sapply(results_80, function(x) x[1]))
H0_160 <- unlist(sapply(results_160, function(x) x[1]))
H0_640 <- unlist(sapply(results_640, function(x) x[1]))


##median 
median_H0_40 <- unlist(sapply(results_40, function(x) x[3]))
median_H0_80 <- unlist(sapply(results_80, function(x) x[3]))
median_H0_160 <- unlist(sapply(results_160, function(x) x[3]))
median_H0_640 <- unlist(sapply(results_640, function(x) x[3]))
##Q1 
q1_H0_40 <- unlist(sapply(results_40, function(x) x[4]))
q1_H0_80 <- unlist(sapply(results_80, function(x) x[4]))
q1_H0_160 <- unlist(sapply(results_160, function(x) x[4]))
q1_H0_640 <- unlist(sapply(results_640, function(x) x[4]))
##Q3 
q3_H0_40 <- unlist(sapply(results_40, function(x) x[5]))
q3_H0_80<- unlist(sapply(results_80, function(x) x[5]))
q3_H0_160 <- unlist(sapply(results_160, function(x) x[5]))
q3_H0_640 <- unlist(sapply(results_640, function(x) x[5]))
#mean 
mean_H0_40 <- unlist(sapply(results_40, function(x) x[6]))
mean_H0_80 <- unlist(sapply(results_80, function(x) x[6]))
mean_H0_160 <- unlist(sapply(results_160, function(x) x[6]))
mean_H0_640 <- unlist(sapply(results_640, function(x) x[6]))
##szd 
sd_H0_40 <- unlist(sapply(results_40, function(x) x[7]))
sd_H0_80 <- unlist(sapply(results_80, function(x) x[7]))
sd_H0_160 <- unlist(sapply(results_160, function(x) x[7]))
sd_H0_640 <- unlist(sapply(results_640, function(x) x[7]))
##prop

prop_H0_40 <- unlist(sapply(results_40, function(x) x[2]))
prop_H0_80 <- unlist(sapply(results_80, function(x) x[2]))
prop_H0_160 <- unlist(sapply(results_160, function(x) x[2]))
prop_H0_640 <- unlist(sapply(results_640, function(x) x[2]))


Table$RHO <- c(H0_40 ,H0_80,H0_160,H0_640 )
Table$Median <- c(median_H0_40,median_H0_80,median_H0_160,median_H0_640)
Table$Q1 <- c(q1_H0_40,q1_H0_80,q1_H0_160,q1_H0_640)
Table$Q3 <- c(q3_H0_40,q3_H0_80,q3_H0_160,q3_H0_640)
Table$Mean <- c(mean_H0_40,mean_H0_80,mean_H0_160,mean_H0_640)
Table$SD <- c(sd_H0_40,sd_H0_80,sd_H0_160,sd_H0_640)
Table$Prop.SIFI <- c(prop_H0_40,prop_H0_80,prop_H0_160,prop_H0_640)


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

write.csv(Table, "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S3/TableS3.csv")























