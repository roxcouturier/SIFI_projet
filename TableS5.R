
### 10 % 
##H0 
directory_path <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S5/censure_inf_plus_non_inf/20per_censure_inf_puissance/H0/10"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}


results_H0_10 <- list()

#setwd("~./Tables/Table 3/10/H0")

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
  
  
  #Keep just SIFI for  non significant test 
  sifi <- data$V1[data$V1<0]
  
  
  n <- rep(148,length(sifi))
  censure <- data$censure[data$V1<0]
  
  prop <- round(abs(sum(sifi,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(sifi, na.rm=T) 
  q1 <- quantile(sifi, na.rm=T, 0.25) 
  q3 <- quantile(sifi, na.rm=T, 0.75) 
  mean <- round(mean(sifi, na.rm=T),4)
  sd <- round(sd(sifi,na.rm=T),4)
  prop_cens <- round(abs(sum(sifi,na.rm=T) / sum(censure,na.rm=T)),4)
  
  
  #Store results in list
  results_H0_10[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd, prop_cens = prop_cens)
}


##### under H1 #####

directory_path <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S5/censure_inf_plus_non_inf/20per_censure_inf_puissance/H1/10"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}


#setwd("~./Tables/Table 3/10/H1")

results_H1_10 <- list()

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
  
  
  n <- rep(148,length(sifi))
  censure <- data$censure[data$V1>0]
  
  prop <- round(abs(sum(sifi,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(sifi, na.rm=T) 
  q1 <- quantile(sifi, na.rm=T, 0.25) 
  q3 <- quantile(sifi, na.rm=T, 0.75) 
  mean <- round(mean(sifi, na.rm=T),4)
  sd <- round(sd(sifi,na.rm=T),4)
  prop_cens <- round(abs(sum(sifi,na.rm=T) / sum(censure,na.rm=T)),4)
  
  
  #Store results in list
  results_H1_10[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd, prop_cens = prop_cens)
}


#### 40% #####
##### under H0 #####

directory_path <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S5/censure_inf_plus_non_inf/20per_censure_inf_puissance/H0/40"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}

results_H0_40 <- list()
#setwd("~./Tables/Table 3/40/H0")

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
  
  
  #Keep just SIFI for  non significant test 
  sifi <- data$V1[data$V1<0]
  
  
  n <- rep(220,length(sifi))
  censure <- data$censure[data$V1<0]
  
  prop <- round(abs(sum(sifi,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(sifi, na.rm=T) 
  q1 <- quantile(sifi, na.rm=T, 0.25) 
  q3 <- quantile(sifi, na.rm=T, 0.75) 
  mean <- round(mean(sifi, na.rm=T),4)
  sd <- round(sd(sifi,na.rm=T),4)
  prop_cens <- round(abs(sum(sifi,na.rm=T) / sum(censure,na.rm=T)),4)
  
  
  #Store results in list
  results_H0_40[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd, prop_cens = prop_cens)
}


##### under H1 #####

directory_path <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S5/censure_inf_plus_non_inf/20per_censure_inf_puissance/H1/40"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}


#setwd("~./Tables/Table 3/40/H1")
results_H1_40 <- list()

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
  
  
  n <- rep(220,length(sifi))
  censure <- data$censure[data$V1>0]
  
  prop <- round(abs(sum(sifi,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(sifi, na.rm=T) 
  q1 <- quantile(sifi, na.rm=T, 0.25) 
  q3 <- quantile(sifi, na.rm=T, 0.75) 
  mean <- round(mean(sifi, na.rm=T),4)
  sd <- round(sd(sifi,na.rm=T),4)
  prop_cens <- round(abs(sum(sifi,na.rm=T) / sum(censure,na.rm=T)),4)
  
  
  #Store results in list
  results_H1_40[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd, prop_cens = prop_cens)
}


#### table #######

Table <- data.frame(matrix(vector(), 24, 9, 
                           dimnames=list(c(),c("Methods","RHO", "Median","Q1","Q3", "Mean","SD", "Prop-SIFI","PropCens"))),
                    stringsAsFactors = F)

Table$Methods <- c("Best Clone","H0 10  - Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete",
                   "Best Clone","H0 40  - Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete",
                   "Best Clone","H1 10  - Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete",
                   "Best Clone","H1 40  - Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete")



##RHO 

H0_10 <- unlist(sapply(results_H0_10, function(x) x[1]))
H0_40 <- unlist(sapply(results_H0_40, function(x) x[1]))
H1_10 <- unlist(sapply(results_H1_10, function(x) x[1]))
H1_40 <- unlist(sapply(results_H1_40, function(x) x[1]))


##median 
median_H0_10 <- unlist(sapply(results_H0_10, function(x) x[3]))
median_H0_40 <- unlist(sapply(results_H0_40, function(x) x[3]))
median_H1_10 <- unlist(sapply(results_H1_10, function(x) x[3]))
median_H1_40  <- unlist(sapply(results_H1_40, function(x) x[3]))
##Q1 
q1_H0_10 <- unlist(sapply(results_H0_10, function(x) x[4]))
q1_H0_40 <- unlist(sapply(results_H0_40, function(x) x[4]))
q1_H1_10 <- unlist(sapply(results_H1_10, function(x) x[4]))
q1_H1_40 <- unlist(sapply(results_H1_40, function(x) x[4]))
##Q3 
q3_H0_10 <- unlist(sapply(results_H0_10, function(x) x[5]))
q3_H0_40<- unlist(sapply(results_H0_40, function(x) x[5]))
q3_H1_10 <- unlist(sapply(results_H1_10, function(x) x[5]))
q3_H1_40 <- unlist(sapply(results_H1_40, function(x) x[5]))
#mean 
mean_H0_10 <- unlist(sapply(results_H0_10, function(x) x[6]))
mean_H0_40 <- unlist(sapply(results_H0_40, function(x) x[6]))
mean_H1_10 <- unlist(sapply(results_H1_10, function(x) x[6]))
mean_H1_40 <- unlist(sapply(results_H1_40, function(x) x[6]))
##szd 
sd_H0_10 <- unlist(sapply(results_H0_10, function(x) x[7]))
sd_H0_40 <- unlist(sapply(results_H0_40, function(x) x[7]))
sd_H1_10 <- unlist(sapply(results_H1_10, function(x) x[7]))
sd_H1_40 <- unlist(sapply(results_H1_40, function(x) x[7]))
##prop

prop_H0_10 <- unlist(sapply(results_H0_10, function(x) x[2]))
prop_H0_40 <- unlist(sapply(results_H0_40, function(x) x[2]))
prop_H1_10 <- unlist(sapply(results_H1_10, function(x) x[2]))
prop_H1_40 <- unlist(sapply(results_H1_40, function(x) x[2]))


#propcens 

propC_H0_10 <- unlist(sapply(results_H0_10, function(x) x[8]))
propC_H0_40 <- unlist(sapply(results_H0_40, function(x) x[8]))
propC_H1_10 <- unlist(sapply(results_H1_10, function(x) x[8]))
propC_H1_40 <- unlist(sapply(results_H1_40, function(x) x[8]))


Table$RHO <- c(H0_10 ,H0_40,H1_10,H1_40 )
Table$Median <- c(median_H0_10,median_H0_40,median_H1_10,median_H1_40)
Table$Q1 <- c(q1_H0_10,q1_H0_40,q1_H1_10,q1_H1_40)
Table$Q3 <- c(q3_H0_10,q3_H0_40,q3_H1_10,q3_H1_40)
Table$Mean <- c(mean_H0_10,mean_H0_40,mean_H1_10,mean_H1_40)
Table$SD <- c(sd_H0_10,sd_H0_40,sd_H1_10,sd_H1_40)
Table$Prop.SIFI <- c(prop_H0_10,prop_H0_40,prop_H1_10,prop_H1_40)
Table$PropCens <- c(propC_H0_10,propC_H0_40,propC_H1_10,propC_H1_40)


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


write.csv(Table, "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S5/TableS5.csv")
