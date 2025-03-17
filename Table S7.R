
##### under H0 ######

directory_path <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S7/contamination/H0"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}


#setwd("~./Tables/Table S5/contamination/H0")


results_H0 <- list()

#Loop to process each file
for (file in file_names) {
  # Lire le fichier
  data <- read.csv(file)
  
  
  #check type 1 error (5%)
  
  nombre_test_sig <- round((sum(data$x==0, na.rm = TRUE)) / 100, 1)
  
  
  
  #Keep just SIFI for  non significant test 
  data <- data$x[data$x>0]
  data <- data*66
  
  n <- rep(132,length(data))
  
  prop <- round(abs(sum(data,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(data, na.rm=T) 
  q1 <- quantile(data, na.rm=T, 0.25) 
  q3 <- quantile(data, na.rm=T, 0.75) 
  mean <- round(mean(data, na.rm=T),4) 
  sd <- round(sd(data,na.rm=T),4)
  
  
  
  #Store results in list
  results_H0[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd)
}

##### under H1 ######

directory_path <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S7/contamination/H1"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}


#setwd("~./Tables/Table S5/contamination/H1")


results_H1 <- list()

#Loop to process each file
for (file in file_names) {
  # Lire le fichier
  data <- read.csv(file)
  
  
  #check type 1 error (5%)
  
  nombre_test_sig <- round((sum(data$x>0, na.rm = TRUE)) / 100, 1)
  
  #Keep just SIFI for  non significant test 
  data <- data$x[data$x>0]
  data <- data*66
  
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

#### table #######

Table <- data.frame(matrix(vector(), 4, 8, 
                           dimnames=list(c(),c("Methods","RHO", "Median","Q1","Q3", "Mean","SD", "Prop-SIFI"))),
                    stringsAsFactors = F)

Table$Methods <- c("Best Respondeurs","H0 - Worst Respondeurs",
                   "Best Respondeurs","H1 - Worst Respondeurs ")



##RHO 

H0 <- unlist(sapply(results_H0, function(x) x[1]))
H1 <- unlist(sapply(results_H1, function(x) x[1]))



##median 
median_H0 <- unlist(sapply(results_H0, function(x) x[3]))
median_H1 <- unlist(sapply(results_H1, function(x) x[3]))
##Q1 
q1_H0<- unlist(sapply(results_H0, function(x) x[4]))
q1_H1 <- unlist(sapply(results_H1, function(x) x[4]))
##Q3 
q3_H0 <- unlist(sapply(results_H0, function(x) x[5]))
q3_H1<- unlist(sapply(results_H1, function(x) x[5]))
#mean 
mean_H0 <- unlist(sapply(results_H0, function(x) x[6]))
mean_H1 <- unlist(sapply(results_H1, function(x) x[6]))
##szd 
sd_H0<- unlist(sapply(results_H0, function(x) x[7]))
sd_H1 <- unlist(sapply(results_H1, function(x) x[7]))
##prop

prop_H0 <- unlist(sapply(results_H0, function(x) x[2]))
prop_H1 <- unlist(sapply(results_H1, function(x) x[2]))


Table$RHO <- c(H0,H1 )
Table$Median <- c(median_H0,median_H1)
Table$Q1 <- c(q1_H0,q1_H1)
Table$Q3 <- c(q3_H0,q3_H1)
Table$Mean <- c(mean_H0,mean_H1)
Table$SD <- c(sd_H0,sd_H1)
Table$Prop.SIFI <- c(prop_H0,prop_H1)

Table <- Table[c(
  2, 1,  
  4, 3 
  
), ]


write.csv(Table, "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S7/TableS7.csv")




