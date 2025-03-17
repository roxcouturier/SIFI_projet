##### under H0 #####

directory_path <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S2/Data"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}


results <- list()

#setwd("~./Tables/Table S2/Data")

#Loop to process each file
for (file in file_names) {
  # Lire le fichier
  data <- read.csv(file)
  
  #Convert transpose
  data <- t(data)
  data <- as.data.frame(data)
  
  
  #check type 1 error (5%)
  
  nombre_test_sig <- round((sum(data$V1>=0))/100,1)
  
  
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





#### table ######

Table <- data.frame(matrix(vector(), 12, 8, 
                           dimnames=list(c(),c("Methods","RHO", "Median","Q1","Q3", "Mean","SD", "Prop-SIFI"))),
                    stringsAsFactors = F)

Table$Methods <- c("Best Clone","H0 1b  - Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete",
                   "Best Clone","H1 1c - Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete")



##RHO 

premiers_elements <- sapply(results, function(x) x[1])
premiers_elements <- unlist(premiers_elements)



##median 

median_H0 <- sapply(results, function(x) x[3])
median_H0 <- unlist(median_H0)


##Q1 

q1_H0 <- sapply(results, function(x) x[4])
q1_H0 <- unlist(q1_H0)



##Q3 

q3_H0 <- sapply(results, function(x) x[5])
q3_H0 <- unlist(q3_H0)



##mean 

mean_H0 <- sapply(results, function(x) x[6])
mean_H0 <- unlist(mean_H0)


##szd 

sd_H0 <- sapply(results, function(x) x[7])
sd_H0 <- unlist(sd_H0)

##prop

prop_H0 <- sapply(results, function(x) x[2])
prop_H0 <- unlist(prop_H0)




Table$RHO <- c(premiers_elements )
Table$Median <- c(median_H0)
Table$Q1 <- c(q1_H0)
Table$Q3 <- c(q3_H0)
Table$Mean <- c(mean_H0)
Table$SD <- c(sd_H0)
Table$Prop.SIFI <- c(prop_H0)

Table <- Table[c(
  2, 1, 3,  
  5, 4, 6, 
  8,7,9,
  11, 10, 12
), ]


write.csv(Table, file = "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Tables/Table S2/TableS2.csv")









