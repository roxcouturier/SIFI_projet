##### under H0 #####

H0_scenario1a_remove_random_66 <- read.csv2("./Tables/Table 4 /data/H0/H0_scenario1a_remove_random_66.csv", sep = ",")
H0_scenario1a_flip_random_66 <- read.csv2("./Tables/Table 4 /data/H0/H0_scenario1a_flip_random_66.csv")
H0_scenario1a_clone_random_66 <- read.csv2("./Tables/Table 4 /data/H0/H0_scenario1a_clone_random_66.csv")


### H0_scenario1a_remove_random_66####


  #check type 1 error (5%)
  
  nombre_test_sig_3 <- round((sum(H0_scenario1a_remove_random_66$V4 <= 0.05, na.rm = TRUE)) / 100, 1)
  
data_non_sig <- subset(H0_scenario1a_remove_random_66, H0_scenario1a_remove_random_66$V4>=0.05)
  data_non_sig$V1 <-  as.numeric(data_non_sig$V1)
  
  sifi_not_reached_3 <- sum(is.na(data_non_sig$V1))/100
  
  
  #Keep just SIFI for  non significant test 
  
  n_3 <- rep(132,nrow(data_non_sig)-sifi_not_reached_3*100)
  
  
  prop_3 <- round(abs(sum(data_non_sig$V1,na.rm=T) / sum(n_3,na.rm=T)),4)
  median_3 <- median(data_non_sig$V1, na.rm=T) 
  q1_3 <- quantile(data_non_sig$V1, na.rm=T, 0.25) 
  q3_3 <- quantile(data_non_sig$V1, na.rm=T, 0.75) 
  mean_3 <- round(mean(data_non_sig$V1, na.rm=T),4)
  sd_3<- round(sd(data_non_sig$V1,na.rm=T),4)


### H0_scenario1a_flip_random_66####
  
  
  #check type 1 error (5%)
  
  nombre_test_si_1 <- round((sum(H0_scenario1a_flip_random_66$V4 <= 0.05, na.rm = TRUE)) / 100, 1)
  
  data_non_sig <- subset(H0_scenario1a_flip_random_66, H0_scenario1a_flip_random_66$V4>=0.05)
  data_non_sig$V1 <-  as.numeric(data_non_sig$V1)
  
  sifi_not_reached_1 <- sum(is.na(data_non_sig$V1))/100
  
  
  #Keep just SIFI for  non significant test 
  
  n_1 <- rep(132,nrow(data_non_sig)-sifi_not_reached_1*100)
  
  
  prop_1 <- round(abs(sum(data_non_sig$V1,na.rm=T) / sum(n_1,na.rm=T)),4)
  median_1 <- median(data_non_sig$V1, na.rm=T) 
  q1_1 <- quantile(data_non_sig$V1, na.rm=T, 0.25) 
  q3_1<- quantile(data_non_sig$V1, na.rm=T, 0.75) 
  mean_1 <- round(mean(data_non_sig$V1, na.rm=T),4)
  sd_1<- round(sd(data_non_sig$V1,na.rm=T),4)
  
  
### H0_scenario1a_clone_random_66####
  
  
  #check type 1 error (5%)
  
  nombre_test_sig_2 <- round((sum(H0_scenario1a_clone_random_66$V4 <= 0.05, na.rm = TRUE)) / 100, 1)
  
  data_non_sig <- subset(H0_scenario1a_clone_random_66, H0_scenario1a_clone_random_66$V4>=0.05)
  data_non_sig$V1 <-  as.numeric(data_non_sig$V1)
  
  sifi_not_reached_2 <- sum(is.na(data_non_sig$V1))/100
  
  
  #Keep just SIFI for  non significant test 
  
  n_2 <- rep(132,nrow(data_non_sig)-sifi_not_reached_2*100)
  
  
  prop_2 <- round(abs(sum(data_non_sig$V1,na.rm=T) / sum(n_2,na.rm=T)),4)
  median_2 <- median(data_non_sig$V1, na.rm=T) 
  q1_2 <- quantile(data_non_sig$V1, na.rm=T, 0.25) 
  q3_2 <- quantile(data_non_sig$V1, na.rm=T, 0.75) 
  mean_2 <- round(mean(data_non_sig$V1, na.rm=T),4)
  sd_2 <- round(sd(data_non_sig$V1,na.rm=T),4)
  
  

  
  
##### under H1 #####

###### H1_scenario1a_remove_random_66 #####
  
  H1_scenario1a_remove_random_66 <- read.csv2("./Tables/Table 4 /data/H1/H1_scenario1a_remove_random_66.csv", sep = ",")
  
  
  #check type 1 error (5%)
  
  nombre_test_sig_31 <- round((sum(H1_scenario1a_remove_random_66$V5 >= 0.05, na.rm = TRUE)) / 100, 1)
  
  data_sig <- subset(H1_scenario1a_remove_random_66, H1_scenario1a_remove_random_66$V5>=0.05)
  data_sig$V1 <-  as.numeric(data_sig$V1)
  
  sifi_not_reached_31 <- round(sum(is.na(data_sig$V1))/100,4)
  
  
  #Keep just SIFI for  non significant test 
  
  n_31 <- rep(132,nrow(data_sig)-sifi_not_reached_31*100)
  
  
  prop_31 <- round(abs(sum(data_sig$V1,na.rm=T) / sum(n_31,na.rm=T)),4)
  median_31<- median(data_sig$V1, na.rm=T) 
  q1_31 <- quantile(data_sig$V1, na.rm=T, 0.25) 
  q3_31 <- quantile(data_sig$V1, na.rm=T, 0.75) 
  mean_31 <- round(mean(data_sig$V1, na.rm=T),4)
  sd_31<- round(sd(data_sig$V1,na.rm=T),4)
  
  
###### H1_scenario1a_flip_random_66 #####
  H1_scenario1a_flip_random_66 <- read.csv2("./Tables/Table 4 /data/H1/H1_scenario1a_flip_random_66.csv")
  
  #check type 1 error (5%)
  
  
  nombre_test_sig_11 <- round((sum(as.numeric(H1_scenario1a_flip_random_66$V4) <=0.05, na.rm = TRUE)) / 100, 1)
  
  data_sig <- subset(H1_scenario1a_flip_random_66, as.numeric(H1_scenario1a_flip_random_66$V4)<=0.05)
  data_sig$V1 <-  as.numeric(data_sig$V1)
  
  sifi_not_reached_11 <- sum(is.na(data_sig$V1))/100
  
  
  #Keep just SIFI for  non significant test 
  
  n_11 <- rep(132,nrow(data_sig)-sifi_not_reached_11*100)
  
  
  prop_11 <- round(abs(sum(data_sig$V1,na.rm=T) / sum(n_11,na.rm=T)),4)
  median_11<- median(data_sig$V1, na.rm=T) 
  q1_11 <- quantile(data_sig$V1, na.rm=T, 0.25) 
  q3_11 <- quantile(data_sig$V1, na.rm=T, 0.75) 
  mean_11 <- round(mean(data_sig$V1, na.rm=T),4)
  sd_11 <- round(sd(data_sig$V1,na.rm=T),4)
  
##### H1_scenario1a_clone_random_66 ######
  H1_scenario1a_clone_random_66 <- read.csv2("./Tables/Table 4 /data/H1/H1_scenario1a_clone_random_66.csv")
  
  H1_scenario1a_clone_random_66$V4 <- as.numeric(H1_scenario1a_clone_random_66$V4)
  
  #check type 1 error (5%)
  
  nombre_test_sig_21 <- round((sum(H1_scenario1a_clone_random_66$V4 <= 0.05, na.rm = TRUE)) / 100, 1)
  
  data_sig <- subset(H1_scenario1a_clone_random_66, H1_scenario1a_clone_random_66$V4<=0.05)
  data_sig$V1 <-  as.numeric(data_sig$V1)
  
  sifi_not_reached_21 <- sum(is.na(data_sig$V1))/100
  
  
  #Keep just SIFI for  non significant test 
  
  n_21 <- rep(132,nrow(data_sig)-sifi_not_reached_21*100)
  
  
  prop_21 <- round(abs(sum(data_sig$V1,na.rm=T) / sum(n_21,na.rm=T)),4)
  median_21 <- median(data_sig$V1, na.rm=T) 
  q1_21 <- quantile(data_sig$V1, na.rm=T, 0.25) 
  q3_21 <- quantile(data_sig$V1, na.rm=T, 0.75) 
  mean_21 <- round(mean(data_sig$V1, na.rm=T),4)
  sd_21 <- round(sd(data_sig$V1,na.rm=T),4)
  
  
  #### table #########
  
  Table <- data.frame(matrix(vector(), 6, 9, 
                             dimnames=list(c(),c("Methods","RHO","% SIFI not reached", "Median","Q1","Q3", "Mean","SD", "Prop"))),
                      stringsAsFactors = F)
  
  Table$Methods <- c("H0 -  Flip","Clone"," Delete",
                     "H1 - Flip","Clone","Delete")
  
  

  Table$RHO <- c(nombre_test_si_1,nombre_test_sig_2,nombre_test_sig_3,nombre_test_sig_11,nombre_test_sig_21,nombre_test_sig_31)
  Table$X..SIFI.not.reached <- c(sifi_not_reached_1,sifi_not_reached_2,sifi_not_reached_3,sifi_not_reached_11,sifi_not_reached_21,sifi_not_reached_31)
  Table$Median <- c(median_1,median_2,median_3,median_11,median_21,median_31)
  Table$Q1 <- c(q1_1,q1_2,q1_3,q1_11,q1_21,q1_31)
  Table$Q3 <- c(q3_1,q3_2,q3_3,q3_11,q3_21,q3_31)
  Table$Mean <- c(mean_1,mean_2,mean_3,mean_11,mean_21,mean_31)
  Table$SD <- c(sd_1,sd_2,sd_3,sd_11,sd_21,sd_31)
  Table$Prop <- c(prop_1,prop_2,prop_3,prop_11,prop_21,prop_31)

  
  
  write.csv(Table, "./Table4.csv")
  
  
  
  
  
  
  
