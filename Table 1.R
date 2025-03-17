
##PFS and OS files : 
#3 columns: treatment arm (Trt), event time (time) and status (event)


library(readr)


source("./functions/functions_sifi.R")
file_path <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI"



PFS <- read_delim(file.path(file_path, "Tables/Table 1/Données PFS.csv"), 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)



best_flip_PFS <- sifi(PFS[,c(2,3,1)], treatment_arm = "A", operation="flip", direction="best")
worst_flip_PFS <- sifi(PFS[,c(2,3,1)], treatment_arm = "A", operation="flip", direction="worst")
best_clone_PFS <- sifi(PFS[,c(2,3,1)], treatment_arm = "A", operation="clone", direction="best")
worst_clone_PFS <- sifi(PFS[,c(2,3,1)], treatment_arm = "A", operation="clone", direction="worst")
best_remove_PFS <- sifi_remove(PFS[,c(2,3,1)], treatment_arm = "A",direction="best")
worst_remove_PFS <- sifi_remove(PFS[,c(2,3,1)], treatment_arm = "A", direction="worst")

# set.seed(145)
# random_flip_PFS <- replicate(10000,sifi_random(PFS[,c(2,3,1)], treatment_arm = "A", operation="flip"))
# random_flip_PFS <- as.data.frame(random_flip_PFS)
# random_flip_PFS <- data.frame(sapply(random_flip_PFS,unlist))
# write.csv(random_flip_PFS, file = paste0(file_path,"random_flip_PFS.csv"), row.names = FALSE)

##random_flip_PFS.csv, random_flip_OS.csv, random_clone_OS.csv,
#random_flip_clone.csv: files with 5 columns (V1 = SIFI value, V2 = HR at the end of the study,
#V3 = the p-value before applying the SIFI, V4 = the p_value after applying the SIFI method, 
#V5 = number of censored patients in the study)

random_flip_PFS <- read_csv(file.path(file_path,"Tables/Table 1/random_flip_PFS.csv"))
random_flip_PFS <- t(random_flip_PFS)
random_flip_PFS <- as.data.frame(random_flip_PFS)
median_flip_random_PFS <- median(as.numeric(random_flip_PFS$V1)) #median 10000 sim SIFI random
sum(is.na(as.numeric(random_flip_PFS$V1))) #SIFI not reached 

# set.seed(145)
# random_clone_PFS <-replicate(10000,sifi_random(PFS[,c(2,3,1)], treatment_arm = "A", operation="clone"))
# random_clone_PFS <- as.data.frame(random_clone_PFS)
# random_flip_PFS <- data.frame(sapply(random_clone_PFS,unlist))
# write.csv(random_clone_PFS, file = paste0(file_path,"random_clone_PFS.csv"), row.names = FALSE)

random_clone_PFS <- read_csv(file.path(file_path,"Tables/Table 1/random_clone_PFS.csv"))
random_clone_PFS <- t(random_clone_PFS)
random_clone_PFS <- as.data.frame(random_clone_PFS)
median_clone_random_PFS <- median <- median(as.numeric(random_clone_PFS$V1), na.rm=T) #median 10000 sim SIFI random
sum(is.na(as.numeric(random_clone_PFS$V1))) #SIFI not reached 

# set.seed(145)
# random_remove_PFS <- replicate(10000,sifi_random_remove(PFS[,c(2,3,1)], treatment_arm = "A"))
# random_remove_PFS <- as.data.frame(random_remove_PFS)
# random_flip_PFS <- data.frame(sapply(random_remove_PFS,unlist))
# write.csv(random_remove_PFS, file = paste0(file_path,"random_remove_PFS.csv"), row.names = FALSE)

random_remove_PFS <- read_csv(file.path(file_path,"Tables/Table 1/random_remove_PFS.csv"))
random_remove_PFS <- t(random_remove_PFS)
random_remove_PFS <- as.data.frame(random_remove_PFS)
median_remove_random_PFS <- median(as.numeric(random_remove_PFS$V1),na.rm=T) #median 10000 sim SIFI random
sum(is.na(as.numeric(random_remove_PFS$V1))) #SIFI not reached 


### table PFS 


OS <- read_delim(file.path(file_path,"/Tables/Table 1/Données OS .csv"), 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

best_flip_OS <- sifi(OS[,c(2,3,1)], treatment_arm = "A", operation="flip", direction="best")
worst_flip_OS <- sifi(OS[,c(2,3,1)], treatment_arm = "A", operation="flip", direction="worst")
best_clone_OS <- sifi(OS[,c(2,3,1)], treatment_arm = "A", operation="clone", direction="best")
worst_clone_OS <- sifi(OS[,c(2,3,1)], treatment_arm = "A", operation="clone", direction="worst")
best_remove_OS <- sifi_remove(OS[,c(2,3,1)], treatment_arm = "A",direction="best")
worst_remove_OS <- sifi_remove(OS[,c(2,3,1)], treatment_arm = "A", direction="worst")

# set.seed(145)
# random_flip_OS <- replicate(10000,sifi_random(OS[,c(2,3,1)], treatment_arm = "A", operation="flip"))
# random_flip_OS <- as.data.frame(random_flip_OS)
# random_flip_OS <- data.frame(sapply(random_flip_OS,unlist))
# write.csv(random_flip_OS, file = paste0(file_path,"random_flip_OS.csv"), row.names = FALSE)

random_flip_OS <- read_csv(file.path(file_path,"Tables/Table 1/random_flip_OS.csv"))
random_flip_OS <- t(random_flip_OS)
random_flip_OS <- as.data.frame(random_flip_OS)
median_flip_random_OS <- median(as.numeric(random_flip_OS$V1), na.rm=T)#median 10000 sim SIFI random
sum(is.na(as.numeric(random_flip_OS$V1)))  #SIFI not reached 


# set.seed(145)
# random_clone_OS <-replicate(10000,sifi_random(OS[,c(2,3,1)], treatment_arm = "A", operation="clone"))
# random_clone_OS <- as.data.frame(random_clone_OS)
# random_clone_OS <- data.frame(sapply(random_clone_OS,unlist))
# write.csv(random_clone_OS, file = paste0(file_path,"random_clone_OS.csv"), row.names = FALSE)

random_clone_OS <- read_csv(file.path(file_path,"Tables/Table 1/random_clone_OS.csv"))
random_clone_OS <- t(random_clone_OS)
random_clone_OS <- as.data.frame(random_clone_OS)
median_clone_random_OS <- median(as.numeric(random_clone_OS$V1), na.rm=T)#median 10000 sim SIFI random
sum(is.na(as.numeric(random_clone_OS$V1)))  #SIFI not reached 


# set.seed(145)
# random_remove_OS <- replicate(10000,sifi_random_remove(OS[,c(2,3,1)], treatment_arm = "A"))
# random_remove_OS <- as.data.frame(random_remove_OS)
# random_remove_OS <- data.frame(sapply(random_remove_OS,unlist))
# write.csv(random_remove_OS, file = paste0(file_path,"random_remove_OS.csv"), row.names = FALSE)

random_remove_OS <- read_csv(file.path(file_path,"Tables/Table 1/random_remove_OS.csv"))
random_remove_OS <- t(random_remove_OS)
random_remove_OS <- as.data.frame(random_remove_OS)
median_remove_random_OS <- median(as.numeric(random_remove_OS$V1), na.rm=T)#median 10000 sim SIFI random
sum(is.na(as.numeric(random_remove_OS$V1)))  #SIFI not reached 

##### table ####

Table1 <- data.frame(matrix(vector(), 9, 5, 
                            dimnames=list(c(),c("Methods", "SIFI", "p-value", "Negative SIFI", "p-value"))),
                     stringsAsFactors = F)

Table1$Methods <- c("Flip Best","Flip Worst","Clone Best","Clone Worst","Delete SIFI Best","Delete SIFI Worst","Random SIFI Flip","Random SIFI Clone","Random SIFI Delete")

Table1$SIFI <- c(best_flip_PFS[[1]],worst_flip_PFS[[1]],best_clone_PFS[[1]],worst_clone_PFS[[1]],best_remove_PFS[[1]],worst_remove_PFS[[1]],median_flip_random_PFS,median_clone_random_PFS,median_remove_random_PFS )
Table1$p.value <- c(best_flip_PFS[[4]],worst_flip_PFS[[4]],best_clone_PFS[[4]],worst_clone_PFS[[4]],best_remove_PFS[[4]],worst_remove_PFS[[4]],NA,NA,NA )

Table1$Negative.SIFI <- c(best_flip_OS[[1]],worst_flip_OS[[1]],best_clone_OS[[1]],worst_clone_OS[[1]],best_remove_OS[[1]],worst_remove_OS[[1]],median_flip_random_OS,median_clone_random_OS,median_remove_random_OS )
Table1$p.value.1 <- c(best_flip_OS[[4]],worst_flip_OS[[4]],best_clone_OS[[4]],worst_clone_OS[[4]],best_remove_OS[[4]],worst_remove_OS[[4]],NA,NA,NA )

write.csv(Table1, file.path(file_path,"Table1.csv"))


