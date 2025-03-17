
source("./functions/function_simulations_contamination.R")

library(dplyr)
library(survival)
library(survminer)

####  under H0 


set.seed(145)
H0_worst_contamination <- replicate(10000,worst_contamination_H0(210,210,66,66,1,1))

file_path <- "./"
write.csv(H0_worst_contamination, file = paste0(file_path,"H0_worst_contamination.csv"), row.names = FALSE)

set.seed(145)
H0_best_contamination <- replicate(10000,best_contamination_H0(210,210,66,66,1,1))
file_path <- "./"
write.csv(H0_best_contamination, file = paste0(file_path,"H0_best_contamination.csv"), row.names = FALSE)


#### under H1 

set.seed(145)
worst_contamination_H1 <- replicate(10000,worst_contamination_H1(210,370,66,66,1,1))
file_path <- "./"
write.csv(worst_contamination_H1, file = paste0(file_path,"worst_contamination_H1.csv"), row.names = FALSE)

set.seed(145)
best_contamination_H1 <- replicate(10000,best_contamination_H1(210,370,66,66,1,1))
file_path <- "."
write.csv(best_contamination_H1, file = paste0(file_path,"best_contamination_H1.csv"), row.names = FALSE)
