library(stringr)

source("./functions/functions_sifi.R")
source("./functions/functions_simulations.R")


####execution

#set.seed(145) for each scenario 

###scenarios

###10% censored data 
set.seed(145) 
H0_scenario1a_best_flip_10per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,74,74 , 1,1, 491,"best","flip"))
set.seed(145) 
H0_scenario1a_best_clone_10per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,74,74, 1,1,491,"best","clone"))
set.seed(145) 
H0_scenario1a_worst_flip_10per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,74,74, 1,1,491,"worst","flip"))
set.seed(145) 
H0_scenario1a_worst_clone_10per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,74,74, 1,1,491,"worst","clone"))
set.seed(145) 
H0_scenario1a_best_remove_10per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,74,74, 1,1,491,"best"))
set.seed(145) 
H0_scenario1a_worst_remove_10per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,74,74, 1,1,491,"worst"))

set.seed(145) 
H1_scenario1a_best_flip_10per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,74,74 , 1,1,710,"best","flip"))
set.seed(145) 
H1_scenario1a_best_clone_10per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,74,74 , 1,1,710,"best","clone"))
set.seed(145) 
H1_scenario1a_worst_flip_10per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,74,74 , 1,1,710,"worst","flip"))
set.seed(145) 
H1_scenario1a_worst_clone_10per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,74,74 , 1,1,710,"worst","clone"))
set.seed(145) 
H1_scenario1a_best_remove_10per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,74,74 , 1,1,710,"best"))
set.seed(145) 
H1_scenario1a_worst_remove_10per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,74,74 , 1,1,710,"worst"))


###40% censored data 
set.seed(145) 
H0_scenario1a_best_flip_40per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,110,110 , 1,1, 165,"best","flip"))
set.seed(145) 
H0_scenario1a_best_clone_40per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,110,110, 1,1,165,"best","clone"))
set.seed(145) 
H0_scenario1a_worst_flip_40per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,110,110, 1,1,165,"worst","flip"))
set.seed(145) 
H0_scenario1a_worst_clone_40per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,110,110, 1,1,165,"worst","clone"))
set.seed(145) 
H0_scenario1a_best_remove_40per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,110,110, 1,1,165,"best"))
set.seed(145) 
H0_scenario1a_worst_remove_40per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,110,110, 1,1,165,"worst"))

set.seed(145) 
H1_scenario1a_best_flip_40per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,110,110 , 1,1,279,"best","flip"))
set.seed(145) 
H1_scenario1a_best_clone_40per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,110,110 , 1,1,279,"best","clone"))
set.seed(145) 
H1_scenario1a_worst_flip_40per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,110,110 , 1,1,279,"worst","flip"))
set.seed(145) 
H1_scenario1a_worst_clone_40per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,110,110 , 1,1,279,"worst","clone"))
set.seed(145) 
H1_scenario1a_best_remove_40per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,110,110 , 1,1,279,"best"))
set.seed(145) 
H1_scenario1a_worst_remove_40per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,110,110 , 1,1,279,"worst"))


###80% censored data 
set.seed(145) 
H0_scenario1a_best_flip_80per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,330,330 , 1,1, 44.7,"best","flip"))
set.seed(145) 
H0_scenario1a_best_clone_80per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,330,330, 1,1,44.7,"best","clone"))
set.seed(145) 
H0_scenario1a_worst_flip_80per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,330,330, 1,1,44.7,"worst","flip"))
set.seed(145) 
H0_scenario1a_worst_clone_80per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,330,330, 1,1,44.7,"worst","clone"))
set.seed(145) 
H0_scenario1a_best_remove_80per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,330,330, 1,1,44.7,"best"))
set.seed(145) 
H0_scenario1a_worst_remove_80per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,330,330, 1,1,44.7,"worst"))

set.seed(145) 
H1_scenario1a_best_flip_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,1,87,"best","flip"))
set.seed(145) 
H1_scenario1a_best_clone_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,1,87,"best","clone"))
set.seed(145) 
H1_scenario1a_worst_flip_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,1,87,"worst","flip"))
set.seed(145) 
H1_scenario1a_worst_clone_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,1,87,"worst","clone"))
set.seed(145) 
H1_scenario1a_best_remove_80per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,330,330 , 1,1,87,"best"))
set.seed(145) 
H1_scenario1a_worst_remove_80per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,330,330 , 1,1,87,"worst"))


set.seed(145) 
H1_scenario2a_best_flip_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,0.7,87,"best","flip"))
set.seed(145) 
H1_scenario2a_best_clone_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,0.7,87,"best","clone"))
set.seed(145) 
H1_scenario2a_worst_flip_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,0.7,87,"worst","flip"))
set.seed(145) 
H1_scenario2a_worst_clone_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,0.7,87,"worst","clone"))
set.seed(145) 
H1_scenario2a_best_remove_80per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,330,330 , 1,0.7,87,"best"))
set.seed(145) 
H1_scenario2a_worst_remove_80per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,330,330 , 1,0.7,87,"worst"))


set.seed(145) 
H1_scenario2b_best_flip_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,2,87,"best","flip"))
set.seed(145) 
H1_scenario2b_best_clone_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,2,87,"best","clone"))
set.seed(145) 
H1_scenario2b_worst_flip_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,2,87,"worst","flip"))
set.seed(145) 
H1_scenario2b_worst_clone_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,2,87,"worst","clone"))
set.seed(145) 
H1_scenario2b_best_remove_80per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,330,330 , 1,2,87,"best"))
set.seed(145) 
H1_scenario2b_worst_remove_80per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,330,330 , 1,2,87,"worst"))

#names of scenarios

scenarios_names_H0 <- ls(pattern = "^H0_scenario")
scenarios_names_H0

file_path <- "./"

#each scenario in a CSV file
for (scenario_name in scenarios_names_H0) {
  scenario_data <- get(scenario_name)  #data
  write.csv(scenario_data, file = paste0(file_path, scenario_name, ".csv"), row.names = FALSE)
}



scenarios_names_H1 <- ls(pattern = "^H1_scenario")
scenarios_names_H1


#each scenario in a CSV file
for (scenario_name in scenarios_names_H1){
  scenario_data <- get(scenario_name)  #data
  write.csv(scenario_data, file = paste0(file_path, scenario_name, ".csv"), row.names = FALSE)
}


