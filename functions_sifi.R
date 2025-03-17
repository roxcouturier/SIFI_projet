library(survival)
library(survminer)
library(ggplot2)
require(dplyr)

################ Original SIFI #######################
#Flip or Clone and Best or Worst 


sifi <- function(sv_data, treatment_arm = NULL,  # 'sv_data' should contain three columns: (1) time, (2) event, (3) arm
                 operation = c("flip","clone"),  # Flip or clone the best/worst responder
                 direction = c("best","worst"),  # Use the best responder or the worst responder
                 agnostic = F   # Agnostic determination of experimental vs reference group (based on the lower HR)
){
  
  
  
  # Evaluate input
  operation <- match.arg(operation)
  direction <- match.arg(direction)
  
  # Prepare data
  names(sv_data) <- c("time","event","arm")
  sv_data$arm <- as.factor(sv_data$arm)
  sv_data$id <- 1:nrow(sv_data)
  count <- 0 ; flag <- T
  
  # If the treatment arm wasn't defined, we use the agnostic approach,
  # by assigning the group that shows benefit (HR < 1) as the experimental group regardless of signifiance
  if((length(treatment_arm) == 0) | agnostic){
    sv_cox <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
    treatment_arm <- ifelse(sv_cox$coefficients < 0, yes = levels(sv_data$arm)[2], no = levels(sv_data$arm)[1])
  }
  
  # Original count
  n_arms <- table(sv_data$arm)
  
  #@@@@@@@@@@ WE NOW HAVE 4 OPTIONS (2x2)
  # 1) Re-designate the best responder  (longest time)  from experimental to control group
  # 2) Re-designate the worst responder (shortest time) from control to experimental group
  # 3) Flip that responder, 4) Clone that responder
  #@@@@@@@@@@
  
  
  #Value of log-rank test on original data
  sdf1 <- survdiff(Surv(time, event, type = "right") ~ arm, data = sv_data)
  pval1 <- 1 - pchisq(sdf1$chisq, length(sdf1$n) - 1)
  nb_censure <- sum(sv_data$event==0)
  
  
  
  # Initialize while loop
  while(flag){
    
    #Initialization of the p-value
    sdf <- survdiff(Surv(time, event, type = "right") ~ arm, data = sv_data)
    pval <- 1 - pchisq(sdf$chisq, length(sdf$n) - 1)  # Log-rank p-value directly from survdiff
    
    #@@@@@@@@@@ NEGATIVE SIFI, if in the first iteration the p-val is insignificant
    # calculate the negative SIFI, i.e. try to get it from non-significant to significant
    if(count == 0 & pval > 0.05){
      # Dump parameters
      count_neg <- neg_sifi(sv_data = sv_data[ , 1:3], treatment_arm = treatment_arm,
                            operation = operation, direction = direction,
                            agnostic = agnostic
                            
      )
      return(count_neg) #Value for the SIFI 
      #Number of participants to flip or clone to go from a non-significant to significant test
    }
    #@@@@@@@@@@
    
    
    # Build survival model, run COXPH, and calculate HR
    sft <- survfit(Surv(time, event, type = "right") ~ arm, data = sv_data)
    cxp <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
    hr <- summary(cxp)$conf.int[1]
    
    # If we reached NON-significance, then we're done and return SIFI, else re-deisgnate/add clone
    if(pval > 0.05){
      return(list(count,hr, pval1,pval,nb_censure)) 
      #count = value of the SIFI 
      #HR =  hazard ratio of the modified base (with fliped or cloned patients)
      #pval1 = p-value of the original base (without modification)
      #pval = p-value of the modified base (with fliped or cloned patients)
      #nb_censure = count the number of censored patient 
    } else {
      count <- count + 1 
    }
    
    # Two options:
    # (1) Re-designate BEST responder from the experimental to the control group (either event or censored)
    if(direction == "best") 
      responder <- sv_data %>% filter(arm == treatment_arm) %>% arrange(time) %>% tail(1)
    
    # (2) Re-desginate WORST responder from the control group to the experimental group (must be event)
    if(direction == "worst") 
      responder <- sv_data %>% filter(arm != treatment_arm & event == 1) %>% arrange(time) %>% head(1)
    
    #if(plot_iteration){
    # Calculate time of new responder
    s <- summary(sft, times = responder$time, extend = TRUE)  
  
    if(direction == "best"){
      sr <- c(responder$time, s$surv[paste0("arm=", treatment_arm) == s$strata]) #new arm treatment
      jd <- c(responder$time, s$surv[paste0("arm=", treatment_arm) != s$strata]) #starting arm treatment
    }
    if(direction == "worst"){
      sr <- c(responder$time, s$surv[paste0("arm=", treatment_arm) != s$strata]) #starting arm treatment
      jd <- c(responder$time, s$surv[paste0("arm=", treatment_arm) == s$strata]) #new arm treatment
    }
    
    
    # Two options:
    # (3) Flip the responder from its original group to the other arm
    if(operation == "flip") sv_data[responder$id , "arm"] <- setdiff(levels(responder$arm) , responder$arm)
    
    # (4) Clone the responder and ADD it to the other arm
    if(operation == "clone"){
      responder$id <- paste0(responder$id,"_clone")  # Add a tag
      responder$arm <- setdiff(levels(responder$arm) , responder$arm)  # Change to the other arm
      sv_data <- rbind(sv_data, responder)  # Concatenate it to the original cohort
    }
    
    #@@@@@@@@@@ KEEP IN MIND THAT IF WE CLONE A *CENSORED* INDIVIDUAL THE HR MAY NOT CHANGE AND WE WILL GET STUCK IN A LOOP
    #@@@@@@@@@@ THIS IS A SAFETY MECHANISM FOR THESE EXTREME CASES OR WHERE WE CAN'T REACH NON-SIGNIFICANCE...
    if(count > min(n_arms)){
      #if(plot_iteration & !is.na(file_iteration)) dev.off()  # Shut down device if we have a filename
      return(NA)
    }
  }
  
  
}



neg_sifi <- function(sv_data, treatment_arm = NULL,  # 'sv_data' should contain three columns: (1) time, (2) event, (3) arm
                     operation = c("flip","clone"),  # Flip or clone the best/worst responder
                     direction = c("best","worst"),  # Use the best responder or the worst responder
                     agnostic = F  # Agnostic determination of experimental vs reference group (based on the lower HR)
){
  
  require(dplyr)
  require(survival)
  
  # Evaluate input
  operation <- match.arg(operation)
  direction <- match.arg(direction)
  
  # Prepare data
  names(sv_data) <- c("time","event","arm")
  sv_data$arm <- as.factor(sv_data$arm)
  sv_data$id <- 1:nrow(sv_data)
  count <- 0 ; flag <- T
  
  # If the treatment arm wasn't defined, we use the agnostic approach,
  # by assigning the group that shows benefit (HR < 1) as the experimental group regardless of signifiance
  if((length(treatment_arm) == 0) | agnostic){
    sv_cox <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
    treatment_arm <- ifelse(sv_cox$coefficients < 0, yes = levels(sv_data$arm)[2], no = levels(sv_data$arm)[1])
  }
  
  # Original count
  n_arms <- table(sv_data$arm)
  
  #@@@@@@@@@@ WE NOW HAVE 4 OPTIONS (2x2)
  # 1) Re-designate the best responder  (longest time)  from experimental to control group
  # 2) Re-designate the worst responder (shortest time) from control to experimental group
  # 3) Flip that responder, 4) Clone that responder
  #@@@@@@@@@@
  
  
  
  #Value of log-rank test on original data
  sdf1 <- survdiff(Surv(time, event, type = "right") ~ arm, data = sv_data)
  pval1 <- 1 - pchisq(sdf1$chisq, length(sdf1$n) - 1)
  nb_censure <- sum(sv_data$event==0)
  
  
  # Initialize while loop
  while(flag){
    
    #Initialization of the p-value
    sdf <- survdiff(Surv(time, event, type = "right") ~ arm, data = sv_data)
    pval <- 1 - pchisq(sdf$chisq, length(sdf$n) - 1)  # Log-rank p-value directly from survdiff
    
    
    # Build survival model, run COXPH, and calculate HR
    sft <- survfit(Surv(time, event, type = "right") ~ arm, data = sv_data)
    cxp <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
    hr <- summary(cxp)$conf.int[1]
    
    
    # If we reached YES-significance, then we're done and return SIFI, else re-designate/add clone (mirror of positive SIFI)
    if(pval < 0.05){
      return(list(count,hr,pval1,pval,nb_censure))
      #count = value of the SIFI 
      #HR =  hazard ratio of the modified base (with fliped or cloned patients)
      #pval1 = p-value of the original base (without modification)
      #pval = p-value of the modified base (with fliped or cloned patients)
      #nb_censure = count the number of censored patient 
      
    } else {
      count <- count - 1   # Negative
    }
    
    # WE NOW MIRROR THE SAME APPROACH as positive SIFI
    control_arm <- setdiff(levels(sv_data$arm) , treatment_arm)
    
    # Two options:
    # (1) Re-designate BEST responder from the CONTROL to the EXPERIMENT group (i.e. the mirror of positive SIFI) (either event or censored)
    if(direction == "best") responder <- sv_data %>% filter(arm == control_arm) %>% arrange(time) %>% tail(1)
    
    # (2) Re-desginate WORST responder from the EXPERIMENTAL group to the CONTROL group (i.e. the mirror of positive SIFI) (must be event)
    if(direction == "worst") responder <- sv_data %>% filter(arm != control_arm & event == 1) %>% arrange(time) %>% head(1)
    
    #if(plot_iteration){
    # Calculate time of new responder
    s <- summary(sft, times = responder$time, extend = TRUE)  # Otherwise we get: 'Error in array(xx, dim = dd) : vector is too large'
    # 'sr' some responder, 'jd' just redesignated
    if(direction == "best"){
      sr <- c(responder$time, s$surv[paste0("arm=", control_arm) == s$strata])  
      jd <- c(responder$time, s$surv[paste0("arm=", control_arm) != s$strata])  
    }
    if(direction == "worst"){
      sr <- c(responder$time, s$surv[paste0("arm=", control_arm) != s$strata]) 
      jd <- c(responder$time, s$surv[paste0("arm=", control_arm) == s$strata]) 
    }
    
    
    # Two options:
    # (3) Flip the responder from its original group to the other arm
    if(operation == "flip") sv_data[responder$id , "arm"] <- setdiff(levels(responder$arm) , responder$arm)
    
    # (4) Clone the responder and ADD it to the other arm
    if(operation == "clone"){
      responder$id <- paste0(responder$id,"_clone")  # Add a tag
      responder$arm <- setdiff(levels(responder$arm) , responder$arm)  # Change to the other arm
      sv_data <- rbind(sv_data, responder)  # Concatenate it to the original cohort
    }
    
    #@@@@@@@@@@ KEEP IN MIND THAT IF WE CLONE A *CENSORED* INDIVIDUAL THE HR MAY NOT CHANGE AND WE WILL GET STUCK IN A LOOP
    #@@@@@@@@@@ THIS IS A SAFETY MECHANISM FOR THESE EXTREME CASES OR WHERE WE CAN'T REACH NON-SIGNIFICANCE...
    if(abs(count) > min(n_arms)){   # Absolute because 'count' is negative
      # if(plot_iteration & !is.na(file_iteration)) dev.off()  # Shut down device we have a filename
      return(NA)
    }
  }
  
  
  
}


################ Modified SIFI : Remove #######################
#Remove and Best or Worst 

sifi_remove <- function(sv_data, treatment_arm = NULL,  # 'sv_data' should contain three columns: (1) time, (2) event, (3) arm
                        direction = c("best","worst"),  # Use the best responder or the worst responder
                        agnostic = F) # Agnostic determination of experimental vs reference group (based on the lower HR)
  {
  
  
  # Evaluate input
  direction <- match.arg(direction)
  
  # Prepare data
  names(sv_data) <- c("time","event","arm")
  sv_data$arm <- as.factor(sv_data$arm)
  sv_data$id <- 1:nrow(sv_data)
  count <- 0 ; flag <- T
  
  # If the treatment arm wasn't defined, we use the agnostic approach,
  # by assigning the group that shows benefit (HR < 1) as the experimental group regardless of signifiance
  if((length(treatment_arm) == 0) | agnostic){
    sv_cox <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
    treatment_arm <- ifelse(sv_cox$coefficients < 0, yes = levels(sv_data$arm)[2], no = levels(sv_data$arm)[1])
  }
  
  # Original count
  n_arms <- table(sv_data$arm)
  
  #@@@@@@@@@@ WE NOW HAVE 2 OPTIONS (1x2)
  # 1) Re-designate the best responder  (longest time)  from experimental to control group
  # 2) Re-designate the worst responder (shortest time) from control to experimental group
  # 3) Remove that responder
  #@@@@@@@@@@
  
  
  
  #Value of log-rank test on original data
  sdf1 <- survdiff(Surv(time, event, type = "right") ~ arm, data = sv_data)
  pval1 <- 1 - pchisq(sdf1$chisq, length(sdf1$n) - 1)  # Log-rank p-value directly from survdiff
  nb_censure <- sum(sv_data$event==0)
  
   
  cxp1 <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data) #Coxph result on original data
  hr1 <- summary(cxp1)$conf.int[1] #HR on original data
  
  # Initialize while loop
  while(flag){
    
    # Option A: Calculate p-value of log-rank test (default) : Initialization of the p-value
    sdf <- survdiff(Surv(time, event, type = "right") ~ arm, data = sv_data)
    pval <- 1 - pchisq(sdf$chisq, length(sdf$n) - 1)  # Log-rank p-value directly from survdiff
    
    
    
    
    #@@@@@@@@@@ NEGATIVE SIFI, if in the first iteration the p-val is insignificant
    # calculate the negative SIFI, i.e. try to get it from non-significant to significant
    if(count == 0 & pval > 0.05){
      # Dump parameters
      count_neg <- neg_sifi_remove(sv_data = sv_data[ , 1:3], treatment_arm = treatment_arm,
                                   direction = direction,
                                   agnostic = agnostic
      )
      return(count_neg)
    }
    #@@@@@@@@@@
    
    
    #Initialization of the coxph, HR
    sft <- survfit(Surv(time, event, type = "right") ~ arm, data = sv_data)
    cxp <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
    hr <- summary(cxp)$conf.int[1]
    
    
    
    
    # If we reached NON-significance, then we're done and return SIFI, else re-deisgnate/add clone
    if(pval > 0.05){
      return(list(count,hr,pval1,pval,nb_censure))
      #count = value of the SIFI 
      #HR =  hazard ratio of the modified base (with fliped or cloned patients)
      #pval1 = p-value of the original base (without modification)
      #pval = p-value of the modified base (with fliped or cloned patients)
      #nb_censure = count the number of censored patient 
      
    } else {
      count <- count + 1
    }
    
    # Two options:
    # (1) Re-designate BEST responder from the experimental to the control group (either event or censored)
    if(direction == "best") 
      responder <- sv_data %>% filter(arm == treatment_arm) %>% arrange(time) %>% tail(1)
    
    # (2) Re-desginate WORST responder from the control group to the experimental group (must be event)
    if(direction == "worst") 
      responder <- sv_data %>% filter(arm != treatment_arm & event == 1) %>% arrange(time) %>% head(1)
    
    # Calculate time of new responder
    s <- summary(sft, times = responder$time, extend = TRUE) 
    
    if(direction == "best"){
      sr <- c(responder$time, s$surv[paste0("arm=", treatment_arm) == s$strata]) 
    }
    if(direction == "worst"){
      sr <- c(responder$time, s$surv[paste0("arm=", treatment_arm) != s$strata]) 
    }
    
    
    
    # (3) Removed the responder from its original group
    
    sv_data <- slice(sv_data, -which(sv_data$id ==responder$id))
    
    
    
    #@@@@@@@@@@ KEEP IN MIND THAT IF WE CLONE A *CENSORED* INDIVIDUAL THE HR MAY NOT CHANGE AND WE WILL GET STUCK IN A LOOP
    #@@@@@@@@@@ THIS IS A SAFETY MECHANISM FOR THESE EXTREME CASES OR WHERE WE CAN'T REACH NON-SIGNIFICANCE...
    if(abs(count) > min(n_arms)-1){   # Absolute because 'count' is negative
      # if(plot_iteration & !is.na(file_iteration)) dev.off()  # Shut down device we have a filename
      return(NA)
    }
   
  }
  
}


neg_sifi_remove<- function(sv_data, treatment_arm = NULL,  # 'sv_data' should contain three columns: (1) time, (2) event, (3) arm
                           direction = c("best","worst"),  # Use the best responder or the worst responder
                           agnostic = F   # Agnostic determination of experimental vs reference group (based on the lower HR)
){
  
  require(dplyr)
  require(survival)
  
  # Evaluate input
  direction <- match.arg(direction)
  
  # Prepare data
  names(sv_data) <- c("time","event","arm")
  sv_data$arm <- as.factor(sv_data$arm)
  sv_data$id <- 1:nrow(sv_data)
  count <- 0 ; flag <- T
  
  # If the treatment arm wasn't defined, we use the agnostic approach,
  # by assigning the group that shows benefit (HR < 1) as the experimental group regardless of signifiance
  if((length(treatment_arm) == 0) | agnostic){
    sv_cox <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
    treatment_arm <- ifelse(sv_cox$coefficients < 0, yes = levels(sv_data$arm)[2], no = levels(sv_data$arm)[1])
  }
  
  # Original count
  n_arms <- table(sv_data$arm)
  
  #@@@@@@@@@@ WE NOW HAVE 2 OPTIONS (1x2)
  # 1) Re-designate the best responder  (longest time)  from experimental to control group
  # 2) Re-designate the worst responder (shortest time) from control to experimental group
  # 3) Remove that responder
  #@@@@@@@@@@
  
  
  
  #Value of log-rank test on original data, HR, and Coxph 
  
  sdf1 <- survdiff(Surv(time, event, type = "right") ~ arm, data = sv_data)
  pval1 <- 1 - pchisq(sdf1$chisq, length(sdf1$n) - 1)  # Log-rank p-value directly from survdiff
  cxp1 <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
  hr1 <- summary(cxp1)$conf.int[1]
  nb_censure <- sum(sv_data$event==0)
  
  # Initialize while loop
  while(flag){
    
    #Initialization:
    # Option A: Calculate p-value of log-rank test (default) 
    sdf <- survdiff(Surv(time, event, type = "right") ~ arm, data = sv_data)
    pval <- 1 - pchisq(sdf$chisq, length(sdf$n) - 1)  # Log-rank p-value directly from survdiff
    
    
    
    # Build survival model, run COXPH, and calculate HR
    sft <- survfit(Surv(time, event, type = "right") ~ arm, data = sv_data)
    cxp <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
    hr <- summary(cxp)$conf.int[1]
    
    
    
    # If we reached YES-significance, then we're done and return SIFI, else re-designate/add clone (mirror of positive SIFI)
    if(pval < 0.05){
      return(list(count,hr, pval1,pval,nb_censure))
      #count = value of the SIFI 
      #HR =  hazard ratio of the modified base (with fliped or cloned patients)
      #pval1 = p-value of the original base (without modification)
      #pval = p-value of the modified base (with fliped or cloned patients)
      #nb_censure = count the number of censored patient 
      
    } else {
      count <- count - 1   # Negative
    }
    
    # WE NOW MIRROR THE SAME APPROACH as positive SIFI
    control_arm <- setdiff(levels(sv_data$arm) , treatment_arm)
    
    # Two options:
    # (1) Re-designate BEST responder from the CONTROL to the EXPERIMENT group (i.e. the mirror of positive SIFI) (either event or censored)
    if(direction == "best") responder <- sv_data %>% filter(arm == control_arm) %>% arrange(time) %>% tail(1)
    
    # (2) Re-desginate WORST responder from the EXPERIMENTAL group to the CONTROL group (i.e. the mirror of positive SIFI) (must be event)
    if(direction == "worst") responder <- sv_data %>% filter(arm != control_arm & event == 1) %>% arrange(time) %>% head(1)
    
    # Calculate time of new responder
    s <- summary(sft, times = responder$time, extend = TRUE)  # Otherwise we get: 'Error in array(xx, dim = dd) : vector is too large'
    if(direction == "best"){
      sr <- c(responder$time, s$surv[paste0("arm=", control_arm) == s$strata])  
    }
    if(direction == "worst"){
      sr <- c(responder$time, s$surv[paste0("arm=", control_arm) != s$strata]) 
    }
    
    
    
    # (3) Removed the responder from its original group
    
    sv_data <- slice(sv_data, -which(sv_data$id ==responder$id))
    
    
    #@@@@@@@@@@ KEEP IN MIND THAT IF WE CLONE A *CENSORED* INDIVIDUAL THE HR MAY NOT CHANGE AND WE WILL GET STUCK IN A LOOP
    #@@@@@@@@@@ THIS IS A SAFETY MECHANISM FOR THESE EXTREME CASES OR WHERE WE CAN'T REACH NON-SIGNIFICANCE...
    if(abs(count) > min(n_arms)-1){   # Absolute because 'count' is negative
      # if(plot_iteration & !is.na(file_iteration)) dev.off()  # Shut down device we have a filename
      return(NA)
    }
  }
  
}



################ Modified SIFI : Random #######################
#Flip and Clone
#Patients are randomly selected from the sample; the direction of the draw, 
#whether from the experimental group to the control group or vice versa, 
#does not change the results because it is a random selection of patients
#here : operation on treatment arm


sifi_random<- function(sv_data, # 'sv_data' should contain three columns: (1) time, (2) event, (3) arm
                       treatment_arm = NULL,  
                       operation = c("flip","clone"), # Flip or clone the best/worst responder
                       agnostic = F   # Agnostic determination of experimental vs reference group (based on the lower HR)
){
  
  require(dplyr)
  require(survival)
  
  # Evaluate input
  operation <- match.arg(operation)
  
  # Prepare data
  names(sv_data) <- c("time","event","arm")
  sv_data$arm <- as.factor(sv_data$arm)
  sv_data$id <- 1:nrow(sv_data)
  count <- 0 ; flag <- T
  
  
  # If the treatment arm wasn't defined, we use the agnostic approach,
  # by assigning the group that shows benefit (HR < 1) as the experimental group regardless of signifiance
  if((length(treatment_arm) == 0) | agnostic){
    sv_cox <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
    treatment_arm <- ifelse(sv_cox$coefficients < 0, yes = levels(sv_data$arm)[2], no = levels(sv_data$arm)[1])
  }
  
  # Original count
  n_arms <- table(sv_data$arm) 
  
  
  #Value of log-rank test on original data, HR, and Coxph 
  sdf1 <- survdiff(Surv(time, event, type = "right") ~ arm, data = sv_data)
  pval1 <- 1 - pchisq(sdf1$chisq, length(sdf1$n) - 1)
  cxp1 <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
  hr1 <- summary(cxp1)$conf.int[1]
  nb_censure <- sum(sv_data$event==0)
  
  
  
  # Initialize while loop
  while(flag){
    
    #Initialization value 
    sdf <- survdiff(Surv(time, event, type = "right") ~ arm, data = sv_data)
    pval <- 1 - pchisq(sdf$chisq, length(sdf$n) - 1) 
    cxp <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
    hr <- summary(cxp)$conf.int[1]
    
    
    #@@@@@@@@@@ NEGATIVE SIFI, if in the first iteration the p-val is insignificant
    # calculate the negative SIFI, i.e. try to get it from non-significant to significant
    if(count == 0 & pval > 0.05){
      count_neg <- neg_sifi_random(sv_data = sv_data[ , 1:3], treatment_arm = treatment_arm,
                                   operation = operation,
                                   agnostic = agnostic
      )
      return(count_neg)
    }
    #@@@@@@@@@@
    
    
  
    
    if(pval > 0.05){
      return(list(count,hr, pval1, pval,nb_censure))
     
      #count = value of the SIFI 
      #HR =  hazard ratio of the modified base (with fliped or cloned patients)
      #pval1 = p-value of the original base (without modification)
      #pval = p-value of the modified base (with fliped or cloned patients)
      #nb_censure = count the number of censored patient 
      

    } else {
      count <- count + 1
    }
    
    
    
    #Random selection of an individual from the experimental group of the trial
    
    responder <- sample(sv_data$id[sv_data$arm==treatment_arm],1)
    responder <- sv_data[responder,]
    #rang <- c(rang, responder$id)    
    
    # Two options:
    # (3) Flip the responder from its original group to the other arm
    if(operation == "flip") sv_data[responder$id , "arm"] <- setdiff(levels(responder$arm) , responder$arm)
    
    # (4) Clone the responder and ADD it to the other arm
    if(operation == "clone"){
      responder$id <- paste0(responder$id,"_clone")  # Add a tag
      responder$arm <- setdiff(levels(responder$arm) , responder$arm)  # Change to the other arm
      sv_data <- rbind(sv_data, responder)  # Concatenate it to the original cohort
    }
    
    
    if(count > (min(n_arms)-1)){
      count <- NA
      return(list(count,hr, pval1,pval,nb_censure))
    }
  }
  
  
}

neg_sifi_random<- function(sv_data, treatment_arm = NULL, 
                           operation = c("flip","clone"),  
                           agnostic = F  
                           
){
  
  require(dplyr)
  require(survival)
  
  # Evaluate input
  operation <- match.arg(operation)
  
  # Prepare data
  names(sv_data) <- c("time","event","arm")
  sv_data$arm <- as.factor(sv_data$arm)
  sv_data$id <- 1:nrow(sv_data)
  count <- 0 ; flag <- T
  
  if((length(treatment_arm) == 0) | agnostic){
    sv_cox <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
    treatment_arm <- ifelse(sv_cox$coefficients < 0, yes = levels(sv_data$arm)[2], no = levels(sv_data$arm)[1])
  }
  
  # Original count
  n_arms <- table(sv_data$arm)
  
  
  
  sdf1 <- survdiff(Surv(time, event, type = "right") ~ arm, data = sv_data)
  pval1 <- 1 - pchisq(sdf1$chisq, length(sdf1$n) - 1)
  cxp1 <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
  hr1 <- summary(cxp1)$conf.int[1]
  nb_censure <- sum(sv_data$event==0)
  
  
  while(flag){
    
    
    sdf <- survdiff(Surv(time, event, type = "right") ~ arm, data = sv_data)
    pval <- 1 - pchisq(sdf$chisq, length(sdf$n) - 1) 
    
    cxp <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
    hr <- summary(cxp)$conf.int[1]
    
    
    if(pval < 0.05){
      return(list(count,hr,pval1, pval,nb_censure))
      #count = value of the SIFI 
      #HR =  hazard ratio of the modified base (with fliped or cloned patients)
      #pval1 = p-value of the original base (without modification)
      #pval = p-value of the modified base (with fliped or cloned patients)
      #nb_censure = count the number of censored patient 
    } else {
      count <- count - 1   # Negative
    }
    
    
    #Random selection of an individual from the experimental group of the trial
    
    responder <- sample(sv_data$id[sv_data$arm==treatment_arm],1)
    responder <- sv_data[responder,]
    #rang <- c(rang, responder$id)
    
    
    
    if(operation == "flip") sv_data[responder$id , "arm"] <- setdiff(levels(responder$arm) , responder$arm)
    
    if(operation == "clone"){
      responder$id <- paste0(responder$id,"_clone")  # Add a tag
      responder$arm <- setdiff(levels(responder$arm) , responder$arm)  # Change to the other arm
      sv_data <- rbind(sv_data, responder)  # Concatenate it to the original cohort
    }
    
    
    #@@@@@@@@@@ KEEP IN MIND THAT IF WE CLONE A *CENSORED* INDIVIDUAL THE HR MAY NOT CHANGE AND WE WILL GET STUCK IN A LOOP
    #@@@@@@@@@@ THIS IS A SAFETY MECHANISM FOR THESE EXTREME CASES OR WHERE WE CAN'T REACH NON-SIGNIFICANCE...
    if(abs(count) > (min(n_arms)-1)){   # Absolute because 'count' is negative
      count <- NA
      return(list(count,hr, pval1,pval,nb_censure))
    }
  }
  
  
}

################ Modified SIFI : Random Remove #######################
#Remove
#Patients are randomly selected from the sample; the direction of the draw, 
#whether from the experimental group to the control group or vice versa, 
#does not change the results because it is a random selection of patients
#here : operation on control arm

sifi_random_remove <- function(sv_data, treatment_arm = NULL, # 'sv_data' should contain three columns: (1) time, (2) event, (3) arm
                               agnostic = F  # Agnostic determination of experimental vs reference group (based on the lower HR)
){
  
  require(dplyr)
  require(survival)
  
  
  
  # Prepare data
  names(sv_data) <- c("time","event","arm")
  sv_data$arm <- as.factor(sv_data$arm)
  sv_data$id <- 1:nrow(sv_data)
  count <- 0 ; flag <- T
  
  
  
  # If the treatment arm wasn't defined, we use the agnostic approach,
  # by assigning the group that shows benefit (HR < 1) as the experimental group regardless of signifiance
  if((length(treatment_arm) == 0) | agnostic){
    sv_cox <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
    treatment_arm <- ifelse(sv_cox$coefficients < 0, yes = levels(sv_data$arm)[2], no = levels(sv_data$arm)[1])
  }
  
  # Original count
  n_arms <- table(sv_data$arm) 
  
  
  #Value of log-rank test on original data, HR, and Coxph 
  sdf1 <- survdiff(Surv(time, event, type = "right") ~ arm, data = sv_data)
  pval1 <- 1 - pchisq(sdf1$chisq, length(sdf1$n) - 1)
  cxp1 <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
  hr1 <- summary(cxp1)$conf.int[1]
  nb_censure <- sum(sv_data$event==0)
  
  rang <-  c() #If you  want know the rang of subjects are deleted 
  
  # Initialize while loop
  while(flag){
    
    #Initialization value 
    
    sdf <- survdiff(Surv(time, event, type = "right") ~ arm, data = sv_data)
    pval <- 1 - pchisq(sdf$chisq, length(sdf$n) - 1)  
    cxp <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
    hr <- summary(cxp)$conf.int[1]
    
    
    #@@@@@@@@@@ NEGATIVE SIFI, if in the first iteration the p-val is insignificant
    # calculate the negative SIFI, i.e. try to get it from non-significant to significant
    if(count == 0 & pval > 0.05){
      count_neg <- neg_sifi_random_remove(sv_data = sv_data[ , 1:3], treatment_arm = treatment_arm,
                                          agnostic = agnostic
      )
      return(count_neg)
    }
    #@@@@@@@@@@@@@
    
    
    
    if(pval > 0.05){
      return(list(count,hr, pval1,pval,nb_censure))
      #count = value of the SIFI 
      #HR =  hazard ratio of the modified base (with fliped or cloned patients)
      #pval1 = p-value of the original base (without modification)
      #pval = p-value of the modified base (with fliped or cloned patients)
      #nb_censure = count the number of censored patient 
    } else {
      count <- count + 1
    }
    
    
    #Random selection of an individual from the control group of the trial
    #and deleted
    
    responder <- sample(sv_data$id[sv_data$arm!=treatment_arm & !is.na(sv_data$arm)],1) 
    responder_bis <- sv_data[responder,] 
    sv_data[responder,]$arm <- NA 
    rang <- c(rang, responder_bis$id)
    
    
    #@@@@@@@@@@ KEEP IN MIND THAT IF WE CLONE A *CENSORED* INDIVIDUAL THE HR MAY NOT CHANGE AND WE WILL GET STUCK IN A LOOP
    #@@@@@@@@@@ THIS IS A SAFETY MECHANISM FOR THESE EXTREME CASES OR WHERE WE CAN'T REACH NON-SIGNIFICANCE...
    if(count > (min(n_arms)-1)){
      count <- NA
      return(list(count,hr, pval1,pval,nb_censure))
      #count = value of the SIFI 
      #HR =  hazard ratio of the modified base (with fliped or cloned patients)
      #pval1 = p-value of the original base (without modification)
      #pval = p-value of the modified base (with fliped or cloned patients)
    }
  }
  
  
}

neg_sifi_random_remove <- function(sv_data, treatment_arm = NULL,  # 'sv_data' should contain three columns: (1) time, (2) event, (3) arm
                                   agnostic = F  # Agnostic determination of experimental vs reference group (based on the lower HR)
){
  
  require(dplyr)
  require(survival)
  
  
  # Prepare data
  names(sv_data) <- c("time","event","arm")
  sv_data$arm <- as.factor(sv_data$arm)
  sv_data$id <- 1:nrow(sv_data)
  count <- 0 ; flag <- T
  
  
  
  if((length(treatment_arm) == 0) | agnostic){
    sv_cox <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
    treatment_arm <- ifelse(sv_cox$coefficients < 0, yes = levels(sv_data$arm)[2], no = levels(sv_data$arm)[1])
  }
  
  # Original count
  n_arms <- table(sv_data$arm)
  
  
  #Value of log-rank test on original data, HR, and Coxph 
  
  sdf1 <- survdiff(Surv(time, event, type = "right") ~ arm, data = sv_data)
  pval1 <- 1 - pchisq(sdf1$chisq, length(sdf1$n) - 1)
  cxp1 <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
  hr1 <- summary(cxp1)$conf.int[1]
  rang <- c()
  nb_censure <- sum(sv_data$event==0)
  
  
  while(flag){
    
    #Initialization value 
    
    sdf <- survdiff(Surv(time, event, type = "right") ~ arm, data = sv_data)
    pval <- 1 - pchisq(sdf$chisq, length(sdf$n) - 1)  
    cxp <- coxph(Surv(time, event, type = "right") ~ arm, data = sv_data)
    hr <- summary(cxp)$conf.int[1]
    
    
    if(pval < 0.05){
      return(list(count,hr,pval1, pval,nb_censure))
      #count = value of the SIFI 
      #HR =  hazard ratio of the modified base (with fliped or cloned patients)
      #pval1 = p-value of the original base (without modification)
      #pval = p-value of the modified base (with fliped or cloned patients)
      #nb_censure = count the number of censored patient 
    } else {
      count <- count - 1   # Negative
    }
    
    
    
    
    
    #Random selection of an individual from the control group of the trial
    #and deleted
    responder <- sample(sv_data$id[sv_data$arm!=treatment_arm & !is.na(sv_data$arm)],1)
    responder_bis <- sv_data[responder,]
    sv_data[responder,]$arm <- NA
    rang <- c(rang, responder_bis$id)
    
    
    
    
    #@@@@@@@@@@ KEEP IN MIND THAT IF WE CLONE A *CENSORED* INDIVIDUAL THE HR MAY NOT CHANGE AND WE WILL GET STUCK IN A LOOP
    #@@@@@@@@@@ THIS IS A SAFETY MECHANISM FOR THESE EXTREME CASES OR WHERE WE CAN'T REACH NON-SIGNIFICANCE...
    
    if(abs(count) > (min(n_arms)-1)){  
      count <- NA 
      return(list(count,hr,pval1,pval,nb_censure))
      #count = value of the SIFI 
      #HR =  hazard ratio of the modified base (with fliped or cloned patients)
      #pval1 = p-value of the original base (without modification)
      #pval = p-value of the modified base (with fliped or cloned patients)
    }
  }
  
  
  
}





