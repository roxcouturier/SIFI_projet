library(ggplot2)
library(survival)
library(survminer)



####PFS 

PFS <- read.csv("/Users/roxanecouturier/Desktop/Doctorat/SIFI/Figures/Figure 1/Données PFS.csv", sep=";")

#We truncate the data at 60 months because after 60 months there are no more events.
#We prefer to display the data in the form of months

PFS$time_month <- PFS$time/30.5
PFS$event_month <- ifelse(PFS$time_month >=60, 0,PFS$event)
PFS$time_month <- ifelse(PFS$time_month >=60, 60,PFS$time_month)



PFS <- PFS[,c(4,5,1)] #time_month, event_month and Trt (treatment)
fit<- survfit(Surv(time_month, event_month) ~ Trt, data = PFS)


plot_PFS <- ggsurvplot(fit, data=PFS, pval = T, risk.table = T, ncensor.plot = TRUE  ,
                       palette  = c("#e4342c","#1c246c"),linetype=c("solid", "dotdash")) + xlab("Time in months") + ylab("PFS survival probability")





####OS 


OS <- read.csv("/Users/roxanecouturier/Desktop/Doctorat/SIFI/Figures/Figure 1/Données OS .csv", sep=";")

#We truncate the data at 60 months because after 60 months there are no more events.
#We prefer to display the data in the form of months

OS$time_month <- OS$suivi/30.5
OS$event_month <- ifelse(OS$time_month >=60, 0,OS$dc)
OS$time_month <- ifelse(OS$time_month >=60, 60,OS$time_month)



OS <- OS[,c(4,5,1)] #time_month, event_month and Trt (treatment)

plot_OS <- ggsurvplot(survfit(Surv(time_month,event_month)~Trt,data=OS), data=OS, pval = T, risk.table = T, ncensor.plot = TRUE  ,palette  = c("#e4342c","#1c246c"), linetype =c("solid", "dotdash") ) + xlab("Time in months") + ylab("OS survival probability")  




