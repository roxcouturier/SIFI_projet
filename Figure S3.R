##### simulations data 


sim_data <- function(n1,n2,lambdaB, lambdaA,agnostic=F){
  
  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= 1) 
  B$event = 1
  
  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= 1) 
  A$event = 1
  
  
  data=bind_rows(A,B)
  
  data$trt<- as.factor(data$trt)
  
  
  if( agnostic==T){
    sv_cox <- coxph(Surv(time, event, type = "right") ~ trt, data = data)
    treatment <- ifelse(sv_cox$coefficients < 0, yes = levels(data$trt)[2], no = levels(data$trt)[1])
    placebo<- setdiff(levels(data$trt) , treatment)
    
  }
  
  indices_tri <- order(data$time) 
  data<- data[indices_tri, ]
  data$rank <- seq(1,nrow(data),1)
  
  #parameters 
  
  n1<-nrow(data)
  p = 1
  
  ###model 
  
  model<-coxph(Surv(time, event)~ trt==treatment, data=data, method="breslow")
  
  #### cook martingal residus  
  
  XX <- model.matrix(~ trt, data = data)
  H<-diag(XX%*%solve(t(XX)%*%XX)%*%t(XX))
  
  
  mresids <- residuals( model, type="martingale" )
  
  sres<-(mresids)/sqrt(1-H)
  S=sum(sres^2)/(n1-p)
  tm<-mresids/S*sqrt(1-H)
  CDM<-(H*tm^2)/((p+1)*(1-H))
  
  data <- cbind(data,CDM)
  
  

  return(CDM)

  
}

set.seed(145)
CDM <- replicate(10000,sim_data(66,66,210,370,agnostic = T))

mean_CDM <- rowMeans(CDM)
rank <- seq(1,132,1)

data <- data.frame(mean_CDM,rank)



ggplot(data, aes(x = rank, y = mean_CDM)) +
  geom_point() +          
  labs(x = "rank", y = "CDM") + 
  scale_x_continuous(breaks = seq(0, 132, by = 10)) +
  scale_y_continuous(breaks = seq(0, 0.13, by = 0.01)) +
  theme(
    panel.background = element_rect(fill = "white"),  # Set background color to white
    panel.grid.major = element_line(color = "lightgrey", size = 0.5),  # Adjust major grid lines
    panel.grid.minor = element_blank(), # Hide minor grid lines
    axis.text.x = element_text(size = 12),   # Increase x-axis label text size
    axis.text.y = element_text(size = 12)
  )


