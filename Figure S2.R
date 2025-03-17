#### data ####
library(here)
library(survival)
library(ggplot2)
library(survminer)
library(ggforce)
library(ggnewscale)
PFS <- read_delim(here("/Users/roxanecouturier/Desktop/Doctorat/SIFI/Figures/Figure S2/PFS.csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(PFS)

#Sort the dataset according to follow-up times: give a rank to the patients
#patients with the longest follow-up time = patient with the longest rank 

indices_tri <- order(PFS$time) 
PFS <- PFS[indices_tri, ]
PFS$rank <- seq(1,nrow(PFS),1)

####

n1<-nrow(PFS) #number of lines: patient databases
p = 1 #a single parameter: a single variable treatment



########### Model de cox ############


model<-coxph(Surv(time, event)~ Trt=="A", data=PFS, method="breslow")

##########Martingale residual standardized and adjusted form ##############


XX <- model.matrix(~ Trt, data = PFS)
H<-diag(XX%*%solve(t(XX)%*%XX)%*%t(XX))

#standardized

mresids <- residuals( model, type="martingale" )

sres<-(mresids)/sqrt(1-H)
S=sum(sres^2)/(n1-p)
tm<-mresids/S*sqrt(1-H)
CDM<-(H*tm^2)/((p+1)*(1-H))

#adjusted

Amresids<-(mresids-mean(mresids))/sd(mresids)
AS=sum(Amresids^2)/(n1-p)
Atm<-Amresids/AS*sqrt(1-H)
ACDM<-(H*Atm^2)/((p+1)*(1-H))


##########Plot ##############


PFS$CDM <- CDM

#graph: standardized martingale residuals for the PFS dataset





ggplot(PFS, aes(x = rank, y = CDM)) +
  geom_point(aes(shape = factor(Trt), 
                 color = factor(event), 
                 fill = ifelse(event == 1, "white", "#fc8d59")), 
             size = 5) +
  scale_color_manual(name = "Event", values = c("black", "#fc8d59")) +
  scale_shape_manual(name = "Treatment", values = c("A" = 21, "B" = 23)) +
  scale_fill_manual(name = "Event", values = c("white", "#fc8d59"), guide="none") +
  new_scale_color() +
  geom_ellipse(data = NULL, mapping = aes(x0 = 3, y0 = .0526, b = .0005, a = 1.2, angle = 0, color = "Flip"), inherit.aes = FALSE) +
  geom_ellipse(data = NULL, mapping = aes(x0 = 4, y0 = .0517, b = .002, a = 2.7, angle = 0, color = "Clone/Remove"), inherit.aes = FALSE) +
  scale_color_manual(name = "SIFI", values = c("red", "black")) +
  
  scale_x_continuous(breaks = seq(0, 155, by = 10)) +
  scale_y_continuous(breaks = seq(0, 0.1, by = 0.01)) +
  xlab("rank") + ylab("CDM") +
  labs(shape = "Treatment") +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color =alpha("lightgrey",0.2), size = 0.5),
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),           # Taille du titre de l'axe x
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    text = element_text(size = 20),
    
  )








