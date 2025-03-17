
#Read files 

library(here)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(hrbrthemes)
library(readr)

###read all files

#directory_path <- here("./Figures/Figure 4/Random_SIFI_n_40_640")

directory_path <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Figures/Figure 3/Random_SIFI_n_40_640"

files_names <- list.files(directory_path, pattern = "\\.csv$")




files_names <- list.files(path = directory_path, full.names = T)

for (file_name in files_names) {
  #read file
  data <- read_csv(file_name)
  
    file_name_clean <- tools::file_path_sans_ext(basename(file_name))
  
  assign(file_name_clean, data)
}




##### graph #### 

#couleurs <- rev(hcl.colors(4,palette = "Oranges"))

couleurs <- c("#fef0d9","#fdcc8a","#fc8d59","#d7301f")

#couleurs <- c("#e66101","#fdb863","#b2abd2","#5e3c99")


flip_40 <- subset(H0_scenario1a_flip_random_40, H0_scenario1a_flip_random_40$V4>0.05)
length(flip_40$V1)#number of non significant test 



#number of test not reached SFI : 
sum(is.na(flip_40$V1)) / length(flip_40$V1)

H0_scenario1a_flip_random_80 <- t(H0_scenario1a_flip_random_80)
H0_scenario1a_flip_random_80 <- as.data.frame(H0_scenario1a_flip_random_80)
flip_80  <- subset(H0_scenario1a_flip_random_80, as.numeric(H0_scenario1a_flip_random_80$V4)>0.05)
length(flip_80$V1) #number of non significant test 
sum(is.na(flip_80$V1)) / length(flip_80$V1)

H0_scenario1a_flip_random_160 <- t(H0_scenario1a_flip_random_160)
H0_scenario1a_flip_random_160 <- as.data.frame(H0_scenario1a_flip_random_160)
flip_160  <- subset(H0_scenario1a_flip_random_160, as.numeric(H0_scenario1a_flip_random_160$V4)>0.05)
length(flip_160$V1)
sum(is.na(flip_160$V1)) / length(flip_160$V1)



H0_scenario1a_flip_random_640 <- t(H0_scenario1a_flip_random_640)
H0_scenario1a_flip_random_640 <- as.data.frame(H0_scenario1a_flip_random_640)
flip_640  <- subset(H0_scenario1a_flip_random_640, as.numeric(H0_scenario1a_flip_random_640$V4)>0.05)
length(flip_640$V1)
sum(is.na(flip_640$V1)) / length(flip_640$V1)


best_flip <- data.frame(
  type = c( rep("n = 40", 10000), rep("n = 80", 10000),rep("n = 160", 10000), rep("n = 640", 10000) ),
  sifi = c(H0_scenario1a_flip_random_40$V1,as.numeric(H0_scenario1a_flip_random_80$V1),as.numeric(H0_scenario1a_flip_random_160$V1),as.numeric(H0_scenario1a_flip_random_640$V1))
) 

best_flip$sifi <- as.numeric(best_flip$sifi)
best_flip$type <- as.factor(best_flip$type)

best_flip <- subset(best_flip,best_flip$sifi<0)

best_flip$type <- factor(best_flip$type, levels = c("n = 640", "n = 160", "n = 80", "n = 40"))

best_flip$sifi <- abs(best_flip$sifi)



flip <- best_flip %>%
  ggplot( aes(x=sifi, fill=type)) +
  geom_bar( position = "stack") +
  scale_fill_manual(values = c("n = 40" = "#fef0d9", "n = 80" = "#fdcc8a", "n = 160" = "#fc8d59", "n = 640" = "#d7301f")) +
  #theme_ipsum() +
  labs(fill="", title = "Flip Ramdom SIFI")+   ylab("count") + xlab("SIFI") + 
  scale_x_continuous(breaks = seq(min(best_flip$sifi), max(best_flip$sifi), by = 50),
                     labels = seq(min(best_flip$sifi), max(best_flip$sifi), by = 50)) + 
  annotate("text", x = 170, y =10,label ="SIFI not reached : \n n=40 : 97.6%  \n n=80 : 57.5% \n n=160 : 53.1% \n n=640 : 43.9% ",  vjust = -0.4, color = "black", fontface = "bold", size=3) + theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color =alpha("lightgrey",0.2), size = 0.5),
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(size = 11),   # Increase x-axis label text size
    axis.text.y = element_text(size = 11),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 10),
    text = element_text(size = 12),
    panel.border = element_blank()  )




##clone


clone_40 <- subset(H0_scenario1a_clone_random_40, H0_scenario1a_clone_random_40$V4>0.05)
length(clone_40$V1)
sum(is.na(clone_40$V1)) / length(clone_40$V1)


clone_80  <- subset(H0_scenario1a_clone_random_80, as.numeric(H0_scenario1a_clone_random_80$V4)>0.05)
length(clone_80$V1)
sum(is.na(clone_80$V1)) / length(clone_80$V1)


H0_scenario1a_clone_random_160 <- t(H0_scenario1a_clone_random_160)
H0_scenario1a_clone_random_160 <- as.data.frame(H0_scenario1a_clone_random_160)
clone_160  <- subset(H0_scenario1a_clone_random_160, as.numeric(H0_scenario1a_clone_random_160$V4)>0.05)
length(clone_160$V1)
sum(is.na(clone_160$V1)) / length(clone_160$V1)


H0_scenario1a_clone_random_640 <- t(H0_scenario1a_clone_random_640)
H0_scenario1a_clone_random_640 <- as.data.frame(H0_scenario1a_clone_random_640)
clone_640  <- subset(H0_scenario1a_clone_random_640, as.numeric(H0_scenario1a_clone_random_640$V4)>0.05)
length(clone_640$V1)
sum(is.na(clone_640$V1)) / length(clone_640$V1)


clone <- data.frame(
  type = c( rep("n = 40", 10000), rep("n = 80", 10000),rep("n = 160", 10000), rep("n = 640", 10000) ),
  sifi = c(H0_scenario1a_clone_random_40$V1,H0_scenario1a_clone_random_80$V1,H0_scenario1a_clone_random_160$V1,H0_scenario1a_clone_random_640$V1)
) 

clone$sifi <- as.numeric(clone$sifi)
clone$type <- as.factor(clone$type)

clone <- subset(clone,clone$sifi<0)

clone$type <- factor(clone$type, levels = c("n = 640", "n = 160", "n = 80", "n = 40"))





clone_graph <- clone %>%
  ggplot( aes(x=sifi, fill=type)) +
  geom_bar( position = "stack") +
  scale_fill_manual(values = c("n = 40" = "#fef0d9", "n = 80" = "#fdcc8a", "n = 160" = "#fc8d59", "n = 640" = "#d7301f")) +
  #theme_ipsum() +
  labs(fill="", title = "Clone Ramdom SIFI")+   ylab("count") + xlab("SIFI") + 
  scale_x_continuous(breaks = seq(min(clone$sifi), max(clone$sifi), by = 50),
                     labels = seq(min(clone$sifi), max(clone$sifi), by = 50)) + 
  annotate("text", x = -200, y =10,label ="SIFI not reached : \n n=40 : 97.6%  \n n=80 : 97.3% \n n=160 : 97.3% \n n=640 : 97.2% ",  vjust = -0.4, color = "black", fontface = "bold", size=3) + theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color =alpha("lightgrey",0.2), size = 0.5),
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(size = 11),   # Increase x-axis label text size
    axis.text.y = element_text(size = 11),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 10),
    text = element_text(size = 12),
    panel.border = element_blank()  )




##delete

H0_scenario1a_remove_random_40 <- t(H0_scenario1a_remove_random_40)
H0_scenario1a_remove_random_40 <- as.data.frame(H0_scenario1a_remove_random_40)

delete_40 <- subset(H0_scenario1a_remove_random_40, as.numeric(H0_scenario1a_remove_random_40$V4)>0.05)
length(delete_40$V1)
sum(is.na(delete_40$V1)) / length(delete_40$V1)


H0_scenario1a_remove_random_80 <- t(H0_scenario1a_remove_random_80)
H0_scenario1a_remove_random_80 <- as.data.frame(H0_scenario1a_remove_random_80)
delete_80  <- subset(H0_scenario1a_remove_random_80, as.numeric(H0_scenario1a_remove_random_80$V4)>0.05)
length(delete_80$V1)
sum(is.na(delete_80$V1)) / length(delete_80$V1)


H0_scenario1a_remove_random_160 <- t(H0_scenario1a_remove_random_160)
H0_scenario1a_remove_random_160 <- as.data.frame(H0_scenario1a_remove_random_160)
delete_160  <- subset(H0_scenario1a_remove_random_160, as.numeric(H0_scenario1a_remove_random_160$V4)>0.05)
length(delete_160$V1)
sum(is.na(delete_160$V1)) / length(delete_160$V1)

H0_scenario1a_remove_random_640 <- t(H0_scenario1a_remove_random_640)
H0_scenario1a_remove_random_640 <- as.data.frame(H0_scenario1a_remove_random_640)
H0_scenario1a_remove_random_640 <- H0_scenario1a_remove_random_640[,1:5]
delete_640  <- subset(H0_scenario1a_remove_random_640,as.numeric(H0_scenario1a_remove_random_640$V4)>0.05)
length(delete_640$V1)
sum(is.na(delete_640$V1)) / length(delete_640$V1)

delete <- data.frame(
  type = c( rep("n = 40", 10000), rep("n = 80", 10000),rep("n = 160", 10000), rep("n = 640", 10000) ),
  sifi = c(H0_scenario1a_remove_random_40$V1,H0_scenario1a_remove_random_80$V1,H0_scenario1a_remove_random_160$V1,H0_scenario1a_remove_random_640$V1)
) 

delete$sifi <- as.numeric(delete$sifi)
delete$type <- as.factor(delete$type)

delete <- subset(delete,delete$sifi<0)

delete$type <- factor(delete$type, levels = c("n = 640", "n = 160", "n = 80", "n = 40"))




delete_graph <- delete %>%
  ggplot( aes(x=sifi, fill=type)) +
  geom_bar( position = "stack") +
  scale_fill_manual(values = c("n = 40" = "#fef0d9", "n = 80" = "#fdcc8a", "n = 160" = "#fc8d59", "n = 640" = "#d7301f")) +
  #theme_ipsum() +
  labs(fill="", title = "Clone Ramdom SIFI")+   ylab("count") + xlab("SIFI") + 
  scale_x_continuous(breaks = seq(min(delete$sifi), max(delete$sifi), by = 50),
                     labels = seq(min(delete$sifi), max(delete$sifi), by = 50)) + 
  annotate("text", x = -200, y =10,label ="SIFI not reached : \n n=40 : 66.5%  \n n=80 : 59.4% \n n=160 : 56.3% \n n=640 : 49.3% ",  vjust = -0.4, color = "black", fontface = "bold", size=3) + theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color =alpha("lightgrey",0.2), size = 0.5),
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(size = 11),   # Increase x-axis label text size
    axis.text.y = element_text(size = 11),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 10),
    text = element_text(size = 12),
    panel.border = element_blank()  )

grid.arrange(flip,clone_graph,delete_graph)










