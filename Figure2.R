#### Sample Size SIFI 
# n = 40 to 640 

#Read files 

library(here)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(hrbrthemes)


source_dir <- "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Figures/Figure 2/SIFI_n_40_640"

files_names <- list.files(source_dir, pattern = "\\.csv$")
nb_files <- length(files_names)

data_names <- vector("list", length = nb_files)

for (i in 1:nb_files) {
  file_path <- file.path(source_dir, files_names[i]) 
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  data <- t(data)
  data <- as.data.frame(data)
  
  data_sub <- subset(data, data$V1 < 0)
  data_sub$V1 <- as.factor(data_sub$V1)
  
  #New modified file
  modified_file_name <- paste("modified_", files_names[i], sep = "")
  modified_file_path <- file.path(source_dir, modified_file_name)
  
  write.csv(data_sub, modified_file_path, row.names = FALSE)
}




data_list <- list()

modified_files <-  list.files(source_dir, pattern = "^modified_")


#Read all files modified 

for (file_name in modified_files) {
  #read file
  file_path <- file.path(source_dir, file_name)
    data <- read.csv(file_path, stringsAsFactors = FALSE)  #Extract specific name to identify type and file
  type <- gsub("modified_", "", file_name)
  type <- gsub(".csv", "", type)
  #Create a dynamic variable name
  variable_name <- paste(type, "", sep = "")
  #Assign data to a variable in the global environment
  assign(variable_name, data)
}


####bases 

H0_scenario1a_best_flip_40_data <- t(H0_scenario1a_best_flip_40)
H0_scenario1a_best_flip_40_data <- as.data.frame(H0_scenario1a_best_flip_40_data)
H0_scenario1a_best_flip_40_data <- subset(H0_scenario1a_best_flip_40_data, H0_scenario1a_best_flip_40_data$V1 < 0)
H0_scenario1a_best_flip_40_data$V1 <- as.factor(H0_scenario1a_best_flip_40_data$V1)

H0_scenario1a_best_flip_80_data <- t(H0_scenario1a_best_flip_80)
H0_scenario1a_best_flip_80_data <- as.data.frame(H0_scenario1a_best_flip_80_data)
H0_scenario1a_best_flip_80_data <- subset(H0_scenario1a_best_flip_80_data, H0_scenario1a_best_flip_80_data$V1 < 0)
H0_scenario1a_best_flip_80_data$V1 <- as.factor(H0_scenario1a_best_flip_80_data$V1)

H0_scenario1a_best_flip_160_data <- t(H0_scenario1a_best_flip_160)
H0_scenario1a_best_flip_160_data <- as.data.frame(H0_scenario1a_best_flip_160_data)
H0_scenario1a_best_flip_160_data <- subset(H0_scenario1a_best_flip_160_data, H0_scenario1a_best_flip_160_data$V1 < 0)
H0_scenario1a_best_flip_160_data$V1 <- as.factor(H0_scenario1a_best_flip_160_data$V1)

H0_scenario1a_best_flip_640_data <- t(H0_scenario1a_best_flip_640)
H0_scenario1a_best_flip_640_data <- as.data.frame(H0_scenario1a_best_flip_640_data)
H0_scenario1a_best_flip_640_data <- subset(H0_scenario1a_best_flip_640_data, H0_scenario1a_best_flip_640_data$V1 < 0)
H0_scenario1a_best_flip_640_data$V1 <- as.factor(H0_scenario1a_best_flip_640_data$V1)

best_flip$sifi <- as.numeric(best_flip$sifi)


best_flip <- data.frame(
  type = c( rep("n = 40", nrow(H0_scenario1a_best_flip_40_data)), rep("n = 80", nrow(H0_scenario1a_best_flip_80_data)),rep("n = 160", nrow(H0_scenario1a_best_flip_160_data)), rep("n = 640", nrow(H0_scenario1a_best_flip_640_data)) ),
  sifi = c(H0_scenario1a_best_flip_40_data$V1,H0_scenario1a_best_flip_80_data$V1,H0_scenario1a_best_flip_160_data$V1,H0_scenario1a_best_flip_640_data$V1)
) 



best_clone <- data.frame(
  type = c( rep("n = 40", nrow(H0_scenario1a_best_clone_40_data)), rep("n = 80", nrow(H0_scenario1a_best_clone_80_data)),rep("n = 160", nrow(H0_scenario1a_best_clone_160_data)), rep("n = 640", nrow(H0_scenario1a_best_clone_640_data)) ),
  sifi = c(H0_scenario1a_best_clone_40_data$V1,H0_scenario1a_best_clone_80_data$V1,H0_scenario1a_best_clone_160_data$V1,H0_scenario1a_best_clone_640_data$V1)
) 


best_remove <- data.frame(
  type = c( rep("n = 40", nrow(H0_scenario1a_best_remove_40_data)), rep("n = 80", nrow(H0_scenario1a_best_remove_80_data)),rep("n = 160", nrow(H0_scenario1a_best_remove_160_data)), rep("n = 640", nrow(H0_scenario1a_best_remove_640_data)) ),
  sifi = c(H0_scenario1a_best_remove_40_data$V1,H0_scenario1a_best_remove_80_data$V1,H0_scenario1a_best_remove_160_data$V1 ,H0_scenario1a_best_remove_640_data$V1)
) 

worst_flip <- data.frame(
  type = c( rep("n = 40", nrow(H0_scenario1a_worst_flip_40_data)), rep("n = 80", nrow(H0_scenario1a_worst_flip_80_data)),rep("n = 160", nrow(H0_scenario1a_worst_flip_160_data)), rep("n = 640", nrow(H0_scenario1a_worst_flip_640_data)) ),
  sifi = c(H0_scenario1a_worst_flip_40_data$V1,H0_scenario1a_worst_flip_80_data$V1,H0_scenario1a_worst_flip_160_data$V1,H0_scenario1a_worst_flip_640_data$V1)
) 

worst_clone <- data.frame(
  type = c( rep("n = 40", nrow(H0_scenario1a_worst_clone_40_data)), rep("n = 80", nrow(H0_scenario1a_worst_clone_80_data)),rep("n = 160", nrow(H0_scenario1a_worst_clone_160_data)), rep("n = 640", nrow(H0_scenario1a_worst_clone_640_data)) ),
  sifi = c(H0_scenario1a_worst_clone_40_data$V1,H0_scenario1a_worst_clone_80_data$V1,H0_scenario1a_worst_clone_160_data$V1,H0_scenario1a_worst_clone_640_data$V1)
) 


worst_remove <- data.frame(
  type = c( rep("n = 40", nrow(H0_scenario1a_worst_remove_40_data)), rep("n = 80", nrow(H0_scenario1a_worst_remove_80_data)),rep("n = 160", nrow(H0_scenario1a_worst_remove_160_data)), rep("n = 640", nrow(H0_scenario1a_worst_remove_640_data)) ),
  sifi = c(H0_scenario1a_worst_remove_40_data$V1,H0_scenario1a_worst_remove_80_data$V1,H0_scenario1a_worst_remove_160_data$V1,H0_scenario1a_worst_remove_640_data$V1)
) 


####graph 

couleurs <- c("#fef0d9","#fdcc8a","#fc8d59","#d7301f")

#couleurs <- rev(hcl.colors(4,palette = "Oranges"))

#best_flip_graph <- best_flip %>%
 # ggplot( aes(x=sifi, fill=type)) +
 # geom_histogram( position = "identity",color="#e9ecef", alpha=0.6) +
#  scale_fill_manual(values=couleurs,breaks = c("n = 40", "n = 80", "n = 160", "n = 640")) +
#  theme_ipsum() +
 # labs(fill="", title = "Best flip Control -> Experimental ") + 
#  scale_x_continuous(breaks = seq(min(best_flip$sifi), max(best_flip$sifi), by = 1),
                     # labels = seq(min(best_flip$sifi), max(best_flip$sifi), by = 1)) + theme(
                     #   axis.text.x = element_text(size = 11),   # Increase x-axis label text size
                     #   axis.text.y = element_text(size = 11),
                     #   legend.text = element_text(size = 12),
                     #   plot.title = element_text(size = 12)
                     # )

best_flip$type <- factor(best_flip$type, levels = c("n = 640", "n = 160", "n = 80", "n = 40"))
best_flip$sifi <- as.numeric(best_flip$sifi)
best_flip$sifi <- abs(as.numeric(best_flip$sifi))

class(best_flip$sifi)
best_flip$sifi <- as.integer(as.character(best_flip$sifi))
best_flip$sifi <- abs(best_flip$sifi)


table(best_flip$sifi)

best_flip_graph <- best_flip %>% ggplot( aes(x=sifi, fill=type)) +
  geom_bar( position = "stack") +
  scale_fill_manual(values = c("n = 40" = "#fef0d9", "n = 80" = "#fdcc8a", "n = 160" = "#fc8d59", "n = 640" = "#d7301f")) +
  theme_ipsum() +
  labs(fill="", title = "Best flip from Control to Experimental ")+ 
  scale_x_continuous(breaks = seq(min(best_clone$sifi), max(best_clone$sifi), by = 2),
                     labels = seq(min(best_clone$sifi), max(best_clone$sifi), by = 2)) + theme(
                       axis.text.x = element_text(size = 11),   # Increase x-axis label text size
                       axis.text.y = element_text(size = 11),
                       legend.text = element_text(size = 12),
                       plot.title = element_text(size = 12)
                     )


best_clone$type <- factor(best_clone$type, levels = c("n = 640", "n = 160", "n = 80", "n = 40"))


best_clone_graph <- best_clone %>%
  ggplot( aes(x=sifi, fill=type)) +
  geom_bar( position = "stack") +
  scale_fill_manual(values = c("n = 40" = "#fef0d9", "n = 80" = "#fdcc8a", "n = 160" = "#fc8d59", "n = 640" = "#d7301f")) +
  theme_ipsum() +
  labs(fill="", title = "Best clone from Control to Experimental ")+ 
  scale_x_continuous(breaks = seq(min(best_clone$sifi), max(best_clone$sifi), by = 2),
                     labels = seq(min(best_clone$sifi), max(best_clone$sifi), by = 2)) + theme(
                       axis.text.x = element_text(size = 11),   # Increase x-axis label text size
                       axis.text.y = element_text(size = 11),
                       legend.text = element_text(size = 12),
                       plot.title = element_text(size = 12)
                     )



best_remove$type <- factor(best_remove$type, levels = c("n = 640", "n = 160", "n = 80", "n = 40"))


best_remove_graph <- best_remove %>%
  ggplot( aes(x=sifi, fill=type)) +
  geom_bar( position = "stack") +
  scale_fill_manual(values = c("n = 40" = "#fef0d9", "n = 80" = "#fdcc8a", "n = 160" = "#fc8d59", "n = 640" = "#d7301f")) +
  theme_ipsum() +
  labs(fill="", title = "Best delete from Control to Experimental ")+ 
  scale_x_continuous(breaks = seq(min(best_remove$sifi), max(best_remove$sifi), by = 2),
                     labels = seq(min(best_remove$sifi), max(best_remove$sifi), by = 2)) + theme(
                       axis.text.x = element_text(size = 11),   # Increase x-axis label text size
                       axis.text.y = element_text(size = 11),
                       legend.text = element_text(size = 12),
                       plot.title = element_text(size = 12)
)


worst_flip$type <- factor(worst_flip$type, levels = c("n = 640", "n = 160", "n = 80", "n = 40"))



worst_flip_graph <- worst_flip %>%
  ggplot( aes(x=sifi, fill=type)) +
  geom_bar( position = "stack") +
  scale_fill_manual(values = c("n = 40" = "#fef0d9", "n = 80" = "#fdcc8a", "n = 160" = "#fc8d59", "n = 640" = "#d7301f")) +
  theme_ipsum() +
  labs(fill="", title = "Worst flip from Experimental to Control ")+ 
  scale_x_continuous(breaks = seq(min(worst_flip$sifi), max(worst_flip$sifi), by = 3),
                     labels = seq(min(worst_flip$sifi), max(worst_flip$sifi), by = 3)) + theme(
                       axis.text.x = element_text(size = 11),   # Increase x-axis label text size
                       axis.text.y = element_text(size = 11),
                       legend.text = element_text(size = 12),
                       plot.title = element_text(size = 12)
                     )


worst_clone$type <- factor(worst_clone$type, levels = c("n = 640", "n = 160", "n = 80", "n = 40"))


worst_clone_graph <- worst_clone %>%
  ggplot( aes(x=sifi, fill=type)) +
  geom_bar( position = "stack") +
  scale_fill_manual(values = c("n = 40" = "#fef0d9", "n = 80" = "#fdcc8a", "n = 160" = "#fc8d59", "n = 640" = "#d7301f")) +
  theme_ipsum() +
  labs(fill="", title = "Worst clone from Experimental to Control ")+ 
  scale_x_continuous(breaks = seq(min(worst_clone$sifi), max(worst_clone$sifi), by = 8),
                     labels = seq(min(worst_clone$sifi), max(worst_clone$sifi), by = 8)) + theme(
                       axis.text.x = element_text(size = 11),   # Increase x-axis label text size
                       axis.text.y = element_text(size = 11),
                       legend.text = element_text(size = 12),
                       plot.title = element_text(size = 12)
                     )


worst_remove$type <- factor(worst_remove$type, levels = c("n = 640", "n = 160", "n = 80", "n = 40"))



worst_remove_graph <- worst_remove %>%
  ggplot( aes(x=sifi, fill=type)) +
  geom_bar( position = "stack") +
  scale_fill_manual(values = c("n = 40" = "#fef0d9", "n = 80" = "#fdcc8a", "n = 160" = "#fc8d59", "n = 640" = "#d7301f")) +
  theme_ipsum() +
  labs(fill="", title = "Worst delete from Experimental to Control ")+ 
  scale_x_continuous(breaks = seq(min(worst_remove$sifi), max(worst_remove$sifi), by = 8),
                     labels = seq(min(worst_remove$sifi), max(worst_remove$sifi), by = 8)) + theme(
                       axis.text.x = element_text(size = 11),   # Increase x-axis label text size
                       axis.text.y = element_text(size = 11),
                       legend.text = element_text(size = 12),
                       plot.title = element_text(size = 12)
                     )



grid.arrange(best_flip_graph,best_clone_graph, best_remove_graph, worst_flip_graph,worst_clone_graph, worst_remove_graph, ncol=3, nrow=2)



