####### SIFI and P-value ######

library(gridExtra)
library(readr)

#List of file names in a specific directory : if you want the original data
# file_list <- list.files(path = "/Users/roxanecouturier/Desktop/Doctorat/SIFI/Figures/Figure 2/Data_fig2", pattern = ".csv", full.names = TRUE) 
# # Use pattern to filter by extension or other criteria if necessary
# 
# #Loop to iterate through each file
# #keep only the data from non-significant trials (exclusion of the 5% of significant trials which corresponds to the type I error)
# for (file in file_list) {
#   
#   data <- read.csv(file)
#   
#   data<- t(data)
#   data <- as.data.frame(data)
# 
#   data_sub <- subset(data, data$V1<0)
#   data_sub$V1 <- as.factor(data_sub$V1)
#   
#   #Write the modified file
#   write.csv(data_sub, file, row.names = FALSE)
# }



title = c("Best Clone Control -> Experimental","Best Flip Control -> Experimental","Best Delete Control -> Experimental",
          "Worst Clone Experimental -> Control","Worst Flip Experimental -> Control","Worst Delete Experimental -> Control")


#Create a list to store charts
liste_graphiques <- list()

#Suppose file_list is a list of files and title contains the corresponding titles
for (i in seq_along(file_list)) {
  # Lire le fichier
  data <- read.csv(file_list[i])
  
  #Create the chart with the corresponding title
  graph <- ggplot(data = data, aes(x = V4, y = as.factor(V1), group = as.factor(V1))) +
    geom_boxplot(color = "#0754A0") +
    labs(title = title[i], # Assign the corresponding title to this file
         x = "p_value",
         y = "SIFI") +
    xlim(0.005, 1) +
    theme(panel.grid = element_blank(), panel.background = element_blank()) +
    geom_vline(xintercept = 0.05, color = '#F12A29', size = 1) +
    theme(
      axis.text.x = element_text(size = 10),   # Increase x-axis label text size
      axis.text.y = element_text(size = 10)
    )
  
  #Add chart to list
  liste_graphiques[[i]] <- graph
}

##### graph ######

grid.arrange( liste_graphiques[[2]], liste_graphiques[[1]], liste_graphiques[[3]], liste_graphiques[[5]],liste_graphiques[[4]], liste_graphiques[[6]], ncol=3, nrow=2)




