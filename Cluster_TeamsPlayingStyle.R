#Setting working Directory
setwd("C:/Users/Rahul moorthy.rahul-PC/Desktop/Soccer analysis/Nashville SC Internship")
getwd()

#Installing libraries
library(readxl)
install.packages("cluster")
library(cluster)
install.packages("NbClust")
library(NbClust)
install.packages("dplyr")
library(dplyr)
library(xlsx)
install.packages("factoextra")
library(factoextra)


#Importing dataset 
MLS_Teams = read_excel("Interns_Teams.xlsx")


#Exploratory Data Analysis
dim(MLS_Teams)
head(MLS_Teams,10)
MLS_Teams$team = as.factor(MLS_Teams$team)
str(MLS_Teams)
anyNA(MLS_Teams)

#Clustering - To group teams with similar playing styles
##Scaling dataset
MLS_Teams_scaled = scale(MLS_Teams[,-c(1:13)])

seed=1000
set.seed(seed)

Initial_Cluster = kmeans(x=MLS_Teams_scaled, centers = 2, nstart = 5)
print(Initial_Cluster)

##Identifying current no. of clusters
set.seed(seed)
nc = NbClust(MLS_Teams[,-c(1:13)], min.nc = 3, max.nc = 5, method = "kmeans")

##Final Clustering
Cluster_final = kmeans(x=MLS_Teams_scaled, centers = 4, nstart = 5)
print(Cluster_final)

clusplot(MLS_Teams_scaled, Cluster_final$cluster, color = TRUE, shade = TRUE, label = 2, lines = 1)



##Aggregating Clusters
MLS_Teams$Cluster = Cluster_final$cluster
MLS.Teams.PlayingStyle = aggregate(MLS_Teams[,-c(1:13)],list(MLS_Teams$Cluster),FUN = "mean")
print(MLS.Teams.PlayingStyle)

#Validating clusters
sil <- silhouette(Cluster_final$cluster, dist(MLS_Teams_scaled))
fviz_silhouette(sil)


#Exporting file
MLS_Teams_Playingstyle = write.xlsx(MLS.Teams.PlayingStyle, file = "MLS_teams_Playing style1.xlsx", row.names = TRUE, append = FALSE)
MLS_Teams.final = write.xlsx(MLS_Teams, file = "MLS_teams.final1.xlsx", row.names = TRUE, append = FALSE)
