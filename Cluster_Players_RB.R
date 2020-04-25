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
Scouting_Players_database = read_excel("Interns_Players.xlsx")


#Filtering players based on Position (CF) & Teams with Similar Playing Style to SJ Earthquakes
Target.teams = c("Toronto","Louisville City","Los Angeles","El Paso Locomotive","Atlanta United","Indy Eleven","Seattle Sounders","New York City","Philadelphia Union","Real Monarchs","Dallas","SJ Earthquakes","LA Galaxy","Phoenix Rising","Reno 1868","New York RB II","Portland Timbers","Nashville","San Antonio","Salzburg")
Scouting_Players_RB = filter(Scouting_Players_database, Scouting_Players_database$Position == "Right Back" & Scouting_Players_database$team %in% Target.teams)


#Exploratory Data Analysis
dim(Scouting_Players_RB)
head(Scouting_Players_RB,10)
Scouting_Players_RB$team = as.factor(Scouting_Players_RB$team)
Scouting_Players_RB$league = as.factor(Scouting_Players_RB$league)
Scouting_Players_RB$season = as.factor(Scouting_Players_RB$season)
Scouting_Players_RB$`1v1%`= as.numeric(Scouting_Players_RB$`1v1%`)
str(Scouting_Players_RB)
anyNA(Scouting_Players_RB)


#Clustering - To group Players with similar playing styles
##Scaling dataset
Scouting_Players_RB_scaled = scale(Scouting_Players_RB[,c(5,6,16:30)])                                                                                                                            


seed=1000
set.seed(seed)

Initial_Cluster = kmeans(x=Scouting_Players_RB_scaled, centers = 3, nstart = 5)
print(Initial_Cluster)                             


##Identifying current no. of clusters
set.seed(seed)
nc = NbClust(Scouting_Players_RB[,c(5,6,16:30)], min.nc = 3, max.nc = 5, method = "kmeans")


##Final Clustering
Cluster_final = kmeans(x=Scouting_Players_RB_scaled, centers = 3, nstart = 25)
print(Cluster_final)
clusplot(Scouting_Players_RB_scaled, Cluster_final$cluster, color = TRUE, shade = TRUE, label = 2, lines = 1)


##Aggregating Clusters
Scouting_Players_RB$Cluster = Cluster_final$cluster
RB.PlayingStyle = aggregate(Scouting_Players_RB[,c(5,6,16:30)],list(Scouting_Players_RB$Cluster),FUN = "mean")
print(RB.PlayingStyle)


#Validating clusters
sil <- silhouette(Cluster_final$cluster, dist(Scouting_Players_RB_scaled))
fviz_silhouette(sil)

#Exporting file
RB.Clusters = write.xlsx(RB.PlayingStyle, file = "RB_Cluisters.xlsx", row.names = TRUE, append = FALSE)
Scouting_Players_RB = write.xlsx(Scouting_Players_RB, file = "Scouting_Players_RB.xlsx", row.names = TRUE, append = FALSE)
