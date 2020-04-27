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
Target.Positions = c("Centre Forward","Second Striker")
Scouting_Players_CF = filter(Scouting_Players_database, Scouting_Players_database$Position %in% Target.Positions & Scouting_Players_database$team %in% Target.teams)


#Exploratory Data Analysis
dim(Scouting_Players_CF)
head(Scouting_Players_CF,10)
Scouting_Players_CF$team = as.factor(Scouting_Players_CF$team)
Scouting_Players_CF$league = as.factor(Scouting_Players_CF$league)
Scouting_Players_CF$season = as.factor(Scouting_Players_CF$season)
Scouting_Players_CF$`1v1%`= as.numeric(Scouting_Players_CF$`1v1%`)
str(Scouting_Players_CF)
anyNA(Scouting_Players_CF)


#Clustering - To group Players with similar playing styles
##Scaling dataset
Scouting_Players_CF_scaled = scale(Scouting_Players_CF[,c(5,16:30)])                                                                                                                            


seed=1000
set.seed(seed)

Initial_Cluster = kmeans(x=Scouting_Players_CF_scaled, centers = 3, nstart = 5)
print(Initial_Cluster)                             


##Identifying current no. of clusters
set.seed(seed)
nc = NbClust(Scouting_Players_CF[,c(5,16:30)], min.nc = 3, max.nc = 5, method = "kmeans")


##Final Clustering
Cluster_final = kmeans(x=Scouting_Players_CF_scaled, centers = 4, nstart = 25)
print(Cluster_final)
clusplot(Scouting_Players_CF_scaled, Cluster_final$cluster, color = TRUE, shade = TRUE, label = 2, lines = 1)


##Aggregating Clusters
Scouting_Players_CF$Cluster = Cluster_final$cluster
CF.PlayingStyle = aggregate(Scouting_Players_CF[,c(5,16:30)],list(Scouting_Players_CF$Cluster),FUN = "mean")
print(CF.PlayingStyle)


#Validating clusters
sil <- silhouette(Cluster_final$cluster, dist(Scouting_Players_CF_scaled))
fviz_silhouette(sil)

#Exporting file
Clusters.CF = write.xlsx(CF.PlayingStyle, file = "Clusters_CF.xlsx", row.names = TRUE, append = FALSE)
Scouting_Players_CF = write.xlsx(Scouting_Players_CF, file = "Scouting_Players_CF.xlsx", row.names = TRUE, append = FALSE)
