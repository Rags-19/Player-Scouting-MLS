
setwd("C:/Users/Rahul moorthy.rahul-PC/Desktop/Soccer analysis/Nashville SC Internship")
library(readxl)
install.packages("ggsoccer")
library(ggsoccer)
library(ggplot2)
library(dplyr)

ggplot() +
  annotate_pitch() +
  theme_pitch()

#Importing Tracking data
tracking_data = read_excel("Tracking_data_1.xlsx")



#Visualization of Pass Locations

##Filtering Passes of Nashville SC

pass_data_Nashville = filter(tracking_data, tracking_data$teamId == "15154" & tracking_data$markingType == "pass")
attach(pass_data_Nashville)

pass_data_Nashville$location.x = as.integer(pass_data_Nashville$location.x)
pass_data_Nashville$location.y = as.integer(pass_data_Nashville$location.y)



###Plotting Pass Locations - NashVille SC

pass_data_Nashville$pass.completion = ifelse( outcome == "1", "completed", "incomplete")

pass_plot = ggplot(pass_data_Nashville, aes(location.x, location.y, colour = pass.completion)) +
  annotate_pitch() +
  theme_pitch() +
  direction_label() +
  ggtitle("Pass Locations", 
          "Nashville SC")

pass_plot + geom_point(aes(shape = direction ), size = 4) 

###Plotting Forward passes - Nashville SC
Frwd_pass_data_Nashville =  filter(pass_data_Nashville, pass_data_Nashville$direction == "forward")
attach(Frwd_pass_data_Nashville)


Frwd_pass_data_Nashville$pass.difficulty.rate = ifelse(passDifficulty>0.9, ">0.9", ifelse(passDifficulty>=0.7, "0.7 to 0.9", ifelse(passDifficulty>=0.5, "0.5 to 0.7", ifelse(passDifficulty>=0.3, "0.3 to 0.5","< 0.3"))))
                  
Frwd_pass_plot = ggplot(Frwd_pass_data_Nashville, aes(location.x, location.y)) +
  annotate_pitch() +
  theme_pitch() +
  direction_label() +
  ggtitle("Forward Passes", 
          "Nashville SC")

Frwd_pass_plot +  geom_point(aes(colour = pass.difficulty.rate),size = 3.5) 




##Filtering Passes of Portland Timbers

pass_data_Opp = filter(tracking_data, tracking_data$teamId == "1581" & tracking_data$markingType == "pass")

attach(pass_data_Opp)

pass_data_Opp$location.x = as.numeric(pass_data_Opp$location.x)
pass_data_Opp$location.y = as.numeric(pass_data_Opp$location.y)


###Plotting Pass Locations - Portland Timbers

pass_data_Opp$pass.completion.opp = ifelse( outcome == "1", "completed", "incomplete")

pass_plot_opp = ggplot(pass_data_Opp, aes(location.x, location.y, colour = pass.completion.opp)) +
  annotate_pitch() +
  theme_pitch() +
  direction_label() +
  ggtitle("Pass Locations", 
          "Portland Timbers")

pass_plot_opp + geom_point(aes(shape = direction), size = 4)


###Plotting Forward passes - Portland Timbers
Frwd_pass_data_Opp =  filter(pass_data_Opp, pass_data_Opp$direction == "forward")
attach(Frwd_pass_data_Opp)


Frwd_pass_data_Opp$pass.difficulty.rate.opp = ifelse(Frwd_pass_data_Opp$passDifficulty>0.9, ">0.9", ifelse(Frwd_pass_data_Opp$passDifficulty>=0.7, "0.7 to 0.9", ifelse(Frwd_pass_data_Opp$passDifficulty>=0.5, "0.5 to 0.7", ifelse(Frwd_pass_data_Opp$passDifficulty>=0.3, "0.3 to 0.5","< 0.3"))))

Frwd_pass_plot_opp = ggplot(Frwd_pass_data_Opp, aes(Frwd_pass_data_Opp$location.x,Frwd_pass_data_Opp$location.y)) +
  annotate_pitch() +
  theme_pitch() +
  direction_label() +
  ggtitle("Forward Passes", 
          "Portland Timbers")

Frwd_pass_plot_opp +  geom_point(aes(colour = pass.difficulty.rate.opp ),size = 3)



#Visualization of Shot Locations

##Filtering Shots of Nashville SC

Shot_data_Nashville = filter(tracking_data, tracking_data$teamId == "15154" & tracking_data$markingType == "shot")
attach(Shot_data_Nashville)

Shot_data_Nashville$location.x = as.numeric(Shot_data_Nashville$location.x)
Shot_data_Nashville$location.y = as.numeric(Shot_data_Nashville$location.y)


###Plotting Shot Locations - Nashville SC

shot_plot_nashville = ggplot(Shot_data_Nashville, aes(location.x, location.y)) +
  annotate_pitch() +
  theme_pitch() +
  direction_label() +
  ggtitle("Shot Locations", 
          "Nashville SC")

shot_plot_nashville + geom_point(aes(size = ShotsxG))



###Plotting Shot Distances - Nashville SC

shot_dist_plot_nashville <- ggplot(Shot_data_Nashville, aes(location.x, location.y, colour = goalDist)) + 
  annotate_pitch() +
  theme_pitch() +
  direction_label() +
  ggtitle("Shot Distances from Goal", 
          "Nashville SC")

shot_dist_plot_nashville + geom_point(aes(size = goalDist))



##Filtering Shots of Portland Timbers
 
Shot_data_Opp = filter(tracking_data, tracking_data$teamId == "1581" & tracking_data$markingType == "shot")

attach(Shot_data_Opp)

Shot_data_Opp$location.x = as.numeric(Shot_data_Opp$location.x)
Shot_data_Opp$location.y = as.numeric(Shot_data_Opp$location.y)

shot_plot_opp = ggplot(Shot_data_Opp, aes(location.x, location.y)) +
  annotate_pitch() +
  theme_pitch() +
  direction_label() +
  ggtitle("Shot Locations", 
          "Portland Timbers")

shot_plot_opp + geom_point(aes(size = ShotsxG))


###Plotting Shot Distances - Portland Timbers


shot_dist_plot_opp <- ggplot(Shot_data_Opp, aes(location.x, location.y, colour = goalDist)) + 
  annotate_pitch() +
  theme_pitch() +
  direction_label() +
  ggtitle("Shot Distances from Goal", 
          "Portland Timbers")

shot_dist_plot_opp + geom_point(aes(size = goalDist))
                           