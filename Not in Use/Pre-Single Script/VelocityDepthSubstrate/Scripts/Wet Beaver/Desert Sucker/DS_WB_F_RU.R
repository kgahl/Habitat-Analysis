#DS WB F RU
# Loop for resource selection function
# HSC using loop generated data 

# Keep in mind the rule-of-thumb of 10 data points (probably 10 locations in your case) per covariate in your model

#ONLY 2 OBSERVATIONS

library(dplyr)
library(readr)


##### DATA #####

NotAsFishy <- read_csv("~/School/Bonar Lab/Project/Data/Analysis/Data Ready for HSC/Data-Ready-for-HSC/Output/NotAsFishy.csv", 
                       col_types = cols(Velocity = col_double()))


##### DS_WB_F_RU #####
##Desert Sucker Wet Beaver Creek Spawning (Sept - Nov) Run Data 

#Combine all desert sucker and available data on WB <----- DS_AV_WB
DS_WB <- NotAsFishy %>%                                                       #separate Beaver Creek Desert Sucker
  filter(Stream %in% c('Beaver'))%>%          
  filter(Species == 'Desert Sucker')      
AV_WB <- NotAsFishy %>%                                                       #separate Beaver Creek Available
  filter(Stream %in% c('Beaver'))%>%          
  filter(Presence == 0)                                                       # 0 = Available
DS_AV_WB <- rbind(DS_WB, AV_WB)                                               #combine DS and available
remove(DS_WB, AV_WB)

#Only Spawning period- September - November and Riffles                        #[2 desert sucker observations, 26 available]
DS_WB_F_RU <- DS_AV_WB %>%
  filter(Month %in% c('September', 'October', 'November')) %>%
  filter(Mesohabitat == 'Run')                                                
remove(DS_AV_WB)


##Number of Unique Individuals Observed
Unique_Fish <- na.omit(unique(DS_WB_F_RU$Tag))                               #Gives number of individuals- doesn't count the same fish more than once

Number_Of_Individuals <- length(Unique_Fish[!is.na(Unique_Fish)])             #removes NA 
length(Unique_Fish)  #2



##Find Min and Max of variables used by fish
Counts <- DS_WB_F_RU %>%
  filter(Species == 'Desert Sucker')

Velocity_Min <- min(Counts$Velocity, na.rm = TRUE)     
Velocity_Max  <- max(Counts$Velocity, na.rm = TRUE)   

Depth_Min <- min(Counts$Depth, na.rm = TRUE)   #
Depth_Max <- max(Counts$Depth, na.rm = TRUE)   #

Substrate_Min  <- min(Counts$Substrate, na.rm = TRUE)   #
Substrate_Max  <- max(Counts$Substrate, na.rm = TRUE)   # 

MinMax <- data.frame(DataSet = "DS_WB_F_RU", Velocity_Min, Velocity_Max, Depth_Min, 
                     Depth_Max, Substrate_Min, Substrate_Max)

