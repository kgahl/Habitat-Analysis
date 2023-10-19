# BB WC SS RU 
# Loop for resource selection function
# HSC using loop generated data 

# Keep in mind the rule-of-thumb of 10 data points (probably 10 locations in your case) per covariate in your model

#ONLY 2 OBSERVED FISH

library(dplyr)
library(readr)


##### DATA #####

NotAsFishy <- read_csv("~/School/Bonar Lab/Project/Data/Analysis/Data Ready for HSC/Data-Ready-for-HSC/Output/NotAsFishy.csv", 
                       col_types = cols(Velocity = col_double()))

##### BB_WC_SS_RU #####
##Black Bass West Beaver Creek Spawning (Dec- Jan) Run Data 

#Combine all Black Bass and available data on WC <----- BB_AV_WB
BB_WB <- NotAsFishy %>%                                                       #separate Beaver Creek Black Bass
  filter(Stream %in% c('Beaver'))%>%          
  filter(Species == 'Black Bass')      
AV_WB <- NotAsFishy %>%                                                       #separate Beaver Creek Available
  filter(Stream %in% c('Beaver'))%>%          
  filter(Presence == 0)                                                       # 0 = Available
BB_AV_WB <- rbind(BB_WB, AV_WB)                                               #combine BB and available
remove(BB_WB, AV_WB)

#Only Spawning period- April - July and Runs                                  #[2 Black Bass observations, 22  available]
BB_WC_SS_RU <- BB_AV_WB %>%
  filter(Month %in% c('April', 'May', 'June', 'July')) %>%
  filter(Mesohabitat == 'Run')                                                  
remove(BB_AV_WB)


##Number of Unique Individuals Observed
Unique_Fish <- na.omit(unique(BB_WC_SS_RU$Tag))                               #Gives number of individuals- doesn't count the same fish more than once

Number_Of_Individuals <- length(Unique_Fish[!is.na(Unique_Fish)])             #removes NA 
length(Unique_Fish)  #2



##Find Min and Max of variables used by fish
Counts <- BB_WC_SS_RU %>%
  filter(Species == 'Black Bass')

Velocity_Min <- min(Counts$Velocity, na.rm = TRUE)     
Velocity_Max  <- max(Counts$Velocity, na.rm = TRUE)   

Depth_Min <- min(Counts$Depth, na.rm = TRUE)   #
Depth_Max <- max(Counts$Depth, na.rm = TRUE)   #

Substrate_Min  <- min(Counts$Substrate, na.rm = TRUE)   #
Substrate_Max  <- max(Counts$Substrate, na.rm = TRUE)   # 

MinMax <- data.frame(DataSet = "BB_WC_SS_RU", Velocity_Min, Velocity_Max, Depth_Min, 
                     Depth_Max, Substrate_Min, Substrate_Max)
