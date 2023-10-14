


library(dplyr)
library(readr)


##### DATA #####

##Read In
#All data available
NotAsFishy <- read_csv("Output/NotAsFishy.csv")

#Read in data file with variable combinations 
Variable_Combos <- read.csv("HighLow Discharge/HighLowCombo_Variables.csv")

##Output 
#Dataframe to write to file
All_Rows <- NULL


##### SETUP #####



##Filter Data
#Occupied and available data by stream and species
Occupied <- NotAsFishy %>%                                                       
  dplyr::filter(Stream == "Beaver")%>%                                     #dplyr:: before a function lets it know to use the function from the dplyr package       
  dplyr::filter(Species == "Desert Sucker")      
Available <- NotAsFishy %>%                                                       
  dplyr::filter(Stream == "Beaver")%>%          
  dplyr::filter(Presence == 0)                                                #0 = Available
Combined <- rbind(Occupied, Available)                                        #combine occupied and available

#Then month and mesohabitat 
Select_Data <- Combined %>%                                                    
  dplyr::filter(Month %in% c("January", "February", "March", "April", "May", "June"))
  


##Count Individuals
#Number of Unique
Unique_Fish <- na.omit(unique(Select_Data$Tag))                              #doesn't count the same fish more than once (gives range of tag numbers)
Unique_Individuals <- length(Unique_Fish[!is.na(Unique_Fish)])               #the number of unique fish
#Number of Observed
Observed <- na.omit(Select_Data$Tag)                                           
Observed_Individuals <- length(Observed[!is.na(Observed)])
#Number of Available
Available <- filter(Select_Data, Presence == 0)
Available <- na.omit(Select_Data$Presence)
Available <- length(Available[!is.na(Available)])



##### LOOP SETUP #####

##Create data frame to store coefficients
Prop_Sample <- 0.75                                                           #specify the proportion of point resampled (75%)  

N_reps <- 100

Storage <- data.frame(Ratio_Pool = NA, Ratio_Riffle = NA, Ratio_Run = NA, 
                      Mesohabitat_Riffle = NA, Mesohabitat_Run = NA)



##### RESOURCE SELECTION LOOP #####

for(i in 1:N_reps){                                                             #do number of reps specified above
  
  Used_Data_i <- NULL                                                         #create empty data frame
  
  if(i %in% seq(1, N_reps, by = 100)) cat("Starting rep", i, "\n")            #Shows progress of loop in output
  
  ##Get used data      
  #Randomly select 75% of tags
  j_Individuals <- as.vector(na.omit(unique(Select_Data$Tag)))[sample
                                                               (1:Unique_Individuals,
                                                                 floor(Unique_Individuals*Prop_Sample))]        #selection of tags to choose from (within the tags of Select_Data dataframe, choose 1 unique individual and add it to the list until 75% of individuals are sampled. floor grabs largest number (brackets give you exact tag numbers)
  
  #Select one observation for each tag
  for(j in j_Individuals){                                                       
    Fish_j <- j                                                              #'j' is the actual tag number of a fish
    Fish_js_Rows <- Select_Data[which(Select_Data$Tag==Fish_j),]             #grabs Fish j's rows of data
    
    #Select the row to resample from fish j's data (i.e., data_j)
    j_Selected_Row <- sample(seq(1, nrow(Fish_js_Rows)), 1, replace = T)     #randomly selects 1 of multiple observations of fish j
    j_Row_Data <- Fish_js_Rows[j_Selected_Row,]                              #pulls out the row of data for the above selected fish           
    
    #Place this randomly sampled row into used_Data_i
    Used_Data_i <- rbind(Used_Data_i, j_Row_Data)
  }
  
  
  ##Get available data                                                            #Consider all of the available points within a species/site/Group/mesohabitat to be equally available across the entire season. Use the same set of available points for every random sample of used point
  All_Avail_Data <- Select_Data[which(Select_Data$Presence == 0), ]              #Separate available data
  Avail_Data <- All_Avail_Data[sample(1:nrow(All_Avail_Data),                    #Randomly select Prop_Sample (75) percent of available points
                                      floor(nrow(All_Avail_Data)*Prop_Sample)),]   
  
  ##Combine available with loop-created used
  Data_i <- rbind(Used_Data_i, Avail_Data)                                         
  

##### RESOURCE RATIOS #####
#For analysis of Mesohabitat 


#Pool
Avail_Pool <- ifelse(Avail_Data$Mesohabitat == 'Pool', (na.omit(Avail_Data$StreamWidth)),0)
Avail_Prob_Pool <- (sum(na.omit(Avail_Pool)/sum(!is.na(Avail_Data$StreamWidth))))     
Used_Pool <- ifelse(Used_Data_i$Mesohabitat == 'Pool', (na.omit(Used_Data_i$StreamWidth)),0)
Used_Prob_Pool<- sum(na.omit(Used_Pool)/sum(!is.na(Used_Data_i$StreamWidth)))
Storage$Ratio_Pool[i] <- Used_Prob_Pool/Avail_Prob_Pool

#Riffle
Avail_Riffle <- ifelse(Avail_Data$Mesohabitat == 'Riffle', (na.omit(Avail_Data$StreamWidth)),0)
Avail_Prob_Riffle <- (sum(na.omit(Avail_Riffle)/sum(!is.na(Avail_Data$StreamWidth))))     
Used_Riffle <- ifelse(Used_Data_i$Mesohabitat == 'Riffle', (na.omit(Used_Data_i$StreamWidth)),0)
Used_Prob_Riffle<- sum(na.omit(Used_Riffle)/sum(!is.na(Used_Data_i$StreamWidth)))
Storage$Ratio_Riffle[i] <- Used_Prob_Riffle/Avail_Prob_Riffle

#Run
Avail_Run <- ifelse(Avail_Data$Mesohabitat == 'Run', (na.omit(Avail_Data$StreamWidth)),0)
Avail_Prob_Run <- (sum(na.omit(Avail_Run)/sum(!is.na(Avail_Data$StreamWidth))))     
Used_Run <- ifelse(Used_Data_i$Mesohabitat == 'Run', (na.omit(Used_Data_i$StreamWidth)),0)
Used_Prob_Run<- sum(na.omit(Used_Run)/sum(!is.na(Used_Data_i$StreamWidth)))
Storage$Ratio_Run[i] <- Used_Prob_Run/Avail_Prob_Run
}
