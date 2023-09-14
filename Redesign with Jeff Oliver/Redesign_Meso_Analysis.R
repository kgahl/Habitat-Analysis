# BB WC F 
# Resource Selection Analysis Loop

# ~15 fish per month and ~3 months per season
# ~45 total location per season
# Keep in mind the rule-of-thumb of 10 data points (probably 10 locations in your case) per covariate in your model

library(dplyr)
library(readr)

##### DATA #####

NotAsFishy <- read_csv("Output/NotAsFishy.csv")


#### Black Bass West Clear Creek Fall/Winter (Sept - Dec) Data 
   # Select_Data

##List of Variables
  Species_ <- "Black Bass"
  Species_Abb <- "BB"
  Stream_ <- "Clear"
  Stream_Abb <- "WC"
  Season_ <- c('September', 'October', 'November', 'December')
  Season_Abb <- "FWtest"


Results_File <- paste0("Mesohabitat/Results/",                       #Makes file name based on current variables
                       Species_Abb, "_",                           
                       Stream_Abb, "_",
                       Season_Abb, "_", "_Results.csv")

##Create Data frame 
#Filter occupied and available data by stream and species
 Occupied <- NotAsFishy %>%                                                       
  filter(Stream == Stream_)%>%                                                  
  filter(Species == Species_)      
 Available <- NotAsFishy %>%                                                       
  filter(Stream == Stream_)%>%          
  filter(Presence == 0)                                                         #0 = Available
 Combined <- rbind(Occupied, Available)                                         #combine occupied and available
 remove(Occupied, Available)

#Then month
 Select_Data <- Combined %>%                                                    #[54 observations, 492 available]    -check in console by: table(Select_Data$Presence)
  filter(Month %in% Season_)
 remove(Combined) 
 
Select_Data$Mesohabitat <- as.factor(Select_Data$Mesohabitat)


##Count Individuals
 Unique_Fish <- na.omit(unique(Select_Data$Tag))                                #gives the number of individuals (not the total number of observations- doesn't count the same fish more than once)

 Number_Of_Individuals <- length(Unique_Fish[!is.na(Unique_Fish)])              #removes NA
 length(Unique_Fish) #31



##### RESOURCE SELECTION LOOP #####

##Create data frame to store coefficients
 N_reps <- 10000
 Storage <- data.frame(Ratio_Pool = NA, Ratio_Riffle = NA, Ratio_Run = NA, 
                        Mesohabitat_Riffle = NA, Mesohabitat_Run = rep(NA,N_reps))
                        

##Loop

for(i in 1:N_reps){                                                             #do 10,000 reps
  
  #Shows progress of loop in output
  if(i %in% seq(1,N_reps,by=100)) cat("Starting rep",i,"\n")
  
  #Sample Used data (Select_Data)
  Used_Data_i <- NULL
  for(j in 1:Number_Of_Individuals){                                            #create empty data frame
    Fish_j <- na.omit(unique(Select_Data$Tag))[j]                               #randomly select one tag number, label it Fish j
    Fish_js_Rows <- Select_Data[which(Select_Data$Tag==Fish_j),]                #grabs Fish j's rows of data       
    
    #Randomly select the row to resample from Fish j’s data (i.e., data_j)
    j_Selected_Row <- sample(seq(1, nrow(Fish_js_Rows)), 1, replace = T)        #randomly selects 1 of fish j's multiple observations
    j_Row_Data  <- Fish_js_Rows[j_Selected_Row,]                                #pulls out the row of data for the above selected fish and places into               
    
    #Place this randomly sampled row into used_Data_i
    Used_Data_i <- rbind(Used_Data_i, j_Row_Data)
  }
  

  #Get available data                                                           #Consider all of the available points within a species/site/season/mesohabitat to be equally available across the entire season. Use the same set of available points for every random sample of used point
   Avail_Data <- Select_Data[which(Select_Data$Presence == 0), ]                 #Separate availabile data
   Data_i <- rbind(Used_Data_i, Avail_Data)                                      #Combine available with loop created used
  
  
  
  ##### RESOURCE RATIOS #####
  
  #Pool
   Avail_Prob_Pool <- (sum(na.omit(Avail_Data$Mesohabitat == 'Pool')/nrow(Avail_Data)))     #Data before this line is appearing properly in tables. tried na.omit and it changes the outcome from NA to num(0)
   Used_Prob_Pool<- sum(na.omit(Used_Data_i$Mesohabitat== 'Pool')/nrow(Used_Data_i))
   Storage$Ratio_Pool[i] <- Used_Prob_Pool/Avail_Prob_Pool
  
  #Riffle
   Avail_Prob_Riffle <- sum(na.omit(Avail_Data$Mesohabitat== 'Riffle')/nrow(Avail_Data)) 
   Used_Prob_Riffle<- sum(na.omit(Used_Data_i$Mesohabitat=='Riffle')/nrow(Used_Data_i))
   Storage$Ratio_Riffle[i] <- Used_Prob_Riffle/Avail_Prob_Riffle
  
  #Run
   Avail_Prob_Run <- sum(na.omit(Avail_Data$Mesohabitat== 'Run')/nrow(Avail_Data)) 
   Used_Prob_Run<- sum(na.omit(Used_Data_i$Mesohabitat== 'Run')/nrow(Used_Data_i))
   Storage$Ratio_Run[i] <- Used_Prob_Run/Avail_Prob_Run
  
  

  ##### GENERAL LINEAR MODELING #####
  
  #Add weight column to make binomial GLM equivalent to Inhomogeneous Poisson Point Process Model
   Data_i$weight <- ifelse(Data_i$Presence == 1, 1, 5000)
  
  #Fit the model
   Model_Meso <- glm(Presence ~ Mesohabitat, data = Data_i, family = binomial, weights = weight)             

  #Pull out and store coefficients
   Storage$Mesohabitat_Riffle[i] <- Model_Meso$coefficients['MesohabitatRiffle']
   Storage$Mesohabitat_Run[i] <- Model_Meso$coefficients['MesohabitatRun']

}


##### SAVE #####
 
View(Storage)
write_csv(Storage, file = Results_File)



##### RESULTS OF SELECTION RATIOS #####

##Selection Ratios
  #the proportion of used points in habitat X divided by the proportion of available points in habitat X. 
  #If the ratio is >1, it indicates selection. 
  #If <1, it indicates avoidance. 


#Pool Ratio
  hist(Storage$Ratio_Pool)                                                        
  mean(rep(Storage$Ratio_Pool), na.rm = TRUE)                                   #>1 indicates selection. <1 avoidance
  #1.3462                         #selecting pools 
  quantile(Storage$Ratio_Pool,c(0.025,0.975), na.rm = TRUE) #2.5%  to  97.5%    #If these numbers cross one it is not significant, if both are >1 or both are <1 it is significant
  #1.235465 1.425536               #not significant   

#Riffle Ratio
  hist(Storage$Ratio_Riffle)                                                        
  mean(rep(Storage$Ratio_Riffle), na.rm = TRUE)                                           
  #0.4758486                         #avoiding riffles
  quantile(Storage$Ratio_Riffle,c(0.025,0.975), na.rm = TRUE) #2.5%  to  97.5%      
  #0.3579918 0.5966529                #significant   
  
#Run Ratio
  hist(Storage$Ratio_Run)                                                        
  mean(rep(Storage$Ratio_Run), na.rm = TRUE)                                          
  #2.383201                         #selecting runs 
  quantile(Storage$Ratio_Run,c(0.025,0.975), na.rm = TRUE) #2.5%  to  97.5%      
  #1.882996 2.958994                 #not significant   
 
  
  
##### RESULTS OF RESOURCE SELECTION FUNCTIONS #####
  
##Model Results
  #if pool is your reference level and the slope for riffle is significantly positive 
  #then the fish are selecting for riffle more strongly than pools. And if the slope of 
  #run is negative that means that fish are avoiding runs more than pools. If none of 
  #your slopes are significant, it means there is no difference in the relative selection 
  #strength across mesohabitats (i.e., there is no difference in selection between pools 
  #and riffles and pools and runs).  
   
#Riffle Mesohabitat
  hist(Storage$Mesohabitat_Riffle)                                                        
  mean(rep(Storage$Mesohabitat_Riffle), na.rm = TRUE)                            #slope     
  #-1.052269                         
  quantile(Storage$Mesohabitat_Riffle,c(0.025,0.975), na.rm = TRUE) #2.5%  to  97.5%      
  #-1.3817932 -0.7278668                 #not significant#Significance determined by crossing of 0
  
#Run Mesohabitat
  hist(Storage$Mesohabitat_Run)                                                        
  mean(rep(Storage$Mesohabitat_Run), na.rm = TRUE)                                           
  #0.5615753                         
  quantile(Storage$Mesohabitat_Run,c(0.025,0.975), na.rm = TRUE) #2.5%  to  97.5%      
  #0.2783163 0.8734023                 #not significant               


