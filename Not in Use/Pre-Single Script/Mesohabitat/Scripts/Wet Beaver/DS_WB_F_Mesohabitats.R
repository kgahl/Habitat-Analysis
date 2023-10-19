#BB WB F 
# Loop for resource selection function

# ~15 fish per month and ~3 months per season
# ~45 total location per season
# Keep in mind the rule-of-thumb of 10 data points (probably 10 locations in your case) per covariate in your model

library(dplyr)
library(readr)

##### DATA #####

NotAsFishy <- read_csv("~/Kaitlyn's Data Analysis/Analysis/Data Ready for HSC/Data-Ready-for-HSC/Output/NotAsFishy.csv", 
                       col_types = cols(Velocity = col_double()))

##### DS_WB_F #####
##Desert Sucker West Beaver Creek Spawning (Sept - November) Riffle Data 

#Combine all Desert Sucker and available data on WB <----- DS_AV_WB
DS_WB <- NotAsFishy %>%                                                       #separate Beaver Creek Desert Sucker
  filter(Stream %in% c('Beaver'))%>%          
  filter(Species == 'Desert Sucker')      
AV_WB <- NotAsFishy %>%                                                       #separate Beaver Creek Available
  filter(Stream %in% c('Beaver'))%>%          
  filter(Presence == 0)                                                       # 0 = Available
DS_AV_WB <- rbind(DS_WB, AV_WB)                                               #combine BB and available
remove(DS_WB, AV_WB)

#Only Spawning period- September - November and Riffles                       #[8 Desert Sucker observations, 126 available]
DS_WB_F <- DS_AV_WB %>%
  filter(Month %in% c('September', 'October', 'November'))
remove(DS_AV_WB)

DS_WB_F$Mesohabitat <- as.factor(DS_WB_F$Mesohabitat)


#Calculate mean and SD of each covariate within data subset to be used later for re-scaling (standardizing) Data_i

   # mean_velocity  <- mean(DS_WB_F$Velocity,na.rm = T)                        #When fitting regression (GLM) models with multiple covariates or comparing models each containing different covariates it is helpful to 
  # sd_velocity    <- sd(DS_WB_F$Velocity,na.rm = T)                            #z-score standardize your covariates. This rescales everything to take covariates that were on different units (meters/percent) and
  # mean_substrate <- mean(DS_WB_F$Substrate,na.rm=T)                           #put them on the same scale such that each covariate will have mean = 0 and SD = 1. Making beta coefficients directly comparable so that
  # sd_substrate   <- sd(DS_WB_F$Substrate,na.rm=T)                             #a larger absolute value of a coefficient means a stronger effect. This is a little tricky with re-sampling because the mean/SD of each
  # mean_depth     <- mean(DS_WB_F$Depth,na.rm = T)                             #iteration will be slightly different on account of having slightly different values. Calculate the mean and SD of your complete data set
  # sd_depth       <- sd(DS_WB_F$Depth,na.rm = T)                               #outside of the i-loop (DS_WB_F) and then using this to do the rescaling inside the i-loop (Data_i)


#Create data frame to store coefficients
  N_reps <- 10000
  Storage <- data.frame(Ratio_Pool = NA, Ratio_Riffle = NA, Ratio_Run = NA, Mesohabitat_Riffle = NA, Mesohabitat_Run = rep(NA,N_reps))
                        
  # Velocity = rep(NA,N_reps), Substrate = NA,
  # Depth = rep(NA,N_reps))#, Velocity_full = NA,
  # Depth_full = NA)
  
  Unique_Fish <- na.omit(unique(DS_WB_F$Tag))                               #Not sure the na.omit() is needed but it is good practice to remove NAs when using the unique() function to make sure they don't cause trouble.                                 #gives the number of individuals (not the total number of observations- doesn't count the same fish more than once)
  
  Number_Of_Individuals <- length(Unique_Fish[!is.na(Unique_Fish)])             #removes NA 
  length(Unique_Fish) #14

##Loop

for(i in 1:N_reps){                                                            #do 10,000 reps
  
  #This is just some code to let you know where your loop is at
  if(i %in% seq(1,N_reps,by=100)) cat("Starting rep",i,"\n")
  
  #Sample Used data (your_full_data ->DS_WB_F<- should be for one species, one site, one season, and (maybe) one mesohabitat) #The maybe will come into play when I look at the n\NumberOfIndividuals for each species/site/season/meso combo. If there are any with less than 10 unique individuals then I should combine mesohabs.
  
  Used_Data_i <- NULL
  for(j in 1:Number_Of_Individuals){                                         #create empty data frame
    #Ditto above on the na.omi(); it is good practice
    Fish_j <- na.omit(unique(DS_WB_F$Tag))[j]                   #randomly select one tag number, label it Fish j
    Fish_js_Rows <- DS_WB_F[which(DS_WB_F$Tag==Fish_j),]    #grabs Fish j's rows of data
    
    #Randomly select the row to resample from fish j’s data (i.e., data_j)
    j_Selected_Row <- sample(seq(1, nrow(Fish_js_Rows)), 1, replace = T)      #randomly selects 1 of J's rows #changed to randomly select 1 of multiple observations of fish j
    Selected_Data <- Fish_js_Rows[j_Selected_Row,]                            #grabs randomly selected J row's data               
    
    #Place this randomly sampled row into used_Data_i
    Used_Data_i <- rbind(Used_Data_i, Selected_Data)
  }
  
  
  # Get available data
  #Consider all of the available points within a species/site/season/mesohabitat to be equally available across the entire season. Use the same set of available points for every random sample of used point
  Avail_Data <- DS_WB_F[which(DS_WB_F$Presence == 0), ]                       #Separate availabile data. y is Presence
  Data_i <- rbind(Used_Data_i, Avail_Data)                                    #Combine available with loop created used
  
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
  
  
  #Add weight column to make binomial GLM equivalent to Inhomogeneous Poisson Point Process Model
  Data_i$weight <- ifelse(Data_i$Presence == 1, 1, 5000)
  
  #Rescale your covariates
  
  #Data_i$zMesohabitat <- (Data_i$Mesohabitat - mean_mesohabitat)/sd_mesohabitat
  
  # Data_i$zSubstrate <- (Data_i$Substrate - mean_substrate)/sd_substrate
  # Data_i$zVelocity <- (Data_i$Velocity - mean_velocity)/sd_velocity
  # Data_i$zDepth <- (Data_i$Depth - mean_depth)/sd_depth
   
  ##General Linear Modeling
  #Fit the model
  Model_Meso <- glm(Presence ~ Mesohabitat, data = Data_i, family = binomial, weights = weight)   #removed weight            
  
  # Model_Vel <- glm(Presence ~ zVelocity, data = Data_i, family = binomial,weights = weight)             #family specifies the details of the model (GLM) used.
  # Model_Subs <- glm(Presence ~ zSubstrate, data = Data_i, family = binomial,weights = weight)           #one covariate: substrate
  # Model_Depth <- glm(Presence ~ zDepth, data = Data_i, family = binomial,weights = weight)
  # Model_Full <- glm(Presence ~ zVelocity + zDepth, data = Data_i, family=binomial,weights = weight)  #model with two covariates (velocity, depth); because there are only 19 observations we can't do all 3 (10/ rule of thumb)
  
  #Pull out and store coefficients
  #Storage$Mesohabitat_Int_Pool[i] <- Model_Meso$coefficients['Intercept']
  Storage$Mesohabitat_Riffle[i] <- Model_Meso$coefficients['MesohabitatRiffle']
  Storage$Mesohabitat_Run[i] <- Model_Meso$coefficients['MesohabitatRun']
  
  
  # Storage$Velocity[i] <- coef(Model_Vel)['zVelocity']
  # Storage$Substrate[i] <- coef(Model_Subs)['zSubstrate']
  # Storage$Depth[i] <- coef(Model_Depth)['zDepth']
  # Storage$Velocity_full[i] <- coef(Model_Full)['zVelocity']                     #use if utilizing model-full
  # Storage$Depth_full[i] <- coef(Model_Full)['zDepth']
  
}


View(Storage)
write_csv(Storage, file = "~/Kaitlyn's Data Analysis/Analysis/Data Ready for HSC/Data-Ready-for-HSC/Output/Storage Results/DS_WB_FMesohabitatResults.csv")


##### Results of the Ratios and RSFs #####


##Selection Ratios
#the proportion of used points in habitat X divided by the proportion of available points in habitat X. 
#If the ratio is >1, it indicates selection. 
#If <1, it indicates avoidance. 


#Pool Ratio
  hist(Storage$Ratio_Pool)                                                        
  mean(rep(Storage$Ratio_Pool), na.rm = TRUE)                                   #>1 indicates selection. <1 avoidance
  #1.195409                         #selecting pools 
  quantile(Storage$Ratio_Pool,c(0.025,0.975), na.rm = TRUE) #2.5%  to  97.5%    #If these numbers cross one it is not significant, if both are >1 or both are <1 it is significant
  #1.195409  1.3920455               #not significant   


#Riffle Ratio
  hist(Storage$Ratio_Riffle)                                                        
  mean(rep(Storage$Ratio_Riffle), na.rm = TRUE)                                           
  #0.8759798                         #avoiding riffles
  quantile(Storage$Ratio_Riffle,c(0.025,0.975), na.rm = TRUE) #2.5%  to  97.5%      
  #0.6060606  1.0101010                #significant   
  
#Run Ratio
  hist(Storage$Ratio_Run)                                                        
  mean(rep(Storage$Ratio_Run), na.rm = TRUE)                                          
  #0.639007                         #selecting runs 
  quantile(Storage$Ratio_Run,c(0.025,0.975), na.rm = TRUE) #2.5%  to  97.5%      
  #0.000000  1.958042                 #not significant   
 
  
##Model Results
  #if pool is your reference level and the slope for riffle is significantly positive 
  #then the fish are selecting for riffle more strongly than pools. And if the slope of 
  #run is negative that means that fish are avoiding runs more than pools. If none of 
  #your slopes are significant, it means there is no difference in the relative selection 
  #strength across mesohabitats (i.e., there is no difference in selection between pools 
  #and riffles and pools and runs).  
   
#Riffle Mesohabitat
  hist(Storage$Mesohabitat_Riffle)                                                        
  mean(rep(Storage$Mesohabitat_Riffle), na.rm = TRUE)                                     #slope     
  #-0.3172646                         #
  quantile(Storage$Mesohabitat_Riffle,c(0.025,0.975), na.rm = TRUE) #2.5%  to  97.5%      
  #-0.83154950    0.01574836                 #not significant#Significance determined by crossing of 0
  
#Run Mesohabitat
  hist(Storage$Mesohabitat_Run)                                                        
  mean(rep(Storage$Mesohabitat_Run), na.rm = TRUE)                                           
  #-6.662151                         #
  quantile(Storage$Mesohabitat_Run,c(0.025,0.975), na.rm = TRUE) #2.5%  to  97.5%      
  #-14.787285     0.677643                 #not significant               


