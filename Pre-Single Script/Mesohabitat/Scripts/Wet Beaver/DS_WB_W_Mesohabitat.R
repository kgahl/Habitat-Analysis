#BB WB W Mesohabitat
#Desert Sucker on Wet Beaver in Winter Months for Mesohabitat selection
# Loop for resource selection function
# Keep in mind the rule-of-thumb of 10 data points (probably 10 locations in your case) per covariate in your model

library(dplyr)
library(readr)

##### DATA #####

NotAsFishy <- read_csv("~/Kaitlyn's Data Analysis/Analysis/Data Ready for HSC/Data-Ready-for-HSC/Output/NotAsFishy.csv", 
                       col_types = cols(Velocity = col_double()))

##### DS_WB_W #####
##Desert Sucker Beaver Creek Spawning (Sept - November) 

#Combine all Desert Sucker and available data on WB <----- DS_AV_WB
  DS_WB <- NotAsFishy %>%                                                       #separate Beaver Creek Desert Sucker
    filter(Stream %in% c('Beaver'))%>%          
    filter(Species == 'Desert Sucker')      
  AV_WB <- NotAsFishy %>%                                                       #separate Beaver Creek Available
    filter(Stream %in% c('Beaver'))%>%          
    filter(Presence == 0)                                                       # 0 = Available
  DS_AV_WB <- rbind(DS_WB, AV_WB)                                               #combine BB and available
  remove(DS_WB, AV_WB)

#Only Spawning period- September - November and Riffles                       #[33 Desert Sucker observations, 310 available]
  DS_WB_W <- DS_AV_WB %>%
    filter(Month %in% c('December', 'January', 'February'))
  remove(DS_AV_WB)

#Make R read Mesohabitat as a three-level categorical factor
  DS_WB_W$Mesohabitat <- as.factor(DS_WB_W$Mesohabitat)


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

  Unique_Fish <- na.omit(unique(DS_WB_W$Tag))                               #Not sure the na.omit() is needed but it is good practice to remove NAs when using the unique() function to make sure they don't cause trouble.                                 #gives the number of individuals (not the total number of observations- doesn't count the same fish more than once)
  
  Number_Of_Individuals <- length(Unique_Fish[!is.na(Unique_Fish)])             #removes NA 
  length(Unique_Fish) #21

##### Loop #####

for(i in 1:N_reps){                                                            #do 10,000 reps
  
  #Code to let you know where your loop is at
  if(i %in% seq(1,N_reps,by=100)) cat("Starting rep",i,"\n")
  
  #Sample Used data 
  Used_Data_i <- NULL
  for(j in 1:Number_Of_Individuals){                                            #create empty data frame
    Fish_j <- na.omit(unique(DS_WB_W$Tag))[j]                                   #randomly select one tag number, label it Fish j
    Fish_js_Rows <- DS_WB_W[which(DS_WB_W$Tag==Fish_j),]                        #grabs Fish j's rows of data
    
    #Randomly select the row to resample from fish j’s data (i.e., data_j)
    j_Selected_Row <- sample(seq(1, nrow(Fish_js_Rows)), 1, replace = T)        #randomly selects 1 of J's rows #changed to randomly select 1 of multiple observations of fish j
    Selected_Data <- Fish_js_Rows[j_Selected_Row,]                              #grabs randomly selected J row's data               
    
    #Place this randomly sampled row into used_Data_i
    Used_Data_i <- rbind(Used_Data_i, Selected_Data)
                                    }
##### Selection Ratios
  
    #Get available data using data generated from loop
    #Consider all of the available points within a species/site/season/mesohabitat to be equally available across the entire season. Use the same set of available points for every random sample of used point
    Avail_Data <- DS_WB_W[which(DS_WB_W$Presence == 0), ]                       #Separate available data.
    Data_i <- rbind(Used_Data_i, Avail_Data)                                    #Combine available with loop-created used
    
  # na.omit(Avail_Data$Mesohabitat == 'Pool')   #114 one NA                     #Piecing apart the selection ratio to find out where things have gone wrong
  # nrow(Avail_Data)   #310
  # na.omit(Avail_Data$Mesohabitat == 'Pool')/nrow(Avail_Data)
  # sum(na.omit(Avail_Data$Mesohabitat == 'Pool')/nrow(Avail_Data))
  
  
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
  

##### General Linear Modeling  

  #Add weight column to make binomial GLM equivalent to Inhomogeneous Poisson Point Process Model
  Data_i$weight <- ifelse(Data_i$Presence == 1, 1, 5000)
  
  #Rescale your covariates                                                        #Do not need this step for Mesohabiatats
  # Data_i$zMesohabitat <- (Data_i$Mesohabitat - mean_mesohabitat)/sd_mesohabitat
  # Data_i$zSubstrate <- (Data_i$Substrate - mean_substrate)/sd_substrate
  # Data_i$zVelocity <- (Data_i$Velocity - mean_velocity)/sd_velocity
  # Data_i$zDepth <- (Data_i$Depth - mean_depth)/sd_depth
  
  
 #Fit the model
  Model_Meso <- glm(Presence ~ Mesohabitat, data = Data_i, family = binomial, weights = weight)               
  
  # Model_Vel <- glm(Presence ~ zVelocity, data = Data_i, family = binomial,weights = weight)             #family specifies the details of the model (GLM) used.
  # Model_Subs <- glm(Presence ~ zSubstrate, data = Data_i, family = binomial,weights = weight)           #one covariate: substrate
  # Model_Depth <- glm(Presence ~ zDepth, data = Data_i, family = binomial,weights = weight)
  # Model_Full <- glm(Presence ~ zVelocity + zDepth, data = Data_i, family=binomial,weights = weight)  #model with two covariates (velocity, depth); because there are only 19 observations we can't do all 3 (10/ rule of thumb)
  
 #Pull out and store coefficients (the two slopes estimated)                    #pool is the level that is the reference level (estimated as the intercept)
  Storage$Mesohabitat_Riffle[i] <- Model_Meso$coefficients['MesohabitatRiffle'] #coefficient (i.e., a slope) for the other two levels. 
  Storage$Mesohabitat_Run[i] <- Model_Meso$coefficients['MesohabitatRun']       #These slopes give the difference in selection between the reference level and that other level
  
  
  # Storage$Velocity[i] <- coef(Model_Vel)['zVelocity']
  # Storage$Substrate[i] <- coef(Model_Subs)['zSubstrate']
  # Storage$Depth[i] <- coef(Model_Depth)['zDepth']
  # Storage$Velocity_full[i] <- coef(Model_Full)['zVelocity']                     
  # Storage$Depth_full[i] <- coef(Model_Full)['zDepth']
  
}


View(Storage)
write_csv(Storage, file = "~/Kaitlyn's Data Analysis/Analysis/Data Ready for HSC/Data-Ready-for-HSC/Output/Storage Results/DS_WB_WMesohabitatResults.csv")


##### Results of the Ratios and RSFs #####


##Selection Ratios
#the proportion of used points in habitat X divided by the proportion of available points in habitat X. 
#If the ratio is >1, it indicates selection. 
#If <1, it indicates avoidance. 

 #Pool Ratio
  hist(Storage$Ratio_Pool)                                                        
  mean(rep(Storage$Ratio_Pool), na.rm = TRUE)                                   #>1 indicates selection. <1 avoidance
  #0.7044196                         #selection of pools
  quantile(Storage$Ratio_Pool,c(0.025,0.975), na.rm = TRUE) #2.5%  to  97.5%    #If these numbers cross one it is not significant, if both are >1 or both are <1 it is significant
  #0.4884454  0.9768908                 #significant                                 
  
  
 #Riffle Ratio
  hist(Storage$Ratio_Riffle)                                                        
  mean(rep(Storage$Ratio_Riffle), na.rm = TRUE)                                           
  #1.30365                         #avoidance of riffles
  quantile(Storage$Ratio_Riffle,c(0.025,0.975), na.rm = TRUE) #2.5%  to  97.5%      
  #1.042017  1.563025                 #significant   
  
 #Run Ratio
  hist(Storage$Ratio_Run)                                                        
  mean(rep(Storage$Ratio_Run), na.rm = TRUE)                                          
  # 1.02415                        #selection of runs 
  quantile(Storage$Ratio_Run,c(0.025,0.975), na.rm = TRUE) #2.5%  to  97.5%      
  #0.7928389  1.3874680               #not significant   


##Model Results
#if pool is your reference level and the slope for riffle is significantly positive 
#then the fish are selecting for riffle more strongly than pools. And if the slope of 
#run is negative that means that fish are avoiding runs more than pools. If none of 
#your slopes are significant, it means there is no difference in the relative selection 
#strength across mesohabitats (i.e., there is no difference in selection between pools 
#and riffles and pools and runs).


 #Riffle Mesohabitat
  hist(Storage$Mesohabitat_Riffle)                                                        
  mean(rep(Storage$Mesohabitat_Riffle), na.rm = TRUE)                           #slope            
  #0.6290162                         # avoiding riffles more strongly than pools         
  quantile(Storage$Mesohabitat_Riffle,c(0.025,0.975), na.rm = TRUE) #2.5%  to  97.5%      
  #0.06453851  1.16315081                 #significant                        #Significance determined by crossing of 0           
  
  
 #Run Mesohabitat
  hist(Storage$Mesohabitat_Run)                                                        
  mean(rep(Storage$Mesohabitat_Run), na.rm = TRUE)                                           
  #0.380751                         #not/selecting for run more strongly than pools
  quantile(Storage$Mesohabitat_Run,c(0.025,0.975), na.rm = TRUE) #2.5%  to  97.5%      
  #-0.2087547    1.0440082                  #not significant               


