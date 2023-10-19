# BB WC FW RU                                    
# Loop for resource selection function
# HSC using loop generated data 

# Keep in mind the rule-of-thumb of 10 data points (probably 10 locations in your case) per covariate in your model

#ALL OBSERVATIONS ARE UNIQUE

library(dplyr)
library(readr)


##### DATA #####

NotAsFishy <- read_csv("~/School/Bonar Lab/Project/Data/Analysis/Data Ready for HSC/Data-Ready-for-HSC/Output/NotAsFishy.csv", 
                       col_types = cols(Velocity = col_double()))

##### BB_WC_FW_RU #####
##Black Bass West Clear Creek Spawning (Sept - Dec) Run Data 

#Combine all Black Bass and available data on WC <----- BB_AV_WC
BB_WC <- NotAsFishy %>%                                                       #separate Clear Creek Black Bass
  filter(Stream %in% c('Clear'))%>%          
  filter(Species == 'Black Bass')      
AV_WC <- NotAsFishy %>%                                                       #separate Clear Creek Available
  filter(Stream %in% c('Clear'))%>%          
  filter(Presence == 0)                                                       # 0 = Available
BB_AV_WC <- rbind(BB_WC, AV_WC)                                               #combine BB and available
remove(BB_WC, AV_WC)

#Only Spawning period- Sept - Dec and Pools                                   #[13 Black Bass observations, 59 available]
BB_WC_FW_RU <- BB_AV_WC %>%
  filter(Month %in% c('September', 'October', 'November', 'December')) %>%
  filter(Mesohabitat == 'Run')                                                  
remove(BB_AV_WC)


#Number of Unique Individuals Observed

Unique_Fish <- na.omit(unique(BB_WC_FW_RU$Tag))                               #Gives number of individuals- doesn't count the same fish more than once

Number_Of_Individuals <- length(Unique_Fish[!is.na(Unique_Fish)])             #removes NA 
length(Unique_Fish)  #13


##Find Min and Max of variables used by fish
Counts <- BB_WC_FW_RU  %>%
  filter(Species == 'Black Bass')

Velocity_Min <- min(Counts$Velocity, na.rm = TRUE)   #   
Velocity_Max  <- max(Counts$Velocity, na.rm = TRUE)   #

Depth_Min <- min(Counts$Depth, na.rm = TRUE)   #
Depth_Max <- max(Counts$Depth, na.rm = TRUE)   #

Substrate_Min  <- min(Counts$Substrate, na.rm = TRUE)   #
Substrate_Max  <- max(Counts$Substrate, na.rm = TRUE)   #  

MinMax <- data.frame(DataSet = "BB_WC_FW_RU", Velocity_Min, Velocity_Max, Depth_Min, 
                     Depth_Max, Substrate_Min, Substrate_Max)


#Do some quick data exploration
boxplot(Velocity ~ Month, BB_WC_FW_RU)
boxplot(Substrate ~ Month, BB_WC_FW_RU)
boxplot(Depth ~ Month, BB_WC_FW_RU)

#Standardize and weight
#Add weight column to make binomial GLM equivalent to Inhomogeneous Poisson Point Process Model
BB_WC_FW_RU$weight <- ifelse(BB_WC_FW_RU$Presence == 1, 1, 5000)

#Calculate mean and SD of each covariate within data subset to be used later for re-scaling (standardizing) BB_WC_FW_RU
mean_velocity  <- mean(BB_WC_FW_RU$Velocity,na.rm = T)                        #When fitting regression (GLM) models with multiple covariates or comparing models each containing different covariates it is helpful to 
sd_velocity    <- sd(BB_WC_FW_RU$Velocity,na.rm = T)                            #z-score standardize your covariates. This rescales everything to take covariates that were on different units (meters/percent) and 
mean_substrate <- mean(BB_WC_FW_RU$Substrate,na.rm=T)                           #put them on the same scale such that each covaPOate will have mean = 0 and SD = 1. Making beta coefficients directly comparable so that 
sd_substrate   <- sd(BB_WC_FW_RU$Substrate,na.rm=T)                             #a larger absolute value of a coefficient means a stronger effect. This is a little tricky with re-sampling because the mean/SD of each 
mean_depth     <- mean(BB_WC_FW_RU$Depth,na.rm = T)                             #iteration will be slightly different on account of having slightly different values. Calculate the mean and SD of your complete data set 
sd_depth       <- sd(BB_WC_FW_RU$Depth,na.rm = T)                               #outside of the i-loop (BB_WC_FW_RU) and then using this to do the rescaling inside the i-loop (BB_WC_FW_RU) 

#Rescale your covariates
BB_WC_FW_RU$zSubstrate <- (BB_WC_FW_RU$Substrate - mean_substrate)/sd_substrate
BB_WC_FW_RU$zVelocity <- (BB_WC_FW_RU$Velocity - mean_velocity)/sd_velocity
BB_WC_FW_RU$zDepth <- (BB_WC_FW_RU$Depth - mean_depth)/sd_depth


##General Linear Modeling
#Fit the model
Model_Vel <- glm(Presence ~ zVelocity, data = BB_WC_FW_RU, family = binomial, weights = weight)             #family specifies the details of the model (GLM) used.  
Model_Subs <- glm(Presence ~ zSubstrate, data = BB_WC_FW_RU, family = binomial, weights = weight)           #one covariate: substrate
Model_Depth <- glm(Presence ~ zDepth, data = BB_WC_FW_RU, family = binomial, weights = weight)
Model_Full <- glm(Presence ~ zVelocity + zDepth, data = BB_WC_FW_RU, family=binomial, weights = weight)  #model with two covariates (velocity, depth); because there are only 19 observations we can't do all 3 (10/ rule of thumb)

#Dataframe for coefficients
Coef <- data.frame(matrix(ncol = 0, nrow = 1))

Coef$VelocityC<- coef(Model_Vel)['zVelocity']   
Coef$VelocityFullC <- coef(Model_Subs)['zSubstrate'] 
Coef$DepthC <- coef(Model_Depth)['zDepth']
Coef$DepthFullC <- coef(Model_Full)['zVelocity']                   #use if utilizing model-full
Coef$SubstrateC <- coef(Model_Full)['zDepth']
  
  
##### HSC #####
HSC_data <- filter(BB_WC_FW_RU, Presence == 1)                                   
Avail_Data <- filter(BB_WC_FW_RU, Presence == 0)

###Velocity
  HSC_Velocity <- HSC_data[,c('Presence','Velocity')]                         #replaced y with Presence
  HSC_Velocity <- HSC_Velocity[order(HSC_Velocity$Velocity),]                 #organizes velocity in ascending order
  HSC_Velocity$Rank <- seq(1,nrow(HSC_Velocity))                              #assigns rank  
  HSC_Velocity$Prob <- HSC_Velocity$Rank/(nrow(HSC_Velocity)+1)               #determine probability: rank/ n+1.  N = number occupied 
  Velocity_25 <- quantile(HSC_Velocity$Velocity, 0.25, na.rm = TRUE)          #determine central 50% of observations (between 25 and 75% probability)
  Velocity_75 <- quantile(HSC_Velocity$Velocity, 0.75, na.rm = TRUE)          #added na.rm = TRUE
  Velocity_5 <- quantile(HSC_Velocity$Velocity, 0.05, na.rm = TRUE)           #determine central 90% of observations 
  Velocity_95 <- quantile(HSC_Velocity$Velocity, 0.95, na.rm = TRUE)
  HSC_Velocity$Suitable <- ifelse(HSC_Velocity$Velocity >= Velocity_25 & HSC_Velocity$Velocity <= Velocity_75, 1, 0) 
  HSC_Velocity$Suitable_90 <- ifelse(HSC_Velocity$Velocity >= Velocity_5 & HSC_Velocity$Velocity <= Velocity_95, 1, 0) 
  
  #Classify each available point as suitable or unsuitable
  Avail_Data$Velocity_Suitable <- ifelse(Avail_Data$Velocity >= Velocity_25 & Avail_Data$Velocity <= Velocity_75, 1, 0) 
  Avail_Data$Velocity_Suitable_90 <- ifelse(Avail_Data$Velocity >= Velocity_5 & Avail_Data$Velocity <= Velocity_95, 1, 0) 
  
  
 #Finding 50% T stat
  used_suit_v <- sum(HSC_Velocity$Suitable==1,na.rm=T)             #Velocity_25_Suit_Used
  used_unsuit_v <- sum(HSC_Velocity$Suitable==0,na.rm=T)           #Velocity_25_UnSuit_Used
  
  avail_suit_v <- sum(Avail_Data$Velocity_Suitable==1,na.rm=T)     #Velocity_25_Suit_Avail
  avail_unsuit_v <- sum(Avail_Data$Velocity_Suitable==0,na.rm=T)   #Velocity_25_UnSuit_Avail
  
  total <- sum(used_suit_v, avail_unsuit_v, used_unsuit_v, avail_suit_v)
  
  t_stat_50_Velocity <- ((total)^0.5)*((used_suit_v*avail_unsuit_v)-
                                         (used_unsuit_v*avail_suit_v))/
    (((used_suit_v+avail_suit_v)*
        (used_unsuit_v+avail_unsuit_v)*(used_unsuit_v+used_suit_v)*
        (avail_suit_v+avail_unsuit_v))^0.5)
  
  p_50_Velocity <- ifelse(t_stat_50_Velocity > 1.6449,1,0)                   #1.6449 is the critical value for t-statistic when alpha = 0.05
  
 #Finding 90% T stat
  used_suit_v_90 <- sum(HSC_Velocity$Suitable_90 ==1,na.rm=T)             #Velocity_25_Suit_Used
  used_unsuit_v_90 <- sum(HSC_Velocity$Suitable_90 ==0,na.rm=T)           #Velocity_25_UnSuit_Used
  
  avail_suit_v_90 <- sum(Avail_Data$Velocity_Suitable_90 ==1,na.rm=T)     #Velocity_25_Suit_Avail
  avail_unsuit_v_90 <- sum(Avail_Data$Velocity_Suitable_90 ==0,na.rm=T)   #Velocity_25_UnSuit_Avail
  
  total <- sum(used_suit_v_90, avail_unsuit_v_90, used_unsuit_v_90, avail_suit_v_90)
  
  t_stat_90_Velocity <- ((total)^0.5)*((used_suit_v_90*avail_unsuit_v_90)-
                                         (used_unsuit_v_90*avail_suit_v_90))/
    (((used_suit_v_90+avail_suit_v_90)*
        (used_unsuit_v_90+avail_unsuit_v_90)*(used_unsuit_v_90+used_suit_v_90)*
        (avail_suit_v_90+avail_unsuit_v_90))^0.5)
  
  p_90_Velocity <- ifelse(t_stat_90_Velocity > 1.6449,1,0)                   #1.6449 is the critical value for t-statistic when alpha = 0.05
  
  
###Substrate test
  HSC_Substrate <- HSC_data[,c('Presence','Substrate')]                         #replaced y with Presence
  HSC_Substrate <- HSC_Substrate[order(HSC_Substrate$Substrate),]                 #organizes Substrate in ascending order
  HSC_Substrate$Rank <- seq(1,nrow(HSC_Substrate))                              #assigns rank  
  HSC_Substrate$Prob <- HSC_Substrate$Rank/(nrow(HSC_Substrate)+1)               #determine probability: rank/ n+1.  N = number occupied 
  Substrate_25 <- quantile(HSC_Substrate$Substrate, 0.25, na.rm = TRUE)          #determine central 50% of observations (between 25 and 75% probability)
  Substrate_75 <- quantile(HSC_Substrate$Substrate, 0.75, na.rm = TRUE)          #added na.rm = TRUE
  Substrate_5 <- quantile(HSC_Substrate$Substrate, 0.05, na.rm = TRUE)           #determine central 90% of observations 
  Substrate_95 <- quantile(HSC_Substrate$Substrate, 0.95, na.rm = TRUE)
  HSC_Substrate$Suitable <- ifelse(HSC_Substrate$Substrate >= Substrate_25 & HSC_Substrate$Substrate <= Substrate_75, 1, 0) 
  HSC_Substrate$Suitable_90 <- ifelse(HSC_Substrate$Substrate >= Substrate_5 & HSC_Substrate$Substrate <= Substrate_95, 1, 0) 
  
  #Classify each available point as suitable or unsuitable
  Avail_Data$Substrate_Suitable <- ifelse(Avail_Data$Substrate >= Substrate_25 & Avail_Data$Substrate <= Substrate_75, 1, 0) 
  Avail_Data$Substrate_Suitable_90 <- ifelse(Avail_Data$Substrate >= Substrate_5 & Avail_Data$Substrate <= Substrate_95, 1, 0) 
  
  
 #Finding 50% T stat
  used_suit_s <- sum(HSC_Substrate$Suitable==1,na.rm=T)             #Substrate_25_Suit_Used
  used_unsuit_s <- sum(HSC_Substrate$Suitable==0,na.rm=T)           #Substrate_25_UnSuit_Used
  
  avail_suit_s <- sum(Avail_Data$Substrate_Suitable==1,na.rm=T)     #Substrate_25_Suit_Avail
  avail_unsuit_s <- sum(Avail_Data$Substrate_Suitable==0,na.rm=T)   #Substrate_25_UnSuit_Avail
  
  total <- sum(used_suit_s, avail_unsuit_s, used_unsuit_s, avail_suit_s)
  
  t_stat_50_Substrate <- ((total)^0.5)*((used_suit_s*avail_unsuit_s)-
                                          (used_unsuit_s*avail_suit_s))/
    (((used_suit_s+avail_suit_s)*
        (used_unsuit_s+avail_unsuit_s)*(used_unsuit_s+used_suit_s)*
        (avail_suit_s+avail_unsuit_s))^0.5)
  
  p_50_Substrate <- ifelse(t_stat_50_Substrate > 1.6449,1,0)                   #1.6449 is the critical value for t-statistic when alpha = 0.05
  
 #Finding 90% T stat
  used_suit_s_90 <- sum(HSC_Substrate$Suitable_90 ==1,na.rm=T)             #Substrate_25_Suit_Used
  used_unsuit_s_90 <- sum(HSC_Substrate$Suitable_90 ==0,na.rm=T)           #Substrate_25_UnSuit_Used
  
  avail_suit_s_90 <- sum(Avail_Data$Substrate_Suitable_90 ==1,na.rm=T)     #Substrate_25_Suit_Avail
  avail_unsuit_s_90 <- sum(Avail_Data$Substrate_Suitable_90 ==0,na.rm=T)   #Substrate_25_UnSuit_Avail
  
  total <- sum(used_suit_s_90, avail_unsuit_s_90, used_unsuit_s_90, avail_suit_s_90)
  
  t_stat_90_Substrate <- ((total)^0.5)*((used_suit_s_90*avail_unsuit_s_90)-
                                          (used_unsuit_s_90*avail_suit_s_90))/
    (((used_suit_s_90+avail_suit_s_90)*
        (used_unsuit_s_90+avail_unsuit_s_90)*(used_unsuit_s_90+used_suit_s_90)*
        (avail_suit_s_90+avail_unsuit_s_90))^0.5)
  
  p_90_Substrate <- ifelse(t_stat_90_Substrate > 1.6449,1,0)                   #1.6449 is the critical value for t-statistic when alpha = 0.05
 
   
###Depth Test
  HSC_Depth <- HSC_data[,c('Presence','Depth')]                         #replaced y with Presence
  HSC_Depth <- HSC_Depth[order(HSC_Depth$Depth),]                 #organizes Depth in ascending order
  HSC_Depth$Rank <- seq(1,nrow(HSC_Depth))                              #assigns rank  
  HSC_Depth$Prob <- HSC_Depth$Rank/(nrow(HSC_Depth)+1)               #determine probability: rank/ n+1.  N = number occupied 
  Depth_25 <- quantile(HSC_Depth$Depth, 0.25, na.rm = TRUE)          #determine central 50% of observations (between 25 and 75% probability)
  Depth_75 <- quantile(HSC_Depth$Depth, 0.75, na.rm = TRUE)          #added na.rm = TRUE
  Depth_5 <- quantile(HSC_Depth$Depth, 0.05, na.rm = TRUE)           #determine central 90% of observations 
  Depth_95 <- quantile(HSC_Depth$Depth, 0.95, na.rm = TRUE)
  HSC_Depth$Suitable <- ifelse(HSC_Depth$Depth >= Depth_25 & HSC_Depth$Depth <= Depth_75, 1, 0) 
  HSC_Depth$Suitable_90 <- ifelse(HSC_Depth$Depth >= Depth_5 & HSC_Depth$Depth <= Depth_95, 1, 0) 
  
  #Classify each available point as suitable or unsuitable
  Avail_Data$Depth_Suitable <- ifelse(Avail_Data$Depth >= Depth_25 & Avail_Data$Depth <= Depth_75, 1, 0) 
  Avail_Data$Depth_Suitable_90 <- ifelse(Avail_Data$Depth >= Depth_5 & Avail_Data$Depth <= Depth_95, 1, 0) 
  
  
 #Finding 50% T stat
  used_suit_d <- sum(HSC_Depth$Suitable==1,na.rm=T)             #Depth_25_Suit_Used
  used_unsuit_d <- sum(HSC_Depth$Suitable==0,na.rm=T)           #Depth_25_UnSuit_Used
  
  avail_suit_d <- sum(Avail_Data$Depth_Suitable==1,na.rm=T)     #Depth_25_Suit_Avail
  avail_unsuit_d <- sum(Avail_Data$Depth_Suitable==0,na.rm=T)   #Depth_25_UnSuit_Avail
  
  total <- sum(used_suit_d, avail_unsuit_d, used_unsuit_d, avail_suit_d)
  
  t_stat_50_Depth <- ((total)^0.5)*((used_suit_d*avail_unsuit_d)-
                                      (used_unsuit_d*avail_suit_d))/
    (((used_suit_d+avail_suit_d)*
        (used_unsuit_d+avail_unsuit_d)*(used_unsuit_d+used_suit_d)*
        (avail_suit_d+avail_unsuit_d))^0.5)
  
  p_50_Depth <- ifelse(t_stat_50_Depth > 1.6449,1,0)                   #1.6449 is the critical value for t-statistic when alpha = 0.05
  
 #Finding 90% T stat
  used_suit_d_90 <- sum(HSC_Depth$Suitable_90 ==1,na.rm=T)             #Depth_25_Suit_Used
  used_unsuit_d_90 <- sum(HSC_Depth$Suitable_90 ==0,na.rm=T)           #Depth_25_UnSuit_Used
  
  avail_suit_d_90 <- sum(Avail_Data$Depth_Suitable_90 ==1,na.rm=T)     #Depth_25_Suit_Avail
  avail_unsuit_d_90 <- sum(Avail_Data$Depth_Suitable_90 ==0,na.rm=T)   #Depth_25_UnSuit_Avail
  
  total <- sum(used_suit_d_90, avail_unsuit_d_90, used_unsuit_d_90, avail_suit_d_90)
  
  t_stat_90_Depth <- ((total)^0.5)*((used_suit_d_90*avail_unsuit_d_90)-
                                      (used_unsuit_d_90*avail_suit_d_90))/
    (((used_suit_d_90+avail_suit_d_90)*
        (used_unsuit_d_90+avail_unsuit_d_90)*(used_unsuit_d_90+used_suit_d_90)*
        (avail_suit_d_90+avail_unsuit_d_90))^0.5)
  
  p_90_Depth <- ifelse(t_stat_90_Depth > 1.6449,1,0)                   #1.6449 is the critical value for t-statistic when alpha = 0.05
  


  
##### Results of the RSFs #####
  
  Velocity_Mean <- Coef$VelocityC
  Velocity_CI <- confint(Model_Vel, 'zVelocity')
  
  VelocityFull_Mean <- Coef$VelocityFullC
  VelocityFull_CI <- confint(Model_Full, 'zVelocity')
  
  Depth_Mean <- Coef$DepthC
  Depth_CI <- confint(Model_Depth, 'zDepth')
  
  DepthFull_Mean <- Coef$DepthFullC
  DepthFull_CI <- confint(Model_Full, 'zDepth')
  
  Substrate_Mean <- Coef$SubstrateC
  Substrate_CI <- confint(Model_Subs, 'zSubstrate')
  
  RSF <-  data.frame(DataSet = "BB_WC_FW_RU", 
                     Velocity_Mean, Velocity_CI, VelocityFull_Mean, VelocityFull_CI, 
                     Depth_Mean, Depth_CI, DepthFull_Mean, DepthFull_CI,
                     Substrate_Mean, Substrate_CI)
  
#Below # answers are incorrect 
 #Velocity
    Coef$VelocityC                      #a negative number here denotes a negative association, and vice versa 
    #-0.6464352                         #avoiding fast water (negative)
    confint(Model_Vel, 'zVelocity')     #95% Confidence Interval of zVelocity
    #-3.0069444  0.3727086              #Not Significant
  
 #Velocity Full
    Coef$VelocityFullC
    #-0.3686769                         #selecting fast water (choosing slowest area within riffle)
    confint(Model_Full, 'zVelocity')
    #-2.248397  1.041846                #Not significant
  
  #Depth
    Coef$DepthC                                          
    # 0.8527769                         #selecting deeper water
    confint(Model_Depth, 'zDepth')                                     
    #0.01914094 1.94687487 
    #Significant
  
  #Depth Full
    Coef$DepthFullC
    #-0.04765755                        #selecting deeper water 
    confint(Model_Full, 'zDepth')
    #-0.1082019  2.1613158              #significant
  
  #Substrate
    Coef$SubstrateC                                          
    #0.8330214                          #selecting larger substrate
    confint(Model_Subs, 'zSubstrate')                                     
    #-1.5473352  0.7944102              #Significant
  
  
  
##### Results for HSC #####
  
##50%
 #Velocity
    Velocity50_25 <- Velocity_25                                                   #25% probability
    #0 
    Velocity50_75 <- Velocity_75                                                   #75% probability
    #0
    Velocity50_P <- p_50_Velocity                                                 #% of the time that test is significant
    #0
    Velocity50_T<- t_stat_50_Velocity                                            #tstat
    #1.468374
  
 #Depth
    Depth50_25<- Depth_25
    #93.25   
    Depth50_75 <- Depth_75                                                   
    #151  
    Depth50_P <- p_50_Depth                                                 
    #1
    Depth50_T <- t_stat_50_Depth                                            
    #1.685495
  
 #Substrate
    Substrate50_25<- Substrate_25
    #93.25   
    Substrate50_75 <- Substrate_75                                                   
    #151  
    Substrate50_P <- p_50_Substrate                                                 
    #1
    Substrate50_T <- t_stat_50_Depth                                            
    #1.685495
  
##90%
 #Velocity
    Velocity90_5 <- Velocity_5
    #0 
    Velocity90_95 <- Velocity_95
    #0.045   
    Velocity90_P <- p_90_Velocity                                                   #% of the time that test is significant
    #0
    Velocity90_T <- t_stat_90_Velocity
    #0.2293824
  
 #Depth
    Depth90_5 <- Depth_5
    #0 
    Depth90_95 <- Depth_95
    #0.045   
    Depth90_P <- p_90_Depth                                                   #% of the time that test is significant
    #0
    Depth90_T <- t_stat_90_Depth
    #0.2293824
  
 #Substrate
    Substrate90_5 <- Substrate_5
    #0 
    Substrate90_95 <- Substrate_95
    #0.045   
    Substrate90_P <- p_90_Substrate                                                   #% of the time that test is significant
    #0
    Substrate90_T <- t_stat_90_Substrate
    #0.2293824
  
HSC <-  data.frame(DataSet = "BB_WC_FW_RU", 
                   Velocity50_25, Velocity50_75, Velocity50_T, Velocity50_P,
                   Depth50_25, Depth50_75, Depth50_T, Depth50_P,
                   Substrate50_25, Substrate50_75, Substrate50_T, Substrate50_P,
                   
                   Velocity90_5, Velocity90_95, Velocity90_T, Velocity90_P,
                   Depth90_5, Depth90_95, Depth90_T, Depth90_P,
                   Substrate90_5, Substrate90_95, Substrate90_T, Substrate90_P)
