#DS WB SS RU
# Loop for resource selection function
# HSC using loop generated data 

# Keep in mind the rule-of-thumb of 10 data points (probably 10 locations in your case) per covariate in your model

##### NO REPEAT FISH ########

library(dplyr)
library(readr)


##### DATA #####

NotAsFishy <- read_csv("~/School/Bonar Lab/Project/Data/Analysis/Data Ready for HSC/Data-Ready-for-HSC/Output/NotAsFishy.csv", 
                       col_types = cols(Velocity = col_double()))

##### DS_WB_SS_RU #####
##Desert Sucker Wet Beaver Creek Spawning (April - June) Riffle Data 

#Combine all desert sucker and available data on WB <----- DS_AV_WB
DS_WB <- NotAsFishy %>%                                                       #separate Beaver Creek Desert Sucker
  filter(Stream %in% c('Beaver'))%>%          
  filter(Species == 'Desert Sucker')      
AV_WB <- NotAsFishy %>%                                                       #separate Beaver Creek Available
  filter(Stream %in% c('Beaver'))%>%          
  filter(Presence == 0)                                                       # 0 = Available
DS_AV_WB <- rbind(DS_WB, AV_WB)                                               #combine DS and available
remove(DS_WB, AV_WB)

#Only Spawning period- April - June and Riffles                               #[7 desert sucker observations, 22 available]
DS_WB_SS_RU <- DS_AV_WB %>%
  filter(Month %in% c('April', 'May', 'June')) %>%
  filter(Mesohabitat == 'Run')                                                  
remove(DS_AV_WB)

##Number of Unique Individuals Observed
Unique_Fish <- na.omit(unique(DS_WB_SS_RU$Tag))                               #Gives number of individuals- doesn't count the same fish more than once

Number_Of_Individuals <- length(Unique_Fish[!is.na(Unique_Fish)])             #removes NA 
length(Unique_Fish)  #7



##Find Min and Max of variables used by fish
Counts <- DS_WB_SS_RU %>%
  filter(Species == 'Desert Sucker')

Velocity_Min <- min(Counts$Velocity, na.rm = TRUE)     
Velocity_Max  <- max(Counts$Velocity, na.rm = TRUE)   

Depth_Min <- min(Counts$Depth, na.rm = TRUE)   #
Depth_Max <- max(Counts$Depth, na.rm = TRUE)   #

Substrate_Min  <- min(Counts$Substrate, na.rm = TRUE)   #
Substrate_Max  <- max(Counts$Substrate, na.rm = TRUE)   # 

MinMax <- data.frame(DataSet = "DS_WB_SS_RU", Velocity_Min, Velocity_Max, Depth_Min, 
                     Depth_Max, Substrate_Min, Substrate_Max)


#Do some quick data exploration
boxplot(Velocity ~ Month, DS_WB_SS_RU)
boxplot(Substrate ~ Month, DS_WB_SS_RU)
boxplot(Depth ~ Month, DS_WB_SS_RU)

#Standardize and weight
#Add weight column to make binomial GLM equivalent to Inhomogeneous Poisson Point Process Model
DS_WB_SS_RU$weight <- ifelse(DS_WB_SS_RU$Presence == 1, 1, 5000)

#Calculate mean and SD of each covariate within data subset for re-scaling (standardizing)
mean_velocity  <- mean(DS_WB_SS_RU$Velocity, na.rm = T)                          #When fitting regression (GLM) models with multiple covaPOates or comparing models each containing different covariates it is helpful to 
sd_velocity    <- sd(DS_WB_SS_RU$Velocity, na.rm = T)                            #z-score standardize your covariates. This rescales everything to take covariates that were on different units (meters/percent) and 
mean_depth     <- mean(DS_WB_SS_RU$Depth, na.rm = T)                             #iteration will be slightly different on account of having slightly different values. Calculate the mean and SD of your complete data set 
sd_depth       <- sd(DS_WB_SS_RU$Depth, na.rm = T)                               #outside of the i-loop (DS_WB_SS_RU) and then using this to do the rescaling inside the i-loop (Data_i) 
mean_substrate <- mean(DS_WB_SS_RU$Substrate, na.rm=T)                           #put them on the same scale such that each covariate will have mean = 0 and SD = 1. Making beta coefficients directly comparable so that 
sd_substrate   <- sd(DS_WB_SS_RU$Substrate, na.rm=T)                             #a larger absolute value of a coefficient means a stronger effect. This is a little tricky with re-sampling because the mean/SD of each 

#Rescale/Standardize your covariates
DS_WB_SS_RU$zVelocity  <- (DS_WB_SS_RU$Velocity - mean_velocity)/sd_velocity
DS_WB_SS_RU$zDepth     <- (DS_WB_SS_RU$Depth - mean_depth)/sd_depth
DS_WB_SS_RU$zSubstrate <- (DS_WB_SS_RU$Substrate - mean_substrate)/sd_substrate

#### RSF ####    
###General Linear Modeling
#Fit the model
Model_Vel <- glm(Presence ~ zVelocity, data = DS_WB_SS_RU, family = binomial, weights = weight)             #family specifies the details of the model (GLM) used.  
Model_Subs <- glm(Presence ~ zSubstrate, data = DS_WB_SS_RU, family = binomial, weights = weight)           #one covariate: substrate
Model_Depth <- glm(Presence ~ zDepth, data = DS_WB_SS_RU, family = binomial, weights = weight)
Model_Full <- glm(Presence ~ zVelocity + zDepth, data = DS_WB_SS_RU, family=binomial, weights = weight)     #model with two covariates (velocity, depth); because there are only 19 observations we can't do all 3 (10/ rule of thumb)

#Dataframe for coefficients
Coef <- data.frame(matrix(ncol = 0, nrow = 1))

Coef$VelocityC<- coef(Model_Vel)['zVelocity']   
Coef$VelocityFullC <- coef(Model_Subs)['zSubstrate'] 
Coef$DepthC <- coef(Model_Depth)['zDepth']
Coef$DepthFullC <- coef(Model_Full)['zVelocity']                   #use if utilizing model-full
Coef$SubstrateC <- coef(Model_Full)['zDepth']



##### HSC #####
HSC_data <- filter(DS_WB_SS_RU, Presence == 1)                                   #isolate occupied site data
Avail_Data <- filter(DS_WB_SS_RU, Presence == 0)

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

#Velocity
Coef$VelocityC                                         #a negative number here denotes a negative association, and vice versa 
#-0.2721463                         #avoiding fast water (negative)
confint(Model_Vel, 'zVelocity')                                     #95% Confidence Interval of zVelocity
#-1.683334  0.415533                  #Not Significant

#Velocity Full
Coef$VelocityFullC
#-0.2094854                              #selecting fast water (choosing slowest area within riffle)
confint(Model_Full, 'zVelocity')
#-1.8561100  0.7099356                   #Not significant

#Depth
Coef$DepthC                                          
#0.9923066                        #selecting deeper water
confint(Model_Depth, 'zDepth')                                     
#0.1207116 1.9685498               #Significant

#Depth Full
Coef$DepthFullC
#-0.1779059                              #selecting deeper water 
confint(Model_Full, 'zDepth')
#0.08933025 1.97505317                  #significant

#Substrate
Coef$SubstrateC                                          
#0.9553436                        #selecting larger substrate
confint(Model_Subs, 'zSubstrate')                                     
#-0.7775241  0.5116299                 #Significant



##### Results for HSC #####

#50%
#Velocity
Velocity_25                                                   #25% probability
#0.07  
Velocity_75                                                   #75% probability
#0.18 
p_50_Velocity                                                 #% of the time that test is significant
#0
t_stat_50_Velocity                                            #tstat
#-0.3293985

#Depth
Depth_25
#38  
Depth_75                                                   
#43.5  
p_50_Depth                                                 
#1
t_stat_50_Depth                                            
#2.560299

#Substrate
Substrate_25
#5 
Substrate_75                                                   
#6
p_50_Substrate                                                 
#0
t_stat_50_Substrate                                            
#0.5859488

#90%
#Velocity
Velocity_5
#0.025  
Velocity_95
#0.259   
p_90_Velocity                                                   #% of the time that test is significant
#0
t_stat_90_Velocity
#0.3777836

#Depth
Depth_5
#31.8  
Depth_95
#95.8  
p_90_Depth
#0
t_stat_90_Depth
#0.9929352

#Substrate
Substrate_5
#4.3 
Substrate_95
#6.7 
p_90_Substrate
#0
t_stat_90_Substrate
#0.5859488

