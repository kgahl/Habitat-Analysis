# DS WB SS RI                                     
#Doesn't need to be looped- no repeat fish observations. 

##### NO REPEAT FISH ########


#Run a standard GLM. Include a couple covariates in your model(s) even though that's pushing the model a little
#Try fitting the RSF with low observations anyway, once using up to two covariates (your full model) and then again with just one covariate per RSF, 
      #and see if/how the results differ. The reason I suggest pushing the limits is simply to see what story you get. You may get completely 
      #non-significant results, in which case you have to say your non-significant results could be due to small sample sizes. But significant 
      #results consistent with biological expectations are worth something (in my opinion!)


library(dplyr)
library(readr)

##### DATA #####

NotAsFishy <- read_csv("~/School/Bonar Lab/Project/Data/Analysis/Data Ready for HSC/Data-Ready-for-HSC/Output/NotAsFishy.csv", 
                       col_types = cols(Velocity = col_double()))
str(NotAsFishy)
head(NotAsFishy)

##### DS_WB_SS_RI #####
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

#Only Spawning period- April - June and Riffles                               #[10 desert sucker observations, 122 available]
  DS_WB_SS_RI <- DS_AV_WB %>%
    filter(Month %in% c('April', 'May', 'June')) %>%
    filter(Mesohabitat == 'Riffle')                                                  # 1 = Riffle
  remove(DS_AV_WB)

##Number of Unique Individuals Observed
  Unique_Fish <- na.omit(unique(DS_WB_SS_RI$Tag))                               #Gives number of individuals- doesn't count the same fish more than once
  
  Number_Of_Individuals <- length(Unique_Fish[!is.na(Unique_Fish)])             #removes NA 
  length(Unique_Fish)  #10
  
  
##Find Min and Max of variables used by fish
  Counts <- DS_WB_SS_RI %>%
    filter(Species == 'Desert Sucker')
  
  Velocity_Min <- min(Counts$Velocity, na.rm = TRUE)     
  Velocity_Max  <- max(Counts$Velocity, na.rm = TRUE)   
  
  Depth_Min <- min(Counts$Depth, na.rm = TRUE)   #
  Depth_Max <- max(Counts$Depth, na.rm = TRUE)   #
  
  Substrate_Min  <- min(Counts$Substrate, na.rm = TRUE)   #
  Substrate_Max  <- max(Counts$Substrate, na.rm = TRUE)   # 
  
  MinMax <- data.frame(DataSet = "DS_WB_SS_RI", Velocity_Min, Velocity_Max, Depth_Min, 
                       Depth_Max, Substrate_Min, Substrate_Max)
  
  
#Do some quick data exploration
  boxplot(Velocity ~ Month, DS_WB_SS_RI)
  boxplot(Substrate ~ Month, DS_WB_SS_RI)
  boxplot(Depth ~ Month, DS_WB_SS_RI)

#Standardize and weight
  #Add weight column to make binomial GLM equivalent to Inhomogeneous Poisson Point Process Model
    DS_WB_SS_RI$weight <- ifelse(DS_WB_SS_RI$Presence == 1, 1, 5000)
  
  #Calculate mean and SD of each covariate within data subset for re-scaling (standardizing)
    mean_velocity  <- mean(DS_WB_SS_RI$Velocity, na.rm = T)                          #When fitting regression (GLM) models with multiple covaPOates or comparing models each containing different covariates it is helpful to 
    sd_velocity    <- sd(DS_WB_SS_RI$Velocity, na.rm = T)                            #z-score standardize your covariates. This rescales everything to take covariates that were on different units (meters/percent) and 
    mean_depth     <- mean(DS_WB_SS_RI$Depth, na.rm = T)                             #iteration will be slightly different on account of having slightly different values. Calculate the mean and SD of your complete data set 
    sd_depth       <- sd(DS_WB_SS_RI$Depth, na.rm = T)                               #outside of the i-loop (DS_WB_SS_RI) and then using this to do the rescaling inside the i-loop (Data_i) 
    mean_substrate <- mean(DS_WB_SS_RI$Substrate, na.rm=T)                           #put them on the same scale such that each covariate will have mean = 0 and SD = 1. Making beta coefficients directly comparable so that 
    sd_substrate   <- sd(DS_WB_SS_RI$Substrate, na.rm=T)                             #a larger absolute value of a coefficient means a stronger effect. This is a little tricky with re-sampling because the mean/SD of each 
  
  #Rescale/Standardize your covariates
    DS_WB_SS_RI$zVelocity  <- (DS_WB_SS_RI$Velocity - mean_velocity)/sd_velocity
    DS_WB_SS_RI$zDepth     <- (DS_WB_SS_RI$Depth - mean_depth)/sd_depth
    DS_WB_SS_RI$zSubstrate <- (DS_WB_SS_RI$Substrate - mean_substrate)/sd_substrate

#### RSF ####    
###General Linear Modeling
  #Fit the model
  Model_Vel <- glm(Presence ~ zVelocity, data = DS_WB_SS_RI, family = binomial, weights = weight)             #family specifies the details of the model (GLM) used.  
  Model_Subs <- glm(Presence ~ zSubstrate, data = DS_WB_SS_RI, family = binomial, weights = weight)           #one covariate: substrate
  Model_Depth <- glm(Presence ~ zDepth, data = DS_WB_SS_RI, family = binomial, weights = weight)
  Model_Full <- glm(Presence ~ zVelocity + zDepth, data = DS_WB_SS_RI, family=binomial, weights = weight)     #model with two covariates (velocity, depth); because there are only 19 observations we can't do all 3 (10/ rule of thumb)

  #Dataframe for coefficients
  Coef <- data.frame(matrix(ncol = 0, nrow = 1))
  
  Coef$VelocityC<- coef(Model_Vel)['zVelocity']   
  Coef$VelocityFullC <- coef(Model_Subs)['zSubstrate'] 
  Coef$DepthC <- coef(Model_Depth)['zDepth']
  Coef$DepthFullC <- coef(Model_Full)['zVelocity']                   #use if utilizing model-full
  Coef$SubstrateC <- coef(Model_Full)['zDepth']

  
  
##### HSC #####
  HSC_data <- filter(DS_WB_SS_RI, Presence == 1)                                   #isolate occupied site data
  Avail_Data <- filter(DS_WB_SS_RI, Presence == 0)
 
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
  #-0.08223761                         #avoiding fast water (negative)
  confint(Model_Vel, 'zVelocity')                                     #95% Confidence Interval of zVelocity
  #-0.8461237   0.4765631              #Not Significant
  
 #Velocity Full
  Coef$VelocityFullC
  #0.1894901                              #selecting fast water (choosing slowest area within riffle)
  confint(Model_Full, 'zVelocity')
  #-0.8265139   0.6524265                #Not significant
  
 #Depth
  Coef$DepthC                                          
  #0.6269869                        #selecting deeper water
  confint(Model_Depth, 'zDepth')                                     
  #0.1100815 1.0956348              #Significant
  
 #Depth Full
  Coef$DepthFullC
  #0.008051644                              #selecting deeper water 
  confint(Model_Full, 'zDepth')
  #0.1065454 1.1001997                 #significant
  
 #Substrate
  Coef$SubstrateC                                          
  #0.6277615                        #selecting larger substrate
  confint(Model_Subs, 'zSubstrate')                                     
  #-0.407413   0.8636253              #Significant
  
  

##### Results for HSC #####
 
#50%
 #Velocity
  Velocity_25                                                   #25% probability
   #0.13 
  Velocity_75                                                   #75% probability
   #0.205
  p_50_Velocity                                                 #% of the time that test is significant
   #1
  t_stat_50_Velocity                                            #tstat
   #3.447025
  
 #Depth
  Depth_25
   #22.25  
  Depth_75                                                   
   #44.5 
  p_50_Depth                                                 
   #0
  t_stat_50_Depth                                            
   #0.2486497
  
 #Substrate
  Substrate_25
   #5 
  Substrate_75                                                   
   #6
  p_50_Substrate                                                 
   #0
  t_stat_50_Substrate                                            
   #1.430722

#90%
 #Velocity
  Velocity_5
  #0.1025 
  Velocity_95
  #0.4785  
  p_90_Velocity                                                   #% of the time that test is significant
  #1
  t_stat_90_Velocity
  #2.023667
  
 #Depth
  Depth_5
  #17.7 
  Depth_95
  #55.1 
  p_90_Depth
  #0
  t_stat_90_Depth
  #1.212348
  
 #Substrate
  Substrate_5
  #5
  Substrate_95
  #6
  
  p_90_Substrate
  #0
  t_stat_90_Substrate
  #1.430722
  