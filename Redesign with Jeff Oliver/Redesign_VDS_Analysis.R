# BB WC FW PO
# Resource Selection Analysis Loop

# ~15 fish per month and ~3 months per season
# ~45 total location per season
# Keep in mind the rule-of-thumb of 10 data points (probably 10 locations in your case) per covariate in your model

library(dplyr)
library(readr)

##### DATA #####

NotAsFishy <- read_csv("Output/NotAsFishy.csv")

str(NotAsFishy)
head(NotAsFishy)


#### Black Bass West Clear Creek Fall/Winter (Sept - Dec) Pool Data 
   # BB_WC_FW_PO

##List of Variables
  Species_ <- "Black Bass"
  Species_Abb <- "BB"
  Stream_ <- "Clear"
  Stream_Abb <- "WC"
  Season_ <- c('September', 'October', 'November', 'December')
  Season_Abb <- "FW"
  Mesohabitat_ <- "Pool"
  Mesohabitat_Abb <- "POtest"
  
  Results_File <- paste0("VelocityDepthSubstrate/Results/",                     #Makes file name based on current variables
                        Species_Abb, "_",                           
                        Stream_Abb, "_",
                        Season_Abb, "_",
                        Mesohabitat_Abb, "_Results.csv")

##Create Data frame 
#Filter occupied and available data by stream and species
 Occupied <- NotAsFishy %>%                                                       
  dplyr::filter(Stream == Stream_)%>%                                           #dplyr:: before a function lets it know to use the function from the dplyr package       
  dplyr::filter(Species == Species_)      
 Available <- NotAsFishy %>%                                                       
  dplyr::filter(Stream == Stream_)%>%          
  dplyr::filter(Presence == 0)                                                  #0 = Available
 Combined <- rbind(Occupied, Available)                                         #combine occupied and available
 remove(Occupied, Available)
 
#Then month and mesohabitat
 Select_Data <- Combined %>%                                                    #[23 observations, 167 available]    -check in console by: table(Select_Data$Presence)
  dplyr::filter(Month %in% Season_) %>%
  dplyr::filter(Mesohabitat == Mesohabitat_)                                    
 remove(Combined)

 
 
##### SETUP #####

##Count Individuals
 Unique_Fish <- na.omit(unique(Select_Data$Tag))                                #doesn't count the same fish more than once
 Number_Of_Individuals <- length(Unique_Fish[!is.na(Unique_Fish)])              #removes NA 
 length(Unique_Fish)  #15                                                       #when unique individuals is less than 10, consider combining mesohabs.
 
##Check data- do months fit together, do ranges, outliers, etc look right
  boxplot(Velocity ~ Month, Select_Data)
  boxplot(Substrate ~ Month, Select_Data)
  boxplot(Depth ~ Month, Select_Data)
 
##Calculate mean and SD of each covariate within data subset to be used later for re-scaling (standardizing) Data_i
  Mean_Velocity  <- mean(Select_Data$Velocity,na.rm = T)                        #When fitting regression (GLM) models with multiple covariates or comparing models each containing different covariates it is helpful to 
  SD_Velocity    <- sd(Select_Data$Velocity,na.rm = T)                            #z-score standardize your covariates. This rescales everything to take covariates that were on different units (meters/percent) and 
  Mean_Substrate <- mean(Select_Data$Substrate,na.rm=T)                           #put them on the same scale such that each covaPOate will have mean = 0 and SD = 1. Making beta coefficients directly comparable so that 
  SD_Substrate   <- sd(Select_Data$Substrate,na.rm=T)                             #a larger absolute value of a coefficient means a stronger effect. This is a little tricky with re-sampling because the mean/SD of each 
  Mean_Depth     <- mean(Select_Data$Depth,na.rm = T)                             #iteration will be slightly different on account of having slightly different values. Calculate the mean and SD of your complete data set 
  SD_Depth       <- sd(Select_Data$Depth,na.rm = T)                               #outside of the i-loop (Select_Data) and then using this to do the rescaling inside the i-loop (Data_i) 



##### MIN MAX #####
  
#Find Minimum and Maximum values of used variables
  Counts <- Select_Data  %>%
    dplyr::filter(Species == Species_)
  
  Velocity_Min  <- min(Counts$Velocity, na.rm = TRUE)    # 0   
  Velocity_Max  <- max(Counts$Velocity, na.rm = TRUE)    # 0.13
  
  Depth_Min     <- min(Counts$Depth, na.rm = TRUE)       # 32
  Depth_Max     <- max(Counts$Depth, na.rm = TRUE)       # 151
  
  Substrate_Min <- min(Counts$Substrate, na.rm = TRUE)   # 1
  Substrate_Max <- max(Counts$Substrate, na.rm = TRUE)   # 7  
  
  MinMax <- data.frame(DataSet = "Select_Data", Velocity_Min, Velocity_Max, Depth_Min, 
                       Depth_Max, Substrate_Min, Substrate_Max)



##### RESOURCE SELECTION LOOP #####

##Create data frame to store coefficients
N_reps <- 10000
Storage <- data.frame(Velocity = rep(NA,N_reps), Substrate = NA,
                      Depth = rep(NA,N_reps), Velocity_full = NA, 
                      Depth_full = NA,
                      
                      Velocity_25=NA, Velocity_75=NA,
                      Velocity_25_Suit_Used=NA, Velocity_25_UnSuit_Avail=NA,
                      Velocity_25_UnSuit_Used=NA, Velocity_25_Suit_Avail=NA,
                      t_stat_50_Velocity=NA, p_50_Velocity = NA,
                      
                      Substrate_25=NA, Substrate_75=NA,
                      Substrate_25_Suit_Used=NA, Substrate_25_UnSuit_Avail=NA,
                      Substrate_25_UnSuit_Used=NA, Substrate_25_Suit_Avail=NA,
                      t_stat_50_Substrate=NA, p_50_Substrate = NA,
                      
                      Depth_25=NA, Depth_75=NA,
                      Depth_25_Suit_Used=NA, Depth_25_UnSuit_Avail=NA,
                      Depth_25_UnSuit_Used=NA, Depth_25_Suit_Avail=NA,
                      t_stat_50_Depth=NA, p_50_Depth = NA,
                      
                      Velocity_5=NA, Velocity_95=NA,
                      Velocity_5_Suit_Used=NA, Velocity_5_UnSuit_Avail=NA,
                      Velocity_5_UnSuit_Used=NA, Velocity_5_Suit_Avail=NA,
                      t_stat_90_Velocity=NA, p_90_Velocity = NA,
                      
                      Substrate_5=NA, Substrate_95=NA,
                      Substrate_5_Suit_Used=NA, Substrate_5_UnSuit_Avail=NA,
                      Substrate_5_UnSuit_Used=NA, Substrate_5_Suit_Avail=NA,
                      t_stat_90_Substrate=NA, p_90_Substrate = NA,
                      
                      Depth_5=NA, Depth_95=NA,
                      Depth_5_Suit_Used=NA, Depth_5_UnSuit_Avail=NA,
                      Depth_5_UnSuit_Used=NA, Depth_5_Suit_Avail=NA,
                      t_stat_90_Depth = NA, p_90_Depth = NA)


##Loop

for(i in 1:N_reps){                                                             #do 10,000 reps
  
  #Shows progress of loop in output
  if(i %in% seq(1, N_reps, by = 100)) cat("Starting rep", i, "\n")
  
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
   Avail_Data <- Select_Data[which(Select_Data$Presence == 0), ]                #Separate available data
   Data_i <- rbind(Used_Data_i, Avail_Data)                                     #Combine available with loop created used

  #Add weight column to make binomial GLM equivalent to Inhomogeneous Poisson Point Process Model
   Data_i$weight <- ifelse(Data_i$Presence == 1, 1, 5000)
  
  #Rescale the covariates
   Data_i$zSubstrate <- (Data_i$Substrate - Mean_Substrate)/SD_Substrate
   Data_i$zVelocity <- (Data_i$Velocity - Mean_Velocity)/SD_Velocity
   Data_i$zDepth <- (Data_i$Depth - Mean_Depth)/SD_Depth
  
  
  
  ##### GENERAL LINEAR MODELING #####
  #Fit the model
   Model_Vel   <- glm(Presence ~ zVelocity, data = Data_i, 
                       family = binomial, weights = weight)                     #family specifies the details of the model (GLM) used.  
  
   Model_Subs  <- glm(Presence ~ zSubstrate, data = Data_i, 
                       family = binomial, weights = weight)                     
  
   Model_Depth <- glm(Presence ~ zDepth, data = Data_i, 
                       family = binomial, weights = weight)
  
   Model_Full  <- glm(Presence ~ zVelocity + zDepth, data = Data_i,             #model with two covariates (velocity, depth); because of limited observations we can't do all 3 (10/ rule of thumb)
                       family = binomial, weights = weight)   
  
  #Pull out and store coefficients
   Storage$Velocity[i] <- coef(Model_Vel)['zVelocity']      
   Storage$Substrate[i] <- coef(Model_Subs)['zSubstrate'] 
   Storage$Depth[i] <- coef(Model_Depth)['zDepth'] 
   Storage$Velocity_full[i] <- coef(Model_Full)['zVelocity']                    #use if utilizing model-full
   Storage$Depth_full[i] <- coef(Model_Full)['zDepth']
  
  
  
  ##### HABITAT SUITABILITY CRITERIA #####
   HSC_data <- filter(Data_i, Presence == 1)                                    #isolate occupied site data
  
  ###Velocity
   #Find suitable ranges 
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
        #table(HSC_Velocity$Presence, HSC_Velocity$Suitable)                             #compares number suitable v number unsuitable out of occupied locations
  
   ##Central 50%
      Storage$Velocity_25_Suit_Used[i] <- sum(HSC_Velocity$Suitable == 1, na.rm = T)
      Storage$Velocity_25_UnSuit_Used[i] <- sum(HSC_Velocity$Suitable == 0, na.rm = T)
      
      Storage$Velocity_25_Suit_Avail[i] <- sum(Avail_Data$Velocity_Suitable == 1, na.rm = T)
      Storage$Velocity_25_UnSuit_Avail[i] <- sum(Avail_Data$Velocity_Suitable == 0, na.rm = T)
      
      Storage$Velocity_25[i] <- Velocity_25
      Storage$Velocity_75[i] <- Velocity_75
  
      con_tab_Suit50_v <- matrix(c(Storage[i,c("Velocity_25_Suit_Used",
                                               "Velocity_25_UnSuit_Used",
                                               "Velocity_25_Suit_Avail",
                                               "Velocity_25_UnSuit_Avail")]),
                                 byrow=T,
                                 nrow=2)
      used_suit_v <- Storage$Velocity_25_Suit_Used[i]
      avail_unsuit_v <- Storage$Velocity_25_UnSuit_Avail[i]
      used_unsuit_v <- Storage$Velocity_25_UnSuit_Used[i]
      avail_suit_v <- Storage$Velocity_25_Suit_Avail[i]
      
      
      t_stat_v <- ((sum(unlist(con_tab_Suit50_v)))^0.5)*
                  ((used_suit_v*avail_unsuit_v) - (used_unsuit_v*avail_suit_v))/
                  (((used_suit_v+avail_suit_v)*
                    (used_unsuit_v+avail_unsuit_v)*
                    (used_unsuit_v+used_suit_v)*
                    (avail_suit_v+avail_unsuit_v))^0.5)
      
      Storage$t_stat_50_Velocity[i] <- t_stat_v 
      
      Storage$p_50_Velocity[i] <- ifelse(t_stat_v > 1.6449,1,0)                 #1.6449 is the critical value for t-statistic when alpha = 0.05
  
   ##Central 90%
      Storage$Velocity_5_Suit_Used[i] <- sum(HSC_Velocity$Suitable_90==1,na.rm=T)
      Storage$Velocity_5_UnSuit_Used[i] <- sum(HSC_Velocity$Suitable_90==0,na.rm=T)
      
      Storage$Velocity_5_Suit_Avail[i] <- sum(Avail_Data$Velocity_Suitable_90==1,na.rm=T)
      Storage$Velocity_5_UnSuit_Avail[i] <- sum(Avail_Data$Velocity_Suitable_90==0,na.rm=T)
      
      Storage$Velocity_5[i] <- Velocity_5
      Storage$Velocity_95[i] <- Velocity_95
      
      con_tab_Suit90_v <- matrix(c(Storage[i,c("Velocity_5_Suit_Used",
                                               "Velocity_5_UnSuit_Used",
                                               "Velocity_5_Suit_Avail",
                                               "Velocity_5_UnSuit_Avail")]),
                                 byrow=T,
                                 nrow=2)
      used_suit_v_90 <- Storage$Velocity_5_Suit_Used[i]
      avail_unsuit_v_90 <- Storage$Velocity_5_UnSuit_Avail[i]
      used_unsuit_v_90 <- Storage$Velocity_5_UnSuit_Used[i]
      avail_suit_v_90 <- Storage$Velocity_5_Suit_Avail[i]
  
  
      t_stat_v_90 <- ((sum(unlist(con_tab_Suit90_v)))^0.5)*((used_suit_v_90*avail_unsuit_v_90)-
                                                              (used_unsuit_v_90*avail_suit_v_90))/(((used_suit_v_90+avail_suit_v_90)*
                                                                                                      (used_unsuit_v_90+avail_unsuit_v_90)*(used_unsuit_v_90+used_suit_v_90)*
                                                                                                      (avail_suit_v_90+avail_unsuit_v_90))^0.5)
      
      Storage$t_stat_90_Velocity[i] <- t_stat_v_90 
      
      Storage$p_90_Velocity[i] <- ifelse(t_stat_v_90 > 1.6449, 1, 0)            #1.6449 is the critical value for t-statistic when alpha = 0.05
  
  
  ###Substrate
   #Find suitable ranges
    HSC_Substrate <- HSC_data[,c('Presence','Substrate')]                       #replaced y with Presence
    HSC_Substrate <- HSC_Substrate[order(HSC_Substrate$Substrate),]             #organizes Substrate in ascending order
    HSC_Substrate$Rank <- seq(1,nrow(HSC_Substrate))                            #assigns rank  
    HSC_Substrate$Prob <- HSC_Substrate$Rank/(nrow(HSC_Substrate)+1)            #determine probability: rank/ n+1.  N = number occupied 
    Substrate_25 <- quantile(HSC_Substrate$Substrate, 0.25, na.rm = TRUE)       #determine central 50% of observations (between 25 and 75% probability)
    Substrate_75 <- quantile(HSC_Substrate$Substrate, 0.75, na.rm = TRUE)       #added na.rm = TRUE
    Substrate_5 <- quantile(HSC_Substrate$Substrate, 0.05, na.rm = TRUE)        #determine central 90% of observations 
    Substrate_95 <- quantile(HSC_Substrate$Substrate, 0.95, na.rm = TRUE)
    HSC_Substrate$Suitable <- ifelse(HSC_Substrate$Substrate >= Substrate_25 & 
                                     HSC_Substrate$Substrate <= Substrate_75, 1, 0) 
    HSC_Substrate$Suitable_90 <- ifelse(HSC_Substrate$Substrate >= Substrate_5 & 
                                        HSC_Substrate$Substrate <= Substrate_95, 1, 0) 
  
   #Classify each available point as suitable or unsuitable
    Avail_Data$Substrate_Suitable <- ifelse(Avail_Data$Substrate >= Substrate_25 & 
                                            Avail_Data$Substrate <= Substrate_75, 1, 0) 
    Avail_Data$Substrate_Suitable_90 <- ifelse(Avail_Data$Substrate >= Substrate_5 & 
                                               Avail_Data$Substrate <= Substrate_95, 1, 0) 
        #table(HSC_Substrate$Presence, HSC_Substrate$Suitable)                      #compares number suitable v number unsuitable out of occupied locations
  
   ##Central 50%
      Storage$Substrate_25_Suit_Used[i] <- sum(HSC_Substrate$Suitable == 1, na.rm = T)
      Storage$Substrate_25_UnSuit_Used[i] <- sum(HSC_Substrate$Suitable == 0, na.rm = T)
      
      Storage$Substrate_25_Suit_Avail[i] <- sum(Avail_Data$Substrate_Suitable == 1, na.rm = T)
      Storage$Substrate_25_UnSuit_Avail[i] <- sum(Avail_Data$Substrate_Suitable == 0, na.rm = T)
      
      Storage$Substrate_25[i] <- Substrate_25
      Storage$Substrate_75[i] <- Substrate_75
      
      con_tab_Suit50_s <- matrix(c(Storage[i,c("Substrate_25_Suit_Used",
                                               "Substrate_25_UnSuit_Used",
                                               "Substrate_25_Suit_Avail",
                                               "Substrate_25_UnSuit_Avail")]),
                                 byrow=T,
                                 nrow=2)
      used_suit_s <- Storage$Substrate_25_Suit_Used[i]
      avail_unsuit_s <- Storage$Substrate_25_UnSuit_Avail[i]
      used_unsuit_s <- Storage$Substrate_25_UnSuit_Used[i]
      avail_suit_s <- Storage$Substrate_25_Suit_Avail[i]
      
      
      t_stat_s <- ((sum(unlist(con_tab_Suit50_s)))^0.5)*
                  ((used_suit_s*avail_unsuit_s) - (used_unsuit_s*avail_suit_s))/
                  (((used_suit_s+avail_suit_s)*
                    (used_unsuit_s+avail_unsuit_s)*
                    (used_unsuit_s+used_suit_s)*
                    (avail_suit_s+avail_unsuit_s))^0.5)
      
      Storage$t_stat_50_Substrate[i] <- t_stat_s 
      
      Storage$p_50_Substrate[i] <- ifelse(t_stat_s > 1.6449, 1, 0)              #1.6449 is the critical value for t-statistic when alpha = 0.05
      
   ##Central 90%
      Storage$Substrate_5_Suit_Used[i] <- sum(HSC_Substrate$Suitable_90 == 1, na.rm = T)
      Storage$Substrate_5_UnSuit_Used[i] <- sum(HSC_Substrate$Suitable_90 == 0, na.rm = T)
      
      Storage$Substrate_5_Suit_Avail[i] <- sum(Avail_Data$Substrate_Suitable_90 == 1, na.rm = T)
      Storage$Substrate_5_UnSuit_Avail[i] <- sum(Avail_Data$Substrate_Suitable_90 == 0, na.rm = T)
      
      Storage$Substrate_5[i] <- Substrate_5
      Storage$Substrate_95[i] <- Substrate_95
      
      con_tab_Suit90_s <- matrix(c(Storage[i,c("Substrate_5_Suit_Used",
                                               "Substrate_5_UnSuit_Used",
                                               "Substrate_5_Suit_Avail",
                                               "Substrate_5_UnSuit_Avail")]),
                                 byrow=T,
                                 nrow=2)
      used_suit_s_90 <- Storage$Substrate_5_Suit_Used[i]
      avail_unsuit_s_90 <- Storage$Substrate_5_UnSuit_Avail[i]
      used_unsuit_s_90 <- Storage$Substrate_5_UnSuit_Used[i]
      avail_suit_s_90 <- Storage$Substrate_5_Suit_Avail[i]
      
      
      t_stat_s_90 <- ((sum(unlist(con_tab_Suit90_s)))^0.5)*
                     ((used_suit_s_90*avail_unsuit_s_90) - (used_unsuit_s_90*avail_suit_s_90))/
                     (((used_suit_s_90+avail_suit_s_90)*
                       (used_unsuit_s_90+avail_unsuit_s_90)*
                       (used_unsuit_s_90+used_suit_s_90)*
                       (avail_suit_s_90+avail_unsuit_s_90))^0.5)
      
      Storage$t_stat_90_Substrate[i] <- t_stat_s_90 
      
      Storage$p_90_Substrate[i] <- ifelse(t_stat_s_90 > 1.6449, 1, 0)           #1.6449 is the critical value for t-statistic when alpha = 0.05
  
  
  ###Depth Test
   #Find Suitable Ranges
    HSC_Depth <- HSC_data[,c('Presence', 'Depth')]                              #replaced y with Presence
    HSC_Depth <- HSC_Depth[order(HSC_Depth$Depth),]                             #organizes Depth in ascending order
    HSC_Depth$Rank <- seq(1,nrow(HSC_Depth))                                    #assigns rank  
    HSC_Depth$Prob <- HSC_Depth$Rank/(nrow(HSC_Depth)+1)                        #determine probability: rank/ n+1.  N = number occupied 
    Depth_25 <- quantile(HSC_Depth$Depth, 0.25, na.rm = TRUE)                   #determine central 50% of observations (between 25 and 75% probability)
    Depth_75 <- quantile(HSC_Depth$Depth, 0.75, na.rm = TRUE)                   #added na.rm = TRUE
    Depth_5 <- quantile(HSC_Depth$Depth, 0.05, na.rm = TRUE)                    #determine central 90% of observations 
    Depth_95 <- quantile(HSC_Depth$Depth, 0.95, na.rm = TRUE)
    HSC_Depth$Suitable <- ifelse(HSC_Depth$Depth >= Depth_25 & HSC_Depth$Depth <= Depth_75, 1, 0) 
    HSC_Depth$Suitable_90 <- ifelse(HSC_Depth$Depth >= Depth_5 & HSC_Depth$Depth <= Depth_95, 1, 0) 
  
   #Classify each available point as suitable or unsuitable
    Avail_Data$Depth_Suitable <- ifelse(Avail_Data$Depth >= Depth_25 & Avail_Data$Depth <= Depth_75, 1, 0) 
    Avail_Data$Depth_Suitable_90 <- ifelse(Avail_Data$Depth >= Depth_5 & Avail_Data$Depth <= Depth_95, 1, 0) 
        #table(HSC_Depth$Presence, HSC_Depth$Suitable)                          #compares number suitable v number unsuitable out of occupied locations
  
   ##Central 50%
      Storage$Depth_25_Suit_Used[i] <- sum(HSC_Depth$Suitable == 1, na.rm = T)
      Storage$Depth_25_UnSuit_Used[i] <- sum(HSC_Depth$Suitable == 0, na.rm = T)
      
      Storage$Depth_25_Suit_Avail[i] <- sum(Avail_Data$Depth_Suitable == 1, na.rm = T)
      Storage$Depth_25_UnSuit_Avail[i] <- sum(Avail_Data$Depth_Suitable == 0, na.rm = T)
      
      Storage$Depth_25[i] <- Depth_25
      Storage$Depth_75[i] <- Depth_75
      
      con_tab_Suit50_d <- matrix(c(Storage[i,c("Depth_25_Suit_Used",
                                               "Depth_25_UnSuit_Used",
                                               "Depth_25_Suit_Avail",
                                               "Depth_25_UnSuit_Avail")]),
                                 byrow=T,
                                 nrow=2)
      used_suit_d <- Storage$Depth_25_Suit_Used[i]
      avail_unsuit_d <- Storage$Depth_25_UnSuit_Avail[i]
      used_unsuit_d <- Storage$Depth_25_UnSuit_Used[i]
      avail_suit_d <- Storage$Depth_25_Suit_Avail[i]
      
      
      t_stat_d <- ((sum(unlist(con_tab_Suit50_d)))^0.5)*
                  ((used_suit_d*avail_unsuit_d) - (used_unsuit_d*avail_suit_d))/
                  (((used_suit_d+avail_suit_d)*
                    (used_unsuit_d+avail_unsuit_d)*
                    (used_unsuit_d+used_suit_d)*
                    (avail_suit_d+avail_unsuit_d))^0.5)
      
      Storage$t_stat_50_Depth[i] <- t_stat_d 
      
      Storage$p_50_Depth[i] <- ifelse(t_stat_d > 1.6449, 1, 0)                  #1.6449 is the critical value for t-statistic when alpha = 0.05
      
   ##Central 90%
      Storage$Depth_5_Suit_Used[i] <- sum(HSC_Depth$Suitable_90 == 1, na.rm = T)
      Storage$Depth_5_UnSuit_Used[i] <- sum(HSC_Depth$Suitable_90 == 0, na.rm = T)
      
      Storage$Depth_5_Suit_Avail[i] <- sum(Avail_Data$Depth_Suitable_90 == 1, na.rm = T)
      Storage$Depth_5_UnSuit_Avail[i] <- sum(Avail_Data$Depth_Suitable_90 == 0, na.rm = T)
      
      Storage$Depth_5[i] <- Depth_5
      Storage$Depth_95[i] <- Depth_95
      
      con_tab_Suit90_d <- matrix(c(Storage[i,c("Depth_5_Suit_Used",
                                               "Depth_5_UnSuit_Used",
                                               "Depth_5_Suit_Avail",
                                               "Depth_5_UnSuit_Avail")]),
                                 byrow=T,
                                 nrow=2)
      used_suit_d_90 <- Storage$Depth_5_Suit_Used[i]
      avail_unsuit_d_90 <- Storage$Depth_5_UnSuit_Avail[i]
      used_unsuit_d_90 <- Storage$Depth_5_UnSuit_Used[i]
      avail_suit_d_90 <- Storage$Depth_5_Suit_Avail[i]
      
      
      t_stat_d_90 <- ((sum(unlist(con_tab_Suit90_d)))^0.5)*
                     ((used_suit_d_90*avail_unsuit_d_90) - (used_unsuit_d_90*avail_suit_d_90))/
                     (((used_suit_d_90+avail_suit_d_90)*
                       (used_unsuit_d_90+avail_unsuit_d_90)*
                       (used_unsuit_d_90+used_suit_d_90)*
                       (avail_suit_d_90+avail_unsuit_d_90))^0.5)
      
      Storage$t_stat_90_Depth[i] <- t_stat_d_90
      Storage$p_90_Depth[i] <- ifelse(t_stat_d_90 > 1.6449, 1, 0)               #1.6449 is the critical value for t-statistic when alpha = 0.05
  
}


##### SAVE #####

View(Storage)
write_csv(Storage, file = Results_File)


##### Results of the RSFs #####

hist(Storage$Velocity)                                                          #par(mfrow=c(3,1)) to show mulitple graphs
mean(Storage$Velocity, na.rm = TRUE)                                            #a negative number here denotes a negative association, and vice versa 
#0.2496401                         #select fast water (positive)
quantile(Storage$Velocity,c(0.025,0.975), na.rm = TRUE) #2.5%  to  97.5%        #If these numbers cross zero it is not significant, if both are negative or both are positive it is significant
#0.2063194 0.3103551              #significant   

hist(Storage$Velocity_full)
mean(Storage$Velocity_full, na.rm = TRUE)
#0.1897555                        #selecting fast water (choosing fastest area within pool- likely head and tail)
quantile(Storage$Velocity_full,c(0.025,0.975), na.rm = TRUE)
#0.1332578 0.2477191              #significant

hist(Storage$Depth)
mean(Storage$Depth, na.rm = TRUE)
# -0.393656                          #avoiding depth in pools 
quantile(Storage$Depth,c(0.025,0.975), na.rm = TRUE)
#-0.6541839 -0.2552037                 #significant

hist(Storage$Depth_full)
mean(Storage$Depth_full, na.rm = TRUE)
# -0.3407555                        #same
quantile(Storage$Depth_full,c(0.025,0.975), na.rm = TRUE)
#-0.6255766 -0.1790696                 #significant

hist(Storage$Substrate)
mean(Storage$Substrate, na.rm = TRUE)
#-0.1059857                          #favoring smaller substrate
quantile(Storage$Substrate,c(0.025,0.975), na.rm = TRUE)
#-0.22554733  0.05504732               #not significant


##### Results for HSC #####
#50%
 #Velocity
  hist(Storage$Velocity_25)
  hist(Storage$Velocity_75)
  
  mean(Storage$Velocity_25, na.rm = TRUE)
  #0.001693
  mean(Storage$Velocity_75, na.rm = TRUE)
  #0.065
  
  hist(Storage$t_stat_50_Velocity, breaks = 100)                   
  abline(v = 1.6449, lwd=2, col="red")                                          #tstats to the right of the red line are significant effects, to the left insignificant
  mean(Storage$p_50_Velocity)                                                   #% of the time that test is significant
  #0
  mean(Storage$t_stat_50_Velocity)
  #-0.8682436
  
 #Depth
  hist(Storage$Depth_25)
  hist(Storage$Depth_75)
  
  mean(Storage$Depth_25, na.rm = TRUE)
  #44.6614
  mean(Storage$Depth_75, na.rm = TRUE)
  #86.1298
  
  hist(Storage$t_stat_50_Depth, breaks = 100)                   
  abline(v = 1.6449, lwd=2, col="red")
  mean(Storage$p_50_Depth)
  #0.0557
  mean(Storage$t_stat_50_Depth)
  #0.4478486
  
 #Substrate
  hist(Storage$Substrate_25)
  hist(Storage$Substrate_75)
  
  mean(Storage$Substrate_25, na.rm = TRUE)
  #1.8871
  mean(Storage$Substrate_75, na.rm = TRUE)
  #5
  
  hist(Storage$t_stat_50_Substrate, breaks = 100)                   
  abline(v = 1.6449, lwd=2, col="red")
  mean(Storage$p_50_Substrate)
  #0.5484
  mean(Storage$t_stat_50_Substrate)
  #1.6756
  
#90%
 #Velocity
  hist(Storage$Velocity_5)
  hist(Storage$Velocity_95)
  
  mean(Storage$Velocity_5, na.rm = TRUE)
  #0
  mean(Storage$Velocity_95, na.rm = TRUE)
  #0.0820949
  
  hist(Storage$t_stat_90_Velocity, breaks = 100)                   
  abline(v = 1.6449, lwd=2, col="red")                                          #tstats to the right of the red line are significant effects, to the left insignificant
  mean(Storage$p_90_Velocity)                                                   #% of the time that test is significant
  #0
  mean(Storage$t_stat_90_Velocity)
  #0.1881763
  
 #Depth
  hist(Storage$Depth_5)
  hist(Storage$Depth_95)
  
  mean(Storage$Depth_5, na.rm = TRUE)
  #33.4
  mean(Storage$Depth_95, na.rm = TRUE)
  #146.5671
  
  hist(Storage$t_stat_90_Depth, breaks = 100)                   
  abline(v = 1.6449, lwd=2, col="red")
  mean(Storage$p_90_Depth)
  #0.1111
  mean(Storage$t_stat_90_Depth)
  #0.5061174
  
 #Substrate
  hist(Storage$Substrate_5)
  hist(Storage$Substrate_95)
  
  mean(Storage$Substrate_5, na.rm = TRUE)
  #1
  mean(Storage$Substrate_95, na.rm = TRUE)
  #5.35574
  
  hist(Storage$t_stat_90_Substrate, breaks = 100)                   
  abline(v = 1.6449, lwd=2, col="red")
  mean(Storage$p_90_Substrate)
  #0.4373
  mean(Storage$t_stat_90_Substrate)
  #1.59212