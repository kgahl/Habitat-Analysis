# Resource Selection Analysis Loop

# ~15 fish per month and ~3 months per season
# ~45 total location per season
# Keep in mind the rule-of-thumb of 10 data points (probably 10 locations in your case) per covariate in your model


#REMEMBER TO Change file names for different runs
            #Change nrow from 100 to 10000


library(dplyr)
library(readr)



##### DATA #####

##Read In
  #All data available
   NotAsFishy <- read_csv("Output/NotAsFishy.csv")
  
  #Read in data file with variable combinations 
   Variable_Combos <- read.csv("Redesign with Jeff Oliver/VelocityDepthSubstrate_Variables.csv")

##Output 
  #Dataframe to write to file
   All_Rows <- NULL

   
   
##### SETUP #####

##Assign combination of variables     
  #Iterate over each row of Variable_Combos                                       #assigns a name to the selected variable from the selected row within Variable_Combo csv file  
  for (row_i in 1:nrow(Variable_Combos)) {
    #Extract values for filtering 
     Species_Name <- Variable_Combos$Species[row_i]
     Stream_Name <- Variable_Combos$Stream[row_i]
     Season_Months <- strsplit(x = Variable_Combos$Season[row_i], split = ",")[[1]]
     Mesohabitat_Name <- Variable_Combos$Mesohabitat[row_i]
    
    #Extract abbreviations for output
     Species_Abb <- Variable_Combos$Species_Abb[row_i]
     Stream_Abb <- Variable_Combos$Stream_Abb[row_i]
     Season_Abb <- Variable_Combos$Season_Abb[row_i]
     Mesohabitat_Abb <- Variable_Combos$Mesohabitat_Abb[row_i]  
  
    
##Filter occupied and available data by stream and species
  Occupied <- NotAsFishy %>%                                                       
    dplyr::filter(Stream == Stream_Name)%>%                                      #dplyr:: before a function lets it know to use the function from the dplyr package       
    dplyr::filter(Species == Species_Name)      
  Available <- NotAsFishy %>%                                                       
    dplyr::filter(Stream == Stream_Name)%>%          
    dplyr::filter(Presence == 0)                                                 #0 = Available
  Combined <- rbind(Occupied, Available)                                         #combine occupied and available
  remove(Occupied, Available)
  
  #Then month and mesohabitat
  Select_Data <- Combined %>%                                                    #[23 observations, 167 available]    -check in console by: table(Select_Data$Presence)
    dplyr::filter(Month %in% Season_Months) %>%
    dplyr::filter(Mesohabitat == Mesohabitat_Name)                                    
  remove(Combined)
  
  #Reality Check
  message("running ", Species_Name, ", ", Stream_Name, ", ", 
          Season_Abb, ", ", Mesohabitat_Name, " (n = ", nrow(Select_Data), ")")
  
  
##Count Individuals
  #Number of Unique
   Unique_Fish <- na.omit(unique(Select_Data$Tag))                                #doesn't count the same fish more than once (gives range of tag numbers)
   Unique_Individuals <- length(Unique_Fish[!is.na(Unique_Fish)])                 #the number of unique fish
  # #Number of Observed
  # Observed <- na.omit(Select_Data$Tag)                                           #
  # Observed_Individuals <- length(Observed[!is.na(Observed)])
  
   
##Calculate mean and SD of each covariate within data subset to be used later for re-scaling (standardizing) Data_i
  Mean_Velocity  <- mean(Select_Data$Velocity,na.rm = T)                        #When fitting regression (GLM) models with multiple covariates or comparing models each containing different covariates it is helpful to 
  SD_Velocity    <- sd(Select_Data$Velocity,na.rm = T)                            #z-score standardize your covariates. This rescales everything to take covariates that were on different units (meters/percent) and 
  Mean_Substrate <- mean(Select_Data$Substrate,na.rm=T)                           #put them on the same scale such that each covaPOate will have mean = 0 and SD = 1. Making beta coefficients directly comparable so that 
  SD_Substrate   <- sd(Select_Data$Substrate,na.rm=T)                             #a larger absolute value of a coefficient means a stronger effect. This is a little tricky with re-sampling because the mean/SD of each 
  Mean_Depth     <- mean(Select_Data$Depth,na.rm = T)                             #iteration will be slightly different on account of having slightly different values. Calculate the mean and SD of your complete data set 
  SD_Depth       <- sd(Select_Data$Depth,na.rm = T)                               #outside of the i-loop (Select_Data) and then using this to do the rescaling inside the i-loop (Data_i) 
  
  
  
##### MIN MAX #####
  
##Find Minimum and Maximum values of used variables
  Counts <- Select_Data  %>%
    dplyr::filter(Species == Species_Name)
  
  Velocity_Min  <- min(Counts$Velocity, na.rm = TRUE)      
  Velocity_Max  <- max(Counts$Velocity, na.rm = TRUE)    
  
  Depth_Min     <- min(Counts$Depth, na.rm = TRUE)       
  Depth_Max     <- max(Counts$Depth, na.rm = TRUE)       
  
  Substrate_Min <- min(Counts$Substrate, na.rm = TRUE)   
  Substrate_Max <- max(Counts$Substrate, na.rm = TRUE)     
  
  #Output
   # MinMax <- data.frame(DataSet = "Select_Data", Velocity_Min, Velocity_Max, Depth_Min, 
   #                     Depth_Max, Substrate_Min, Substrate_Max)
  
  
  
  ##### RESOURCE SELECTION LOOP #####
  
  #Only do resampling when number of individuals is great than 10
  #if(Unique_Individuals < Observed_Individuals){
    ##Create data frame to store coefficients
  Prop_Sample <- 0.75                                                                      #specify the proportion of point resampled (75%)  
  
  N_reps <- 100
    
  Storage <- data.frame(Velocity = rep(NA,N_reps), Substrate = NA,
                          Depth = rep(NA,N_reps), Velocity_full = NA, 
                          Depth_full = NA,
                          
                          Velocity_25=NA, Velocity_75=NA,
                          Velocity_25_Suit_Used=NA, Velocity_25_UnSuit_Avail=NA,
                          Velocity_25_UnSuit_Used=NA, Velocity_25_Suit_Avail=NA,
                          Velocity_t_stat_50=NA, Velocity_p_50 = NA,
                          
                          Substrate_25=NA, Substrate_75=NA,
                          Substrate_25_Suit_Used=NA, Substrate_25_UnSuit_Avail=NA,
                          Substrate_25_UnSuit_Used=NA, Substrate_25_Suit_Avail=NA,
                          Substrate_t_stat_50=NA, Substrate_p_50 = NA,
                          
                          Depth_25=NA, Depth_75=NA,
                          Depth_25_Suit_Used=NA, Depth_25_UnSuit_Avail=NA,
                          Depth_25_UnSuit_Used=NA, Depth_25_Suit_Avail=NA,
                          Depth_t_stat_50=NA, Depth_p_50 = NA,
                          
                          Velocity_5=NA, Velocity_95=NA,
                          Velocity_5_Suit_Used=NA, Velocity_5_UnSuit_Avail=NA,
                          Velocity_5_UnSuit_Used=NA, Velocity_5_Suit_Avail=NA,
                          Velocity_t_stat_90=NA, Velocity_p_90 = NA,
                          
                          Substrate_5=NA, Substrate_95=NA,
                          Substrate_5_Suit_Used=NA, Substrate_5_UnSuit_Avail=NA,
                          Substrate_5_UnSuit_Used=NA, Substrate_5_Suit_Avail=NA,
                          Substrate_t_stat_90=NA, Substrate_p_90 = NA,
                          
                          Depth_5=NA, Depth_95=NA,
                          Depth_5_Suit_Used=NA, Depth_5_UnSuit_Avail=NA,
                          Depth_5_UnSuit_Used=NA, Depth_5_Suit_Avail=NA,
                          Depth_t_stat_90 = NA, Depth_p_90 = NA)
    
    
##Loop
    
  for(i in 1:N_reps){                                                               #do number of reps specified above
      
      Used_Data_i <- NULL                                                           #create empty data frame
      
      #Shows progress of loop in output
      if(i %in% seq(1, N_reps, by = 100)) cat("Starting rep", i, "\n")
      
      #Randomly select 75% of tags
      j_Individuals <- as.vector(na.omit(unique(Select_Data$Tag)))[sample
                                (1:Unique_Individuals,floor(Unique_Individuals*Prop_Sample))]  #selection of tags to choose from (within the tags of Select_Data dataframe, choose 1 unique individual and add it to the list until 75% of individuals are sampled. , floor grabs largest number (brackets give you exact tag numbers)
      
      #Select one observation for each tag
      for(j in j_Individuals){                                                       
        Fish_j <- j                                                                 #'j' is the actual tag number of a fish
        Fish_js_Rows <- Select_Data[which(Select_Data$Tag==Fish_j),]                #grabs Fish j's rows of data
        
        #Select the row to resample from fish j's data (i.e., data_j)
         j_Selected_Row <- sample(seq(1, nrow(Fish_js_Rows)), 1, replace = T)        #randomly selects 1 of multiple observations of fish j
         j_Row_Data <- Fish_js_Rows[j_Selected_Row,]                                 #pulls out the row of data for the above selected fish           

        #Place this randomly sampled row into used_Data_i
         Used_Data_i <- rbind(Used_Data_i, j_Row_Data)
      }
        
        
      # for(j in 1:Unique_Individuals){                                            #create empty data frame
      #   Fish_j <- na.omit(unique(Select_Data$Tag))[j]                               #randomly select one tag number, label it Fish j
      #   Fish_js_Rows <- Select_Data[which(Select_Data$Tag==Fish_j),]                #grabs Fish j's rows of data
      #   
      #   #Randomly select the row to resample from Fish j’s data (i.e., data_j)
      #   j_Selected_Row <- sample(seq(1, nrow(Fish_js_Rows)), 1, replace = T)        #randomly selects 1 of fish j's multiple observations
      #   j_Row_Data  <- Fish_js_Rows[j_Selected_Row,]                                #pulls out the row of data for the above selected fish and places into               
      #   
      #   #Place this randomly sampled row into used_Data_i
      #   Used_Data_i <- rbind(Used_Data_i, j_Row_Data)
      # }
  
      
##Get available data                                                               #Consider all of the available points within a species/site/season/mesohabitat to be equally available across the entire season. Use the same set of available points for every random sample of used point
  All_Avail_Data <- Select_Data[which(Select_Data$Presence == 0), ]                #Separate available data
  Avail_Data <- All_Avail_Data[sample(1:nrow(All_Avail_Data),                      #Randomly select Prop_Sample (75) percent of available points
                                      floor(nrow(All_Avail_Data)*Prop_Sample)),]   

##Combine available with loop created used
  Data_i <- rbind(Used_Data_i, Avail_Data)                                         
      
    
      
##### GENERAL LINEAR MODELING #####
  
  #Add weight column to make binomial GLM equivalent to Inhomogeneous Poisson Point Process Model
   Data_i$weight <- ifelse(Data_i$Presence == 1, 1, 5000)
  
  #Re-scale the covariates (z score scale)
   Data_i$zVelocity <- (Data_i$Velocity - Mean_Velocity)/SD_Velocity
   Data_i$zDepth <- (Data_i$Depth - Mean_Depth)/SD_Depth
   Data_i$zSubstrate <- (Data_i$Substrate - Mean_Substrate)/SD_Substrate
   
##Fit the model
  Model_Vel   <- glm(Presence ~ zVelocity, data = Data_i, 
                         family = binomial, weights = weight)                      #family specifies the details of the model (GLM) used.  
      
  Model_Depth <- glm(Presence ~ zDepth, data = Data_i, 
                         family = binomial, weights = weight)
 
  Model_Subs  <- glm(Presence ~ zSubstrate, data = Data_i, 
                     family = binomial, weights = weight)                     
  
  Model_Full  <- glm(Presence ~ zVelocity + zDepth, data = Data_i,             #model with two covariates (velocity, depth); because of limited observations we can't do all 3 (10/ rule of thumb)
                         family = binomial, weights = weight)   
      
  #Pull out and store coefficients
   Storage$Velocity[i] <- coef(Model_Vel)['zVelocity']      
   Storage$Depth[i] <- coef(Model_Depth)['zDepth'] 
   Storage$Substrate[i] <- coef(Model_Subs)['zSubstrate'] 
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
      
      Storage$Velocity_t_stat_50[i] <- t_stat_v 
      
      Storage$Velocity_p_50[i] <- ifelse(t_stat_v > 1.6449,1,0)                 #1.6449 is the critical value for t-statistic when alpha = 0.05
      
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
      
      Storage$Velocity_t_stat_90[i] <- t_stat_v_90 
      
      Storage$Velocity_p_90[i] <- ifelse(t_stat_v_90 > 1.6449, 1, 0)            #1.6449 is the critical value for t-statistic when alpha = 0.05
   
      
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
      
      Storage$Depth_t_stat_50[i] <- t_stat_d 
      
      Storage$Depth_p_50[i] <- ifelse(t_stat_d > 1.6449, 1, 0)                  #1.6449 is the critical value for t-statistic when alpha = 0.05
      
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
      
      Storage$Depth_t_stat_90[i] <- t_stat_d_90
      Storage$Depth_p_90[i] <- ifelse(t_stat_d_90 > 1.6449, 1, 0)               #1.6449 is the critical value for t-statistic when alpha = 0.05
      
      
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
      
      Storage$Substrate_t_stat_50[i] <- t_stat_s 
      
      Storage$Substrate_p_50[i] <- ifelse(t_stat_s > 1.6449, 1, 0)              #1.6449 is the critical value for t-statistic when alpha = 0.05
      
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
      
      Storage$Substrate_t_stat_90[i] <- t_stat_s_90 
      
      Storage$Substrate_p_90[i] <- ifelse(t_stat_s_90 > 1.6449, 1, 0)           #1.6449 is the critical value for t-statistic when alpha = 0.05
    
      
      
    }
   
  ##### ELSE ##### 
  # } else {
  #   #No resampling needed - too few individuals
  #   i <- 1
  #   Storage <- data.frame(Velocity = NA, Substrate = NA,
  #                         Depth = NA, Velocity_full = NA,
  #                         Depth_full = NA,
  # 
  #                         Velocity_25=NA, Velocity_75=NA,
  #                         Velocity_25_Suit_Used=NA, Velocity_25_UnSuit_Avail=NA,
  #                         Velocity_25_UnSuit_Used=NA, Velocity_25_Suit_Avail=NA,
  #                         t_stat_50_Velocity=NA, p_50_Velocity = NA,
  # 
  #                         Substrate_25=NA, Substrate_75=NA,
  #                         Substrate_25_Suit_Used=NA, Substrate_25_UnSuit_Avail=NA,
  #                         Substrate_25_UnSuit_Used=NA, Substrate_25_Suit_Avail=NA,
  #                         t_stat_50_Substrate=NA, p_50_Substrate = NA,
  # 
  #                         Depth_25=NA, Depth_75=NA,
  #                         Depth_25_Suit_Used=NA, Depth_25_UnSuit_Avail=NA,
  #                         Depth_25_UnSuit_Used=NA, Depth_25_Suit_Avail=NA,
  #                         t_stat_50_Depth=NA, p_50_Depth = NA,
  # 
  #                         Velocity_5=NA, Velocity_95=NA,
  #                         Velocity_5_Suit_Used=NA, Velocity_5_UnSuit_Avail=NA,
  #                         Velocity_5_UnSuit_Used=NA, Velocity_5_Suit_Avail=NA,
  #                         t_stat_90_Velocity=NA, p_90_Velocity = NA,
  # 
  #                         Substrate_5=NA, Substrate_95=NA,
  #                         Substrate_5_Suit_Used=NA, Substrate_5_UnSuit_Avail=NA,
  #                         Substrate_5_UnSuit_Used=NA, Substrate_5_Suit_Avail=NA,
  #                         t_stat_90_Substrate=NA, p_90_Substrate = NA,
  # 
  #                         Depth_5=NA, Depth_95=NA,
  #                         Depth_5_Suit_Used=NA, Depth_5_UnSuit_Avail=NA,
  #                         Depth_5_UnSuit_Used=NA, Depth_5_Suit_Avail=NA,
  #                         t_stat_90_Depth = NA, p_90_Depth = NA)
  # 
  # 
  #   #Rescale/Standardize your covariates
  #   Select_Data$zVelocity  <- (Select_Data$Velocity - Mean_Velocity)/SD_Velocity
  #   Select_Data$zDepth     <- (Select_Data$Depth - Mean_Depth)/SD_Depth
  #   Select_Data$zSubstrate <- (Select_Data$Substrate - Mean_Substrate)/SD_Substrate
  # 
  #   #Add weight column to make binomial GLM equivalent to Inhomogeneous Poisson Point Process Model
  #   Select_Data$weight <- ifelse(Select_Data$Presence == 1, 1, 5000)
  # 
  #   Avail_Data <- Select_Data[which(Select_Data$Presence == 0), ]
  # 
  #   #### RSF
  #   ###General Linear Modeling
  #   #Fit the model
  #   Model_Vel <- glm(Presence ~ zVelocity, data = Select_Data, family = binomial, weights = weight)             #family specifies the details of the model (GLM) used.
  #   Model_Subs <- glm(Presence ~ zSubstrate, data = Select_Data, family = binomial, weights = weight)           #one covariate: substrate
  #   Model_Depth <- glm(Presence ~ zDepth, data = Select_Data, family = binomial, weights = weight)
  #   Model_Full <- glm(Presence ~ zVelocity + zDepth, data = Select_Data, family=binomial, weights = weight)     #model with two covariates (velocity, depth); because there are only 19 observations we can't do all 3 (10/ rule of thumb)
  # 
  #   #Pull out and store coefficients
  #   Storage$Velocity[i] <- coef(Model_Vel)['zVelocity']
  #   Storage$Substrate[i] <- coef(Model_Subs)['zSubstrate']
  #   Storage$Depth[i] <- coef(Model_Depth)['zDepth']
  #   Storage$Velocity_full[i] <- coef(Model_Full)['zVelocity']                    #use if utilizing model-full
  #   Storage$Depth_full[i] <- coef(Model_Full)['zDepth']
  # 
  # 
  # 
  #   ##### HABITAT SUITABILITY CRITERIA 
  #   HSC_data <- filter(Select_Data, Presence == 1)                                    #isolate occupied site data
  # 
  #   ###Velocity
  #   #Find suitable ranges
  #   HSC_Velocity <- HSC_data[,c('Presence','Velocity')]                         #replaced y with Presence
  #   HSC_Velocity <- HSC_Velocity[order(HSC_Velocity$Velocity),]                 #organizes velocity in ascending order
  #   HSC_Velocity$Rank <- seq(1,nrow(HSC_Velocity))                              #assigns rank
  #   HSC_Velocity$Prob <- HSC_Velocity$Rank/(nrow(HSC_Velocity)+1)               #determine probability: rank/ n+1.  N = number occupied
  #   Velocity_25 <- quantile(HSC_Velocity$Velocity, 0.25, na.rm = TRUE)          #determine central 50% of observations (between 25 and 75% probability)
  #   Velocity_75 <- quantile(HSC_Velocity$Velocity, 0.75, na.rm = TRUE)          #added na.rm = TRUE
  #   Velocity_5 <- quantile(HSC_Velocity$Velocity, 0.05, na.rm = TRUE)           #determine central 90% of observations
  #   Velocity_95 <- quantile(HSC_Velocity$Velocity, 0.95, na.rm = TRUE)
  #   HSC_Velocity$Suitable <- ifelse(HSC_Velocity$Velocity >= Velocity_25 & HSC_Velocity$Velocity <= Velocity_75, 1, 0)
  #   HSC_Velocity$Suitable_90 <- ifelse(HSC_Velocity$Velocity >= Velocity_5 & HSC_Velocity$Velocity <= Velocity_95, 1, 0)
  # 
  #   #Classify each available point as suitable or unsuitable
  #   Avail_Data$Velocity_Suitable <- ifelse(Avail_Data$Velocity >= Velocity_25 & Avail_Data$Velocity <= Velocity_75, 1, 0)
  #   Avail_Data$Velocity_Suitable_90 <- ifelse(Avail_Data$Velocity >= Velocity_5 & Avail_Data$Velocity <= Velocity_95, 1, 0)
  #   #table(HSC_Velocity$Presence, HSC_Velocity$Suitable)                             #compares number suitable v number unsuitable out of occupied locations
  # 
  #   ##Central 50%
  #   Storage$Velocity_25_Suit_Used[i] <- sum(HSC_Velocity$Suitable == 1, na.rm = T)
  #   Storage$Velocity_25_UnSuit_Used[i] <- sum(HSC_Velocity$Suitable == 0, na.rm = T)
  # 
  #   Storage$Velocity_25_Suit_Avail[i] <- sum(Avail_Data$Velocity_Suitable == 1, na.rm = T)
  #   Storage$Velocity_25_UnSuit_Avail[i] <- sum(Avail_Data$Velocity_Suitable == 0, na.rm = T)
  # 
  #   Storage$Velocity_25[i] <- Velocity_25
  #   Storage$Velocity_75[i] <- Velocity_75
  # 
  #   con_tab_Suit50_v <- matrix(c(Storage[i,c("Velocity_25_Suit_Used",
  #                                            "Velocity_25_UnSuit_Used",
  #                                            "Velocity_25_Suit_Avail",
  #                                            "Velocity_25_UnSuit_Avail")]),
  #                              byrow=T,
  #                              nrow=2)
  #   used_suit_v <- Storage$Velocity_25_Suit_Used[i]
  #   avail_unsuit_v <- Storage$Velocity_25_UnSuit_Avail[i]
  #   used_unsuit_v <- Storage$Velocity_25_UnSuit_Used[i]
  #   avail_suit_v <- Storage$Velocity_25_Suit_Avail[i]
  # 
  # 
  #   t_stat_v <- ((sum(unlist(con_tab_Suit50_v)))^0.5)*
  #     ((used_suit_v*avail_unsuit_v) - (used_unsuit_v*avail_suit_v))/
  #     (((used_suit_v+avail_suit_v)*
  #         (used_unsuit_v+avail_unsuit_v)*
  #         (used_unsuit_v+used_suit_v)*
  #         (avail_suit_v+avail_unsuit_v))^0.5)
  # 
  #   Storage$t_stat_50_Velocity[i] <- t_stat_v
  # 
  #   Storage$p_50_Velocity[i] <- ifelse(t_stat_v > 1.6449,1,0)                 #1.6449 is the critical value for t-statistic when alpha = 0.05
  # 
  #   ##Central 90%
  #   Storage$Velocity_5_Suit_Used[i] <- sum(HSC_Velocity$Suitable_90==1,na.rm=T)
  #   Storage$Velocity_5_UnSuit_Used[i] <- sum(HSC_Velocity$Suitable_90==0,na.rm=T)
  # 
  #   Storage$Velocity_5_Suit_Avail[i] <- sum(Avail_Data$Velocity_Suitable_90==1,na.rm=T)
  #   Storage$Velocity_5_UnSuit_Avail[i] <- sum(Avail_Data$Velocity_Suitable_90==0,na.rm=T)
  # 
  #   Storage$Velocity_5[i] <- Velocity_5
  #   Storage$Velocity_95[i] <- Velocity_95
  # 
  #   con_tab_Suit90_v <- matrix(c(Storage[i,c("Velocity_5_Suit_Used",
  #                                            "Velocity_5_UnSuit_Used",
  #                                            "Velocity_5_Suit_Avail",
  #                                            "Velocity_5_UnSuit_Avail")]),
  #                              byrow=T,
  #                              nrow=2)
  #   used_suit_v_90 <- Storage$Velocity_5_Suit_Used[i]
  #   avail_unsuit_v_90 <- Storage$Velocity_5_UnSuit_Avail[i]
  #   used_unsuit_v_90 <- Storage$Velocity_5_UnSuit_Used[i]
  #   avail_suit_v_90 <- Storage$Velocity_5_Suit_Avail[i]
  # 
  # 
  #   t_stat_v_90 <- ((sum(unlist(con_tab_Suit90_v)))^0.5)*((used_suit_v_90*avail_unsuit_v_90)-
  #                                                           (used_unsuit_v_90*avail_suit_v_90))/(((used_suit_v_90+avail_suit_v_90)*
  #                                                                                                   (used_unsuit_v_90+avail_unsuit_v_90)*(used_unsuit_v_90+used_suit_v_90)*
  #                                                                                                   (avail_suit_v_90+avail_unsuit_v_90))^0.5)
  # 
  #   Storage$t_stat_90_Velocity[i] <- t_stat_v_90
  # 
  #   Storage$p_90_Velocity[i] <- ifelse(t_stat_v_90 > 1.6449, 1, 0)            #1.6449 is the critical value for t-statistic when alpha = 0.05
  # 
  # 
  #   ###Substrate
  #   #Find suitable ranges
  #   HSC_Substrate <- HSC_data[,c('Presence','Substrate')]                       #replaced y with Presence
  #   HSC_Substrate <- HSC_Substrate[order(HSC_Substrate$Substrate),]             #organizes Substrate in ascending order
  #   HSC_Substrate$Rank <- seq(1,nrow(HSC_Substrate))                            #assigns rank
  #   HSC_Substrate$Prob <- HSC_Substrate$Rank/(nrow(HSC_Substrate)+1)            #determine probability: rank/ n+1.  N = number occupied
  #   Substrate_25 <- quantile(HSC_Substrate$Substrate, 0.25, na.rm = TRUE)       #determine central 50% of observations (between 25 and 75% probability)
  #   Substrate_75 <- quantile(HSC_Substrate$Substrate, 0.75, na.rm = TRUE)       #added na.rm = TRUE
  #   Substrate_5 <- quantile(HSC_Substrate$Substrate, 0.05, na.rm = TRUE)        #determine central 90% of observations
  #   Substrate_95 <- quantile(HSC_Substrate$Substrate, 0.95, na.rm = TRUE)
  #   HSC_Substrate$Suitable <- ifelse(HSC_Substrate$Substrate >= Substrate_25 &
  #                                      HSC_Substrate$Substrate <= Substrate_75, 1, 0)
  #   HSC_Substrate$Suitable_90 <- ifelse(HSC_Substrate$Substrate >= Substrate_5 &
  #                                         HSC_Substrate$Substrate <= Substrate_95, 1, 0)
  # 
  #   #Classify each available point as suitable or unsuitable
  #   Avail_Data$Substrate_Suitable <- ifelse(Avail_Data$Substrate >= Substrate_25 &
  #                                             Avail_Data$Substrate <= Substrate_75, 1, 0)
  #   Avail_Data$Substrate_Suitable_90 <- ifelse(Avail_Data$Substrate >= Substrate_5 &
  #                                                Avail_Data$Substrate <= Substrate_95, 1, 0)
  #   #table(HSC_Substrate$Presence, HSC_Substrate$Suitable)                      #compares number suitable v number unsuitable out of occupied locations
  # 
  #   ##Central 50%
  #   Storage$Substrate_25_Suit_Used[i] <- sum(HSC_Substrate$Suitable == 1, na.rm = T)
  #   Storage$Substrate_25_UnSuit_Used[i] <- sum(HSC_Substrate$Suitable == 0, na.rm = T)
  # 
  #   Storage$Substrate_25_Suit_Avail[i] <- sum(Avail_Data$Substrate_Suitable == 1, na.rm = T)
  #   Storage$Substrate_25_UnSuit_Avail[i] <- sum(Avail_Data$Substrate_Suitable == 0, na.rm = T)
  # 
  #   Storage$Substrate_25[i] <- Substrate_25
  #   Storage$Substrate_75[i] <- Substrate_75
  # 
  #   con_tab_Suit50_s <- matrix(c(Storage[i,c("Substrate_25_Suit_Used",
  #                                            "Substrate_25_UnSuit_Used",
  #                                            "Substrate_25_Suit_Avail",
  #                                            "Substrate_25_UnSuit_Avail")]),
  #                              byrow=T,
  #                              nrow=2)
  #   used_suit_s <- Storage$Substrate_25_Suit_Used[i]
  #   avail_unsuit_s <- Storage$Substrate_25_UnSuit_Avail[i]
  #   used_unsuit_s <- Storage$Substrate_25_UnSuit_Used[i]
  #   avail_suit_s <- Storage$Substrate_25_Suit_Avail[i]
  # 
  # 
  #   t_stat_s <- ((sum(unlist(con_tab_Suit50_s)))^0.5)*
  #     ((used_suit_s*avail_unsuit_s) - (used_unsuit_s*avail_suit_s))/
  #     (((used_suit_s+avail_suit_s)*
  #         (used_unsuit_s+avail_unsuit_s)*
  #         (used_unsuit_s+used_suit_s)*
  #         (avail_suit_s+avail_unsuit_s))^0.5)
  # 
  #   Storage$t_stat_50_Substrate[i] <- t_stat_s
  # 
  #   Storage$p_50_Substrate[i] <- ifelse(t_stat_s > 1.6449, 1, 0)              #1.6449 is the critical value for t-statistic when alpha = 0.05
  # 
  #   ##Central 90%
  #   Storage$Substrate_5_Suit_Used[i] <- sum(HSC_Substrate$Suitable_90 == 1, na.rm = T)
  #   Storage$Substrate_5_UnSuit_Used[i] <- sum(HSC_Substrate$Suitable_90 == 0, na.rm = T)
  # 
  #   Storage$Substrate_5_Suit_Avail[i] <- sum(Avail_Data$Substrate_Suitable_90 == 1, na.rm = T)
  #   Storage$Substrate_5_UnSuit_Avail[i] <- sum(Avail_Data$Substrate_Suitable_90 == 0, na.rm = T)
  # 
  #   Storage$Substrate_5[i] <- Substrate_5
  #   Storage$Substrate_95[i] <- Substrate_95
  # 
  #   con_tab_Suit90_s <- matrix(c(Storage[i,c("Substrate_5_Suit_Used",
  #                                            "Substrate_5_UnSuit_Used",
  #                                            "Substrate_5_Suit_Avail",
  #                                            "Substrate_5_UnSuit_Avail")]),
  #                              byrow=T,
  #                              nrow=2)
  #   used_suit_s_90 <- Storage$Substrate_5_Suit_Used[i]
  #   avail_unsuit_s_90 <- Storage$Substrate_5_UnSuit_Avail[i]
  #   used_unsuit_s_90 <- Storage$Substrate_5_UnSuit_Used[i]
  #   avail_suit_s_90 <- Storage$Substrate_5_Suit_Avail[i]
  # 
  # 
  #   t_stat_s_90 <- ((sum(unlist(con_tab_Suit90_s)))^0.5)*
  #     ((used_suit_s_90*avail_unsuit_s_90) - (used_unsuit_s_90*avail_suit_s_90))/
  #     (((used_suit_s_90+avail_suit_s_90)*
  #         (used_unsuit_s_90+avail_unsuit_s_90)*
  #         (used_unsuit_s_90+used_suit_s_90)*
  #         (avail_suit_s_90+avail_unsuit_s_90))^0.5)
  # 
  #   Storage$t_stat_90_Substrate[i] <- t_stat_s_90
  # 
  #   Storage$p_90_Substrate[i] <- ifelse(t_stat_s_90 > 1.6449, 1, 0)           #1.6449 is the critical value for t-statistic when alpha = 0.05
  # 
  # 
  #   ###Depth Test
  #   #Find Suitable Ranges
  #   HSC_Depth <- HSC_data[,c('Presence', 'Depth')]                              #replaced y with Presence
  #   HSC_Depth <- HSC_Depth[order(HSC_Depth$Depth),]                             #organizes Depth in ascending order
  #   HSC_Depth$Rank <- seq(1,nrow(HSC_Depth))                                    #assigns rank
  #   HSC_Depth$Prob <- HSC_Depth$Rank/(nrow(HSC_Depth)+1)                        #determine probability: rank/ n+1.  N = number occupied
  #   Depth_25 <- quantile(HSC_Depth$Depth, 0.25, na.rm = TRUE)                   #determine central 50% of observations (between 25 and 75% probability)
  #   Depth_75 <- quantile(HSC_Depth$Depth, 0.75, na.rm = TRUE)                   #added na.rm = TRUE
  #   Depth_5 <- quantile(HSC_Depth$Depth, 0.05, na.rm = TRUE)                    #determine central 90% of observations
  #   Depth_95 <- quantile(HSC_Depth$Depth, 0.95, na.rm = TRUE)
  #   HSC_Depth$Suitable <- ifelse(HSC_Depth$Depth >= Depth_25 & HSC_Depth$Depth <= Depth_75, 1, 0)
  #   HSC_Depth$Suitable_90 <- ifelse(HSC_Depth$Depth >= Depth_5 & HSC_Depth$Depth <= Depth_95, 1, 0)
  # 
  #   #Classify each available point as suitable or unsuitable
  #   Avail_Data$Depth_Suitable <- ifelse(Avail_Data$Depth >= Depth_25 & Avail_Data$Depth <= Depth_75, 1, 0)
  #   Avail_Data$Depth_Suitable_90 <- ifelse(Avail_Data$Depth >= Depth_5 & Avail_Data$Depth <= Depth_95, 1, 0)
  #   #table(HSC_Depth$Presence, HSC_Depth$Suitable)                          #compares number suitable v number unsuitable out of occupied locations
  # 
  #   ##Central 50%
  #   Storage$Depth_25_Suit_Used[i] <- sum(HSC_Depth$Suitable == 1, na.rm = T)
  #   Storage$Depth_25_UnSuit_Used[i] <- sum(HSC_Depth$Suitable == 0, na.rm = T)
  # 
  #   Storage$Depth_25_Suit_Avail[i] <- sum(Avail_Data$Depth_Suitable == 1, na.rm = T)
  #   Storage$Depth_25_UnSuit_Avail[i] <- sum(Avail_Data$Depth_Suitable == 0, na.rm = T)
  # 
  #   Storage$Depth_25[i] <- Depth_25
  #   Storage$Depth_75[i] <- Depth_75
  # 
  #   con_tab_Suit50_d <- matrix(c(Storage[i,c("Depth_25_Suit_Used",
  #                                            "Depth_25_UnSuit_Used",
  #                                            "Depth_25_Suit_Avail",
  #                                            "Depth_25_UnSuit_Avail")]),
  #                              byrow=T,
  #                              nrow=2)
  #   used_suit_d <- Storage$Depth_25_Suit_Used[i]
  #   avail_unsuit_d <- Storage$Depth_25_UnSuit_Avail[i]
  #   used_unsuit_d <- Storage$Depth_25_UnSuit_Used[i]
  #   avail_suit_d <- Storage$Depth_25_Suit_Avail[i]
  # 
  # 
  #   t_stat_d <- ((sum(unlist(con_tab_Suit50_d)))^0.5)*
  #     ((used_suit_d*avail_unsuit_d) - (used_unsuit_d*avail_suit_d))/
  #     (((used_suit_d+avail_suit_d)*
  #         (used_unsuit_d+avail_unsuit_d)*
  #         (used_unsuit_d+used_suit_d)*
  #         (avail_suit_d+avail_unsuit_d))^0.5)
  # 
  #   Storage$t_stat_50_Depth[i] <- t_stat_d
  # 
  #   Storage$p_50_Depth[i] <- ifelse(t_stat_d > 1.6449, 1, 0)                  #1.6449 is the critical value for t-statistic when alpha = 0.05
  # 
  #   ##Central 90%
  #   Storage$Depth_5_Suit_Used[i] <- sum(HSC_Depth$Suitable_90 == 1, na.rm = T)
  #   Storage$Depth_5_UnSuit_Used[i] <- sum(HSC_Depth$Suitable_90 == 0, na.rm = T)
  # 
  #   Storage$Depth_5_Suit_Avail[i] <- sum(Avail_Data$Depth_Suitable_90 == 1, na.rm = T)
  #   Storage$Depth_5_UnSuit_Avail[i] <- sum(Avail_Data$Depth_Suitable_90 == 0, na.rm = T)
  # 
  #   Storage$Depth_5[i] <- Depth_5
  #   Storage$Depth_95[i] <- Depth_95
  # 
  #   con_tab_Suit90_d <- matrix(c(Storage[i,c("Depth_5_Suit_Used",
  #                                            "Depth_5_UnSuit_Used",
  #                                            "Depth_5_Suit_Avail",
  #                                            "Depth_5_UnSuit_Avail")]),
  #                              byrow=T,
  #                              nrow=2)
  #   used_suit_d_90 <- Storage$Depth_5_Suit_Used[i]
  #   avail_unsuit_d_90 <- Storage$Depth_5_UnSuit_Avail[i]
  #   used_unsuit_d_90 <- Storage$Depth_5_UnSuit_Used[i]
  #   avail_suit_d_90 <- Storage$Depth_5_Suit_Avail[i]
  # 
  # 
  #   t_stat_d_90 <- ((sum(unlist(con_tab_Suit90_d)))^0.5)*
  #     ((used_suit_d_90*avail_unsuit_d_90) - (used_unsuit_d_90*avail_suit_d_90))/
  #     (((used_suit_d_90+avail_suit_d_90)*
  #         (used_unsuit_d_90+avail_unsuit_d_90)*
  #         (used_unsuit_d_90+used_suit_d_90)*
  #         (avail_suit_d_90+avail_unsuit_d_90))^0.5)
  # 
  #   Storage$t_stat_90_Depth[i] <- t_stat_d_90
  #   Storage$p_90_Depth[i] <- ifelse(t_stat_d_90 > 1.6449, 1, 0)               #1.6449 is the critical value for t-statistic when alpha = 0.05
  # 
  # }###End Else for 10 or Fewer Fish
  
  
  
  
##### SUMMARIZE RESULTS ##### 
  
##Database for each variable combo
  
  Results_File <- paste0(Species_Abb, "_",                           
                         Stream_Abb, "_",
                         Season_Abb, "_",
                         Mesohabitat_Abb, "_StorageTest.csv")                  #Change _Storage if distinguishing between different test runs
  
  write.csv(x = Storage, file = Results_File)
  
  
##Summarize and save results into one master file

  #Storage_Means <- Storage %>%
  #  summarize(across(everything(), mean))
  
  Storage_Means <- Storage %>%
    summarize(across(-c(8,9,10,11,  16,17,18,19,  24,25,26,27,                              #one row with means from Storage columns
                        32,33,34,35,  40,41,42,43,  48,49,50,51), mean)) %>%
    rename('Velocity Coef' = Velocity, 'Depth Coef' = Depth, 'Sub Coef' = Substrate,        
           'VelFull Coef' = Velocity_full, 'DepthFull Coef' = Depth_full) %>%
    cbind(Velocity_Min, Velocity_Max, Depth_Min, Depth_Max, Substrate_Min, Substrate_Max)   #Add Min/Max columns to database

  #RSF Quantiles
  Storage_Means$Velocity_95L <- quantile(Storage$Velocity, probs = 0.025)                                                       #95 CI (2.5 to 97.5)
  Storage_Means$Velocity_95H <- quantile(Storage$Velocity, probs = 0.975)
  Storage_Means$Velocity_80L <- quantile(Storage$Velocity, probs = 0.1)                                                         #80 CI (10 to 90)
  Storage_Means$Velocity_80H <- quantile(Storage$Velocity, probs = 0.9)
  Storage_Means$V_Percent_Support <- ifelse(mean(Storage$Velocity)>0,mean(Storage$Velocity>0),mean(Storage$Velocity<0))         #gives percentage of re-sampled slopes that match the charge(+/-) of the mean estimate 
  
  Storage_Means$Depth_95L <- quantile(Storage$Depth, probs = 0.025)
  Storage_Means$Depth_95H <- quantile(Storage$Depth, probs = 0.975)
  Storage_Means$Depth_80L <- quantile(Storage$Velocity, probs = 0.1)
  Storage_Means$Depth_80H <- quantile(Storage$Velocity, probs = 0.9)
  Storage_Means$D_Percent_Support <- ifelse(mean(Storage$Depth)>0,mean(Storage$Depth>0),mean(Storage$Depth<0))
  
  Storage_Means$Sub_95L <- quantile(Storage$Substrate, probs = 0.025)
  Storage_Means$Sub_95H <- quantile(Storage$Substrate, probs = 0.975)
  Storage_Means$Sub_80L <- quantile(Storage$Velocity, probs = 0.1)
  Storage_Means$Sub_80H <- quantile(Storage$Velocity, probs = 0.9)
  Storage_Means$S_Percent_Support <- ifelse(mean(Storage$Depth)>0,mean(Storage$Depth>0),mean(Storage$Depth<0))
  
  
  Storage_Means$VelFull_95L <- quantile(Storage$Substrate, probs = 0.025)
  Storage_Means$VelFull_95H <- quantile(Storage$Substrate, probs = 0.975)
  Storage_Means$VelFull_80L <- quantile(Storage$Velocity, probs = 0.1)
  Storage_Means$VelFull_80H <- quantile(Storage$Velocity, probs = 0.9)
  Storage_Means$VF_Percent_Support <- ifelse(mean(Storage$Depth)>0,mean(Storage$Depth>0),mean(Storage$Depth<0))
  
  Storage_Means$DepthFull_95L <- quantile(Storage$Substrate, probs = 0.025)
  Storage_Means$DepthFull_95H <- quantile(Storage$Substrate, probs = 0.975)
  Storage_Means$DepthFull_80L <- quantile(Storage$Velocity, probs = 0.1)
  Storage_Means$DepthFull_80H <- quantile(Storage$Velocity, probs = 0.9)
  Storage_Means$DF_Percent_Support <- ifelse(mean(Storage$Depth)>0,mean(Storage$Depth>0),mean(Storage$Depth<0))
  
  
  
  #dput(colnames(Storage_Means))                                                #Prints the column names in order  

  Storage_Means = Storage_Means %>%
    select("Velocity Coef", "Velocity_95L", "Velocity_95H", "Velocity_80L", "Velocity_80H", "V_Percent_Support",                #RSF
           "Depth Coef", "Depth_95L", "Depth_95H", "Depth_80L", "Depth_80H", "D_Percent_Support",
           "Sub Coef", "Sub_95L", "Sub_95H", "Sub_80L", "Sub_80H", "S_Percent_Support",
           "VelFull Coef", "VelFull_95L", "VelFull_95H", "VelFull_80L", "VelFull_80H", "VF_Percent_Support",
           "DepthFull Coef", "DepthFull_95L", "DepthFull_95H", "DepthFull_80L", "DepthFull_80H", "DF_Percent_Support",
           
           "Velocity_25", "Velocity_75", "Velocity_t_stat_50", "Velocity_p_50",                                                 #HSC
           "Velocity_5", "Velocity_95", "Velocity_t_stat_90", "Velocity_p_90", 'Velocity_Min', 'Velocity_Max',
           "Depth_25", "Depth_75", "Depth_t_stat_50", "Depth_p_50",
           "Depth_5", "Depth_95", "Depth_t_stat_90", "Depth_p_90", 'Depth_Min', 'Depth_Max',
           "Substrate_25", "Substrate_75", "Substrate_t_stat_50", "Substrate_p_50",
           "Substrate_5", "Substrate_95", "Substrate_t_stat_90", "Substrate_p_90", 'Substrate_Min', 'Substrate_Max')
  

              
  
  

 #  #Only need quantiles for resampled
 # if (nrow(Storage) >1) {
 #
 #
 #    Storage_Means$Velocity_2.5 <- quantile(Storage$Velocity, probs = 0.025)
 #    Storage_Means$Velocity_97.5 <- quantile(Storage$Velocity, probs = 0.975)
 #
 #    Storage_Means$Depth_2.5 <- quantile(Storage$Depth, probs = 0.025)
 #    Storage_Means$Depth_97.5 <- quantile(Storage$Depth, probs = 0.975)
 #    Storage_Means$Sub_2.5 <- quantile(Storage$Substrate, probs = 0.025)
 #    Storage_Means$Sub_97.5 <- quantile(Storage$Substrate, probs = 0.975)
 #    #Full model
 #    Storage_Means$VelFull_2.5 <- quantile(Storage$Substrate, probs = 0.025)
 #    Storage_Means$DepthFull_97.5 <- quantile(Storage$Substrate, probs = 0.975)
 #
 #     }


  One_Row <- data.frame(Species = Species_Abb,
                        Stream = Stream_Abb,
                        Season = Season_Abb,
                        Mesohabitat = Mesohabitat_Abb) %>%
             bind_cols(Storage_Means)


  if(is.null(All_Rows)){                                                        #The first run through there will be no All Rows
   All_Rows <- One_Row                                                          #So this creates All Rows from One Row
   } else {
    All_Rows <- All_Rows %>%                                                    #Subsequent runs One Row gets added to All Rows
      bind_rows(One_Row)
   }
}

#Output to File
write.csv(x = All_Rows, file = "AllVDSOutputTest.csv")









# Velocity_25_Suit_Used, Velocity_25_Suit_Avail, Velocity_25_UnSuit_Used, Velocity_25_UnSuit_Avail
# Depth_25_Suit_Used, Depth_25_Suit_Avail, Depth_25_UnSuit_Used, Depth_25_UnSuit_Avail,
# Substrate_25_Suit_Used, Substrate_25_Suit_Avail, Substrate_25_UnSuit_Used, Substrate_25_UnSuit_Avail,
# Velocity_5_Suit_Used, Velocity_5_Suit_Avail, Velocity_5_UnSuit_Used, Velocity_5_UnSuit_Avail,
# Depth_5_Suit_Used, Depth_5_Suit_Avail, Depth_5_UnSuit_Used, Depth_5_UnSuit_Avail,
# Substrate_5_Suit_Used, Substrate_5_Suit_Avail, Substrate_5_UnSuit_Used, Substrate_5_UnSuit_Avail

