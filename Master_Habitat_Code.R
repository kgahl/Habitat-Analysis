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
Variable_Combos <- read.csv("Means_Variables.csv")

##Output 
#Dataframe to write to file
All_Rows <- NULL


##### SETUP #####

##Assign combination of variables     
#Iterate over each row of Variable_Combos                                     #assigns a name to the selected variable from the selected row within Variable_Combo csv file  
for (row_i in 1:nrow(Variable_Combos)) {
  #Extract values for filtering 
  Species_Name <- Variable_Combos$Species[row_i]
  Stream_Name <- Variable_Combos$Stream[row_i]
  Group_Months <- strsplit(x = Variable_Combos$Group[row_i], split = ",")[[1]]
  Mesohabitat_Name <- Variable_Combos$Mesohabitat[row_i]
  
  #Extract abbreviations for output
  Species_Abb <- Variable_Combos$Species_Abb[row_i]
  Stream_Abb <- Variable_Combos$Stream_Abb[row_i]
  Group_Abb <- Variable_Combos$Group_Abb[row_i]
  Mesohabitat_Abb <- Variable_Combos$Mesohabitat_Abb[row_i]  
  
  
  ##Filter Data
  #Occupied and available data by stream and species
  Occupied <- NotAsFishy %>%                                                       
    dplyr::filter(Stream == Stream_Name)%>%                                     #dplyr:: before a function lets it know to use the function from the dplyr package       
    dplyr::filter(Species == Species_Name)      
  Available <- NotAsFishy %>%                                                       
    dplyr::filter(Stream == Stream_Name)%>%          
    dplyr::filter(Presence == 0)                                                #0 = Available
  Combined <- rbind(Occupied, Available)                                        #combine occupied and available
  
  #Then month and mesohabitat 
  #For Mesohabitat analysis
  if(Mesohabitat_Name == 'None'){                                                
    Select_Data <- Combined %>%                                                 
      dplyr::filter(Month %in% Group_Months)
    Meso = 1                                                                    #For later if/else functions to separate Mesohabitat run from VDS run
    #For VDS variables analysis
  }else {                                                                      
    Select_Data <- Combined %>%                                                    
      dplyr::filter(Month %in% Group_Months) %>%
      dplyr::filter(Mesohabitat == Mesohabitat_Name)
    Meso = 0
  }
  
  #Only run if there is data
  if(length(which(Select_Data$Presence == 1)) > 1){
    if(length(which(Select_Data$Presence == 0)) > 1){
    } 
    
    ##Reality Check
    message("running ", Species_Name, ", ", Stream_Name, ", ",                    #Shows which combination is running in console when loop is started
            Group_Abb, ", ", Mesohabitat_Name, " (n = ", nrow(Select_Data), ")")
    
    
    ##Count Individuals
    #Number of Unique
    Unique_Fish <- na.omit(unique(Select_Data$Tag))                              #doesn't count the same fish more than once (gives range of tag numbers)
    Unique_Individuals <- length(Unique_Fish[!is.na(Unique_Fish)])               #the number of unique fish
    #Number of Observed
    Observed <- na.omit(Select_Data$Tag)                                           
    Observed_Individuals <- length(Observed[!is.na(Observed)])
    #Number of Available
    Available <- filter(Select_Data, Presence == 0)
    Available <- na.omit(Available$Presence)
    Available <- length(Available[!is.na(Available)])

    
    
    ##Calculate mean and SD of each covariate within data subset to be used later for re-scaling (standardizing) Data_i
    Mean_Velocity  <- mean(Select_Data$Velocity,na.rm = T)                        #When fitting regression (GLM) models with multiple covariates or comparing models each containing different covariates it is helpful to 
    SD_Velocity    <- sd(Select_Data$Velocity,na.rm = T)                            #z-score standardize your covariates. This rescales everything to take covariates that were on different units (meters/percent) and 
    Mean_Depth     <- mean(Select_Data$Depth,na.rm = T)                             #iteration will be slightly different on account of having slightly different values. Calculate the mean and SD of your complete data set 
    SD_Depth       <- sd(Select_Data$Depth,na.rm = T)                               #outside of the i-loop (Select_Data) and then using this to do the rescaling inside the i-loop (Data_i) 
    Mean_Substrate <- mean(Select_Data$Substrate,na.rm=T)                           #put them on the same scale such that each covariate will have mean = 0 and SD = 1. Making beta coefficients directly comparable so that 
    SD_Substrate   <- sd(Select_Data$Substrate,na.rm=T)                             #a larger absolute value of a coefficient means a stronger effect. This is a little tricky with re-sampling because the mean/SD of each 
    Mean_Instream  <- mean(Select_Data$Instream,na.rm=T)
    SD_Instream    <- sd(Select_Data$Instream,na.rm=T)
    Mean_Canopy    <- mean(Select_Data$Canopy,na.rm=T)
    SD_Canopy      <- sd(Select_Data$Canopy,na.rm=T)
    
    
    
    ##### MIN MAX #####
    
    ##Find Minimum and Maximum values of used variables
    Counts <- Select_Data  %>%
      dplyr::filter(Species == Species_Name)                                      #only occupied
    
    MinMax <- NULL                                                                #Min/Max storage
    
    MinMax$Velocity_Min  <- min(Counts$Velocity, na.rm = TRUE)      
    MinMax$Velocity_Max  <- max(Counts$Velocity, na.rm = TRUE)    
    
    MinMax$Depth_Min     <- min(Counts$Depth, na.rm = TRUE)       
    MinMax$Depth_Max     <- max(Counts$Depth, na.rm = TRUE)       
    
    MinMax$Substrate_Min <- min(Counts$Substrate, na.rm = TRUE)   
    MinMax$Substrate_Max <- max(Counts$Substrate, na.rm = TRUE)     
    
    MinMax$Instream_Min     <- min(Counts$Instream, na.rm = TRUE)       
    MinMax$Instream_Max     <- max(Counts$Instream, na.rm = TRUE)       
    
    MinMax$Canopy_Min <- min(Counts$Canopy, na.rm = TRUE)   
    MinMax$Canopy_Max <- max(Counts$Canopy, na.rm = TRUE)     
    
    
    
    ##### LOOP SETUP #####
    
    ##Create data frame to store coefficients
    Prop_Sample <- 0.75                                                           #specify the proportion of point resampled (75%)  
    
    N_reps <- 1
    
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
                          Depth_t_stat_90 = NA, Depth_p_90 = NA,
                          
                          Instream_5=NA, Instream_95=NA,
                          Instream_5_Suit_Used=NA, Instream_5_UnSuit_Avail=NA,
                          Instream_5_UnSuit_Used=NA, Instream_5_Suit_Avail=NA,
                          Instream_t_stat_90 = NA, Instream_p_90 = NA,
                          
                          Canopy_5=NA, Canopy_95=NA,
                          Canopy_Chi = NA, Canopy_P = NA, Canopy_0.05 = NA, 
                          Canopy_0.1 = NA, OccCan = NA, OccNoCan = NA, 
                          AvailCan = NA, AvailNoCan = NA,
                          
                          Ratio_Pool = NA, Ratio_Riffle = NA, Ratio_Run = NA, 
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
      if(Meso == 1){
        
        #Pool
        Avail_Pool <- ifelse(Avail_Data$Mesohabitat == 'Pool', (na.omit(Avail_Data$StreamWidth)),0)
        Avail_Prob_Pool <- (sum(na.omit(Avail_Pool)))/sum(na.omit(Avail_Data$StreamWidth))     
        Used_Prob_Pool<- sum(na.omit(Used_Data_i$Mesohabitat == "Pool"))/sum(!is.na(Used_Data_i$Mesohabitat))
        Storage$Ratio_Pool[i] <- Used_Prob_Pool/Avail_Prob_Pool
        
        #Riffle
        Avail_Riffle <- ifelse(Avail_Data$Mesohabitat == 'Riffle', (na.omit(Avail_Data$StreamWidth)),0)
        Avail_Prob_Riffle <- (sum(na.omit(Avail_Riffle)))/sum(na.omit(Avail_Data$StreamWidth))     
        Used_Prob_Riffle<- sum(na.omit(Used_Data_i$Mesohabitat == "Riffle"))/sum(!is.na(Used_Data_i$Mesohabitat))
        Storage$Ratio_Riffle[i] <- Used_Prob_Riffle/Avail_Prob_Riffle
        
        #Run
        Avail_Run <- ifelse(Avail_Data$Mesohabitat == 'Run', (na.omit(Avail_Data$StreamWidth)),0)
        Avail_Prob_Run <- (sum(na.omit(Avail_Run)))/sum(na.omit(Avail_Data$StreamWidth))     
        Used_Prob_Run<- sum(na.omit(Used_Data_i$Mesohabitat == "Run"))/sum(!is.na(Used_Data_i$Mesohabitat))
        Storage$Ratio_Run[i] <- Used_Prob_Run/Avail_Prob_Run
      }  
      
      
      
      ##### GENERAL LINEAR MODELING #####
      
      ##Add weight column to make binomial GLM equivalent to Inhomogeneous Poisson Point Process Model
      Data_i$weight <- ifelse(Data_i$Presence == 1, 1, 5000)
      
      ##Mesohabitat  
      if(Meso == 1){
        
        #Fit the model
        Model_Meso <- glm(Presence ~ Mesohabitat, data = Data_i, 
                          family = binomial, weights = weight)             
        
        #Pull out and store coefficients
        Storage$Mesohabitat_Riffle[i] <- Model_Meso$coefficients['MesohabitatRiffle']
        Storage$Mesohabitat_Run[i] <- Model_Meso$coefficients['MesohabitatRun']
      }
      
      ##VDS 
      else{
        
        #Re-scale the covariates (z score scale)
        Data_i$zVelocity <- (Data_i$Velocity - Mean_Velocity)/SD_Velocity
        Data_i$zDepth <- (Data_i$Depth - Mean_Depth)/SD_Depth
        Data_i$zSubstrate <- (Data_i$Substrate - Mean_Substrate)/SD_Substrate
        Data_i$zInstream <- (Data_i$Instream - Mean_Instream)/SD_Instream
        Data_i$zCanopy <- (Data_i$Canopy - Mean_Canopy)/SD_Canopy
        
        
        #Fit the model
        Model_Vel   <- glm(Presence ~ zVelocity, data = Data_i, 
                           family = binomial, weights = weight)                 #family specifies the details of the model (GLM) used.  
        
        Model_Depth <- glm(Presence ~ zDepth, data = Data_i, 
                           family = binomial, weights = weight)
        
        Model_Subs  <- glm(Presence ~ zSubstrate, data = Data_i, 
                           family = binomial, weights = weight)                     
        
        Model_Full  <- glm(Presence ~ zVelocity + zDepth, data = Data_i,           #model with two covariates (velocity, depth); because of limited observations we can't do all 3 (10/ rule of thumb)
                           family = binomial, weights = weight)   
        
        Model_Instream  <- glm(Presence ~ zInstream, data = Data_i, 
                               family = binomial, weights = weight)                     
        
        Model_Canopy  <- glm(Presence ~ zCanopy, data = Data_i, 
                             family = binomial, weights = weight)                     
        
        
        #Pull out and store coefficients
        Storage$Velocity[i] <- coef(Model_Vel)['zVelocity']      
        Storage$Depth[i] <- coef(Model_Depth)['zDepth'] 
        Storage$Substrate[i] <- coef(Model_Subs)['zSubstrate'] 
        Storage$Velocity_full[i] <- coef(Model_Full)['zVelocity']                 #use if utilizing model-full
        Storage$Depth_full[i] <- coef(Model_Full)['zDepth']
        Storage$Instream[i] <- coef(Model_Instream)['zInstream'] 
        Storage$Canopy[i] <- coef(Model_Canopy)['zCanopy'] 
      }
      
      
      
      ##### HABITAT SUITABILITY CRITERIA #####
      if(Meso == 0){
        
        HSC_data <- filter(Data_i, Presence == 1)                                     #isolate occupied site data
        
        ###Velocity
        #Find suitable ranges 
        HSC_Velocity <- HSC_data[,c('Presence','Velocity')]                       #replaced y with Presence
        HSC_Velocity <- HSC_Velocity[order(HSC_Velocity$Velocity),] 
        
        # if(nrow(HSC_Velocity)<=0){
        #   HSC_Velocity$Rank <- seq(1,0.001)
        # }else{
        HSC_Velocity$Rank <- seq(1,nrow(HSC_Velocity))
        
        #assigns rank  
        HSC_Velocity$Prob <- HSC_Velocity$Rank/(nrow(HSC_Velocity)+1)             #determine probability: rank/ n+1.  N = number occupied 
        Velocity_25 <- quantile(HSC_Velocity$Velocity, 0.25, na.rm = TRUE)        #quantiles to determine ends of percentage range
        Velocity_75 <- quantile(HSC_Velocity$Velocity, 0.75, na.rm = TRUE)          
        Velocity_5 <- quantile(HSC_Velocity$Velocity, 0.05, na.rm = TRUE)            
        Velocity_95 <- quantile(HSC_Velocity$Velocity, 0.95, na.rm = TRUE)
        
        HSC_Velocity$Suitable <- ifelse(HSC_Velocity$Velocity                     #determine central 50% of observations (between 25 and 75% probability)
                                        >= Velocity_25 & HSC_Velocity$Velocity 
                                        <= Velocity_75, 1, 0) 
        HSC_Velocity$Suitable_90 <- ifelse(HSC_Velocity$Velocity                  #determine central 90% of observations
                                           >= Velocity_5 & HSC_Velocity$Velocity    
                                           <= Velocity_95, 1, 0) 
        
        #Classify each available point as suitable or unsuitable
        Avail_Data$Velocity_Suitable <- ifelse(Avail_Data$Velocity >= Velocity_25 
                                               & Avail_Data$Velocity <= Velocity_75, 
                                               1, 0) 
        Avail_Data$Velocity_Suitable_90 <- ifelse(Avail_Data$Velocity >= Velocity_5 
                                                  & Avail_Data$Velocity <= Velocity_95, 
                                                  1, 0) 
        
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
        
        t_stat_v_90 <- ((sum(unlist(con_tab_Suit90_v)))^0.5)*
          ((used_suit_v_90*avail_unsuit_v_90)-(used_unsuit_v_90*avail_suit_v_90))/
          (((used_suit_v_90+avail_suit_v_90)*
              (used_unsuit_v_90+avail_unsuit_v_90)*
              (used_unsuit_v_90+used_suit_v_90)*
              (avail_suit_v_90+avail_unsuit_v_90))^0.5)
        
        Storage$Velocity_t_stat_90[i] <- t_stat_v_90 
        
        Storage$Velocity_p_90[i] <- ifelse(t_stat_v_90 > 1.6449, 1, 0)            #1.6449 is the critical value for t-statistic when alpha = 0.05
        
        
        ###Depth Test
        #Find Suitable Ranges
        HSC_Depth <- HSC_data[,c('Presence', 'Depth')]                            #replaced y with Presence
        HSC_Depth <- HSC_Depth[order(HSC_Depth$Depth),]                           #organizes Depth in ascending order
        HSC_Depth$Rank <- seq(1,nrow(HSC_Depth))                                  #assigns rank  
        HSC_Depth$Prob <- HSC_Depth$Rank/(nrow(HSC_Depth)+1)                      #determine probability: rank/ n+1.  N = number occupied 
        Depth_25 <- quantile(HSC_Depth$Depth, 0.25, na.rm = TRUE)                   
        Depth_75 <- quantile(HSC_Depth$Depth, 0.75, na.rm = TRUE)                   
        Depth_5 <- quantile(HSC_Depth$Depth, 0.05, na.rm = TRUE)                     
        Depth_95 <- quantile(HSC_Depth$Depth, 0.95, na.rm = TRUE)
        
        HSC_Depth$Suitable <- ifelse(HSC_Depth$Depth                              #determine central 50% of observations (between 25 and 75% probability)
                                     >= Depth_25 & HSC_Depth$Depth 
                                     <= Depth_75, 1, 0) 
        HSC_Depth$Suitable_90 <- ifelse(HSC_Depth$Depth                           #determine central 90% of observations
                                        >= Depth_5 & HSC_Depth$Depth 
                                        <= Depth_95, 1, 0) 
        
        #Classify each available point as suitable or unsuitable
        Avail_Data$Depth_Suitable <- ifelse(Avail_Data$Depth >= Depth_25 
                                            & Avail_Data$Depth <= Depth_75, 
                                            1, 0) 
        Avail_Data$Depth_Suitable_90 <- ifelse(Avail_Data$Depth >= Depth_5 
                                               & Avail_Data$Depth <= Depth_95, 
                                               1, 0) 
        
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
        HSC_Substrate <- HSC_data[,c('Presence','Substrate')]                     #replaced y with Presence
        HSC_Substrate <- HSC_Substrate[order(HSC_Substrate$Substrate),]           #organizes Substrate in ascending order
        HSC_Substrate$Rank <- seq(1,nrow(HSC_Substrate))                          #assigns rank  
        HSC_Substrate$Prob <- HSC_Substrate$Rank/(nrow(HSC_Substrate)+1)          #determine probability: rank/ n+1.  N = number occupied 
        Substrate_25 <- quantile(HSC_Substrate$Substrate, 0.25, na.rm = TRUE)       
        Substrate_75 <- quantile(HSC_Substrate$Substrate, 0.75, na.rm = TRUE)       
        Substrate_5 <- quantile(HSC_Substrate$Substrate, 0.05, na.rm = TRUE)         
        Substrate_95 <- quantile(HSC_Substrate$Substrate, 0.95, na.rm = TRUE)
        
        HSC_Substrate$Suitable <-ifelse(HSC_Substrate$Substrate >= Substrate_25   #determine central 50% of observations (between 25 and 75% probability)
                                        & HSC_Substrate$Substrate <= Substrate_75,
                                        1, 0) 
        HSC_Substrate$Suitable_90 <-ifelse(HSC_Substrate$Substrate >= Substrate_5 #determine central 90% of observations
                                           & HSC_Substrate$Substrate <= Substrate_95, 
                                           1, 0) 
        
        #Classify each available point as suitable or unsuitable
        Avail_Data$Substrate_Suitable <- ifelse(Avail_Data$Substrate >= Substrate_25 
                                                &Avail_Data$Substrate <= Substrate_75,
                                                1, 0) 
        Avail_Data$Substrate_Suitable_90 <- ifelse(Avail_Data$Substrate >= Substrate_5 
                                                   & Avail_Data$Substrate <= Substrate_95, 
                                                   1, 0) 
        
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
        
        t_stat_s_90 <-((sum(unlist(con_tab_Suit90_s)))^0.5)*
          ((used_suit_s_90*avail_unsuit_s_90) - (used_unsuit_s_90*avail_suit_s_90))/
          (((used_suit_s_90+avail_suit_s_90)*
              (used_unsuit_s_90+avail_unsuit_s_90)*
              (used_unsuit_s_90+used_suit_s_90)*
              (avail_suit_s_90+avail_unsuit_s_90))^0.5)
        
        Storage$Substrate_t_stat_90[i] <- t_stat_s_90 
        
        Storage$Substrate_p_90[i] <- ifelse(t_stat_s_90 > 1.6449, 1, 0)           #1.6449 is the critical value for t-statistic when alpha = 0.05
        
        
        ###Instream
        #Find suitable ranges 
        HSC_Instream <- HSC_data[,c('Presence','Instream')]                       #replaced y with Presence
        HSC_Instream <- HSC_Instream[order(HSC_Instream$Instream),]               #organizes Instream in ascending order
        HSC_Instream$Rank <- seq(1,nrow(HSC_Instream))                            #assigns rank  
        HSC_Instream$Prob <- HSC_Instream$Rank/(nrow(HSC_Instream)+1)             #determine probability: rank/ n+1.  N = number occupied 
        Instream_25 <- quantile(HSC_Instream$Instream, 0.25, na.rm = TRUE)        #quantiles to determine ends of percentage range
        Instream_75 <- quantile(HSC_Instream$Instream, 0.75, na.rm = TRUE)          
        Instream_5 <- quantile(HSC_Instream$Instream, 0.05, na.rm = TRUE)            
        Instream_95 <- quantile(HSC_Instream$Instream, 0.95, na.rm = TRUE)
        
        HSC_Instream$Suitable <- ifelse(HSC_Instream$Instream                     #determine central 50% of observations (between 25 and 75% probability)
                                        >= Instream_25 & HSC_Instream$Instream 
                                        <= Instream_75, 1, 0) 
        HSC_Instream$Suitable_90 <- ifelse(HSC_Instream$Instream                  #determine central 90% of observations
                                           >= Instream_5 & HSC_Instream$Instream    
                                           <= Instream_95, 1, 0) 
        
        #Classify each available point as suitable or unsuitable
        Avail_Data$Instream_Suitable <- ifelse(Avail_Data$Instream >= Instream_25 
                                               & Avail_Data$Instream <= Instream_75, 
                                               1, 0) 
        Avail_Data$Instream_Suitable_90 <- ifelse(Avail_Data$Instream >= Instream_5 
                                                  & Avail_Data$Instream <= Instream_95, 
                                                  1, 0) 
        
        ##Central 50%
        Storage$Instream_25_Suit_Used[i] <- sum(HSC_Instream$Suitable == 1, na.rm = T)
        Storage$Instream_25_UnSuit_Used[i] <- sum(HSC_Instream$Suitable == 0, na.rm = T)
        
        Storage$Instream_25_Suit_Avail[i] <- sum(Avail_Data$Instream_Suitable == 1, na.rm = T)
        Storage$Instream_25_UnSuit_Avail[i] <- sum(Avail_Data$Instream_Suitable == 0, na.rm = T)
        
        Storage$Instream_25[i] <- Instream_25
        Storage$Instream_75[i] <- Instream_75
        
        con_tab_Suit50_i <- matrix(c(Storage[i,c("Instream_25_Suit_Used",
                                                 "Instream_25_UnSuit_Used",
                                                 "Instream_25_Suit_Avail",
                                                 "Instream_25_UnSuit_Avail")]),
                                   byrow=T,
                                   nrow=2)
        
        used_suit_i <- Storage$Instream_25_Suit_Used[i]
        avail_unsuit_i <- Storage$Instream_25_UnSuit_Avail[i]
        used_unsuit_i <- Storage$Instream_25_UnSuit_Used[i]
        avail_suit_i <- Storage$Instream_25_Suit_Avail[i]
        
        t_stat_i <- ((sum(unlist(con_tab_Suit50_i)))^0.5)*
          ((used_suit_i*avail_unsuit_i) - (used_unsuit_i*avail_suit_i))/
          (((used_suit_i+avail_suit_i)*
              (used_unsuit_i+avail_unsuit_i)*
              (used_unsuit_i+used_suit_i)*
              (avail_suit_i+avail_unsuit_i))^0.5)
        
        Storage$Instream_t_stat_50[i] <- t_stat_i 
        
        Storage$Instream_p_50[i] <- ifelse(t_stat_i > 1.6449,1,0)                 #1.6449 is the critical value for t-statistic when alpha = 0.05
        
        ##Central 90%
        Storage$Instream_5_Suit_Used[i] <- sum(HSC_Instream$Suitable_90==1,na.rm=T)
        Storage$Instream_5_UnSuit_Used[i] <- sum(HSC_Instream$Suitable_90==0,na.rm=T)
        
        Storage$Instream_5_Suit_Avail[i] <- sum(Avail_Data$Instream_Suitable_90==1,na.rm=T)
        Storage$Instream_5_UnSuit_Avail[i] <- sum(Avail_Data$Instream_Suitable_90==0,na.rm=T)
        
        Storage$Instream_5[i] <- Instream_5
        Storage$Instream_95[i] <- Instream_95
        
        con_tab_Suit90_i <- matrix(c(Storage[i,c("Instream_5_Suit_Used",
                                                 "Instream_5_UnSuit_Used",
                                                 "Instream_5_Suit_Avail",
                                                 "Instream_5_UnSuit_Avail")]),
                                   byrow=T,
                                   nrow=2)
        
        used_suit_i_90 <- Storage$Instream_5_Suit_Used[i]
        avail_unsuit_i_90 <- Storage$Instream_5_UnSuit_Avail[i]
        used_unsuit_i_90 <- Storage$Instream_5_UnSuit_Used[i]
        avail_suit_i_90 <- Storage$Instream_5_Suit_Avail[i]
        
        t_stat_i_90 <- ((sum(unlist(con_tab_Suit90_i)))^0.5)*
          ((used_suit_i_90*avail_unsuit_i_90)-(used_unsuit_i_90*avail_suit_i_90))/
          (((used_suit_i_90+avail_suit_i_90)*
              (used_unsuit_i_90+avail_unsuit_i_90)*
              (used_unsuit_i_90+used_suit_i_90)*
              (avail_suit_i_90+avail_unsuit_i_90))^0.5)
        
        Storage$Instream_t_stat_90[i] <- t_stat_i_90 
        
        Storage$Instream_p_90[i] <- ifelse(t_stat_i_90 > 1.6449, 1, 0)            #1.6449 is the critical value for t-statistic when alpha = 0.05
        
        
        ###Canopy
        #Count of each category 
        Occupied_Canopy <- sum(na.omit(Used_Data_i$Canopy > 0))      
        Available_Canopy <- sum(na.omit(Avail_Data$Canopy > 0))      
        Occupied_NoCanopy <- sum(na.omit(Used_Data_i$Canopy == 0))   
        Available_NoCanopy <- sum(na.omit(Avail_Data$Canopy == 0))   
        
        #Restructure into a matrix 
        Canopy_Matrix <- matrix(c(Occupied_Canopy,   Available_Canopy,            #Occupied (observed) should be in left column, Available (expected) should be in right column
                                  Occupied_NoCanopy, Available_NoCanopy), 
                                byrow = T, nrow = 2)
        
        #Store Chi square value and p value 
        Chi_df <- as.data.frame(t(unlist(chisq.test(Canopy_Matrix, 
                                                    correct = FALSE))), 
                                quote=FALSE)                                    #run chi square, separate each value (unlist), place into dataframe (as.data.frame), transpose rows into columns (t), remove quotations (quote = false), don't apply continuity correction (correct = False) 
        
        Storage$Canopy_Chi[i] <- as.numeric(Chi_df$`statistic.X-squared`)
        Storage$Canopy_P[i]   <- as.numeric(Chi_df$p.value)
        Storage$OccCan[i]     <- Occupied_Canopy
        Storage$OccNoCan[i]   <- Occupied_NoCanopy
        Storage$AvailCan[i]   <- Available_Canopy
        Storage$AvailNoCan[i] <- Available_NoCanopy
        
        #Setup for support probability
        Storage$C_Prob_Support0.05[i] <- ifelse(Storage$Canopy_P > 0.05, 0, 1)   #0 shows occupied is not different from available 
      }  
    }
    
    ##### SUMMARIZE RESULTS ##### 
    
    #### Storage Database ####
    
    #for each variable combo
    Results_File <- paste0(Species_Abb, "_",                           
                           Stream_Abb, "_",
                           Group_Abb, "_",
                           Mesohabitat_Abb, "_Tolerance_March.csv")                   #Change _Storage.csv if distinguishing between different test runs
    
    write.csv(x = Storage, file = Results_File)
    
    
    
    #### Storage Means ####
    #Summarize and save results into one master file
    
    
    ##Mesohabitat
    if (Meso == 1) {
      
      Storage_Means <- Storage %>%
        summarize(across(c(Ratio_Pool, Ratio_Riffle, Ratio_Run,
                           Mesohabitat_Riffle, Mesohabitat_Run), mean))                                         #create and pull out means of Mesohabitat storage columns
      
      #Mesohabitat Ratio and RSF Quantiles
      #Pool Ratio
      Storage_Means$RPool_95L <- quantile(Storage$Ratio_Pool, probs = 0.025, na.rm = TRUE)      #95 CI (2.5 to 97.5)
      Storage_Means$RPool_95H <- quantile(Storage$Ratio_Pool, probs = 0.975, na.rm = TRUE)
      Storage_Means$RPool_80L <- quantile(Storage$Ratio_Pool, probs = 0.1, na.rm = TRUE)                                                       
      Storage_Means$RPool_80H <- quantile(Storage$Ratio_Pool, probs = 0.9, na.rm = TRUE)
      Storage_Means$RPool_Percent_Support <- ifelse(mean(Storage$Ratio_Pool)>1,   #for Selection Ratio 1 is the measure of proportional use (<1 = avoid, >1 = select)
                                                    mean(Storage$Ratio_Pool>1),
                                                    mean(Storage$Ratio_Pool<1))   #gives percentage of re-sampled slopes that match the charge(+/-) of the mean estimate 
      #Riffle Ratio
      Storage_Means$RRiffle_95L <- quantile(Storage$Ratio_Riffle, probs = 0.025, na.rm = TRUE)                                                       
      Storage_Means$RRiffle_95H <- quantile(Storage$Ratio_Riffle, probs = 0.975, na.rm = TRUE)
      Storage_Means$RRiffle_80L <- quantile(Storage$Ratio_Riffle, probs = 0.1, na.rm = TRUE)                                                       
      Storage_Means$RRiffle_80H <- quantile(Storage$Ratio_Riffle, probs = 0.9, na.rm = TRUE)
      Storage_Means$RRiffle_Percent_Support <- ifelse(mean(Storage$Ratio_Riffle)>1,
                                                      mean(Storage$Ratio_Riffle>1),
                                                      mean(Storage$Ratio_Riffle<1))
      #Run Ratio
      Storage_Means$RRun_95L <- quantile(Storage$Ratio_Run, probs = 0.025, na.rm = TRUE)                                                       
      Storage_Means$RRun_95H <- quantile(Storage$Ratio_Run, probs = 0.975, na.rm = TRUE)
      Storage_Means$RRun_80L <- quantile(Storage$Ratio_Run, probs = 0.1, na.rm = TRUE)                                                       
      Storage_Means$RRun_80H <- quantile(Storage$Ratio_Run, probs = 0.9, na.rm = TRUE)
      Storage_Means$RRun_Percent_Support <- ifelse(mean(Storage$Ratio_Run)>1,
                                                   mean(Storage$Ratio_Run>1),
                                                   mean(Storage$Ratio_Run<1))
      #Riffle RSF
      Storage_Means$MesoRiffle_95L <- quantile(Storage$Mesohabitat_Riffle, probs = 0.025, na.rm = TRUE)                                                       
      Storage_Means$MesoRiffle_95H <- quantile(Storage$Mesohabitat_Riffle, probs = 0.975, na.rm = TRUE)
      Storage_Means$MesoRiffle_80L <- quantile(Storage$Mesohabitat_Riffle, probs = 0.1, na.rm = TRUE)                                                       
      Storage_Means$MesoRiffle_80H <- quantile(Storage$Mesohabitat_Riffle, probs = 0.9, na.rm = TRUE)
      Storage_Means$MesoRiffle_Percent_Support <- ifelse(mean(Storage$Mesohabitat_Riffle)>0,       #For GLM RSFs 0 is the divide, - equals avoid, + equals selection
                                                         mean(Storage$Mesohabitat_Riffle>0),
                                                         mean(Storage$Mesohabitat_Riffle<0))
      #Run RSF
      Storage_Means$MesoRun_95L <- quantile(Storage$Mesohabitat_Run, probs = 0.025, na.rm = TRUE)                                                       
      Storage_Means$MesoRun_95H <- quantile(Storage$Mesohabitat_Run, probs = 0.975, na.rm = TRUE)
      Storage_Means$MesoRun_80L <- quantile(Storage$Mesohabitat_Run, probs = 0.1, na.rm = TRUE)                                                       
      Storage_Means$MesoRun_80H <- quantile(Storage$Mesohabitat_Run, probs = 0.9, na.rm = TRUE)
      Storage_Means$MesoRun_Percent_Support <- ifelse(mean(Storage$Mesohabitat_Run)>0,                   
                                                      mean(Storage$Mesohabitat_Run>0),
                                                      mean(Storage$Mesohabitat_Run<0))
      
      Storage_Means = Storage_Means %>%
        select("Ratio_Pool", "RPool_95L", "RPool_95H", "RPool_80L", "RPool_80H", "RPool_Percent_Support", 
               "Ratio_Riffle", "RRiffle_95L", "RRiffle_95H", "RRiffle_80L", "RRiffle_80H", "RRiffle_Percent_Support", 
               "Ratio_Run", "RRun_95L", "RRun_95H", "RRun_80L", "RRun_80H", "RRun_Percent_Support", 
               "Mesohabitat_Riffle", "MesoRiffle_95L", "MesoRiffle_95H", "MesoRiffle_80L", "MesoRiffle_80H", "MesoRiffle_Percent_Support", 
               "Mesohabitat_Run", "MesoRun_95L", "MesoRun_95H", "MesoRun_80L", "MesoRun_80H", "MesoRun_Percent_Support") %>%
        mutate(across(where(is.numeric), round, 3))
      
    }
    ##VDS
    else{
      
      mean(Storage$Canopy_Chi)
      
      Storage_Means <- Storage %>%
        summarize(across(-c(Velocity_5_Suit_Used, Velocity_5_Suit_Avail, Velocity_5_UnSuit_Used, Velocity_5_UnSuit_Avail, 
                            Substrate_5_Suit_Used, Substrate_5_Suit_Avail, Substrate_5_UnSuit_Used, Substrate_5_UnSuit_Avail,
                            Depth_5_Suit_Used, Depth_5_Suit_Avail, Depth_5_UnSuit_Used, Depth_5_UnSuit_Avail,
                            Instream_5_Suit_Used, Instream_5_Suit_Avail, Instream_5_UnSuit_Used, Instream_5_UnSuit_Avail, 
                            Velocity_25_Suit_Used, Velocity_25_Suit_Avail, Velocity_25_UnSuit_Used, Velocity_25_UnSuit_Avail, 
                            Substrate_25_Suit_Used, Substrate_25_Suit_Avail, Substrate_25_UnSuit_Used, Substrate_25_UnSuit_Avail,
                            Depth_25_Suit_Used, Depth_25_Suit_Avail, Depth_25_UnSuit_Used, Depth_25_UnSuit_Avail,
                            Instream_25_Suit_Used, Instream_25_Suit_Avail, Instream_25_UnSuit_Used, Instream_25_UnSuit_Avail, 
                            OccCan, OccNoCan, AvailCan, AvailNoCan), mean)) %>%
        rename('Velocity Coef' = Velocity, 'Depth Coef' = Depth, 'Sub Coef' = Substrate,        
               'VelFull Coef' = Velocity_full, 'DepthFull Coef' = Depth_full,
               'Instream Coef' = Instream, 'Canopy Coef' = Canopy) %>%
        bind_cols(MinMax)                                                                 #Add Min/Max columns to database
      
      
      #VDS RSF Quantiles
      #Velocity RSF
      Storage_Means$Velocity_95L <- quantile(Storage$Velocity, probs = 0.025, na.rm = TRUE)             #95 CI (2.5 to 97.5)
      Storage_Means$Velocity_95H <- quantile(Storage$Velocity, probs = 0.975, na.rm = TRUE)
      Storage_Means$Velocity_80L <- quantile(Storage$Velocity, probs = 0.1, na.rm = TRUE)               #80 CI (10 to 90)
      Storage_Means$Velocity_80H <- quantile(Storage$Velocity, probs = 0.9, na.rm = TRUE)
      Storage_Means$V_Percent_Support <- ifelse(mean(Storage$Velocity)>0,
                                                mean(Storage$Velocity>0),
                                                mean(Storage$Velocity<0))                 #gives percentage of re-sampled slopes that match the charge(+/-) of the mean estimate 
      #Depth RSF
      Storage_Means$Depth_95L <- quantile(Storage$Depth, probs = 0.025, na.rm = TRUE)
      Storage_Means$Depth_95H <- quantile(Storage$Depth, probs = 0.975, na.rm = TRUE)
      Storage_Means$Depth_80L <- quantile(Storage$Depth, probs = 0.1, na.rm = TRUE)
      Storage_Means$Depth_80H <- quantile(Storage$Depth, probs = 0.9, na.rm = TRUE)
      Storage_Means$D_Percent_Support <- ifelse(mean(Storage$Depth)>0,
                                                mean(Storage$Depth>0),
                                                mean(Storage$Depth<0))
      #Substrate RSF 
      Storage_Means$Sub_95L <- quantile(Storage$Substrate, probs = 0.025, na.rm = TRUE)
      Storage_Means$Sub_95H <- quantile(Storage$Substrate, probs = 0.975, na.rm = TRUE)
      Storage_Means$Sub_80L <- quantile(Storage$Substrate, probs = 0.1, na.rm = TRUE)
      Storage_Means$Sub_80H <- quantile(Storage$Substrate, probs = 0.9, na.rm = TRUE)
      Storage_Means$S_Percent_Support <- ifelse(mean(Storage$Substrate)>0,
                                                mean(Storage$Substrate>0),
                                                mean(Storage$Substrate<0))
      #Velocity Full RSF
      Storage_Means$VelFull_95L <- quantile(Storage$Velocity_full, probs = 0.025, na.rm = TRUE)
      Storage_Means$VelFull_95H <- quantile(Storage$Velocity_full, probs = 0.975, na.rm = TRUE)
      Storage_Means$VelFull_80L <- quantile(Storage$Velocity_full, probs = 0.1, na.rm = TRUE)
      Storage_Means$VelFull_80H <- quantile(Storage$Velocity_full, probs = 0.9, na.rm = TRUE)
      Storage_Means$VF_Percent_Support <- ifelse(mean(Storage$Velocity_full)>0,
                                                 mean(Storage$Velocity_full>0),
                                                 mean(Storage$Velocity_full<0))
      #Depth Full RSF
      Storage_Means$DepthFull_95L <- quantile(Storage$Depth_full, probs = 0.025, na.rm = TRUE)
      Storage_Means$DepthFull_95H <- quantile(Storage$Depth_full, probs = 0.975, na.rm = TRUE)
      Storage_Means$DepthFull_80L <- quantile(Storage$Depth_full, probs = 0.1, na.rm = TRUE)
      Storage_Means$DepthFull_80H <- quantile(Storage$Depth_full, probs = 0.9, na.rm = TRUE)
      Storage_Means$DF_Percent_Support <- ifelse(mean(Storage$Depth_full)>0,
                                                 mean(Storage$Depth_full>0),
                                                 mean(Storage$Depth_full<0))
      #Instream RSF
      Storage_Means$Instream_95L <- quantile(Storage$Instream, probs = 0.025, na.rm = TRUE)
      Storage_Means$Instream_95H <- quantile(Storage$Instream, probs = 0.975, na.rm = TRUE)
      Storage_Means$Instream_80L <- quantile(Storage$Instream, probs = 0.1, na.rm = TRUE)
      Storage_Means$Instream_80H <- quantile(Storage$Instream, probs = 0.9, na.rm = TRUE)
      Storage_Means$I_Percent_Support <- ifelse(mean(Storage$Instream)>0,
                                                mean(Storage$Instream>0),
                                                mean(Storage$Instream<0))
      #Canopy RSF
      Storage_Means$Canopy_95L <- quantile(Storage$Canopy, probs = 0.025, na.rm = TRUE)
      Storage_Means$Canopy_95H <- quantile(Storage$Canopy, probs = 0.975, na.rm = TRUE)
      Storage_Means$Canopy_80L <- quantile(Storage$Canopy, probs = 0.1, na.rm = TRUE)
      Storage_Means$Canopy_80H <- quantile(Storage$Canopy, probs = 0.9, na.rm = TRUE)
      Storage_Means$C_Percent_Support <- ifelse(mean(Storage$Canopy)>0,
                                                mean(Storage$Canopy>0),
                                                mean(Storage$Canopy<0))
      
      
      
      
      #Organize and round dataframe
      Storage_Means = Storage_Means %>%                                                                                             #Use  dput(colnames(Storage_Means))  to print all column namesr 
        select("Velocity Coef", "Velocity_95L", "Velocity_95H", "Velocity_80L", "Velocity_80H", "V_Percent_Support",                #RSF
               "Depth Coef", "Depth_95L", "Depth_95H", "Depth_80L", "Depth_80H", "D_Percent_Support",
               "Sub Coef", "Sub_95L", "Sub_95H", "Sub_80L", "Sub_80H", "S_Percent_Support",
               "VelFull Coef", "VelFull_95L", "VelFull_95H", "VelFull_80L", "VelFull_80H", "VF_Percent_Support",
               "DepthFull Coef", "DepthFull_95L", "DepthFull_95H", "DepthFull_80L", "DepthFull_80H", "DF_Percent_Support",
               "Instream Coef", "Instream_95L", "Instream_95H", "Instream_80L", "Instream_80H", "I_Percent_Support",
               "Canopy Coef", "Canopy_95L", "Canopy_95H", "Canopy_80L", "Canopy_80H", "C_Percent_Support",
               
               "Velocity_25", "Velocity_75", "Velocity_t_stat_50", "Velocity_p_50",                                                 #HSC
               "Velocity_5", "Velocity_95", "Velocity_t_stat_90", "Velocity_p_90", 'Velocity_Min', 'Velocity_Max',
               "Depth_25", "Depth_75", "Depth_t_stat_50", "Depth_p_50",
               "Depth_5", "Depth_95", "Depth_t_stat_90", "Depth_p_90", 'Depth_Min', 'Depth_Max',
               "Substrate_25", "Substrate_75", "Substrate_t_stat_50", "Substrate_p_50",
               "Substrate_5", "Substrate_95", "Substrate_t_stat_90", "Substrate_p_90", 'Substrate_Min', 'Substrate_Max',
               "Instream_25", "Instream_75", "Instream_t_stat_50", "Instream_p_50",                                                 
               "Instream_5", "Instream_95", "Instream_t_stat_90", "Instream_p_90", 'Instream_Min', 'Instream_Max',
               "Canopy_Chi", "Canopy_P", "C_Prob_Support0.05", 'Canopy_Min', 'Canopy_Max') %>%
        
        mutate(across(c("Velocity_t_stat_50", "Velocity_t_stat_90", "Depth_t_stat_50", "Depth_t_stat_90",                       #round to match critical tstat
                        "Substrate_t_stat_50", "Substrate_t_stat_90", "Instream_t_stat_50", "Instream_t_stat_90",
                        "Instream_t_stat_50",  "Instream_t_stat_90", "Canopy_Chi"
        ), round, 4)) %>%
        
        mutate(across(c("Velocity Coef", "Depth Coef", "Sub Coef", "VelFull Coef", "DepthFull Coef", "Instream Coef", "Canopy Coef",             #regression results (coefficients) commonly rounded 2 places, add 1 because it's a mean
                        
                        "Velocity_95L", "Velocity_95H", "Velocity_80L", "Velocity_80H", "Depth_95L", "Depth_95H", "Depth_80L", "Depth_80H",      #round CI 1 beyond raw data (coefficients = 2)             
                        "Sub_95L", "Sub_95H", "Sub_80L", "Sub_80H", "VelFull_95L", "VelFull_95H", "VelFull_80L", "VelFull_80H", 
                        "DepthFull_95L", "DepthFull_95H", "DepthFull_80L", "DepthFull_80H", "Instream_95L", "Instream_95H", "Instream_80L", 
                        "Instream_80H", "Canopy_95L", "Canopy_95H", "Canopy_80L", "Canopy_80H", 
                        
                        "Velocity_25", "Velocity_75", "Velocity_5", "Velocity_95",                                                                
                        "Depth_25", "Depth_75", "Depth_5", "Depth_95", "Substrate_25", "Substrate_75", 
                        "Substrate_5", "Substrate_95", "Instream_25", "Instream_75",
                        "Instream_5", "Instream_95", "Canopy_P",
                        
                        'Velocity_Min', 'Velocity_Max', 'Instream_Min', 'Instream_Max', 'Canopy_Min', 'Canopy_Max'                             #round 1 place beyond raw data (velocity, instream, canopy are all 2 places)
        ), round, 3))
    }              
    
    #Add variable combination to Dataframe
    One_Row <- data.frame(Species = Species_Abb,
                          Stream = Stream_Abb,
                          Group = Group_Abb,
                          Mesohabitat = Mesohabitat_Abb,
                          Observed = Observed_Individuals,
                          Unique = Unique_Individuals,
                          Available = Available) %>%
      bind_cols(Storage_Means)
    
    #Add current run to new Dataframe (All Rows)
    if(is.null(All_Rows)){                                                        #The first run through there will be no All Rows
      All_Rows <- One_Row                                                          #So this creates All Rows from One Row
    } else {
      All_Rows <- All_Rows %>%                                                    #Subsequent runs One Row gets added to All Rows
        bind_rows(One_Row)
    }
  }
  
}


##### Print #####
#RUN ME BEFORE YOU FORGET (please)

#Output to File
write.csv(x = All_Rows, file = "debug.csv")


