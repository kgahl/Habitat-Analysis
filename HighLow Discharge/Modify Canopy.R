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
      dplyr::filter(Month %in% c("January", "February", "March", "April", "May", "June")) %>%
      dplyr::filter(Mesohabitat == "Pool")
    

  
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
  
  

  ##### MIN MAX #####
  
  ##Find Minimum and Maximum values of used variables
  Counts <- Select_Data  %>%
    dplyr::filter(Species == "Desert Sucker")                                      #only occupied
  
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
                        Depth_t_stat_90 = NA, Depth_p_90 = NA,
                        
                        Instream_5=NA, Instream_95=NA,
                        Instream_5_Suit_Used=NA, Instream_5_UnSuit_Avail=NA,
                        Instream_5_UnSuit_Used=NA, Instream_5_Suit_Avail=NA,
                        Instream_t_stat_90 = NA, Instream_p_90 = NA,
                        
                        Canopy_5=NA, Canopy_95=NA,
                        Canopy_5_Suit_Used=NA, Canopy_5_UnSuit_Avail=NA,
                        Canopy_5_UnSuit_Used=NA, Canopy_5_Suit_Avail=NA,
                        Canopy_t_stat_90 = NA, Canopy_p_90 = NA,
                        
                        Ratio_Pool = NA, Ratio_Riffle = NA, Ratio_Run = NA, 
                        Mesohabitat_Riffle = NA, Mesohabitat_Run = NA,
                        
                        Canopy_Chi = NA, Canopy_P = NA, Canopy_0.05 = NA, Canopy_0.1 = NA, 
                        OccCan = NA, OccNoCan = NA, AvailCan = NA, AvailNoCan = NA)
  
  
  
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
    
    
    
    ##### HABITAT SUITABILITY CRITERIA #####
   
      
      HSC_data <- filter(Data_i, Presence == 1)                                     #isolate occupied site data
      
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
      
      
      
      
      
      
      
      #NEW CANOPY
      
      
      
  #Within Resample
      #Avail_Data = Available data (for this resample -> 75%)
      #Used_Data_i = Occupied data (for this resample -> 75%)
      
      #Count of each category 
      Occupied_Canopy <- sum(na.omit(Used_Data_i$Canopy > 0))      #4
      Available_Canopy <- sum(na.omit(Avail_Data$Canopy > 0))      #128
      Occupied_NoCanopy <- sum(na.omit(Used_Data_i$Canopy == 0))   #5
      Available_NoCanopy <- sum(na.omit(Avail_Data$Canopy == 0))   #66
      
      #Structure above into matrix 
         Canopy_Matrix <- matrix(c(Occupied_Canopy,Available_Canopy,Occupied_NoCanopy,Available_NoCanopy), byrow = T, nrow = 2)
      
      #Chi Square test 
         chisq.test(Canopy_Matrix, correct = FALSE)                             #correct = False -> don't apply continuty correction
         
      #Store Chi square value and p value 
         Chi_df <- as.data.frame(t(unlist(chisq.test(Canopy_Matrix, correct = FALSE))), quote=FALSE)                        #run chi square, separate each value (unlist), place into dataframe (as.data.frame), transpose rows into columns (t), remove quotations (quote = false) 
         
         Storage$Canopy_Chi[i] <- Chi_df$`statistic.X-squared`
         Storage$Canopy_P[i] <- Chi_df$p.value
         Storage$OccCan[i] <- Occupied_Canopy
         Storage$OccNoCan[i] <- Occupied_NoCanopy
         Storage$AvailCan[i] <- Available_Canopy
         Storage$AvailNoCan[i] <- Available_NoCanopy
         
         
       #Setup for percent support
         Storage$Canopy_0.05[i] <- ifelse(Storage$Canopy_P > 0.05, 0, 1)
         Storage$Canopy_0.1[i] <- ifelse(Storage$Canopy_P > 0.1, 0, 1)
         
         
         #0 tells us occupied is not different from available. If I want to know the percentage of canopy vs no canopy I can look at that through available 
     
         

         
         #for each resample if p value is > alpha = 1 if <alpha = 0. Use these to find percent support?
         #same as t test 
         #chi square chart
         #at .05 and .10
        # >.05  null accepted variables are not different
        # =/< .05 null rejected - variables are different
      # Storage_Means$Canopy_Percent_Support <- ifelse(mean(Storage$Canopy_P)>1,   #for Selection Ratio 1 is the measure of proportional use (<1 = avoid, >1 = select)
      #                                                  mean(Storage$Ratio_Pool>1),
      #                                                  mean(Storage$Ratio_Pool<1))   #gives percentage of re-sampled slopes that match the charge(+/-) of the mean estimate 
         
         
                
  # #before resample
  #     #Select_Data = all data within the current variable combo
  #     #Counts = occupied data within Select_Data
  #     
  #     #Count of each category 
  #        Available_Canopy <- sum(na.omit(Select_Data$Canopy > 0))     #187
  #        Occupied_Canopy <- sum(na.omit(Counts$Canopy > 0))           #13
  #        Available_NoCanopy <- sum(na.omit(Select_Data$Canopy == 0))  #91
  #        Occupied_NoCanopy <- sum(na.omit(Counts$Canopy == 0))        #7
  # 
  #     #Structure above into matrix
  #        Matrix_Canopy <- matrix(c(Occupied_Canopy,Available_Canopy,Occupied_NoCanopy,Available_NoCanopy), byrow = T, nrow = 2)
  #        
  #     #Chi Square Test
  #        chisq.test(Matrix_Canopy, correct = FALSE)
  #     
  #     #Store Chi square value and p value 
  #       Chi_df <- as.data.frame(t(unlist(chisq.test(Matrix_Canopy, correct = FALSE))), quote=FALSE)                        #run chi square, separate each value (unlist), place into dataframe (as.data.frame), transpose rows into columns (t), remove quotations (quote = false) 
  #       
  #       Storage$Canopy_Chi[i] <- Chi_df$`statistic.X-squared`
  #       Storage$Canopy_P[i] <- Chi_df$p.value
      
        
        
        
        
      # ###Canopy
      # #Find suitable ranges 
      # HSC_Canopy <- HSC_data[,c('Presence','Canopy')]                           #replaced y with Presence
      # HSC_Canopy <- HSC_Canopy[order(HSC_Canopy$Canopy),]                       #organizes Canopy in ascending order
      # HSC_Canopy$Rank <- seq(1,nrow(HSC_Canopy))                                #assigns rank  
      # HSC_Canopy$Prob <- HSC_Canopy$Rank/(nrow(HSC_Canopy)+1)                   #determine probability: rank/ n+1.  N = number occupied 
      # Canopy_25 <- quantile(HSC_Canopy$Canopy, 0.25, na.rm = TRUE)              #quantiles to determine ends of percentage range
      # Canopy_75 <- quantile(HSC_Canopy$Canopy, 0.75, na.rm = TRUE)          
      # Canopy_5 <- quantile(HSC_Canopy$Canopy, 0.05, na.rm = TRUE)            
      # Canopy_95 <- quantile(HSC_Canopy$Canopy, 0.95, na.rm = TRUE)
      # 
      # HSC_Canopy$Suitable <- ifelse(HSC_Canopy$Canopy                           #determine central 50% of observations (between 25 and 75% probability)
      #                               >= Canopy_25 & HSC_Canopy$Canopy 
      #                               <= Canopy_75, 1, 0) 
      # HSC_Canopy$Suitable_90 <- ifelse(HSC_Canopy$Canopy                        #determine central 90% of observations
      #                                  >= Canopy_5 & HSC_Canopy$Canopy    
      #                                  <= Canopy_95, 1, 0) 
      # 
      # #Classify each available point as suitable or unsuitable
      # Avail_Data$Canopy_Suitable <- ifelse(Avail_Data$Canopy >= Canopy_25 
      #                                      & Avail_Data$Canopy <= Canopy_75, 
      #                                      1, 0) 
      # Avail_Data$Canopy_Suitable_90 <- ifelse(Avail_Data$Canopy >= Canopy_5 
      #                                         & Avail_Data$Canopy <= Canopy_95, 
      #                                         1, 0) 
      # 
      # ##Central 50%
      # Storage$Canopy_25_Suit_Used[i] <- sum(HSC_Canopy$Suitable == 1, na.rm = T)
      # Storage$Canopy_25_UnSuit_Used[i] <- sum(HSC_Canopy$Suitable == 0, na.rm = T)
      # 
      # Storage$Canopy_25_Suit_Avail[i] <- sum(Avail_Data$Canopy_Suitable == 1, na.rm = T)
      # Storage$Canopy_25_UnSuit_Avail[i] <- sum(Avail_Data$Canopy_Suitable == 0, na.rm = T)
      # 
      # Storage$Canopy_25[i] <- Canopy_25
      # Storage$Canopy_75[i] <- Canopy_75
      # 
      # con_tab_Suit50_c <- matrix(c(Storage[i,c("Canopy_25_Suit_Used",
      #                                          "Canopy_25_UnSuit_Used",
      #                                          "Canopy_25_Suit_Avail",
      #                                          "Canopy_25_UnSuit_Avail")]),
      #                            byrow=T,
      #                            nrow=2)
      # 
      # used_suit_c <- Storage$Canopy_25_Suit_Used[i]
      # avail_unsuit_c <- Storage$Canopy_25_UnSuit_Avail[i]
      # used_unsuit_c <- Storage$Canopy_25_UnSuit_Used[i]
      # avail_suit_c <- Storage$Canopy_25_Suit_Avail[i]
      # 
      # t_stat_c <- ((sum(unlist(con_tab_Suit50_c)))^0.5)*
      #   ((used_suit_c*avail_unsuit_c) - (used_unsuit_c*avail_suit_c))/
      #   (((used_suit_c+avail_suit_c)*
      #       (used_unsuit_c+avail_unsuit_c)*
      #       (used_unsuit_c+used_suit_c)*
      #       (avail_suit_c+avail_unsuit_c))^0.5)
      # 
      # Storage$Canopy_t_stat_50[i] <- t_stat_c 
      # 
      # Storage$Canopy_p_50[i] <- ifelse(t_stat_c > 1.6449,1,0)                   #1.6449 is the critical value for t-statistic when alpha = 0.05
      # 
      # ##Central 90%
      # Storage$Canopy_5_Suit_Used[i] <- sum(HSC_Canopy$Suitable_90==1,na.rm=T)
      # Storage$Canopy_5_UnSuit_Used[i] <- sum(HSC_Canopy$Suitable_90==0,na.rm=T)
      # 
      # Storage$Canopy_5_Suit_Avail[i] <- sum(Avail_Data$Canopy_Suitable_90==1,na.rm=T)
      # Storage$Canopy_5_UnSuit_Avail[i] <- sum(Avail_Data$Canopy_Suitable_90==0,na.rm=T)
      # 
      # Storage$Canopy_5[i] <- Canopy_5
      # Storage$Canopy_95[i] <- Canopy_95
      # 
      # con_tab_Suit90_c <- matrix(c(Storage[i,c("Canopy_5_Suit_Used",
      #                                          "Canopy_5_UnSuit_Used",
      #                                          "Canopy_5_Suit_Avail",
      #                                          "Canopy_5_UnSuit_Avail")]),
      #                            byrow=T,
      #                            nrow=2)
      # 
      # used_suit_c_90 <- Storage$Canopy_5_Suit_Used[i]
      # avail_unsuit_c_90 <- Storage$Canopy_5_UnSuit_Avail[i]
      # used_unsuit_c_90 <- Storage$Canopy_5_UnSuit_Used[i]
      # avail_suit_c_90 <- Storage$Canopy_5_Suit_Avail[i]
      # 
      # t_stat_c_90 <-((sum(unlist(con_tab_Suit90_c)))^0.5)*
      #   ((used_suit_c_90*avail_unsuit_c_90)-(used_unsuit_c_90*avail_suit_c_90))/
      #   (((used_suit_c_90+avail_suit_c_90)*
      #       (used_unsuit_c_90+avail_unsuit_c_90)*
      #       (used_unsuit_c_90+used_suit_c_90)*
      #       (avail_suit_c_90+avail_unsuit_c_90))^0.5)
      # 
      # Storage$Canopy_t_stat_90[i] <- t_stat_c_90 
      # 
      # Storage$Canopy_p_90[i] <- ifelse(t_stat_c_90 > 1.6449, 1, 0)              #1.6449 is the critical value for t-statistic when alpha = 0.05
    }  
  
  
  
  
  
  
    #Canopy RSF
    Storage_Means$Canopy_95L <- quantile(Storage$Canopy, probs = 0.025)
    Storage_Means$Canopy_95H <- quantile(Storage$Canopy, probs = 0.975)
    Storage_Means$Canopy_80L <- quantile(Storage$Canopy, probs = 0.1)
    Storage_Means$Canopy_80H <- quantile(Storage$Canopy, probs = 0.9)
    Storage_Means$C_Percent_Support <- ifelse(mean(Storage$Canopy)>0,
                                              mean(Storage$Canopy>0),
                                              mean(Storage$Canopy<0))
    
