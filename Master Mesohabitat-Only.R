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
Variable_Combos <- read.csv("Discharge/HighLowMesohabitat_Variables.csv")

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
  Select_Data <- Combined %>%                                                 
    dplyr::filter(Month %in% Group_Months)
  
  
  
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
  Available <- na.omit(Select_Data$Presence)
  Available <- length(Available[!is.na(Available)])

  
  
  # ##### MIN MAX #####
  # 
  # ##Find Minimum and Maximum values of used variables
  # Counts <- Select_Data  %>%
  #   dplyr::filter(Species == Species_Name)                                      #only occupied
  # 
  # MinMax <- NULL                                                                #Min/Max storage
  # 
  # MinMax$Velocity_Min  <- min(Counts$Velocity, na.rm = TRUE)      
  # MinMax$Velocity_Max  <- max(Counts$Velocity, na.rm = TRUE)    
  # 
  # MinMax$Depth_Min     <- min(Counts$Depth, na.rm = TRUE)       
  # MinMax$Depth_Max     <- max(Counts$Depth, na.rm = TRUE)       
  # 
  # MinMax$Substrate_Min <- min(Counts$Substrate, na.rm = TRUE)   
  # MinMax$Substrate_Max <- max(Counts$Substrate, na.rm = TRUE)     
  # 
  # MinMax$Instream_Min     <- min(Counts$Instream, na.rm = TRUE)       
  # MinMax$Instream_Max     <- max(Counts$Instream, na.rm = TRUE)       
  # 
  # MinMax$Canopy_Min <- min(Counts$Canopy, na.rm = TRUE)   
  # MinMax$Canopy_Max <- max(Counts$Canopy, na.rm = TRUE)     
  
  
  
  ##### LOOP SETUP #####
  
  ##Create data frame to store coefficients
  Prop_Sample <- 0.75                                                           #specify the proportion of points resampled (75%)  
  
  N_reps <- 100
  
  Storage <- data.frame(Ratio_Pool = rep(NA,N_reps), Ratio_Riffle = NA, Ratio_Run = NA, 
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

  #USE THIS ONE
    Avail_Pool <- ifelse(Avail_Data$Mesohabitat == 'Pool', (na.omit(Avail_Data$StreamWidth)),0)
    Avail_Prob_Pool <- (sum(na.omit(Avail_Pool)))/sum(na.omit(Avail_Data$StreamWidth))     
    Used_Prob_Pool<- sum(na.omit(Used_Data_i$Mesohabitat == "Pool"))/sum(!is.na(Used_Data_i$Mesohabitat))
    Storage$Ratio_Pool[i] <- Used_Prob_Pool/Avail_Prob_Pool
    
    
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
    
    

    
##### GENERAL LINEAR MODELING #####
    
    ##Add weight column to make binomial GLM equivalent to Inhomogeneous Poisson Point Process Model
    Data_i$weight <- ifelse(Data_i$Presence == 1, 1, 5000)
    
    ##Mesohabitat  
      #Fit the model
      Model_Meso <- glm(Presence ~ Mesohabitat, data = Data_i, 
                        family = binomial, weights = weight)             
      
      #Pull out and store coefficients
      Storage$Mesohabitat_Riffle[i] <- Model_Meso$coefficients['MesohabitatRiffle']
      Storage$Mesohabitat_Run[i] <- Model_Meso$coefficients['MesohabitatRun']

  }
  
  ##### SUMMARIZE RESULTS ##### 
  
  #### Storage Database ####
  
  # #for each variable combo
  # Results_File <- paste0(Species_Abb, "_",                           
  #                        Stream_Abb, "_",
  #                        Group_Abb, "_",
  #                        Mesohabitat_Abb, "_HighLow_Mesohabitat.csv")                   #Change _Storage.csv if distinguishing between different test runs
  # 
  # write.csv(x = Storage, file = Results_File)
  
  
  
  #### Storage Means ####
  #Summarize and save results into one master file
  
  
  ##Mesohabitat

    Storage_Means <- Storage %>%
      summarize(across(c(Ratio_Pool, Ratio_Riffle, Ratio_Run,
                         Mesohabitat_Riffle, Mesohabitat_Run), mean))                                         #create and pull out means of Mesohabitat storage columns
    
    #Mesohabitat Ratio and RSF Quantiles
    #Pool Ratio
    Storage_Means$RPool_95L <- quantile(Storage$Ratio_Pool, probs = 0.025)      #95 CI (2.5 to 97.5)
    Storage_Means$RPool_95H <- quantile(Storage$Ratio_Pool, probs = 0.975)
    Storage_Means$RPool_80L <- quantile(Storage$Ratio_Pool, probs = 0.1)                                                       
    Storage_Means$RPool_80H <- quantile(Storage$Ratio_Pool, probs = 0.9)
    Storage_Means$RPool_Percent_Support <- ifelse(mean(Storage$Ratio_Pool)>1,   #for Selection Ratio 1 is the measure of proportional use (<1 = avoid, >1 = select)
                                                  mean(Storage$Ratio_Pool>1),
                                                  mean(Storage$Ratio_Pool<1))   #gives percentage of re-sampled slopes that match the charge(+/-) of the mean estimate 
    #Riffle Ratio
    Storage_Means$RRiffle_95L <- quantile(Storage$Ratio_Riffle, probs = 0.025)                                                       
    Storage_Means$RRiffle_95H <- quantile(Storage$Ratio_Riffle, probs = 0.975)
    Storage_Means$RRiffle_80L <- quantile(Storage$Ratio_Riffle, probs = 0.1)                                                       
    Storage_Means$RRiffle_80H <- quantile(Storage$Ratio_Riffle, probs = 0.9)
    Storage_Means$RRiffle_Percent_Support <- ifelse(mean(Storage$Ratio_Riffle)>1,
                                                    mean(Storage$Ratio_Riffle>1),
                                                    mean(Storage$Ratio_Riffle<1))
    #Run Ratio
    Storage_Means$RRun_95L <- quantile(Storage$Ratio_Run, probs = 0.025)                                                       
    Storage_Means$RRun_95H <- quantile(Storage$Ratio_Run, probs = 0.975)
    Storage_Means$RRun_80L <- quantile(Storage$Ratio_Run, probs = 0.1)                                                       
    Storage_Means$RRun_80H <- quantile(Storage$Ratio_Run, probs = 0.9)
    Storage_Means$RRun_Percent_Support <- ifelse(mean(Storage$Ratio_Run)>1,
                                                 mean(Storage$Ratio_Run>1),
                                                 mean(Storage$Ratio_Run<1))
    #Riffle RSF
    Storage_Means$MesoRiffle_95L <- quantile(Storage$Mesohabitat_Riffle, probs = 0.025)                                                       
    Storage_Means$MesoRiffle_95H <- quantile(Storage$Mesohabitat_Riffle, probs = 0.975)
    Storage_Means$MesoRiffle_80L <- quantile(Storage$Mesohabitat_Riffle, probs = 0.1)                                                       
    Storage_Means$MesoRiffle_80H <- quantile(Storage$Mesohabitat_Riffle, probs = 0.9)
    Storage_Means$MesoRiffle_Percent_Support <- ifelse(mean(Storage$Mesohabitat_Riffle)>0,       #For GLM RSFs 0 is the divide, - equals avoid, + equals selection
                                                       mean(Storage$Mesohabitat_Riffle>0),
                                                       mean(Storage$Mesohabitat_Riffle<0))
    #Run RSF
    Storage_Means$MesoRun_95L <- quantile(Storage$Mesohabitat_Run, probs = 0.025)                                                       
    Storage_Means$MesoRun_95H <- quantile(Storage$Mesohabitat_Run, probs = 0.975)
    Storage_Means$MesoRun_80L <- quantile(Storage$Mesohabitat_Run, probs = 0.1)                                                       
    Storage_Means$MesoRun_80H <- quantile(Storage$Mesohabitat_Run, probs = 0.9)
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

##### Print #####
#RUN ME BEFORE YOU FORGET (please)

#Output to File
write.csv(x = All_Rows, file = "HighLow_MesoTest.csv")


