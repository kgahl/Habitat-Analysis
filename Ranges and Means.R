
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
    Select_Data <- Combined %>%                                                    
      dplyr::filter(Month %in% Group_Months) %>%
      dplyr::filter(Mesohabitat == Mesohabitat_Name)

  
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
    Available <- na.omit(Select_Data$Presence)
    Available <- length(Available[!is.na(Available)])
    
    
    ##Create data frame to store coefficients
    Prop_Sample <- 0.75                                                           #specify the proportion of point resampled (75%)  
    
    N_reps <- 100
    
    Storage <- data.frame(Mean_VelocityU = NA, Velocity_25U = rep(NA,N_reps), Velocity_75U = NA, Velocity_5U = NA, Velocity_95U = NA, 
                          Mean_VelocityA = NA, Velocity_25A = NA, Velocity_75A = NA, Velocity_5A = NA, Velocity_95A = NA,
                          Mean_DepthU = NA, Depth_25U = NA, Depth_75U = NA, Depth_5U = NA, Depth_95U = NA, 
                          Mean_DepthA = NA, Depth_25A = NA, Depth_75A = NA, Depth_5A = NA, Depth_95A = NA, 
                          Mean_SubstrateU = NA, Substrate_25U = NA, Substrate_75U = NA, Substrate_5U = NA, Substrate_95U = NA,
                          Mean_SubstrateA = NA, Substrate_25A = NA, Substrate_75A = NA, Substrate_5A = NA, Substrate_95A = NA,
                          Mean_InstreamU = NA, Instream_25U = NA, Instream_75U = NA, Instream_5U = NA, Instream_95U = NA,
                          Mean_InstreamA = NA, Instream_25A = NA, Instream_75A = NA, Instream_5A = NA, Instream_95A = NA,
                          Mean_CanopyU = NA, Canopy_25U = NA, Canopy_75U = NA, Canopy_5U = NA, Canopy_95U = NA,
                          Mean_CanopyA = NA, Canopy_25A = NA, Canopy_75A = NA, Canopy_5A = NA, Canopy_95A = NA)
                          
    
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
     
      
       
  ##Means
      Storage$Mean_VelocityU[i] <- mean(Used_Data_i$Velocity, na.rm = TRUE)
      Storage$Mean_DepthU[i] <- mean(Used_Data_i$Depth, na.rm = TRUE)
      Storage$Mean_SubstrateU[i] <- mean(Used_Data_i$Substrate, na.rm = TRUE)
      Storage$Mean_InstreamU[i] <- mean(Used_Data_i$Instream, na.rm = TRUE)
      Storage$Mean_CanopyU[i] <- mean(Used_Data_i$Canopy, na.rm = TRUE)
      
      Storage$Mean_VelocityA[i] <- mean(Avail_Data$Velocity, na.rm = TRUE)
      Storage$Mean_DepthA[i] <- mean(Avail_Data$Depth, na.rm = TRUE)
      Storage$Mean_SubstrateA[i] <- mean(Avail_Data$Substrate, na.rm = TRUE)
      Storage$Mean_InstreamA[i] <- mean(Avail_Data$Instream, na.rm = TRUE)
      Storage$Mean_CanopyA[i] <- mean(Avail_Data$Canopy, na.rm = TRUE)
      

      
##### HABITAT SUITABILITY CRITERIA #####
        
  ###Velocity
    ##Used ranges
        Used_Velocity <- Used_Data_i[order(Used_Data_i$Velocity),] 
        Used_Velocity$Rank <- seq(1,nrow(Used_Velocity))
        
      #Rank and Quantiles  
        Storage$Velocity_25U[i] <- quantile(Used_Velocity$Velocity, 0.25, na.rm = TRUE)        
        Storage$Velocity_75U[i] <- quantile(Used_Velocity$Velocity, 0.75, na.rm = TRUE)          
        Storage$Velocity_5U[i] <- quantile(Used_Velocity$Velocity, 0.05, na.rm = TRUE)            
        Storage$Velocity_95U[i] <- quantile(Used_Velocity$Velocity, 0.95, na.rm = TRUE)
    
    ##Available ranges
        Avail_Velocity <- Avail_Data[order(Avail_Data$Velocity),] 
        Avail_Velocity$Rank <- seq(1,nrow(Avail_Velocity))
        
      #Rank and Quantiles  
        Storage$Velocity_25A[i] <- quantile(Avail_Velocity$Velocity, 0.25, na.rm = TRUE)        
        Storage$Velocity_75A[i] <- quantile(Avail_Velocity$Velocity, 0.75, na.rm = TRUE)          
        Storage$Velocity_5A[i] <- quantile(Avail_Velocity$Velocity, 0.05, na.rm = TRUE)            
        Storage$Velocity_95A[i] <- quantile(Avail_Velocity$Velocity, 0.95, na.rm = TRUE)  
        
  ###Depth
    ##Used ranges
        Used_Depth <- Used_Data_i[order(Used_Data_i$Depth),] 
        Used_Depth$Rank <- seq(1,nrow(Used_Depth))
        
        #Rank and Quantiles  
        Storage$Depth_25U[i] <- quantile(Used_Depth$Depth, 0.25, na.rm = TRUE)        
        Storage$Depth_75U[i] <- quantile(Used_Depth$Depth, 0.75, na.rm = TRUE)          
        Storage$Depth_5U[i] <- quantile(Used_Depth$Depth, 0.05, na.rm = TRUE)            
        Storage$Depth_95U[i] <- quantile(Used_Depth$Depth, 0.95, na.rm = TRUE)
        
     ##Available ranges
        Avail_Depth <- Avail_Data[order(Avail_Data$Depth),] 
        Avail_Depth$Rank <- seq(1,nrow(Avail_Depth))
        
        #Rank and Quantiles  
        Storage$Depth_25A[i] <- quantile(Avail_Depth$Depth, 0.25, na.rm = TRUE)        
        Storage$Depth_75A[i] <- quantile(Avail_Depth$Depth, 0.75, na.rm = TRUE)          
        Storage$Depth_5A[i] <- quantile(Avail_Depth$Depth, 0.05, na.rm = TRUE)            
        Storage$Depth_95A[i] <- quantile(Avail_Depth$Depth, 0.95, na.rm = TRUE)  
        
        
  ###Substrate
    ##Used ranges
        Used_Substrate <- Used_Data_i[order(Used_Data_i$Substrate),] 
        Used_Substrate$Rank <- seq(1,nrow(Used_Substrate))
        
        #Rank and Quantiles  
        Storage$Substrate_25U[i] <- quantile(Used_Substrate$Substrate, 0.25, na.rm = TRUE)        
        Storage$Substrate_75U[i] <- quantile(Used_Substrate$Substrate, 0.75, na.rm = TRUE)          
        Storage$Substrate_5U[i] <- quantile(Used_Substrate$Substrate, 0.05, na.rm = TRUE)            
        Storage$Substrate_95U[i] <- quantile(Used_Substrate$Substrate, 0.95, na.rm = TRUE)
        
    ##Available ranges
        Avail_Substrate <- Avail_Data[order(Avail_Data$Substrate),] 
        Avail_Substrate$Rank <- seq(1,nrow(Avail_Substrate))
        
        #Rank and Quantiles  
        Storage$Substrate_25A[i] <- quantile(Avail_Substrate$Substrate, 0.25, na.rm = TRUE)        
        Storage$Substrate_75A[i] <- quantile(Avail_Substrate$Substrate, 0.75, na.rm = TRUE)          
        Storage$Substrate_5A[i] <- quantile(Avail_Substrate$Substrate, 0.05, na.rm = TRUE)            
        Storage$Substrate_95A[i] <- quantile(Avail_Substrate$Substrate, 0.95, na.rm = TRUE)  
        
  ###Instream
    ##Used ranges
        Used_Instream <- Used_Data_i[order(Used_Data_i$Instream),] 
        Used_Instream$Rank <- seq(1,nrow(Used_Instream))
        
        #Rank and Quantiles  
        Storage$Instream_25U[i] <- quantile(Used_Instream$Instream, 0.25, na.rm = TRUE)        
        Storage$Instream_75U[i] <- quantile(Used_Instream$Instream, 0.75, na.rm = TRUE)          
        Storage$Instream_5U[i] <- quantile(Used_Instream$Instream, 0.05, na.rm = TRUE)            
        Storage$Instream_95U[i] <- quantile(Used_Instream$Instream, 0.95, na.rm = TRUE)
        
    ##Available ranges
        Avail_Instream <- Avail_Data[order(Avail_Data$Instream),] 
        Avail_Instream$Rank <- seq(1,nrow(Avail_Instream))
        
        #Rank and Quantiles  
        Storage$Instream_25A[i] <- quantile(Avail_Instream$Instream, 0.25, na.rm = TRUE)        
        Storage$Instream_75A[i] <- quantile(Avail_Instream$Instream, 0.75, na.rm = TRUE)          
        Storage$Instream_5A[i] <- quantile(Avail_Instream$Instream, 0.05, na.rm = TRUE)            
        Storage$Instream_95A[i] <- quantile(Avail_Instream$Instream, 0.95, na.rm = TRUE)  
        
  ###Canopy
    ##Used ranges
        Used_Canopy <- Used_Data_i[order(Used_Data_i$Canopy),] 
        Used_Canopy$Rank <- seq(1,nrow(Used_Canopy))
        
        #Rank and Quantiles  
        Storage$Canopy_25U[i] <- quantile(Used_Canopy$Canopy, 0.25, na.rm = TRUE)        
        Storage$Canopy_75U[i] <- quantile(Used_Canopy$Canopy, 0.75, na.rm = TRUE)          
        Storage$Canopy_5U[i] <- quantile(Used_Canopy$Canopy, 0.05, na.rm = TRUE)            
        Storage$Canopy_95U[i] <- quantile(Used_Canopy$Canopy, 0.95, na.rm = TRUE)
        
    ##Available ranges
        Avail_Canopy <- Avail_Data[order(Avail_Data$Canopy),] 
        Avail_Canopy$Rank <- seq(1,nrow(Avail_Canopy))
        
        #Rank and Quantiles  
        Storage$Canopy_25A[i] <- quantile(Avail_Canopy$Canopy, 0.25, na.rm = TRUE)        
        Storage$Canopy_75A[i] <- quantile(Avail_Canopy$Canopy, 0.75, na.rm = TRUE)          
        Storage$Canopy_5A[i] <- quantile(Avail_Canopy$Canopy, 0.05, na.rm = TRUE)            
        Storage$Canopy_95A[i] <- quantile(Avail_Canopy$Canopy, 0.95, na.rm = TRUE)  
        
#### Storage Database ####
    #for each variable combo
        Results_File <- paste0(Species_Abb, "_",                           
                               Stream_Abb, "_",
                               Group_Abb, "_",
                               Mesohabitat_Abb, "_Means.csv")                   #Change _Storage.csv if distinguishing between different test runs
        
        write.csv(x = Storage, file = Results_File)      
          
#### Storage Means ####
      #Summarize and save results into one master file

        
    ##VDS

      Storage_Means <- Storage %>%
          summarize(across(c(1:50), mean)) %>%
          mutate(across(c(1:50), round, 3))
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
  write.csv(x = All_Rows, file = "Means.csv")
  

      