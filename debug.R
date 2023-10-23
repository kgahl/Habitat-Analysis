
library(dplyr)
library(readr)


##### DATA #####

##Read In
#All data available
NotAsFishy <- read.csv("Output/NotAsFishy.csv")


#Read in data file with variable combinations 
Variable_Combos <- read.csv("Means_Variables.csv")

##Output 
#Dataframe to write to file
All_Rows <- NULL


##### SETUP #####

##Assign combination of variables     
#Iterate over each row of Variable_Combos
#for (row_i in 1:nrow(Variable_Combos)) {#assigns a name to the selected variable from the selected row within Variable_Combo csv file  
for (row_i in 1:25) {
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
   if(length(which(Select_Data$Presence == 0)) > 1){
   } 
  #   
    ##Reality Check
    message("running ", Species_Name, ", ", Stream_Name, ", ",                    #Shows which combination is running in console when loop is started
            Group_Abb, ", ", Mesohabitat_Name, " (n = ", nrow(Select_Data), ")")
    
    message("Available Filter ","(n = ", nrow(filter(Select_Data, Presence == 0)),  ")")
    
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
    #Available <- length(Available[!is.na(Available)])
 

      #Add variable combination to Dataframe
      One_Row <- data.frame(Species = Species_Abb,
                            Stream = Stream_Abb,
                            Group = Group_Abb,
                            Mesohabitat = Mesohabitat_Abb,
                            Observed = Observed_Individuals,
                            Unique = Unique_Individuals,
                            Available = Available) 
       
      
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
  write.csv(x = All_Rows, file = "Count2.csv")
  

      