#Wrangling and Cleaning <º)))>< occupied and available habitat data 

library(tidyverse)
library(readr)
library(dplyr)

##### All data #####
#Load in multiple files and combine into one file
all <- list.files(path = "C:/Users/kaitl/OneDrive/Documents/School/Bonar Lab/Project/Analysis/Habitat - Telemetry/Habitat-Analysis/Data/All", full.names = TRUE) %>%
  lapply(read_csv) %>%
  lapply(\(x) mutate(x, across(.fns = as.character))) %>%
  bind_rows()

write_csv(all, file = "Output/AllData.csv")

#All Data
AllData = read_csv("Output/AllData.csv", na =c('Na', 'NA', ''))

#Change names, change characters to numerical coding, make changes to depth
AllDataCoded <- AllData %>%
  rename(Presence = 'Site Type', Canopy = 'Canopy Cover', Instream = 'Instream Cover', Undercut = 'Undercut Bank', 
         TerrestrialVeg = 'Terrestrial Vegetation', LWD = 'Large Woody Debris', StreamWidth = 'Stream Width', 
         AquaticVeg = 'Aquatic Vegetation', SurfaceTurb = 'Surface Turbulence', SubFeature = 'Substrate Feature', 
         CreationDate = 'Creation Date', CaptureMethod = 'Capture Method', CaptureDate = 'Capture Date', 
         DataID = '...1', DataChange = 'Data Change') %>% 
  mutate(Presence = recode(Presence, "Occupied" = "1", "Available" = "0"),
         Substrate = recode(Substrate, "Silt" = "1", "Sand" = "2", "Gravel" = "3", 
                   "Pebble" = "4", "Cobble" = "5", "Boulder" = "6", "Bedrock" = "7"),
         Canopy = recode(Canopy, "Yes" = "1", "No" = "0"),
         Depth = recode(Depth, ">150" = "151"))

AllDataCoded$Depth = as.numeric(as.character(AllDataCoded$Depth))                 #make depth numeric instead of character
NotAsFishy <- AllDataCoded %>%
  mutate(across(9, round, 0),                                                   #round up the depths that were converted from foot tenths and therefore have 3 decimals #needed to use column number (9) rather than name (Depth) 
         Depth = replace(Depth, is.na(Depth) & Mesohabitat == 0, 151),          #if there is a pool and the Depth is NA replace NA with 151                                                                  
         Instream = replace(Instream, Instream > '1.00', '1'),                  #change any instream cover >1.0 into 1.0
         Latitude = abs(Latitude),
         Latitude = (Latitude * -1))                   
  
NotAsFishy$Velocity = ifelse(NotAsFishy$Depth == 151 & is.na(NotAsFishy$Velocity), 0, NotAsFishy$Velocity)           #If depth in a pool is 151 (>150) and velocity is NA, make velocity 0

#Master Data file that has all data coded and corrected
write_csv(NotAsFishy, file = "Output/NotAsFishy.csv")



str(NotAsFishy)



#### Filter for Occupied ####

All_Movement_Data <- read_csv("Output/NotAsFishy.csv")

All_Movement_Data = subset(All_Movement_Data, select = c(Stream, Month, Species, Tag, Mesohabitat, 
                                                         Longitude, Latitude, Depth, Velocity, 
                                                         Substrate, Instream, Canopy)) %>%
   filter(All_Movement_Data$Presence == '1')


write_csv(All_Movement_Data, file = "Output/All_Movement_Data.csv")



########## DS_WC_SP_RI Data ##########
#Desert Sucker West Clear Creek Spawning (Mar-Jun) Riffle

## Combine all desert sucker and available data on WC ----- DS_AV_WC
DS_WC <- AllData %>%
  filter(Stream %in% c('Clear'))%>%          
  filter(Species == 'Desert Sucker')      

AV_WC <- AllData %>%
  filter(Stream %in% c('Clear'))%>%          
  filter(Site.Type == 'Available')

DS_AV_WC <- rbind(DS_WC, AV_WC)

##Only Spawning period- Mar-June and Riffles
DS_WC_SP_RI <- DS_AV_WC %>%
  filter(Month %in% c('March', 'April', 'May', 'June')) %>%
  filter(Mesohabitat == 'Riffle')

write.csv(DS_WC_SP_RI, file = "DW_WC_SP_RI.csv")




####### Checking large data patterns #######

NotAsFishy = read.csv("~/School/Bonar Lab/Project/Data/Analysis/Data Ready for HSC/Data-Ready-for-HSC/Output/NotAsFishy.csv", 
                      na.strings =c('Na', 'NA', '')) 

#Look at Clear Creek Available by month to see patterns
WC <- NotAsFishy %>%                                                       
  filter(Stream %in% c('Clear'))%>%          
  filter(Presence == 0)
WC$Substrate = as.numeric(as.character((WC$Substrate))) 

#Do some quick data exploration
par(mfrow=c(3,1))                                                               #mfrow=c(nrows, ncols)
boxplot(Velocity ~ Month, WC)
boxplot(Substrate ~ Month, WC)
boxplot(Depth ~ Month, WC)
mtext("West Clear Creek", side = 3, line = - 2, outer = TRUE)                   #side: on which side of the plot (1=bottom, 2=left, 3=top, 4=right).line: on which MARgin line, starting at 0 countings outwards. outer: use outer margins if available.

#Look at means of velocity with and without March
WC_NM <- WC %>%
  filter(Month != 'March')

##West Clear Creek 
 #Velocity with and without March
  mean(WC$Velocity, na.rm = T)      #0.1678
  mean(WC_NM$Velocity, na.rm = T)   #0.1528
 #Substrate
  mean(WC$Substrate, na.rm = T)     #4.54
  mean(WC_NM$Substrate, na.rm = T)  #4.56
 #Depth
  mean(WC$Depth, na.rm = T)         #55.70
  mean(WC_NM$Depth, na.rm = T)      #54.86


#Look at Wet Beaver Creek Available by month to see patterns
WB <- NotAsFishy %>%                                                       
  filter(Stream %in% c('Beaver'))%>%          
  filter(Presence == 0)
  
WB$Substrate = as.numeric(as.character((WB$Substrate))) 

#Do some quick data exploration
par(mfrow=c(3,1))                                                               #mfrow=c(nrows, ncols)
boxplot(Velocity ~ Month, WB)
boxplot(Substrate ~ Month, WB)
boxplot(Depth ~ Month, WB)
mtext("Wet Beaver Creek", side = 3, line = - 2, outer = TRUE)                   #side: on which side of the plot (1=bottom, 2=left, 3=top, 4=right).line: on which MARgin line, starting at 0 countings outwards. outer: use outer margins if available.

#Look at means of velocity with and without March
WB_NM <- WB %>%
  filter(Month != 'March')

##Wet Beaver Creek 
 #Velocity with and without March
  mean(WB$Velocity, na.rm = T)      #0.1455
  mean(WB_NM$Velocity, na.rm = T)   #0.1240
 #Substrate
  mean(WB$Substrate, na.rm = T)     #4.73
  mean(WB_NM$Substrate, na.rm = T)  #4.74
 #Depth
  mean(WB$Depth, na.rm = T)         #50.81
  mean(WB_NM$Depth, na.rm = T)      #49.27


#Look at grouping of months
 #West Clear
  #April May June July, September October November December
  #No January, February, March, August
  Group1 <- WC %>%
    filter(Month %in% c('September', 'October', 'November', 'December'))
  Group2 <- WC %>%
    filter(Month %in% c('April', 'May', 'June', 'July'))    #fit great together 
  Group3 <- WC %>%
    filter(Month %in% c('March', 'April'))
  WC$Month <- factor(WC$Month , levels=c('April', 'May', 'June', 'July', 'September', 'October', 'November', 'December'))
  
  boxplot(Velocity ~ Month, Group3)
  boxplot(Depth ~ Month, Group1)
  boxplot(Substrate ~ Month, Group1)

  
 #Wet Beaver
  #April May June,  September October November,   December January February 
  Group4 <- WB %>%    #No August, No July, No March
    filter(Month %in% c('September', 'October', 'November','December')) #pretty much the same across vel, Sep/Oct deeper than nov/dec
  Group5 <- WB %>%    
    filter(Month %in% c('December', 'January', 'February', 'March'))
  Group6 <- WB %>%
    filter(Month %in% c('March', 'April', 'May', 'June'))   
  Group7<- WB %>%
    filter(Month %in% c('January', 'February', 'March'))
  
  #WB$Month <- factor(WB$Month , levels=c('January', 'February', 'March', 'April', 'May', 'June', 'September', 'October', 'November', 'December'))
  
  boxplot(Velocity ~ Month, Group7)          #April/May/June good match for velocity and depth
  boxplot(Depth ~ Month, Group4)
  boxplot(Substrate ~ Month, Group4)        #September substrate is lower (4) than Dec/Nov/Oct (5)

  FindMean <- WB %>%                   #December 0.0839  January 0.1448 February 0.1549  March 0.3623 April 0.1225
    filter(Month %in% c('April'))
  mean(FindMean$Velocity, na.rm = T)
  
 #Monthly precip
  #January .17/5.87     February 1.07    March 1.75    April 0.02     May  0    June .88    July 2.94
  #August  5.46    September 1.35   October 1.44  November .16   December 4.54

  #April May June     July August    September October   November     December January February
  
  
########### Location Data ##########
#Select Tag Location Data
TagLocation <- AllData %>%
  select(Stream, Month, 'Site Type', Tag, Species, Longitude, Latitude) %>%
  filter(!is.na(Tag))
write.csv(TagLocation, file = "TagLocationData.csv")

#Tag Locations for Beaver 
BeaverLocation <- TagLocation %>%
  filter(Stream %in% c('Beaver'))
write.csv(BeaverLocation, file = "BeaverLocationData.csv")
  #Beaver Desert Sucker
    BeaverDS <- BeaverLocation %>%
    filter(Species %in% c('Desert Sucker'))
    write.csv(BeaverDS, file = "BeaverDSLocation.csv")
  #Beaver Black Bass
    BeaverBB <- BeaverLocation %>%
    filter(Species %in% c('Black Bass'))
    write.csv(BeaverBB, file = "BeaverBBLocation.csv")

#Tag Locations for Clear
ClearLocation <- TagLocation %>%
 filter(Stream %in% c('Clear'))
write.csv(ClearLocation, file = "ClearLocationData.csv")
    #Clear Desert Sucker
    ClearDS <- ClearLocation %>%
      filter(Species %in% c('Desert Sucker'))
    write.csv(ClearDS, file = "ClearDSLocation.csv")
    #Clear Black Bass
    ClearBB <- ClearLocation %>%
      filter(Species %in% c('Black Bass'))
    write.csv(ClearBB, file = "ClearBBLocation.csv")
    
#####################################

############# GRAVEYARD #################
    
    #Mesohabitat = recode(Mesohabitat, "Pool" = "0", "Riffle" = "1", "Run" = "2"),
    
    #Latitude
    #head(AllData$Latitude)
    #AllData$Latitude <- format(round(AllData$Latitude, 6), nsmall = 6) 
    
    
    #specify column type when importing data
    #AllData <- read_csv("~/School/Bonar Lab/Project/Data/Analysis/Data Ready for HSC/Data-Ready-for-HSC/Output/Alldata.csv", na.strings =c ('Na','')) 
    #col_types = cols(Tag = col_character(), 'Stream Width' = col_double(), Depth = col_double(), Velocity = col_double()))  
  
    #AllData <- AllData %>%
    #  mutate(Depth = gsub(">", "", as.character(AllData$Depth)))
    
    #delete extra columns that were added when using read.csv instead of read_csv
    #select(-c(32, 33, 34, 35, 36, 37, 38))
    
    #DataID, Presence, StreamWidth, Depth, 
    #Velocity, Substrate, Canopy, SubFeature, 
    #SurfaceTurb, AquaticVeg, LWD, TerrestrialVeg, Undercut,
    #Instream, Weight, Length, Notes, CaptureDate, CaptureMethod,
    #CreationDate, DataChange, Release, Notes.y, ObjectID)
    