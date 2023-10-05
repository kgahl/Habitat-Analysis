#USGS Water Data using dataRetrieval tool
#https://waterdata.usgs.gov/blog/dataretrieval/
#dv = Data aggregated to a daily statistic such as mean, min, or max.
#uv = Regular frequency data reported from a sensor (e.g. 15 minute interval). This data can include ‘real-time’ data.


#Get Package
library(dataRetrieval)
library(ggplot2)


#Get data for Wet Beaver Creek  

WBSite <- "09505200"                                                       #Wet Beaver Creek near Rimrock AZ monitoring location
Discharge <- "00060"                                                       #Parameter code - this one is for discharge
start_date <- "2022-03-01"
end_date <- "2023-06-30"

Wet_Beaver <- readNWISdv(siteNumbers = WBSite,
                         parameterCd = Discharge,
                         startDate = start_date,
                         endDate = end_date)

#Clean up column names
Wet_Beaver <- renameNWISColumns(Wet_Beaver)
names(Wet_Beaver)

#List of attributes
names(attributes(Wet_Beaver))

#Access attributes
variableinfo <- attr(Wet_Beaver, "variableInfo")
variableinfo

#Plot
WBPlot <- ggplot(data = Wet_Beaver) +
  geom_line(aes(
    x = Date,                                                            #daily value
    y = Flow)) +
  xlab("Date") +
  labs(title = attr(Wet_Beaver, "siteInfo")$station_nm,
       caption = paste("Data pulled on:", 
                       as.Date(attr(Wet_Beaver, "queryTime")))) +
  ylab(attr(Wet_Beaver, "variableInfo")$param_unit)                      #mean

WBPlot


#Split by month
March <- Wet_Beaver[c(1:31), c("Date", "Flow")]
April <- Wet_Beaver[c(32:61), c("Date", "Flow")]
May <- Wet_Beaver[c(62:92), c("Date", "Flow")]
September  <- Wet_Beaver[c(185:214), c("Date", "Flow")]
October <- Wet_Beaver[c(215:245), c("Date", "Flow")]
November <- Wet_Beaver[c(246:275), c("Date", "Flow")]
December <- Wet_Beaver[c(276:306), c("Date", "Flow")]
#2023
January <- Wet_Beaver[c(307:337), c("Date", "Flow")]
February <- Wet_Beaver[c(338:365), c("Date", "Flow")]
June <- Wet_Beaver[c(458:487), c("Date", "Flow")]


#Min and Max and Mean for month
#January
JanuaryL <- NULL
JanuaryL$Month <- "January"
JanuaryL$Min <- min(January$Flow, na.rm = TRUE)
JanuaryL$Max <- max(January$Flow, na.rm = TRUE)
JanuaryL$Mean <- mean(January$Flow, na.rm = TRUE)
JanuaryL$Day <- Wet_Beaver$Flow[334]

#February
FebruaryL <- NULL
FebruaryL$Month <- "February"
FebruaryL$Min <- min(February$Flow, na.rm = TRUE)
FebruaryL$Max <- max(February$Flow, na.rm = TRUE)
FebruaryL$Mean <- mean(February$Flow, na.rm = TRUE)
FebruaryL$Day <- Wet_Beaver$Flow[356]

#March
MarchL <- NULL
MarchL$Month <- "March"
MarchL$Min <- min(March$Flow, na.rm = TRUE)
MarchL$Max <- max(March$Flow, na.rm = TRUE)
MarchL$Mean <- mean(March$Flow, na.rm = TRUE)
MarchL$Day <- Wet_Beaver$Flow[14]

#April
AprilL <- NULL
AprilL$Month <- "April"
AprilL$Min <- min(April$Flow, na.rm = TRUE)
AprilL$Max <- max(April$Flow, na.rm = TRUE)
AprilL$Mean <- mean(April$Flow, na.rm = TRUE)
AprilL$Day <- Wet_Beaver$Flow[54]

#May
MayL <- NULL
MayL$Month <- "May"
MayL$Min <- min(May$Flow, na.rm = TRUE)
MayL$Max <- max(May$Flow, na.rm = TRUE)
MayL$Mean <- mean(May$Flow, na.rm = TRUE)
MayL$Day <- Wet_Beaver$Flow[78]

#June
JuneL <- NULL
JuneL$Month <- "June"
JuneL$Min <- min(June$Flow, na.rm = TRUE)
JuneL$Max <- max(June$Flow, na.rm = TRUE)
JuneL$Mean <- mean(June$Flow, na.rm = TRUE)
JuneL$Day <- Wet_Beaver$Flow[472]

#September
SeptemberL <- NULL
SeptemberL$Month <- "September"
SeptemberL$Min <- min(September$Flow, na.rm = TRUE)
SeptemberL$Max <- max(September$Flow, na.rm = TRUE)
SeptemberL$Mean <- mean(September$Flow, na.rm = TRUE)
SeptemberL$Day <- Wet_Beaver$Flow[199]

#October
OctoberL <- NULL
OctoberL$Month <- "October"
OctoberL$Min <- min(October$Flow, na.rm = TRUE)
OctoberL$Max <- max(October$Flow, na.rm = TRUE)
OctoberL$Mean <- mean(October$Flow, na.rm = TRUE)
OctoberL$Day <- Wet_Beaver$Flow[235]

#November
NovemberL <- NULL
NovemberL$Month <- "November"
NovemberL$Min <- min(November$Flow, na.rm = TRUE)
NovemberL$Max <- max(November$Flow, na.rm = TRUE)
NovemberL$Mean <- mean(November$Flow, na.rm = TRUE)
NovemberL$Day <- Wet_Beaver$Flow[263]

#December
DecemberL <- NULL
DecemberL$Month <- "December"
DecemberL$Min <- min(December$Flow, na.rm = TRUE)
DecemberL$Max <- max(December$Flow, na.rm = TRUE)
DecemberL$Mean <- mean(December$Flow, na.rm = TRUE)
DecemberL$Day <- Wet_Beaver$Flow[279]


Discharge <- data.frame(JanuaryL) %>%
  rbind(FebruaryL, MarchL, AprilL, MayL, JuneL, SeptemberL, OctoberL, NovemberL, DecemberL)

#Convert to M3/s
DischargeM <- Discharge %>%  
  mutate(across(c(2:5), function(x) x*0.028316831998814504)) %>%
  mutate(across(where(is.numeric), round, 3))


write.csv(x = Discharge, file = "WetBeaverDischarge(Ft).csv")
write.csv(x = DischargeM, file = "WetBeaverDischarge(M).csv")



#include breakdown for temperature?
    #West Clear has water temp
    #access to hobo logger temp for wet beaver


######################################################################################################
#WEST CLEAR 
######################################################################################################
#Clear workspace before proceeding

#Get Package
library(dataRetrieval)
library(ggplot2)


#Get data for Wet Beaver Creek  

WCSite <- "09505800"                                                       #West Clear Creek near Rimrock AZ monitoring location
Discharge <- "00060"                                                       #Parameter code - this one is for discharge
start_date <- "2022-03-01"
end_date <- "2022-11-30"

West_Clear <- readNWISdv(siteNumbers = WCSite,
                         parameterCd = Discharge,
                         startDate = start_date,
                         endDate = end_date)

#Clean up column names
West_Clear <- renameNWISColumns(West_Clear)
names(West_Clear)

#List of attributes
names(attributes(West_Clear))

#Access attributes
variableinfo <- attr(West_Clear, "variableInfo")
variableinfo

#Plot
WCPlot <- ggplot(data = West_Clear) +
  geom_line(aes(
    x = Date,                                                            #daily value
    y = Flow)) +
  xlab("Date") +
  labs(title = attr(West_Clear, "siteInfo")$station_nm,
       caption = paste("Data pulled on:", 
                       as.Date(attr(West_Clear, "queryTime")))) +
  ylab(attr(West_Clear, "variableInfo")$param_unit)                      #mean

WCPlot


#Split by month
March <- West_Clear[c(1:31), c("Date", "Flow")]
April <- West_Clear[c(32:61), c("Date", "Flow")]
May <- West_Clear[c(62:92), c("Date", "Flow")]
June <- West_Clear[c(93:122), c("Date", "Flow")]
July <- West_Clear[c(123:153), c("Date", "Flow")]
September  <- West_Clear[c(185:214), c("Date", "Flow")]
October <- West_Clear[c(215:245), c("Date", "Flow")]
November <- West_Clear[c(246:275), c("Date", "Flow")]


#Min and Max and Mean for month

#March
MarchL <- NULL
MarchL$Month <- "March"
MarchL$Min <- min(March$Flow, na.rm = TRUE)
MarchL$Max <- max(March$Flow, na.rm = TRUE)
MarchL$Mean <- mean(March$Flow, na.rm = TRUE)
MarchL$Day <- West_Clear$Flow[16]

#April
AprilL <- NULL
AprilL$Month <- "April"
AprilL$Min <- min(April$Flow, na.rm = TRUE)
AprilL$Max <- max(April$Flow, na.rm = TRUE)
AprilL$Mean <- mean(April$Flow, na.rm = TRUE)
AprilL$Day <- West_Clear$Flow[56]

#May
MayL <- NULL
MayL$Month <- "May"
MayL$Min <- min(May$Flow, na.rm = TRUE)
MayL$Max <- max(May$Flow, na.rm = TRUE)
MayL$Mean <- mean(May$Flow, na.rm = TRUE)
MayL$Day <- West_Clear$Flow[92]

#June
JuneL <- NULL
JuneL$Month <- "June"
JuneL$Min <- min(June$Flow, na.rm = TRUE)
JuneL$Max <- max(June$Flow, na.rm = TRUE)
JuneL$Mean <- mean(June$Flow, na.rm = TRUE)
JuneL$Day <- West_Clear$Flow[107]

#July
JulyL <- NULL
JulyL$Month <- "July"
JulyL$Min <- min(July$Flow, na.rm = TRUE)
JulyL$Max <- max(July$Flow, na.rm = TRUE)
JulyL$Mean <- mean(July$Flow, na.rm = TRUE)
JulyL$Day <- West_Clear$Flow[143]


#September
SeptemberL <- NULL
SeptemberL$Month <- "September"
SeptemberL$Min <- min(September$Flow, na.rm = TRUE)
SeptemberL$Max <- max(September$Flow, na.rm = TRUE)
SeptemberL$Mean <- mean(September$Flow, na.rm = TRUE)
SeptemberL$Day <- West_Clear$Flow[200]

#October
OctoberL <- NULL
OctoberL$Month <- "October"
OctoberL$Min <- min(October$Flow, na.rm = TRUE)
OctoberL$Max <- max(October$Flow, na.rm = TRUE)
OctoberL$Mean <- mean(October$Flow, na.rm = TRUE)
OctoberL$Day <- West_Clear$Flow[236]

#November
NovemberL <- NULL
NovemberL$Month <- "November"
NovemberL$Min <- min(November$Flow, na.rm = TRUE)
NovemberL$Max <- max(November$Flow, na.rm = TRUE)
NovemberL$Mean <- mean(November$Flow, na.rm = TRUE)
NovemberL$Day <- West_Clear$Flow[264]


Discharge <- data.frame(MarchL) %>%
  rbind(AprilL, MayL, JuneL, JulyL, SeptemberL, OctoberL, NovemberL)

#Convert to M3/s
DischargeM <- Discharge %>%  
  mutate(across(c(2:5), function(x) x*0.028316831998814504)) %>%
  mutate(across(where(is.numeric), round, 3))


write.csv(x = Discharge, file = "WestClearDischarge(Ft).csv")
write.csv(x = DischargeM, file = "WestClearDischarge(M).csv")



#############################################################################
#Combined

WC <- read.csv(file = "WestClearDischarge(Ft).csv")
WB <- read.csv(file = "WetBeaverDischarge(Ft).csv")

WC$Month = factor(WC$Month, levels = month.name)
WB$Month = factor(WB$Month, levels = month.name)


Both <- ggplot(NULL)+
  geom_point(data = WC,aes(x=Month,y=Mean), color = "black")+
  geom_point(data = WB,aes(x=Month,y=Mean), color = "red")+
  scale_x_discrete(limits = month.name)
Both
