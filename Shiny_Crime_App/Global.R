library(tidyverse)
library(data.table)
library(lubridate)
library(leaflet)
library(plotly)
library(ggthemes)
library(leaflet.extras)


SF <- fread("Map_of_Police_Department_Incidents.csv")
SF$Date <- mdy_hms(as.character(SF$Date))
SF$Time <- hm(as.character(SF$Time))

SF$month <- month(SF$Date, label=FALSE)

#For a clean neighbourhood, these are the offences I don't want

crimes <- c("ASSAULT","BURGLARY","DRUG/NARCOTIC","DRUNKENNESS","LARCENY/THEFT","PROSTITUTION",
            "VEHICLE THEFT","TRESPASS","SEX OFFENSES, FORCIBLE","VANDALISM")

SF <- SF%>%filter(Category %in% crimes)
SF$PdDistrict <- as.factor(SF$PdDistrict)
#Remove blank districts
SF <- SF %>% filter(PdDistrict!="")
SF <- droplevels(SF)
SF$Year <- year(SF$Date)
SFsumm <- SF %>% group_by(Year, month, PdDistrict, Category) %>% summarize(Count=n())
rm(crimes)
