library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(lubridate)
library(readr)
library(plotly)
library(dtplyr)
library(leaflet)
library(ggmap)
library(sp)
library(classInt)
library(rgeos)
library(maptools)
library(RColorBrewer)
library(raster)
library(rgdal)


SF <- fread("Map_of_Police_Department_Incidents.csv")

# Juvenile crime and the frequency of these crimes on a daily basis 
juvecrime_freq <- SF %>% select(Category, Descript, DayOfWeek) %>% filter(SF$Descript %in% c("JUVENILE INVOLVED"), SF$PdDistrict %in% c("SOUTHERN")) %>% group_by(Category, Descript, DayOfWeek) %>% summarise(Count = n()) %>% arrange(desc(Count))
ggplot(juvecrime_freq, aes(x = DayOfWeek, y = Count)) + geom_bar(stat = "identity", alpha = 0.7, width = 0.6, fill = juvecrime_freq$Count)  + theme(axis.text.x = element_text(angle = 45, hjust= 1), axis.title.x = element_blank(), axis.title.y = element_text(), panel.background = element_rect(fill = "white", colour = "grey50")) + labs(title = "Juvenile Crimes for each day of the Week", size = rel(2))

# Criminal cases of various nature for the top 10 precincts
juvecrime_Pd <- SF1 %>% select(Category, Descript, PdDistrict) %>% filter(SF1$Resolution %in% c("JUVENILE BOOKED", "JUVENILE ADMONISHED", "JUVENILE CITED", "JUVENILE DIVERTED", "CLEARED-CONTACT JUVENILE FOR MORE INFO")) %>% group_by(Category, Descript, PdDistrict) %>% summarise(Count = n()) %>% arrange(desc(Count)) %>% head(n = 10)
plot_pd = ggplot(juvecrime_Pd, aes(x= PdDistrict, y = Category)) + geom_point(aes(size = Count, col = Count)) + scale_size(range = c(1,10)) + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + labs(title = "Juvenile Crimes in each of the Districts", size = rel(2))
ggplotly(plot_pd, tooltip = c('x','y','colour'))

#Illegal sale of arms 
illegal_arms <- SF %>% select(Category, Descript, PdDistrict) %>% filter(SF$Descript %in% c("POSS OF FIREARM BY CONVICTED FELON/ADDICT/ALIEN", "POSS OF PROHIBITED WEAPON", "FIREARM WITH ALTERED IDENTIFICATION", "AMMUNITION, POSS. BY PROHIBITED PERSON")) %>% group_by(Category, Descript, PdDistrict) %>% summarise(Count = n()) %>% arrange(desc(Count))
plot_illegal_arms <- ggplot(illegal_arms, aes(x= PdDistrict, y = Count, fill = Descript)) + geom_bar(stat= "identity")  + theme(axis.title.x = element_blank(), axis.title.y = element_text("Count"), axis.text.x = element_text(angle = 45, hjust= 1) ) + labs(title = "Illegal sale of arms in each of the districts", size = rel(2))
ggplotly(plot_illegal_arms, tooltip = c('x','y','colour'))

#Year to year crime patterns

crime10to15 <- SF %>% select(Category:Location) %>% filter(SF$Date >= as.Date("2010-01-01") & SF$Date <= as.Date("2015-12-31")) %>% mutate( Year = case_when(year(Date) == "2015"~"2015", year(Date) == "2014"~"2014",year(Date) == "2013"~"2013", year(Date) == "2012"~"2012", year(Date) == "2011"~"2011", year(Date) == "2010"~"2010")) %>% group_by(Year, Category) %>% summarise(Count=n())
ggplot(crime10to15, aes(x = Year, y = Count)) + geom_point(aes(col = Category)) + geom_line(aes(col = Category, group = crime10to15$Category)) +  theme(panel.grid = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(family = "Garamond", size =14), axis.text.y = element_text(family = "Garamond", size =14)) + labs(title = "Crime patterns from '10 to '15", size = rel(2))

#plotting area of juvenile crime using leaflet()
juvecrime_plot <- SF %>% select(Category, Descript, Time, X, Y) %>% filter(SF$Descript %in% c("JUVENILE INVOLVED"), SF$PdDistrict %in% c("SOUTHERN")) %>% group_by(Category, Descript, Time, X, Y) %>% summarise(Count = n()) %>% arrange(desc(juvecrime_freq$Count))
juve_map <- juvecrime_plot %>% leaflet() %>% addTiles() %>% addMarkers(lat = juvecrime_plot$Y, lng = juvecrime_plot$X, popup = "Areas of Juvenile crime")
juve_map

#Heat map to show the various times of crime incidents reported
SF$Time <- paste0(SF$Time,":00")
SF$Time <- hms(SF$Time)
juvecrime_time <- SF %>% select(Category:Location) %>% filter(SF$Resolution %in% c("JUVENILE BOOKED", "JUVENILE ADMONISHED", "JUVENILE CITED", "JUVENILE DIVERTED", "CLEARED-CONTACT JUVENILE FOR MORE INFO")) %>% group_by(Category, Descript, PdDistrict, Time) %>% summarise(Count = n())
juvecrime_time$DayPeriod <- ifelse(juvecrime_time$Time >= '00H 00M 00S' & juvecrime_time$Time <= '06H 00M 00S',"EarlyMorn", ifelse(juvecrime_time$Time > '06H 00M 00S' & juvecrime_time$Time <= '12H 00M 00S', "Forenoon", ifelse(juvecrime_time$Time > '12H 00M 00S' & juvecrime_time$Time <= '18H 00M 00S', "Afternoon", "Evening" )))
temp <- aggregate(juvecrime_time$Category, by= list(juvecrime_time$Category, juvecrime_time$DayPeriod), FUN = length)
names(temp) <- c("crime", "day_period", "count")
ggplot(temp, aes(x= crime, y = factor(day_period))) +
  + geom_tile(aes(fill= count)) + scale_x_discrete("Crime", expand = c(0,0)) + scale_y_discrete("Time of day", expand = c(0,-2)) +
  + scale_fill_gradient("Number of crimes", low = "white", high = "steelblue") + theme_bw() + ggtitle("Crimes by time of day") +
  + theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line (colour = NA))
