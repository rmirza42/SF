
1. Introduction

The San Francisco Police operate across 10 precincts within the city and aim to make San Francisco a safe city to reside in. To this end, an accessible method allowing both residents and tourists to check where crime hotspots are located would allow them to make informed decisions about where they go and encourage preventative action. This would be a useful service for the Police to promote with the aim of reducing opportunistic crimes by raising awareness of the highest risk areas.
For locals, this may be in the context of checking incidents of juvenile crime to identify which areas in the city need more attention in terms of curbing juvenile crime.

This project aims to address several key questions:
1. Is juvenile crime on the increase or decrease in San Francisco?
2. Is the frequency of specific crimes reducing or increasing on an annual basis?
3. Is there any seasonal variation in the frequency of crimes?
4. Are certain crimes more prevalent in certain areas - where are these ‘hotspots’?
5. Create a visualisation allowing easy identification of risk level to a certain crime
6. Can correlations be made between crime occurrences and other indicators e.g. employment levels? 
7. Can time series forecasting be used to estimate occurence of crime in the future?

2. Data acquisition
The dataset used for analyzing the various trends was avaiable to download from the following URL:
  https://data.sfgov.org/Public-Safety/Map-of-Police-Department-Incidents/gxxq-x39z

3. Important fields and information
Variable names, types and number of observations were reviewed using glimpse() on the original dataframe. This reveals 12 variables - 9 characters, 3 numerics (1 integer, 2 double precision). There are 2,118,887 observations in total.
The following variables are returned: Incident No, category, Description, Day of the week, Date, Time, PdDistrict, Resolution, Address, latitude, longitude, Location.
There are 39 different types of crime recorded in the category variable. Latitude and longitude are provided as geographical coordinates in decimal degrees. Location data is only referencing the approximate location using ‘anonymous’ map points.

4. Data limitations
The main limitations that have been identified with the dataset are:
  
• Anonymistation of the data means the locations are fixed to a ‘map point’ and jittering is required to make all the events visible, although as the dataset gets larger it is not possible to create an effective map visualisation of individual crimes.
• Anonymisation also means no day/time is provided so it is not possible to analyse potential relationships on a weekly scale with certain crimes.

5. Data cleaning and wrangling:


6. Data Investigation:

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

# Vandalism Time series plot starting from 2012 :
df_vandalism <- SF %>% select(Category:PdDistrict) %>% filter(Category == "VANDALISM" & Date >= "2012-01-01")
getSeason <- c(
  "Winter", "Winter",
  "Spring", "Spring", "Spring",
  "Summer", "Summer", "Summer",
  "Fall", "Fall", "Fall",
  "Winter"
)
df_vandalism$Season <- getSeason[ 1 + as.POSIXlt(df_vandalism$Date)$mon ]
df_vandalism$Date <- as.Date(df_vandalism$Date)
f_vandalism <- df_vandalism %>% group_by(Category, Date, Season) %>% summarise(Count = n())
season.cols <- c("Summer" = "orange", "Fall" = "darkgreen", "Winter" = "darkblue", "Spring" = "purple")
TDates <- c("2011-06-27", "2012-08-19", "2013-07-22", "2014-07-18", "2015-07-01", "2016-07-19")
ggplot(df_vandalism, aes(x = Date, y = Count)) +
  geom_line(aes(group = Category, col = Season)) + 
  scale_colour_manual(values = season.cols) +
  scale_x_date(date_breaks = "6 month", date_labels = "%m-%y") +
  ylab("Number of crimes") +
  xlab("Date") +
  ggtitle("Vandalism time series 2012 - 2017") +
  theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(lineheight=.8, face="bold")) +
  geom_vline(xintercept = as.numeric(as.Date(TDates)), linetype=2, col = "red")


# point pattern analysis - kernel density estimation for various types of crime

SF <- fread("Map_of_Police_Department_Incidents.csv")

juvecrime_plot <- SF %>% select(Category, Descript, X, Y) %>% filter(SF$Descript %in% c("JUVENILE INVOLVED"), SF$PdDistrict %in% c("SOUTHERN")) %>% group_by(Category, Descript, X, Y) %>% summarise(Count = n())
latlong <- cbind(juvecrime_plot$X, juvecrime_plot$Y)
kde <- bkde2D(latlong, bandwidth = c(.0055, .0048), gridsize = c(75,75))
cl <- contourLines(kde$x1 , kde$x2 , kde$fhat)
levs <- as.factor(sapply(cl, `[[`, "level"))
nlevs <- length(levels(levs))
pgons <- lapply(1:length(cl), function(i)
  Polygons(list(Polygon(cbind(cl[[i]]$x, cl[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)
leaflet(spgons) %>% addTiles() %>%
  addPolygons(color = heat.colors(nlevs, NULL)[levs])



vandalism_plot <- SF %>% select(Category, Descript, X, Y) %>% filter(SF$Category == "VANDALISM") %>% group_by(Category, Descript, X, Y) %>% summarise(Count = n())
latlong <- cbind(vandalism_plot$X, vandalism_plot$Y)
kde <- bkde2D(latlong, bandwidth = c(.0055, .0048), gridsize = c(7500,7500))
cl <- contourLines(kde$x1 , kde$x2 , kde$fhat)
levs <- as.factor(sapply(cl, `[[`, "level"))
nlevs <- length(levels(levs))
pgons <- lapply(1:length(cl), function(i)
  Polygons(list(Polygon(cbind(cl[[i]]$x, cl[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)
leaflet(spgons) %>% addTiles() %>%
  addPolygons(color = heat.colors(nlevs, NULL)[levs])

burglary_plot <- SF %>% select(Category, Descript, X, Y) %>% filter(SF$Category == "BURGLARY") 
kde <- bkde2D(latlong, bandwidth = c(.0055, .0048), gridsize = c(6000,6000))
cl <- contourLines(kde$x1 , kde$x2 , kde$fhat)
levs <- as.factor(sapply(cl, `[[`, "level"))
nlevs <- length(levels(levs))
pgons <- lapply(1:length(cl), function(i)
  Polygons(list(Polygon(cbind(cl[[i]]$x, cl[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)
leaflet(spgons) %>% addTiles() %>%
  addPolygons(color = heat.colors(nlevs, NULL)[levs])

#choropleth maps for analysis 
# arson crime

#step 1 : download the shapefile(.shp) for the San Francisco police department districts

sf <- fread("Map_of_Police_Department_Incidents.csv")
sf$Date <- mdy_hms(as.character(sf$Date))
setwd("/Users/Infy/Desktop/SpringBoard/sfpd_districts")

districts <- readOGR(dsn = ".", "sfpd_districts", verbose = FALSE)
pd_districts <- districts %>% subset(DISTRICT %in% c('CENTRAL' , 'SOUTHERN', 'BAYVIEW', 'MISSION', 'PARK', 'RICHMOND', 'INGLESIDE', 'TARAVAL', 'NORTHERN', 'TENDERLOIN'))
crime16 <- sf %>% select (Category, PdDistrict, Date, Address, X, Y) %>% filter(year(Date) == "2015") %>% group_by(Category, PdDistrict, Date, Address, X, Y) %>% summarise(Count =n())
names(crime16) <- sub("^Date$", "Year", names(crime16))
names(crime16) <- sub("^Count$", "Frequency", names(crime16))
names(crime16) <- sub("^Y$", "Latitude", names(crime16))
names(crime16) <- sub("^X$", "Longitude", names(crime16))

coords <- SpatialPoints(crime16[,c("Longitude","Latitude")])
crime16 <- SpatialPointsDataFrame(coords, crime16)
proj4string(crime16) <- CRS("+init=epsg:4269")
pd_districts <- spTransform(pd_districts, CRS("+init=epsg:4269"))
pd_districts@proj4string

arson_crime <- crime16[crime16$Category == "ARSON",]
proj4string(crime16) <- proj4string(pd_districts)
proj4string(arson_crime) <- proj4string(pd_districts)
pointsinpolygon <- over(SpatialPolygons(pd_districts@polygons), SpatialPoints(arson_crime), returnList = TRUE)
pd_districts$arson_crime <- unlist(lapply(pointsinpolygon, length))

classes <- classIntervals(pd_districts$arson_crime, n=5, style="jenks")
pd_districts_f <- fortify(pd_districts, region="DISTRICT")
pd_districts_f <- left_join(pd_districts_f, pd_districts@data, by = c("id" = "DISTRICT"))

ggplot() +
  +     geom_polygon(data = pd_districts_f, aes(x = long, y = lat, group = group,
                                                +                                        fill = arson_crime), color = "white", size = 0.2) +
  +     coord_map() +
  +     scale_fill_gradientn('Frequency\n', colours = brewer.pal(5,"YlOrRd"), breaks = classes$brks) +
  +     theme_nothing(legend = TRUE) +
  +     labs(title = "Reported arson crime in San Francisco by district (2015)")

district_popup <- paste0("<strong>District: </strong>",
                         +                         pd_districts$DISTRICT,
                         +                         "<br><strong>Crime (2015): </strong>",
                         +                         pd_districts$arson_crime)

colcode2 <- findColours(classes, c("darkgrey", "yellow", "orange", "red", "sienna"))
breaks <- round(classes$brks, 1)
labels = matrix(1:(length(breaks)-1))
for(j in 1:length(labels)){labels [j] =
  +     paste(as.character(breaks[j]),"-",as.character(breaks[j+1]))}

leaflet(data = pd_districts) %>% 
  +     addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  +     addPolygons(data = pd_districts, 
                    +                 fillColor = colcode2, 
                    +                 fillOpacity = 0.6, 
                    +                 color = "#636363", 
                    +                 weight = 2, 
                    +                 popup = district_popup)  %>%
  +     addLegend(position = "bottomright",
                  +               colors = c("darkgrey", "yellow", "orange", "red", "sienna"),
                  +               labels = labels,
                  +               opacity = 0.8,
                  +               title = "Number of Arson Crime")



















