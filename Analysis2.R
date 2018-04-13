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
