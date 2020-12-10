#Author: GeighZ
#Created: 12/10/2020
#Shape file Creator based on Covid-19 Data pulled from Github
#
#!!! Set your directory here. setwd(C:/Users/Someone/....)
install.packages(c("maps", "mapproj"))
library(RColorBrewer)
library(httr)
library(jsonlite)
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(sqldf)
library(maps)
library(mapproj)
library(zoo)
library("data.table")  
library(rgdal)

#Read Covid-19 Data from Github
res = read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")


# Download the base shapefile. (note that I store it in a folder called DATA. You have to change that if needed.)
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="DATA/world_shape_file.zip")
# You now have it in your current working directory, have a look!

# Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.
system("unzip DATA/world_shape_file.zip")
#  -- > You now have 4 files. One of these files is a .shp file! (TM_WORLD_BORDERS_SIMPL-0.3.shp)

#Change the dsn to your working directory, or the read in won't work.
world_spdf <- readOGR(dsn= "C:/Users/someone../Documents/R/rShinyProject/DATA/world_shape_file" , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

# Clean the data object a bit, although unneccessary...
world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)

#Gathering all Quarterly cases.
qtrNew_cases<-res%>%
  mutate(date=as.Date(date, format='%Y-%m-%d')) %>%
  group_by(iso_code, Qtr=as.character(as.yearqtr(date))) %>% 
  summarise(new_cases= sum(new_cases), date=last(date))


#Spliting Quarterly cases
class(qtrNew_cases)
qtrNew_cases[6,"new_cases"]
#Quarter 1 cases
cq1 <- qtrNew_cases[qtrNew_cases$Qtr %like% "2020 Q1", ]
aggregate(new_cases~+iso_code,cq1,sum)
cq1 = subset(cq1, select = -c(date,Qtr))
names(cq1)[names(cq1) == "new_cases"] <- "new_casesQ1"

#Quarter 2 cases
cq2 <- qtrNew_cases[qtrNew_cases$Qtr %like% "2020 Q2", ]
names(cq2)[names(cq2) == "new_cases"] <- "new_casesQ2"
cq2 = subset(cq2, select = -c(date,Qtr))

#Quarter 3 cases
cq3 <- qtrNew_cases[qtrNew_cases$Qtr %like% "2020 Q3", ]
names(cq3)[names(cq3) == "new_cases"] <- "new_casesQ3"
cq3 = subset(cq3, select = -c(date,Qtr))

#Quarter 4 cases
cq4 <- qtrNew_cases[qtrNew_cases$Qtr %like% "2020 Q4", ]
names(cq4)[names(cq4) == "new_cases"] <- "new_casesQ4"
cq4 = subset(cq4, select = -c(date,Qtr))

#Cases Merging
qtrValuesLeft <- merge(cq1,cq2, by = "iso_code")
qtrValuesRight<- merge(cq3,cq4, by = "iso_code")
qtrValuesCases <- merge(qtrValuesLeft,qtrValuesRight, by="iso_code")

#Gathering all Quarterly deaths.
qtrNew_deaths<-res%>%
  mutate(date=as.Date(date, format='%Y-%m-%d')) %>%
  group_by(iso_code, Qtr=as.character(as.yearqtr(date))) %>% 
  summarise(new_deaths= sum(new_deaths), date=last(date))

#Quarter 1 deaths
dq1 <- qtrNew_deaths[qtrNew_deaths$Qtr %like% "2020 Q1", ]
aggregate(new_deaths~+iso_code,res,sum)
dq1 = subset(dq1, select = -c(date,Qtr))
names(dq1)[names(dq1) == "new_deaths"] <- "new_deathsQ1"

#Quarter 2 deaths
dq2 <- qtrNew_deaths[qtrNew_deaths$Qtr %like% "2020 Q2", ]
dq2 = subset(dq2, select = -c(date,Qtr))
names(dq2)[names(dq2) == "new_deaths"] <- "new_deathsQ2"

#Quarter 3 deaths
dq3 <- qtrNew_deaths[qtrNew_deaths$Qtr %like% "2020 Q3", ]
dq3 = subset(dq3, select = -c(date,Qtr))
names(dq3)[names(dq3) == "new_deaths"] <- "new_deathsQ3"

#Quarter 4 deaths
dq4 <- qtrNew_deaths[qtrNew_deaths$Qtr %like% "2020 Q4", ]
dq4 = subset(dq4, select = -c(date,Qtr))
names(dq4)[names(dq4) == "new_deaths"] <- "new_deathsQ4"

#Deaths merging
qtrValuesLeft <- merge(dq1,dq2, by = "iso_code")
qtrValuesRight<- merge(dq3,dq4, by = "iso_code")
qtrValuesDeaths<- merge(qtrValuesRight,qtrValuesLeft, by = "iso_code")

#Gathering quarterly Stringency data
qtrStr_index<-res%>%
  mutate(date=as.Date(date, format='%Y-%m-%d')) %>%
  group_by(iso_code, Qtr=as.character(as.yearqtr(date))) %>% 
  summarise(stringency_index= mean(stringency_index), date=last(date))

#Quarter 1 stringency
sq1 <- qtrStr_index[qtrStr_index$Qtr %like% "2020 Q1", ]
aggregate(stringency_index~+iso_code,res,mean)
sq1 = subset(sq1, select = -c(date,Qtr))
names(sq1)[names(sq1) == "stringency_index"] <- "str_indexQ1"

#Quarter 2 stringency
sq2 <- qtrStr_index[qtrStr_index$Qtr %like% "2020 Q2", ]
sq2 = subset(sq2, select = -c(date,Qtr))
names(sq2)[names(sq2) == "stringency_index"] <- "str_indexQ2"

#Quarter 3 stringency
sq3 <- qtrStr_index[qtrStr_index$Qtr %like% "2020 Q3", ]
sq3 = subset(sq3, select = -c(date,Qtr))
names(sq3)[names(sq3) == "stringency_index"] <- "str_indexQ3"

#Quarter 4 stringency
sq4 <- qtrStr_index[qtrStr_index$Qtr %like% "2020 Q4", ]
sq4 = subset(sq4, select = -c(date,Qtr))
names(sq4)[names(sq4) == "stringency_index"] <- "str_indexQ4"

#Stringency merging.
qtrValuesLeft <- merge(sq1,sq2, by = "iso_code")
qtrValuesRight<- merge(sq3,sq4, by = "iso_code")
qtrValuesStringe<- merge(qtrValuesRight,qtrValuesLeft, by = "iso_code")

#Subsetting data for important values, I only care about these columns.
res = subset(res, select = c(iso_code,continent,location,population,population_density,aged_65_older,aged_70_older,
                             gdp_per_capita,handwashing_facilities,hospital_beds_per_thousand,human_development_index,
                             cardiovasc_death_rate,diabetes_prevalence))

#Removing Redundancy for smooth merging
res = distinct(res)
colTest <-colnames(world_spdf@data)

#Merging all Data
res <- merge(res,qtrValuesCases, by= "iso_code")
res <- merge(res,qtrValuesDeaths, by= "iso_code")
res <- merge(res,qtrValuesStringe, by="iso_code")

#Creation of the custom Shape file. All 4 files must be put into folder accessible
# by app.R, right now, I dragged mine into a folder named "custom". Will show up
# in current directory.
y<-merge(world_spdf,res, by.x= "NAME",by.y="location",all.y=TRUE)
writeOGR(y, ".", "yShape", driver="ESRI Shapefile") 


## TESTING PURPOSES ONLY!!!! Shape file is generated above already. ##

mybins <- c(0,10,20,50,100,500,Inf)
mypalette <- colorNumeric(palette="YlOrBr", domain=y@data$new_casesQ1, na.color="transparent", bins=mybins)

# Prepare the text for tooltips:
mytext <- paste(
  "Country: ", y@data$NAME,"<br/>", 
  "iso_code: ", y@data$ISO3, "<br/>", 
  "New Cases for Quarter 1: ", y@data$new_casesQ1, 
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map
leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(y@data$new_casesQ1), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~y@data$new_casesQ1, opacity=0.9, title = "Population (M)", position = "bottomleft" )





