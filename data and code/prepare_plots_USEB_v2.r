
library(ggplot2)
library(reshape2)
#library(plyr)
library(dplyr)
library(tidyr)
library(ggpubr)


#rm(list=ls())

attendance<-read.delim("number of attendies3.txt", sep="\t", header=TRUE, stringsAsFactors = FALSE)

attendance

attendance_mlt <-melt(attendance)

attendance_mlt_mean<-attendance_mlt %>%
  group_by(day,variable) %>%
  summarise_at(vars(value), list(name = mean))

attendance_mlt_mean

orderDays<-c("Monday","Tuesday","Wednesday","Thursday","Friday")


attendance_mlt_mean

attendance_mlt_mean$variable_clean <- NA

attendance_mlt_mean$variable_clean[attendance_mlt_mean$variable == "in_person_lecturers"] <- "Lecturers, in person"
attendance_mlt_mean$variable_clean[attendance_mlt_mean$variable == "in_person_participants"] <- "Participants, in person"
attendance_mlt_mean$variable_clean[attendance_mlt_mean$variable == "online.zoom"] <- "Online via zoom"
attendance_mlt_mean$variable_clean[attendance_mlt_mean$variable == "YoutubeLive"] <- "Youtube live stream"
attendance_mlt_mean$variable_clean[attendance_mlt_mean$variable == "YoutubeTotal"] <- "Total Youtube views"
attendance_mlt_mean$variable_clean[attendance_mlt_mean$variable == "total"] <- "Total views and attendance"


add_statsAttendance_plot<-ggplot(data=attendance_mlt_mean, mapping=aes(fill=as.factor(variable_clean),x=name, y=factor(day,levels=rev(orderDays))))+geom_point(size=6,alpha=0.7,shape=21)+
  theme(legend.title = element_blank(),panel.background = element_blank(),axis.title.y = element_blank(), axis.title.x = element_text(size=16),axis.text.x = element_text(size=16),axis.text.y = element_text(size=16))+xlab("Number of attendees estimated on the last day of school\nat 10am Eastern European Time")#+scale_fill_manual(values=c('skyblue', 'salmon'))
add_statsAttendance_plot


#### advertisement stats

add_stats<-read.delim("advertisement_stats.txt", sep="\t", header=TRUE, stringsAsFactors = FALSE)
add_stats

add_statsCount<-as.data.frame(table(add_stats$clean_col))


add_statsCountSorted <- add_statsCount[order(-add_statsCount$Freq),]
row.names(add_statsCountSorted) <- NULL
add_statsCountSorted

orderAdds<-(add_statsCountSorted$Var1)
orderAdds

add_statsCountSorted_plot<-ggplot(data=add_statsCountSorted, mapping=aes(fill="firebrick4",x=Freq, y=factor(Var1,levels=rev(orderAdds))))+geom_bar(stat = "identity",color="black",alpha=0.7)+
  theme(legend.position="none",legend.title = element_blank(),panel.background = element_blank(),axis.title.y = element_blank(), axis.title.x = element_text(size=16),axis.text.x = element_text(size=16),axis.text.y = element_text(size=16))+xlab("Number of applicants")
add_statsCountSorted_plot




## country breakdown on the map:
library(leaflet)
library(sf)
library(htmlwidgets)

locationLecturers <-read.delim("List_of_speakers locations v3.txt", sep="\t", header=TRUE, stringsAsFactors = FALSE)
locationLecturers


  

ctryResCount<-read.delim("USEB 2025 Application repllies 29.01.2024 v3 count by country of residence.txt", sep="\t", header=TRUE, stringsAsFactors = FALSE)
ctryResCount

world_sf <- read_sf("TM_WORLD_BORDERS_SIMPL-0.3.shp")


#map <- leaflet(world_sf) %>% addTiles()



world_sf_plus <- world_sf %>% 
  left_join(ctryResCount, by = c("NAME" = "Country"))




map <- leaflet(world_sf_plus) %>% addTiles()
map



pal <- colorNumeric(
  palette = "plasma",
  domain =  world_sf_plus$Number,
  na.color = "white"
)

map<-map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
              color = ~pal(Number)
  ) %>%
  addLegend("topright", pal = pal, values = ~Number,
            title = "Number of\n participants",
            labFormat = labelFormat(prefix = ""),
            opacity = 1
  )

map


map <- map %>% addCircleMarkers(data = data.frame(lng = locationLecturers$Longitude , lat = locationLecturers$Latitude  ),radius=2) 
map




###############
## map no legend

map_no_legend <- leaflet(world_sf_plus) %>% addTiles()
map_no_legend



pal <- colorNumeric(
  palette = "plasma",
  domain =  world_sf_plus$Number,
  na.color = "white"
)

map_no_legend<-map_no_legend %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
              color = ~pal(Number)
  )

map_no_legend


map_no_legend <- map_no_legend %>% addCircleMarkers(data = data.frame(lng = locationLecturers$Longitude , lat = locationLecturers$Latitude  ),radius=2) 
map_no_legend






####################
## number of publications per year
## historical events

eventsUA <-read.delim("historical events Ukraine v1.txt", sep="\t", header=TRUE, stringsAsFactors = FALSE)
eventsUA$event[eventsUA$event == ''] <- NA
eventsUA$year_bk <- eventsUA$year
eventsUA<-eventsUA[!is.na(eventsUA$event) ,]
eventsUA




eventsAll <-read.delim("historical events All.txt", sep="\t", header=TRUE, stringsAsFactors = FALSE)
eventsAll$event[eventsAll$event == ''] <- NA
eventsAll$year_bk <- eventsAll$year
eventsAll<-eventsAll[!is.na(eventsAll$event) ,]
eventsAll



## plots numbers and annotations
countriesAndPublications <-read.delim("allCountries_in_v1.txt", sep="\t", header=TRUE, stringsAsFactors = FALSE)
countriesAndPublications

countriesAndPublications<-countriesAndPublications[countriesAndPublications$Final.Publication.Year > 1990, ]
countriesAndPublications<-countriesAndPublications[countriesAndPublications$Final.Publication.Year < 2025, ]

countriesAndPublications<-merge(countriesAndPublications,eventsAll,by.x="Final.Publication.Year",by.y="year_bk",all.x=TRUE)
countriesAndPublications



countriesAndPublications_UA<-countriesAndPublications[countriesAndPublications$Country == "Ukraine" & countriesAndPublications$Field == "Evolution",  ]
colnames(countriesAndPublications_UA)




colnames(countriesAndPublications_UA) <- c("Final.Publication.Year","Count","Country","Field","year_event_global","event_global")
countriesAndPublications_UA

eventsUA


countriesAndPublications_UA<-merge(countriesAndPublications_UA,eventsUA,by.x="Final.Publication.Year",by.y="year_bk",all.x=TRUE)
countriesAndPublications_UA



countriesAndPublications_noUSA <- countriesAndPublications[countriesAndPublications$Country !="USA",]
countriesAndPublications_noUSA


head(countriesAndPublications_UA)


########################################################
## final figures


## with years
publicationByYearEvolUA<-ggplot(data=countriesAndPublications_UA, mapping=aes(group=Country,x=Final.Publication.Year, y=Count,label=year))+
  geom_vline(xintercept = 1991, color = "gray80", size=1)+
  geom_vline(xintercept = 2000, color = "gray80", size=1)+
  geom_vline(xintercept = 2004, color = "gray80", size=1)+
  geom_vline(xintercept = 2008, color = "gray80", size=1)+
  geom_vline(xintercept = 2013, color = "gray80", size=1)+
  geom_vline(xintercept = 2014, color = "gray80", size=1)+
  geom_vline(xintercept = 2020, color = "gray80", size=1)+
  geom_vline(xintercept = 2022, color = "gray80", size=1)+
  geom_line(size=1.5,color="skyblue2",alpha=0.7)+ geom_point(size=3,fill="skyblue2",shape=21,alpha=0.7)+geom_text(hjust=0, vjust=-2)+
  theme(legend.position="none",legend.title = element_blank(),panel.background = element_blank(),axis.title.y = element_text(size=16), axis.title.x = element_text(size=16),axis.text.x = element_text(size=16),axis.text.y = element_text(size=16))+
  xlab("Year")+ylab("Number of publications")

publicationByYearEvolUA


# remove year label, for aligning with seceond plot with other countries
publicationByYearEvolUA<-ggplot(data=countriesAndPublications_UA, mapping=aes(group=Country,x=Final.Publication.Year, y=Count,label=year))+
  geom_vline(xintercept = 1991, color = "gray80", size=1)+
  geom_vline(xintercept = 2000, color = "gray80", size=1)+
  geom_vline(xintercept = 2004, color = "gray80", size=1)+
  geom_vline(xintercept = 2008, color = "gray80", size=1)+
  geom_vline(xintercept = 2013, color = "gray80", size=1)+
  geom_vline(xintercept = 2014, color = "gray80", size=1)+
  geom_vline(xintercept = 2020, color = "gray80", size=1)+
  geom_vline(xintercept = 2022, color = "gray80", size=1)+
  geom_line(size=1.5,color="skyblue2",alpha=0.7)+ geom_point(size=3,fill="skyblue2",shape=21,alpha=0.7)+geom_text(hjust=0, vjust=-2)+
  theme(legend.position="none",legend.title = element_blank(),panel.background = element_blank(),axis.title.y = element_blank(), axis.title.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(size=16))+
  xlab("Year")+ylab("Number of publications")

publicationByYearEvolUA


#########################################

## subset without USA to reduce total y-axis size
publicationByYear_noUSA_Evol<-countriesAndPublications[countriesAndPublications$Field =="Evolution" & countriesAndPublications$Country !="USA" , ]

countryColors=c("France"="navy",
                "Germany"="orange3",
                "Japan"="firebrick4",
                "Poland" = "brown2",
                "Ukraine"="skyblue2")


publicationByYear_noUSA_Evol_plot<-ggplot(data=publicationByYear_noUSA_Evol, mapping=aes(group=Country,x=Final.Publication.Year, y=Count,fill=Country))+
  geom_vline(xintercept = 2000, color = "gray80", size=1)+
  geom_vline(xintercept = 2008, color = "gray80", size=1)+
  geom_vline(xintercept = 2020, color = "gray80", size=1)+
  geom_vline(xintercept = 2022, color = "gray80", size=1)+
  geom_line()+ geom_point(size=3,shape=21,alpha=0.7)+
  theme(legend.title = element_blank(),panel.background = element_blank(),axis.title.y = element_text(size=16), axis.title.x = element_text(size=16),axis.text.x = element_text(size=16),axis.text.y = element_text(size=16))+
  xlab("Year")+ylab("Number of publications")+
  scale_fill_manual(values = countryColors)
publicationByYear_noUSA_Evol_plot




ggarrange(publicationByYearEvolUA,publicationByYear_noUSA_Evol_plot,ncol=1,align="hv")


print("all done ")