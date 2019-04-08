install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("maps")

library(dplyr)
library(ggplot2)
library(plotly)
library(maps)

# setwd('~/Downloads/')
# df<-read.csv("ndbcSosWind.csv")
df<-read.csv("/Users/heyunyu/Downloads/ndbcSosWind.csv")
usa<-map_data('state')
# head(usa)
df_mean<-df %>% group_by(station_id,longitude, latitude) %>% 
  summarize(speed_mean=mean(wind_speed,na.rm=T),direction_mean=mean(wind_from_direction,na.rm=T)) %>% 
  mutate(u_wnd=speed_mean*cos(pi*direction_mean/180),v_wnd=speed_mean*sin(pi*direction_mean/180))

df_usa<-df_mean %>% 
  filter(latitude<=max(usa$lat) & latitude>=min(usa$lat)
         & longitude<= max(usa$long) & longitude >= min(usa$long))

usa_plot<-ggplot()+
  geom_polygon(data=usa,aes(x=long,y=lat,group=group),fill=NA,color="grey60")+
  coord_fixed(1.3)+ theme_bw()
usa_plot

usa_plot_scalar<- usa_plot + 
  geom_point(data=df_usa,aes(x=longitude,y=latitude,color=speed_mean))+
  scale_color_gradient(low='light blue', high='dark blue')
usa_plot_scalar

usa_plot_vector<-usa_plot + 
  geom_segment(data=df_usa,aes(x=longitude,y=latitude,xend=longitude+u_wnd/10,yend=latitude+v_wnd/10),arrow=arrow(length=unit(0.1,'cm')),size=0.3)
usa_plot_vector

lakes<-subset(usa,region %in% c("ohio",'indiana','michigan',"illinois","winsconsin",'minnesota'))
head(lakes)
df_lakes<-df_mean %>% filter(latitude<=max(lakes$lat) & latitude>=min(lakes$lat)
                             & longitude<= max(lakes$long) & longitude >= min(lakes$long))

lakes_plot<-ggplot() + geom_polygon(data=lakes,aes(x=long,y=lat,group=group),fill=NA,color="grey60")+
  coord_fixed(1.3)+ theme_bw()
lakes_plot

lakes_plot_scalar<- lakes_plot + 
  geom_point(data=df_lakes,aes(x=longitude,y=latitude,color=speed_mean))+
  scale_color_gradient(low='light blue', high='dark blue')
lakes_plot_scalar

lakes_plot_vector<-lakes_plot + 
  geom_segment(data=df_lakes,aes(x=longitude,y=latitude,xend=longitude+u_wnd/10,yend=latitude+v_wnd/10),arrow=arrow(length=unit(0.1,'cm')),size=0.3)
lakes_plot_vector

df$date<-as.Date(df$time,formate='%Y-%m-%d')

df_daily <- df %>% 
  group_by(station_id, longitude, latitude, date) %>% 
  summarize(speed_mean = mean(wind_speed, na.rm=T),
            direction_mean = mean(wind_from_direction, na.rm=T)) %>% 
  mutate(u_wnd = speed_mean*cos(pi*direction_mean/180),
         v_wnd = speed_mean*sin(pi*direction_mean/180)) %>% 
  filter(latitude <= max(lakes$lat) &
           latitude >= min(lakes$lat) &
           longitude <= max(lakes$lon) &
           longitude >= min(lakes$lon))

df_lakes_animate <- df_daily %>%
  filter(n() == 8)
#df_daily<-df %>% 
  #group_by(station_id,longitude,latitude,date) %>%
  #summarize(speed_mean=mean(wind_speed,na.rm=T),direction_mean=mean(wind_from_direction,na.rm=T)) %>%
  #mutate()

animated_vectors<- lakes_plot+
  geom_segment(data = df_lakes_animate, aes(x=longitude, y=latitude, xend=longitude+u_wnd/10,
                                            yend=latitude+v_wnd/10, frame=as.numeric(as.POSIXct(date))),
               arrow = arrow(length = unit(0.1, 'cm')), size=0.3)
ggplotly(animated_vectors)
