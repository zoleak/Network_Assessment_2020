### Script to create map of population density for all counties in NJ with 
### ambient air quality monitoring stations. This script will utlize the tidycensus package to 
### retrieve population data from the US Census Bureau ###
### Author: Kevin Zolea; Date: 9/2019 ###
########## Download necessary packages to make script run ##################################
if (!require(pacman)) {
  install.packages('pacman')
  
}

pacman::p_load("tidyverse","tidycensus","sf","ggsn","cowplot","viridis","readxl")
###########################################################################################
### Install package from github that allows nice labeling on maps ###
### Would normally use above method with pacman but getting trouble for some reason ###
devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel) # load package in 
### Read in ambient air quality monitoring stations shapefile ###
NJ_air_stations<-read_csv("Ambient_Air_Quality_Monitors_of_New_Jersey.csv",col_names = T)%>%
  dplyr::select(-OBJECTID,-AIRS_CODE)
###########################################################################################
## Read in NJ Municipality population density ###
NJ_pop_estimates<-read_xlsx("density.xlsx","NJ-MCD2018",col_names = T,skip = 2)%>%
  select(MUN,pop_density_2018)%>%
  mutate(MUN = toupper(MUN))
### Make NJ_air_stations an sf object so we can plot it on map ###
NJ_air_stations<-st_as_sf(NJ_air_stations,coords = c("LON", "LAT"))
### transform coordinate system to match NJ counties ###
NJ_air_stations <- st_set_crs(NJ_air_stations, "+proj=longlat +datum=WGS84")
### Read in NJ muncipalities shapefile ###
NJ_muni<-st_read(dsn= getwd(),layer="New_Jersey_Municipalities")
NJ_pop_estimates$MUN<-gsub("TOWNSHIP","TWP",NJ_pop_estimates$MUN)
NJ_pop_estimates$MUN<-gsub("BOROUGH","BORO",NJ_pop_estimates$MUN)
### join muni shapefile and muni pop density together ###
nj_pop_muni<-left_join(NJ_pop_estimates,NJ_muni,by = "MUN")
nj_pop_muni<-st_as_sf(nj_pop_muni)
###########################################################################################
### Create connection to API with special API key from US Census Bureau ###
#census_api_key("1c32b297e99b22d82fb12c683f56d95eb9e12168")
###########################################################################################
### Now retrieve US Census Bureau data from API by using function from tidycensus package ###
#NJ_pop_estimates_test<-get_estimates(geography = "county",state = "NJ",
#                                variables = "DENSITY",geometry = T)
###########################################################################################
### Make breaks for legend ###
nj_pop_muni$brks <- cut(nj_pop_muni$pop_density_2018, 
                   breaks=c(0, 100, 500, 1000, 10000, 15000), 
                   labels=c("0 - 100", "100 - 500", "500 - 1000",
                            "1000-10000","10000 - 15000"))
###########################################################################################
### Create map using ggplot2 ###
pop_map<-ggplot()+
  #geom_sf(data = nj_pop_muni,aes(fill=brks))+
  geom_sf(data = nj_pop_muni)+
  #geom_sf(data = NJ_counties,fill="white")+
  geom_sf(data = NJ_air_stations,aes(color="NJ Ambient Air Quality Monitoring Stations"),
          size=3.5,key_glyph = draw_key_dotplot)+
  #geom_sf_label_repel(data = NJ_air_stations,aes(label=SITE_NAME))+
  #geom_sf_text_repel(data = subset(NJ_air_stations, X < -75),aes(label = SITE_NAME),
  #                    force = 100, nudge_x = 3.5- subset(NJ_air_stations, X < -75)$X,hjust=1,segment.size = 0.2,
  #                    direction     = "y",
  #                    hjust         = 0)+
  #geom_sf_label_repel(data = subset(NJ_air_stations, X > -75),aes(label = SITE_NAME),
  #                    force = 100, nudge_x = 2.7- subset(NJ_air_stations, X > -75)$X,hjust=1,segment.size = 0.2,
  #                    direction     = "y",
  #                    hjust         = 1)+
  ggtitle("New Jersey Population Density")+
  xlab("")+
  ylab("")+
  labs(subtitle = "Source: NJ Department of Labor and Workforce Development")+
  cowplot::background_grid(major = "none", minor = "none") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position=c(-0.3,0.8),
        legend.text = element_text(size=12),
        legend.title = element_text(colour="black", size=12, face="bold"),
        plot.title=element_text(size=20, face="bold",hjust =0.5),
        legend.box.background = element_rect(colour = "black"),
        plot.subtitle = element_text(hjust = 0.5,size=12))+
  ggsn::scalebar(NJ_pop_estimates, dist = 25, st.size=3, height=0.01,model = 'WGS84',
                 transform = T,dist_unit = "mi",location = "bottomright")+
  scale_fill_manual("",values = c("green","yellow","orange","red","#8b0000"))+
  scale_color_manual("Population Density per Municipalities (2018 Estimates)",
                     values = c("NJ Ambient Air Quality Monitoring Stations"="black"))
### Creates north arrow for map using the ggsn package ###
north2(pop_map, x = 0.35, y = 0.15, scale = 0.1, symbol = 3)
###########################################################################################


# save 13 by 11










