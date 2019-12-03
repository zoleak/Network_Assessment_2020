### Script to create map of population density for all Municipalities in NJ with 
### ambient air quality monitoring stations ###
### Author: Kevin Zolea; Date: 12/2019 ###
########## Download necessary packages to make script run ##################################
if (!require(pacman)) {
  install.packages('pacman')
  
}

pacman::p_load("tidyverse","tidycensus","sf","ggsn","cowplot","viridis","readxl","ggrepel")
pacman::p_load_gh("ggsflabel")
###########################################################################################
### Install package from github that allows nice labeling on maps ###
#devtools::install_github("yutannihilation/ggsflabel")
### Would normally use above method with pacman but getting trouble for some reason ###
#library(ggsflabel) # load package in 
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
### Need to make changes to values in dataframe so we can join the shapefile and data together to map ###
NJ_pop_estimates$MUN<-gsub("TOWNSHIP","TWP",NJ_pop_estimates$MUN)
NJ_pop_estimates$MUN<-gsub("BOROUGH","BORO",NJ_pop_estimates$MUN)
NJ_pop_estimates$MUN<-gsub("PEAPACK AND GLADSTONE BORO","PEAPACK-GLADSTONE BORO",NJ_pop_estimates$MUN)
NJ_pop_estimates$MUN<-gsub("HILLSBORO TWP","HILLSBOROUGH TWP",NJ_pop_estimates$MUN)
### join muni shapefile and muni pop density together ###
nj_pop_muni<-left_join(NJ_pop_estimates,NJ_muni,by = "MUN")
nj_pop_muni<-st_as_sf(nj_pop_muni)
###########################################################################################
### THIS IS JUST TO LOOK BACK ON THIS WILL NOT BE USED ###
##########################################################
### Create connection to API with special API key from US Census Bureau ###
#census_api_key("1c32b297e99b22d82fb12c683f56d95eb9e12168")
###########################################################################################
### Now retrieve US Census Bureau data from API by using function from tidycensus package ###
#NJ_pop_estimates_test<-get_estimates(geography = "county",state = "NJ",
#                                variables = "DENSITY",geometry = T)
###########################################################################################
### Make breaks for legend ###
### This is needed to make the different color breaks in the legend ###
nj_pop_muni$brks <- cut(nj_pop_muni$pop_density_2018, 
                   breaks=c(0, 10, 25, 50, 100, 250,500,1000,2500,5000,58000), 
                   labels=c("0 - 10", "10 - 25", "25 - 50",
                            "50-100","100 - 250","250 - 500","500 - 1000","1000 - 2500",
                            "2500 - 5000","5000 - 58000"))
###########################################################################################
### Create map using ggplot2 ###
pop_map<-ggplot()+
  geom_sf(data = nj_pop_muni,aes(fill=brks))+
  geom_sf(data = NJ_air_stations,aes(color="NJ Ambient Air Quality Monitoring Stations"),
          size=3.5,key_glyph = draw_key_dotplot)+
  #geom_sf_text_repel(data = subset(NJ_air_stations, X < -75),aes(label = SITE_NAME),
  #                    force = 100, nudge_x = 3.5- subset(NJ_air_stations, X < -75)$X,hjust=1,segment.size = 0.2,
  #                    direction     = "y",
  #                    hjust         = 0)+
  #geom_sf_text_repel(data = subset(NJ_air_stations, X > -75),aes(label = SITE_NAME),
  #                    force = 100, nudge_x = 2.7- subset(NJ_air_stations, X > -75)$X,hjust=1,segment.size = 0.2,
  #                    direction     = "y",
  #                    hjust         = 1)+
  geom_sf_text_repel(data = NJ_air_stations,aes(label = SITE_NAME),
                                         nudge_x = 2.7,fontface="bold")+
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
        legend.position=c(-0.4,0.7),
        legend.text = element_text(size=12),
        legend.title = element_text(colour="black", size=12, face="bold"),
        plot.title=element_text(size=20, face="bold",hjust =0.5),
        legend.box.background = element_rect(colour = "black"),
        plot.subtitle = element_text(hjust = 0.5,size=12))+
  ggsn::scalebar(nj_pop_muni, dist = 25, st.size=4, height=0.01,model = 'WGS84',
                 transform = F,dist_unit = "mi",location = "bottomright")+
  scale_fill_manual("Population Density\nper Municipalities (2018 Estimates)",
                    values = c("green","#4ca64c","#9acd32","#b8dc6f","yellow",
                                  "#FFAE42","orange","#FF4500","red","#8b0000"))+
  scale_color_manual("",
                     values = c("NJ Ambient Air Quality Monitoring Stations"="black"))

                         
### Creates north arrow for map using the ggsn package and saves map ###
pdf(file="NJ_pop_density_2018_estimates.pdf",width = 13,height = 11)
north2(pop_map, x = 0.35, y = 0.15, scale = 0.1, symbol = 3)
dev.off()
###########################################################################################


# save 13 by 11










