#####################################################X
#--------------Code for study area map---------------X
#----------------Started Dec 17 2020-----------------X
#-----------------------VAW--------------------------X
######################################################
######################################################

#Set options----
options(stringsAsFactors = FALSE)
Sys.setenv(TZ ="UTC") #R needs to know TimeZone to bring in time properly

#installing packages code
install.packages("rgdal")

#Load packages----
library(lubridate) #for dates and times
library(dplyr) #for data.frame manipulation
library(ggplot2) # prefferd graphing package 
library(rnaturalearth) # for plotting maps (lines 25 - )
library(rnaturalearthdata)
library(geosphere)
library(ggspatial)
library(sf)
library(rgeos) 
library(maps)
#library(plotly) # another graphing package

# set directory
setwd("~/Documents/Projects/Data/division /GIS/PRHO20200625")
getwd()

# read in shapefiles
PH <- st_read("Pronghorn.shp")

#View shapefile
st_geometry_type(PH)
st_crs(PH)

# visualize the boundaries
ggplot() + 
  geom_sf(data = PH, size = 1, color = "black", fill = "green") + 
  ggtitle("UDWR boundaries") + 
  coord_sf()



##### Trying some mapping code--------
#try and plot management areas for study area map

### load the states map----
# sf: objects with simple features frame row, 
#                       consisting of attributes and geometry.

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# Add in states so I can label/input state boundaries
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)
class(states)

##view the US map, this sometimes has to run twice before it works
ggplot(data = states) +
  geom_sf()

## map out by management area -----
## Don't know why this takes a few tries to load - but works eventually
#  facet wrapped bc I could figure out how to show on single map
#                                             with diff colors for each area

ggplot(data = states, fill = "lightgrey", color = "black") +
  geom_sf(data = PH, fill = "lightgrey", color = "black") +
  geom_point(data = ph_2, aes(x = lon, y = lat, color = unit)) +
  xlab("Longitude") + ylab("Latitude")+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ggtitle("UDWR Management Units")+
  coord_sf(xlim = c(-116.5, -106.5), ylim = c(36.5, 42.5), expand = FALSE)
