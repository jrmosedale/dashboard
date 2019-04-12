#  Preapre mapping data of Tevi businesses, sector etc for use by Dashboard app
# Source data = Gazeteer and Sector data shapefiles

library(flexdashboard)
library(bsselectR)
library(raster)
library(leaflet)
library(magrittr)
library(colorspace)
library(rgeos)
library(ggplot2)
library(rgdal)
library(sf)
library(units)
library(leaflet.extras)
library(htmlwidgets)
library(zonator)
library(RColorBrewer)

in.root<-paste0(getwd(),"/data/")

# From working project directory
# Load parishes
#parishes<-st_read(paste0(in.root,"Parishes/cornwallparish.shp"))
#st_crs(parishes)<-st_crs(27700)
# Load CNA
cna.shp<-st_read(paste0(in.root,"community_network_areas.shp"))
st_crs(cna.shp)<-st_crs(27700)
names(cna.shp)<-c("OBJECTID","CNA","MEMBERS","LINK", "PROFLINK","Shape_Leng","Shape_Area","geometry")

tevibus<-st_read(paste0(in.root,"tevi/TeviEnterprises_130219.shp"))
tevibus<-st_read(paste0(in.root,"tevi/tevi_enterprises/locations.shp"))

st_crs(tevibus)<-st_crs(27700)

#tevibus<- st_read(paste0("tevidata/Gazetteer261018/Gazetteer261018.shp"))
#sectorbus<- st_read(paste0("tevidata/Sectors261018/Sector_enterprises261018.shp"))
#csawinners<-st_read(paste(in.root,"CSA_winners/CSA_winners_shp.shp",sep=""))
#cta2018<- st_read(paste0(in.root,"CTA_2018/CTA_winners_shp.shp"))
#csatevi<- st_read(paste0(in.root,"CSA_Tevi_interractions/CSA_Tevi_interractions_shp.shp"))
#st_crs(sectorbus)<-st_crs(27700)
#st_crs(csawinners)<-st_crs(27700)
#st_crs(cta2018)<-st_crs(27700)
#st_crs(csatevi)<-st_crs(27700)

#  Merge tevibus and sector data 
#st_geometry(sectorbus) <- NULL
#tevibus<-st_sf(merge(as.data.frame(tevibus),sectorbus[,c("Enterprise","Sector")],by="Enterprise"))

#  Merge tevibus and csatevi data 
#st_geometry(csatevi) <- NULL
#tevibus<-st_sf(merge(as.data.frame(tevibus),csatevi[,c("Enterprise","CSA_status")],by="Enterprise",all.x=TRUE))

### Add presentation variables - icon and colour values
# Set tevi colour on basis of Status
teviColour <- function(tevibus) {
  sapply(tevibus$Status, function(Status) {
    if(Status == "In process") "lightgreen" else if(Status == "Initial engagement") "lightblue" else if(Status == "Complete") "darkgreen"
    
  })
}
tevibus$Colour<-teviColour(tevibus)


# Create lookup table for sector icons
sector.labels<- levels(tevibus$Sector)
# Set icon type from font awesome
sector.icons<-c( "leaf", # Agric
                 "home", #Architect
                 "car", # Automobile
                 "archive",# Beekeeping
                 "users", # Charity
                 "tint", # Cleaning
                 "globe", # Conservation 
                 "globe", # Conservation and tourism
                 "home", # Construction
                 "briefcase", # Consultancy
                 "paint-brush",  # creative
                 "tree", # Environmental
                 "shopping-basket", # Food & Drink
                 "leaf",  # Garden maint
                 "medkit", # Healthcare
                 "leaf",  # Hort
                 "desktop", # IT
                 "home", # Land & property
                 "shipping-fast",  # Logistics
                 "wrench", # Manufacturing
                  "industry" , # Minerals
                 "battery-half", # Renewable energy
                 "graduation-cap", # Research
                 "coffee", # Rest/Cafe
                 "shopping-cart", # Retail 
                 "android", # Technology
                 "child", # Tourism
                 "recycle", # Waste
                 "recycle" # Waste management
                 )

print(sector.icons)
length(sector.icons)==length(sector.labels)

sectoricon.lookup<-as.data.frame(cbind(sector.labels,sector.icons)     )
names(sectoricon.lookup)<-c("Sector","Icon")
# Add icon column to tevibus data
tevibus<-st_sf(merge(as.data.frame(tevibus),sectoricon.lookup,by="Sector",all.x=TRUE))

# Add Geogrpahical labels - ie CNA, parish etc

tevibus<-st_intersection(tevibus,cna.shp[,c("CNA","geometry")])
plot(tevibus$geometry)
#csawinners<-st_intersection(csawinners,parishes[,c("NAME","geometry")])
#cta2018<-st_intersection(cta2018,parishes[,c("NAME","geometry")])


# Save tevi data in data folder ready for use by Dashboard app
#tevibus.filename<-"D:/tevibus.shp"
tevibus.filename<-paste0(in.root,"tevibus.shp")

print(paste("Writing file: ",tevibus.filename))

st_write(tevibus,tevibus.filename,delete_layer = T)
#  tevibus<- st_read(paste0("data/tevibus.shp")); st_crs(tevibus)<-st_crs(27700)


