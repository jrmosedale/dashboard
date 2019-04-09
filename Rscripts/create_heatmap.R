library(leaflet.extras)
library(RColorBrewer)
library(viridis)
library(htmlwidgets)

display.brewer.all()

allbus<-read.csv(paste0(in.root,"Tevi/all_businesses_apr19.csv"))
head(allbus)

allbus<-allbus[-which(is.na(allbus$lon)|is.na(allbus$lat)),]

pal<-colorRampPalette(c("red","yellow")) (7)
pal<-c("blue","red","yellow")
pal<-colorNumeric(c("purple","red","yellow"))(6)
pal<-colorNumeric("plasma",domain=NULL,bins=7)

leaflet() %>% 
  setView(lng = -4.9, lat = 50.4, zoom = 10)  %>%
  addProviderTiles("OpenStreetMap.Mapnik")  %>% 
  #addMarkers(lng = allbus$lon, lat = allbus$lat)
  addHeatmap(lng = allbus$lon, lat = allbus$lat ,
             blur = 7, max = 0.8, radius = 3)





leaflet() %>% 
  setView(lng = -4.9, lat = 50.4, zoom = 10)  %>%
  addProviderTiles("OpenStreetMap.Mapnik")  %>% 
  addMarkers(data=allbus, 
           popup = allbus$comp_name, 
           clusterOptions = markerClusterOptions() )


businessmap<-leaflet() %>% 
  setView(lng = -4.9, lat = 50.4, zoom = 10)  %>%
  addProviderTiles("OpenStreetMap.Mapnik")  %>% 
  #addMarkers(lng = allbus$lon, lat = allbus$lat)
  addHeatmap(lng = allbus$lon, lat = allbus$lat ,
             blur = 7, max = 0.8, radius = 3, 
             group="Heat map")%>%
  addMarkers(data=allbus, 
           popup = allbus$comp_name, 
           clusterOptions = markerClusterOptions(),
           group="Cluster map")  %>%
  addLayersControl(overlayGroups = c("Heat map","Cluster map"),options = layersControlOptions(collapsed=FALSE))

saveWidget(businessmap, file=paste0(dir_onedrive,"Rprojects/Dashboard/maps/tevi/business_heatmap.html"))
