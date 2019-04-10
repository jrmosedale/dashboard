# Create Community Network Area maps

# Load CNA shapefile data
cna.shp<-st_read(paste0(in.root,"Boundaries/Community_Network_Areas/community_network_areas.shp"))
st_crs(cna.shp)<-st_crs(27700)
cna.shp<-st_transform(cna.shp,4326)
cna.list<-paste0(cna.shp$NAME)
names(cna.list)<-cna.list


for (n in cna.list){
  cna.map<-leaflet() %>% 
    setView(lng = -4.9, lat = 50.4, zoom = 10)  %>%
    addProviderTiles("OpenStreetMap.Mapnik") %>%
    addScaleBar() %>%
    addPolygons(data=cna.shp[which(cna.shp$NAME==n),], color="black",fill=FALSE)
  mapname<-gsub(" ","_",n)
  mapname<-gsub( "[[:punct:]]", "", mapname) 
  #mapname<-paste0(dir_cna,"map",mapname,".html")
  mapname<-paste0("map",mapname,".html")
  print(mapname)
  saveWidget(cna.map, file=mapname)
}


