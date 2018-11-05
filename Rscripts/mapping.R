#################################################################################### 
### Map 1 - Tevi businesses
#################################################################################### 

# Marker appearance functions etc
# Set colour on basis of Status
teviColor <- function(tevibus) {
  sapply(tevibus$Status, function(Status) {
    if(Status == "In process") "orange" else if(Status == "Initial engagement") "green"
    })
}
# Crate custom marker 
markericons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = teviColor(tevibus)
)
# Create popup message
popup<-paste(tevibus$Enterprise,"; Sector: ",tevibus$Sector,"; Status: ",tevibus$Status)

leaflet(tevibus) %>% 
  setView(lng = -4.9, lat = 50.4, zoom = 10)  %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addScaleBar() %>%
  addAwesomeMarkers(icon=markericons, 
                    label = ~as.character(Enterprise),
                    popup = popup
                    )

#################################################################################### 
### Map 2 - CSA and CTA winners
#################################################################################### 

# Crate custom marker 
csa.icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "green"
)

cta.icons<- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "blue"
)

# Create popup message
csa.popup<-paste("<b>",csawinners$Business,"</b>","-",csawinners$Award,csawinners$Year)
cta.popup<-paste("<b>",cta2018$Enterprise,"</b>","-",cta2018$Award,cta2018$Year)

leaflet() %>% 
  setView(lng = -4.9, lat = 50.4, zoom = 10)  %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addScaleBar() %>%
  addAwesomeMarkers(data=csawinners,
                    icon=csa.icons, 
                    label = ~as.character(Business),
                    popup = csa.popup,
                    group= "CSA businesses")  %>%
  addAwesomeMarkers(data=cta2018,icon=cta.icons, 
                    label = ~as.character(Enterprise),
                    popup = cta.popup,
                    group ="CTA businesses" )  %>%
  addLayersControl(
                    overlayGroups =c("CSA businesses", "CTA businesses"),
                    options = layersControlOptions(collapsed=FALSE)  
  )  %>%
  hideGroup(c("CTA businesses"))


#################################################################################### 
### Map 3 CUSTOM ICONS
#################################################################################### 
## Consider custom icon by sector
# Glyphicon
# sector.icons<-c( "glyphicon-scale", "glyphicon-wrench","glyphicon-glass","glyphicon-globe", "glyphicon-oil","glyphicon-sunglasses", "glyphicon-magnet",
                  "glyphicon-globe","glyphicon-leaf", "glyphicon-cutlery","glyphicon-heart","glyphicon-tint","glyphicon-retweet","glyphicon-user","glyphicon-home", "glyphicon-pencil",
                  "glyphicon-flash","glyphicon-qrcode","glyphicon-education", "glyphicon-glass","glyphicon-tree-deciduous","glyphicon-shopping-cart", "glyphicon-hourglass" )
# ion
#sector.icons<-c( "rocket", "build","nutrition","globe", "archive","happy", "magnet", 
                "globe","leaf", "restaurant","medkit","color-fill","sync","contacts","home","brush", 
                "flash", "desktop","flask", "nutrition","flower","cart", "clock" ) 
# font awesome
sector.icons<-c( "android", "wrench","shopping-basket","globe", "archive","child", "magnet",
                 "globe","leaf", "coffee","medkit","tint","recycle","users","home","paint-brush",
                 "battery-half", "desktop","graduation-cap", "shopping-basket","tree","shopping-cart", "shipping-fast"  )

# Create lookup table for sector icons
sector.labels<- unique(as.character(tevibus$Sector))
sectoricon.lookup<-as.data.frame(cbind(sector.labels,sector.icons)     )
names(sectoricon.lookup)<-c("Sector","Icon")

# Add icon column to tevibus data
tevibus<-st_sf(merge(as.data.frame(tevibus),sectoricon.lookup,by="Sector",all.x=TRUE))

# Crate custom marker for data
sectoricons <- awesomeIcons(
  icon = tevibus$Icon,
  iconColor = 'black',
 library = 'fa',
  markerColor = tevibus$Colour
)

# Create popup message
popup<-paste("<b>",tevibus$Enterprise,"</b>","<br>","Sector: ",tevibus$Sector,"<br>","Tevi Status: ",tevibus$Status,"<br>","CSA Status:",tevibus$CSA_status) 

leaflet(prepare_sf_for_leaflet(tevibus)) %>% 
  setView(lng = -4.9, lat = 50.4, zoom = 10)  %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addScaleBar() %>%
  addAwesomeMarkers(icon=sectoricons, 
                    label = ~as.character(Enterprise),
                    popup = popup
)

#################################################################################### 
### Map 4 Business & Environmental info
#################################################################################### 
phabitat.plot<-prepare_sf_for_leaflet(phabitat)
protected.plot<-prepare_sf_for_leaflet(protected)

leaflet() %>% 
  setView(lng = -4.9, lat = 50.4, zoom = 10)  %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addScaleBar() %>%
  addAwesomeMarkers(data=tevibus,
                    icon=sectoricons, 
                    label = ~as.character(Enterprise),
                    popup = popup
  ) %>%
  
leaflet() %>% 
  setView(lng = -4.9, lat = 50.4, zoom = 10)  %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addScaleBar() %>%
  addPolygons(data=protected.plot, 
              color=~protectpal(Type), weight = 1, fillOpacity=0.6,
              group="Protected_Areas",
              label = paste(protected.plot$Type,protected.plot$Name)
              ) %>%
  addPolygons(data=phabitat.plot,
              color = ~phpal(Main_habit), fillOpacity=0.8, weight = 1, 
              group="Priority_habitat",
              label = paste(phabitat.plot$Main_habit) 
              ) %>%
  addLayersControl(
              overlayGroups =c("Protected_Areas", "Priority_habitat"),
              options = layersControlOptions(collapsed=FALSE)  
              )  %>%
  hideGroup(c("Priority_habitat"))



#################################################################################### 
### Map 5 Parish info
#################################################################################### 

sectoricons <- awesomeIcons(
  icon = tevibus$Icon,
  iconColor = 'black',
  library = 'fa',
  markerColor = tevibus$Colour
)

# Create popup message
popup<-paste("<b>",tevibus$Enterprise,"</b>","<br>","Sector: ",tevibus$Sector,"<br>","Tevi Status: ",tevibus$Status,"<br>","CSA Status:",tevibus$CSA_status) 

leaflet(prepare_sf_for_leaflet(tevibus[which(tevibus$NAME==parish$NAME),])) %>% 
  setView(lng = -4.9, lat = 50.4, zoom = 10)  %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addScaleBar() %>%
  addAwesomeMarkers(icon=sectoricons, 
                    label = ~as.character(Enterprise),
                    popup = popup
  )







              %>% 
                lapply(htmltools::HTML),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",direction = "auto") ) 
  

 
              
              %>% 
                lapply(htmltools::HTML),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",direction = "auto"))
%>% 
  
  # Agricultural Class
  addPolygons(data=prepare_sf_for_leaflet(habitatmap.data()[["Agri_Class"]]), 
              color=~agripal(rID), weight = 1, fillOpacity=0.5, group="Agri_Class",
              #highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
              label = sprintf("<strong>%s</strong>",
                              habitatmap.data()[["Agri_Class"]]$ALC_GRADE) %>% 
                lapply(htmltools::HTML),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",direction = "auto"))  %>% 
  
  # Protected Areas
  #addPolygons(data=prepare_sf_for_leaflet(habitatmap.data()[["AONB"]]), 
  #            color="green", weight = 1, fillOpacity=1,group="Protected_Areas") %>% 
  
 
  
  
################
# Test raster colour scale (eg for ecosystem services)
###################
r<-raster(paste0(dir_hydrology,"RUSLE_current.tif"))
r2<-raster(paste0(dir_zinputs,"ecoservice_pollination.tif"))
r3<-raster(paste0(dir_zinputs,"ecoservice_drinkingwater1.tif"))


#crs(r)<-
# Set colour scheme according to variable mapped
# mnmx = 2 element array holding min and max
mapcolour <- function(var,r) {
  if (class(r)[1]=="RasterLayer") r<-getValues(r)
  q<-quantile(r,c(0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1),names=FALSE,na.rm=TRUE)
  mnmx<-c(min(q),max(q)) # or could be added as parameter
  switch(var,
         "abovecarbon" = colorBin("YlOrRd",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE,na.color=TRUE) ,
         "floodmitig"=colorBin("PuBuGn",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE,na.color=NA),
         "aquamitig"=colorBin("PRGn",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE,na.color=NA),
         "drinkmitig"=colorBin("BuGn",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE,na.color=NA),
         "soilmitig" = colorBin("YlOrRd",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE,na.color=NA),
         "pollination" = colorBin(c("blue","green"),domain=round(mnmx),bins=unique(round(q)),pretty=TRUE,na.color=NA),
        # "t20_gsdays"= colorBin(c("white","red"),domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
        # "t25_gsdays"= colorBin(c("white","red"),domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
        # "t30_gsdays"= colorBin(c("white","red"),domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
        # "lastspfr_doy"=colorBin("Blues",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE) ,
        # "firstautfr_doy"=colorBin("Blues",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
        # "frostfree_days"=colorBin("BuGn",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
        # "fl_tmean"=colorBin("YlOrRd",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
        # "fl_numday"=colorBin("Greens",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
        # "elevation"=colorBin(c("blue","green"),domain=c(round((floor(mnmx[1])/10)*10),round((ceiling(mnmx[2])/10)*10)),bins=unique(round(ceiling(q)/10)*10),pretty=TRUE),
        # "slope"=colorBin("Purples",domain=round(mnmx),bins=c(0,1,2,4,8,12,16,20,24,28,mnmx[2]),pretty=TRUE),
        # "gdd10_gs" = colorBin("YlOrRd",domain=c(round((floor(mnmx[1])/100)*100),round((ceiling(mnmx[2])/100)*100)),bins=unique(round(ceiling(q)/100)*100),pretty=TRUE),
        # "gdd5_gs" = colorBin("YlOrRd",domain=c(round((floor(mnmx[1])/100)*100),round((ceiling(mnmx[2])/100)*100)),bins=unique(round(ceiling(q)/100)*100),pretty=TRUE) ,
         "aspect"=colorBin("PRGn",domain=c(round((floor(mnmx[1])/10)*10),round((ceiling(mnmx[2])/10)*10)),bins=unique(round(ceiling(q)/10)*10),pretty=TRUE) 
  )
} # mapcolour


v<-getValues(r)
palette<-mapcolour("soil_erosion",v)
v2<-getValues(r2)
palette2<-mapcolour("pollination",v2)

leaflet() %>% 
  setView(lng = -4.9, lat = 50.4, zoom = 10)  %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addScaleBar() %>%
  addRasterImage(r, colors = palette,
                 opacity = 0.7,project=TRUE,group="Soil_erosion") %>%
  addRasterImage(r2, colors = palette2,
                 opacity = 0.7,project=TRUE,group="Pollination") %>%
  addLegend(position = "bottomright",
            pal=palette,values=v,
            group="Soil_erosion",
            opacity=1) %>%
  addLegend(position = "bottomright",
            pal=palette2,values=v2,
            group="Pollination",
            opacity=1) %>%
  addLayersControl(
   overlayGroups =c("Soil_erosion","Pollination"),
    options = layersControlOptions(collapsed=FALSE)  ) %>%
  hideGroup("Pollination")

  

  








    addRasterImage(arable.r,color="brown", 
                 opacity = 0.7,project=TRUE,group="Arable") %>%
  addRasterImage(improvgrass.r,color="green", 
                 opacity = 0.7,project=TRUE,group="Improved_Grass") %>%
  addRasterImage(forest.r,color="dark green", 
                 opacity = 0.7,project=TRUE,group="Priority_habitats") %>%
  addRasterImage(heath.r,color="purple", 
                 opacity = 0.7,project=TRUE,group="Priority_habitats") %>%
  addRasterImage(protected.r,color="yellow", 
                 opacity = 0.7,project=TRUE,group="Protected_areas") %>%
  addLayersControl(
    overlayGroups =c("Urban", "Arable","Improved_Grass","Priority_habitats","Protected_areas"),
    options = layersControlOptions(collapsed=FALSE)  ) %>%
  hideGroup(c("Arable","Improved_Grass","Priority_habitats","Protected_areas"))


saveWidget(map1, file=paste(dir_maps,"map1.html"))