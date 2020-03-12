
dir_data<-paste0(dir_onedrive,'Rprojects/Dashboard/data/')

lightemission.r<-mask(raster(paste0(dir_data,"ecoservices/light_emissions.tif")),cornwall)
crs(lightemission.r)<- CRS('+init=EPSG:27700')
lightem.vals<-getValues(lightemission.r)

lightskyglow.r<-mask(raster(paste0(dir_data,"ecoservices/light_skyglow.tif")),cornwall)
crs(lightskyglow.r)<- CRS('+init=EPSG:27700')
lightsky.vals<-getValues(lightskyglow.r)


# Aboslute values
lightemission_3857.r<-projectRaster(lightemission.r, crs=CRS('+init=EPSG:3857') ,method='ngb')
lightskyglow_3857.r<-projectRaster(lightskyglow.r, crs=CRS('+init=EPSG:3857') ,method='ngb')
sgpal<-colorBin("Spectral", reverse=TRUE,domain = c(0:1.5),bins=c(0,0.05,0.1,0.2,0.4,0.5,0.6,0.7,0.8,0.9,1,1.5) ,na.color = "transparent")
empal<-colorBin("Spectral", reverse=TRUE,domain = c(0:1.46),bins=c(0,0.25,0.5,1,1.2,1.4,1.3,1.44,1.45) ,na.color = "transparent")

lightmap<-leaflet() %>%
  setView(lng = -4.9, lat = 50.4, zoom = 10)  %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addScaleBar() %>%
  addRasterImage(lightemission_3857.r, colors=empal,
                 opacity = 0.7,project=FALSE, group="Emissions") %>%
  addRasterImage(lightskyglow_3857.r, colors=sgpal,
                 opacity = 0.7,project=FALSE, group="Skyglow") %>%
  addLayersControl(
    overlayGroups =c( "Emissions", "Skyglow"),
    options = layersControlOptions(collapsed=FALSE)  )  %>%
  addLegend(title='Light emissions', position = "bottomright",
            pal=empal, values=0:1.5,opacity=1, group='Emissions') %>%
  addLegend(title='Skyglow', position = "bottomright",
            pal=sgpal, values=0:1.5,opacity=1, group='Skyglow') %>%
  hideGroup(c("Skyglow"))

saveWidget(lightmap, file=paste0(dir_onedrive,"Rprojects/Dashboard/maps/light/light_map.html"))

# Relative values
# Scale
lightemission_scale.r<-projectRaster(scaleraster(lightemission.r,0,1)*100, crs=CRS('+init=EPSG:3857') ,method='ngb')
lightskyglow_scale.r<-projectRaster(scaleraster(lightskyglow.r,0,1)*100, crs=CRS('+init=EPSG:3857') ,method='ngb')
plot(lightemission_scale.r)

#sgpal<-colorBin("Spectral", reverse=TRUE,domain = c(0:100),bins=c(0,20,30,40,50,60,70,80,90,95,100) ,na.color = "transparent")
empal<-colorBin("Spectral", reverse=TRUE,domain = c(0:100),bins=c(0,20,40,60,70,80,90,95,99,100) ,na.color = "transparent")

#empal<-colorBin("Spectral", reverse=TRUE,domain = c(0:100),bins=c(0,20,40,50,60,70,80,90,95,99,100) ,na.color = "transparent")
sgpal<-colorBin("Spectral", reverse=TRUE,domain = c(0:100),bins=c(0,5,10,20,40,60,70,80,90,95,100) ,na.color = "transparent")

lightmap<-leaflet() %>%
  setView(lng = -4.9, lat = 50.4, zoom = 10)  %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addScaleBar() %>%
  addRasterImage(lightemission_scale.r, colors=empal,
                 opacity = 0.7,project=FALSE, group="Emissions") %>%
  addRasterImage(lightskyglow_scale.r, colors=sgpal,
                 opacity = 0.7,project=FALSE, group="Skyglow") %>%

  addLayersControl(
    overlayGroups =c(   "Emissions", "Skyglow"),
    options = layersControlOptions(collapsed=FALSE)  )  %>%

  addLegend(title='Relative service value', position = "bottomright",
            pal=servicepal, values=0:100,opacity=1)

  hideGroup(c("Skyglow"))

saveWidget(lightmap, file=paste0(dir_onedrive,"Rprojects/Dashboard/maps/light/light_map.html"))


# Orig values
sgpal<-colorBin("Spectral", reverse=TRUE,domain = c(0:1.5),bins=c(0,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.5) ,na.color = "transparent")
empal<-colorBin("Spectral", reverse=TRUE,domain = c(0:1.46),bins=c(0,0.25,0.5,1,1.2,1.4,1.3,1.44,1.45) ,na.color = "transparent")

leaflet() %>%
  setView(lng = -4.9, lat = 50.4, zoom = 10)  %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addScaleBar() %>%
  addRasterImage(lightemission.r, colors=empal,
                 opacity = 0.7,project=TRUE, group="Emissions") %>%
  addRasterImage(lightskyglow.r, colors=sgpal,
                 opacity = 0.7,project=TRUE, group="Skyglow") %>%

  addLayersControl(
    baseGroups =c(   "Emissions", "Skyglow"),
    options = layersControlOptions(collapsed=FALSE)  )  %>%

  hideGroup(c("Skyglow"))
