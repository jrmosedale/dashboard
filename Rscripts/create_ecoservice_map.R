
dir_data<-paste0(dir_onedrive,'Rprojects/Dashboard/data/')
### Ecosystem Services
cornwall<-st_read("data/Cornwall.shp")

# Load service rasters and determine palette for each
# Carbon stock
carbonstock.r<-zero_to_NA(raster(paste0(dir_data,"ecoservices/ecoservice_cstock_0-100_100m.tif")))
crs(carbonstock.r)<- CRS('+init=EPSG:27700')
carbonstock.vals<-getValues(carbonstock.r)

cornwall.r<-fasterize(cornwall,carbonstock.r)
# Carbon sequestration
carbonseq.r<-mask(raster(paste0(dir_data,"ecoservices/ecoservice_cseq_0-100_100m.tif")),cornwall)
crs(carbonseq.r)<- CRS('+init=EPSG:27700')
carbonseq.vals<-getValues(carbonseq.r)

# Pollination
pollination.r<-raster(paste0(dir_data,"ecoservices/ecoservice_pollination.tif"))
crs(pollination.r)<- CRS('+init=EPSG:27700')
pollination.r<-crop(pollination.r,carbonstock.r)
pollination.vals<-getValues(pollination.r)

# Flood mitigation
floodmitig.r<-mask(raster(paste0(dir_data,"ecoservices/ecoservice_floodrisk_average_100m.tif")),cornwall)
crs(floodmitig.r)<- CRS('+init=EPSG:27700')
floodmitig.r<-crop(floodmitig.r,carbonstock.r)
floodmitig.vals<-getValues(floodmitig.r)

# Drinking water quality
drinkqual.r<-mask(raster(paste0(dir_data,"ecoservices/ecoservice_mitigdrink_100m.tif")),cornwall)
crs(drinkqual.r)<- CRS('+init=EPSG:27700')
drinkqual.r<-crop(drinkqual.r,carbonstock.r)
drinkqual.vals<-getValues(drinkqual.r)

# Aquaculture water quality
aquaqual.r<-mask(raster(paste0(dir_data,"ecoservices/ecoservice_mitigaqua_100m.tif")),cornwall)
crs(aquaqual.r)<- CRS('+init=EPSG:27700')
aquaqual.r<-crop(aquaqual.r,carbonstock.r)
aquaqual.vals<-getValues(aquaqual.r)

# Bathing water quality
bathqual.r<-mask(raster(paste0(dir_data,"ecoservices/ecoservice_mitigbeach_100m.tif")),cornwall)
crs(bathqual.r)<- CRS('+init=EPSG:27700')
bathqual.r<-crop(bathqual.r,carbonstock.r)
bathqual.vals<-getValues(bathqual.r)

# Not good catchments water quality
catchqual.r<-mask(raster(paste0(dir_data,"ecoservices/ecoservice_mitigngcatch_100m.tif")),cornwall)
crs(catchqual.r)<- CRS('+init=EPSG:27700')
catchqual.r<-crop(catchqual.r,carbonstock.r)
catchqual.vals<-getValues(catchqual.r)

# Aesthetics
aesthetics.r<-raster(paste0(dir_data,"ecoservices/ecoservice_aesthetics.tif"))
crs(aesthetics.r)<- CRS('+init=EPSG:27700')
aesthetics.r<-crop(aesthetics.r,carbonstock.r)
aesthetics.vals<-getValues(aesthetics.r)

# Soil loss mitigation
soilmitig.r<-raster(paste0(dir_data,"ecoservices/ecoservice_soillossmitig.tif"))
crs(soilmitig.r)<- CRS('+init=EPSG:27700')
soilmitig.r<-mask(crop(soilmitig.r,carbonstock.r),cornwall.r)
soilmitig.vals<-getValues(soilmitig.r)

servicepal<-colorBin("YlOrRd", domain = c(0,100),  na.color = "transparent")
servicepal<-colorBin("Greens", domain = c(0,100),  na.color = "transparent")


# Trim to same size stack and reproject
service.stack<-stack(carbonstock.r,carbonseq.r,pollination.r,soilmitig.r,floodmitig.r,
                     drinkqual.r,aquaqual.r,bathqual.r,aesthetics.r)
service.stack<-projectRasterForLeaflet(service.stack,method='ngb')
names(service.stack)<-c('carbonstock','carbonseq','pollination','soilmitig','floodmitig',
                        'drinkqual','aquaqual','bathqual','aesthetics')
  
ecomap <- leaflet() %>%
  setView(lng = -4.9, lat = 50.4, zoom = 10)  %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addScaleBar() %>%
  addRasterImage(service.stack[['carbonstock']], colors=servicepal, project=FALSE,
                 opacity = 0.8, group="Carbon stock") %>%
  addRasterImage(service.stack[['carbonseq']], colors=servicepal, project=F,
                 opacity = 0.8, group="Carbon sequestration") %>%
  addRasterImage(service.stack[['pollination']], colors=servicepal, project=F,
                 opacity = 0.8, group="Pollination") %>%
  addRasterImage(service.stack[['soilmitig']], colors=servicepal, project=F,
                 opacity = 0.8, group="Soil loss mitigation") %>%
  addRasterImage(service.stack[['floodmitig']], colors=servicepal, project=F,
                 opacity = 0.8, group="Flood mitigation") %>%
  addRasterImage(service.stack[['drinkqual']], colors=servicepal, project=F,
                 opacity = 0.8, group="Drinking water pollution mitigation") %>%
  addRasterImage(service.stack[['aquaqual']], colors=servicepal, project=F,
                 opacity = 0.8, group="Aquaculture pollution mitigation") %>%
  addRasterImage(service.stack[['bathqual']], colors=servicepal, project=F,
                 opacity = 0.8, group="Bathing water pollution mitigation") %>%
  addRasterImage(service.stack[['aesthetics']], colors=servicepal, project=F,
                 opacity = 0.8, group="Photogenic popularity") %>%
  addLegend(title='Relative service value', position = "bottomright",
            pal=servicepal, values=0:100,opacity=1) %>%
  addLayersControl(
  baseGroups =c("Carbon stock","Carbon sequestration","Pollination","Soil loss mitigation",
                "Flood mitigation","Drinking water pollution mitigation","Aquaculture pollution mitigation","Bathing water pollution mitigation",
                "Photogenic popularity"),
  options = layersControlOptions(collapsed=T)  )  %>%
  hideGroup(c("Carbon sequestration","Pollination","Soil loss mitigation",
            "Flood mitigation","Drinking water pollution mitigation","Aquaculture pollution mitigation","Bathing water pollution", "Photogenic popularity"))

saveWidget(ecomap, file=paste0(dir_onedrive,"Rprojects/Dashboard/maps/ecoservices/ecoservice_map.html") )

