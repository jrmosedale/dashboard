# Create Community Network Area maps
library(htmlwidgets)
library(RColorBrewer)
library(rgl)
library(rayshader)
# Load CNA shapefile data
cna.shp<-st_read(paste0(in.root,"Boundaries/Community_Network_Areas/community_network_areas.shp"))
st_crs(cna.shp)<-st_crs(27700)
#cna.shp<-st_transform(cna.shp,4326)
cna.list<-paste0(cna.shp$NAME)
names(cna.list)<-cna.list

dir_out<-paste0(dir_onedrive,"Rprojects/Dashboard/maps/threed/")
dir_out<-paste0(in.root,"GIFS/")

#############################################
# Load inputs for whole of Cornwall
#############################################
# dem
demsw<-raster(paste0(dir_dem100m,"DTM_OS_southwest_100m.tif"))
crs(demsw)<-CRS('+init=EPSG:27700')
cornwall.r<-NA_to_val(crop(demsw,template.r),0)
sea.r<-zero_to_NA(calc(cornwall.r<0.5,fun=function(x){ifelse(is.na(x),1,x)}))
plot(sea.r)
# Landcover
landcover.r<-raster(paste0(dir_landcover,"landcover_simple_100m_res.tif"))
lc.lup<-cbind(c(110,120,200,300,400,500,530,610,620,710,720, 800, 910, 920, 930),
              c(1,  1,  2,  3,   5, 2,   2, 3,   3,  3, 3,   0,   0,   0,   4))
lc.col<-c("transparent","green","yellow","purple","black","orange")
lc.r<-reclassify(landcover.r,lc.lup)
plot(lc.r,col=lc.col)

# Water
rivers.r<-raster(paste0(dir_zinputs,"Zonation_pmap100m_data/landfeature_rivers.tif"))
lcwater.r<-zero_to_NA(landcover.r==800)
water.r<-overlay(sea.r,rivers.r,lcwater.r,fun=function(x,y,z){ifelse(is.na(x)&is.na(y)&is.na(z),NA,1)})
plot(water.r)



for (n in cna.list){
for (n in cna.list[c(9)]){
    
  # Crop inputs to CNA 
  mask.shp<-cna.shp[which(cna.shp$NAME==n),]
  area.dem<-crop(cornwall.r,extent_from_sf(mask.shp))
  #area.dem<-mask(area.dem,mask.shp)
  #plot(area.dem)
  area.water<-crop(water.r,extent_from_sf(mask.shp))
  #area.water<-mask(area.water,mask.shp)
  #plot(area.water,add=T,col="blue")
  area.lc<-crop(lc.r,extent_from_sf(mask.shp))
  
  # Prepare data for map
  elev = matrix( raster::extract(area.dem, raster::extent(area.dem), buffer = 1000), nrow = ncol(area.dem), ncol = nrow(area.dem) )
  water = matrix( raster::extract(area.water, raster::extent(area.water), buffer = 1000), nrow = ncol(area.water), ncol = nrow(area.water) )
  water<-water[,ncol(water):1] # WHY ???
  
  
  # write png of landcover
  lc.pngname<-paste0(dir_out,"lcover_colour.png")
  png(filename=lc.pngname, width=ncol(area.lc), height=nrow(area.lc), units = "px", pointsize = 1)
  par(mar = c(0,0,0,0), xaxs = "i", yaxs = "i") #Parameters to create a borderless image
  lc.rgb<-raster::image(
    area.lc,
    col =lc.col,
    maxpixels = raster::ncell(area.lc),
    axes = FALSE
  )
  dev.off()
  
  # Create map plot
  lc_image<-png::readPNG(lc.pngname)
  raymat = ray_shade(elev)
  ambmat = ambient_shade(elev)
  
  rgl::rgl.clear()
  elev %>% 
    sphere_shade(texture = "imhof1") %>% 
    add_overlay(lc_image, alphalayer = 0.7) %>%
    add_water(water, color = "blue") %>%
    add_shadow(raymat, max_darken = 0.5) %>%
    add_shadow(ambmat, max_darken = 0.5) %>%
    plot_3d(elev,zscale=z,zoom=0.5, windowsize=c(2000,1600),
            theta=-45 , phi=25)
  
  # Create filename for map
  mapname<-gsub(" ","_",n)
  mapname<-gsub( "[[:punct:]]", "", mapname) 
  #mapname<-paste0(dir_cna,"map",mapname,".html")
  mapname<-paste0(dir_out,"map3d_",mapname,"_v2.html")
  print(mapname)
  
  # Save map as widget
  rglwidget() %>% saveWidget(file=mapname)
  
}

