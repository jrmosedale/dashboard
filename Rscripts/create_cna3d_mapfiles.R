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

z<-50

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
    add_overlay(lc_image, alphalayer = 0.8) %>%
    add_water(water, color = "blue") %>%
    add_shadow(raymat, max_darken = 0.4) %>%
    add_shadow(ambmat, max_darken = 0.4) %>%
    plot_3d(elev,zscale=z,zoom=0.5, windowsize=c(3500,2200),
            theta=-45 , phi=25)
  
  # Create filename for map
  mapname<-gsub(" ","_",n)
  mapname<-gsub( "[[:punct:]]", "", mapname) 
  mapname<-paste0(dir_onedrive,"Rprojects/Dashboard/maps/threed/map3d_",mapname,".html")
  #mapname<-paste0(dir_out,"map3d_",mapname,"_v3.html")
  print(mapname)
  
  # Save map as widget
  rglwidget() %>% saveWidget(file=mapname)
  
}

  # HTML widget sizeing
  # sizingPolicy = htmlwidgets::sizingPolicy()
  # sizingPolicy(padding = 0, knitr.figure=FALSE, browser.padding = , browser.fill = TRUE)

#####################
# Opportunity mapping with current landcover and opps
# ISSUES: 
# Design Legend
# Layer controls - could be done in shiny and re-plotting rayshade maps?
#####################
  
# Merge opportunity raster with simplified current landcover
# Landcover - reclassify woods, wetland & coastal, grass-heath
landcover.r<-raster(paste0(dir_landcover,"landcover_simple_100m_res.tif"))
lc.lup<-cbind(c(110,120,200,300,400,500,530,610,620,710,720, 800, 910, 920, 930),
              c(1,  1,  2,  3,   5, 2,   2, 3,   3,  3, 3,   0,   0,   0,   4))
lc.col<-c("transparent","green","yellow","purple","black","yellow")
lc.r<-reclassify(landcover.r,lc.lup)
plot(lc.r,col=lc.col)

opp.r<-zero_to_NA(raster(paste0(in.root,"Zonation_opptype/opptype_wd19_wt12_gr05_v2.tif")))
opp.lup<-cbind(c(1,2,3), c(6,7,8))
opp.r<-reclassify(opp.r,opp.lup)
opp.col<-c("red","cyan","orange")
plot(opp.r,col=opp.col)

lcopp.r<-zero_to_NA(overlay(NA_to_val(lc.r,0),NA_to_val(opp.r,0),fun=function(x,y){x+y}))
lcopp.col<-c("green","yellow","purple","black","yellow","red","cyan","orange")
plot(lcopp.r,col=lcopp.col)

########
# Produce 3D overlay of existing landcover and opportunities
# ISSUE: add Legend??
########
# Crop inputs to CNA 
n<-cna.list[c(9)]

mask.shp<-cna.shp[which(cna.shp$NAME==n),]
area.dem<-crop(cornwall.r,extent_from_sf(mask.shp))
area.water<-crop(water.r,extent_from_sf(mask.shp))
area.lc<-crop(lcopp.r,extent_from_sf(mask.shp))

# Prepare data for map
elev = matrix( raster::extract(area.dem, raster::extent(area.dem), buffer = 1000), nrow = ncol(area.dem), ncol = nrow(area.dem) )
water = matrix( raster::extract(area.water, raster::extent(area.water), buffer = 1000), nrow = ncol(area.water), ncol = nrow(area.water) )
water<-water[,ncol(water):1] # WHY ???


# write png of landcover
lcopp.pngname<-paste0(dir_out,"lcopp_colour.png")
png(filename=lcopp.pngname, width=ncol(area.lc), height=nrow(area.lc), units = "px", pointsize = 1)
par(mar = c(0,0,0,0), xaxs = "i", yaxs = "i") #Parameters to create a borderless image
lc.rgb<-raster::image(
  area.lc,
  col =lcopp.col,
  maxpixels = raster::ncell(area.lc),
  axes = FALSE
)
dev.off()


# Create map plot
lcopp_image<-png::readPNG(lcopp.pngname)
raymat = ray_shade(elev)
ambmat = ambient_shade(elev)

rgl::rgl.clear()
elev %>% 
  sphere_shade(texture = "imhof1") %>% 
  add_overlay(lcopp_image, alphalayer = 0.8) %>%
  add_water(water, color = "blue") %>%
  add_shadow(raymat, max_darken = 0.4) %>%
  add_shadow(ambmat, max_darken = 0.4) %>%
  plot_3d(elev,zscale=z,zoom=0.5, windowsize=c(3500,2200),
          theta=-45 , phi=25)

  
  
  
  
  
  
  #####################
  # Use toggle widget to control landcover layers???
  # https://mran.microsoft.com/snapshot/2017-12-24/web/packages/rgl/vignettes/WebGL.html 
  ####################
  
elev %>% 
    sphere_shade(texture = "imhof1") %>% 
    add_water(water, color = "blue") %>%
    add_shadow(raymat, max_darken = 0.4) %>%
    add_shadow(ambmat, max_darken = 0.4) %>%
    plot_3d(elev,zscale=z,zoom=0.5, windowsize=c(3500,2200),
            theta=-45 , phi=25) 
p1<-rglwidget(elementId = "p1")
rgl.attrib.info(p1)

rgl::rgl.clear()
  
elev %>% 
    sphere_shade(texture = "imhof1") %>% 
    add_overlay(lc_image, alphalayer = 0.8) %>%
    add_water(water, color = "blue") %>%
    add_shadow(raymat, max_darken = 0.4) %>%
    add_shadow(ambmat, max_darken = 0.4) %>%
    plot_3d(elev,zscale=z,zoom=0.5, windowsize=c(3500,2200),
            theta=-45 , phi=25)

p2<-rglwidget(elementId = "p2")

  
  rglwidget() %>%
    toggleWidget(ids = p1) %>%
    toggleWidget(ids = p2)
  
  
  rglwidget() %>%  toggleWidget(ids = p1["x"], label = "Data")
  
  toggleWidget(sceneId = "p1", ids = p1["x"], label = "Data")
  