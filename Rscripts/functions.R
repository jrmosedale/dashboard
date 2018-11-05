
################################################
# Global FUNCTIONS
###############################################


# Calculate cell number from lon lat values
lonlat_to_cellnumber<-function(r,x,y,zone=0){ #http://stackoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+init=epsg:4326")  ## lat lon
  xy <- spTransform(xy, CRS("+init=epsg:3857")) # convert to raster xy
  cell <- cellFromXY(r, c(xy$X,xy$Y))
  return(cell)
}

# Creates circular polygon buffer of defined distance around point
# Inputs and outputs = sf objects
create_point_buffer<-function(point,buffer_dist){
  buffered_point<-st_buffer(point,buffer_dist)
  return(buffered_point)
}

# Returns any shapes from polygon_set that overlaps with polygon but does not clip
# Inputs and outputs = sf objects
get_overlapping_polygons<-function(polygon,polygon_set){
  if(st_crs(polygon)!=st_crs(polygon_set)) warning("Polygon crs are not the same in get_overlapping_polygons")
  # Find polygons overlapping with buffer shape
  overlap_list<-which(st_intersects(polygon_set,polygon)==1)
  overlap_polygons<-polygon_set[overlap_list,]
  return(overlap_polygons)
}

# Returns clipped polygons from polygon_set that overlap with polygon
# Inputs and outputs = sf objects
get_clipped_polygons<-function(polygon,polygon_set){
  if(st_crs(polygon)!=st_crs(polygon_set)) warning("Polygon crs are not the same in get_clipped_polygons")
  # Find polygons overlapping with buffer shape
  clipped_polygons<-st_intersection(polygon_set,polygon)
  return(clipped_polygons)
}

# Prepares a sf object for projection in Leaflet
# Converts sfc object to sf dataframe
# Returns sf dataframe
prepare_sf_for_leaflet<-function(sf_object){
  if (class(sf_object)[2]!="data.frame") sf_object<-st_sf(geometry=sf_object)
  # Deal with empty sf object
  if (st_crs(sf_object)$epsg!=4326){
    if (length(sf_object$geometry)==0) st_crs(sf_object)<-4326 else sf_object<-st_transform(sf_object,4326)
  }
  sf_object<-st_zm(sf_object, drop = T, what = "ZM")
  st_crs(sf_object)<-st_crs(4326)
  return(sf_object)
}

# FUNCTION creates raster from sf object
# Input: sf object and raster template
# Resolution must be in appropriate units for the raster projection 
empty_raster_from_sf<-function(sf_object,res=20){
  if (st_crs(sf_object)$epsg==4326) sf_object<-st_transform(sf_object,27700)
  r.crs<-st_crs(sf_object)$proj4string
  bb<-st_bbox(sf_object)
  r.extent<-extent(c(bb[1],bb[3],bb[2],bb[4]))
  r_template<-raster(ext=r.extent,res=res,crs= r.crs , vals=NA)
  return(r_template)
}

fillraster_from_sf<-function(sf_object,r_template,r_value){
  if (st_crs(sf_object)$epsg==4326) sf_object<-st_transform(sf_object,27700)
  sp_object<-as(sf_object, "Spatial")
  new_r<-rasterize(sp_object, r_template, field=r_value, fun='last', background=NA)
  #plot(new_r)
  return(new_r)
}

# FUNCTION mapcolour
# Creates quantile palette of colours dependent on variable name
# Parameters
# var = Variable name
# r = rasterlayer or vector of values
mapcolour <- function(var,r) {
  if (class(r)[1]=="RasterLayer") r<-getValues(r)
  q<-quantile(r,c(0,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,1),names=FALSE,na.rm=TRUE)
  mnmx<-c(min(q),max(q)) # or could be added as parameter
  switch(var,
         "abovecarbon" =colorBin("YlOrRd",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE,na.color=NA) ,
         "floodmitig"=colorBin("PuBuGn",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE,na.color=NA),
         "aquaqual"=colorBin("BuGn",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE,na.color=NA),
         "bathqual"=colorBin("BuGn",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE,na.color=NA),
         "drinkqual"=colorBin("BuGn",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE,na.color=NA),
         "soilmitig"=colorBin("YlOrBr",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE,na.color=NA),
         "pollination" =colorBin(c("YlOrRd"),domain=round(mnmx),bins=unique(round(q)),pretty=TRUE,na.color=NA),
         # "t25_gsdays"= colorBin(c("white","red"),domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
         # "firstautfr_doy"=colorBin("Blues",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
         # "frostfree_days"=colorBin("BuGn",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
         # "fl_numday"=colorBin("Greens",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
         # "elevation"=colorBin(c("blue","green"),domain=c(round((floor(mnmx[1])/10)*10),round((ceiling(mnmx[2])/10)*10)),bins=unique(round(ceiling(q)/10)*10),pretty=TRUE),
         # "slope"=colorBin("Purples",domain=round(mnmx),bins=c(0,1,2,4,8,12,16,20,24,28,mnmx[2]),pretty=TRUE),
         # "gdd5_gs" = colorBin("YlOrRd",domain=c(round((floor(mnmx[1])/100)*100),round((ceiling(mnmx[2])/100)*100)),bins=unique(round(ceiling(q)/100)*100),pretty=TRUE) ,
         "aspect"=colorBin("PRGn",domain=c(round((floor(mnmx[1])/10)*10),round((ceiling(mnmx[2])/10)*10)),bins=unique(round(ceiling(q)/10)*10),pretty=TRUE) 
  )
} # mapcolour

