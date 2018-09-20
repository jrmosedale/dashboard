

########################################## 
### Prepare TEviHub RASTER data   ###
### write outputs to dir_maps
##########################################
in.root<-"data/"
dir_habitat<-paste0("../../SWEEP/Data/habitat_shapes/")
dir_maps<-paste0(in.root,"Zmap/")

landcover.r<-raster(paste0(dir_maps,"landcover_simple_100m_res.tif"))
intertidal.r<-raster(paste0(dir_maps,"mask_intertidal.tif"))
cornwallLT.r<-raster(paste0(dir_maps,"cornwall_LT.tif"))
cornwallHT.r<-mask(cornwallLT.r,intertidal.r,inverse=TRUE)

# Protected 
protected.r<-raster(paste0(dir_maps,"protected.tif"))
protected.r<-mask(protected.r,cornwallLT.r)
excluded.r<-raster(paste0(dir_maps,"mask_excluded.tif"))
writeRaster(protected.r,paste0(dir_maps,"protected.tif"),overwrite=TRUE)

# Habitats
woods.r<-raster::calc(landcover.r,fun=function(x){ifelse(x<190,1,NA)})
woods.r<-zero_to_NA(woods.r)

arable.r<-landcover.r==910
arable.r<-zero_to_NA(arable.r)

improvgrass.r<-landcover.r==920
improvgrass.r<-zero_to_NA(improvgrass.r)

urban.r<-landcover.r==930
urban.r<-zero_to_NA(urban.r)

heath.r<-landcover.r==400
heath.r<-zero_to_NA(heath.r)
priority.r<-zero_to_NA(raster(paste0(dir_maps,"habitat_priority.tif")))

writeRaster(woods.r,paste0(dir_maps,"woods.tif"))
writeRaster(arable.r,paste0(dir_maps,"arable.tif"))
writeRaster(improvgrass.r,paste0(dir_maps,"improvgrass.tif"))
writeRaster(urban.r,paste0(dir_maps,"urban.tif"))
writeRaster(heath.r,paste0(dir_maps,"heath.tif"))
writeRaster(priority.r,paste0(dir_maps,"priority.tif"))


# Zonation Outputs
nogo1.r<-raster(paste0(dir_maps,"priorityrank09.tif"))
opp1.r<-mask(raster(paste0(dir_maps,"opprank06.tif")),cornwallHT.r)
nogo1.r<-rank_after_mask(nogo1.r,protected.r)
opp1.r<-rank_after_mask(opp1.r,excluded.r)

# Top ranks
nogo1.rank10<-raster::calc(nogo1.r,fun=function(x){ifelse(x>0.9,1,NA)})
nogo1.rank20<-raster::calc(nogo1.r,fun=function(x){ifelse(x>0.8,1,NA)})
nogo1.rank20<-mask(nogo1.rank20,nogo1.rank10,inverse=TRUE)

opp1.rank10<-raster::calc(opp1.r,fun=function(x){ifelse(x>0.9,1,NA)})
opp1.rank20<-raster::calc(opp1.r,fun=function(x){ifelse(x>0.8,1,NA)})
opp1.rank20<-mask(opp1.rank20,opp1.rank10,inverse=TRUE)

writeRaster(nogo1.r,paste0(dir_maps,"nogo1.tif"))
writeRaster(opp1.r,paste0(dir_maps,"opp1.tif"))
writeRaster(nogo1.rank10,paste0(dir_maps,"nogo1rank10.tif"))
writeRaster(opp1.rank10,paste0(dir_maps,"opp1rank10.tif"))
writeRaster(nogo1.rank20,paste0(dir_maps,"nogo1rank20.tif"))
writeRaster(opp1.rank20,paste0(dir_maps,"opp1rank20.tif"))




### Prepare parish rasters
rank_after_mask<-function(r,msk){
  msk.cells<-getValues(mask(r,msk))
  min.prot<-min(msk.cells,na.rm=TRUE)
  # num.prot<-length(msk.cells[-which(is.na(msk.cells))])
  rem.cells<-getValues(mask(r,msk,inverse=TRUE))
  max.rem<-max(rem.cells,na.rm=TRUE)
  new.ranks<-(rem.cells/max.rem)*1
  new.r<-setValues(r,new.ranks)
  return(new.r)
}
extent_from_sf<-function(polygon){
  bb<-st_bbox(polygon)
  ext<-extent(bb[1],bb[3],bb[2],bb[4])
  return(ext)
}

# Protected 
protected.r<-raster(paste0(dir_maps,"protected.tif"))
excluded.r<-raster(paste0(dir_maps,"mask_excluded.tif"))

# Habitats
woods.r<-raster(paste0(dir_maps,"woods.tif"))
arable.r<-raster(paste0(dir_maps,"arable.tif"))
improvgrass.r<-raster(paste0(dir_maps,"improvgrass.tif"))
urban.r<-raster(paste0(dir_maps,"urban.tif"))
heath.r<-raster(paste0(dir_maps,"heath.tif"))
priority.r<-raster(paste0(dir_maps,"priority.tif"))

# Zonation Outputs
nogo1.r<-raster(paste0(dir_maps,"nogo1.tif"))
opp1.r<-raster(paste0(dir_maps,"opp1.tif"))
nogo1.rank10<-raster(paste0(dir_maps,"nogo1rank10.tif"))
opp1.rank10<-raster(paste0(dir_maps,"opp1rank10.tif"))
nogo1.rank20<-raster(paste0(dir_maps,"nogo1rank20.tif"))
opp1.rank20<-raster(paste0(dir_maps,"opp1rank20.tif"))


dir_parishrasters<-"data/parish_rasters/"

parishes<-st_read(paste0(in.root,"Parishes/cornwallparish.shp"))
st_crs(parishes)<-st_crs(27700)

parish.list<-unique(parishes$NAME)
# Create unique distinguishing names suitable for file names
parish.name<-substr(parish.list,1,15)
parish.name<-gsub(" ", "", parish.name)
parish.name<-gsub("[[:punct:]]", "_", parish.name)
length(parish.name)==length(unique(parish.name))

for (p in 1: length(parish.list)){
  print(p)
  parish<-parish.list[p]
  print(parish)
  parish.polygon<-parishes[which(parishes$NAME==parish),]
  e<-extent_from_sf(parish.polygon)
  parish.nogoranks<-rank_after_mask(nogo1.r,parish.polygon)
  parish.nogoranks10<-zero_to_NA(parish.nogoranks>0.9)
  parish.oppranks<-rank_after_mask(opp1.r,parish.polygon)
  parish.oppranks10<-zero_to_NA(parish.oppranks>0.9)
  parish.nogoranks10<-crop(parish.nogoranks10,e)
  parish.oppranks10<-crop(parish.oppranks10,e)
  
  # Write rasters
  parishname<-parish.name[p]
  writeRaster(parish.nogoranks10,paste0(dir_parishrasters,parishname,"_nogo10.tif"),overwrite=TRUE)
  writeRaster(parish.oppranks10,paste0(dir_parishrasters,parishname,"_opp10.tif"),overwrite=TRUE)
  
}



