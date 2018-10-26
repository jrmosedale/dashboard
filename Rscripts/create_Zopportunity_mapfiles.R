
library(rgdal)
library(rgeos)
library(raster)
library(ggplot2)
library(sf)
library(mapview)
library(RColorBrewer)
library(leaflet)
library(zonator)
library(htmlwidgets)

###############################################################
# Set directory to write to
###############################################################
#dir_out<-paste0(dir_o)



###############################################################
# FUNCTIONS & DIRECTORIES
###############################################################

# Recalculating rank after masking
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

rank_after_crop<-function(r,local.r){
  local.cells<-getValues(local.r)
  min.local<-min(local.cells,na.rm=TRUE)
  max.local<-max(local.cells,na.rm=TRUE)
  # num.prot<-length(msk.cells[-which(is.na(msk.cells))])
  all.cells<-getValues(r)
  min.all<-min(all.cells,na.rm=TRUE)
  max.all<-max(all.cells,na.rm=TRUE)
  
  new.ranks<-(local.cells/(max.local-min.local))*(max.all-min.all)
  new.r<-setValues(local.r,new.ranks)
  return(new.r)
}

pal <- colorRampPalette(brewer.pal(10, "BrBG"))
pal <- colorRampPalette(brewer.pal(9, "BuGn"))
leg<-zlegend("spectral")

###############################################################
# Choose Zproject
###############################################################
### SET PROJECT PATH ###
dir_ZProject<-"D:/Zonation_Projects/Cornwall_Woodland/"
# or if on portable drive
dir_ZProject<-paste0("/Volumes/Pocket_Sam/data/Zonation_Projects/Cornwall_Woodland/")

dir_zinputs<-"/Volumes/Pocket_Sam/data/Zonation_data_opps/"
dir_zinputs2<-"/Volumes/Pocket_Sam/data/Zonation_data/"

###############################################################
### CHOOSE VARIANT ###
###############################################################

v<-"v06" # Applies connectivity matrix


###############################################################
# Analyse Variant Results
###############################################################
# RANKING MAP
# RANKING MAP
r<-raster(paste0(dir_ZProject,v,"/",v,"_out/",v,"_output.ABF_MEBLP10.rank.compressed.tif"))
plot(r,breaks=leg$values, col=leg$colors, main=v)

# Data layers etc 
cornwall.r<-raster(paste0(dir_zinputs,"cornwall_LT.tif"))
protected.r<-raster(paste0(dir_zinputs,"protected_mask.tif"))
protected.r<-mask(protected.r,cornwall.r)

woods1<-raster(paste0(dir_zinputs2,"habitat_110.tif"))
woods2<-raster(paste0(dir_zinputs2,"habitat_120.tif"))
woods.r<-overlay(woods1,woods2,fun=function(x,y){ifelse(is.na(x)&is.na(y),NA,1)})

plot(protected.r)
plot(mask(r,protected.r,inverse=TRUE),breaks=leg$values, col=leg$colors)
priority.r<-raster(paste0(dir_zinputs,"desig_priority.tif"))

# Masking - woodland
excluded.r<-raster(paste0(dir_zinputs,"mask_excluded.tif"))
new.r<-rank_after_mask(r,excluded.r)


plot(new.r,breaks=leg$values, col=leg$colors, main="New ranking without existing protected areas")
cws.r<-raster(paste0(dir_zinputs,"desig_cws.tif"))
plot(cws.r,add=T,col="purple")
plot(zero_to_NA(priority.r),col="green",add=T)

# Extract Top Ranked Areas
toprank5<-calc(new.r,fun=function(x){ifelse(x>=0.95,1,NA)})
plot(toprank5,col="red")
toprank10<-calc(new.r,fun=function(x){ifelse(x>=0.9 & x<0.95,1,NA)})
plot(toprank10,add=T,col="orange")
toprank20<-calc(new.r,fun=function(x){ifelse(x>=0.8 & x < 0.9,1,NA)})
plot(toprank20,add=T,col="brown")


###############################################################
# WRITE MAP FILE
###############################################################

zmap<-leaflet() %>% addTiles() %>%
  addRasterImage(zero_to_NA(excluded.r), colors = "grey", opacity = 0.7,group="Excluded") %>%
  addRasterImage(protected.r, colors = "blue", opacity = 0.7,group="Protected areas") %>%
  addRasterImage(zero_to_NA(woods.r),colors="green",opacity=0.7,group="Existing woods") %>%
  addRasterImage(toprank5, colors = "red", opacity = 0.7,group="Top 5%") %>%
  addRasterImage(toprank10, colors = "orange", opacity = 0.7,group="Top 10%") %>%
  addRasterImage(toprank20, colors = "brown", opacity = 0.7,group="Top 20%") %>%
  addLayersControl(overlayGroups = c("Excluded","Protected areas","Existing woods","Top 5%","Top 10%","Top 20%"),
                   options = layersControlOptions(collapsed=FALSE) ) %>%
  hideGroup(c("Protected areas","Existing woods","Top 20%"))



filename<-paste0(getwd(),"/maps/opps/opp_woodland_map.html")
print(filename)
saveWidget(zmap,file=filename)
