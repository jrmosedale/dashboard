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
  parish.nogoranks10<-raster(paste0(dir_parishrasters,parishname,"_nogo10.tif"))
  parish.oppranks10<-raster(paste0(dir_parishrasters,parishname,"_opp10.tif"))
  
}