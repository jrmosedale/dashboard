
mapcolour4scale<- function(r) {
  if (class(r)[1]=="RasterLayer") v<-getValues(r)
  #q<-quantile(v,c(0.25,0.5,0.8,1),names=FALSE,na.rm=TRUE)
  #mnmx<-c(min(q),max(q)) # or could be added as parameter
  #pal<-colorNumeric("YlOrBr",domain=q,na.color=NA)
  pal <- colorQuantile("YlOrBr", domain=v, n = 4,na.color=NA)
  return(pal)
}


leaflet() %>% 
  setView(lng = -4.9, lat = 50.4, zoom = 10)  %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addScaleBar() %>%
  addRasterImage(soilmitig.r, colors=mapcolour4scale(zero_to_NA(soilmitig.r)),
                 opacity = 1,project=TRUE, group="Soil_loss_mitigation") %>%
  addLegend(position = "bottomright",
            pal=mapcolour4scale(soilmitig.r),values<-values(soilmitig.r) ),labels=c("Low","Medium","High","Very High"),
            na.label="None",group="Soil_loss_mitigation", opacity=1) 


# Display data table of method info
tablefile<-"data/documentation/woodland_exclusion_table.csv"
t1.df<-read.csv("data/documentation/woodland_exclusion_table.csv" ,header=TRUE,stringsAsFactors=FALSE, na.strings = "") 

datatable(t1.df, class='hover row-border',options = list(dom = 't',searching = FALSE,
          pageLength = nrow(t1.df)),
          caption=htmltools::tags$caption(
  style = 'caption-side: top; text-align: left;',
  htmltools::h3('Exclusion areas'),'Locations excluded from consideration, such as where existing land cover to be maintained or inherently unsuitable for woodland creation.'),
rownames=FALSE,colnames = c('Excluded Area', 'Data Source'))

