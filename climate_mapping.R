#Mapping records and assessing site similarity.

#Libraries
library(raster)
library(maps)
library(grDevices)

#Start with locations of cores, extract climate and vegetation data.

all_read <- read.csv("data/MB_data-read.csv", header = TRUE)

locations <- unique(all_read$Map.Locale)

rd_loc <- locations

rd_lon <- sapply(locations, function(x){
  all_read$LON[all_read$Map.Locale == x][1]
})

rd_lat <- sapply(locations, function(x){
  all_read$LAT[all_read$Map.Locale == x][1]
})

all_locations <- data.frame(rd_lon, rd_lat, row.names = locations)
colnames(all_locations) = c("LON", "LAT")

#Extract chronological data from each record.

#Pull and extract spatial data.

tavg.files=list.files("worldclim/wc2.1_30s_tavg/",".tif",full.names=TRUE)
tavg=stack(tavg.files)

prec.files=list.files("worldclim/wc2.1_30s_prec/",".tif",full.names=TRUE)
prec=stack(prec.files)

month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

names(tavg)=month
names(prec)=month

###Extracts the data based on lat and long.
#Making average temp data frame

tavg.data=extract(tavg,all_locations)
tavg.data=as.data.frame(tavg.data)
row.names(tavg.data)=row.names(all_locations)
#
##Making precip data frame
#
prec.data=extract(prec,all_locations)
prec.data=as.data.frame(prec.data)
row.names(prec.data)=row.names(all_locations)

#Basic mapping.

###MAPPING SETUP

LAT_RANGE=c(-9,9)
LON_RANGE=c(6,25)

#LAT_RANGE=c(20,30)
#LON_RANGE=c(-90,-80)


tempcol=colorRampPalette(c("purple","blue","skyblue","green","lightgreen","yellow","orange","red","darkred")) #This is a cool means of constructing gradient colors

map_LAT=c(-10,10) #Defining a different mapped area from the latitude/longitude selection of the taxa
map_LON=c(5,26) #Defining a different mapped area from the latitude/longitude selection of the taxa

#map_LAT=c(15,35) #Defining a different mapped area from the latitude/longitude selection of the taxa
#map_LON=c(-100,-70) #Defining a different mapped area from the latitude/longitude selection of the taxa

plot_months=c("Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")

old.mar=par(mar=c(5, 4, 4, 2) + 0.1)
old.par=par(mfrow=c(1,1))

#Below is some plotting code as an example.

#plot(0,0,xlim=LON_RANGE,ylim=LAT_RANGE,xlab="Longitude",ylab="Latitude",pch=NA) #Solving plotting problems by making an empty plot with defined boundaries
#plot(prec$Aug,col=tempcol(100),xlim=map_LON,ylim=map_LAT,add=TRUE) #Draw map within predefined area up to the area limits.
#isohyets=rasterToContour(prec$Aug,maxpixels=1e6,nlevels=30)
#plot(isohyets,add=TRUE,xlim=map_LON,ylim=map_LAT)
#map("world",add=TRUE,xlim=map_LON) #Can also control the drawing here, tends to bleed over. 
#title(main="January temperatures, WorldClim 2.1")
#points(nt_sites$lon, nt_sites$lat, pch = 21, bg = "black")

project.area=extent(map_LON[1],map_LON[2],map_LAT[1],map_LAT[2])
proj.prec=crop(prec,project.area)
proj.tavg=crop(tavg,project.area)
proj.isohyets=rasterToContour(proj.prec$Apr,maxpixels = 1e4, nlevels=20)

DJF.prec=sum(proj.prec$Dec,proj.prec$Jan,proj.prec$Feb)
MAM.prec=sum(proj.prec$Mar,proj.prec$Apr,proj.prec$May)
JJA.prec=sum(proj.prec$Jun,proj.prec$Jul,proj.prec$Aug)
SON.prec=sum(proj.prec$Sep,proj.prec$Oct,proj.prec$Nov)

seasonal=array(c(DJF.prec,MAM.prec,JJA.prec,SON.prec))
#seasonal=as.raster(seasonal)
names(seasonal)=c("DJF.prec","MAM.prec","JJA.prec","SON.prec")
seas_names=c("DJF","MAM","JJA","SON")

###Annual precip.
par(mar=c(5,4,4,7)+0.1)
prec.annual=sum(proj.prec[[1:12]])
ann.iso=rasterToContour(prec.annual, nlevels = 20)
plot(0,0,xlim=LON_RANGE,ylim=LAT_RANGE,xlab="Lon",ylab="Lat",pch=NA)
plot(prec.annual,col=tempcol(100),xlim=map_LON,ylim=map_LAT,add=TRUE)
plot(ann.iso,add=TRUE,lty=3)
points(all_read$LON, all_read$LAT, pch = 21, bg = "gold")
map("world",add=TRUE,xlim=map_LON)
title(main="Annual Precipitation, WorldClim 2.1")
par(mar=c(5, 4, 4, 2) + 0.1)

gc()

#Map seasonal precipitation.
par(mar=c(5,4,4,7)+0.1, mfrow = c(2,2))
for(i in 1:length(seasonal)){
  ses.iso = rasterToContour(seasonal[[i]])
  plot(0, 0, xlim = LON_RANGE, ylim = LAT_RANGE, xlab = "Lon", ylab = "Lat", pch = NA)
  plot(seasonal[[i]], col = tempcol(100), xlim = map_LON, ylim = map_LAT, add = TRUE)
  plot(ses.iso, add = TRUE, lty = 3)
  map("world", add = TRUE, xlim = map_LON, ylim = map_LAT)
  points(all_read$LON, all_read$LAT, pch = 21, bg = "gold")
  title(main = paste0(seas_names[i], " Precip in mm"))
}
par(mar=c(5, 4, 4, 2) + 0.1, mfrow = c(1,1))

gc()
#Let's work with some climate and precipitation data.

#Order by latitutde

prec.data = prec.data[order(all_locations$LAT), ]
tavg.data = tavg.data[order(all_locations$LAT), ]

new_lat <- all_locations$LAT[order(all_locations$LAT)]
new_lon <- all_locations$LON[order(all_locations$LAT)]

#filter by lat/long

prsl.data = prec.data[new_lat > LAT_RANGE[1] & new_lat < 8 & new_lon > LON_RANGE[1] & new_lon < LON_RANGE[2], ]

par(mar = c(5, 6, 4, 5) + 0.1, mfrow = c(1,2))
barplot(t(prsl.data), horiz = TRUE, las = 1, cex.names = 0.7, cex.axis = 0.9, main = "Monthly Precipitation From WorldClim 2.1")

barplot(t(tavg.data), horiz = TRUE, las = 1, cex.names = 0.7, cex.axis = 0.9, main = "Monthly Avg. Temp. From WorldClim 2.1")
par(mar=c(5, 4, 4, 2) + 0.1, mfrow = c(1,1))

#Can we do something more interesting with WorlClim and site type?



















