#Mapping Bantu script

#Libraries

library(raster)
library(terra)
library(lipdR)
library(neotoma2)
library(maps)

#Start with locations of cores, extract climate and vegetation data.

#1) Needs a list of records from Neotoma.
#2) Needs a list of records from APD.

nt_sites <- read.csv("neotoma_all.csv", header = TRUE)

site_latlong <- data.frame(nt_sites$lat, nt_sites$lon, row.names = nt_sites$collectionunit)

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

tavg.data=extract(tavg,site_latlong)
tavg.data=as.data.frame(tavg.data)
row.names(tavg.data)=nt_sites$collectionunit
#
##Making precip data frame
#
prec.data=extract(prec,site_latlong)
prec.data=as.data.frame(prec.data)
row.names(prec.data)=nt_sites$collectionunit

#Basic mapping.

###MAPPING SETUP

LAT_RANGE=c(-10,15)
LON_RANGE=c(-5,30)

#LAT_RANGE=c(20,30)
#LON_RANGE=c(-90,-80)


tempcol=colorRampPalette(c("purple","blue","skyblue","green","lightgreen","yellow","orange","red","darkred")) #This is a cool means of constructing gradient colors

map_LAT=c(-25,25) #Defining a different mapped area from the latitude/longitude selection of the taxa
map_LON=c(-20,40) #Defining a different mapped area from the latitude/longitude selection of the taxa

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
par(mar=c(5,4,4,5)+0.1)
prec.annual=sum(proj.prec[[1:12]])
ann.iso=rasterToContour(prec.annual)
plot(0,0,xlim=LON_RANGE,ylim=LAT_RANGE,xlab="Lon",ylab="Lat",pch=NA)
plot(prec.annual,col=tempcol(100),xlim=map_LON,ylim=map_LAT,add=TRUE)
plot(ann.iso,add=TRUE,lty=2)
points(nt_sites$lon, nt_sites$lat, pch = 21, bg = "black")
map("world",add=TRUE,xlim=map_LON)
title(main="Annual Precipitation, WorldClim 2.1")
par(old.mar)

gc()
