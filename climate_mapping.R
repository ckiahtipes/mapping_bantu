#Mapping records and assessing site similarity.

#Libraries
library(raster)
library(maps)
library(grDevices)
library(sf)
library(rnaturalearth)

#Plotting and saving controls.

save_figs = TRUE

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

#We can also get some elevation data.

if(!file.exists("srtm/tiled_region.tif")){
  lon_grab <- c(rep(5, 4), rep(10, 4), rep(15, 4), rep(20, 4))
  lat_grab <- c(rep(c(seq(-5, 10, 5)),4))
  
  srtm_tiles <- lapply(1:15, function(x){
    array
  })
  
  tile_name <- list(paste0('N',lon_grab,'W',lat_grab))
  names(srtm_tiles) = tile_name
  
  for(i in 2:length(tile_name[[1]])){ #There's a tile of ocean and we need to leave it, so start at 2
    srtm_tiles[i-1] = getData('SRTM', lon = lon_grab[i], lat = lat_grab[i], path = "srtm/")
  }
  
  region_SRTM <- mosaic(srtm_tiles[[1]],
                        srtm_tiles[[2]],
                        srtm_tiles[[3]],
                        srtm_tiles[[4]],
                        srtm_tiles[[5]],
                        srtm_tiles[[6]],
                        srtm_tiles[[7]],
                        srtm_tiles[[8]],
                        srtm_tiles[[9]],
                        srtm_tiles[[10]],
                        srtm_tiles[[11]],
                        srtm_tiles[[12]],
                        srtm_tiles[[13]],
                        srtm_tiles[[14]],
                        srtm_tiles[[15]],
                        fun = mean)
  
  writeRaster(region_SRTM, "srtm/tiled_region.tif")
} else {
  region_SRTM <- raster("srtm/tiled_region.tif")
}



#Get rivers, lakes, and ocean

# download if needed
if(!file.exists("ne_maps/ne_10m_lakes.cpg")){
ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", 
              destdir = "ne_maps/", load = FALSE) # major rivers

ne_download(scale = 10, type = "lakes", category = "physical", 
              destdir = "ne_maps/", load = FALSE) # major lakes

ne_download(scale = 10, type = "ocean", category = "physical",
            destdir = "ne_maps/", load = FALSE) # ocean

}

rivers <- ne_load(scale = 10, type = "rivers_lake_centerlines", destdir = "ne_maps", returnclass = "sf")
lakes <- ne_load(scale = 10, type = "lakes", destdir = "ne_maps", returnclass = "sf")
ocean <- ne_load(scale = 10, type = "ocean", destdir = "ne_maps", returnclass = "sf")

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

##Get elevations
elev.data <- extract(region_SRTM, all_locations)
elev.data <- as.data.frame(elev.data)
row.names(elev.data) <- row.names(all_locations)

#Basic mapping.

###MAPPING SETUP

LAT_RANGE=c(-9,9)
LON_RANGE=c(6,24)

#LAT_RANGE=c(20,30)
#LON_RANGE=c(-90,-80)


tempcol=colorRampPalette(c("purple","blue","skyblue","green","lightgreen","yellow","orange","red","darkred"), alpha = FALSE) #This is a cool means of constructing gradient colors

map_LAT=c(-10,10) #Defining a different mapped area from the latitude/longitude selection of the taxa
map_LON=c(5,25) #Defining a different mapped area from the latitude/longitude selection of the taxa

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
prec.annual=sum(proj.prec[[1:12]])
ann.iso=rasterToContour(prec.annual, nlevels = 20)

if(save_figs == TRUE){
  #Plotting method
  setEPS()
  #pdf("Figure-1_vegmap.pdf", height = 5, width = 8)
  tiff("Figure-2_ann-precip.tiff", height = 1900, width = 2400, res = 300)
}

par(mar=c(5,4,4,10)+0.1)

plot(0,0,xlim=LON_RANGE,ylim=LAT_RANGE,pch=NA, axes = FALSE, ann = FALSE)
plot(prec.annual,col = tempcol(100),xlim=map_LON,ylim=map_LAT,add=TRUE, legend = TRUE)
plot(ann.iso, add=TRUE, lty=1, lwd = 0.5)
plot(rivers$geometry, add = TRUE, col = "darkblue", lty = 1, lwd = 1)
plot(lakes$geometry, add = TRUE, col = "lightblue")
plot(ocean$geometry, add = TRUE, col = "lightblue")
plot(region_SRTM, col = grey.colors(4024, start = 0.0001, end = 0.9999, gamma = 0.01, alpha = 0.3, rev = TRUE) ,add = TRUE, legend = FALSE)

#map("world",add=TRUE,xlim=map_LON, lwd = 1, col = "black")



axis(1, at = seq(5, 20, 5), labels = seq(5, 20, 5), cex.axis = 0.8)
axis(1, at = c(20,35), labels = NA, lwd.ticks = 0)
axis(2, at = seq(-10, 10, 5), labels = seq(-10, 10, 5), cex.axis = 0.8)
axis(3, at= c(2, 35), labels = NA, lwd.ticks = 0)
axis(4, at = seq(-10, 10, 5), labels = NA, lwd.ticks = 0)
title(main="Avg. Annual Precipitation in mm/yr, WorldClim 2.1",
      xlab = "LON",
      ylab = "LAT",
      cex.lab = 0.8)

label_lat <- c(3.5, #Niger River Delta
               1.5, #Nyabessan
               3, #Ossa
               6, #Mbaodang
               7, #Barombi Mbo
               7, #Monoun 
               7, #Bambili
               8, #Mbi
               9, #Tizong
               8, #Mbalang
               -4, #Kamalete
               -3, #Nguene
               -0.5, #Mariador
               -6.5, #Songolo
               -6.75, #Coraf
               -5, #Kitina
               -6.5, #Sinnda
               -6.25, #Ngamakala pond
               -5.5, #Bois de Bilanko
               1, #Ekolongouma
               1.75, #Goualougo
               2.75, #Bemba yanga
               3, # Mopo Bai
               5.5, #FC400
               -3) #ING100

label_lon <- c(6.5, #Niger River Fan
               8.5, #Nyabessan
               8.5, #Ossa
               7, #Mbaodang
               7.75, #Barombi Mbo
               11, #Monoun
               9.25, #Bambili
               9.5, #Mbi
               13, #Tizong
               12.5, #Mbalang
               9.5, #Kamalete
               8.5, #Nguene
               8, #Mariador
               11, #Songolo
               10, #Coraf
               10.5, #Kitina
               13, #Sinnda
               15, #Ngamakala
               17, #Bois de bilanko
               19, #Ekolongouma
               18.25, #Goualougo
               18.75, #Bemba yanga
               17.5, #Mopo Bai
               16.5, #FC400
               19) #ING100

label_plots <- data.frame(label_lon, label_lat)

arrows(label_plots$label_lon, label_plots$label_lat, all_locations$LON, all_locations$LAT, length = 0)
points(label_plots$label_lon, label_plots$label_lat, pch = 21, bg = "gold", cex = 3)
points(all_read$LON, all_read$LAT, pch = 21, bg = "gold")
text(label_lon, label_lat, labels= c(1:nrow(label_plots)), cex = 0.8)

arrows(8, -6.5, 8, -7.5, lwd = 3, angle = 40, code = 1, length = 0.05)
text(7.5,-7, "N", cex = 1.5)
gc()
if(save_figs == TRUE){
  dev.off()
}

par(mar=c(5, 4, 4, 2) + 0.1)

#Map seasonal precipitation.
if(save_figs == TRUE){
  setEPS()
  #pdf("Figure-1_vegmap.pdf", height = 5, width = 8)
  tiff("Figure-3_seas-precip.tiff", height = 1900, width = 2400, res = 300)
}

par(mar=c(3,2,1,5)+0.1, mfrow = c(2,2))
for(i in 1:length(seasonal)){
  ses.iso = rasterToContour(seasonal[[i]])
  plot(0, 0, xlim = LON_RANGE, ylim = LAT_RANGE, axes = FALSE, ann = FALSE, pch = NA)
  plot(seasonal[[i]], col = tempcol(100), xlim = map_LON, ylim = map_LAT, add = TRUE)
  plot(region_SRTM, col = grey.colors(4024, start = 0.0001, end = 0.9999, gamma = 0.01, alpha = 0.4, rev = TRUE) ,add = TRUE, legend = FALSE)
  plot(ses.iso, add = TRUE, lty = 3)
  map("world", add = TRUE, xlim = map_LON, ylim = map_LAT)
  points(all_read$LON, all_read$LAT, pch = 21, bg = "gold")
  title(main = paste0(seas_names[i], " Precip in mm"))
  axis(1, at = seq(5, 20, 5), labels = seq(5, 20, 5), cex.axis = 0.8)
  axis(1, at = c(20,35), labels = NA, lwd.ticks = 0)
  axis(2, at = seq(-10, 10, 5), labels = seq(-10, 10, 5), cex.axis = 0.8)
  axis(3, at= c(2, 35), labels = NA, lwd.ticks = 0)
  axis(4, at = seq(-10, 10, 5), labels = NA, lwd.ticks = 0)
  arrows(8, -3.5, 8, -4.5, lwd = 3, angle = 40, code = 1, length = 0.05)
  #text(7.5,-4, "N", cex = 1.5)
}

if(save_figs == TRUE){
  dev.off()
}
par(mar=c(5, 4, 4, 2) + 0.1, mfrow = c(1,1))
gc()

#Let's work with some climate and precipitation data.

#Order by latitutde

prec.data = prec.data[order(all_locations$LAT), ]
tavg.data = tavg.data[order(all_locations$LAT), ]
elev.ordr = elev.data[order(all_locations$LAT), ]
elev.nmes = row.names(elev.data)[order(all_locations$LAT)]

new_lat <- all_locations$LAT[order(all_locations$LAT)]
new_lon <- all_locations$LON[order(all_locations$LAT)]

#filter by lat/long

prsl.data = prec.data[new_lat > LAT_RANGE[1] & new_lat < 8 & new_lon > LON_RANGE[1] & new_lon < LON_RANGE[2], ]
if(save_figs == TRUE){
  setEPS()
  #pdf("Figure-1_vegmap.pdf", height = 5, width = 8)
  tiff("Figure-4_sites.tiff", height = 1900, width = 2400, res = 300)
}

par(mar = c(5, 6, 4, 5) + 0.1, mfrow = c(1,2))
barplot(t(prsl.data), horiz = TRUE, las = 1, cex.names = 0.7, cex.axis = 0.9, main = "Monthly Precipitation", col = heat.colors(12), lwd = 1)

barplot(t(elev.ordr), horiz = TRUE, names.arg = elev.nmes, las = 1, cex.names = 0.7, cex.axis = 0.9, main = "Elevations", col = "lightblue", lwd = 1)
par(mar=c(5, 4, 4, 2) + 0.1, mfrow = c(1,1))
gc()
if(save_figs == TRUE){
  dev.off()
}
#Can we do something more interesting with WorlClim and site type?

#wBio <- getData("worldclim", var = "bio", res = 2.5)
#
#wBio_0 <- getData("worldclim", var="bio", res = 0.5, lon = 15, lat = 0)
#wBio_10 <- getData("worldclim", var="bio", res = 0.5, lon = 15, lat = 10)
#
#seas <- crop(wBio$bio4, project.area)
#seas_coeff <- crop(wBio$bio15, project.area)
#
#seas_0 <- crop(wBio_0$bio4_36, project.area)
#seas_10 <- crop(wBio_10$bio4_26, project.area)
#
#plot(0, 0, xlim = LON_RANGE, ylim = LAT_RANGE, xlab = "Lon", ylab = "Lat", pch = NA)
##plot(seas, col = tempcol(100), slim = map_LONG, ylim = map_LAT, add = TRUE)
#plot(seas_coeff, col = tempcol(100), slim = map_LONG, ylim = map_LAT, add = TRUE)
#plot(seas_0, col=tempcol(100), xlim=map_LON, ylim=map_LAT, add=TRUE)
#plot(seas_10, col = tempcol(100), slim = map_LONG, ylim = map_LAT, add = TRUE)
#

#Testing plotting of labels


