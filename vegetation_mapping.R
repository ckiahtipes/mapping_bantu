#Vegetation zones for Mapping Bantu Environments

library(raster)
library(maps)
library(grDevices)
library(sf)
library(rnaturalearth)

#Logical for saving figures

save_figs = TRUE

grayscale = TRUE

#Read file with collection units and site locations.

all_read <- read.csv("data/MB_data-read.csv", header = TRUE)

all_latlong <- data.frame(all_read$LON, all_read$LAT, row.names = all_read$COLL_UNIT)
colnames(all_latlong) = c("LON", "LAT")

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

#Get file paths for necessary vegetation maps.

hw_path="~/Desktop/ARCHAEO_BIC/mapping/Crezee_Peatmap_ML_Dec20_1000runs/ENVI_ML_Dec20_1000runs_probability_Hardwood_swamp.tif"
pl_path="~/Desktop/ARCHAEO_BIC/mapping/Crezee_Peatmap_ML_Dec20_1000runs/ENVI_ML_Dec20_1000runs_probability_Palm_swamp.tif"
comb_path="~/Desktop/ARCHAEO_BIC/mapping/Crezee_Peatmap_ML_Dec20_1000runs/ENVI_ML_Dec20_1000runs_Total_Peat_Probability.tif"

fortypes_path="~/Desktop/ARCHAEO_BIC/mapping/Phillipon_2019_veg_classes/classification_10classes_georefOK_WGS84_UINT8/classification_10classes_georefOK_WGS84_UINT8.tif"

ESAtrsh_path="~/Desktop/ARCHAEO_BIC/mapping/ESA_vegmap20m/ESA_trsh200m.tif"
ESAgrcp_path="~/Desktop/ARCHAEO_BIC/mapping/ESA_vegmap20m/ESA_grcp200m.tif"

#Use paths to load raster data.

hw.raster <- raster(hw_path)
pl.raster <- raster(pl_path)
cm.raster <- raster(comb_path)
ft.raster <- raster(fortypes_path)
ET.raster <- raster(ESAtrsh_path)
EG.raster <- raster(ESAgrcp_path)

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


#Downsampling code below, but not really worth the time.

#hw.new <- projectRaster(hw.raster, crs = crs(tavg$Jan))
#hw.new <- resample(x = hw.raster, y = tavg$Jan, method = "bilinear")


#Extract some values.

hw.extract <- extract(hw.raster, all_latlong)
pl.extract <- extract(pl.raster, all_latlong)
cm.extract <- extract(cm.raster, all_latlong)
ft.extract <- extract(ft.raster, all_latlong)
ET.extract <- extract(ET.raster, all_latlong)
EG.extract <- extract(EG.raster, all_latlong)

veg_assign <- data.frame(hw.extract, pl.extract, cm.extract, ft.extract, ET.extract, EG.extract, row.names = all_read$COLL_UNIT)

#Map and data range

LAT_RANGE=c(-10,15)
LON_RANGE=c(-3,30)

map_LAT=c(-9,9) #Defining a different mapped area from the latitude/longitude selection of the taxa
map_LON=c(6,30) #Defining a different mapped area from the latitude/longitude selection of the taxa

#Need to assign colors to make these plot properly.

if(grayscale == TRUE){
  
  palette(gray.colors(1000))
  
  cm_col=c(1000,1000)
  #ft_col=colorRampPalette(c("#878cff","#d2fc9e","#748729"))
  
  ft_col=c("dark green", "blue", "green", "skyblue", "lightgreen", "darkorange", "red", "brown", "purple")

    ft_col=c(300, #dark green
           300, #blue
           400, #green
           500, #skyblue
           600, #lightgreen
           700, #darkorange
           800, #red
           900, #brown
           1000) #purple
  
  #hw_col=colorRampPalette(c("#c5c5c500","#3d85c699"), alpha = TRUE)
  hw_col=c(200,200)
  #pl_col=colorRampPalette(c(NA,"light blue"))
  ESAtrsh_col=c(NA,100)
  ESAgrcp_col=c(NA,0)
  
} else {
  
  cm_col=colorRampPalette(c("#c5c5c500","#3d85c699"), alpha = TRUE)
  #ft_col=colorRampPalette(c("#878cff","#d2fc9e","#748729"))
  
  ft_col=c("dark green", "blue", "green", "skyblue", "lightgreen", "darkorange", "red", "brown", "purple")
  
  hw_col=colorRampPalette(c("#c5c5c500","#3d85c699"), alpha = TRUE)
  #pl_col=colorRampPalette(c(NA,"light blue"))
  ESAtrsh_col=c(NA,"#6c9575")
  ESAgrcp_col=c(NA,"#ffd731")
  
}


#Basic plotting method.
if(save_figs == TRUE){
  setEPS()
  #pdf("Figure-1_vegmap.pdf", height = 5, width = 8)
  tiff("Figure-1_vegmap.tiff", height = 1900, width = 2400, res = 300)
}

par(mar=c(5,4,4,3)+0.1)
plot(0, 0, pch=NA, axes = FALSE, ann = FALSE, xlim=map_LON, ylim=map_LAT)

if(grayscale == TRUE){
  
  plot(ET.raster,col=ESAtrsh_col, add = TRUE, legend = FALSE, axes = FALSE, ann = FALSE)
  plot(EG.raster,col=ESAgrcp_col, add = TRUE, legend = FALSE, axes = FALSE, ann = FALSE)
  #plot(hw.new, col = hw_col(1000), add = TRUE, legend = FALSE, axes = FALSE, ann = FALSE)
  plot(cm.raster,col=cm_col(1000),add = TRUE, legend = FALSE, axes = FALSE, ann = FALSE)
  plot(ft.raster,col=ft_col,add=TRUE,legend=FALSE, axes = FALSE, ann = FALSE)
  plot(rivers$geometry, add = TRUE, col = "darkblue", lty = 1, lwd = 1)
  plot(lakes$geometry, add = TRUE, col = "lightblue")
  plot(ocean$geometry, add = TRUE, col = "lightblue")
  plot(region_SRTM, col = grey.colors(4024, start = 0.0001, end = 0.9999, gamma = 0.1, alpha = 0.3, rev = TRUE) ,add = TRUE, legend = FALSE)
  map("world",add=TRUE,xlim=map_LON,ylim=map_LAT, lty = 1)
  #tmap("rivers",add = TRUE)
  
  legend(c(25,32), c(10,-10),
         bty = "o",
         box.lwd = NA,
         c(NA,
           NA,
           NA,
           "ESA-tree", 
           "ESA-herb", 
           "EVG forest", 
           "S-D sw. frag",
           "EVG-SD mix",
           "S-D west Af",
           "S-D sest Af",
           "Decid n. frag",
           "S-D nest Af",
           "2ndry S-D se Af",
           "2ndry S-D ne Af",
           "ICB Hydro Complex",
           NA,
           NA,
           "Pollen Records",
           "Rivers",
           "Lakes",
           "Borders"),
         pch = c(NA, NA, NA, rep(22, 12), NA, NA, 21, NA, 22),
         pt.bg = c(NA,
                   NA,
                   NA,
                   "#6c9575",
                   "#ffd731",
                   "dark green",
                   "blue",
                   "green",
                   "skyblue",
                   "lightgreen",
                   "darkorange",
                   "red",
                   "brown",
                   "purple",
                   "#3d85c699",
                   NA,
                   NA,
                   "gold",
                   NA,
                   "lightblue",
                   NA),
         lty = c(rep(NA, 18), 1, NA, 1),
         col = c(rep("black", 18), "blue", "black", "black"),
         cex = 0.7)
  
  
  
  #Contours!
  #ft.contour = rasterToContour(ft.raster, nlevels = 9)
  #plot(ann.iso, lty = 1, lwd = 2, col = "#54524c",add = TRUE, legend = FALSE, axes = FALSE, ann = FALSE)
  
  arrows(8, -6.5, 8, -7.5, lwd = 3, angle = 40, code = 1, length = 0.05)
  text(7.5,-7, "N", cex = 1.5)
  
  points(all_latlong$LON, all_latlong$LAT, pch = 21, bg = "gold")
  axis(1, at = seq(5, 20, 5), labels = seq(5, 20, 5), cex.axis = 0.8)
  axis(1, at = c(20,35), labels = NA, lwd.ticks = 0)
  axis(2, at = seq(-10, 10, 5), labels = seq(-10, 10, 5), cex.axis = 0.8)
  axis(3, at= c(2, 35), labels = NA, lwd.ticks = 0)
  axis(4, at = seq(-10, 10, 5), labels = NA, lwd.ticks = 0)
  title(main = "Vegetation Type Composite for Central-West African Rain Forests",
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
  
  
} else {
  
  plot(ET.raster,col=ESAtrsh_col, add = TRUE, legend = FALSE, axes = FALSE, ann = FALSE)
  plot(EG.raster,col=ESAgrcp_col, add = TRUE, legend = FALSE, axes = FALSE, ann = FALSE)
  #plot(hw.new, col = hw_col(1000), add = TRUE, legend = FALSE, axes = FALSE, ann = FALSE)
  plot(cm.raster,col=cm_col(1000),add = TRUE, legend = FALSE, axes = FALSE, ann = FALSE)
  plot(ft.raster,col=ft_col,add=TRUE,legend=FALSE, axes = FALSE, ann = FALSE)
  plot(rivers$geometry, add = TRUE, col = "darkblue", lty = 1, lwd = 1)
  plot(lakes$geometry, add = TRUE, col = "lightblue")
  plot(ocean$geometry, add = TRUE, col = "lightblue")
  plot(region_SRTM, col = grey.colors(4024, start = 0.0001, end = 0.9999, gamma = 0.1, alpha = 0.3, rev = TRUE) ,add = TRUE, legend = FALSE)
  map("world",add=TRUE,xlim=map_LON,ylim=map_LAT, lty = 1)
  #tmap("rivers",add = TRUE)
  
  legend(c(25,32), c(10,-10),
         bty = "o",
         box.lwd = NA,
         c(NA,
           NA,
           NA,
           "ESA-tree", 
           "ESA-herb", 
           "EVG forest", 
           "S-D sw. frag",
           "EVG-SD mix",
           "S-D west Af",
           "S-D sest Af",
           "Decid n. frag",
           "S-D nest Af",
           "2ndry S-D se Af",
           "2ndry S-D ne Af",
           "ICB Hydro Complex",
           NA,
           NA,
           "Pollen Records",
           "Rivers",
           "Lakes",
           "Borders"),
         pch = c(NA, NA, NA, rep(22, 12), NA, NA, 21, NA, 22),
         pt.bg = c(NA,
                   NA,
                   NA,
                   "#6c9575",
                   "#ffd731",
                   "dark green",
                   "blue",
                   "green",
                   "skyblue",
                   "lightgreen",
                   "darkorange",
                   "red",
                   "brown",
                   "purple",
                   "#3d85c699",
                   NA,
                   NA,
                   "gold",
                   NA,
                   "lightblue",
                   NA),
         lty = c(rep(NA, 18), 1, NA, 1),
         col = c(rep("black", 18), "blue", "black", "black"),
         cex = 0.7)
  
  
  
  #Contours!
  #ft.contour = rasterToContour(ft.raster, nlevels = 9)
  #plot(ann.iso, lty = 1, lwd = 2, col = "#54524c",add = TRUE, legend = FALSE, axes = FALSE, ann = FALSE)
  
  arrows(8, -6.5, 8, -7.5, lwd = 3, angle = 40, code = 1, length = 0.05)
  text(7.5,-7, "N", cex = 1.5)
  
  points(all_latlong$LON, all_latlong$LAT, pch = 21, bg = "gold")
  axis(1, at = seq(5, 20, 5), labels = seq(5, 20, 5), cex.axis = 0.8)
  axis(1, at = c(20,35), labels = NA, lwd.ticks = 0)
  axis(2, at = seq(-10, 10, 5), labels = seq(-10, 10, 5), cex.axis = 0.8)
  axis(3, at= c(2, 35), labels = NA, lwd.ticks = 0)
  axis(4, at = seq(-10, 10, 5), labels = NA, lwd.ticks = 0)
  title(main = "Vegetation Type Composite for Central-West African Rain Forests",
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
  
}


if(save_figs == TRUE){
  dev.off()
}
par(mar=c(5, 4, 4, 2) + 0.1)
gc()














