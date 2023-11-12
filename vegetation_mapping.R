#Vegetation zones for Mapping Bantu Environments

library(raster)
library(maps)
library(grDevices)

#Logical for saving picures

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
map_LON=c(6,25) #Defining a different mapped area from the latitude/longitude selection of the taxa

#Need to assign colors to make these plot properly.

cm_col=colorRampPalette(c("#c5c5c500","#3d85c699"), alpha = TRUE)
ft_col=colorRampPalette(c("#878cff","#d2fc9e","#748729"))

ft_col=c("dark green", "blue", "green", "sky blue", "light green", "dark orange", "red", "brown", "brown")

hw_col=colorRampPalette(c("#c5c5c500","#3d85c699"), alpha = TRUE)
#pl_col=colorRampPalette(c(NA,"light blue"))
ESAtrsh_col=c(NA,"#6c9575")
ESAgrcp_col=c(NA,"#ffd731")

#Basic plotting method.

plot(0, 0, pch=NA, xlim=map_LON, ylim=map_LAT, ylab="Lat", xlab="Long")

plot(ET.raster,col=ESAtrsh_col, add = TRUE, legend = FALSE, axes = FALSE, ann = FALSE)
plot(EG.raster,col=ESAgrcp_col, add = TRUE, legend = FALSE, axes = FALSE, ann = FALSE)
#plot(hw.new, col = hw_col(1000), add = TRUE, legend = FALSE, axes = FALSE, ann = FALSE)
plot(cm.raster,col=cm_col(1000),add = TRUE, legend = FALSE, axes = FALSE, ann = FALSE)
plot(ft.raster,col=ft_col,add=TRUE,legend=FALSE, axes = FALSE, ann = FALSE)
map("world",add=TRUE,xlim=map_LON,ylim=map_LAT, lty = 2)

#Contours!
#ft.contour = rasterToContour(ft.raster, nlevels = 9)
plot(ann.iso, lty = 1, lwd = 2, col = "#54524c",add = TRUE, legend = FALSE, axes = FALSE, ann = FALSE)

points(all_latlong$LON, all_latlong$LAT, pch = 21, bg = "magenta")

gc()













