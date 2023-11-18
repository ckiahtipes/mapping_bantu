#Chronological trends in Central African pollen records.

#Libraries

library(rcarbon)

#Setting some parameters.

LAT_RANGE=c(-9,9)
LON_RANGE=c(6,25)

min_range = 0
max_range = 11700

#Call sites, datasets, and downloads from neotoma.

all_read <- read.csv("data/MB_data-read.csv", header = TRUE)
C14_read <- read.csv("data/combined_14Craw_groups.csv", header = TRUE)

#Use rcarbon functions to make SPD

all.caldates <- calibrate(x = C14_read$all_14C,
                          errors = C14_read$all_err,
                          calCurves = C14_read$cal_curve) 

#Make bins

all.bin <- binPrep(sites=C14_read$all_nme, 
                  ages = C14_read$all_14C, 
                  h = 100)

all.spd.bins <- spd(all.caldates, bins = all.bin, timeRange = c(max_range, min_range))

plot(all.spd.bins)

#Kdensity

all.rdates <- sampleDates(all.caldates, bins = all.bin, nsim = 500, verbose = FALSE)

all.ckde <- ckde(all.rdates, timeRange = c(max_range, min_range), bw = 200)
setEPS()
#pdf("Figure-1_vegmap.pdf", height = 5, width = 8)
tiff("Figure-5_allKDE.tiff", height = 1900, width = 2400, res = 300)
plot(all.ckde)
dev.off()
#Make groups and do group SPD

all.stack <- stackspd(x = all.caldates, 
                     group = C14_read$region, 
                     timeRange = c(max_range, min_range), 
                     bins = all.bin, 
                     runm = 50, 
                     verbose = FALSE)

#Make fancy plot from Crema and Bevan (2021)
setEPS()
#pdf("Figure-1_vegmap.pdf", height = 5, width = 8)
tiff("Figure-6_regionSPD.tiff", height = 1900, width = 2400, res = 300)
par(mfrow = c(2,2))
plot(all.stack, type = "stacked", legend = FALSE)
plot(all.stack, type = "lines")
plot(all.stack, type = "multipanel", legend = FALSE )
plot(all.stack, type = "proportion", legend = FALSE)
par(mfrow = c(1,1))
dev.off()
#This makes a sort of sensible figure.
