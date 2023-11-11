#Chronological trends in Central African pollen records.

#Libraries

library(rcarbon)

#Setting some parameters.

LAT_RANGE=c(-9,9)
LON_RANGE=c(6,25)

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

all.spd.bins <- spd(all.caldates, bins = all.bin, timeRange = c(11700,100))

plot(all.spd.bins)

#Kdensity

all.rdates <- sampleDates(all.caldates, bins = all.bin, nsim = 500, verbose = FALSE)

all.ckde <- ckde(all.rdates, timeRange = c(11700, 100), bw = 200)

plot(all.ckde)

#Make groups and do group SPD

all.stack <- stackspd(x = all.caldates, 
                     group = C14_read$region, 
                     timeRange = c(11700, 100), 
                     bins = all.bin, 
                     runm = 50, 
                     verbose = FALSE)

#Make fancy plot from Crema and Bevan (2021)

par(mfrow = c(2,2))
plot(all.stack, type = "stacked", legend = FALSE)
plot(all.stack, type = "lines")
plot(all.stack, type = "multipanel", legend = FALSE )
plot(all.stack, type = "proportion", legend = FALSE)
par(mfrow = c(1,1))

#This makes a sort of sensible figure.
