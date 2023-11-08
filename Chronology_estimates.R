#Using neotoma to pull 14C dates and run SPDs

library(neotoma2)
library(rcarbon)
library(lipdR)

#Setting some parameters.

LAT_RANGE=c(-9,9)
LON_RANGE=c(6,25)

#Call sites, datasets, and downloads from neotoma.

nt_read <- read.csv("neotoma_all.csv", header = TRUE, row.names = "X")

nt_sites <- get_sites(nt_read$siteid)

nt_datasets <- get_datasets(nt_sites, all_data = FALSE)

nt_download <- get_downloads(nt_datasets, all_data = FALSE)

#Use download to pull chronologies.

nt_chron <- chroncontrols(nt_download)

#Pull out the radiocarbon dates.

nt_radiocarbon <- nt_chron[nt_chron$chroncontroltype == "Radiocarbon" & is.na(nt_chron$chroncontroltype) == FALSE, ]

#Here we extract the names of the sites for which we returned chronologies.

chron_ids <- unique(nt_chron$siteid)
chron_names <- sapply(chron_ids, function(x){
  n = nt_read$sitename[nt_read$siteid == x]
  r = unique(n)
  r
})

site_names <- vector("character", length = nrow(nt_radiocarbon))

for(i in 1:length(chron_ids)){
  site_names[nt_radiocarbon$siteid == chron_ids[i]] = chron_names[chron_ids[i]]
}

nt_radiocarbon <- cbind(site_names, nt_radiocarbon)

###This is where we need to add the APD and manually-entered 14C data.

lpd.files <- list.files("APD/",".lpd",full.names = TRUE)

lpd.read <- readLipd(path = "APD/")

#We can read and navigate these with some careful use of lapply.

APD_names <- names(lpd.read) #Get all the record names.

#Let's get the lat/longs and filter.

APD_lat <- sapply(lpd.read, function(x){
  x$geo$latitude
})

APD_lon <- sapply(lpd.read, function(x){
  x$geo$longitude
})

APD_locations <- data.frame(as.numeric(APD_lon), as.numeric(APD_lat), row.names = names(APD_lon))
colnames(APD_locations) <- c("APD_lon", "APD_lat")

#Filter by mapping locations.

APD_sel <- APD_names[APD_locations$APD_lon > LON_RANGE[1] &
                     APD_locations$APD_lon < LON_RANGE[2] &
                     APD_locations$APD_lat > LAT_RANGE[1] & 
                     APD_locations$APD_lat < 8]

array_guide <- c(1:length(APD_names))[APD_locations$APD_lon > LON_RANGE[1] &
                                        APD_locations$APD_lon < LON_RANGE[2] &
                                        APD_locations$APD_lat > LAT_RANGE[1] & 
                                        APD_locations$APD_lat < 8]

#Making matrix of 14C results for all these cores.

APD_14C <- lapply(1:length(APD_sel), function(x){
  matrix()
})

chron_guide <- sapply(lpd.read, function(x){
  length(x$chronData)
})

for(i in 1:length(APD_sel)){
  if(chron_guide[array_guide[i]] == 1){
    yr14C <- lpd.read[[array_guide[i]]]$chronData[[chron_guide[array_guide[i]]]]$measurementTable[[2]]$`14C age (yr BP)`$values
    error <- lpd.read[[array_guide[i]]]$chronData[[chron_guide[array_guide[i]]]]$measurementTable[[2]]$error$values
    
    if(length(yr14C) == 0){
      yr14C <- lpd.read[[array_guide[i]]]$chronData[[chron_guide[array_guide[i]]]]$measurementTable[[2]]$`14C age (BP)`$values
    }
    
  }
  if(chron_guide[array_guide[i]] == 2){
    yr14C <- lpd.read[[array_guide[i]]]$chronData[[chron_guide[array_guide[i]]]]$measurementTable[[1]]$`14C age (yr BP)`$values
    error <- lpd.read[[array_guide[i]]]$chronData[[chron_guide[array_guide[i]]]]$measurementTable[[1]]$error$values
    
    if(length(yr14C) == 0){
      yr14C <- lpd.read[[array_guide[i]]]$chronData[[chron_guide[array_guide[i]]]]$measurementTable[[1]]$`14C age (cal yr BP)`$values
    }
    
  }
  if(chron_guide[array_guide[i]] == 0 | length(yr14C) == 0){
    print(paste0("No Chron for ", APD_sel[i]))
    yr14C <- c(NA, NA, NA)
    error <- c(NA, NA, NA)
  }
  
  if(is.character(yr14C) == TRUE){
    yr14C = yr14C[-c(1)]
    error = error[-c(1)]
  }
  
  site_name <- rep(APD_sel[i], length(yr14C))
  
  APD_14C[[i]] = data.frame(yr14C, error, site_name)
  
}

APD_14C <- as.array(APD_14C)

names(APD_14C) <- as.list(APD_sel) #This yields an array with every radiocarbon date and its error.

#Pretty sure we eventually just need a huge list of dates and errors.

for(i in 1:length(APD_14C)){
  if(i == 1){
    APD_all = APD_14C[[i]]
  } else {
    APD_all = rbind(APD_all, APD_14C[[i]])
  }
}

#This works, now we scrub it for random bullshit mistakes in APD

APD_repair <- APD_all[is.na(APD_all$yr14C) == FALSE, ]

all_14C <- as.numeric(c(APD_repair$yr14C,nt_radiocarbon$chroncontrolage))
all_err <- as.numeric(c(APD_repair$error,nt_radiocarbon$agelimitolder - nt_radiocarbon$agelimityounger))
all_nme <- c(APD_repair$site_name, nt_radiocarbon$site_names)

combined_sites <- data.frame(all_14C, all_err, all_nme)

#Use rcarbon functions to make SPD

nt.caldates <- calibrate(x = nt_radiocarbon$chroncontrolage,
                         errors = nt_radiocarbon$agelimitolder - nt_radiocarbon$agelimityounger,
                         calCurves = 'intcal20') #need a vector of curves by location!

all.caldates <- calibrate(x = combined_sites$all_14C,
                          errors = combined_sites$all_err,
                          calCurves = 'intcal20') #still need a fix here!

#Make bins

nt.bin <- binPrep(sites=nt_radiocarbon$siteid, 
                  ages = nt_radiocarbon$chroncontrolage, 
                  h = 100)

nt.spd.bins <- spd(nt.caldates, bins = nt.bin, timeRange = c(11700,100))

plot(nt.spd.bins)

comb.bin <- binPrep(sites = combined_sites$all_nme, 
                    ages = combined_sites$all_14C,
                    h = 100)

comb.spd.bins <- spd(all.caldates,
                     bins = comb.bin,
                     timeRange = c(11700, 100))

#Kdensity

nt.rdates <- sampleDates(nt.caldates, bins = nt.bin, nsim = 500, verbose = FALSE)

nt.ckde <- ckde(nt.rdates, timeRange = c(11700, 100), bw = 200)

plot(nt.ckde)

comb.rdates <- sampleDates(all.caldates, bins = comb.bin, nsim = 500, verbose = FALSE)

comb.ckde <- ckde(comb.rdates, timeRange = c(11700, 100), bw = 200)

plot(comb.ckde)

#Make groups and do group SPD

nt.stack <- stackspd(x = nt.caldates, 
                     group = nt_read$region, 
                     timeRange = c(11700, 100), 
                     bins = nt.bin, 
                     runm = 50, 
                     verbose = FALSE)

#Make fancy plot from Crema and Bevan (2021)

par(mfrow = c(2,2))
plot(nt.stack, type = "stacked", legend = FALSE)
plot(nt.stack, type = "lines")
plot(nt.stack, type = "multipanel", legend = FALSE )
plot(nt.stack, type = "proportion", legend = FALSE)
par(mfrow = c(1,1))

#This makes a sort of sensible figure.





