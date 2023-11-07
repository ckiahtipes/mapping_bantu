#Using neotoma to pull 14C dates and run SPDs

library(neotoma2)
library(rcarbon)
library(lipdR)

#Call sites, datasets, and downloads from neotoma.

nt_read <- read.csv("neotoma_all.csv", header = TRUE, row.names = "X")

nt_sites <- get_sites(nt_sites$siteid)

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

###This is where we need to add the APD and manually-entered 14C data.
#Do we use lipidR?

lpd.files <- list.files("APD/",".lpd",full.names = TRUE)

lpd.read <- readLipd(path = "APD/")

#This reads some chronological values.

lpd.read$BAL$chronData[[1]][[2]][[1]]$`14C age (yr BP)`$values #This works for some files

lpd.read$`B-YANGA`$chronData[[1]][[1]][[2]]$`14C age (yr BP)`$values #This works for others

#WTF there's multiple organizational styles?
#Some must have an original and a corrected chronology?
#Think we need to check length of object at "chronData" level, then get 14C results.

lpd.read$BAL$chronData[[2]]$measurementTable[[1]]$`14C age (yr BP)`$values
lpd.read$BAL$chronData[[2]]$measurementTable[[1]]$filetype
lpd.read$`B-YANGA`$chronData[[1]]$measurementTable[[1]]$`Sample age (cal yr BP)`
lpd.read$`B-YANGA`$chronData[[1]]$measurementTable[[2]]$`14C age (yr BP)`$values
lpd.read$`B-YANGA`$chronData[[1]]$measurementTable[[1]]$filetype

#Okay so can paste0 help us?

APD_names <- names(lpd.read) #Get all the record names.

###PICK IT UP HERE


#Use rcarbon functions to make SPD

nt.caldates <- calibrate(x = nt_radiocarbon$chroncontrolage,
                         errors = nt_radiocarbon$agelimitolder - nt_radiocarbon$agelimityounger,
                         calCurves = 'intcal20') #need a vector of curves by location!
#Make bins

nt.bin <- binPrep(sites=nt_radiocarbon$siteid, ages = nt_radiocarbon$chroncontrolage, h = 100)

nt.spd.bins <- spd(nt.caldates, bins = nt.bin, timeRange = c(11700,100))

plot(nt.spd.bins)

#Kdensity

nt.rdates <- sampleDates(nt.caldates, bins = nt.bin, nsim = 500, verbose = FALSE)

nt.ckde <- ckde(nt.rdates, timeRange = c(11700, 100), bw = 200)

plot(nt.ckde)

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





