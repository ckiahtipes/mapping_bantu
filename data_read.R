#R script for reading and combining Neotoma, APD, and manual entry 14C/location data.

library(neotoma2)
library(lipdR)

#Read main data file with locations, coll units, and database list.

all_read <- read.csv("data/MB_data-read.csv", header = TRUE)

#Read manual entries.

manual <- read.csv("manual/manual_entries.csv", header = TRUE)

#Set long/lat range.

LAT_RANGE=c(-9,9)
LON_RANGE=c(6,25)

#Call sites, datasets, and downloads from neotoma.

nt_sites <- get_sites(all_read$NEOTOMA_ID[all_read$Source == "Neotoma"])

site_latlong <- data.frame(all_read$LON, all_read$LAT, row.names = all_read$COLL_UNIT)

nt_sites_detail <- summary(nt_sites)

nt_datasets <- get_datasets(nt_sites, all_data = TRUE)

nt_download <- get_downloads(nt_datasets, all_data = TRUE)

#Use download to pull chronologies.

nt_chron <- chroncontrols(nt_download)

#Pull out the radiocarbon dates.

nt_radiocarbon <- nt_chron[nt_chron$chroncontroltype == "Radiocarbon" & is.na(nt_chron$chroncontroltype) == FALSE, ]

#nt_notread <- nt_chron[nt_chron$chroncontroltype == "Radiocarbon, calibrated" & is.na(nt_chron$chroncontroltype) == FALSE, ]

#nt_nrd_ids <- unique(nt_notread$siteid)

#nrd_names <- sapply(nt_nrd_ids, function(x){
#  n = nt_read$sitename[nt_read$siteid == x]
#  r = unique(n)
#  r
#})

#Here we extract the names of the sites for which we returned chronologies.

chron_ids <- unique(all_read$COLL_UNIT[all_read$Source == "Neotoma"])
chron_names <- sapply(chron_ids, function(x){
  n = all_read$COLL_UNIT[all_read$COLL_UNIT == x]
  r = n[is.na(n) == FALSE]
  r = unique(r)
  r
})

site_names <- vector("character", length = nrow(nt_radiocarbon))
nt_names <- all_read$Map.Locale[all_read$Source == "Neotoma"]
nt_ids <- all_read$NEOTOMA_ID[all_read$Source == "Neotoma"]

for(i in 1:length(nt_names)){
  site_names[nt_radiocarbon$siteid == nt_ids[i]] = nt_names[i]
}

nt_radiocarbon <- cbind(site_names, nt_radiocarbon)

#Reading APD data from the website, 48 total files. 

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

for(i in 1:length(APD_sel)){ #How hard is it to add retrieval of the lab #s?
  if(i == 1){
    APD_noread <- vector("character") #Create object to put no read results into. 
  }

  
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
    if(length(APD_noread) == 0){ #Trying to make a list of sites that aren't read.
      APD_noread = APD_sel[i]
    } else {
      new_list = c(APD_noread, APD_sel[i])
      APD_noread = new_list
    } #End of custom if statement for listing unread sites. 
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

#Now we combine the data into a single table.

for(i in 1:length(APD_14C)){
  if(i == 1){
    APD_all = APD_14C[[i]]
  } else {
    APD_all = rbind(APD_all, APD_14C[[i]])
  }
}

#This works, now we scrub it for random errors from APD

APD_repair <- APD_all[is.na(APD_all$yr14C) == FALSE, ]

all_14C <- as.numeric(c(APD_repair$yr14C,nt_radiocarbon$chroncontrolage, manual$X14C.age))
all_err <- as.numeric(c(APD_repair$error,(nt_radiocarbon$agelimitolder - nt_radiocarbon$agelimityounger)/2, manual$Error))
all_nme <- c(APD_repair$site_name, nt_radiocarbon$site_names, manual$Record)

#Pull manual entries.

combined_sites <- data.frame(all_14C, all_err, all_nme)

write.csv(combined_sites, "data/combined_14Craw.csv")

#Last, we manually add the sites we do not have in the list using the published dates.

#Now, we save a master mapping file (by site) and a master 14C file (by date).

APD_rloc <- APD_locations[is.na(APD_all$yr14C) == FALSE, ]

APD_rloc <- sapply(unique(APD_repair$site_name), function(x){
  pull = APD_locations[row.names(APD_locations) == x,]
  pull[1,]
})

APD_rloc = as.data.frame(t(APD_rloc))
APD_nlst <- row.names(APD_rloc)
APD_rloc <- apply(APD_rloc, 2, as.numeric)
row.names(APD_rloc) = APD_nlst

colnames(APD_rloc) = c("LON","LAT")

nt_rloc <- sapply(unique(chron_names), function(x){
  pull = all_read[all_read$COLL_UNIT == x,c("LON", "LAT")]
  pull[1,]
})

nt_rloc = as.data.frame(t(nt_rloc))
colnames(nt_rloc) = c("LON","LAT")

mn_rloc = data.frame(all_read$LON[all_read$Source == "Manual entry"],
                     all_read$LAT[all_read$Source == "Manual entry"],
                     row.names = all_read$COLL_UNIT[all_read$Source == "Manual entry"])
colnames(mn_rloc) = c("LON", "LAT")

combined_locations <- rbind(nt_rloc, APD_rloc, mn_rloc) #How to filter by where we have results?
all_sitenames <- row.names(combined_locations)
combined_locations <- apply(combined_locations, 2, as.numeric)
row.names(combined_locations) <- all_sitenames

write.csv(APD_rloc, "APD/APD_read.csv", row.names = TRUE)
write.csv(combined_locations, "data/combined_14Clocations.csv")


