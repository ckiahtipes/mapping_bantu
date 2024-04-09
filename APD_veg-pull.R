#Reading APD downloads in lipid format.

#Download lipidR package from github (https://github.com/nickmckay/lipdR) and install it.

#Libraries 

library(lipdR)

#Start with a list of files downloaded from APD website in .lpd format.

lpd.files <- list.files("APD/",".lpd",full.names = TRUE)

lpd.read <- readLipd(path = "APD/")

#Pull names and then use apply to disassemble .lpd objects.

APD_names <- names(lpd.read) #Get all the record names.

#Can retrieve info with sapply now...below we pull lat/lon for mapping.

APD_lat <- sapply(lpd.read, function(x){
  x$geo$latitude
})

APD_lon <- sapply(lpd.read, function(x){
  x$geo$longitude
})

#Assemble location data into a data frame.

APD_locations <- data.frame(as.numeric(APD_lon), as.numeric(APD_lat), row.names = names(APD_lon))
colnames(APD_locations) <- c("APD_lon", "APD_lat")

#Create array guide using APD_names object and extracted location data.

APD_sel <- APD_names[APD_locations$APD_lon > LON_RANGE[1] &
                       APD_locations$APD_lon < LON_RANGE[2] &
                       APD_locations$APD_lat > LAT_RANGE[1] & 
                       APD_locations$APD_lat < 8]

array_guide <- c(1:length(APD_names))[APD_locations$APD_lon > LON_RANGE[1] &
                                        APD_locations$APD_lon < LON_RANGE[2] &
                                        APD_locations$APD_lat > LAT_RANGE[1] & 
                                        APD_locations$APD_lat < 8]

#This retrieves the taxon names from a given record.

APD_taxa <- lapply(lpd.read, function(x){
  names(x$paleoData[[1]]$measurementTable[[1]])
})

#Need depths to establish size of matrices

APD_depths <- lapply(lpd.read, function(x){
  x$paleoData[[1]]$measurementTable[[1]]$'Depth (cm)'$values
})

#Write into new array.

APD_array <- lapply(1:length(lpd.read), function(x){
  matrix()
})

APD_array <- as.array(APD_array)
names(APD_array) <- names(lpd.read)

#Fill array with data

for(i in 1:length(lpd.read)){
  xrow = APD_depths[[i]]
  xcol = APD_taxa[[i]][5:length(APD_taxa[[i]])]
  pollen = matrix(nrow = length(xrow), ncol = length(xcol))
  C14yr = as.numeric(lpd.read[[i]]$chronData[[1]]$measurementTable[[1]][[5]][['values']])
  
  for(j in 5:length(APD_taxa[[i]])){
    
    pull = as.numeric(lpd.read[[i]]$paleoData[[1]]$measurementTable[[1]][[j]][['values']])
    
    pull[is.na(pull) == TRUE] = 0
    
    pollen[,j-4] = pull
    
  }
  
  #pollen[is.na(pollen)==TRUE] = 0 #Replace NAs with 0s.
  pollen = as.data.frame(pollen)
  
  row.names(pollen) <- C14yr
  colnames(pollen) <- xcol
  
  APD_array[[i]] = pollen
  
}

#This gives us an array with all pollen/spores.



#This pulls depths from a given record

lpd.read$MBI$paleoData[[1]]$measurementTable[[1]]$`Depth (cm)`$values















######OLD CODE CHUNK BELOW######

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
