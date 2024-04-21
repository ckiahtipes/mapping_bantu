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

#Assemble location data into a data frame

APD_locations <- data.frame(as.numeric(APD_lon), as.numeric(APD_lat), row.names = names(APD_lon))
colnames(APD_locations) <- c("APD_lon", "APD_lat")

#This retrieves the taxon names from a given record.

APD_taxa <- lapply(lpd.read, function(x){
  names(x$paleoData[[1]]$measurementTable[[1]])
})

#Need depths to establish size of matrices

APD_depths <- lapply(lpd.read, function(x){
  x$paleoData[[1]]$measurementTable[[1]]$'Depth (cm)'$values
})

#Random errors in depths, some entries include "depth (cm)" in the "values" vector. Need to clean up.

for(i in 1:length(APD_depths)){
  if(is.character(APD_depths[[i]]) == TRUE){ #Look for characters
    APD_depths[[i]] = as.numeric(APD_depths[[i]][2:length(APD_depths[[i]])]) #Cut character sting and make numeric
    
  
    } else {
    
  }
}

#Write into new array.

APD_array <- lapply(1:length(lpd.read), function(x){
  lapply(1:3, function(y){
    matrix()
  })
})

APD_array <- as.array(APD_array)
names(APD_array) <- names(lpd.read)
lapply(APD_array, function(x){
  names(x) <- c("pollen_spores","sample_depths","chronology")
})

#Fill array with data

for(i in 1:length(lpd.read)){
  xrow = APD_depths[[i]]
  xcol = APD_taxa[[i]][5:length(APD_taxa[[i]])]
  pollen = matrix(nrow = length(xrow), ncol = length(xcol))
  C14yr = data.frame(as.numeric(lpd.read[[i]]$chronData[[1]]$measurementTable[[1]][[5]][['values']]),
                     as.numeric(lpd.read[[i]]$chronData[[1]]$measurementTable[[1]][[4]][['values']]))
  
  colnames(C14yr) = c("cal_yrBP","depth")
  
  for(j in 5:length(APD_taxa[[i]])){
    if(APD_names[[i]] == "MONOUN"){
      pull = lpd.read[[i]]$paleoData[[1]]$measurementTable[[1]][[j]][['values']]
      pull = pull[2:length(pull)]
      
    } else {
      
      pull = as.numeric(lpd.read[[i]]$paleoData[[1]]$measurementTable[[1]][[j]][['values']])
    
    }
    
    pull[is.na(pull) == TRUE] = 0
    
    pollen[,j-4] = pull
    
  }
  
  #pollen[is.na(pollen)==TRUE] = 0 #Replace NAs with 0s.
  pollen = as.data.frame(pollen)
  
  #row.names(pollen) <- C14yr
  #colnames(pollen) <- xcol
  
  colnames(pollen) = xcol
  
  APD_array[[i]][["pollen_spores"]] = pollen
  APD_array[[i]][["sample_depths"]] = xrow
  APD_array[[i]][["chronology"]] = C14yr
  
}

#Now we have an array with the records and component parts.

for(i in 1:length(APD_array)){
  write.csv(APD_array[[i]]$pollen_spores, paste0('APD_write',APD_names[i],'_pollen-spores.csv'))
  write.csv(APD_array[[i]]$chronology, paste0('APD_write',APD_names[i],'_chronology.csv'))
}













