###############################################################################
# Occupational Mobility Network for Europe
# INET Oxford Project
# EU-LFS data
# Martin Lukac @mblukac
###############################################################################
source("libraries.R")
# data/LFS/Omnibus Data Folder

setwd("/Users/martinlukac/Desktop/Academic Projects/git-Occupational Mobility Network/")
read_in <- function(country){
  #country <- "AT"
  setwd(paste0("data/LFS/Omnibus Data Folder/", country))
  list.files() %>%
    llply(read.csv, header = T) %>%
    llply(subset, select = c(COUNTRY,
                             REFYEAR,
                             ISCO1D,
                             ISCO3D, 
                             ISCOPR1D, 
                             ISCOPR3D,
                             COEFF
    )) %>%
    rbindlist(fill = TRUE) -> temp
  return(temp)
}

AT <- read_in("AT")