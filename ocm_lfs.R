###############################################################################
# Occupational Mobility Network for Europe
# INET Oxford Project
# EU-LFS data
# Martin Lukac @mblukac
###############################################################################
source("libraries.R")

setwd("/Users/martinlukac/Desktop/Academic Projects/git-Occupational Mobility Network/")

read_in <- function(country){
  
  # Save old WD and set to location of files
  oldwd <- file.path(getwd())
  setwd(file.path(paste0(oldwd, "/data/LFS/Omnibus Data Folder/", country)))
  
  # Get a files list and load in all country files
  list.files() %>%
    llply(read.csv, header = T) %>%
    llply(subset, select = c(COUNTRY,
                             REFYEAR,
                             IS881D,
                             IS883D,
                             ISCO1D,
                             ISCO3D, 
                             COEFF
    )) %>%
    rbindlist(fill = TRUE) -> country_dataset
  
  # reset WD
  setwd(file.path(oldwd))
  
  # Return the final dataset
  return(country_dataset)
}

# Load it all country codes from dir names
all_cntrs <- list.files("data/LFS/Omnibus Data Folder")
# Read all country datafiles in
for(i in 1:length(all_cntrs)){
  message(paste0("=== Reading in ", country, " === Progress ", 
                 i, "/", length(all_cntrs), " ==="))
  assign(all_cntrs[i], read_in(all_cntrs[i]))
}


# Problem with change in ISCO coding
at_isco <- names(table(AT$ISCO3D))
at_is88 <- names(table(AT$IS883D))

intersect(at_isco, at_is88)
setdiff(at_isco, at_is88)


## Try BHPS and Understanding Society
us1 <- import("/Users/martinlukac/Desktop/Projects/OLD Academic Projects/BHPS and Understanding Society Data/Understanding Society/spss/spss19/a_empstat.sav")
us2 <- import("/Users/martinlukac/Desktop/Projects/OLD Academic Projects/BHPS and Understanding Society Data/Understanding Society/spss/spss19/e_empstat.sav")

bhps1 <- import("/Users/martinlukac/Desktop/Projects/OLD Academic Projects/BHPS and Understanding Society Data/BHPS/spss/spss12/ajobhist.sav")

