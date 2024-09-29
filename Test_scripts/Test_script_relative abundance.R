# Load libraries
libs <- c("dplyr")

installed_libs <- libs %in% rownames(
  installed.packages())

if (any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs]
  )
}

invisible(lapply(
  libs,
  library,
  character.only = T
))
rm(list=ls())

# Load functions
source("./Functions/Compute_relative_abundance.R")

# Load cleaned raw data
load("./Raw_Data/bigdf_familynames.Rda")


bigdf_familynames = bigdf_familynames[!duplicated(colnames(bigdf_familynames))] # Species "Cyperaceae" is duplicated in the data.
time_ID = bigdf_familynames[c("dataset_ID", "meantimes")] # Subset data to be removed from relative abundance calculation
raw_pollen = bigdf_familynames %>% select(-colnames(time_ID))
raw_pollen = as.matrix(raw_pollen)

pollen_relative_abun = compute_relative_abundance(raw_pollen)
pollen_relative_abun = cbind(time_ID, data.frame(pollen_relative_abun)) # re-bind time and site data





############## IGNORE ###########################

## CREATING SLICED DATA TO SHOW CO-AUTHORS
splice_data = pollen_relative_abun[1:20, 1:20]
splice_raw_data = bigdf_familynames[1:20, 1:20]

write.csv(splice_data, "./Test_scripts/relative_abun_slice.csv")
write.csv(splice_raw_data, "./Test_scripts/raw_data_slice.csv")

#################################

total_abundance =  rowSums(x, na.rm = T) # Calculate total abundance per site

for (i in 1:nrow(x)){
  b = total_abundance[i]
    for (y in 1:ncol(x)) {
      z = x[i,y]
      Percent_abun = z/b
      x[i,y] = Percent_abun
    }
}

