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

# Load raw data
load("./Raw_Data/bigdf_familynames.Rda")
time_ID = bigdf_familynames[c("dataset_ID", "meantimes")] # subset time/site data

# Apply data quality control and compute relative abundance
pollen_relative_abun <- bigdf_familynames %>%
  {bigdf_familynames[!duplicated(colnames(bigdf_familynames))]} %>% # "Cyperaceae" is duplicated in the data.
  dplyr::select(-colnames(time_ID)) %>%
  as.matrix() %>%
  compute_relative_abundance() %>%
  cbind(time_ID) %>%
  dplyr::select(colnames(time_ID), everything())





############## IGNORE ###########################

## CREATING SLICED DATA TO SHOW CO-AUTHORS

splice_raw_data = bigdf_familynames[1:20, 1:20]
time_ID = splice_raw_data[c("dataset_ID", "meantimes")]
splice_raw_data = splice_raw_data[3:20]
splice_data = compute_relative_abundance(splice_raw_data)
splice_data = cbind(time_ID, splice_data)
splice_raw_data = cbind(time_ID, splice_raw_data)

x = rowSums(splice_raw_data, na.rm = T)

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

