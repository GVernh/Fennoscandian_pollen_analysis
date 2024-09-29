# Install/load libraries
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

# Source functions
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