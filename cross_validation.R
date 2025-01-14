### cross validation ###
# Last modifed by G.Vernham 14/01/2025

# LOAD LIBRARIES ----
rm(list=ls())

libs <- c("dplyr","modelr", "glm2", "timetk", "purrr")

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

# LOAD DATA ----
alldataNs = read.csv("./Processed_data/Full_datasets/alldataNs.csv")
alldataMMs = read.csv("./Processed_data/Full_datasets/alldataMMs.csv")
alldataMWs = read.csv("./Processed_data/Full_datasets/alldataMWs.csv")
alldataSEs = read.csv("./Processed_data/Full_datasets/alldataSEs.csv")
alldataSMs = read.csv("./Processed_data/Full_datasets/alldataSMs.csv")
alldataSWs = read.csv("./Processed_data/Full_datasets/alldataSWs.csv")

# Subset: BEFORE THE ONSET OF FARMING -----
alldataNb <- alldataNs[which(alldataNs$yearsBP >= 2500),]
alldataSEb <- alldataSEs[which(alldataSEs$yearsBP >= 4000),]
alldataMWb <- alldataMWs[which(alldataMWs$yearsBP >= 3200),]
alldataMMb <- alldataMMs[which(alldataMMs$yearsBP >= 3200),]
alldataSWb <- alldataSWs[which(alldataSWs$yearsBP >= 6000),]
alldataSMb <- alldataSMs[which(alldataSMs$yearsBP >= 6000),]

# Subset: AFTER THE ONSET OF FARMING -----

alldataNa <- alldataNs[which(alldataNs$yearsBP < 2500),]
alldataSEa <- alldataSEs[which(alldataSEs$yearsBP < 4000),]
alldataMWa <- alldataMWs[which(alldataMWs$yearsBP < 3200),]
alldataMMa <- alldataMMs[which(alldataMMs$yearsBP < 3200),]
alldataSWa <- alldataSWs[which(alldataSWs$yearsBP < 6000),]
alldataSMa <- alldataSMs[which(alldataSMs$yearsBP < 6000),]

# FUNCTIONS ----
source("./Functions/Cross_validation.R")
source("./Functions/Create_vectors.R")

# Parametres ----
nfolds = 2
maxlag = 7

#results

#whole holocene
yearsBP = alldataNs$yearsBP
crossvalidation(conNs, SPD.N, clim.N)#2
crossvalidation(decNs, SPD.N, clim.N) #1
crossvalidation(wetwNs, SPD.N, clim.N) #1
crossvalidation(wetmNs, SPD.N, clim.N) #2
crossvalidation(pasNs, SPD.N, clim.N) #1
crossvalidation(araNs, SPD.N, clim.N) #1
crossvalidation(heaNs, SPD.N, clim.N) #2

yearsBP = alldataSEs$yearsBP
crossvalidation(conSEs, SPD.SE, clim.SE) #4
crossvalidation(decSEs, SPD.SE, clim.SE) #6
crossvalidation(wetwSEs, SPD.SE, clim.SE) #1
crossvalidation(wetmSEs, SPD.SE, clim.SE) #4
crossvalidation(pasSEs, SPD.SE, clim.SE) #1
crossvalidation(araSEs, SPD.SE, clim.SE) #3
crossvalidation(heaSEs, SPD.SE, clim.SE) #1

yearsBP = alldataMWs$yearsBP
crossvalidation(conMWs, SPD.MW, clim.MW) #1
crossvalidation(decMWs, SPD.MW, clim.MW) #3
crossvalidation(wetwMWs, SPD.MW, clim.MW) #2
crossvalidation(wetmMWs, SPD.MW, clim.MW) #1
crossvalidation(pasMWs, SPD.MW, clim.MW) #1
crossvalidation(araMWs, SPD.MW, clim.MW) #1
crossvalidation(heaMWs, SPD.MW, clim.MW) #2

yearsBP = alldataMMs$yearsBP
crossvalidation(conMMs, SPD.MM, clim.MM) #4
crossvalidation(decMMs, SPD.MM, clim.MM) #4
crossvalidation(wetwMMs, SPD.MM, clim.MM) #4
crossvalidation(wetmMMs, SPD.MM, clim.MM) #1
crossvalidation(pasMMs, SPD.MM, clim.MM) #3
crossvalidation(araMMs, SPD.MM, clim.MM) #4
crossvalidation(heaMMs, SPD.MM, clim.MM) #6

yearsBP = alldataSWs$yearsBP
crossvalidation(conSWs, SPD.SW, clim.SW) #1
crossvalidation(decSWs, SPD.SW, clim.SW) #1
crossvalidation(wetwSWs, SPD.SW, clim.SW) #1
crossvalidation(wetmSWs, SPD.SW, clim.SW) #1
crossvalidation(pasSWs, SPD.SW, clim.SW) #5
crossvalidation(araSWs, SPD.SW, clim.SW) #3
crossvalidation(heaSWs, SPD.SW, clim.SW) #3

yearsBP = alldataSMs$yearsBP
crossvalidation(conSMs, SPD.SM, clim.SM) #6
crossvalidation(decSMs, SPD.SM, clim.SM) #1
crossvalidation(wetwSMs, SPD.SM, clim.SM) #1
crossvalidation(wetmSMs, SPD.SM, clim.SM) #1
crossvalidation(pasSMs, SPD.SM, clim.SM) #1
crossvalidation(araSMs, SPD.SM, clim.SM) #5
crossvalidation(heaSMs, SPD.SM, clim.SM) #1