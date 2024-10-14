### Summed Probability Distribution ###
# Modified 14/10/24 by G.Vernham
# TIME TO RUN: 20 mins on 1 processor

### Load and install packages ###
libs <- c("dplyr","ggplot2", "stats", "rcarbon", "zoo", "graphics", "stringr")

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

### Data ###
human.footprint <- read.csv("./Processed_data/human.footprint.csv")
human.withoutRussia <-read.csv("./Processed_data/human.withoutRussia.csv")
dir.create(file.path("./Processed_data/", "Footprint_calibration_results"), showWarnings = FALSE)

### DATA WRANGLING ###
#!!! CONSIDER MOVING TO "archeological_data.R" !!!
human.footprint$SiteName <- sub("_", "", human.footprint$SiteName)
hfSubset <- human.footprint[grep("Sundfj", human.footprint$SiteName), ]
hfSubset$SiteName <- "Sundfj"
human.footprint <- human.footprint[-grep("Sundfj", human.footprint$SiteName), ]
human.footprint <- rbind(human.footprint, hfSubset)

### LOAD FUNCTIONS ###

# !!! GRANT: Several of these functions are unused, consider removing? !!!
# interval_spd, arc_spd, nullmodel_spd are used n this script

source("./Functions/make_interval_arc.R")
source("./Functions/plot_arc.R")
source("./Functions/interval_spd.R")
source("./Functions/arc_spd.R")
source("./Functions/nullmodel_spd.R")
#barCodes(arc.bins.med,yrng = c(0,0.01))

### NORTH_15 ###
#-------------------------------------------------------------------------------
arc.north <- subset(human.footprint, Lat>67.5)
spdN <- arc_spd(arc.north, 1400)
nullmodelN <- nullmodel_spd(arc.north, 1400)
saveRDS(nullmodelN, file="./Processed_data/Footprint_calibration_results/nullmodelN.Rdata")
#plot.nullN <- plot(nullmodelN)
#sites <- unique(arc.north$Lat)
#sites <- unique(arc.north$Long)

### SOUTHEAST_911 ###
#-------------------------------------------------------------------------------
arc.southeast <- subset(human.footprint, Lat>60 & Lat<=65 & Long>20)
spdSE <- arc_spd(arc.southeast, 500)
nullmodelSE <- nullmodel_spd(arc.southeast, 500)
saveRDS(nullmodelSE, file="./Processed_data/Footprint_calibration_results/nullmodelSE.Rdata")
#plot.nullSE <-  plot(nullmodelSE)

# !!! Originally set up wrong. data (incl Russia) not used for the models. Possibly remove if not used. !!!
arc.southeast2 <- subset(human.withoutRussia, Lat>60 & Lat<=65 & Long>20)
spdSE2 <- arc_spd(arc.southeast2, 500)
nullmodelSE2 <- nullmodel_spd(arc.southeast2, 500)
saveRDS(nullmodelSE2, file="./Processed_data/Footprint_calibration_results/nullmodelSE2.Rdata")
#plot.nullSE2 <-  plot(nullmodelSE2)

### MIDWEST_2 ###
#-------------------------------------------------------------------------------
arc.midwest <- subset(human.footprint, Lat>60 & Long<=10)
spdMW <- arc_spd(arc.midwest, 700)
nullmodelMW <- nullmodel_spd(arc.midwest, 700)
saveRDS(nullmodelMW, file="./Processed_data/Footprint_calibration_results/nullmodelMW.Rdata")
#plot.nullMW <- plot(nullmodelMW)

### MIDMID_47 ###
#-------------------------------------------------------------------------------
arc.midmid <- subset(human.footprint, Lat>60 & Lat<=65 & Long>10 & Long<=20)
spdMM <- arc_spd(arc.midmid, 700)
nullmodelMM <- nullmodel_spd(arc.midmid, 700)
saveRDS(nullmodelMM, file="./Processed_data/Footprint_calibration_results/nullmodelMM.Rdata")
#plot.nullMM <- plot(nullmodelMM)

### SOUTHWEST_1 ###
#-------------------------------------------------------------------------------
arc.southwest <- subset(human.footprint, Lat <= 60 & Long<=10)
spdSW <- arc_spd(arc.southwest, 700)
nullmodelSW <- nullmodel_spd(arc.southwest, 700)
saveRDS(nullmodelSW, file="./Processed_data/Footprint_calibration_results/nullmodelSW.Rdata")
#plot.nullSW <- plot(nullmodelSW, ylim=c(0,1))

### SOUTHMID_36 ###
#-------------------------------------------------------------------------------
arc.southmid <- subset(human.footprint, Lat<=60 & Long>10 & Long<=20)
spdSM <- arc_spd(arc.southmid, 1400)
nullmodelSM <- nullmodel_spd(arc.southmid, 1400)
saveRDS(nullmodelSM, file="./Processed_data/Footprint_calibration_results/nullmodelSM.Rdata")
#plot.nullSM <- plot(nullmodelSM)