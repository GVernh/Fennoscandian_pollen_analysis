### Summed Probability Distribution ###

library(rcarbon)
library(stats)
library(ggplot2)
library(graphics)
library(dplyr)
library(zoo)

#load
human.footprint <- read.csv("human.footprint.csv")
human.footprint$SiteName <- sub("_", "", human.footprint$SiteName)
hfSubset <- human.footprint[grep("Sundfj", human.footprint$SiteName), ]
hfSubset$SiteName <- "Sundfj"
human.footprint <- human.footprint[-c(8246:8358),]
human.footprint <- rbind(human.footprint, hfSubset)

#FUNCTIONS
#make_interval_arc
make_interval_arc = function(dataset, interval){
  dataset$lower_ends = floor(dataset$weighted_mean/interval)*interval
  dataset$find <- c(1)
  dataset <- dataset %>%
    group_by(lower_ends) %>%
    summarise(
      mean_age = mean(weighted_mean),
      nr_finds = sum(find, na.rm = TRUE))
  dataset <- data.frame(lower_ends = seq(-100, dataset$lower_ends[nrow(dataset)], by = 100)) %>%
    full_join(dataset, by = "lower_ends") %>%
    mutate(NRfinds_new = na.approx(nr_finds, na.rm=FALSE))
  dataset$NRfinds <- round(dataset$NRfinds_new, digits=0)
  dataset <- dataset[-4]
  dataset <- dataset[-c(103:nrow(dataset)),]
  return(dataset)
}

#plot_arc
plot_arc = function(df){
  ggplot(df, aes(x = lower_ends)) + 
    scale_colour_manual(values=c(col1="orange"),labels=c("arc. finds")) +
    geom_line(aes(y = NRfinds,colour="col1")) + 
    labs(x = "Year BP",y = "Nr of finds",colour = "Legend")
}

#make_interval_spd
interval_spd = function(spd.bins){
  df <- data.frame(age=spd.bins$grid$calBP, PrDens=spd.bins$grid$PrDens)
  df$calBP = floor(df$age/100)*100
  df <- df %>% group_by(calBP) %>%
    summarise(SPD_med = median(PrDens)) #take the median. they also use the median in the rcarbon package.
  df <- arrange(df, -row_number())
  return(df)
}

#make_spd
arc_spd = function(arc, youngBP){
  arc.caldates=calibrate(x=arc$Age,errors=arc$Error,calCurves='intcal20') #calibrates the dates
  arc.spd = spd(arc.caldates,timeRange=c(9700,youngBP)) #aggregates (sums) calibrated dates
  arc.bins = binPrep(sites=arc$SiteName,ages=arc$Age,h=100) #binning in 100 years
  arc.spd.bins = spd(arc.caldates,bins=arc.bins,timeRange=c(9700,youngBP))
  #arc.bins.med=binMed(x = arc.caldates,bins=arc.bins)
  spd <- interval_spd(arc.spd.bins)
  return(spd)
}
#barCodes(arc.bins.med,yrng = c(0,0.01))

#null-model
nullmodel_spd = function(arc, youngBP){
  arc.caldates=calibrate(x=arc$Age,errors=arc$Error,calCurves='intcal20') #calibrates the dates
  arc.spd = spd(arc.caldates,timeRange=c(9700,youngBP)) #aggregates (sums) calibrated dates
  arc.bins = binPrep(sites=arc$SiteName,ages=arc$Age,h=100) #binning in 100 years
  arc.spd.bins = spd(arc.caldates,bins=arc.bins,timeRange=c(9700,youngBP))
  expnull <- modelTest(arc.caldates, errors=arc$Error, bins=arc.bins, nsim=100, timeRange=c(9700,youngBP), model="exponential",runm=100)
  return(expnull)
}


### NORTH_15 ###
#-------------------------------------------------------------------------------
arc.north <- subset(human.footprint, Lat>67.5)
spdN <- arc_spd(arc.north, 1400)
nullmodelN <- nullmodel_spd(arc.north, 1400)
saveRDS(nullmodelN, file="nullmodelN.Rdata")
nullmodelN <- readRDS("nullmodelN.Rdata")
plot.nullN <- plot(nullmodelN)
#sites <- unique(arc.north$Lat)
#sites <- unique(arc.north$Long)

### SOUTHEAST_911 ###
#-------------------------------------------------------------------------------
arc.southeast <- subset(human.footprint, Lat>60 & Lat<=65 & Long>20)
spdSE <- arc_spd(arc.southeast, 500)
nullmodelSE <- nullmodel_spd(arc.southeast, 500)
saveRDS(nullmodelSE, file="nullmodelSE.Rdata")
nullmodelSE <- readRDS("nullmodelSE.Rdata")
plot.nullSE <-  plot(nullmodelSE)

arc.southeast2 <- subset(human.withoutRussia, Lat>60 & Lat<=65 & Long>20)
spdSE2 <- arc_spd(arc.southeast, 500)
nullmodelSE2 <- nullmodel_spd(arc.southeast, 500)
saveRDS(nullmodelSE2, file="nullmodelSE2.Rdata")
nullmodelSE2 <- readRDS("nullmodelSE2.Rdata")
plot.nullSE2 <-  plot(nullmodelSE2)

### MIDWEST_2 ###
#-------------------------------------------------------------------------------
arc.midwest <- subset(human.footprint, Lat>60 & Long<=10)
spdMW <- arc_spd(arc.midwest, 700)
nullmodelMW <- nullmodel_spd(arc.midwest, 700)
saveRDS(nullmodelMW, file="nullmodelMW.Rdata")
nullmodelMW <- readRDS("nullmodelMW.Rdata")
plot.nullMW <- plot(nullmodelMW)

### MIDMID_47 ###
#-------------------------------------------------------------------------------
arc.midmid <- subset(human.footprint, Lat>60 & Lat<=65 & Long>10 & Long<=20)
spdMM <- arc_spd(arc.midmid, 700)
nullmodelMM <- nullmodel_spd(arc.midmid, 700)
saveRDS(nullmodelMM, file="nullmodelMM.Rdata")
nullmodelMM <- readRDS("nullmodelMM.Rdata")
plot.nullMM <- plot(nullmodelMM)

### SOUTHWEST_1 ###
#-------------------------------------------------------------------------------
arc.southwest <- subset(human.footprint, Lat <= 60 & Long<=10)
spdSW <- arc_spd(arc.southwest, 700)
nullmodelSW <- nullmodel_spd(arc.southwest, 700)
saveRDS(nullmodelSW, file="nullmodelSW.Rdata")
nullmodelSW <- readRDS("nullmodelSW.Rdata")
plot.nullSW <- plot(nullmodelSW, ylim=c(0,1))

### SOUTHMID_36 ###
#-------------------------------------------------------------------------------
arc.southmid <- subset(human.footprint, Lat<=60 & Long>10 & Long<=20)
spdSM <- arc_spd(arc.southmid, 1400)
nullmodelSM <- nullmodel_spd(arc.southmid, 1400)
saveRDS(nullmodelSM, file="nullmodelSM.Rdata")
nullmodelSM <- readRDS("nullmodelSM.Rdata")
plot.nullSM <- plot(nullmodelSM)


# PLOTS nullmodel #
par(mfrow=c(2,3))
plot(nullmodelN, ylim=c(0,1), main="North")
plot(nullmodelSE, ylim=c(0,1), main="SouthEast")
plot(nullmodelMW, ylim=c(0,1), main="MidWest")
plot(nullmodelMM, ylim=c(0,1), main="MidMid")
plot(nullmodelSW, ylim=c(0,1), main="SouthWest")
plot(nullmodelSM, ylim=c(0,1), main="SouthMid")
par(mfrow=c(1,1))
