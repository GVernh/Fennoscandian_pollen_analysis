### Human footprint data ###

library(dplyr)
library(ggplot2)
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")

p3k14c <- read.csv("p3k14c_original.csv")
footprint = p3k14c

footprint <- subset(footprint, Long >= 4 & Long < 42)
footprint <- subset(footprint, Lat >= 55 & Lat < 71)
footprint <- subset(footprint, Age <= 15000)
Sweden <- subset(footprint, Country == "Sweden")
Sweden <- Sweden[!(Sweden$SiteName=="Auve"),]
Finland <- subset(footprint, Country == "Finland")
Norway <- subset(footprint, Country == "Norway")
Russia <- subset(footprint, Country == "Russian Federation")
Russia <- Russia[!(Russia$SiteName=="Sakhtysh 2" | Russia$SiteName=="Sungir Soungir Sunghir" |
                     Russia$SiteName=="Stanovoe 4" | Russia$SiteName=="Ivanovskoye" | Russia$SiteName=="Jagerhaushohle" |
                     Russia$SiteName=="Starnska Skala" | Russia$SiteName=="Stanovoje" | Russia$SiteName=="Zolotoruce" |
                     Russia$SiteName=="Turginovo 5" | Russia$SiteName=="Vergara" | Russia$SiteName=="Berendeevo 2a" |
                     Russia$SiteName=="Ivanovskoe 3" | Russia$SiteName=="Podoli V" | Russia$SiteName=="Zolotovka"),]
human.footprint = rbind(Sweden, Finland, Norway, Russia)
human.withoutRussia = rbind(Sweden, Finland, Norway)

write.csv(human.footprint, file = "human.footprint.csv")
human.footprint <- read.csv("human.footprint.csv")


#footprint <- subset(footprint, Lat >= 61 & Lat < 67,
#                    select=c(ID, Weight))


Material = data.frame(unique(human.footprint$Material))
write.csv(Material, file = "human.footprint.Material.csv")
Material <- read.csv("human.footprint.Material.csv")

Age = data.frame(human.footprint$Age)
Age = Age[order(Age$human.footprint.Age),]
Age = as.data.frame(Age)
write.csv(Age, file = "human.footprint.Age.csv")

archaeological.data = data.frame(human.footprint$Age, human.footprint$Material)
archaeological.data = archaeological.data[order(Age$Age),]
write.csv(archaeological.data, file = "archaeological.data.csv")



### map footprint ###

#install.packages("sf")
library(ggplot2)
theme_set(theme_bw()) #classic dark on light theme
library(sf)

#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
library(rnaturalearth)
library(rnaturalearthdata)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(4, 42), ylim = c(55, 71), expand = FALSE)


(sites <- data.frame(longitude = human.footprint$Long, latitude = human.footprint$Lat))
(sites <- data.frame(longitude = human.withoutRussia$Long, latitude = human.withoutRussia$Lat))

ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(4, 42), ylim = c(55, 71), expand = FALSE) +
  ggtitle("archaeological.data")

ggplot(human.footprint, aes(x = Age)) +
  geom_line(aes(y = Age, colour="red"))

ggplot(human.footprint, aes(x=Age, y=Lat)) + geom_point() +
  ggtitle("archaeological.data") +
  labs(x = "Year BP",y = "Latitude")


### calibration ###
#-------------------------------------------------------------------------------

#Bchron
#install.packages("Bchron")
library(Bchron)

calibrated.arch.data = NULL
for(row in 1:nrow(human.footprint))
  {
  namedf = BchronCalibrate(
    ages = human.footprint[row, "Age"],
    ageSds = human.footprint[row, "Error"],
    calCurves = "intcal20",
    ids = human.footprint[row, "LabID"],
    allowOutside = TRUE)
  if(i=="1"){
    calibrated.arch.data = namedf
  }
  else{
    calibrated.arch.data = append(calibrated.arch.data, namedf)
  }
}

#cal <- BchronCalibrate(
#  ages = human.footprint$Age,
#  ageSds = human.footprint$Error,
#  calCurves = "intcal20",
#  ids = human.footprint$LabID,
#  allowOutside = TRUE)

saveRDS(calibrated.arch.data, file="calibrated.arch.data.RData")
calibrated.arch.data = readRDS("calibrated.arch.data.RData")

# calculate weighted mean
mean.arch.age = function(calibrated.arch.data, site) {
  ages = calibrated.arch.data[[site]]$ageGrid
  densities = calibrated.arch.data[[site]]$densities
  weighted_mean = weighted.mean(ages, densities)
  mean.arch.age = data.frame(LabID=site, weighted_mean = weighted_mean)
}

weighted_mean_df = NULL
for(names in names(calibrated.arch.data))
{
  namedf = mean.arch.age(calibrated.arch.data, names)
  if(names=="AA 1841"){
    weighted_mean_df = namedf
  }
  else{
    weighted_mean_df = dplyr::bind_rows(weighted_mean_df, namedf)
  }
}

weighted_mean_df$long = human.footprint$Long
weighted_mean_df$lat = human.footprint$Lat
weighted_mean_df$Material = human.footprint$Material

write.csv(weighted_mean_df, file = "archaeological_cal.csv")
archaeological_cal <- read.csv("archaeological_cal.csv")


# summed probability distribution #
#install.packages("rcarbon")
library(rcarbon)

arc15_ages <- arc15$weighted_mean
arc15.spd <- spd(arc15_ages)


