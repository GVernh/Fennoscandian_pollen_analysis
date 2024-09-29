### Climatic data ###

library(dplyr)
library(ggplot2)

load("Temp12k_v1_0_0.RData")
climate = TS

#makeStandardDF
  climateDF = function(climate, sitenum)
  {
    data.frame(site=sitenum, long=climate[[sitenum]]$geo_longitude,
               lat=climate[[sitenum]]$geo_latitude, location=climate[[sitenum]]$geo_gcmdLocation, dataSet=climate[[sitenum]]$dataSetName)
  }


climate.df = NULL
for(i in 1:length(climate))
{
  namedf = climateDF(climate, i)
  if(i==1){
    climate.df = namedf
  }
  else{
    climate.df = dplyr::bind_rows(climate.df, namedf)
  }
}

#clean
climate.points <- subset(climate.df, long >= 4 & long < 42)
climate.points <- subset(climate.points, lat >= 55 & lat < 71)
#climate.points <- subset(climate.points, Age <= 15000)
Sweden <- climate.points[grep("Sweden", climate.points$location), ]
Finland <- climate.points[grep("Finland", climate.points$location), ]
Norway <- climate.points[grep("Norway", climate.points$location), ]
Russia <- climate.points[grep("Russia", climate.points$location), ]

climate.points = rbind(Sweden, Finland, Norway, Russia)

write.csv(climate.points, file = "climate.points.csv")
climate.points <- read.csv("climate.points.csv")
temp_longlat <- data.frame(climate.points$dataSet, climate.points$long, climate.points$lat)


#map
(sites <- data.frame(longitude = climate.points$long, latitude = climate.points$lat))

ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(4, 42), ylim = c(55, 71), expand = FALSE) +
  ggtitle("climatic.data")


# load LiPD file
#install.packages("devtools")
library(devtools)
#devtools::install_github("nickmckay/LiPD-Utilities", subdir = "R")
library(lipdR)

climateSets <- list(unique(climate.points$dataSet))

#setworkingdirectory (Session/...) to load the files
Lake.Shemesh.2001 <- readLipd(path = "850Lake.Shemesh.2001.lpd")
AgeroedsMosse.Nilsson.1964 <- readLipd(path = "AgeroedsMosse.Nilsson.1964.lpd")
AlanenLaanijarvi.Heinrichs.2005<- readLipd(path = "AlanenLaanijarvi.Heinrichs.2005.lpd")
Flarken.Berglund.1966<- readLipd(path = "Flarken.Berglund.1966.lpd")
Flarken.Seppa.2005<- readLipd(path = "Flarken.Seppa.2005.lpd")
Gilltjarnen.Antonsson.2006<- readLipd(path = "Gilltjarnen.Antonsson.2006.lpd")
Gloppsjon.Seppa.2009<- readLipd(path = "Gloppsjon.Seppa.2009.lpd")
Holtjaernen.Giesecke.2008<- readLipd(path = "Holtjaernen.Giesecke.2008.lpd")
Kansjon.EPD<- readLipd(path = "Kansjon.EPD.lpd")
Klotjaernen.Pollen.Sweden<- readLipd(path = "Klotjaernen.Pollen.Sweden.lpd")
#klotjarnen.Seppa.2009<- readLipd(path = "klotjarnen.Seppa.2009.lpd")
Njakajaure.Bigler.2006<- readLipd(path = "Njakajaure.Bigler.2006.lpd")
Njulla.Larocque.2004<- readLipd(path = "Njulla.Larocque.2004.lpd")
#sjuuodjijaure.Rosen.2001<- readLipd(path = "sjuuodjijaure.Rosen.2001.lpd")
Spaime.Hammarlund.2004<- readLipd(path = "Spaime.Hammarlund.2004.lpd")
Tibetanus.Hammarlund.2002<- readLipd(path = "Tibetanus.Hammarlund.2002.lpd")
Tornetrask.Grudd.2002<- readLipd(path = "Tornetrask.Grudd.2002.lpd")
Trehorningen.Antonsson.2007<- readLipd(path = "Trehorningen.Antonsson.2007.lpd")
Tsuolbmajavri.Korhola.2002<- readLipd(path = "Tsuolbmajavri.Korhola.2002.lpd")
Vuoksjavratje.Berntsson.2014<- readLipd(path = "Vuoksjavratje.Berntsson.2014.lpd")
VuolepNjakajaure.Heinrichs.2006<- readLipd(path = "VuolepNjakajaure.Heinrichs.2006.lpd")
#vuoskkujavri.Bigler.2002<- readLipd(path = "vuoskkujavri.Bigler.2002.lpd")
# CAN'T LOAD # Arapisto.Sarmaja-Korjonen.2007<- readLipd(path = "Arapisto.Sarmaja-Korjonen.2007.lpd")
Hirvijaervi.Luoto.2010<- readLipd(path = "Hirvijaervi.Luoto.2010.lpd")
Kaartlamminsuo.Rankama.1988<- readLipd(path = "Kaartlamminsuo.Rankama.1988.lpd")
Laihalampi.Giesecke.2008<- readLipd(path = "Laihalampi.Giesecke.2008.lpd")
Laihalampi.Heikkila.2003<- readLipd(path = "Laihalampi.Heikkila.2003.lpd")
Nautajarvi.Seppa.2009<- readLipd(path = "Nautajarvi.Seppa.2009.lpd")
Rukatunturi.Hicks.1985<- readLipd(path = "Rukatunturi.Hicks.1985.lpd")
Sokli.Shala.2017<- readLipd(path = "Sokli.Shala.2017.lpd")
Toskaljavri.Seppa.2002<- readLipd(path = "Toskaljavri.Seppa.2002.lpd")
Ylimysneva.Huttunen.1990<- readLipd(path = "Ylimysneva.Huttunen.1990.lpd")
Austerkjosen.Seppa.2009<- readLipd(path = "Austerkjosen.Seppa.2009.lpd")
Bjornfjell.Brooks.2006<- readLipd(path = "Bjornfjell.Brooks.2006.lpd")
#brurskardstjorni.Velle.2005<- readLipd(path = "brurskardstjorni.Velle.2005.lpd")
Dalene.Seppa.2009<- readLipd(path = "Dalene.Seppa.2009.lpd")
Dalmutladdo.Bjune.2004<- readLipd(path = "Dalmutladdo.Bjune.2004.lpd")
Donvold.Nilssen.1983<- readLipd(path = "Donvold.Nilssen.1983.lpd")
FauskeCave.Linge.2009<- readLipd(path = "FauskeCave.Linge.2009.lpd")
Flotatjonn.Seppa.2009<- readLipd(path = "Flotatjonn.Seppa.2009.lpd")
Gammelheimvatnet.Seppa.2009<- readLipd(path = "Gammelheimvatnet.Seppa.2009.lpd")
Grostjorn.Eide.2009<- readLipd(path = "Grostjorn.Eide.2009.lpd")
Haugtjern.Eide.2009<- readLipd(path = "Haugtjern.Eide.2009.lpd")
Holebudalen.Seppa.2009<- readLipd(path = "Holebudalen.Seppa.2009.lpd")
Isbenttjonn.Seppa.2009<- readLipd(path = "Isbenttjonn.Seppa.2009.lpd")
Kinnshaugen.Seppa.2009<- readLipd(path = "Kinnshaugen.Seppa.2009.lpd")
Lapland.Helama.2009<- readLipd(path = "Lapland.Helama.2009.lpd")
Liltlvatn.Seppa.2009<- readLipd(path = "Liltlvatn.Seppa.2009.lpd")
Myrvatn.Seppa.2009<- readLipd(path = "Myrvatn.Seppa.2009.lpd")
Ratasjoen.Velle.2005<- readLipd(path = "Ratasjoen.Velle.2005.lpd")
Reiarsdalvatnet.Seppa.2009<- readLipd(path = "Reiarsdalvatnet.Seppa.2009.lpd")
Soylegrotta.Lauritzen.1999<- readLipd(path = "Soylegrotta.Lauritzen.1999.lpd")
Svanavatnet.Bjune.2008<- readLipd(path = "Svanavatnet.Bjune.2008.lpd")
#CAN'T LOAD #Svartvatnet-Norway.Seppa.2009<- readLipd(path = "Svartvatnet-Norway.Seppa.2009.lpd")
Tiavatnet.Seppa.2009<- readLipd(path = "Tiavatnet.Seppa.2009.lpd")
Topptjonna.Paus.2011<- readLipd(path = "Topptjonna.Paus.2011.lpd")
Trettetjorn.Bjune.2005<- readLipd(path = "Trettetjorn.Bjune.2005.lpd")
VestreOykjamyrtorn.EPD<- readLipd(path = "VestreOykjamyrtorn.EPD.lpd")
VestreOykjamytjorn.Velle.2005<- readLipd(path = "VestreOykjamytjorn.Velle.2005.lpd")
#vikjordvatnet.Balascio.2012<- readLipd(path = "vikjordvatnet.Balascio.2012.lpd")
Berkut.Ilyashuk.2005<- readLipd(path = "Berkut.Ilyashuk.2005.lpd")
Chuna.Jones.2005<- readLipd(path = "Chuna.Jones.2005.lpd")
KP2.Seppa.2009<- readLipd(path = "KP2.Seppa.2009.lpd")
Medvedevskoe.Nazarova.2018<- readLipd(path = "Medvedevskoe.Nazarova.2018.lpd")
#NOT FENNOSCANDIA# StaroselskyMoch.Novenko.2018<- readLipd(path = "StaroselskyMoch.Novenko.2018.lpd")
Yarnyshnoe.Seppa.2008<- readLipd(path = "Yarnyshnoe.Seppa.2008.lpd")

all_temp_sites = list("850Lake.Shemesh.2001"=Lake.Shemesh.2001,AgeroedsMosse.Nilsson.1964=AgeroedsMosse.Nilsson.1964,AlanenLaanijarvi.Heinrichs.2005=AlanenLaanijarvi.Heinrichs.2005,Flarken.Berglund.1966=Flarken.Berglund.1966,
                      Flarken.Seppa.2005=Flarken.Seppa.2005,Gilltjarnen.Antonsson.2006=Gilltjarnen.Antonsson.2006,Gloppsjon.Seppa.2009=Gloppsjon.Seppa.2009,Holtjaernen.Giesecke.2008=Holtjaernen.Giesecke.2008,Kansjon.EPD=Kansjon.EPD,
                      Klotjaernen.Pollen.Sweden=Klotjaernen.Pollen.Sweden,Njakajaure.Bigler.2006=Njakajaure.Bigler.2006,
                      Njulla.Larocque.2004=Njulla.Larocque.2004,Spaime.Hammarlund.2004=Spaime.Hammarlund.2004,
                      Tibetanus.Hammarlund.2002=Tibetanus.Hammarlund.2002,Tornetrask.Grudd.2002=Tornetrask.Grudd.2002,Trehorningen.Antonsson.2007=Trehorningen.Antonsson.2007,
                      Tsuolbmajavri.Korhola.2002=Tsuolbmajavri.Korhola.2002,Vuoksjavratje.Berntsson.2014=Vuoksjavratje.Berntsson.2014,VuolepNjakajaure.Heinrichs.2006=VuolepNjakajaure.Heinrichs.2006,
                      Hirvijaervi.Luoto.2010=Hirvijaervi.Luoto.2010,
                      Kaartlamminsuo.Rankama.1988=Kaartlamminsuo.Rankama.1988,Laihalampi.Giesecke.2008=Laihalampi.Giesecke.2008,Laihalampi.Heikkila.2003=Laihalampi.Heikkila.2003,
                      Nautajarvi.Seppa.2009=Nautajarvi.Seppa.2009,Rukatunturi.Hicks.1985=Rukatunturi.Hicks.1985,Sokli.Shala.2017=Sokli.Shala.2017,
                      Toskaljavri.Seppa.2002=Toskaljavri.Seppa.2002,Ylimysneva.Huttunen.1990=Ylimysneva.Huttunen.1990,Austerkjosen.Seppa.2009=Austerkjosen.Seppa.2009,
                      Bjornfjell.Brooks.2006=Bjornfjell.Brooks.2006,Dalene.Seppa.2009=Dalene.Seppa.2009,
                      Dalmutladdo.Bjune.2004=Dalmutladdo.Bjune.2004,Donvold.Nilssen.1983=Donvold.Nilssen.1983,FauskeCave.Linge.2009=FauskeCave.Linge.2009,
                      Flotatjonn.Seppa.2009=Flotatjonn.Seppa.2009,Gammelheimvatnet.Seppa.2009=Gammelheimvatnet.Seppa.2009,Grostjorn.Eide.2009=Grostjorn.Eide.2009,
                      Haugtjern.Eide.2009=Haugtjern.Eide.2009,Holebudalen.Seppa.2009=Holebudalen.Seppa.2009,Isbenttjonn.Seppa.2009=Isbenttjonn.Seppa.2009,
                      Kinnshaugen.Seppa.2009=Kinnshaugen.Seppa.2009,Lapland.Helama.2009=Lapland.Helama.2009,Liltlvatn.Seppa.2009=Liltlvatn.Seppa.2009,
                      Myrvatn.Seppa.2009=Myrvatn.Seppa.2009,Ratasjoen.Velle.2005=Ratasjoen.Velle.2005,Reiarsdalvatnet.Seppa.2009=Reiarsdalvatnet.Seppa.2009,
                      Soylegrotta.Lauritzen.1999=Soylegrotta.Lauritzen.1999,Svanavatnet.Bjune.2008=Svanavatnet.Bjune.2008,
                      Tiavatnet.Seppa.2009=Tiavatnet.Seppa.2009,Topptjonna.Paus.2011=Topptjonna.Paus.2011,Trettetjorn.Bjune.2005=Trettetjorn.Bjune.2005,
                      VestreOykjamyrtorn.EPD=VestreOykjamyrtorn.EPD,VestreOykjamytjorn.Velle.2005=VestreOykjamytjorn.Velle.2005,
                      Berkut.Ilyashuk.2005=Berkut.Ilyashuk.2005,Chuna.Jones.2005=Chuna.Jones.2005,KP2.Seppa.2009=KP2.Seppa.2009,
                      Medvedevskoe.Nazarova.2018=Medvedevskoe.Nazarova.2018,Yarnyshnoe.Seppa.2008=Yarnyshnoe.Seppa.2008)

saveRDS(all_temp_sites, file = "all_temp_sites.RDS")
all_temp_sites <- readRDS("all_temp_sites.RDS")

# make final list #
climate_list <- function(climate.points, all_temp_sites, list_name) {
  temp_list <- list(list(site = all_temp_sites[[list_name]]$dataSetName,
                         age = all_temp_sites[[list_name]]$paleoData[[1]]$measurementTable[[1]]$age$values,
                         temp = all_temp_sites[[list_name]]$paleoData[[1]]$measurementTable[[1]]$temperature$values,
                         long= all_temp_sites[[list_name]]$geo$longitude, lat=all_temp_sites[[list_name]]$geo$latitude,
                         calibration= all_temp_sites[[list_name]]$paleoData[[1]]$measurementTable[[1]]$temperature$calibration$method))
}

temp.list <- NULL
for (i in 1:length(all_temp_sites)) {
  namelist <- climate_list(climate.points,all_temp_sites, i)
  if(i=="Lake.Shemesh.2001"){
    temp.list <- namelist
  }
  else{
    temp.list <- append(temp.list, namelist)
  }
}

# plot single sites
plot(temp.list[[15]]$age, temp.list[[15]]$temp, type = "l")

plot(temp.list[[1]]$age, temp.list[[1]]$temp, type = "l", ylim = c(1,12))
par(new=TRUE)
plot(temp.list[[2]]$age, temp.list[[2]]$temp, type = "l", ylim = c(1,12))


#calibration methods used
calibration.list = NULL
for (i in 1:length(all_temp_sites)) {
  namelist <- list(temp.list[[i]]$calibration)
  if(i==1){
    calibration.list <- namelist
  }
  else{
    calibration.list <- append(calibration.list, namelist)
  }
}

#archiveType
archiveType = NULL
for (i in 1:length(all_temp_sites)) {
  namelist <- list(all_temp_sites[[i]]$archiveType)
  if(i==1){
    archiveType <- namelist
  }
  else{
    archiveType <- append(archiveType, namelist)
  }
}


saveRDS(temp.list, file = "temp.list.RDS")
temp.list <- readRDS("temp.list.RDS")


# adapt the temperatures #
#North-summer
temp.list[[32]]$temp <- all_temp_sites$Dalmutladdo.Bjune.2004$paleoData[[1]]$measurementTable[[2]]$temperature$values
temp.list[[32]]$age <- all_temp_sites$Dalmutladdo.Bjune.2004$paleoData[[1]]$measurementTable[[2]]$age$values

temp.list[[33]]$temp <- all_temp_sites[["Donvold.Nilssen.1983"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperature-1"]][["values"]]

temp.list[[17]]$temp <- all_temp_sites[["Tsuolbmajavri.Korhola.2002"]][["paleoData"]][[2]][["measurementTable"]][[1]][["temperature"]][["values"]]
temp.list[[17]]$age <- all_temp_sites[["Tsuolbmajavri.Korhola.2002"]][["paleoData"]][[2]][["measurementTable"]][[1]][["age"]][["values"]]

temp.list[[19]]$temp <- all_temp_sites[["VuolepNjakajaure.Heinrichs.2006"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperature-2"]][["values"]]


#SouthWest-summer
temp.list[[52]]$temp <- all_temp_sites[["VestreOykjamyrtorn.EPD"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperature-1"]][["values"]]

temp.list[[39]]$temp <- all_temp_sites[["Holebudalen.Seppa.2009"]][["paleoData"]][[1]][["measurementTable"]][[2]][["temperature"]][["values"]]
temp.list[[39]]$age <- all_temp_sites[["Holebudalen.Seppa.2009"]][["paleoData"]][[1]][["measurementTable"]][[2]][["age"]][["values"]]


#SouthMid_annual
temp.list[[2]]$temp <- all_temp_sites[["AgeroedsMosse.Nilsson.1964"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperatureComposite"]][["values"]]
temp.list[[4]]$temp <- all_temp_sites[["Flarken.Berglund.1966"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperatureComposite"]][["values"]]
temp.list[[9]]$temp <- all_temp_sites[["Kansjon.EPD"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperatureComposite"]][["values"]]


#SoutheEast_annual
temp.list[[21]]$temp <- all_temp_sites[["Kaartlamminsuo.Rankama.1988"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperatureComposite"]][["values"]]

temp.list[[28]]$temp <- all_temp_sites[["Ylimysneva.Huttunen.1990"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperatureComposite"]][["values"]]

temp.list[[22]]$temp <- all_temp_sites[["Laihalampi.Giesecke.2008"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperatureComposite"]][["values"]]


