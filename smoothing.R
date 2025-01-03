### smoothing + model selection ###

libs <- c("ggplot2","tidyverse", "lubridate", "fpp2", "smooth", "zoo", "TTR", "vars")

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

#library(RRatepol)

dir.create(file.path("./Processed_data/", "Full_datasets"), showWarnings = FALSE)
#smoothing HOW TO MAKE MODEL SELECTION? median value = 7

# MODEL SELECTION - mean value = 4
# model selection according to the first value after the comma? --> 3
# select according to the full value. --> 1
# selecting according to the absolut lowest. --> 5

### NORTH

########
merged_data_N <-read.csv("./Processed_data/LCC_data/merged_data_N.csv")
spdN <- read.csv("./Processed_data/SPD_data/spdN.csv")
paleoviewN <- read.csv("./Processed_data/Climate/paleoviewN.csv") %>%
  dplyr::rename(calBP = Year..BP.) %>%
  dplyr::select(calBP, Area.Mean)

alldataN <- merge(merged_data_N, spdN, by="calBP", all.x=TRUE) %>%
  merge(paleoviewN, by="calBP", all.x=TRUE)
#write.csv(alldataN, file = "alldataN.csv", row.names = FALSE)

#model testing smoothing
sma(alldataN$conN, ic="BIC", h=0, interval="none") #6
sma(decN, ic="BIC", h=0, interval="none") #8
sma(wetwN, ic="BIC", h=0, interval="none") #3
sma(wetmN, ic="BIC", h=0, interval="none") #8
sma(pasN, ic="BIC", h=0, interval="none") #7
sma(araN, ic="BIC", h=0, interval="none") #2
sma(heaN, ic="BIC", h=0, interval="none") #7

#smoothing
alldataN$conNs <- TTR::SMA(alldataN$conN, n=6)
alldataN$decNs <- TTR::SMA(alldataN$decN, n=8)
alldataN$wetwNs <- SMA(alldataN$wetwN, n=3)
alldataN$wetmNs <- SMA(alldataN$wetmN, n=8)
alldataN$pasNs <- SMA(alldataN$pasN, n=7)
alldataN$araNs <- SMA(alldataN$araN, n=2)
alldataN$heaNs <- SMA(alldataN$heaN, n=7)
alldataNs <- alldataN[8:nrow(alldataN),]
write.csv(alldataNs, file = "alldataNs.csv", row.names = FALSE)
alldataNs = read.csv("alldataNs.csv")

#model testing VAR all dates
VARselect(alldataNs$conNs, lag.max=7, type="none") #8 7
VARselect(alldataNs$decNs, lag.max=7, type="none") #9 2
VARselect(alldataNs$wetwNs, lag.max=7, type="none") #6 1
VARselect(alldataNs$wetmNs, lag.max=7, type="none") #10 2
VARselect(alldataNs$pasNs, lag.max=7, type="none") #1 1
VARselect(alldataNs$araNs, lag.max=7, type="none") #7 5
VARselect(alldataNs$heaNs, lag.max=7, type="none") #8 1

#model testing VAR before farming
VARselect(alldataNb$conNs, lag.max=7, type="none") #7 7
VARselect(alldataNb$decNs, lag.max=7, type="none") #9 2
VARselect(alldataNb$wetwNs, lag.max=7, type="none") #6 1
VARselect(alldataNb$wetmNs, lag.max=7, type="none") #10 2
VARselect(alldataNb$pasNs, lag.max=7, type="none") #3 1
VARselect(alldataNb$araNs, lag.max=7, type="none") #7 1
VARselect(alldataNb$heaNs, lag.max=7, type="none") #8 1

#model testing VAR after farming
VARselect(alldataNa$conNs, lag.max=7, type="none") #8
VARselect(alldataNa$decNs, lag.max=7, type="none") #9
VARselect(alldataNa$wetwNs, lag.max=7, type="none") #6
VARselect(alldataNa$wetmNs, lag.max=7, type="none") #10
VARselect(alldataNa$pasNs, lag.max=7, type="none") #1
VARselect(alldataNa$araNs, lag.max=7, type="none") #7
VARselect(alldataNa$heaNs, lag.max=7, type="none") #8

### SOUTHEAST
#alldataSE <- data.frame(yearsBP=spdSE$calBP, SPD=spdSE$SPD_med, clim=paleoviewSE$Area.Mean[1:93], conSE=conSE[1:93],
#                        decSE=decSE[1:93], wetwSE=wetwSE[1:93],wetmSE=wetmSE[1:93],pasSE=pasSE[1:93],araSE=araSE[1:93],heaSE=heaSE[1:93])
#write.csv(alldataSE, file = "alldataSE.csv", row.names = FALSE)
alldataSE = read.csv("alldataSE.csv")

sma(conSE, ic="BIC", h=0, interval="none") #4
sma(decSE, ic="BIC", h=0, interval="none") #4
sma(wetwSE, ic="BIC", h=0, interval="none") #12
sma(wetmSE, ic="BIC", h=0, interval="none") #9
sma(pasSE, ic="BIC", h=0, interval="none") #2
sma(araSE, ic="BIC", h=0, interval="none") #1
sma(heaSE, ic="BIC", h=0, interval="none") #8

alldataSE$conSEs <- SMA(alldataSE$conSE, n=4)
alldataSE$decSEs <- SMA(alldataSE$decSE, n=4)
alldataSE$wetwSEs <- SMA(alldataSE$wetwSE, n=8)
alldataSE$wetmSEs <- SMA(alldataSE$wetmSE, n=8)
alldataSE$pasSEs <- SMA(alldataSE$pasSE, n=2)
alldataSE$araSEs <- SMA(alldataSE$araSE, n=1)
alldataSE$heaSEs <- SMA(alldataSE$heaSE, n=8)
alldataSEs <- alldataSE[8:nrow(alldataSE),]

VARselect(alldataSEs$conSEs, lag.max=7, type="none") #10 2
VARselect(alldataSEs$decSEs, lag.max=7, type="none") #10 1
VARselect(alldataSEs$wetwSEs, lag.max=7, type="none") #10 1
VARselect(alldataSEs$wetmSEs, lag.max=7, type="none") #2 2
VARselect(alldataSEs$pasSEs, lag.max=7, type="none") #10 10
VARselect(alldataSEs$araSEs, lag.max=7, type="none") #9 9
VARselect(alldataSEs$heaSEs, lag.max=7, type="none") #8 2


### MIDWEST
#alldataMW <- data.frame(yearsBP=spdMW$calBP, SPD=spdMW$SPD_med, clim=paleoviewMW$Area.Mean[1:91], conMW=conMW[1:91],
#                        decMW=decMW[1:91], wetwMW=wetwMW[1:91],wetmMW=wetmMW[1:91],pasMW=pasMW[1:91],araMW=araMW[1:91],heaMW=heaMW[1:91])
#write.csv(alldataMW, file = "alldataMW.csv", row.names = FALSE)
alldataMW = read.csv("alldataMW.csv")

sma(conMW, ic="BIC", h=0, interval="none") #101
sma(decMW, ic="BIC", h=0, interval="none") #7
sma(wetwMW, ic="BIC", h=0, interval="none") #5
sma(wetmMW, ic="BIC", h=0, interval="none") #8
sma(pasMW, ic="BIC", h=0, interval="none") #2
sma(araMW, ic="BIC", h=0, interval="none") #9
sma(heaMW, ic="BIC", h=0, interval="none") #8

alldataMW$conMWs <- SMA(alldataMW$conMW, n=8)
alldataMW$decMWs <- SMA(alldataMW$decMW, n=7)
alldataMW$wetwMWs <- SMA(alldataMW$wetwMW, n=5)
alldataMW$wetmMWs <- SMA(alldataMW$wetmMW, n=8)
alldataMW$pasMWs <- SMA(alldataMW$pasMW, n=2)
alldataMW$araMWs <- SMA(alldataMW$araMW, n=8)
alldataMW$heaMWs <- SMA(alldataMW$heaMW, n=8)
alldataMWs <- alldataMW[8:nrow(alldataMW),]

VARselect(alldataMWs$conMWs, lag.max=7, type="none") #9 9
VARselect(alldataMWs$decMWs, lag.max=7, type="none") #8 8
VARselect(alldataMWs$wetwMWs, lag.max=7, type="none") #8 1
VARselect(alldataMWs$wetmMWs, lag.max=7, type="none") #9 3
VARselect(alldataMWs$pasMWs, lag.max=7, type="none") #7 7
VARselect(alldataMWs$araMWs, lag.max=7, type="none") #2 2
VARselect(alldataMWs$heaMWs, lag.max=7, type="none") #9 3


### MIDMID
#alldataMM <- data.frame(yearsBP=spdMM$calBP, SPD=spdMM$SPD_med, clim=paleoviewMM$Area.Mean[1:91], conMM=conMM[1:91],
#                       decMM=decMM[1:91], wetwMM=wetwMM[1:91],wetmMM=wetmMM[1:91],pasMM=pasMM[1:91],araMM=araMM[1:91],heaMM=heaMM[1:91])
#write.csv(alldataMM, file = "alldataMM.csv", row.names = FALSE)
alldataMM = read.csv("alldataMM.csv")

sma(conMM, ic="BIC", h=0, interval="none") #5
sma(decMM, ic="BIC", h=0, interval="none") #5
sma(wetwMM, ic="BIC", h=0, interval="none") #4
sma(wetmMM, ic="BIC", h=0, interval="none") #8
sma(pasMM, ic="BIC", h=0, interval="none") #3
sma(araMM, ic="BIC", h=0, interval="none") #5
sma(heaMM, ic="BIC", h=0, interval="none") #3

alldataMM$conMMs <- SMA(alldataMM$conMM, n=5)
alldataMM$decMMs <- SMA(alldataMM$decMM, n=5)
alldataMM$wetwMMs <- SMA(alldataMM$wetwMM, n=4)
alldataMM$wetmMMs <- SMA(alldataMM$wetmMM, n=8)
alldataMM$pasMMs <- SMA(alldataMM$pasMM, n=3)
alldataMM$araMMs <- SMA(alldataMM$araMM, n=5)
alldataMM$heaMMs <- SMA(alldataMM$heaMM, n=3)
alldataMMs <- alldataMM[8:nrow(alldataMM),]

VARselect(alldataMMs$conMMs, lag.max=7, type="none") #6 1
VARselect(alldataMMs$decMMs, lag.max=7, type="none") #4 1
VARselect(alldataMMs$wetwMMs, lag.max=7, type="none") #10 1
VARselect(alldataMMs$wetmMMs, lag.max=7, type="none") #2 2
VARselect(alldataMMs$pasMMs, lag.max=7, type="none") #9 1
VARselect(alldataMMs$araMMs, lag.max=7, type="none") #6 6
VARselect(alldataMMs$heaMMs, lag.max=7, type="none") #6 6


### SOUTHWEST
#alldataSW <- data.frame(yearsBP=spdSW$calBP, SPD=spdSW$SPD_med, clim=paleoviewSW$Area.Mean[1:91], conSW=conSW[1:91],
#                        decSW=decSW[1:91], wetwSW=wetwSW[1:91],wetmSW=wetmSW[1:91],pasSW=pasSW[1:91],araSW=araSW[1:91],heaSW=heaSW[1:91])
#write.csv(alldataSW, file = "alldataSW.csv", row.names = FALSE)
alldataSW = read.csv("alldataSW.csv")

sma(conSM, ic="BIC", h=0, interval="none") #4
sma(decSM, ic="BIC", h=0, interval="none") #2
sma(wetwSM, ic="BIC", h=0, interval="none") #5
sma(wetmSM, ic="BIC", h=0, interval="none") #3
sma(pasSM, ic="BIC", h=0, interval="none") #2
sma(araSM, ic="BIC", h=0, interval="none") #2
sma(heaSM, ic="BIC", h=0, interval="none") #2

alldataSW$conSWs <- SMA(alldataSW$conSW, n=4)
alldataSW$decSWs <- SMA(alldataSW$decSW, n=2)
alldataSW$wetwSWs <- SMA(alldataSW$wetwSW, n=5)
alldataSW$wetmSWs <- SMA(alldataSW$wetmSW, n=3)
alldataSW$pasSWs <- SMA(alldataSW$pasSW, n=2)
alldataSW$araSWs <- SMA(alldataSW$araSW, n=2)
alldataSW$heaSWs <- SMA(alldataSW$heaSW, n=2)
alldataSWs <- alldataSW[8:nrow(alldataSW),]

VARselect(alldataSWs$conSWs, lag.max=7, type="none") #10 5
VARselect(alldataSWs$decSWs, lag.max=7, type="none") #7 3
VARselect(alldataSWs$wetwSWs, lag.max=7, type="none") #10 1
VARselect(alldataSWs$wetmSWs, lag.max=7, type="none") #7 4
VARselect(alldataSWs$pasSWs, lag.max=7, type="none") #5 3
VARselect(alldataSWs$araSWs, lag.max=7, type="none") #7 7
VARselect(alldataSWs$heaSWs, lag.max=7, type="none") #4 4


### SOUTHMID
alldataSM <- data.frame(yearsBP=spdSM$calBP, SPD=spdSM$SPD_med, clim=paleoviewSM$Area.Mean[1:84], conSM=conSM[1:84],
                        decSM=decSM[1:84], wetwSM=wetwSM[1:84],wetmSM=wetmSM[1:84],pasSM=pasSM[1:84],araSM=araSM[1:84],heaSM=heaSM[1:84])
#write.csv(alldataSM, file = "alldataSM.csv", row.names = FALSE)
alldataSM = read.csv("alldataSM.csv")

sma(conSW, ic="BIC", h=0, interval="none") #8
sma(decSW, ic="BIC", h=0, interval="none") #6
sma(wetwSW, ic="BIC", h=0, interval="none") #4
sma(wetmSW, ic="BIC", h=0, interval="none") #6
sma(pasSW, ic="BIC", h=0, interval="none") #6
sma(araSW, ic="BIC", h=0, interval="none") #2
sma(heaSW, ic="BIC", h=0, interval="none") #2

alldataSM$conSMs <- SMA(alldataSM$conSM, n=8)
alldataSM$decSMs <- SMA(alldataSM$decSM, n=6)
alldataSM$wetwSMs <- SMA(alldataSM$wetwSM, n=4)
alldataSM$wetmSMs <- SMA(alldataSM$wetmSM, n=6)
alldataSM$pasSMs <- SMA(alldataSM$pasSM, n=6)
alldataSM$araSMs <- SMA(alldataSM$araSM, n=2)
alldataSM$heaSMs <- SMA(alldataSM$heaSM, n=2)
alldataSMs <- alldataSM[8:nrow(alldataSM),]

VARselect(alldataSMs$conSMs, lag.max=7, type="none") #9 1
VARselect(alldataSMs$decSMs, lag.max=7, type="none") #7 7
VARselect(alldataSMs$wetwSMs, lag.max=7, type="none") #9 5
VARselect(alldataSMs$wetmSMs, lag.max=7, type="none") #8 2
VARselect(alldataSMs$pasSMs, lag.max=7, type="none") #7 7
VARselect(alldataSMs$araSMs, lag.max=7, type="none") #10 10
VARselect(alldataSMs$heaSMs, lag.max=7, type="none") #8 5

#mean BIC = 7 / mean BIC = 4


