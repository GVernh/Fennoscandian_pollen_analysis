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
# selecting according to the absolute lowest. --> 5

# TO DO (GRANT): Run smoothing on all data, write data to directory.
### NORTH

########
merged_data_N <-read.csv("./Processed_data/LCC_data/merged_data_N.csv")
spdN <- read.csv("./Processed_data/SPD_data/spdN.csv")
paleoviewN <- read.csv("./Processed_data/Climate/paleoviewN.csv") %>%
  dplyr::rename(calBP = Year..BP.) %>%
  dplyr::select(calBP, Area.Mean)

alldataN <- merge(merged_data_N, spdN, by="calBP", all.x=TRUE) %>%
  merge(paleoviewN, by="calBP", all.x=TRUE) %>%
  dplyr::rename(clim = Area.Mean) %>%
  dplyr::rename(SPD = SPD_med) %>%
  dplyr::rename(yearsBP = calBP) %>%
  .[order(.$yearsBP, decreasing = TRUE), ] # Data by years in descending order

#model testing smoothing
sma(alldataN$conN, ic="BIC", h=0, interval="none") #6
sma(alldataN$decN, ic="BIC", h=0, interval="none") #8
sma(alldataN$wetwN, ic="BIC", h=0, interval="none") #3
sma(alldataN$wetmN, ic="BIC", h=0, interval="none") #8
sma(alldataN$pasN, ic="BIC", h=0, interval="none") #7
sma(alldataN$araN, ic="BIC", h=0, interval="none") #2
sma(alldataN$heaN, ic="BIC", h=0, interval="none") #7

#smoothing
alldataN$conNs <- TTR::SMA(alldataN$conN, n=6)
alldataN$decNs <- TTR::SMA(alldataN$decN, n=8)
alldataN$wetwNs <- TTR::SMA(alldataN$wetwN, n=3)
alldataN$wetmNs <- TTR::SMA(alldataN$wetmN, n=8)
alldataN$pasNs <- TTR::SMA(alldataN$pasN, n=7)
alldataN$araNs <- TTR::SMA(alldataN$araN, n=2)
alldataN$heaNs <- TTR::SMA(alldataN$heaN, n=7)
alldataNs <- alldataN[8:nrow(alldataN),]
write.csv(alldataNs, file = "./Processed_data/Full_datasets/alldataNs.csv", row.names = FALSE)

#model testing VAR all dates
VARselect(alldataNs$conNs, lag.max=7, type="none") #8 7
VARselect(alldataNs$decNs, lag.max=7, type="none") #9 2
VARselect(alldataNs$wetwNs, lag.max=7, type="none") #6 1
VARselect(alldataNs$wetmNs, lag.max=7, type="none") #10 2
VARselect(alldataNs$pasNs, lag.max=7, type="none") #1 1
VARselect(alldataNs$araNs, lag.max=7, type="none") #7 5
VARselect(alldataNs$heaNs, lag.max=7, type="none") #8 1

# GRANT: These variables aren't created until the cross validation script is run.
# #model testing VAR before farming
# VARselect(alldataNb$conNs, lag.max=7, type="none") #7 7
# VARselect(alldataNb$decNs, lag.max=7, type="none") #9 2
# VARselect(alldataNb$wetwNs, lag.max=7, type="none") #6 1
# VARselect(alldataNb$wetmNs, lag.max=7, type="none") #10 2
# VARselect(alldataNb$pasNs, lag.max=7, type="none") #3 1
# VARselect(alldataNb$araNs, lag.max=7, type="none") #7 1
# VARselect(alldataNb$heaNs, lag.max=7, type="none") #8 1
# 
# #model testing VAR after farming
# VARselect(alldataNa$conNs, lag.max=7, type="none") #8
# VARselect(alldataNa$decNs, lag.max=7, type="none") #9
# VARselect(alldataNa$wetwNs, lag.max=7, type="none") #6
# VARselect(alldataNa$wetmNs, lag.max=7, type="none") #10
# VARselect(alldataNa$pasNs, lag.max=7, type="none") #1
# VARselect(alldataNa$araNs, lag.max=7, type="none") #7
# VARselect(alldataNa$heaNs, lag.max=7, type="none") #8

### SOUTHEAST
merged_data_SE <-read.csv("./Processed_data/LCC_data/merged_data_SE.csv")
spdSE <- read.csv("./Processed_data/SPD_data/spdSE.csv")
paleoviewSE <- read.csv("./Processed_data/Climate/paleoviewSE.csv") %>%
  dplyr::rename(calBP = Year..BP.) %>%
  dplyr::select(calBP, Area.Mean)

alldataSE <- merge(merged_data_SE, spdSE, by="calBP", all.x=TRUE) %>%
  merge(paleoviewSE, by="calBP", all.x=TRUE) %>%
  dplyr::rename(clim = Area.Mean) %>%
  dplyr::rename(SPD = SPD_med) %>%
  dplyr::rename(yearsBP = calBP)%>%
  .[order(.$yearsBP, decreasing = TRUE), ]

sma(alldataSE$conSE, ic="BIC", h=0, interval="none") #4
sma(alldataSE$decSE, ic="BIC", h=0, interval="none") #4
sma(alldataSE$wetwSE, ic="BIC", h=0, interval="none") #12
sma(alldataSE$wetmSE, ic="BIC", h=0, interval="none") #9
sma(alldataSE$pasSE, ic="BIC", h=0, interval="none") #2
sma(alldataSE$araSE, ic="BIC", h=0, interval="none") #1
sma(alldataSE$heaSE, ic="BIC", h=0, interval="none") #8

alldataSE$conSEs <- SMA(alldataSE$conSE, n=4)
alldataSE$decSEs <- SMA(alldataSE$decSE, n=4)
alldataSE$wetwSEs <- SMA(alldataSE$wetwSE, n=8)
alldataSE$wetmSEs <- SMA(alldataSE$wetmSE, n=8)
alldataSE$pasSEs <- SMA(alldataSE$pasSE, n=2)
alldataSE$araSEs <- SMA(alldataSE$araSE, n=1)
alldataSE$heaSEs <- SMA(alldataSE$heaSE, n=8)
alldataSEs <- alldataSE[8:nrow(alldataSE),]
write.csv(alldataSEs, file = "./Processed_data/Full_datasets/alldataSEs.csv", row.names = FALSE)

VARselect(alldataSEs$conSEs, lag.max=7, type="none") #10 2
VARselect(alldataSEs$decSEs, lag.max=7, type="none") #10 1
VARselect(alldataSEs$wetwSEs, lag.max=7, type="none") #10 1
VARselect(alldataSEs$wetmSEs, lag.max=7, type="none") #2 2
VARselect(alldataSEs$pasSEs, lag.max=7, type="none") #10 10
VARselect(alldataSEs$araSEs, lag.max=7, type="none") #9 9
VARselect(alldataSEs$heaSEs, lag.max=7, type="none") #8 2


### MIDWEST
merged_data_MW <-read.csv("./Processed_data/LCC_data/merged_data_MW.csv")
spdMW <- read.csv("./Processed_data/SPD_data/spdMW.csv")
paleoviewMW <- read.csv("./Processed_data/Climate/paleoviewMW.csv") %>%
  dplyr::rename(calBP = Year..BP.) %>%
  dplyr::select(calBP, Area.Mean)

alldataMW <- merge(merged_data_MW, spdMW, by="calBP", all.x=TRUE) %>%
  merge(paleoviewMW, by="calBP", all.x=TRUE) %>%
  dplyr::rename(clim = Area.Mean) %>%
  dplyr::rename(SPD = SPD_med) %>%
  dplyr::rename(yearsBP = calBP)%>%
  .[order(.$yearsBP, decreasing = TRUE), ]

sma(alldataMW$conMW, ic="BIC", h=0, interval="none") #101
sma(alldataMW$decMW, ic="BIC", h=0, interval="none") #7
sma(alldataMW$wetwMW, ic="BIC", h=0, interval="none") #5
sma(alldataMW$wetmMW, ic="BIC", h=0, interval="none") #8
sma(alldataMW$pasMW, ic="BIC", h=0, interval="none") #2
sma(alldataMW$araMW, ic="BIC", h=0, interval="none") #9
sma(alldataMW$heaMW, ic="BIC", h=0, interval="none") #8

alldataMW$conMWs <- SMA(alldataMW$conMW, n=8)
alldataMW$decMWs <- SMA(alldataMW$decMW, n=7)
alldataMW$wetwMWs <- SMA(alldataMW$wetwMW, n=5)
alldataMW$wetmMWs <- SMA(alldataMW$wetmMW, n=8)
alldataMW$pasMWs <- SMA(alldataMW$pasMW, n=2)
alldataMW$araMWs <- SMA(alldataMW$araMW, n=8)
alldataMW$heaMWs <- SMA(alldataMW$heaMW, n=8)
alldataMWs <- alldataMW[8:nrow(alldataMW),]
write.csv(alldataMWs, file = "./Processed_data/Full_datasets/alldataMWs.csv", row.names = FALSE)

VARselect(alldataMWs$conMWs, lag.max=7, type="none") #9 9
VARselect(alldataMWs$decMWs, lag.max=7, type="none") #8 8
VARselect(alldataMWs$wetwMWs, lag.max=7, type="none") #8 1
VARselect(alldataMWs$wetmMWs, lag.max=7, type="none") #9 3
VARselect(alldataMWs$pasMWs, lag.max=7, type="none") #7 7
VARselect(alldataMWs$araMWs, lag.max=7, type="none") #2 2
VARselect(alldataMWs$heaMWs, lag.max=7, type="none") #9 3


### MIDMID

merged_data_MM <-read.csv("./Processed_data/LCC_data/merged_data_MM.csv")
spdMM <- read.csv("./Processed_data/SPD_data/spdMM.csv")
paleoviewMM <- read.csv("./Processed_data/Climate/paleoviewMM.csv") %>%
  dplyr::rename(calBP = Year..BP.) %>%
  dplyr::select(calBP, Area.Mean)

alldataMM <- merge(merged_data_MM, spdMM, by="calBP", all.x=TRUE) %>%
  merge(paleoviewMM, by="calBP", all.x=TRUE) %>%
  dplyr::rename(clim = Area.Mean) %>%
  dplyr::rename(SPD = SPD_med) %>%
  dplyr::rename(yearsBP = calBP)%>%
  .[order(.$yearsBP, decreasing = TRUE), ]

sma(alldataMM$conMM, ic="BIC", h=0, interval="none") #5
sma(alldataMM$decMM, ic="BIC", h=0, interval="none") #5
sma(alldataMM$wetwMM, ic="BIC", h=0, interval="none") #4
sma(alldataMM$wetmMM, ic="BIC", h=0, interval="none") #8
sma(alldataMM$pasMM, ic="BIC", h=0, interval="none") #3
sma(alldataMM$araMM, ic="BIC", h=0, interval="none") #5
sma(alldataMM$heaMM, ic="BIC", h=0, interval="none") #3

alldataMM$conMMs <- SMA(alldataMM$conMM, n=5)
alldataMM$decMMs <- SMA(alldataMM$decMM, n=5)
alldataMM$wetwMMs <- SMA(alldataMM$wetwMM, n=4)
alldataMM$wetmMMs <- SMA(alldataMM$wetmMM, n=8)
alldataMM$pasMMs <- SMA(alldataMM$pasMM, n=3)
alldataMM$araMMs <- SMA(alldataMM$araMM, n=5)
alldataMM$heaMMs <- SMA(alldataMM$heaMM, n=3)
alldataMMs <- alldataMM[8:nrow(alldataMM),]
write.csv(alldataMMs, file = "./Processed_data/Full_datasets/alldataMMs.csv", row.names = FALSE)

VARselect(alldataMMs$conMMs, lag.max=7, type="none") #6 1
VARselect(alldataMMs$decMMs, lag.max=7, type="none") #4 1
VARselect(alldataMMs$wetwMMs, lag.max=7, type="none") #10 1
VARselect(alldataMMs$wetmMMs, lag.max=7, type="none") #2 2
VARselect(alldataMMs$pasMMs, lag.max=7, type="none") #9 1
VARselect(alldataMMs$araMMs, lag.max=7, type="none") #6 6
VARselect(alldataMMs$heaMMs, lag.max=7, type="none") #6 6


### SOUTHWEST
merged_data_SW <-read.csv("./Processed_data/LCC_data/merged_data_SW.csv")
spdSW <- read.csv("./Processed_data/SPD_data/spdSW.csv")
paleoviewSW <- read.csv("./Processed_data/Climate/paleoviewSW.csv") %>%
  dplyr::rename(calBP = Year..BP.) %>%
  dplyr::select(calBP, Area.Mean)

alldataSW <- merge(merged_data_SW, spdSW, by="calBP", all.x=TRUE) %>%
  merge(paleoviewSW, by="calBP", all.x=TRUE) %>%
  dplyr::rename(clim = Area.Mean) %>%
  dplyr::rename(SPD = SPD_med) %>%
  dplyr::rename(yearsBP = calBP)%>%
  .[order(.$yearsBP, decreasing = TRUE), ]

sma(alldataSW$conSW, ic="BIC", h=0, interval="none") #4
sma(alldataSW$decSW, ic="BIC", h=0, interval="none") #2
sma(alldataSW$wetwSW, ic="BIC", h=0, interval="none") #5
sma(alldataSW$wetmSW, ic="BIC", h=0, interval="none") #3
sma(alldataSW$pasSW, ic="BIC", h=0, interval="none") #2
sma(alldataSW$araSW, ic="BIC", h=0, interval="none") #2
sma(alldataSW$heaSW, ic="BIC", h=0, interval="none") #2

alldataSW$conSWs <- SMA(alldataSW$conSW, n=4)
alldataSW$decSWs <- SMA(alldataSW$decSW, n=2)
alldataSW$wetwSWs <- SMA(alldataSW$wetwSW, n=5)
alldataSW$wetmSWs <- SMA(alldataSW$wetmSW, n=3)
alldataSW$pasSWs <- SMA(alldataSW$pasSW, n=2)
alldataSW$araSWs <- SMA(alldataSW$araSW, n=2)
alldataSW$heaSWs <- SMA(alldataSW$heaSW, n=2)
alldataSWs <- alldataSW[8:nrow(alldataSW),]
write.csv(alldataSWs, file = "./Processed_data/Full_datasets/alldataSWs.csv", row.names = FALSE)

VARselect(alldataSWs$conSWs, lag.max=7, type="none") #10 5
VARselect(alldataSWs$decSWs, lag.max=7, type="none") #7 3
VARselect(alldataSWs$wetwSWs, lag.max=7, type="none") #10 1
VARselect(alldataSWs$wetmSWs, lag.max=7, type="none") #7 4
VARselect(alldataSWs$pasSWs, lag.max=7, type="none") #5 3
VARselect(alldataSWs$araSWs, lag.max=7, type="none") #7 7
VARselect(alldataSWs$heaSWs, lag.max=7, type="none") #4 4


### SOUTHMID
merged_data_SM <-read.csv("./Processed_data/LCC_data/merged_data_SM.csv")
spdSM <- read.csv("./Processed_data/SPD_data/spdSM.csv")
paleoviewSM <- read.csv("./Processed_data/Climate/paleoviewSM.csv") %>%
  dplyr::rename(calBP = Year..BP.) %>%
  dplyr::select(calBP, Area.Mean)

alldataSM <- merge(merged_data_SM, spdSM, by="calBP", all.x=TRUE) %>%
  merge(paleoviewSM, by="calBP", all.x=TRUE) %>%
  dplyr::rename(clim = Area.Mean) %>%
  dplyr::rename(SPD = SPD_med) %>%
  dplyr::rename(yearsBP = calBP)%>%
  .[order(.$yearsBP, decreasing = TRUE), ]

sma(alldataSM$conSM, ic="BIC", h=0, interval="none") #8
sma(alldataSM$decSM, ic="BIC", h=0, interval="none") #6
sma(alldataSM$wetwSM, ic="BIC", h=0, interval="none") #4
sma(alldataSM$wetmSM, ic="BIC", h=0, interval="none") #6
sma(alldataSM$pasSM, ic="BIC", h=0, interval="none") #6
sma(alldataSM$araSM, ic="BIC", h=0, interval="none") #2
sma(alldataSM$heaSM, ic="BIC", h=0, interval="none") #2

alldataSM$conSMs <- SMA(alldataSM$conSM, n=8)
alldataSM$decSMs <- SMA(alldataSM$decSM, n=6)
alldataSM$wetwSMs <- SMA(alldataSM$wetwSM, n=4)
alldataSM$wetmSMs <- SMA(alldataSM$wetmSM, n=6)
alldataSM$pasSMs <- SMA(alldataSM$pasSM, n=6)
alldataSM$araSMs <- SMA(alldataSM$araSM, n=2)
alldataSM$heaSMs <- SMA(alldataSM$heaSM, n=2)
alldataSMs <- alldataSM[8:nrow(alldataSM),]
write.csv(alldataSMs, file = "./Processed_data/Full_datasets/alldataSMs.csv", row.names = FALSE)

VARselect(alldataSMs$conSMs, lag.max=7, type="none") #9 1
VARselect(alldataSMs$decSMs, lag.max=7, type="none") #7 7
VARselect(alldataSMs$wetwSMs, lag.max=7, type="none") #9 5
VARselect(alldataSMs$wetmSMs, lag.max=7, type="none") #8 2
VARselect(alldataSMs$pasSMs, lag.max=7, type="none") #7 7
VARselect(alldataSMs$araSMs, lag.max=7, type="none") #10 10
VARselect(alldataSMs$heaSMs, lag.max=7, type="none") #8 5