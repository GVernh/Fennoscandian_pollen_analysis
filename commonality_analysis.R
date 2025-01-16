### commonality analysis function ###

library(dplyr)

#vectors
SPD.N <- alldataNs$SPD
clim.N <- alldataNs$clim
conNs <- alldataNs$conNs
decNs <- alldataNs$decNs
wetwNs <- alldataNs$wetwNs
wetmNs <- alldataNs$wetmNs
pasNs <- alldataNs$pasNs
araNs <- alldataNs$araNs
heaNs <- alldataNs$heaNs

SPD.SE <- alldataSEs$SPD
clim.SE <- alldataSEs$clim
conSEs <- alldataSEs$conSEs
decSEs <- alldataSEs$decSEs
wetwSEs <- alldataSEs$wetwSEs
wetmSEs <- alldataSEs$wetmSEs
pasSEs <- alldataSEs$pasSEs
araSEs <- alldataSEs$araSEs
heaSEs <- alldataSEs$heaSEs

SPD.MW <- alldataMWs$SPD
clim.MW <- alldataMWs$clim
conMWs <- alldataMWs$conMWs
decMWs <- alldataMWs$decMWs
wetwMWs <- alldataMWs$wetwMWs
wetmMws <- alldataMWs$wetmMWs
pasMWs <- alldataMWs$pasMWs
araMWs <- alldataMWs$araMWs
heaMWs <- alldataMWs$heaMWs

SPD.MM <- alldataMMs$SPD
clim.MM <- alldataMM$clim
conMMs <- alldataMMs$conMMs
decMMs <- alldataMMs$decMMs
wetwMMs <- alldataMMs$wetwMMs
wetmMMs <- alldataMMs$wetmMMs
pasMMs <- alldataMMs$pasMMs
araMMs <- alldataMMs$araMMs
heaMMs <- alldataMMs$heaMMs

SPD.SW <- alldataSWs$SPD
clim.SW <- alldataSWs$clim
conSWs <- alldataSWs$conSWs
decSWs <- alldataSWs$decSWs
wetwSWs <- alldataSWs$wetwSWs
wetmSWs <- alldataSWs$wetmSWs
pasSWs <- alldataSWs$pasSWs
araSWs <- alldataSWs$araSWs
heaSWs <- alldataSWs$heaSWs

SPD.SM <- alldataSMs$SPD
clim.SM <- alldataSMs$clim
conSMs <- alldataSMs$conSMs
decSMs <- alldataSMs$decSMs
wetwSMs <- alldataSMs$wetwSMs
wetmSMs <- alldataSMs$wetmSMs
pasSMs <- alldataSMs$pasSMs
araSMs <- alldataSMs$araSMs
heaSMs <- alldataSMs$heaSMs

### commonality analysis ###
test1 <- custom_commonality_analysis(conSEs, SPD.SE, clim.SE, 1)
test4 <- custom_commonality_analysis(conSEs, SPD.SE, clim.SE, 4)
test7 <- custom_commonality_analysis(conSEs, SPD.SE, clim.SE, 7)
