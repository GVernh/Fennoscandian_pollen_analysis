if (!(paste0("Temp12k_v1_0_0.RData") %in% list.files("./Raw_Data/"))){
  zipF<- "./Raw_Data/Temp12k_v1_0_0.RData.zip"
  outDir<-"./Raw_Data/"
  unzip(zipF,exdir=outDir)
}

if (!(paste0("p3k14c_original.csv") %in% list.files("./Raw_Data/"))) {
  zipF<- "./Raw_Data/p3k14c_original.csv.zip"
  outDir<-"./Raw_Data/"
  unzip(zipF,exdir=outDir)
}


rm(list=ls())