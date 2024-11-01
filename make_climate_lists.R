all_temp_sites = readRDS("./Processed_data/all_temp_sites.RDS")


### Functions ###
source("./Functions/Make_clim_df.R")

age = all_temp_sites$AgeroedsMosse.Nilsson.1964$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$AgeroedsMosse.Nilsson.1964$paleoData[[1]]$measurementTable[[1]]$temperature$values
df <- data.frame(age, temp)
df$site = "AgeroedsMosse.Nilsson.1964" 

# changed to get rid of the error..
df$long= all_temp_sites$AgeroedsMosse.Nilsson.1964$geo$longitude
df$lat=all_temp_sites$AgeroedsMosse.Nilsson.1964$geo$latitude
makeStandardDF = df


test = make_clim_df(all_temp_sites, "850Lake.Shemesh.2001")

test.list <- NULL
for (site in clim15$site) {
  age = all_temp_sites$site$paleoData[[1]]$measurementTable[[1]]$age$values
  temp = all_temp_sites$site$paleoData[[1]]$measurementTable[[1]]$temperature$values
  df <- data.frame(age, temp)
  namelist = df
  if(site=="850Lake.Shemesh.2001"){
    test.list <- namelist
  }
  else{
    test.list <- dplyr::bind_rows(test.list, namelist)
  }
}

#make bigdf
#source("makeStandardDF.R")
# WHY? WHY WAS IT DEFINED TWICE? JT

makeStandardDF = function(All_Age_Depth_Curves, pollen_all, sitenum)
{
  counts <- pollen_all[[sitenum]]$counts
  meantimes = apply(All_Age_Depth_Curves[[sitenum]]$thetaPredict,c(2),mean)
  df <- data.frame(meantimes, counts)
  df$dataset_ID = sitenum
  makeStandardDF = df
}

bigdf = NULL
for(name in names(All_Age_Depth_Curves))
{
  namedf = makeStandardDF(All_Age_Depth_Curves, pollen_all, name)
  if(name=="12"){
    bigdf = namedf
  }
  else{
    bigdf = dplyr::bind_rows(bigdf, namedf)
  }
}

