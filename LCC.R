### LCC's ###

#load("bigdf_familynames.Rda")

#names_list <- data.frame(names(bigdf_familynames))
#write.csv(names_list, file = "names_list.csv")


#clean family.names-list
#bigdf_familynames <- subset(bigdf_familynames, select = -c(Operculodinium.centrocarpum, Pediastrum, Spiniferites, Tilletia))

#load adapted names_list-file
#names_list_LCC <- read.csv("names_list_LCC.csv")

#names_list_LCC <- as.data.frame(t(names_list_LCC))
#names(names_list_LCC) <- names_list_LCC[1,]
#names_list_LCC <- names_list_LCC[-1,]
#names_list_LCC <- names_list_LCC[,order(colnames(names_list_LCC))]
#bigdf_familynames <- bigdf_familynames[,order(colnames(bigdf_familynames))]
#LCC_familynames <- rbind(bigdf_familynames, names_list_LCC)
# Save the file
#save(LCC_familynames,file="LCC_familynames.Rda")
load("LCC_familynames.Rda")

# install package #
#install.packages("dplyr")

#t
LCC_familynames.t <- t(LCC_familynames)
LCC_familynames.t <- as.data.frame(LCC_familynames.t)

#coniferous woodland
coniferous_woodland = filter(LCC_familynames.t, LCC_familynames.t$LCC == "coniferous woodland")
coniferous_woodland = t(coniferous_woodland)
coniferous_woodland = coniferous_woodland[-c(4157), ]
coniferous_woodland = data.frame(dataset_ID=bigdf_familynames$dataset_ID, meantimes=bigdf_familynames$meantimes, coniferous_woodland)
#Save
write.csv(coniferous_woodland, file = "coniferous_woodland.csv", row.names = FALSE)

coniferous_woodland_sum = subset(coniferous_woodland, select = -c(dataset_ID, meantimes))
coniferous_woodland_sum = data.matrix(coniferous_woodland_sum)
coniferous_woodland_sum = rowSums(coniferous_woodland_sum, na.rm = TRUE)
coniferous_woodland_sum = as.data.frame(coniferous_woodland_sum)
#coniferous_woodland_all = data.frame(coniferous_woodland_all=coniferous_woodland_sum$coniferous_woodland_sum, meantimes=coniferous_woodland$meantimes)
#Save
#write.csv(coniferous_woodland_all, file = "coniferous_woodland_all.csv", row.names = FALSE)

#deciduous woodland
deciduous_woodland = filter(LCC_familynames.t, LCC_familynames.t$LCC == "deciduous woodland")
deciduous_woodland = t(deciduous_woodland)
deciduous_woodland = deciduous_woodland[-c(4157), ]
deciduous_woodland = data.frame(dataset_ID=bigdf_familynames$dataset_ID, meantimes=bigdf_familynames$meantimes, deciduous_woodland)
#Save
write.csv(deciduous_woodland, file = "deciduous_woodland.csv", row.names = FALSE)

deciduous_woodland_sum = subset(deciduous_woodland, select = -c(dataset_ID, meantimes))
deciduous_woodland_sum = data.matrix(deciduous_woodland_sum)
deciduous_woodland_sum = rowSums(deciduous_woodland_sum, na.rm = TRUE)
deciduous_woodland_sum = as.data.frame(deciduous_woodland_sum)
#deciduous_woodland_all = data.frame(deciduous_woodland_all=deciduous_woodland_sum$deciduous_woodland_sum, meantimes=deciduous_woodland$meantimes)
#Save
#write.csv(deciduous_woodland_all, file = "deciduous_woodland_all.csv", row.names = FALSE)

#wet woodland/fen carr
wet_woodland = filter(LCC_familynames.t, LCC_familynames.t$LCC == "wet woodland")
wet_woodland = t(wet_woodland)
wet_woodland = wet_woodland[-c(4157), ]
wet_woodland = data.frame(dataset_ID=bigdf_familynames$dataset_ID, meantimes=bigdf_familynames$meantimes, wet_woodland)
#Save
write.csv(wet_woodland, file = "wet_woodland.csv", row.names = FALSE)

wet_woodland_sum = subset(wet_woodland, select = -c(dataset_ID, meantimes))
wet_woodland_sum = data.matrix(wet_woodland_sum)
wet_woodland_sum = rowSums(wet_woodland_sum, na.rm = TRUE)
wet_woodland_sum = as.data.frame(wet_woodland_sum)
#wet_woodland_all = data.frame(wet_woodland_all=wet_woodland_sum$wet_woodland_sum, meantimes=wet_woodland$meantimes)
#Save
#write.csv(wet_woodland_all, file = "wet_woodland_all.csv", row.names = FALSE)

#pasture/meadow
pasture = filter(LCC_familynames.t, LCC_familynames.t$LCC == "pasture")
pasture = t(pasture)
pasture = pasture[-c(4157), ]
pasture = data.frame(dataset_ID=bigdf_familynames$dataset_ID, meantimes=bigdf_familynames$meantimes, pasture)
#Save
write.csv(pasture, file = "pasture.csv", row.names = FALSE)

pasture_sum = subset(pasture, select = -c(dataset_ID, meantimes))
pasture_sum = data.matrix(pasture_sum)
pasture_sum = rowSums(pasture_sum, na.rm = TRUE)
pasture_sum = as.data.frame(pasture_sum)
#pasture_all = data.frame(pasture_all=pasture_sum$pasture_sum, meantimes=pasture$meantimes)
#Save
#write.csv(pasture_all, file = "pasture_all.csv", row.names = FALSE)

#wet meadow
wet_meadow = filter(LCC_familynames.t, LCC_familynames.t$LCC == "wet meadow")
wet_meadow = t(wet_meadow)
wet_meadow = wet_meadow[-c(4157), ]
wet_meadow = data.frame(dataset_ID=bigdf_familynames$dataset_ID, meantimes=bigdf_familynames$meantimes, wet_meadow)
#Save
write.csv(wet_meadow, file = "wet_meadow.csv", row.names = FALSE)

wet_meadow_sum = subset(wet_meadow, select = -c(dataset_ID, meantimes))
wet_meadow_sum = data.matrix(wet_meadow_sum)
wet_meadow_sum = rowSums(wet_meadow_sum, na.rm = TRUE)
wet_meadow_sum = as.data.frame(wet_meadow_sum)
#wet_meadow_all = data.frame(wet_meadow_all=wet_meadow_sum$wet_meadow_sum, meantimes=wet_meadow$meantimes)
#Save
#write.csv(wet_meadow_all, file = "wet_meadow_all.csv", row.names = FALSE)

#arable
arable = filter(LCC_familynames.t, LCC_familynames.t$LCC == "arable")
arable = t(arable)
arable = arable[-c(4157), ]
arable = data.frame(dataset_ID=bigdf_familynames$dataset_ID, meantimes=bigdf_familynames$meantimes, arable)
#Save
write.csv(arable, file = "arable.csv", row.names = FALSE)

arable_sum = subset(arable, select = -c(dataset_ID, meantimes))
arable_sum = data.matrix(arable_sum)
arable_sum = rowSums(arable_sum, na.rm = TRUE)
arable_sum = as.data.frame(arable_sum)
#arable_all = data.frame(arable_all=arable_sum$arable_sum, meantimes=arable$meantimes)
#Save
#write.csv(arable_all, file = "arable_all.csv", row.names = FALSE)

#heath
heath = filter(LCC_familynames.t, LCC_familynames.t$LCC == "heath")
heath = t(heath)
heath = heath[-c(4157), ]
heath = data.frame(dataset_ID=bigdf_familynames$dataset_ID, meantimes=bigdf_familynames$meantimes, heath)
#Save
write.csv(heath, file = "heath.csv", row.names = FALSE)

heath_sum = subset(heath, select = -c(dataset_ID, meantimes))
heath_sum = data.matrix(heath_sum)
heath_sum = rowSums(heath_sum, na.rm = TRUE)
heath_sum = as.data.frame(heath_sum)
#heath_all = data.frame(heath_all=heath_sum$heath_sum, meantimes=heath$meantimes)
#Save
#write.csv(heath_all, file = "heath_all.csv", row.names = FALSE)

#water
water = filter(LCC_familynames.t, LCC_familynames.t$LCC == "water")
water = t(water)
water = water[-c(4157), ]
water = data.frame(dataset_ID=bigdf_familynames$dataset_ID, meantimes=bigdf_familynames$meantimes, water)
#Save
write.csv(water, file = "water.csv", row.names = FALSE)
water <- read.csv("water.csv")

water_sum = subset(water, select = -c(dataset_ID, meantimes))
water_sum = data.matrix(water_sum)
water_sum = rowSums(water_sum, na.rm = TRUE)
water_sum = as.data.frame(water_sum)

#cliff
cliff = filter(LCC_familynames.t, LCC_familynames.t$LCC == "cliff")
cliff = t(cliff)
cliff = cliff[-c(4157), ]
cliff = data.frame(dataset_ID=bigdf_familynames$dataset_ID, meantimes=bigdf_familynames$meantimes, cliff)
#Save
write.csv(cliff, file = "cliff.csv", row.names = FALSE)
cliff <- read.csv("cliff.csv")

cliff_sum = subset(cliff, select = -c(dataset_ID, meantimes))
cliff_sum = data.matrix(cliff_sum)
cliff_sum = rowSums(cliff_sum, na.rm = TRUE)
cliff_sum = as.data.frame(cliff_sum)

#ruderal
ruderal = filter(LCC_familynames.t, LCC_familynames.t$LCC == "ruderal")
ruderal = t(ruderal)
ruderal = ruderal[-c(4157), ]
ruderal = data.frame(dataset_ID=bigdf_familynames$dataset_ID, meantimes=bigdf_familynames$meantimes, ruderal)
#Save
write.csv(ruderal, file = "ruderal.csv", row.names = FALSE)
ruderal <- read.csv("ruderal.csv")

ruderal_sum = subset(ruderal, select = -c(dataset_ID, meantimes))
ruderal_sum = data.matrix(ruderal_sum)
ruderal_sum = rowSums(ruderal_sum, na.rm = TRUE)
ruderal_sum = as.data.frame(ruderal_sum)

#NA's
isNA <- filter(LCC_familynames.t, is.na(LCC))
isNA = t(isNA)
isNA = isNA[-c(4157), ]
isNA = data.frame(isNA)
#Save
write.csv(isNA, file = "isNA.csv", row.names = FALSE)
isNA <- read.csv("isNA.csv")

isNA_sum = subset(isNA, select = -c(dataset_ID, meantimes))
isNA_sum = data.matrix(isNA_sum)
isNA_sum = rowSums(isNA_sum, na.rm = TRUE)
isNA_sum = as.data.frame(isNA_sum)


#dataframe relevant LCC's
LCC <- data.frame(dataset_ID = bigdf_familynames$dataset_ID, meantimes = bigdf_familynames$meantimes, coniferous_woodland_sum, deciduous_woodland_sum, wet_woodland_sum,
                  pasture_sum, wet_meadow_sum, arable_sum, heath_sum)
#Save
write.csv(LCC, file = "LCC.csv", row.names = FALSE)
LCC <- read.csv("LCC.csv")

