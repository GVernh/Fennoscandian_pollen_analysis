### LCC's ###
rm(list=ls())

libs <- c("dplyr","magrittr")

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

### LCC's ###
dir.create(file.path("./Processed_data/", "LCC_data"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/LCC_data/", "LCC_abun"), showWarnings = FALSE)

# load("./Processed_data/Pollen_relative_abun.Rda")
# bigdf_familynames = pollen_relative_abun
load("./Raw_Data/bigdf_familynames.Rda")
Full_data_sum = bigdf_familynames[c("dataset_ID", "meantimes")]
#names_list <- data.frame(names(bigdf_familynames)) # GRANT: Currently  never used
#write.csv(names_list, file = "names_list.csv")

#clean family.names-list
bigdf_familynames <- subset(bigdf_familynames, 
                            select = -c(Operculodinium.centrocarpum, Pediastrum, Spiniferites, Tilletia))

##########
# POSSIBLE BUG: For some reason, a duplicate for Cyperaceae is created in the above code.
# Remove once using relative abundance data.
bigdf_familynames <- subset(bigdf_familynames,
select = -c(Cyperaceae.1))
#########
bigdf_familynames <- bigdf_familynames[,order(colnames(bigdf_familynames))]


names_list_LCC <- read.csv("./Raw_Data/names_list_LCC.csv")
names_list_LCC = names_list_LCC %>%
  {setNames(data.frame(t(.[,-1])), .[,1])} %>%
  {.[!duplicated(colnames(.))]} %>%
  dplyr::select(order(colnames(.))) %>% 
  magrittr::set_rownames("LCC")

LCC_familynames <- rbind(bigdf_familynames, names_list_LCC)

# Save the file
#save(LCC_familynames,file="LCC_familynames.Rda")
#load("LCC_familynames.Rda")
LCC_familynames.t <- as.data.frame(t(LCC_familynames))


################################
x = unique(LCC_familynames.t$LCC)
x <- x[! x %in% NA] # Remove unwanted LCC's
iterator = 3

for (i in seq_along(x)){
  print(x[i])
  LCC_name = x[i]
  LCC <- LCC_familynames.t %>%
    dplyr::filter(., LCC == paste0(LCC_name)) %>% t(.) %>%
    .[-c(4157), ]
  LCC = data.frame(dataset_ID=bigdf_familynames$dataset_ID,
                                   meantimes=bigdf_familynames$meantimes, LCC)
  #write.csv(LCC, file = paste0("./Processed_data/LCC_data/",LCC_name,".csv"), row.names = FALSE)


  LCC_sum = LCC %>%
    subset(., select = -c(dataset_ID, meantimes)) %>%
    data.matrix(.) %>%
    rowSums(., na.rm = TRUE) %>%
    as.data.frame(.) %>%
    magrittr::set_colnames(paste0(LCC_name, "_sum"))

  Full_data_sum[iterator] <- LCC_sum
  iterator = iterator + 1
}

##########

### Below 2 lines of code maybe helpful for merging, delete if not ###
# full_data_sum <- merge(LCC_sum, meantimes, by=0, all=TRUE, sort = F)
# test = merge(LCC_sum, de, by.x = 0, by.y = 1, all = T, sort= F)

# ONLY TO BE RUN ON RELATIVE ABUNDANCE ####
# for (i in seq_along(x)){
#   print(x[i])
#   LCC_name = x[i]
#   LCC <- LCC_familynames.t %>%
#     dplyr::filter(., LCC == paste0(LCC_name)) %>% t(.) %>%
#     .[-c(4157), ]
#   LCC = data.frame(dataset_ID=bigdf_familynames$dataset_ID,
#                    meantimes=bigdf_familynames$meantimes, LCC)
#   
#   write.csv(LCC, file = paste0("./Processed_data/LCC_data/LCC_abun/",LCC_name,"_abun.csv"), row.names = FALSE)
# 
#   LCC_sum = LCC %>%
#     subset(., select = -c(dataset_ID, meantimes)) %>%
#     sapply(., as.numeric ) %>%
#     rowSums(., na.rm = TRUE) %>%
#     as.data.frame(.) %>%
#     round(.,5) %>% # Currently rounding to 5 decimals
#     magrittr::set_colnames(paste0(LCC_name, "_sum"))
# 
#   Full_data_sum[iterator] <- LCC_sum
#   iterator = iterator + 1
# }

#######################

isNA <-LCC_familynames.t %>% 
  dplyr::filter(., is.na(LCC)) %>% t(.) %>%
  .[-c(4157), ] %>%
  data.frame(.)
write.csv(isNA, file = "./Processed_data/LCC_data/isNA.csv", row.names = FALSE)

isNA_sum <- isNA %>%
  subset(., select = -c(dataset_ID, meantimes)) %>%
  data.matrix(.) %>% # Needs changing for relative abundance
  rowSums(., na.rm = TRUE) %>%
  as.data.frame(.) %>%
  magrittr::set_colnames("isNA_sum")


names(Full_data_sum) <- sub(" ", "_", names(Full_data_sum))
Full_data_sum <- Full_data_sum[c("dataset_ID", "meantimes", "coniferous_woodland_sum", "deciduous_woodland_sum", "wet_woodland_sum",
                                 "pasture_sum", "wet_meadow_sum", "arable_sum", "heath_sum")]

#Save
write.csv(Full_data_sum, file = "./Processed_data/LCC_data/LCC.csv", row.names = FALSE)
#write.csv(Full_data_sum, file = "./Processed_data/LCC_data/LCC_abun/LCC_abun.csv", row.names = FALSE)