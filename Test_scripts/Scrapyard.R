# SCRAPYARD
#### LCC.R ###
names_list_LCC <- data.frame(names(bigdf_familynames))
#write.csv(names_list, file = "names_list.csv")


#clean family.names-list
bigdf_familynames <- subset(bigdf_familynames, select = -c(Operculodinium.centrocarpum, 
                                                           Pediastrum, Spiniferites, Tilletia))

#load adapted names_list-file
#names_list_LCC <- read.csv("names_list_LCC.csv")

names_list_LCC <- as.data.frame(t(names_list_LCC))
names(names_list_LCC) <- names_list_LCC[1,]
names_list_LCC <- names_list_LCC[-1,]
names_list_LCC <- names_list_LCC[,order(colnames(names_list_LCC))]
LCC_familynames <- rbind(bigdf_familynames, names_list_LCC)
# Save the file
#save(LCC_familynames,file="LCC_familynames.Rda")
load("LCC_familynames.Rda")
################




#coniferous woodland
coniferous_woodland = LCC_familynames.t %>%
  dplyr::filter(., LCC == "heath") %>% t(.) %>%
  .[-c(4157), ]
coniferous_woodland = data.frame(dataset_ID=bigdf_familynames$dataset_ID, 
                                 meantimes=bigdf_familynames$meantimes, coniferous_woodland)


#Save
#write.csv(coniferous_woodland, file = "./Processed_data/coniferous_woodland.csv", row.names = FALSE)

coniferous_woodland_sum = coniferous_woodland %>% 
  subset(., select = -c(dataset_ID, meantimes)) %>%
  sapply(., as.numeric ) %>% 
  rowSums(., na.rm = TRUE) %>%
  as.data.frame(.) %>%
  magrittr::set_colnames("coniferous_woodland_sum")

coniferous_woodland_all = data.frame(coniferous_woodland_all=coniferous_woodland_sum$coniferous_woodland_sum, 
                                     meantimes=coniferous_woodland$meantimes)