### cross validation ###

library(modelr)
library(glm2)
library(timetk)
library(dplyr)
library(purrr)

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
wetmMWs <- alldataMWs$wetmMWs
pasMWs <- alldataMWs$pasMWs
araMWs <- alldataMWs$araMWs
heaMWs <- alldataMWs$heaMWs

SPD.MM <- alldataMMs$SPD
clim.MM <- alldataMMs$clim
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

### BEFORE THE ONSET OF FARMING #-----------------------------------------------
# Grant:: Needs to be subset by year.
alldataNb <- alldataNs[1:67,]
alldataSEb <- alldataSEs[1:50,]
alldataMWb <- alldataMWs[1:60,]
alldataMMb <- alldataMMs[1:60,]
alldataSWb <- alldataSWs[1:29,]
alldataSMb <- alldataSMs[1:29,]

SPD.Nb <- alldataNb$SPD
clim.Nb <- alldataNb$clim
conNb <- alldataNb$conNs
decNb <- alldataNb$decNs
wetwNb <- alldataNb$wetwNs
wetmNb <- alldataNb$wetmNs
pasNb <- alldataNb$pasNs
araNb <- alldataNb$araNs
heaNb <- alldataNb$heaNs

SPD.SEb <- alldataSEb$SPD
clim.SEb <- alldataSEb$clim
conSEb <- alldataSEb$conSEs
decSEb <- alldataSEb$decSEs
wetwSEb <- alldataSEb$wetwSEs
wetmSEb <- alldataSEb$wetmSEs
pasSEb <- alldataSEb$pasSEs
araSEb <- alldataSEb$araSEs
heaSEb <- alldataSEb$heaSEs

SPD.MWb <- alldataMWb$SPD
clim.MWb <- alldataMWb$clim
conMWb <- alldataMWb$conMWs
decMWb <- alldataMWb$decMWs
wetwMWb <- alldataMWb$wetwMWs
wetmMWb <- alldataMWb$wetmMWs
pasMWb <- alldataMWb$pasMWs
araMWb <- alldataMWb$araMWs
heaMWb <- alldataMWb$heaMWs

SPD.MMb <- alldataMMb$SPD
clim.MMb <- alldataMMb$clim
conMMb <- alldataMMb$conMMs
decMMb <- alldataMMb$decMMs
wetwMMb <- alldataMMb$wetwMMs
wetmMMb <- alldataMMb$wetmMMs
pasMMb <- alldataMMb$pasMMs
araMMb <- alldataMMb$araMMs
heaMMb <- alldataMMb$heaMMs

SPD.SWb <- alldataSWb$SPD
clim.SWb <- alldataSWb$clim
conSWb <- alldataSWb$conSWs
decSWb <- alldataSWb$decSWs
wetwSWb <- alldataSWb$wetwSWs
wetmSWb <- alldataSWb$wetmSWs
pasSWb <- alldataSWb$pasSWs
araSWb <- alldataSWb$araSWs
heaSWb <- alldataSWb$heaSWs

SPD.SMb <- alldataSMb$SPD
clim.SMb <- alldataSMb$clim
conSMb <- alldataSMb$conSMs
decSMb <- alldataSMb$decSMs
wetwSMb <- alldataSMb$wetwSMs
wetmSMb <- alldataSMb$wetmSMs
pasSMb <- alldataSMb$pasSMs
araSMb <- alldataSMb$araSMs
heaSMb <- alldataSMb$heaSMs

### AFTER THE ONSET OF FARMING #------------------------------------------------
alldataNa <- alldataNs[68:nrow(alldataNs),]
alldataSEa <- alldataSEs[51:nrow(alldataSEs),]
alldataMWa <- alldataMWs[61:nrow(alldataMWs),]
alldataMMa <- alldataMMs[61:nrow(alldataMMs),]
alldataSWa <- alldataSWs[30:nrow(alldataSWs),]
alldataSMa <- alldataSMs[30:nrow(alldataSMs),]

SPD.Na <- alldataNa$SPD
clim.Na <- alldataNa$clim
conNa <- alldataNa$conNs
decNa <- alldataNa$decNs
wetwNa <- alldataNa$wetwNs
wetmNa <- alldataNa$wetmNs
pasNa <- alldataNa$pasNs
araNa <- alldataNa$araNs
heaNa <- alldataNa$heaNs

SPD.SEa <- alldataSEa$SPD
clim.SEa <- alldataSEa$clim
conSEa <- alldataSEa$conSEs
decSEa <- alldataSEa$decSEs
wetwSEa <- alldataSEa$wetwSEs
wetmSEa <- alldataSEa$wetmSEs
pasSEa <- alldataSEa$pasSEs
araSEa <- alldataSEa$araSEs
heaSEa <- alldataSEa$heaSEs

SPD.MWa <- alldataMWa$SPD
clim.MWa <- alldataMWa$clim
conMWa <- alldataMWa$conMWs
decMWa <- alldataMWa$decMWs
wetwMWa <- alldataMWa$wetwMWs
wetmMWa <- alldataMWa$wetmMWs
pasMWa <- alldataMWa$pasMWs
araMWa <- alldataMWa$araMWs
heaMWa <- alldataMWa$heaMWs

SPD.MMa <- alldataMMa$SPD
clim.MMa <- alldataMMa$clim
conMMa <- alldataMMa$conMMs
decMMa <- alldataMMa$decMMs
wetwMMa <- alldataMMa$wetwMMs
wetmMMa <- alldataMMa$wetmMMs
pasMMa <- alldataMMa$pasMMs
araMMa <- alldataMMa$araMMs
heaMMa <- alldataMMa$heaMMs

SPD.SWa <- alldataSWa$SPD
clim.SWa <- alldataSWa$clim
conSWa <- alldataSWa$conSWs
decSWa <- alldataSWa$decSWs
wetwSWa <- alldataSWa$wetwSWs
wetmSWa <- alldataSWa$wetmSWs
pasSWa <- alldataSWa$pasSWs
araSWa <- alldataSWa$araSWs
heaSWa <- alldataSWa$heaSWs

SPD.SMa <- alldataSMa$SPD
clim.SMa <- alldataSMa$clim
conSMa <- alldataSMa$conSMs
decSMa <- alldataSMa$decSMs
wetwSMa <- alldataSMa$wetwSMs
wetmSMa <- alldataSMa$wetmSMs
pasSMa <- alldataSMa$pasSMs
araSMa <- alldataSMa$araSMs
heaSMa <- alldataSMa$heaSMs

#parameters
nfolds = 2
maxlag = 7
#yearsBP = alldataNs$yearsBP

#function
crossvalidation = function(LCC, SPD, clim){
  varnames <- c(deparse(substitute(LCCadapt)), deparse(substitute(SPD)), deparse(substitute(clim)))
  lagnames <- paste0(varnames, "_lag", rep(1:maxlag, each = 3))
  # Then we created the lagged variables / data for models
  VAR_data <- embed(as.matrix(cbind(LCC, SPD, clim)), maxlag + 1)
  colnames(VAR_data) <- c(varnames, lagnames)
  VAR_data <- as.data.frame(VAR_data[, -c(2,3)])
  
  dataconN_cv  <- crossv_kfold(VAR_data, k = nfolds) # just an old structure that we'll fill out differently
  
  chunksize = ceiling(nrow(VAR_data)/(nfolds+1))
  BP = -yearsBP[(maxlag + 1):length(yearsBP)] # so that time goes up with time
  VAR_data_ext = cbind(BP,VAR_data)
  tscv <- time_series_cv(data = VAR_data_ext, date_var = BP, initial = chunksize, skip = chunksize - 1, assess = chunksize - 1, cumulative = TRUE)
  
  for(i in 1:nfolds){
    dataconN_cv$train[[i]]$idx <- tscv$splits[[i]]$in_id
    dataconN_cv$test[[i]]$idx <- tscv$splits[[i]]$out_id
    dataconN_cv$train[[i]]$idx
    dataconN_cv$test[[i]]$idx
  }
  # the full model with LCC, SPD and clim
  formula7 <- paste(colnames(VAR_data)[-1], collapse = " + ")
  formula7 <- formula(paste0(varnames[1], " ~ ", formula7))
  
  formula6 <- select(VAR_data,-contains("7"))
  formula6 <- paste(colnames(formula6)[-1], collapse = " + ")
  formula6 <- formula(paste0(varnames[1], " ~ ", formula6))
  
  formula5 <- select(VAR_data,-contains(c("7", "6")))
  formula5 <- paste(colnames(formula5)[-1], collapse = " + ")
  formula5 <- formula(paste0(varnames[1], " ~ ", formula5))
  
  formula4 <- select(VAR_data,-contains(c("7", "6", "5")))
  formula4 <- paste(colnames(formula4)[-1], collapse = " + ")
  formula4 <- formula(paste0(varnames[1], " ~ ", formula4))
  
  formula3 <- select(VAR_data,-contains(c("7", "6", "5", "4")))
  formula3 <- paste(colnames(formula3)[-1], collapse = " + ")
  formula3 <- formula(paste0(varnames[1], " ~ ", formula3))
  
  formula2 <- select(VAR_data,-contains(c("7", "6", "5", "4", "3")))
  formula2 <- paste(colnames(formula2)[-1], collapse = " + ")
  formula2 <- formula(paste0(varnames[1], " ~ ", formula2))
  
  formula1 <- select(VAR_data,-contains(c("7", "6", "5", "4", "3", "2")))
  formula1 <- paste(colnames(formula1)[-1], collapse = " + ")
  formula1 <- formula(paste0(varnames[1], " ~ ", formula1))
  
  model7  <- map(dataconN_cv$train, ~glm2(formula7, data = .))
  model6  <- map(dataconN_cv$train, ~glm2(formula6, data = .))
  model5  <- map(dataconN_cv$train, ~glm2(formula5, data = .))
  model4  <- map(dataconN_cv$train, ~glm2(formula4, data = .))
  model3  <- map(dataconN_cv$train, ~glm2(formula3, data = .))
  model2  <- map(dataconN_cv$train, ~glm2(formula2, data = .))
  model1  <- map(dataconN_cv$train, ~glm2(formula1, data = .))
  
  get_pred  <- function(model, test_data){
    data  <- as.data.frame(test_data)
    pred  <- add_predictions(data, model)
    return(pred)
  }
  pred7  <- map2_df(model7, dataconN_cv$test, get_pred, .id = "Run")
  pred6  <- map2_df(model6, dataconN_cv$test, get_pred, .id = "Run")
  pred5  <- map2_df(model5, dataconN_cv$test, get_pred, .id = "Run")
  pred4  <- map2_df(model4, dataconN_cv$test, get_pred, .id = "Run")
  pred3  <- map2_df(model3, dataconN_cv$test, get_pred, .id = "Run")
  pred2  <- map2_df(model2, dataconN_cv$test, get_pred, .id = "Run")
  pred1  <- map2_df(model1, dataconN_cv$test, get_pred, .id = "Run")
  
  MSE7  <- pred7 %>% group_by(Run) %>%
    summarise(MSE = mean( (LCCadapt - pred)^2))
  
  MSE6  <- pred6 %>% group_by(Run) %>%
    summarise(MSE = mean( (LCCadapt - pred)^2))
  
  MSE5  <- pred5 %>% group_by(Run) %>%
    summarise(MSE = mean( (LCCadapt - pred)^2))
  
  MSE4  <- pred4 %>% group_by(Run) %>%
    summarise(MSE = mean( (LCCadapt - pred)^2))
  
  MSE3  <- pred3 %>% group_by(Run) %>%
    summarise(MSE = mean( (LCCadapt - pred)^2))
  
  MSE2  <- pred2 %>% group_by(Run) %>%
    summarise(MSE = mean( (LCCadapt - pred)^2))
  
  MSE1  <- pred1 %>% group_by(Run) %>%
    summarise(MSE = mean( (LCCadapt - pred)^2))
  
  v1 = c("lag1", "lag2", "lag3", "lag4", "lag5", "lag6", "lag7")
  v2 = c(MSE1=mean(MSE1$MSE), MSE2=mean(MSE2$MSE), MSE3=mean(MSE3$MSE), MSE4=mean(MSE4$MSE), MSE5=mean(MSE5$MSE), MSE6=mean(MSE6$MSE), MSE7=mean(MSE7$MSE))
  list = data.frame(v1, v2)
  return(list[which.min(list$v2),])
  #return(list(MSE1=mean(MSE1$MSE), MSE2=mean(MSE2$MSE), MSE3=mean(MSE3$MSE), MSE4=mean(MSE4$MSE), MSE5=mean(MSE5$MSE), MSE6=mean(MSE6$MSE), MSE7=mean(MSE7$MSE)))
}

#results

#whole holocene
yearsBP = alldataNs$yearsBP
crossvalidation(conNs, SPD.N, clim.N) #2
crossvalidation(decNs, SPD.N, clim.N) #1
crossvalidation(wetwNs, SPD.N, clim.N) #1
crossvalidation(wetmNs, SPD.N, clim.N) #2
crossvalidation(pasNs, SPD.N, clim.N) #1
crossvalidation(araNs, SPD.N, clim.N) #1
crossvalidation(heaNs, SPD.N, clim.N) #2

yearsBP = alldataSEs$yearsBP
crossvalidation(conSEs, SPD.SE, clim.SE) #4
crossvalidation(decSEs, SPD.SE, clim.SE) #6
crossvalidation(wetwSEs, SPD.SE, clim.SE) #1
crossvalidation(wetmSEs, SPD.SE, clim.SE) #4
crossvalidation(pasSEs, SPD.SE, clim.SE) #1
crossvalidation(araSEs, SPD.SE, clim.SE) #3
crossvalidation(heaSEs, SPD.SE, clim.SE) #1

yearsBP = alldataMWs$yearsBP
crossvalidation(conMWs, SPD.MW, clim.MW) #1
crossvalidation(decMWs, SPD.MW, clim.MW) #3
crossvalidation(wetwMWs, SPD.MW, clim.MW) #2
crossvalidation(wetmMWs, SPD.MW, clim.MW) #1
crossvalidation(pasMWs, SPD.MW, clim.MW) #1
crossvalidation(araMWs, SPD.MW, clim.MW) #1
crossvalidation(heaMWs, SPD.MW, clim.MW) #2

yearsBP = alldataMMs$yearsBP
crossvalidation(conMMs, SPD.MM, clim.MM) #4
crossvalidation(decMMs, SPD.MM, clim.MM) #4
crossvalidation(wetwMMs, SPD.MM, clim.MM) #4
crossvalidation(wetmMMs, SPD.MM, clim.MM) #1
crossvalidation(pasMMs, SPD.MM, clim.MM) #3
crossvalidation(araMMs, SPD.MM, clim.MM) #4
crossvalidation(heaMMs, SPD.MM, clim.MM) #6

yearsBP = alldataSWs$yearsBP
crossvalidation(conSWs, SPD.SW, clim.SW) #1
crossvalidation(decSWs, SPD.SW, clim.SW) #1
crossvalidation(wetwSWs, SPD.SW, clim.SW) #1
crossvalidation(wetmSWs, SPD.SW, clim.SW) #1
crossvalidation(pasSWs, SPD.SW, clim.SW) #5
crossvalidation(araSWs, SPD.SW, clim.SW) #3
crossvalidation(heaSWs, SPD.SW, clim.SW) #3

yearsBP = alldataSMs$yearsBP
crossvalidation(conSMs, SPD.SM, clim.SM) #6
crossvalidation(decSMs, SPD.SM, clim.SM) #1
crossvalidation(wetwSMs, SPD.SM, clim.SM) #1
crossvalidation(wetmSMs, SPD.SM, clim.SM) #1
crossvalidation(pasSMs, SPD.SM, clim.SM) #1
crossvalidation(araSMs, SPD.SM, clim.SM) #5
crossvalidation(heaSMs, SPD.SM, clim.SM) #1

