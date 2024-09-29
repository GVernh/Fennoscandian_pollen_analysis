### Granger causality test ###

library(vars)
library(bruceR)
library(glm2)
library(lmtest)
library(stats)
library(dplyr)

#function
custom_commonality_analysis <- function(LCC, SPD, clim, p) {
  #commonality analysis
  varnames <- c(deparse(substitute(LCCadapt)), deparse(substitute(SPD)), deparse(substitute(clim)))
  lagnames <- paste0(varnames, "_lag", rep(1:p, each = 3))
  # Then we created the lagged variables / data for models
  VAR_data <- embed(as.matrix(cbind(LCC, SPD, clim)), p + 1)
  colnames(VAR_data) <- c(varnames, lagnames)
  VAR_data <- VAR_data[, -c(2,3)]
  # the full model with LCC, SPD and clim
  model_formula <- paste(colnames(VAR_data)[-1], collapse = " + ")
  model_formula <- formula(paste0(varnames[1], " ~ ", model_formula))
  full_mod <- lm(model_formula, data = as.data.frame(VAR_data))
  # LCC and clim
  VAR_data_c <- select(as.data.frame(VAR_data),contains(c("LCCadapt", "clim")))
  model_formula <- paste(colnames(VAR_data_c)[-1], collapse = " + ")
  model_formula <- formula(paste0(varnames[1], " ~ ", model_formula))
  clim_mod <- lm(model_formula, data = as.data.frame(VAR_data_c))
  # LCC and SPD
  VAR_data_s <- select(as.data.frame(VAR_data),contains(c("LCCadapt", "SPD")))
  model_formula <- paste(colnames(VAR_data_s)[-1], collapse = " + ")
  model_formula <- formula(paste0(varnames[1], " ~ ", model_formula))
  SPD_mod <- lm(model_formula, data = as.data.frame(VAR_data_s))
  # only LCC
  VAR_data_l <- select(as.data.frame(VAR_data),contains("LCCadapt"))
  model_formula <- paste(colnames(VAR_data_l)[-1], collapse = " + ")
  model_formula <- formula(paste0(varnames[1], " ~ ", model_formula))
  LCC_mod <- lm(model_formula, data = as.data.frame(VAR_data_l))
  # Granger test
  granger_test_f <- anova(LCC_mod, full_mod)
  granger_test_s <- anova(LCC_mod, SPD_mod)
  granger_test_c <- anova(LCC_mod, clim_mod)
  # getting R squared values
  R2_lsc = (granger_test_f$RSS[1] - granger_test_f$RSS[2]) / granger_test_f$RSS[1]
  R2_ls = (granger_test_s$RSS[1] - granger_test_s$RSS[2]) / granger_test_s$RSS[1]
  R2_lc = (granger_test_c$RSS[1] - granger_test_c$RSS[2]) / granger_test_c$RSS[1]
  # commonality analysis
  Us <- R2_lsc - R2_lc
  Uc <- R2_lsc - R2_ls
  Csc <- R2_lsc - Us - Uc
  # Return the results
  return(list(R2_lsc =R2_lsc,
              R2_ls =R2_ls,
              R2_lc =R2_lc,
              Us=Us,
              Uc=Uc,
              Csc=Csc))
}

# multivariate granger causality test from the bruceR package.------------------

### NORTH ###
#-------------------------------------------------------------------------------
vm <- VAR(alldataNs[,c(2,3,11)], p=2) 
granger_causality(varmodel = vm, var.y = "conNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNs[,c(2,3,12)], p=1)
granger_causality(varmodel = vm, var.y = "decNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNs[,c(2,3,13)], p=1)
granger_causality(varmodel = vm, var.y = "wetwNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNs[,c(2,3,14)], p=2)
granger_causality(varmodel = vm, var.y = "wetmNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNs[,c(2,3,15)], p=1)
granger_causality(varmodel = vm, var.y = "pasNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNs[,c(2,3,16)], p=1)
granger_causality(varmodel = vm, var.y = "araNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNs[,c(2,3,17)], p=2)
granger_causality(varmodel = vm, var.y = "heaNs", var.x = c("SPD", "clim"))


### SOUTHEAST ###
#-------------------------------------------------------------------------------
vm <- VAR(alldataSEs[,c(2,3,11)], p=4)
granger_causality(varmodel = vm, var.y = "conSEs", var.x = c("SPD", "clim")) #sig all
custom_commonality_analysis(conSEs, SPD.SE, clim.SE, 4) #all

vm <- VAR(alldataSEs[,c(2,3,12)], p=6)
granger_causality(varmodel = vm, var.y = "decSEs", var.x = c("SPD", "clim")) #low for clim
custom_commonality_analysis(decSEs, SPD.SE, clim.SE, 6) #all

vm <- VAR(alldataSEs[,c(2,3,13)], p=1)
granger_causality(varmodel = vm, var.y = "wetwSEs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSEs[,c(2,3,14)], p=4)
granger_causality(varmodel = vm, var.y = "wetmSEs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSEs[,c(2,3,15)], p=1)
granger_causality(varmodel = vm, var.y = "pasSEs", var.x = c("SPD", "clim")) #spd
custom_commonality_analysis(pasSEs, SPD.SE, clim.SE, 1)

vm <- VAR(alldataSEs[,c(2,3,16)], p=3)
granger_causality(varmodel = vm, var.y = "araSEs", var.x = c("SPD", "clim")) #low spd
custom_commonality_analysis(araSEs, SPD.SE, clim.SE, 3)

vm <- VAR(alldataSEs[,c(2,3,17)], p=1)
granger_causality(varmodel = vm, var.y = "heaSEs", var.x = c("SPD", "clim"))
custom_commonality_analysis(heaSEs, SPD.SE, clim.SE, 1)


### MIDWEST ###
#-------------------------------------------------------------------------------
vm <- VAR(alldataMWs[,c(2,3,11)], p=1)
granger_causality(varmodel = vm, var.y = "conMWs", var.x = c("SPD", "clim"))

vm <- VAR(alldataMWs[,c(2,3,12)], p=3)
granger_causality(varmodel = vm, var.y = "decMWs", var.x = c("SPD", "clim"))

vm <- VAR(alldataMWs[,c(2,3,13)], p=2)
granger_causality(varmodel = vm, var.y = "wetwMWs", var.x = c("SPD", "clim")) #clim
custom_commonality_analysis(wetwMWs, SPD.MW, clim.MW, 2)

vm <- VAR(alldataMWs[,c(2,3,14)], p=1)
granger_causality(varmodel = vm, var.y = "wetmMWs", var.x = c("SPD", "clim"))

vm <- VAR(alldataMWs[,c(2,3,15)], p=1)
granger_causality(varmodel = vm, var.y = "pasMWs", var.x = c("SPD", "clim")) #low for spd
custom_commonality_analysis(pasMWs, SPD.MW, clim.MW, 1)

vm <- VAR(alldataMWs[,c(2,3,16)], p=1)
granger_causality(varmodel = vm, var.y = "araMWs", var.x = c("SPD", "clim")) #sig both
custom_commonality_analysis(araMWs, SPD.MW, clim.MW, 1) 

vm <- VAR(alldataMWs[,c(2,3,17)], p=2)
granger_causality(varmodel = vm, var.y = "heaMWs", var.x = c("SPD", "clim")) #low for both
custom_commonality_analysis(heaMWs, SPD.MW, clim.MW, 2) 

### MIDMID ###
#-------------------------------------------------------------------------------
vm <- VAR(alldataMMs[,c(2,3,11)], p=4)
granger_causality(varmodel = vm, var.y = "conMMs", var.x = c("SPD", "clim"))

vm <- VAR(alldataMMs[,c(2,3,12)], p=4)
granger_causality(varmodel = vm, var.y = "decMMs", var.x = c("SPD", "clim")) #sig spd
custom_commonality_analysis(decMMs, SPD.MM, clim.MM, 4) 

vm <- VAR(alldataMMs[,c(2,3,13)], p=4)
granger_causality(varmodel = vm, var.y = "wetwMMs", var.x = c("SPD", "clim")) #low spd
custom_commonality_analysis(wetwMMs, SPD.MM, clim.MM, 4) 

vm <- VAR(alldataMMs[,c(2,3,14)], p=1)
granger_causality(varmodel = vm, var.y = "wetmMMs", var.x = c("SPD", "clim")) #spd
custom_commonality_analysis(wetmMMs, SPD.MM, clim.MM, 1) 

vm <- VAR(alldataMMs[,c(2,3,15)], p=3)
granger_causality(varmodel = vm, var.y = "pasMMs", var.x = c("SPD", "clim")) #sig spd
custom_commonality_analysis(pasMMs, SPD.MM, clim.MM, 3)

vm <- VAR(alldataMMs[,c(2,3,16)], p=4)
granger_causality(varmodel = vm, var.y = "araMMs", var.x = c("SPD", "clim")) #sig spd
custom_commonality_analysis(araMMs, SPD.MM, clim.MM, 4)

vm <- VAR(alldataMMs[,c(2,3,17)], p=6)
granger_causality(varmodel = vm, var.y = "heaMMs", var.x = c("SPD", "clim")) #sig spd
custom_commonality_analysis(heaMMs, SPD.MM, clim.MM, 6)


### SOUTHWEST ###
#-------------------------------------------------------------------------------
vm <- VAR(alldataSWs[,c(2,3,11)], p=1)
granger_causality(varmodel = vm, var.y = "conSWs", var.x = c("SPD", "clim")) #low spd and clim
custom_commonality_analysis(conSWs, SPD.SW, clim.SW, 1)

vm <- VAR(alldataSWs[,c(2,3,12)], p=1)
granger_causality(varmodel = vm, var.y = "decSWs", var.x = c("SPD", "clim")) #sig all
custom_commonality_analysis(decSWs, SPD.SW, clim.SW, 1)

vm <- VAR(alldataSWs[,c(2,3,13)], p=1)
granger_causality(varmodel = vm, var.y = "wetwSWs", var.x = c("SPD", "clim")) #low all
custom_commonality_analysis(wetwSWs, SPD.SW, clim.SW, 1)

vm <- VAR(alldataSWs[,c(2,3,15)], p=1)
granger_causality(varmodel = vm, var.y = "wetmSWs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSWs[,c(2,3,14)], p=5)
granger_causality(varmodel = vm, var.y = "pasSWs", var.x = c("SPD", "clim")) #low both
custom_commonality_analysis(pasSWs, SPD.SW, clim.SW, 5)

vm <- VAR(alldataSWs[,c(2,3,16)], p=3)
granger_causality(varmodel = vm, var.y = "araSWs", var.x = c("SPD", "clim")) #sig spd
custom_commonality_analysis(araSWs, SPD.SW, clim.SW, 3)

vm <- VAR(alldataSWs[,c(2,3,17)], p=3)
granger_causality(varmodel = vm, var.y = "heaSWs", var.x = c("SPD", "clim"))


### SOUTHMID ###
#-------------------------------------------------------------------------------
vm <- VAR(alldataSMs[,c(2,3,11)], p=6)
granger_causality(varmodel = vm, var.y = "conSMs", var.x = c("SPD", "clim")) #spd
custom_commonality_analysis(conSMs, SPD.SM, clim.SM, 6)

vm <- VAR(alldataSMs[,c(2,3,12)], p=1)
granger_causality(varmodel = vm, var.y = "decSMs", var.x = c("SPD", "clim")) #spd
custom_commonality_analysis(decSMs, SPD.SM, clim.SM, 1)

vm <- VAR(alldataSMs[,c(2,3,13)], p=1)
granger_causality(varmodel = vm, var.y = "wetwSMs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSMs[,c(2,3,14)], p=1)
granger_causality(varmodel = vm, var.y = "wetmSMs", var.x = c("SPD", "clim")) #spd
custom_commonality_analysis(wetmSMs, SPD.SM, clim.SM, 1)

vm <- VAR(alldataSMs[,c(2,3,15)], p=1)
granger_causality(varmodel = vm, var.y = "pasSMs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSMs[,c(2,3,16)], p=5)
granger_causality(varmodel = vm, var.y = "araSMs", var.x = c("SPD", "clim")) #spd
custom_commonality_analysis(araSMs, SPD.SM, clim.SM, 5)

vm <- VAR(alldataSMs[,c(2,3,17)], p=1)
granger_causality(varmodel = vm, var.y = "heaSMs", var.x = c("SPD", "clim"))


### BEFORE THE ONSET OF FARMING #-----------------------------------------------

### NORTH ### before 2300 BP (9000 - 2400 BP)
#-------------------------------------------------------------------------------
vm <- VAR(alldataNb[,c(2,3,11)], p=2) 
granger_causality(varmodel = vm, var.y = "conNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNb[,c(2,3,12)], p=2)
granger_causality(varmodel = vm, var.y = "decNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNb[,c(2,3,13)], p=1)
granger_causality(varmodel = vm, var.y = "wetwNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNb[,c(2,3,14)], p=3)
granger_causality(varmodel = vm, var.y = "wetmNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNb[,c(2,3,15)], p=1)
granger_causality(varmodel = vm, var.y = "pasNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNb[,c(2,3,16)], p=2)
granger_causality(varmodel = vm, var.y = "araNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNb[,c(2,3,17)], p=1)
granger_causality(varmodel = vm, var.y = "heaNs", var.x = c("SPD", "clim")) # clim
custom_commonality_analysis(heaNb, SPD.Nb, clim.Nb, 1)

### SOUTHEAST ### 4000 BP
#-------------------------------------------------------------------------------
vm <- VAR(alldataSEb[,c(2,3,11)], p=3)
granger_causality(varmodel = vm, var.y = "conSEs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSEb[,c(2,3,12)], p=1)
granger_causality(varmodel = vm, var.y = "decSEs", var.x = c("SPD", "clim"))
custom_commonality_analysis(decSEb, SPD.SEb, clim.SEb, 1)

vm <- VAR(alldataSEb[,c(2,3,13)], p=1)
granger_causality(varmodel = vm, var.y = "wetwSEs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSEb[,c(2,3,14)], p=1)
granger_causality(varmodel = vm, var.y = "wetmSEs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSEb[,c(2,3,15)], p=1)
granger_causality(varmodel = vm, var.y = "pasSEs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSEb[,c(2,3,16)], p=3)
granger_causality(varmodel = vm, var.y = "araSEs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSEb[,c(2,3,17)], p=1)
granger_causality(varmodel = vm, var.y = "heaSEs", var.x = c("SPD", "clim"))


### MIDWEST ### 3000 BP
#-------------------------------------------------------------------------------
vm <- VAR(alldataMWb[,c(2,3,11)], p=1)
granger_causality(varmodel = vm, var.y = "conMWs", var.x = c("SPD", "clim"))

vm <- VAR(alldataMWb[,c(2,3,12)], p=1)
granger_causality(varmodel = vm, var.y = "decMWs", var.x = c("SPD", "clim"))

vm <- VAR(alldataMWb[,c(2,3,13)], p=1)
granger_causality(varmodel = vm, var.y = "wetwMWs", var.x = c("SPD", "clim"))
custom_commonality_analysis(wetwMWb, SPD.MWb, clim.MWb, 1)

vm <- VAR(alldataMWb[,c(2,3,14)], p=3)
granger_causality(varmodel = vm, var.y = "wetmMWs", var.x = c("SPD", "clim")) # sig spd
custom_commonality_analysis(wetmMWb, SPD.MWb, clim.MWb, 3)

vm <- VAR(alldataMWb[,c(2,3,15)], p=3)
granger_causality(varmodel = vm, var.y = "pasMWs", var.x = c("SPD", "clim"))

vm <- VAR(alldataMWb[,c(2,3,16)], p=1)
granger_causality(varmodel = vm, var.y = "araMWs", var.x = c("SPD", "clim"))

vm <- VAR(alldataMWb[,c(2,3,17)], p=2)
granger_causality(varmodel = vm, var.y = "heaMWs", var.x = c("SPD", "clim")) #clim
custom_commonality_analysis(heaMWb, SPD.MWb, clim.MWb, 2)


### MIDMID ### 3000 BP
#-------------------------------------------------------------------------------
vm <- VAR(alldataMMb[,c(2,3,11)], p=1)
granger_causality(varmodel = vm, var.y = "conMMs", var.x = c("SPD", "clim")) #sig all
custom_commonality_analysis(conMMb, SPD.MMb, clim.MMb, 1)

vm <- VAR(alldataMMb[,c(2,3,12)], p=1)
granger_causality(varmodel = vm, var.y = "decMMs", var.x = c("SPD", "clim"))

vm <- VAR(alldataMMb[,c(2,3,13)], p=1)
granger_causality(varmodel = vm, var.y = "wetwMMs", var.x = c("SPD", "clim"))
custom_commonality_analysis(wetwMMb, SPD.MMb, clim.MMb, 1)

vm <- VAR(alldataMMb[,c(2,3,14)], p=1)
granger_causality(varmodel = vm, var.y = "wetmMMs", var.x = c("SPD", "clim"))

vm <- VAR(alldataMMb[,c(2,3,15)], p=2)
granger_causality(varmodel = vm, var.y = "pasMMs", var.x = c("SPD", "clim"))

vm <- VAR(alldataMMb[,c(2,3,16)], p=1)
granger_causality(varmodel = vm, var.y = "araMMs", var.x = c("SPD", "clim")) # clim
custom_commonality_analysis(araMMb, SPD.MMb, clim.MMb, 1)

vm <- VAR(alldataMMb[,c(2,3,17)], p=2)
granger_causality(varmodel = vm, var.y = "heaMMs", var.x = c("SPD", "clim"))


### SOUTHWEST ### (6000 BP)
#-------------------------------------------------------------------------------
vm <- VAR(alldataSWb[,c(2,3,11)], p=1)
granger_causality(varmodel = vm, var.y = "conSWs", var.x = c("SPD", "clim"))
custom_commonality_analysis(conSWb, SPD.SWb, clim.SWb, 1)

vm <- VAR(alldataSWb[,c(2,3,12)], p=1)
granger_causality(varmodel = vm, var.y = "decSWs", var.x = c("SPD", "clim"))
custom_commonality_analysis(decSWb, SPD.SWb, clim.SWb, 1)

vm <- VAR(alldataSWb[,c(2,3,13)], p=1)
granger_causality(varmodel = vm, var.y = "wetwSWs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSWb[,c(2,3,15)], p=1)
granger_causality(varmodel = vm, var.y = "wetmSWs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSWb[,c(2,3,14)], p=1)
granger_causality(varmodel = vm, var.y = "pasSWs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSWb[,c(2,3,16)], p=1)
granger_causality(varmodel = vm, var.y = "araSWs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSWb[,c(2,3,17)], p=1)
granger_causality(varmodel = vm, var.y = "heaSWs", var.x = c("SPD", "clim"))


### SOUTHMID ### (600 BP)
#-------------------------------------------------------------------------------
vm <- VAR(alldataSMb[,c(2,3,11)], p=1)
granger_causality(varmodel = vm, var.y = "conSMs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSMb[,c(2,3,12)], p=3)
granger_causality(varmodel = vm, var.y = "decSMs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSMb[,c(2,3,13)], p=2)
granger_causality(varmodel = vm, var.y = "wetwSMs", var.x = c("SPD", "clim")) #clim
custom_commonality_analysis(wetwSMb, SPD.SMb, clim.SMb, 2)

vm <- VAR(alldataSMb[,c(2,3,14)], p=3)
granger_causality(varmodel = vm, var.y = "wetmSMs", var.x = c("SPD", "clim")) #spd
custom_commonality_analysis(wetmSMb, SPD.SMb, clim.SMb, 3)

vm <- VAR(alldataSMb[,c(2,3,15)], p=3)
granger_causality(varmodel = vm, var.y = "pasSMs", var.x = c("SPD", "clim")) #both
custom_commonality_analysis(pasSMb, SPD.SMb, clim.SMb, 3)

vm <- VAR(alldataSMb[,c(2,3,16)], p=2)
granger_causality(varmodel = vm, var.y = "araSMs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSMb[,c(2,3,17)], p=1)
granger_causality(varmodel = vm, var.y = "heaSMs", var.x = c("SPD", "clim"))


### AFTER THE ONSET OF FARMING #------------------------------------------------

### NORTH ### (2300 - 1400 BP) DATASET TOO SHORT !!!
#-------------------------------------------------------------------------------
vm <- VAR(alldataNa[,c(2,3,11)], p=4) 
granger_causality(varmodel = vm, var.y = "conNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNa[,c(2,3,12)], p=4)
granger_causality(varmodel = vm, var.y = "decNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNa[,c(2,3,13)], p=4)
granger_causality(varmodel = vm, var.y = "wetwNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNa[,c(2,3,14)], p=4)
granger_causality(varmodel = vm, var.y = "wetmNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNa[,c(2,3,15)], p=4)
granger_causality(varmodel = vm, var.y = "pasNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNa[,c(2,3,16)], p=4)
granger_causality(varmodel = vm, var.y = "araNs", var.x = c("SPD", "clim"))

vm <- VAR(alldataNa[,c(2,3,17)], p=4)
granger_causality(varmodel = vm, var.y = "heaNs", var.x = c("SPD", "clim"))

### SOUTHEAST ### 4000 BP
#-------------------------------------------------------------------------------
vm <- VAR(alldataSEa[,c(2,3,11)], p=3)
granger_causality(varmodel = vm, var.y = "conSEs", var.x = c("SPD", "clim"))
custom_commonality_analysis(conSEa, SPD.SEa, clim.SEa, 3)

vm <- VAR(alldataSEa[,c(2,3,12)], p=1)
granger_causality(varmodel = vm, var.y = "decSEs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSEa[,c(2,3,13)], p=1)
granger_causality(varmodel = vm, var.y = "wetwSEs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSEa[,c(2,3,14)], p=1)
granger_causality(varmodel = vm, var.y = "wetmSEs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSEa[,c(2,3,15)], p=1)
granger_causality(varmodel = vm, var.y = "pasSEs", var.x = c("SPD", "clim"))
custom_commonality_analysis(pasSEa, SPD.SEa, clim.SEa, 1)

vm <- VAR(alldataSEa[,c(2,3,16)], p=1)
granger_causality(varmodel = vm, var.y = "araSEs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSEa[,c(2,3,17)], p=1)
granger_causality(varmodel = vm, var.y = "heaSEs", var.x = c("SPD", "clim"))
custom_commonality_analysis(pasSEa, SPD.SEa, clim.SEa, 1)


### MIDWEST ### 3000 BP
#-------------------------------------------------------------------------------
vm <- VAR(alldataMWa[,c(2,3,11)], p=3)
granger_causality(varmodel = vm, var.y = "conMWs", var.x = c("SPD", "clim"))

vm <- VAR(alldataMWa[,c(2,3,12)], p=1)
granger_causality(varmodel = vm, var.y = "decMWs", var.x = c("SPD", "clim")) #spd
custom_commonality_analysis(decMWa, SPD.MWa, clim.MWa, 1)

vm <- VAR(alldataMWa[,c(2,3,13)], p=1)
granger_causality(varmodel = vm, var.y = "wetwMWs", var.x = c("SPD", "clim")) #sig spd
custom_commonality_analysis(wetwMWa, SPD.MWa, clim.MWa, 1)

vm <- VAR(alldataMWa[,c(2,3,14)], p=1)
granger_causality(varmodel = vm, var.y = "wetmMWs", var.x = c("SPD", "clim")) # sig all
custom_commonality_analysis(wetmMWa, SPD.MWa, clim.MWa, 1) 

vm <- VAR(alldataMWa[,c(2,3,15)], p=1)
granger_causality(varmodel = vm, var.y = "pasMWs", var.x = c("SPD", "clim"))

vm <- VAR(alldataMWa[,c(2,3,16)], p=1)
granger_causality(varmodel = vm, var.y = "araMWs", var.x = c("SPD", "clim")) #sig spd
custom_commonality_analysis(araMWa, SPD.MWa, clim.MWa, 1)

vm <- VAR(alldataMWa[,c(2,3,17)], p=3)
granger_causality(varmodel = vm, var.y = "heaMWs", var.x = c("SPD", "clim"))


### MIDMID ### 3000 BP
#-------------------------------------------------------------------------------
vm <- VAR(alldataMMa[,c(2,3,11)], p=1)
granger_causality(varmodel = vm, var.y = "conMMs", var.x = c("SPD", "clim")) #clim
custom_commonality_analysis(conMMa, SPD.MMa, clim.MMa, 1)

vm <- VAR(alldataMMa[,c(2,3,12)], p=2)
granger_causality(varmodel = vm, var.y = "decMMs", var.x = c("SPD", "clim"))

vm <- VAR(alldataMMa[,c(2,3,13)], p=1)
granger_causality(varmodel = vm, var.y = "wetwMMs", var.x = c("SPD", "clim"))

vm <- VAR(alldataMMa[,c(2,3,14)], p=1)
granger_causality(varmodel = vm, var.y = "wetmMMs", var.x = c("SPD", "clim"))

vm <- VAR(alldataMMa[,c(2,3,15)], p=1)
granger_causality(varmodel = vm, var.y = "pasMMs", var.x = c("SPD", "clim")) #sig spd
custom_commonality_analysis(pasMMa, SPD.MMa, clim.MMa, 1)

vm <- VAR(alldataMMa[,c(2,3,16)], p=1)
granger_causality(varmodel = vm, var.y = "araMMs", var.x = c("SPD", "clim")) #sig spd
custom_commonality_analysis(araMMa, SPD.MMa, clim.MMa, 1)

vm <- VAR(alldataMMa[,c(2,3,17)], p=1)
granger_causality(varmodel = vm, var.y = "heaMMs", var.x = c("SPD", "clim"))
custom_commonality_analysis(heaMMa, SPD.MMa, clim.MMa, 1)


### SOUTHWEST ### 6000 BP
#-------------------------------------------------------------------------------
vm <- VAR(alldataSWa[,c(2,3,11)], p=1)
granger_causality(varmodel = vm, var.y = "conSWs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSWa[,c(2,3,12)], p=1)
granger_causality(varmodel = vm, var.y = "decSWs", var.x = c("SPD", "clim")) #sig spd
custom_commonality_analysis(decSWa, SPD.SWa, clim.SWa, 1)

vm <- VAR(alldataSWa[,c(2,3,13)], p=1)
granger_causality(varmodel = vm, var.y = "wetwSWs", var.x = c("SPD", "clim")) #sig spd
custom_commonality_analysis(wetwSWa, SPD.SWa, clim.SWa, 1)

vm <- VAR(alldataSWa[,c(2,3,15)], p=1)
granger_causality(varmodel = vm, var.y = "wetmSWs", var.x = c("SPD", "clim")) #all
custom_commonality_analysis(wetmSWa, SPD.SWa, clim.SWa, 1)

vm <- VAR(alldataSWa[,c(2,3,14)], p=3)
granger_causality(varmodel = vm, var.y = "pasSWs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSWa[,c(2,3,16)], p=3)
granger_causality(varmodel = vm, var.y = "araSWs", var.x = c("SPD", "clim")) #
custom_commonality_analysis(heaSWa, SPD.SWa, clim.SWa, 3)

vm <- VAR(alldataSWa[,c(2,3,17)], p=3)
granger_causality(varmodel = vm, var.y = "heaSWs", var.x = c("SPD", "clim")) #sig all


### SOUTHMID ### 6000 BP
#-------------------------------------------------------------------------------
vm <- VAR(alldataSMa[,c(2,3,11)], p=3)
granger_causality(varmodel = vm, var.y = "conSMs", var.x = c("SPD", "clim")) #sig spd
custom_commonality_analysis(conSMa, SPD.SMa, clim.SMa, 3)

vm <- VAR(alldataSMa[,c(2,3,12)], p=2)
granger_causality(varmodel = vm, var.y = "decSMs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSMa[,c(2,3,13)], p=1)
granger_causality(varmodel = vm, var.y = "wetwSMs", var.x = c("SPD", "clim")) #clim
custom_commonality_analysis(wetwSMa, SPD.SMa, clim.SMa, 1)

vm <- VAR(alldataSMa[,c(2,3,14)], p=2)
granger_causality(varmodel = vm, var.y = "wetmSMs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSMa[,c(2,3,15)], p=2)
granger_causality(varmodel = vm, var.y = "pasSMs", var.x = c("SPD", "clim")) #clim
custom_commonality_analysis(pasSMa, SPD.SMa, clim.SMa, 2)

vm <- VAR(alldataSMa[,c(2,3,16)], p=2)
granger_causality(varmodel = vm, var.y = "araSMs", var.x = c("SPD", "clim"))

vm <- VAR(alldataSMa[,c(2,3,17)], p=2)
granger_causality(varmodel = vm, var.y = "heaSMs", var.x = c("SPD", "clim"))
