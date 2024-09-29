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

### commonality analysis ###
test1 <- custom_commonality_analysis(conSEs, SPD.SE, clim.SE, 1)
test4 <- custom_commonality_analysis(conSEs, SPD.SE, clim.SE, 4)
test7 <- custom_commonality_analysis(conSEs, SPD.SE, clim.SE, 7)
