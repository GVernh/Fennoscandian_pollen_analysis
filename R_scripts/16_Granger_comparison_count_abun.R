#ALL DATA
granger_abun = read.csv("./Results/Granger_causality/Granger_results_allData_abun.csv")
granger_count = read.csv("./Results/Granger_causality/Granger_results_allData_count.csv")

cor(granger_abun$F, granger_count$F)
cor(granger_abun$Chisq, granger_count$Chisq)

# TO DO: MAKE PLOTS FOR APPENDICES
plot(granger_abun$F, granger_count$F)
plot(granger_abun$Chisq, granger_count$Chisq)

# BEFORE FARMING
granger_abun_before = read.csv("./Results/Granger_causality/Granger_results_beforeFarming_abun.csv")
granger_count_before = read.csv("./Results/Granger_causality/Granger_results_beforeFarming_count.csv")

cor(granger_abun_before$F, granger_count_before$F)
cor(granger_abun_before$Chisq, granger_count_before$Chisq)

# AFTER FARMING
granger_abun_after = na.omit(read.csv("./Results/Granger_causality/Granger_results_afterFarming_abun.csv"))
granger_count_after = na.omit(read.csv("./Results/Granger_causality/Granger_results_afterFarming_count.csv"))

cor(granger_abun_after$F, granger_count_after$F)
cor(granger_abun_after$Chisq, granger_count_after$Chisq)
