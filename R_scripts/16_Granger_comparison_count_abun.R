libs <- c("dplyr","ggplot2")

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
rm(list = setdiff(ls(), "relative_abun"))

options(scipen=999)

source("./Functions/Sig_test.R")
#ALL DATA
granger_abun = read.csv("./Results/Granger_causality/Granger_results_allData_abun.csv")
granger_count = read.csv("./Results/Granger_causality/Granger_results_allData_count.csv")

# RUN CORRELATION TESTS
x = cor.test(granger_abun$F, granger_count$F)
cor_F = round(x$estimate[[1]], 3)
p_F = x$p.value[[1]]

y = cor.test(granger_abun$Chisq, granger_count$Chisq)
cor_chi = round(y$estimate[[1]], 3)
p_chi = x$p.value[[1]]

# DETERMINE SIGNIFICANCE
sig_chi = sig_test(p_chi)
sig_F = sig_test(p_F)

# DATA CLEANING
F_abun_sub = granger_abun %>%
  dplyr::select(F, Causality) %>%
  dplyr::mutate(Type = "F") %>% 
  dplyr::rename(Abun = F)

Chi_abun_sub = granger_abun %>%
  dplyr::select(Chisq, Causality) %>%
  dplyr::mutate(Type = "χ2") %>% 
  dplyr::rename(Abun = Chisq)

F_count_sub = granger_count %>%
  dplyr::select(F, Causality) %>%
  dplyr::mutate(Type = "F") %>% 
  dplyr::rename(Count = F)

Chi_count_sub = granger_count %>%
  dplyr::select(Chisq, Causality) %>%
  dplyr::mutate(Type = "χ2") %>% 
  dplyr::rename(Count = Chisq)

all_abun_sub = rbind(Chi_abun_sub, F_abun_sub)
all_count_sub = rbind(Chi_count_sub, F_count_sub)
all_data = merge(all_abun_sub, all_count_sub, by = c("Causality", "Type"))

png(filename="./Results/Plots/granger_causality_comparison.png", width = 900, height = 600,)

ggplot(all_data, aes(x=Abun, y=Count, color=Type, shape=Type)) +
  geom_point() + 
  geom_smooth(method=lm,) +
  xlab("Pollen relative abundance") + ylab("Pollen count") +
  theme(legend.title=element_blank()) +
  annotate("text", x=20, y=35, label= paste0("r(χ2) = ", cor_chi, sig_chi), size = 8) +
  annotate("text", x=19.8, y=38, label= paste0("r(F) = ", cor_F, sig_F), size = 8) +
  theme(text = element_text(size = 25))

dev.off()

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

F_abun_sub = granger_abun[c("F")]
F_abun_sub$Type = "F" 
