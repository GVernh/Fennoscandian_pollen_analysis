libs <- c("dplyr","ggplot2", "remotes", "stringr","tidyr","rnaturalearth", "rcarbon",
          "sf", "rnaturalearthdata", "Bchron", "stats", "zoo", "graphics", "remotes",
          "ggspatial", "magrittr", "ecp", "mcp", "ggpubr", "purrr","lubridate", "fpp2", 
          "smooth", "TTR", "vars","modelr", "glm2", "timetk", "bruceR", 
          "lmtest", "scales")

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

lipdR_check <- "lipdR" %in% rownames(
  installed.packages())

if (lipdR_check == F) {
  remotes::install_github("nickmckay/lipdR")
}
invisible(library(lipdR))
rm(list=ls())

dir.create(file.path("./", "Processed_data"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "Pollen_data"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "archeological_data"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "Footprint_calibration_results"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "SPD_data"), showWarnings = FALSE)
dir.create(file.path("./", "Plots"), showWarnings = FALSE)
dir.create(file.path("./Plots/", "Null_model_plots"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "Climate"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "lpd_datasets"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "gridded_data"), showWarnings = FALSE)
dir.create(file.path("Plots"), showWarnings = FALSE)
dir.create(file.path("./Plots/", "Datapoint_plots" ), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "LCC_data"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/LCC_data/", "LCC_abun"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "mcp_models"), showWarnings = FALSE)
dir.create(file.path("./Plots/", "mcp_plots"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "Full_datasets"), showWarnings = FALSE)
dir.create(file.path("./Processed_data/", "Cross_validation"), showWarnings = FALSE)
dir.create(file.path("./", "Results"), showWarnings = FALSE)
dir.create(file.path("./Results/", "Granger_causality"), showWarnings = FALSE)
dir.create(file.path("./Results/", "Plots"), showWarnings = FALSE)

source("./R_scripts/1_Script1_bigdf_with_familynames.R")
source("./R_scripts/2_Compute_relative_abundance.R")
source("./R_scripts/3_archaeological_data.R")
source("./R_scripts/4_Calibrate_archeological_data.R")
source("./R_scripts/5_SPD.R")
source("./R_scripts/6_Plot_human_calibration_results.R")
source("./R_scripts/7_Climatic_data.R")
source("./R_scripts/8_all_lat_long_grids.R")
source("./R_scripts/9_LCC.R")
source("./R_scripts/10_model_testing.R")
source("./R_scripts/11_smoothing.R")
source("./R_scripts/12_cross_validation.R")
source("./R_scripts/13_grangercausality.R")
source("./R_scripts/14_final_plots.R")
print("Results and plots for Granger causality models can be found in ./Results/")
print("Maps and calibration plots can be found in ./Plots/ ")
