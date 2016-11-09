#Initialize
setwd("~/Google Drive/scripts/R")
closeAllConnections()
rm(list = ls())
library(xlsx)
library(brahm)
source("mm_functions.R")

wt_data <- read.xlsx(file = "In Velocities_9_2_16_NH.xlsx", sheetName = "Iso", startRow = 2, endRow = 18)

#datalist <- wt_data$Iso.Conc.
#result_list <- data.frame()
result_list <- data.frame(wt_data$Iso.Conc., wt_data$Vis.Abs.U)
mm_nls(result_list$wt_data.Iso.Conc., result_list$wt_data.Vis.Abs.U)

#for (dataset in datalist) {
  #reg_results <- regression_loop(mtx, "Time [s]", dataset)
  #if (is.numeric(dataset)) {
    #new_result <- data.frame(as.numeric(dataset), reg_results[[1]])
  #} else {
    #new_result <- data.frame(dataset, reg_results[[1]])
  #}
  #result_list <- rbind(result_list, new_result)
#}

#names(result_list) <- c("Dataset", "Rate")

#Check if the data frame is numeric to do non-linear regression
#if (is.numeric(result_list$Dataset) && is.numeric(result_list$Rate)) {
  #plot(result_list$Dataset, result_list$Rate)
  #conc = result_list$Dataset
  #rates = result_list$Rate
  #mmfit = nls(rates ~ vmax * conc/(km + conc), start = list(vmax = max(rates), km = mean(conc)), control = c(maxiter = 5000))
  #summary(mmfit)
#}
