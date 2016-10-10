#Initialize
setwd("~/Google Drive/Data/CPG2/Activity Assays")
closeAllConnections()
rm(list = ls())
library(xlsx)
source("~/Google Drive/Scripts/R/mm_functions.R")
source("~/Google Drive/scripts/R/tecan_io.R")

#Load the contents of the various sheets into variables and transpose.
mtx <-
  read_tecan("2016_07_09 - MTX and PAB-E Assay for CPG2 Circular Permutations.xlsx",
             "MTX")

#Do the linear regression loop.  Store the result in reg_results
#reg_results <- regression_loop(mtx, "Time [s]", "MTX1-S2N")

datalist <- c("MTX1-WT", "MTX1-S2N")
result_list <- data.frame()

for (dataset in datalist) {
  reg_results <- regression_loop(mtx, "Time [s]", dataset)
  if (is.numeric(dataset)) {
    new_result <- data.frame(as.numeric(dataset), reg_results[[1]])
  } else {
    new_result <- data.frame(dataset, reg_results[[1]])
  }
  result_list <- rbind(result_list, new_result)
}

names(result_list) <- c("Dataset", "Rate")

result_list$Dataset <- c(1, 3)

#Check if the data frame is numeric to do non-linear regression
if (is.numeric(result_list$Dataset) && is.numeric(result_list$Rate)) {
  plot(result_list$Dataset, result_list$Rate)
  conc = result_list$Dataset
  rates = result_list$Rate
  #mmfit = nls(result_list$Rate ~ vmax*result_list$Dataset/(km + result_list$Dataset), start = list(vmax = max(result_list$Rate, km = mean(result_list$Dataset))))
  #mmfit = nls(rates ~ vmax * conc/(km + conc), start = list(vmax = max(result_list$Rate, km = mean(result_list$Dataset))))
  mmfit = nls(rates ~ vmax * conc/(km + conc), start = list(vmax = max(rates), km = mean(conc)), control = c(maxiter = 5000))
  #mmfit = nls(result_list$Rate ~ vmax * result_list$Dataset/(km + result_list$Dataset), start = list(vmax = -0.000130, km = 2))
  summary(mmfit)
}