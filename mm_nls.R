#Initialize
setwd("~/Google Drive/scripts/R")
closeAllConnections()
rm(list = ls())
library(xlsx)
library(brahm)
source("mm_functions.R")

wt_data <- read_spectramax("9-2-16WTIso_NH.xlsx", "Test2")

result_list <- data.frame()

for (dataset in colnames(wt_data[-1])) {
  reg_results <- regression_loop(wt_data, colnames(wt_data[1]), dataset)
  if (is.numeric(dataset)) {
    new_result <- data.frame(as.numeric(dataset), reg_results[[1]])
  } else {
    new_result <- data.frame(dataset, reg_results[[1]])
  }
  result_list <- rbind(result_list, new_result)
}

names(result_list) <- c("Dataset", "Rate")

#Hard code in the result_list dataset names to force it to be numeric for testing.
result_list$Dataset <- c(10, 20, 40)

#Check if the data frame is numeric to do non-linear regression
if (is.numeric(result_list$Dataset) && is.numeric(result_list$Rate)) {
  mm_fit <- mm_nls(result_list$Dataset, result_list$Rate)
}