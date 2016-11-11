#Initialize
setwd("~/Google Drive/scripts/R")
closeAllConnections()
rm(list = ls())
library(xlsx)
library(brahm)
library(utils)
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
  #Perform the non-linear regression, and then allow the user to re-process.
  while (TRUE) {
    #Do the non-linear regression.
    mm_fit <- mm_nls(result_list$Dataset, result_list$Rate)
    summary(mm_fit)
    #Provide a menu with all of the datasets.  Re-run the linear regression for those.
    setnumber <- menu(result_list$Dataset, title = "Select a dataset to re-process:")
    #If the number from the menu is 0, we're done.  Break out of the while loop.
    if (setnumber == 0) {break}
    reg_results <- regression_loop(wt_data, colnames(wt_data[1]), colnames(wt_data[1+setnumber]))
    #Replace the value in result_list.
    result_list[setnumber, 2] <- reg_results[[1]]
  }
}