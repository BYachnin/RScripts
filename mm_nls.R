#Initialize
setwd("~/Google Drive/scripts/R")
closeAllConnections()
rm(list = ls())
library(xlsx)
library(brahm)
library(utils)
source("mm_functions.R")

infile <- "mm_out.csv"
outfile <- "mm_out.csv"

wt_data <- read_spectramax("9-2-16WTIso_NH.xlsx", "Test")

#Set up a list of the dataset concentrations.
subconcentrations <- colnames(wt_data[-1])

#Go through and remove the leading X and trailing .1 that R puts in.
for (concidx in seq_len(length(subconcentrations))) {
  if (substring(subconcentrations[concidx], 1, 1) == 'X') {
    subconcentrations[concidx] <- substring(subconcentrations[concidx], 2)
    if (grep('.', subconcentrations[concidx], value = FALSE)) {
      subconcentrations[concidx] <- sub('\\..$', '', subconcentrations[concidx])
    }
  }
}

result_list <- data.frame()

#If infile is not given, run through all datasets first.  Otherwise, skip this and load the input data.
if (infile == "") {
  for (dataset in colnames(wt_data[-1])) {
    reg_results <- regression_loop(wt_data, colnames(wt_data[1]), dataset)
    if (is.numeric(dataset)) {
      new_result <- data.frame(as.numeric(dataset), reg_results[[1]], reg_results[[3]], reg_results[[4]], FALSE)
    } else {
      new_result <- data.frame(dataset, reg_results[[1]], reg_results[[3]], reg_results[[4]], FALSE)
    }
    result_list <- rbind(result_list, new_result)
  } 
} else {
  #Read in the old result_list from the input file
  result_list <- read.csv(infile)
  #Ensure that the headings in result_list and those derived from the raw data match up.
  #Otherwise, the data are probably incompatible and we should quit.
  if (! identical(as.numeric(result_list$Dataset), as.numeric(subconcentrations)))
    stop("The raw data file and the results file are incompatible.")
}

names(result_list) <- c("Dataset", "Rate", "Min Time", "Max Time", "Exclude?")

#Move the numeric version of the dataset names (subconcentrations) into result_list$Dataset.
result_list$Dataset <- as.numeric(subconcentrations)

#Check if the data frame is numeric to do non-linear regression
if (is.numeric(result_list$Dataset) && is.numeric(result_list$Rate)) {
  #Perform the non-linear regression, and then allow the user to re-process.
  while (TRUE) {
    #Do the non-linear regression.
    mm_fit <- mm_nls(result_list$Dataset, result_list$Rate, result_list$`Exclude?`)
    print(result_list)
    print(mm_fit)
    #Provide a menu with all of the datasets.  Re-run the linear regression for those.
    setnumber <- menu(c(result_list$Dataset, "Include/exclude datasets"), title = "Select a dataset to re-process (0 to quit):")
    #If the number from the menu is 0, we're done.  Break out of the while loop.
    if (setnumber == 0) {break}
    #If the number from the menu is the highest value, switch to the exclude dataset mode.
    if (setnumber == length(result_list$Dataset)+1) {
      #Stay in include/exclude mode until the user chooses 0.
      switchset <- 1
      while (switchset != 0) {
        #Get the set to toggle.
        switchset <- menu(paste(result_list$Dataset, result_list$`Exclude?`, sep = ': '), title = "Select a dataset to toggle include/exclude state (0 to quit):")
        #Toggle switchset.
        result_list[switchset, 5] <- !result_list[switchset, 5]
      }
    } else {
      #Otherwise, reprocess the selected datapoint.
      #Print out the old data.
      print(result_list[setnumber,])
      #Re-do linear regression.
      reg_results <- regression_loop(wt_data, colnames(wt_data[1]), colnames(wt_data[1+setnumber]), result_list[setnumber,3], result_list[setnumber,4])
      #Replace the values in result_list.
      result_list[setnumber,] <- data.frame(result_list[setnumber, 1], reg_results[[1]], reg_results[[3]], reg_results[[4]], result_list[setnumber, 5])
    }
  }
}

#Output the result_list to outfile.
write.csv(result_list, file = outfile, row.names = FALSE)
#Output final mm_fit
print(mm_fit)