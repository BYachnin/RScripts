#Initialize
setwd("~/Google Drive/scripts/R")
closeAllConnections()
rm(list = ls())
library(xlsx)
library(brahm)
library(utils)
source("mm_functions.R")

infile <- "mm_out.csv"
#infile <- ""
outfile <- "mm_out.csv"

#wt_data <- read_spectramax("9-2-16WTIso_NH.xlsx", "Test")
wt_data <- read.xlsx("9-2-16WTIso_NH.xlsx", "Test", header = FALSE)

#Go through and remove the leading X and trailing .1 that R puts in.
#for (concidx in seq_len(length(subconcentrations))) {
  #if (substring(subconcentrations[concidx], 1, 1) == 'X') {
    #subconcentrations[concidx] <- substring(subconcentrations[concidx], 2)
    #if (grep('.', subconcentrations[concidx], value = FALSE)) {
      #subconcentrations[concidx] <- sub('\\..$', '', subconcentrations[concidx])
    #}
  #}
#}

#mm_analysis <- mm_kinetics(wt_data, infile, outfile)