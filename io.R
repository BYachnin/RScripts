#Initialize library
library(xlsx)

#Function to load contents of Tecan sheets into variables and transpose.
read_tecan <- function(excelfile, excelsheet) {
  tecandata <-
    t(read.xlsx((excelfile), sheetName = excelsheet, header = FALSE))
  #Make the header row the row name, and delete.
  colnames(tecandata) = tecandata[1,]
  tecandata = tecandata[-1,]
  return(tecandata)
}

#Function to load contents of Spectramax sheets into variables and transpose.
read_spectramax <- function(excelfile, excelsheet) {
  spectramaxdata <-
    (read.xlsx((excelfile), sheetName = excelsheet))
  return(spectramaxdata)
}