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

#Function to load a Rosetta score file into a variable.
#Remove the header row and the "SCORE:" leader string, and set the row names to description.
read_rosetta_score <- function(scorefile) {
  rosettadata <-
    (read.table(scorefile, header = TRUE, skip = 1, row.names = "description"))
  rosettadata <- rosettadata[,-1]
  return(rosettadata)
}