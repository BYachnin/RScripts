#Initialize library
#library(xlsx)

#' Loads the contents of Tecan-generated Excel sheets into a data frame.
#'
#' @param excelfile The name of the Excel file containing the sheet.
#' @param excelsheet The name of the sheet in the Excel file containing your data.
#' @return A data frame containing the Tecan data.
#' @export
#' @seealso \code{\link[xlsx]{read.xlsx}}
read_tecan <- function(excelfile, excelsheet) {
  tecandata <-
    t(xlsx::read.xlsx((excelfile), sheetName = excelsheet, header = FALSE))
  #Make the header row the row name, and delete.
  colnames(tecandata) = tecandata[1,]
  tecandata = tecandata[-1,]
  return(tecandata)
}

#' Loads the contents of Tecan-generated Excel sheets into a data frame.
#' @inherit read_tecan params seealso
#' @return A data frame containing the Spectramax data.
#' @export
read_spectramax <- function(excelfile, excelsheet) {
  spectramaxdata <-
    (xlsx::read.xlsx((excelfile), sheetName = excelsheet))
  return(spectramaxdata)
}

#' Loads a Rosetta-style score file into a data frame.
#'
#' @param scorefile The path and filename of the Rosetta-style scorefile to load.
#' @return A data frame object containing the data from the scorefile given in \code{scorefile}.
#' @export
#' @seealso \code{\link{read.table}}
read_rosetta_score <- function(scorefile) {
  #Remove the header row and the "SCORE:" leader string, and set the row names to description.
  rosettadata <-
    (read.table(scorefile, header = TRUE, skip = 1, row.names = "description"))
  rosettadata <- rosettadata[,-1]
  return(rosettadata)
}
