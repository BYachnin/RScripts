#Some scripts to deal with output from Rosetta.

#Load in functions to read files from disk.
#source("https://raw.githubusercontent.com/BYachnin/RScripts/master/io.R")

#' Plots a graph of two score terms from a score file.
#'
#' @details If the axes are not given, assume they are total_score vs. rmsd.
#'
#' @param datatable A table containing a Rosetta-style scorefile data.
#' @param xterm The name of the scoreterm to be plotted on the x-axis.  Defaults to "rmsd".
#' @param yterm The name of the scoreterm to be plotted on the y-axis.  Defaults to "total_score".
#' @param title A title for the plot, if desired.
#' @export
#' @seealso \code{\link{score_plot}}
ros_plot <- function(datatable, xterm = "rmsd", yterm = "total_score", title = "") {
  #graphics.off()
  plot(datatable[,xterm], datatable[,yterm], xlab = xterm, ylab = yterm, main = title)
}

#' Loads a Rosetta-style scorefile (*.sc) and generates a plot.
#'
#' @inherit read_rosetta_score params return
#' @inherit ros_plot params details
#' @export
#' @seealso \code{\link{ros_plot}}, \code{\link{read_rosetta_score}}
score_plot <- function(scorefile, xterm = "rmsd", yterm = "total_score", title = scorefile) {
  scores <- read_rosetta_score(scorefile)
  ros_plot(scores, xterm = xterm, yterm = yterm, title = title)
  return(scores)
}

#' Return a list of the top designs in a Rosetta scorefile, based on a scoreterm of choice.
#'
#' @details The first and last design can be customized.
#'
#' @inherit ros_plot datatable
#' @param scoreterm The name of the scoreterm to be used to sort.
#' @param top The first design to include.  For example, if it is 3, the top two designs will be skipped.
#' @param number The total number of designs to list.
#' @param descending If true, the highest score will be considered the "best."  If false (default), the lowest score will be considered the "best."
#' @return A list of the design names requested.
#' @export
#' @seealso \code{\link{score_plot}} \code{\link{read_rosetta_score}}
best_designs <- function(datatable, scoreterm = "total_score", top = 1, number = 50, descending = FALSE) {
  #Sort the data according to scoreterm.
  sortedscores <- scores[order(scores[,scoreterm], decreasing = descending),]
  #Pull out the names of the desired designs.
  selnames <- rownames(sortedscores[top:(top + number - 1),])
  return(selnames)
}
