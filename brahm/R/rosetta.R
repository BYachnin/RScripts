#Some scripts to deal with output from Rosetta.

#Load in functions to read files from disk.
source("https://raw.githubusercontent.com/BYachnin/RScripts/master/io.R")

#Plot a graph of two score terms from a score file.
#If the axes are not given, assume they are total_score vs. rmsd.
ros_plot <- function(datatable, xterm = "rmsd", yterm = "total_score", title = "") {
  #graphics.off()
  plot(datatable[,xterm], datatable[,yterm], xlab = xterm, ylab = yterm, main = title)
}

#Combine the load data and plotting functions.  Return the score file.
score_plot <- function(scorefile, xterm = "rmsd", yterm = "total_score", title = scorefile) {
  scores <- read_rosetta_score(scorefile)
  ros_plot(scores, xterm = xterm, yterm = yterm, title = title)
  return(scores)
}