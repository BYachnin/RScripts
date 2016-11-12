#Initialize library
library(plotrix)

#Function to calculate the regression, plot the data, and return the regression data
make_reggraph <- function(fulldata, lowx, highx) {
  #Plot the data as 2x1 grid (traces, spectra)
  par(mfrow = c(1, 2))   #Side-by-side plots
  par(mar = c(5, 5, 2, 1))	#Margins
  datacol = "red"       #Graph data colour
  
  #Add 10% extra space so that the a bit more of the data are plotted than is being used for fitting.
  width = abs(highx - lowx)
  graphlow = lowx - 0.1 * width
  graphhigh = highx + 0.1 * width
  #Determine the corresponding y-values for graphlow and graphhigh.
  lowy = fulldata[which(fulldata$x == lowx), 2]
  highy = fulldata[which(fulldata$x == highx), 2]
  #Swap low and high y values in cases where the observable decreases during the assay.
  if (lowy > highy) {
    tmp = lowy
    lowy = highy
    highy = tmp
  }
  #Add 10% extra space in the y direction.  We need this to avoid plotting full extent of y in the zoomed plot.
  widthy = highy - lowy
  graphlowy = lowy - 0.1 * widthy
  graphhighy = highy + 0.1 * widthy
  
  #Subset the data
  reg_subset <-
    subset(fulldata, x >= as.numeric(lowx) & x <= as.numeric(highx))
  
  #Calculate a linear regression on the entire subregdata range.
  regression <- lm(reg_subset$y ~ reg_subset$x)
  print(regression)
  
  #Plot the data in full
  plot(
    fulldata$x,
    fulldata$y,
    type = 'l',
    col = datacol,
    xlab = 'Time',
    ylab = 'Activity'
  )
  title(main = "Full dataset")
  
  #Add a thicker line for the selected region
  lines(reg_subset, lwd=3, col=datacol)
  
  #Add the linear regression line over the approriate range
  ablineclip(regression,
             col = "black",
             x1 = lowx,
             x2 = highx,
             lwd=2)

  #Plot the zoomed-in data
  plot(
    fulldata$x,
    fulldata$y,
    type = 'l',
    xlim = c(graphlow, graphhigh),
    ylim = c(graphlowy, graphhighy),
    col = datacol,
    xlab = 'Time',
    ylab = 'Activity'
  )
  title(main = "Zoomed dataset")
  
  #Add a thicker line for the selected region
  lines(reg_subset, lwd=3, col=datacol)
  
  #Add the linear regression line over the approriate range
  ablineclip(regression,
             col = "black",
             x1 = lowx,
             x2 = highx,
             lwd=2)
  
  #Return the regression results.
  return(regression)
}

#Pass a data matrix and two columns to this function.
#Optionally, include a time minimum and maximum that sets the regression limits on the first round.
#(If those arguments are not set, first round of regression will cover the entire dataset.)
#First, the data will be plotted with a regression line covering the whole dataset.
#The user will then be prompted to enter alternate limits for the dataset.  The data will be re-plotted.
#This will continue until the user hits enter for both data limits.
#At this point, the function will return the selected data limits and the regression object.
regression_loop <-
  function(kineticdata, timeheader, activityheader, startreg = min(as.numeric(kineticdata[, timeheader])), endreg = max(as.numeric(kineticdata[, timeheader]))) {
    #Going in, we will go through the loop.
    loop = TRUE
    
    #Convert the appropriate two columns into a data frame for linear regression.
    regdata = data.frame(as.numeric(kineticdata[, timeheader]), as.numeric(kineticdata[, activityheader]))
    #Call those columns x and y
    colnames(regdata) <- c('x', 'y')
    
    #Calculate the regression and plot the data.  Store the regression in cur_result
    cur_result <-
      make_reggraph(regdata, as.numeric(startreg), as.numeric(endreg))
    
    while (loop) {
      #Save the old start and end points of the regression.
      oldstart <- startreg
      oldend <- endreg
      #Ask the user for new start and/or end points.
      startreg <-
        readline(paste("Enter minimum (", oldstart, "): ", sep = ""))
      endreg   <-
        readline(paste("Enter maximum (", oldend, "): ", sep = ""))
      
      #If the user pressed enter for both limits, stop looping.
      if (startreg == "" & endreg == "") {
        loop = FALSE
      }
      
      #If the user pressed enter for the start limit, use the old start limit
      if (startreg == "") {
        startreg = oldstart
      }
      
      #If the user pressed enter for the end limit, use the old end limit
      if (endreg == "") {
        endreg = oldend
      }
      
      #Subset the data to include only those x-values within the requested range.
      subregdata <-
        subset(regdata, x >= as.numeric(startreg) & x <= as.numeric(endreg))
      
      #The values entered by the user for startreg and endreg are probably not the true range.
      #Determine the actual min and max values, and replace startreg and endreg with those.
      #This will allow the user to see what the true start and end values are.
      startreg <- min(subregdata$x)
      endreg <- max(subregdata$x)
      
      print(paste("Range: ", startreg, " - ", endreg, sep = ""))
      
      #Do the linear regression with the selected start and end limits.  Store the regression in cur_result
      #cur_result <- make_reggraph(kineticdata, timeheader, activityheader, as.numeric(startreg), as.numeric(endreg))
      cur_result <-
        make_reggraph(regdata, as.numeric(startreg), as.numeric(endreg))
    }
    
    #Now that the regression loop is finished, return the final regression results, as well as the start and end points
    print(paste("Final minimum: ", startreg, sep = ""))
    print(paste("Final maximum: ", endreg, sep = ""))
    print(paste("Final slope: ", cur_result$coefficients[2], sep = ""))
    return(c(
      as.numeric(cur_result$coefficients[2]),
      as.numeric(cur_result$coefficients[[1]]),
      as.numeric(startreg),
      as.numeric(endreg),
      cur_result
    ))
  }

#Pass concentration and rate data to this function.
#Based on this data, calculate a best-fit using non-linear regression to the
#Michaelis-Menten equation.  It will print the regression table, and 
#make a graph of the results.

mm_nls <- function(concentration, rates) {
  #Plot the raw data
  plot(concentration, rates, main = "Michaelis-Menten Kinetics", xlab = "[Substrate]", ylab = "Initial Rate")
  #Do the non-linear regression and store it in mmfit.
  #The starting value for vmax is set to the highest rate in the set.
  #The starting value for km is set to average concentraton in the dataset.
  #A maximum of 5000 iterations are allowed.
  mmfit = nls(rates ~ vmax * concentration/(km + concentration), start = list(vmax = max(rates), km = mean(concentration)), control = c(maxiter = 5000))
  #Add the fitted curve to the plot.
  lines(concentration, predict(mmfit), col = 'Red')
  #Print out the regression data.
  summary(mmfit)
}