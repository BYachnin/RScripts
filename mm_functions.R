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
  function(kineticdata, timeheader, activityheader, startreg = min(as.numeric(as.character(kineticdata[-1, timeheader]))), endreg = max(as.numeric(as.character(kineticdata[-1, timeheader])))) {
    #Going in, we will go through the loop.
    loop = TRUE
    
    #Convert the appropriate two columns into a data frame for linear regression.
    regdata = data.frame(as.numeric(as.character(kineticdata[-1, timeheader])), as.numeric(as.character(kineticdata[-1, activityheader])))
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

mm_nls <- function(concentration, rates, exclude = c(FALSE)) {
  #Combine the data into a dataframe called raw_data
  raw_data <- data.frame(concentration, rates, exclude)
  colnames(raw_data) <- c("concentration", "rates", "exclude")
  #Subset into excl_data and incl_data
  #incl_data has to be a matrix for nls.
  incl_data <- as.matrix(subset(raw_data, exclude == FALSE))
  excl_data <- subset(raw_data, exclude == TRUE)
  #Sort incl_data for nls
  incl_data <- incl_data[order(incl_data[,1]),]
  #Plot the raw data
  graphics.off()
  plot(incl_data[,1], incl_data[,2], main = "Michaelis-Menten Kinetics", xlab = "[Substrate]", ylab = "Initial Rate")
  #Do the non-linear regression and store it in mmfit.
  #The starting value for vmax is set to the highest rate in the set.
  #The starting value for km is set to average concentraton in the dataset.
  #A maximum of 5000 iterations are allowed.
  mmfit = nls(incl_data[,2] ~ vmax * incl_data[,1]/(km + incl_data[,1]), start = list(vmax = max(abs(incl_data[,2]))*(max(abs(incl_data[,2]))/max(incl_data[,2])), km = mean(incl_data[,1])), control = c(maxiter = 5000))
  #Add the fitted curve to the plot.
  lines(incl_data[,1], predict(mmfit), col = 'Red')
  #Plot the excluded data as blue Xs.
  points(excl_data, col = 'blue', pch = 4)
  #Print out and return the regression data.
  summary(mmfit)
  return(mmfit)
}

#The function nls_loop takes the processed initial rate data.
#This function directs looping the mm_nls function, allowing the user to reprocess or exclude datapoints.
#mm_nls, which is called by nls_loop, is responsible for actually doing the NLS and plotting the result.
#nls_loop is responsible for calling calling regression_loop on selected data, excluded datapoints, and then calling mm_nls when ready.

#nls_loop takes the initial rate data as an argument.  It must be in the standard format, with columns Dataset, Rate, Min Time, Max Time, and Exclude?
#nls_loop returns the final NLS regression object.

nls_loop <- function(initial_rates) {
  #Perform the non-linear regression, and then allow the user to re-process.
  while (TRUE) {
    #Do the non-linear regression.
    mm_fit <- mm_nls(initial_rates$Dataset, initial_rates$Rate, initial_rates$`Exclude?`)
    print(initial_rates)
    print(mm_fit)
    #Provide a menu with all of the datasets.  Re-run the linear regression for those.
    setnumber <- menu(c(initial_rates$Dataset, "Include/exclude datasets"), title = "Select a dataset to re-process (0 to quit):")
    #If the number from the menu is 0, we're done.  Break out of the while loop.
    if (setnumber == 0) {break}
    #If the number from the menu is the highest value, switch to the exclude dataset mode.
    if (setnumber == length(initial_rates$Dataset)+1) {
      #Stay in include/exclude mode until the user chooses 0.
      switchset <- 1
      while (switchset != 0) {
        #Get the set to toggle.
        switchset <- menu(paste(initial_rates$Dataset, initial_rates$`Exclude?`, sep = ': '), title = "Select a dataset to toggle include/exclude state (0 to quit):")
        #Toggle switchset.
        initial_rates[switchset, 5] <- !initial_rates[switchset, 5]
      }
    } else {
      #Otherwise, reprocess the selected datapoint.
      #Print out the old data.
      print(initial_rates[setnumber,])
      #Re-do linear regression.
      reg_results <- regression_loop(wt_data, colnames(wt_data[1]), colnames(wt_data[1+setnumber]), initial_rates[setnumber,3], initial_rates[setnumber,4])
      #Replace the values in initial_rates.
      initial_rates[setnumber,] <- data.frame(initial_rates[setnumber, 1], reg_results[[1]], reg_results[[3]], reg_results[[4]], initial_rates[setnumber, 5])
    }
  }
  #Return updated initial rates and mm_fit
  return(list(initial_rates, mm_fit))
  #return(initial_rates)
}

#mm_kinetics is a master function that does complete Michaelis-Menten kinetics analysis.
#It takes as input a data frame, with the following specificatons:
#1. The first row (should NOT be a header row) should contain numeric values for the substrate concentration for each dataset.
#2. The first column should contain numberic values for the time data.
#3. All subsequent columns should contain numeric values for substrate/product concentration at the timepoints listed in the first column.
#4. The value of the top-left cell is ignored.
#Optionally, mm_kinetics can take two filenames of CSV files containing result data: one for input and one for output.
#If an input filename is provided, the result data is preloaded, rather than asking for the user to process all kinetic datasets.
#If the same filename is used, the input file will be OVERWRITTEN with the new results.
#The file should contain five columns with headers in the first row:
#A. Dataset (should hold numeric substrate concentrations).
#B. Rate (should contain the numeric calculated initial rate for that dataset).
#C. Min.Time (should contain the numeric time value for the first datapoint used in linear regression to calculate Rate).
#D. Max.Time (should contain the numeric time value for the last datapoint used in linear regression to calculate Rate).
#E. Exclude. (should contain a boolean flag indicating whether to exclude that dataset during non-linear regression).
#If used for input, the file containing the result data must correspond exactly to the raw data dataframe.
#In practice, this means that mm_kinetics should be used to generate the result files.

#As output, mm_kinetics returns the non-linear regression results, and also print those to the terminal.
#It will also plot the results as a graph in a window.
#Note that all results will be in the same units as the input data.