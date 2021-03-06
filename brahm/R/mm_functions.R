#Initialize library
#library(plotrix)

#Function to calculate the regression, plot the data, and return the regression data
#Do not export (internal only)
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
  lines(reg_subset, lwd = 3, col = datacol)

  #Add the linear regression line over the approriate range
  plotrix::ablineclip(
    regression,
    col = "black",
    x1 = lowx,
    x2 = highx,
    lwd = 2
  )

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
  lines(reg_subset, lwd = 3, col = datacol)

  #Add the linear regression line over the approriate range
  plotrix::ablineclip(
    regression,
    col = "black",
    x1 = lowx,
    x2 = highx,
    lwd = 2
  )

  #Return the regression results.
  return(regression)
}

#' Calculate the slope of a single data series.
#'
#' This function calculates the slope for a single kinetic data series.
#' The user will then be prompted to enter alternate x-limits for the dataset.  The data will be re-plotted with the new regression curve.
#' If the user hits enter for either the minimum or maximum limit, the previous value will be used.
#' When the user hits enter for both the minimum and maximum limit, the function will end.
#'
#' @param kineticdata A dataframe that contains one or more kinetic series.  The column names of the dataframe should be the name of each dataset.
#' @param timeheader A string containing the column name of the time data for the kinetic series.
#' @param activityheader A string containing the activity (or concentration) data for the kinetic series.
#' @param startreg A numeric value indicating the smallest timepoint to be used in the first round of regression.  If omitted, the smallest timepoint is used.  If the timepoint is not present in the dataset, the smallest timepoint greater than \code{startreg} is used.
#' @param endreg A numeric value indicating the largest timepoint to be used in the first round of regression.  If omitted, the largest timepoint is used.  If the timepoint is not present in the dataset, the largest timepoint smaller than \code{endreg} is used.

#' @return A list object containing, from the final round of linear regression, (1) the slope, (2) the intercept, (3) the lower timepoint limit, (4) the higher timepoint limit, and (5) the complete regression object.
#' @seealso \code{\link{lm}}
#' @export
regression_loop <-
  function(kineticdata,
           timeheader,
           activityheader,
           startreg = min(as.numeric(as.character(kineticdata[-1, timeheader]))),
           endreg = max(as.numeric(as.character(kineticdata[-1, timeheader])))) {
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
        subset(regdata, x >= as.numeric(startreg) &
                 x <= as.numeric(endreg))

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
#' Performs a non-linear regression fit to the Michaelis-Menten equation and prints and graphs the results.
#'
#' @param concentration The substrate concentrations used in the non-linear regression.
#' @param rates The rates corresponding to the substrate concentrations given in \code{concentration}.
#' @param exclude An optional flag indicating that this datapoint should be excluded from the non-linear regression analysis.
#'
#' @return The non-linear regression object is returned.
#' @seealso \code{\link{nls}}
#' @export
mm_nls <- function(concentration, rates, exclude = c(FALSE)) {
  #Combine the data into a dataframe called raw_data
  raw_data <- data.frame(concentration, rates, exclude)
  colnames(raw_data) <- c("concentration", "rates", "exclude")
  #Subset into excl_data and incl_data
  #incl_data has to be a matrix for nls.
  incl_data <- as.matrix(subset(raw_data, exclude == FALSE))
  excl_data <- subset(raw_data, exclude == TRUE)
  #Sort incl_data for nls
  incl_data <- incl_data[order(incl_data[, 1]), ]
  #Plot the raw data
  graphics.off()
  plot(
    incl_data[, 1],
    incl_data[, 2],
    main = "Michaelis-Menten Kinetics",
    xlab = "[Substrate]",
    ylab = "Initial Rate"
  )
  #Do the non-linear regression and store it in mmfit.
  #The starting value for vmax is set to the highest rate in the set.
  #The starting value for km is set to average concentraton in the dataset.
  #A maximum of 5000 iterations are allowed.
  mmfit = nls(
    incl_data[, 2] ~ vmax * incl_data[, 1] / (km + incl_data[, 1]),
    start = list(
      vmax = max(abs(incl_data[, 2])) * (max(abs(incl_data[, 2])) / max(incl_data[, 2])),
      km = mean(incl_data[, 1])
    ),
    control = c(maxiter = 5000)
  )
  #Add the fitted curve to the plot.
  lines(incl_data[, 1], predict(mmfit), col = 'Red')
  #Plot the excluded data as blue Xs.
  points(excl_data, col = 'blue', pch = 4)
  #Print out and return the regression data.
  summary(mmfit)
  return(mmfit)
}

#The function nls_loop takes the raw data and processed initial rate data.
#This function directs looping the mm_nls function, allowing the user to reprocess or exclude datapoints.
#mm_nls, which is called by nls_loop, is responsible for actually doing the NLS and plotting the result.
#nls_loop is responsible for calling calling regression_loop on selected data, excluded datapoints, and then calling mm_nls when ready.

#nls_loop takes the initial rate data as an argument.  It must be in the standard format, with columns Dataset, Rate, Min Time, Max Time, and Exclude?
#nls_loop returns the final NLS regression object.

#Do not export.
nls_loop <- function(raw_data, initial_rates) {
  #Perform the non-linear regression, and then allow the user to re-process.
  while (TRUE) {
    #Do the non-linear regression.
    mm_fit <-
      mm_nls(initial_rates$Dataset,
             initial_rates$Rate,
             initial_rates$`Exclude?`)
    print(initial_rates)
    print(mm_fit)
    #Provide a menu with all of the datasets.  Re-run the linear regression for those.
    setnumber <-
      menu(c(initial_rates$Dataset, "Include/exclude datasets"),
           title = "Select a dataset to re-process (0 to quit):")
    #If the number from the menu is 0, we're done.  Break out of the while loop.
    if (setnumber == 0) {
      break
    }
    #If the number from the menu is the highest value, switch to the exclude dataset mode.
    if (setnumber == length(initial_rates$Dataset) + 1) {
      #Stay in include/exclude mode until the user chooses 0.
      switchset <- 1
      while (switchset != 0) {
        #Get the set to toggle.
        switchset <-
          menu(paste(
            initial_rates$Dataset,
            initial_rates$`Exclude?`,
            sep = ': '
          ),
          title = "Select a dataset to toggle include/exclude state (0 to quit):")
        #Toggle switchset.
        initial_rates[switchset, 5] <- !initial_rates[switchset, 5]
      }
    } else {
      #Otherwise, reprocess the selected datapoint.
      #Print out the old data.
      print(initial_rates[setnumber, ])
      #Re-do linear regression.
      reg_results <-
        regression_loop(
          raw_data,
          colnames(raw_data[1]),
          colnames(raw_data[1 + setnumber]),
          initial_rates[setnumber, 3],
          initial_rates[setnumber, 4]
        )
      #Replace the values in initial_rates.
      initial_rates[setnumber, ] <-
        data.frame(initial_rates[setnumber, 1],
                   reg_results[[1]],
                   reg_results[[3]],
                   reg_results[[4]],
                   initial_rates[setnumber, 5])
    }
  }
  #Return updated initial rates and mm_fit
  return(list(initial_rates, mm_fit))
}

#mm_validation checks the data for compatibility with the mm_kinetics requirements.
#The raw_data is checked to be numeric.  If the first row (other than the first column) is not, warn that NLS is not possible.
#If the second and higher rows are not numeric, quit with an error.
#For the result_list data (if provided), it is checked to have the right header rows.  If not, quit with an error.
#It is also checked to match up with the raw_data.  If not, quit with an error.
#mm_validation returns the TRUE/FALSE value of whether to perform NLS on the data.  All other error conditions will quit.

#Do not export.
mm_validation <- function(raw_data, result_list = '') {
  #Check if the non-header raw data is numeric.  If not, stop execution and output an error message.
  check.num <-
    function(X) {
      is.numeric(as.numeric(as.character(X)))
    }
  check.nas <- function(X) {
    is.na(as.numeric(as.character(X)))
  }
  if (!all(sapply(raw_data[-1, ], check.num)) ||
      any(sapply(raw_data[-1, ], check.nas))) {
    stop(
      "The time data or substrate/product concentration data are not numeric.  Initial rate analysis cannot proceed."
    )
  }

  #The following checks apply only if result_list is given.
  if (!identical(result_list, '')) {
    #Check if the result_list data has the correct headers.  If not, stop execution and output an error message.
    if (!identical(names(result_list),
                   c("Dataset", "Rate", "Min Time", "Max Time", "Exclude?"))) {
      stop(
        "The Michaelis-Menten result list CSV file is in an incorrect format.\nIt should contain five column: Dataset, Rate, Min Time, Max Time, and Exclude?\nCheck the input file, or do not provide a result file to process all datasets and re-generate a properly formatted file."
      )
    }

    #Check if the result_list data matches up with raw_data.  If not, stop execution and output an error message.
    if (!identical(as.numeric(result_list$Dataset), as.numeric(raw_data[1, -1]))) {
      stop(
        "The raw data file and the results file substrate concentrations are incompatible.\nThe order and values of the substrate concentrations must match.\nEither fix order of the values, or do not provide a result file to process all datasets and re-generate a properly formatted file."
      )
    }

    #Check if the result_list data are numeric/logical.  If not, stop execution and output an error message.
    if (!all(sapply(result_list[, c(-1, -5)], check.num)) ||
        any(sapply(result_list[, c(-1, -5)], check.nas))) {
      stop("The result file columns Rate, Min Time, or Max Time are not numeric.  Cannot proceed.")
    }

    if (!is.logical(as.logical(result_list[, 5])) ||
        any(is.na(as.logical(result_list[, 5])))) {
      stop("The result file column Exclude? is not TRUE/FALSE.  Cannot proceed.")
    }

    #Check if the substrate concentrations are numeric.  If not, warn that NLS cannot proceed, prompt to continue.  Return the TRUE/FALSE result.
    subnumeric <-
      (is.numeric(as.numeric(result_list$Dataset)) &&
         !any(is.na(as.numeric(
           result_list$Dataset
         ))))
    if (!subnumeric) {
      print("The substrate list is not numeric, so non-linear regression cannot be performed.")
      continue <-
        menu(c("Yes", "No"), title = "Do you want to proceed?")
      if (continue == 2) {
        stop("User requested termination.")
      }
    }
    return(subnumeric)
  } else {
    #If result_list is not given, return FALSE (ie. cannot proceed with NLS).
    return(FALSE)
  }
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
#' This is a master function that does complete Michaelis-Menten kinetics analysis.
#'
#' The raw data kinetic data, as a single table, is passed to the function.  Each data series is analyzed in sequence, allowing the user to select appropriate minimum and maximum limits for calculating the initial rate.
#' Once all data series are analyzed, the Michaelis-Menten curve (rate vs. substrate concentration) is plotted and non-linear regression is performed.
#' The user will then be prompted to reanalyze any of the kinetic series, or to exclude one or more series from analysis.
#' Once the user is satisfied with the results, the function ends, printing and returning the non-linear regression object.
#' If the optional \code{infile} string is given, a CSV file containing a previous set of results for \code{raw_data} is loaded.
#' In this case, the initial analysis of the data series is skipped.
#' If the optional \code{outfile} string is given, a CSV file is written containing the final results of the analysis.
#' \code{infile} needs to be generated by \code{mm_kinetics} to be a valid result file.
#'
#' @param raw_data A data table containing time data in the leftmost column and activity data in the other columns.  The top row should be the concentration series.  Row and column names are not used (i.e. time data should not be the row names, and concentration data should not be the column names).
#' @param infile An optional filename of a CSV file containing the results of a previous mm_kinetics run.
#' @param outfile An optional filename of a CSV file that will be written with the results of this mm_kinetics run, for future re-analysis.
#'
#' @return The nls object generated in the final round of non-linear regression.
#' @export
mm_kinetics <- function(raw_data, infile = "", outfile = "") {
  #Run a validation on the raw_data before doing anything else.
  mm_validation(raw_data)

  #Initialize a dataframe to hold the result data.
  result_list <- data.frame()

  #Set up a list of the dataset concentrations.
  subconcentrations <- raw_data[1, -1]

  #If infile is not given, run through all datasets first.  Otherwise, skip this and load the input data.
  if (infile == "") {
    for (dataset in colnames(raw_data[-1])) {
      reg_results <-
        regression_loop(raw_data, colnames(raw_data[1]), dataset)
      new_result <-
        data.frame(as.numeric(raw_data[1, dataset]),
                   reg_results[[1]],
                   reg_results[[3]],
                   reg_results[[4]],
                   FALSE)
      result_list <- rbind(result_list, new_result)
    }
  } else {
    #Read in the old result_list from the input file
    result_list <- read.csv(infile)
  }

  names(result_list) <-
    c("Dataset", "Rate", "Min Time", "Max Time", "Exclude?")

  #Validate the input data.
  #The program will quit if the data are not able to be used for NLS, or if there is a mismatch between raw_data and result_list.
  #If the substrate data series titles are not numeric, it will prompt to either quit or skip NLS.
  perform_nls <- mm_validation(raw_data, result_list)

  #If data validation passes and we have numeric substrate concentrations (ie. perform_nls is TRUE)
  if (perform_nls) {
    #Move the numeric version of the dataset names (subconcentrations) into result_list$Dataset.
    result_list$Dataset <- as.numeric(subconcentrations)

    #Call nls_loop() to do the non-linear regression in a loop.
    nls_loop_result <- nls_loop(raw_data, result_list)
    #Extract the components into their own variables.
    result_list <- nls_loop_result[1]
    final_mm <- nls_loop_result[2]
  }

  #Output the result_list to outfile.
  if (outfile != "") {
    write.csv(result_list, file = outfile, row.names = FALSE)
  }
  #Output final_mm
  print(final_mm)
  return(final_mm)
}
