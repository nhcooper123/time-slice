## Functions for time slice analyses
## Natalie Cooper 2017

##---------------------------------------------------------------------------
## Define sets of bins based on stratigraphy
## tree is the phylogeny
## type is "Age" or "Epoch" - see get.bin.ages documentation
##---------------------------------------------------------------------------

get.bins <- function(tree, type){
  ## Get the stratigraphic ages
  time_strat <- get.bin.ages(tree, type = type)
  ## Get the median duration of the stratigraphic period (to create equal time bins)
  strat_duration <- median(abs(diff(time_strat)))
  ## Get the range of the tree age
  min_max <- range(time_strat)
  ## Create the sequence of bins of equal duration
  strat_duration <- rev(seq(from = min_max[1], to = min_max[2], by = strat_duration))
  ## Get the number of stratigraphic periods (to create equal time bins)
  strat_number <- length(time_strat)-1 #TG: -1 because they are bin boundaries, not bin numbers
  
  return(list(time_strat, strat_duration, strat_number))
}

##---------------------------------------------------------------------------
## Extract stratigraphy subsets
## morphospace is the cleaned morphospace
## tree is the phylogeny matched to the morphospace
## FADLAD is first and last occurrence data
## inc.nodes is logical for whether to estimate disparity using nodal values
## type is "Age" or "Epoch" - see get.bin.ages documentation
##--------------------------------------------------------------------------

get.subsets.stratigraphy <- function(morphospace, tree, FADLAD, inc.nodes, type){
  time.subsets(data = morphospace, tree = tree, 
                  method = "discrete", time = get.bins(tree, type)[[1]], 
                  FADLAD = FADLAD, inc.nodes = inc.nodes)
}  
  
##---------------------------------------------------------------------------
## Extract all types of subset for both methods and four continuous models
## morphospace is the cleaned morphospace
## tree is the phylogeny matched to the morphospace
## bins is a vector of bin times or number of bins
## FADLAD is first and last occurrence data
## inc.nodes is logical for whether to estimate disparity using nodal values
##--------------------------------------------------------------------------

get.subsets <- function(morphospace, tree, bins, FADLAD, inc.nodes){
  
  ## Subset samples by required number of time bins
  subsets.bins <- time.subsets(data = morphospace, tree = tree, 
  	                                 method = "discrete", time = bins, 
  	                                 FADLAD = FADLAD, inc.nodes = inc.nodes)
  

  #TG: For the time slicing, should we use the same boundaries of the time bins or should we go with the centre of the bins?
  #TG: for example, if the median strat length is 5, the time bins will be 0-5, 5-10, 10-15 etc...
  #TG: The slices, using the same data will be at 5, 10, 15. Is that what we want or we want them in the centre of the bins? I.e. 2.5, 7.5, 12.5, etc...
  #TG: If so (I would argue yes) it's doing it below. Else, we can comment this out.

  if(length(bins) != 1) {
    ## Removing the last bin (that would be a negative slice)
    slices <- bins[-length(bins)]
    ## Getting the centre of the bins as slicing values
    slices <- slices + (diff(bins)/2)
  } else {
    slices <- bins
  }

  ## Subset samples by required number of time slices with acctran model
  subsets.acctran <- time.subsets(data = morphospace, tree = tree, 
  	                                    method = "continuous", time = slices, 
  	                                    FADLAD = FADLAD, inc.nodes = inc.nodes,
                                        model = "acctran")

  ## Subset samples by required number of time slices with deltran model
  subsets.deltran <- time.subsets(data = morphospace, tree = tree, 
  	                                    method = "continuous", time = slices, 
  	                                    FADLAD = FADLAD, inc.nodes = inc.nodes,
                                        model = "deltran")
  
  ## Subset samples by required number of time slices with random model
  subsets.random <- time.subsets(data = morphospace, tree = tree, 
  	                                     method = "continuous", time = slices, 
  	                                     FADLAD = FADLAD, inc.nodes = inc.nodes,
                                         model = "random")
  
  ## Subset samples by required number of time slices with proximity model
  subsets.proximity <- time.subsets(data = morphospace, tree = tree, 
  	                                    method = "continuous", time = slices, 
  	                                    FADLAD = FADLAD, inc.nodes = inc.nodes,
  	                                    model = "proximity")

  ## Subset samples by required number of time slices with proximity model
  subsets.punctuated <- time.subsets(data = morphospace, tree = tree, 
                                        method = "continuous", time = slices, 
                                        FADLAD = FADLAD, inc.nodes = inc.nodes,
                                        model = "punctuated")

  ## Subset samples by required number of time slices with proximity model
  subsets.gradual <- time.subsets(data = morphospace, tree = tree, 
                                        method = "continuous", time = slices, 
                                        FADLAD = FADLAD, inc.nodes = inc.nodes,
                                        model = "gradual")

  return(list(subsets.bins, subsets.acctran, subsets.deltran, subsets.random,
              subsets.proximity, subsets.punctuated, subsets.gradual))
}

##--------------------------------------------------------------------------
## Bootstrap and rarefy subsets
## subsets is a subset output from time.subsets
## bootstraps is the number of bootstraps to run
##--------------------------------------------------------------------------

boot.rarefy <- function(subsets, bootstraps){
  # Obtain a rarefied bootstrap matrix matrix for the subsets
  boot.matrix(subsets, bootstraps = bootstraps, rarefaction = TRUE)
}

##--------------------------------------------------------------------------
## Extract disparity for all subsets
## Estimate disparity - observed and bootstrapped and rarefied
## subsetd_data is output from boot.matrix
## metric is the chosen metric - see dispRity documentation for details
##--------------------------------------------------------------------------

get.disparity <- function(subsetd_data, metric){
  summary(dispRity(subsetd_data, metric = metric), round = 10)
}

##--------------------------------------------------------------------------
## Extract outputs needed for results
## disparity.object is the output of get.disparity
## bins is a vector of bin times or number of bins
## metricname is a string specifying the name of the metric being used
## inc.nodes is logical for whether diparity was estimated using nodal values
##--------------------------------------------------------------------------

get.outputs <- function(disparity.object, bins, metric.name, inc.nodes,
                        model.name){
  output <- as.data.frame(disparity.object)
  output$metric <- metric.name
  output$inc.nodes <- inc.nodes
  if(length(bins) == 1){output$n_bins <- bins}
  else{output$n_bins <- length(bins)}
  return(output)
 }

##--------------------------------------------------------------------------
## Putting it all together for four models plus equal bins
## This function will run models of various time bin/slice numbers ignoring
## stratigraphic information

## morphospace is the cleaned morphospace
## tree is the phylogeny matched to the morphospace
## bins is a vector of bin times or number of bins
## FADLAD is first and last occurrence data
## inc.nodes is logical for whether to estimate diparity using nodal values
## bootstraps is the number of bootstraps
## metric is the chosen metric - see dispRity documentation for details 
## metricname is a string specifying the name of the metric being used
##--------------------------------------------------------------------------

run.all.disparity <- function(morphospace, tree, bins, FADLAD, inc.nodes = TRUE,
                              bootstraps = bootstraps, metric, metric.name,
                              rarefaction){

  # # Need to remove the nodes if not using them
  # if(inc.nodes == FALSE){
  #   remove.nodes <- clean.data(morphospace, tree)
  #   morphospace <- remove.nodes$data  
  # }

  ## Extract all types of subset for both methods and four continuous models
  subsets <- get.subsets(morphospace, tree, bins, FADLAD, inc.nodes)

  ## Bootstrap and rarefy all types of subsets
  subsetd_data <- purrr::map(subsets, boot.matrix, bootstraps = bootstraps,
                             rarefaction = rarefaction)

  ## Get disparity for all types of subsets
  disparity.object.out <- purrr::map(subsetd_data, dispRity, metric = metric)

  ## Summarise the disparity for output
  disparity.object <- purrr::map(disparity.object.out, summary.dispRity,
                                 round = 10)

  ## Outputs
  ## Extract output information
  disparity.outputs <- purrr::map(disparity.object, get.outputs, bins = bins[-1], 
                                  metric.name = metric.name,
                                  inc.nodes = inc.nodes)
  
  ## Add model designations
  models <- c("equalbins", "acctran", "deltran", "random", "proximity",
              "punctuated", "gradual")
  disparity.outputs <- mapply(cbind, disparity.object, "model" = models,
                              SIMPLIFY = FALSE)
  ## Flatten list to a dataframe
  ## This will throw warnings as factor levels of subsets are converted to characters
  disparity.outputs <- purrr::map_dfr(disparity.outputs, cbind)
  
  return(list("results" = disparity.outputs, "dispRity" = disparity.object.out))
}

##--------------------------------------------------------------------------
## Running all the disparity analysis
## 
## morphospace is the cleaned morphospace
## tree is the phylogeny matched to the morphospace
## type is "Age" or "Epoch" - see get.bin.ages documentation
## FADLAD is first and last occurrence data
## inc.nodes is logical for whether to estimate diparity using nodal values
## bootstraps is the number of bootstraps
## metric is the chosen metric - see dispRity documentation for details 
## metricname is a string specifying the name of the metric being used
##--------------------------------------------------------------------------

## Argument list for debugging
# warning("DEBUG MODE FOR run.all.disparity.wrapper")
# type = "Age"
# inc.nodes = TRUE
# bootstraps = 3
# metric = c(sum, variances)
# metric.name = "sum_var"
# rarefaction = FALSE


run.all.disparity.wrapper <- function(morphospace, tree, type, FADLAD,
                                      inc.nodes = TRUE, bootstraps = bootstraps,
                                      metric, metric.name, rarefaction = FALSE){
  
  # Need to remove the nodes if not using them
  if(inc.nodes == FALSE){
    remove.nodes <- clean.data(morphospace, tree)
    morphospace <- remove.nodes$data  
  }
  
  # Extract bins to go with particular stratigraphy
  strat.bins <- get.bins(tree, type)
  
  
  ##--------------------------------------------------------------------------
  ## BIN/SLICES of based ONLY on STRATIGRAPHY #TG: the stratigraphy slices are the centre of the stratigraphic zone
  disparity.stratigraphy <- run.all.disparity(morphospace, tree,
                                              strat.bins[[1]], FADLAD,
                                              inc.nodes = inc.nodes,
                                              bootstraps = bootstraps, metric,
                                              metric.name,
                                              rarefaction = rarefaction)
  disparity.stratigraphy.results <- cbind(disparity.stratigraphy$results,
                                          "stratigraphy" = type,
                                          "bin_type" = "stratigraphy")


  ##--------------------------------------------------------------------------
  ## BIN/SLICES of same DURATION as STRATIGRAPHY
  disparity.duration <- run.all.disparity(morphospace, tree, strat.bins[[2]],
                                          FADLAD, inc.nodes = inc.nodes,
                                          bootstraps = bootstraps, metric,
                                          metric.name,
                                          rarefaction = rarefaction)
  disparity.duration.results <- cbind(disparity.duration$results,
                                      "stratigraphy" = type,
                                      "bin_type" = "duration")
  

  ##--------------------------------------------------------------------------
  ## BIN/SLICES of same NUMBER as STRATIGRAPHY
  disparity.number <- run.all.disparity(morphospace, tree, strat.bins[[3]],
                                        FADLAD, inc.nodes = inc.nodes,
                                        bootstraps = bootstraps, metric,
                                        metric.name, rarefaction = rarefaction)
  disparity.number.results <- cbind(disparity.number$results,
                                    "stratigraphy" = type,
                                    "bin_type" = "number")




  ## ERROR WITH NUMBER





  ##--------------------------------------------------------------------------
  ## Combine results
  #disparity.outputs <- cbind(disparity.stratigraphy.results, disparity.duration.results, disparity.number.results)
  #TG: this won't work since the amount of bins/rows may vary per dataset
  disparity.outputs <- list("stratigraphy" = disparity.stratigraphy.results,
                            "duration" = disparity.duration.results,
                            "number" = disparity.number.results)

  ## Combining the objects
  disparity.objects <- list("stratigraphy" = disparity.stratigraphy$dispRity,
                            "duration" = disparity.duration$dispRity,
                            "number" = disparity.number$dispRity)
  
  return(list("results" = disparity.outputs, "objects" = disparity.objects))
}


##--------------------------------------------------------------------------
## Plotting the results using dispRity objects
## 
## slug.object The chain object element (e.g. ou1$object)
## method Which method: 1 = stratigraphy, 2 = duration, 3 = number
## colors The central tendency colors
## colors_CI The central CI colors
## main The plot title
## ylim The Y axis limits
## legend Whether to display the legend or not
## CI Whether to display the CIs or not
## time.bins.line Whether to display the time bins as lines or not
##--------------------------------------------------------------------------

plot.results.dispRity <- function(slug.object, method, colors, colors_CI, main,
                                  ylim, legend = FALSE, CI = TRUE,
                                  time.bins.line = FALSE) {

    bins_type <- ifelse(time.bins.line, "c", "l")

    ## STRATIGRAPHY
    if(CI) {
      ## Plotting the continuous data CIs
      plot(slug.object[[method]][[2]], density = 70, col = c("white", colors_CI[1]), quantile = c(95), ylim = ylim, ylab = "Sum of variance", main = main)
      plot(slug.object[[method]][[3]], density = 70, col = c("white", colors_CI[2]), quantile = c(95), ylim = ylim, add = TRUE, xlab = "", ylab = "")
      plot(slug.object[[method]][[4]], density = 70, col = c("white", colors_CI[3]), quantile = c(95), ylim = ylim, add = TRUE, xlab = "", ylab = "")
      plot(slug.object[[method]][[5]], density = 70, col = c("white", colors_CI[4]), quantile = c(95), ylim = ylim, add = TRUE, xlab = "", ylab = "")
      ## Adding the probabilistic models
      if(length(slug.object[[1]]) == 7) {
        plot(slug.object[[method]][[6]], density = 70, col = c("white", colors_CI[5]), quantile = c(95), ylim = ylim, add = TRUE, xlab = "", ylab = "")
        plot(slug.object[[method]][[7]], density = 70, col = c("white", colors_CI[6]), quantile = c(95), ylim = ylim, add = TRUE, xlab = "", ylab = "")
      }
    } else {
      ## Plotting the continuous data (first plot)
      plot(slug.object[[method]][[2]], density = 0, col = colors[1], ylim = ylim, ylab = "Sum of variance", main = main)
    }


    ## Plotting the continuous data medians (other plots)
    plot(slug.object[[method]][[2]], density = 0, col = colors[1], ylim = ylim, add = TRUE, xlab = "", ylab = "")
    plot(slug.object[[method]][[3]], density = 0, col = colors[2], ylim = ylim, add = TRUE, xlab = "", ylab = "")
    plot(slug.object[[method]][[4]], density = 0, col = colors[3], ylim = ylim, add = TRUE, xlab = "", ylab = "")
    plot(slug.object[[method]][[5]], density = 0, col = colors[4], ylim = ylim, add = TRUE, xlab = "", ylab = "")

    if(length(slug.object[[1]]) == 7) {
      plot(slug.object[[method]][[6]], density = 0, col = colors[5], ylim = ylim, add = TRUE, xlab = "", ylab = "")
      plot(slug.object[[method]][[7]], density = 0, col = colors[6], ylim = ylim, add = TRUE, xlab = "", ylab = "")
    }



    col_num <- ifelse(length(slug.object[[1]]) == 7, 7, 5)
    
    if(CI) {
      ## Plotting the discrete data (CI)
      plot(slug.object[[method]][[1]], density = 70, col = c(colors[col_num], colors_CI[col_num]), quantile = c(95), ylim = ylim, add = TRUE, type = bins_type, xlab = "", ylab = "")
    } else {
      plot(slug.object[[method]][[1]], density = 0, col = c(colors[col_num], "white"), quantile = c(1), ylim = ylim, add = TRUE, type = bins_type, xlab = "", ylab = "")
    }

    ## Legend
    if(legend) {
      if(length(slug.object[[1]]) == 7) {
        legend(x = "bottomright", legend = c("acctran", "deltran", "random", "proximity", "punctuated", "gradual", "time bins"), col = colors[1:7], lty = 1)
      } else {
        legend(x = "bottomright", legend = c("acctran", "deltran", "random", "proximity", "time bins"), col = colors[1:5], lty = 1)
      }
    }
}


##--------------------------------------------------------------------------
## Plotting the results using dispRity objects
## 
## model Which model: "equalbins", "acctran", "deltran", "random", "proximity"
## slug.results The chain results element (e.g. ou1$results)
## method Which method: 1 = stratigraphy, 2 = duration, 3 = number
##
## Returns an histogram class object
##--------------------------------------------------------------------------

make.class.histogram <- function(model, slug.results, method) {
  ## Getting the rows
  rows <- which(slug.results[[method]]$model == model)
  ## Creating the histogram object for the time bins
  stratigraphy_hist <- list()
  ## Get the breaks
  stratigraphy_hist$breaks <- as.numeric(unique(unlist(strsplit(slug.results[[method]]$subsets[rows], split = " - "))))
  ## Get the disparity scores
  stratigraphy_hist$counts <- slug.results[[method]]$bs.median[rows]
  ## Remove NAs
  if(any(is.na(stratigraphy_hist$counts))) stratigraphy_hist$counts[is.na(stratigraphy_hist$counts)] <- 0
  ## Add the midpoints
  stratigraphy_hist$mids <- sapply(strsplit(slug.results[[method]]$subsets[rows], split = " - "), function(X) mean(as.numeric(X)))
  ## Add the name
  stratigraphy_hist$xname <- names(slug.results)[[method]]
  ## Graphical parameters
  stratigraphy_hist$equidist <- TRUE
  class(stratigraphy_hist) <- "histogram"
  return(stratigraphy_hist)
}