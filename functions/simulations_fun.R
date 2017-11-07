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
## Extract stratigraphy subsamples
## morphospace is the cleaned morphospace
## tree is the phylogeny matched to the morphospace
## FADLAD is first and last occurrence data
## inc.nodes is logical for whether to estimate disparity using nodal values
## type is "Age" or "Epoch" - see get.bin.ages documentation
##--------------------------------------------------------------------------

get.subsamples.stratigraphy <- function(morphospace, tree, FADLAD, inc.nodes, type){
  time.subsamples(data = morphospace, tree = tree, 
                  method = "discrete", time = get.bins(tree, type)[[1]], 
                  FADLAD = FADLAD, inc.nodes = inc.nodes)
}  
  
##---------------------------------------------------------------------------
## Extract all types of subsample for both methods and four continuous models
## morphospace is the cleaned morphospace
## tree is the phylogeny matched to the morphospace
## bins is a vector of bin times or number of bins
## FADLAD is first and last occurrence data
## inc.nodes is logical for whether to estimate disparity using nodal values
##--------------------------------------------------------------------------

get.subsamples <- function(morphospace, tree, bins, FADLAD, inc.nodes){
  
  ## Subset samples by required number of time bins
  subsamples.bins <- time.subsamples(data = morphospace, tree = tree, 
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
    slices <- slices - (diff(bins)/2)
  } else {
    slices <- bins
  }

  ## Subset samples by required number of time slices with acctran model
  subsamples.acctran <- time.subsamples(data = morphospace, tree = tree, 
  	                                    method = "continuous", time = slices, 
  	                                    FADLAD = FADLAD, inc.nodes = inc.nodes,
                                        model = "acctran")

  ## Subset samples by required number of time slices with deltran model
  subsamples.deltran <- time.subsamples(data = morphospace, tree = tree, 
  	                                    method = "continuous", time = slices, 
  	                                    FADLAD = FADLAD, inc.nodes = inc.nodes,
                                        model = "deltran")
  
  ## Subset samples by required number of time slices with punctuated model
  subsamples.punctuated <- time.subsamples(data = morphospace, tree = tree, 
  	                                       method = "continuous", time = slices, 
  	                                       FADLAD = FADLAD, inc.nodes = inc.nodes,
                                           model = "punctuated")
  
  ## Subset samples by required number of time slices with gradual model
  subsamples.gradual <- time.subsamples(data = morphospace, tree = tree, 
  	                                    method = "continuous", time = slices, 
  	                                    FADLAD = FADLAD, inc.nodes = inc.nodes,
  	                                    model = "gradual")

  return(list(subsamples.bins, subsamples.acctran, subsamples.deltran, 
  	          subsamples.punctuated, subsamples.gradual))
}

##--------------------------------------------------------------------------
## Bootstrap and rarefy subsamples
## subsamples is a subsample output from time.subsamples
## bootstraps is the number of bootstraps to run
##--------------------------------------------------------------------------

boot.rarefy <- function(subsamples, bootstraps){
  # Obtain a rarefied bootstrap matrix matrix for the subsamples
  boot.matrix(subsamples, bootstraps = bootstraps, rarefaction = TRUE)
}

##--------------------------------------------------------------------------
## Extract disparity for all subsamples
## Estimate disparity - observed and bootstrapped and rarefied
## subsampled_data is output from boot.matrix
## metric is the chosen metric - see dispRity documentation for details
##--------------------------------------------------------------------------

get.disparity <- function(subsampled_data, metric){
  summary(dispRity(subsampled_data, metric = metric), round = 10)
}

##--------------------------------------------------------------------------
## Extract outputs needed for results
## disparity.object is the output of get.disparity
## bins is a vector of bin times or number of bins
## metricname is a string specifying the name of the metric being used
## inc.nodes is logical for whether diparity was estimated using nodal values
##--------------------------------------------------------------------------

get.outputs <- function(disparity.object, bins, metric.name, inc.nodes, model.name){
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
                              bootstraps = bootstraps, metric, metric.name, rarefaction){

  # Need to remove the nodes if not using them
  if(inc.nodes == FALSE){
    remove.nodes <- clean.data(morphospace, tree)
    morphospace <- remove.nodes$data  
  }

  ## Extract all types of subsample for both methods and four continuous models
  subsamples <- get.subsamples(morphospace, tree, bins, FADLAD, inc.nodes)

  ## Bootstrap and rarefy all types of subsamples
  subsampled_data <- purrr::map(subsamples, boot.matrix, bootstraps = bootstraps, rarefaction = rarefaction)

  ## Get disparity for all types of subsamples
  disparity.object.out <- purrr::map(subsampled_data, dispRity, metric = metric)

  ## Summarise the disparity for output
  disparity.object <- purrr::map(disparity.object.out, summary.dispRity, round = 10)

  ## Outputs
  ## Extract output information
  disparity.outputs <- purrr::map(disparity.object, get.outputs, bins = bins, 
                                  metric.name = metric.name, inc.nodes = inc.nodes)
  
  ## Add model designations
  models <- c("equalbins", "acctran", "deltran", "punctuated", "gradual")
  disparity.outputs <- mapply(cbind, disparity.object, "model" = models, SIMPLIFY = FALSE)
  ## Flatten list to a dataframe
  ## This will throw warnings as factor levels of subsamples are converted to characters
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
# type = "Epoch"
# inc.nodes = TRUE
# bootstraps = 100
# metric = c(sum, variances)
# metric.name = "sum_var"
# rarefaction = FALSE

run.all.disparity.wrapper <- function(morphospace, tree, type, FADLAD, inc.nodes = TRUE,
                              bootstraps = bootstraps, metric, metric.name, rarefaction = FALSE){
  
  # Need to remove the nodes if not using them
  if(inc.nodes == FALSE){
    remove.nodes <- clean.data(morphospace, tree)
    morphospace <- remove.nodes$data  
  }
  
  # Extract bins to go with particular stratigraphy
  strat.bins <- get.bins(tree, type)
  
  
  ##--------------------------------------------------------------------------
  ## BIN/SLICES of based ONLY on STRATIGRAPHY #TG: the stratigraphy slices are the centre of the stratigraphic zone
  disparity.stratigraphy <- run.all.disparity(morphospace, tree, strat.bins[[1]], FADLAD, inc.nodes = inc.nodes,
                                          bootstraps = bootstraps, metric, metric.name, rarefaction = rarefaction)
  disparity.stratigraphy.results <- cbind(disparity.stratigraphy$results, "stratigraphy" = type, "bin_type" = "stratigraphy")


  ##--------------------------------------------------------------------------
  ## BIN/SLICES of same DURATION as STRATIGRAPHY
  disparity.duration <- run.all.disparity(morphospace, tree, strat.bins[[2]], FADLAD, inc.nodes = inc.nodes,
                                          bootstraps = bootstraps, metric, metric.name, rarefaction = rarefaction)
  disparity.duration.results <- cbind(disparity.duration$results, "stratigraphy" = type, "bin_type" = "duration")
  

  ##--------------------------------------------------------------------------
  ## BIN/SLICES of same NUMBER as STRATIGRAPHY
  disparity.number <- run.all.disparity(morphospace, tree, strat.bins[[3]], FADLAD, inc.nodes = inc.nodes,
                                          bootstraps = bootstraps, metric, metric.name, rarefaction = rarefaction)
  disparity.number.results <- cbind(disparity.number$results, "stratigraphy" = type, "bin_type" = "number")


  ##--------------------------------------------------------------------------
  ## Combine results
  #disparity.outputs <- cbind(disparity.stratigraphy.results, disparity.duration.results, disparity.number.results)
  #TG: this won't work since the amount of bins/rows may vary per dataset
  disparity.outputs <- list("stratigraphy" = disparity.stratigraphy.results, "duration" = disparity.duration.results, "number" = disparity.number.results)

  ## Combining the objects
  disparity.objects <- list("stratigraphy" = disparity.stratigraphy$dispRity, "duration" = disparity.duration$dispRity, "number" = disparity.number$dispRity)
  
  return(list("results" = disparity.outputs, "objects" = disparity.objects))
}