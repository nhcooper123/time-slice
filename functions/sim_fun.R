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
  ## Get the number of stratigraphic periods (to create equal time bins)
  strat_number <- length(time_strat)
  
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
## bins is a vector of bin times
## FADLAD is first and last occurrence data
## inc.nodes is logical for whether to estimate disparity using nodal values
##--------------------------------------------------------------------------

get.subsamples <- function(morphospace, tree, bins, FADLAD, inc.nodes){
  
  ## Subset samples by required number of time bins
  subsamples.bins <- time.subsamples(data = morphospace, tree = tree, 
  	                                 method = "discrete", time = bins, 
  	                                 FADLAD = FADLAD, inc.nodes = inc.nodes)
  
  ## Subset samples by required number of time slices with acctran model
  subsamples.acctran <- time.subsamples(data = morphospace, tree = tree, 
  	                                    method = "continuous", time = bins, 
  	                                    FADLAD = FADLAD, inc.nodes = inc.nodes,
                                        model = "acctran")

  ## Subset samples by required number of time slices with deltran model
  subsamples.deltran <- time.subsamples(data = morphospace, tree = tree, 
  	                                    method = "continuous", time = bins, 
  	                                    FADLAD = FADLAD, inc.nodes = inc.nodes,
                                        model = "deltran")
  
  ## Subset samples by required number of time slices with punctuated model
  subsamples.punctuated <- time.subsamples(data = morphospace, tree = tree, 
  	                                       method = "continuous", time = bins, 
  	                                       FADLAD = FADLAD, inc.nodes = inc.nodes,
                                           model = "punctuated")
  
  ## Subset samples by required number of time slices with gradual model
  subsamples.gradual <- time.subsamples(data = morphospace, tree = tree, 
  	                                    method = "continuous", time = bins, 
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
## bins is a vector of bin times
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
## bins is a vector of bin times
## FADLAD is first and last occurrence data
## inc.nodes is logical for whether to estimate diparity using nodal values
##--------------------------------------------------------------------------

run.all.disparity <- function(morphospace, tree, bins, FADLAD, inc.nodes = TRUE,
                              bootstraps = 1, metric, metric.name){

  # Need to remove the nodes if not using them
  if(inc.nodes == FALSE){
    remove.nodes <- clean.data(morphospace, tree)
    morphospace <- remove.nodes$data  
  }

  ## Extract all types of subsample for both methods and four continuous models
  subsamples <- get.subsamples(morphospace, tree, bins, FADLAD, inc.nodes)

  ## Bootstrap and rarefy all types of subsamples
  subsampled_data <- purrr::map(subsamples, boot.rarefy, bootstraps = bootstraps)

  ## Get disparity for all types of subsamples
  disparity.object <- purrr::map(subsampled_data, get.disparity, metric = metric)

  ## Outputs
  ## Extract output information
  disparity.outputs <- purrr::map(disparity.object, get.outputs, bins = bins, 
                                  metric.name = metric.name, inc.nodes = inc.nodes)
  
  ## Add model designations
  models <- c("equalbins", "acctran", "deltran", "punctuated", "gradual")
  disparity.outputs <- mapply(cbind, disparity.object, "model" = models, SIMPLIFY = FALSE)
  ## Flatten list to a dataframe
  ## This will throw warnings as factor levels of subsamples are converted to characters
  disparity.outputs <- purrr::map_dfr(disparity.object, cbind)
  
  return(disparity.outputs)
}

##--------------------------------------------------------------------------
## Putting it all together for four models plus equal bins
## morphospace is the cleaned morphospace
## tree is the phylogeny matched to the morphospace
## bins is a vector of bin times
## FADLAD is first and last occurrence data
## inc.nodes is logical for whether to estimate diparity using nodal values
##--------------------------------------------------------------------------

run.all.disparity.strat <- function(morphospace, tree, type, FADLAD, inc.nodes = TRUE,
                              bootstraps = 1, metric, metric.name){
  
  # Need to remove the nodes if not using them
  if(inc.nodes == FALSE){
    remove.nodes <- clean.data(morphospace, tree)
    morphospace <- remove.nodes$data  
  }
  
  # Extract bins to go with particular stratigraphy
  strat.bins <-get.bins(tree, type)
  
  ## STRATIGRAPHY
  ## Extract subsamples for stratigraphy
  subsamples <- get.subsamples.stratigraphy(morphospace, tree, FADLAD, inc.nodes, type)
  ## Bootstrap and rarefy
  subsampled_data <- boot.rarefy(subsamples, bootstraps = bootstraps)
  ## Get disparity 
  disparity.object <- get.disparity(subsampled_data, metric = metric)
  ## Extract output information
  disparity.stratigraphy <- get.outputs(disparity.object, bins = strat.bins[[1]], 
                                        metric.name = metric.name, inc.nodes = inc.nodes)
  disparity.stratigraphy <- cbind(disparity.stratigraphy, "statigraphy" = type, "bin_type" = "unequal")
  
  ## BIN/SLICES of same DURATION as STRATIGRAPHY
  disparity.duration <- run.all.disparity(morphospace, tree, strat.bins[[2]], FADLAD, inc.nodes = inc.nodes,
                                          bootstraps = bootstraps, metric, metric.name)
  disparity.duration <- cbind(disparity.duration, "statigraphy" = type, "bin_type" = "duration")
  
  ## BIN/SLICES of same NUMBER as STRATIGRAPHY
  disparity.number <- run.all.disparity(morphospace, tree, strat.bins[[3]], FADLAD, inc.nodes = inc.nodes,
                                          bootstraps = bootstraps, metric, metric.name)
  disparity.number <- cbind(disparity.number, "statigraphy" = type, "bin_type" = "number")

  ## Outputs
  disparity.outputs <- cbind(disparity.stratigraphy, disparity.duration, disparity.number)
  
  return(disparity.outputs)
}