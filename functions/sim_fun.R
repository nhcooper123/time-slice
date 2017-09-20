## Functions for time slice analyses
## Natalie Cooper 2017

##---------------------------------------------------------------------------
## Extract all types of subsample for both methods and four continuous models
## morphospace is the cleaned morphospace
## tree is the phylogeny matched to the morphospace
## bins is a vector of bin times
## FADLAD is first and last occurrence data
## inc.nodes is logical for whether to estimate diparity using nodal values
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
## disparity.object is the output of disparity
## bins is a vector of bin times
## metricname is a string specifying the name of the metric being used
## inc.nodes is logical for whether diparity was estimated using nodal values
##--------------------------------------------------------------------------

get.outputs <- function(disparity.object, bins, metric.name, inc.nodes){
  output <- as.data.frame(disparity.object)
  output$bins <- paste(as.character(bins), collapse = "")
  output$metric <- metric.name
  output$inc.nodes <- inc.nodes
  return(output)
 }

#### Also need some way to name lists/extract models to put into the output somehow???

##--------------------------------------------------------------------------
## Putting it all together
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
  disparity.outputs <- purrr::map(disparity.object, get.outputs, bins = bins, 
                                  metric.name = metric.name, inc.nodes = inc.nodes)

}
