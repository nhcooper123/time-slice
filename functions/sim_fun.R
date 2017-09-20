## Functions


##-------------------------------------------
## Creating subsamples for disparity analyses
##-------------------------------------------

## Extract all types of subsample for both methods and four continuous models
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

## Bootstrap and rarefy subsamples
## subsamples is an individual subsample
## bootstraps is the number of bootstraps to run
boot.rarefy <- function(subsamples, bootstraps){
  # Obtain a rarefied bootstrap matrix matrix for the subsamples
  bootstrapped_data <- boot.matrix(subsamples, bootstraps = bootstraps, rarefaction = TRUE)
  return(bootstrapped_data)
}

## Bootstrap and rarefy all types of subsamples
## subsamples is output from get.subsamples as a list
boot.rarefy.all <- function(subsamples, bootstraps){
  purrr::map(subsamples, boot.rarefy, bootstraps = bootstraps)
}

##-------------------------------------------
## Extract disparity for all subsamples
##-------------------------------------------

## Estimate disparity - observed and bootstrapped
## subsampled_data is the output of boot.rarefy.all
get.disparity <- function(subsampled_data, metric){
  summary(dispRity(subsampled_data, metric = metric), round = 10)
}

## Get disparity for all types of subsamples
## subsampled_data is the output of boot.rarefy.all as a list
get.disparity.all <- function(subsampled_data, metric){
  purrr::map(subsampled_data, get.disparity, metric = metric)
}

##------------------------------------------
## Output disparity results from simulations
##------------------------------------------

## Extract outputs needed for results
get.outputs <- function(disparity.object, bins, metric.name, inc.nodes){
  output <- as.data.frame(disparity.object)
  output$bins <- paste(as.character(bins), collapse = "")
  output$metric <- metric.name
  output$inc.nodes <- inc.nodes
  return(output)
 }

## Save outputs to correct folder and file
save.outputs <- function(output, path.to.folder, slug){
  readr::write_csv(output, path = paste0(path.to.folder, "_", slug, ".csv"), 
                   append = TRUE)
}

## Combine output extraction and saving
output.results <- function(disparity.object, bins, metric.name, inc.nodes, 
	                            path.to.folder, slug){
  output <- get.outputs(disparity.object, bins, metric.name, inc.nodes)
  save.outputs(output, path.to.folder, slug)
}

## Apply to list
output.all.results <- function(disparity.object, bins, metric.name, inc.nodes, 
	                            path.to.folder, slug){

  purrr::map(disparity.object, output.results, bins = bins, metric.name = metric.name, 
  	         inc.nodes = inc.nodes, path.to.folder = path.to.folder, slug = slug)
}

##### Still not outputting to file
#### Also need some way to name lists/extract models to put into the output somehow???

##------------------------------------
## Putting it all together
##------------------------------------
#path.to.folder = ../outputs/timebins_rarefied

run.all.disparity <- function(morphospace, tree, bins, FADLAD, inc.nodes = TRUE,
                              bootstraps = 1, metric, metric.name, path.to.folder, slug){

  # Need to remove the nodes if not using them
  if(inc.nodes == FALSE){
    remove.nodes <- clean.data(morphospace, tree)
    morphospace <- remove.nodes$data  
  }

  ## Extract all types of subsample for both methods and four continuous models
  subsamples <- get.subsamples(morphospace, tree, bins, FADLAD, inc.nodes)

  ## Bootstrap and rarefy all types of subsamples
  subsampled_data <- boot.rarefy.all(subsamples, bootstraps)

  ## Get disparity for all types of subsamples
  disparity.object <- get.disparity.all(subsampled_data, metric)

  ## Outputs
  output.results(disparity.object, bins, metric.name, inc.nodes, 
	             path.to.folder, slug)

}
