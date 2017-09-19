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
## subsamples is output from get.subsamples as a list
boot.rarefy <- function(subsamples, bootstraps){
  # Obtain a bootstrap matrix for the subsamples
  bootstrapped_data <- boot.matrix(subsamples, bootstraps = bootstraps, rarefaction = FALSE)
  # Obtain a rarefied bootstrap matrix for the subsamples
  rarefied_data <- boot.matrix(subsamples, bootstraps = bootstraps, rarefaction = TRUE)
}  
  return(list(bootstrapped_data, rarefied_data))
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
  output$bins <- bins
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
