## Functions

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











# Bootstrap and rarefy

boot.rarefy.data <- function(subsamples, bootstraps, metric){
  # Obtain a bootstrap matrix for the subsamples
  bootstrapped_data <- boot.matrix(subsamples, bootstraps = bootstraps, rarefaction = FALSE)
  # Estimate disparity - observed and bootstrapped
  disparity <- summary(dispRity(bootstrapped_data, metric = metric), round = 10)
  
  # Obtain a rarified bootstrap matrix for the subsamples
  rarefied_data <- boot.matrix(subsamples, bootstraps = bootstraps, rarefaction = TRUE)
  # Estimate disparity - observed and bootstrapped with rarefaction
  disparity_rarefied <- summary(dispRity(rarefied_data, metric = metric), round = 10)
  
  # Return both as list
  return(list(disparity, disparity_rarefied))
}


  





