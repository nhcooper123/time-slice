# Functions to run the simulations and extract data
#---------------------------------------------------

#---------------------------------------------------
# 1. STRATIGRAPHY BINS
#---------------------------------------------------


#---------------------------------------------------
# 2. TIME BINS
# Allows you to vary the metric, bins and whether
# to include nodes
# Outputs to a slug specific text file
# Bootstraps and rarefies
#---------------------------------------------------
time.bin.sims <- function(morphospace, tree, FADLAD, bins, metric, 
                          metric.name, inc.nodes = TRUE, slug) {
  
  # Need to remove the nodes if not using them
  if(inc.nodes == FALSE){
    remove.nodes <- clean.data(morphospace, tree)
    morphospace <- remove.nodes$data  
  }
  
  # Subset samples by required number of time bins
  subsamples <- time.subsamples(data = morphospace, tree = tree, method = "discrete",
                                time = bins, FADLAD = FADLAD, inc.nodes = inc.nodes)
  
  # Obtain a bootstrap matrix for the subsamples
  bootstrapped_data <- boot.matrix(subsamples, bootstraps = 1, rarefaction = FALSE)
  
  # Estimate disparity - observed and bootstrapped
  disparity <- summary(dispRity(bootstrapped_data, metric = metric), round = 10)
  
  # Rarefaction appears to be broken...
  # Obtain a rarified bootstrap matrix for the subsamples
  rarefied_data <- boot.matrix(subsamples, bootstraps = 1, rarefaction = TRUE)
  
  # Estimate disparity - observed and bootstrapped with rarefaction
  disparity_rarefied <- summary(dispRity(rarefied_data, metric = metric), round = 10)
  
  # Outputs
  output <- as.data.frame(disparity)
  output$bins <- bins
  output$metric <- metric.name
  output$inc.nodes <- inc.nodes
  readr::write_csv(output, path = paste0("../outputs/timebins.", slug ,".csv"), 
                   append = TRUE)
  
  output2 <- as.data.frame(disparity_rarefied)
  output2$bins <- bins
  output2$metric <- metric.name
  output2$inc.nodes <- inc.nodes
  readr::write_csv(output2, path = paste0("../outputs/timebins_rarefied.", slug ,".csv"), 
                   append = TRUE)
    
}

#---------------------------------------------------
# 3. TIME SLICES
# Allows you to vary the metric, slices and whether
# to include nodes, plus model of evolution
# Outputs to a slug specific text file
# Bootstraps and rarefies
#---------------------------------------------------
time.slice.sims <- function(morphospace, tree, FADLAD, bins, metric, 
                          metric.name, inc.nodes = TRUE, slug) {
  
  # Need to remove the nodes if not using them
  if(inc.nodes == FALSE){
    remove.nodes <- clean.data(morphospace, tree)
    morphospace <- remove.nodes$data  
  }
  
  # Argh need to write function here at this is a mess!!!!!
  # Subset samples by required number of time bins with acctran model
  subsamples.acctran <- time.subsamples(data = morphospace, tree = tree, method = "continuous",
                                time = bins, FADLAD = FADLAD, inc.nodes = inc.nodes,
                                model = "acctran")
  
  # Subset samples by required number of time bins with acctran model
  subsamples.acctran <- time.subsamples(data = morphospace, tree = tree, method = "continuous",
                                        time = bins, FADLAD = FADLAD, inc.nodes = inc.nodes,
                                        model = "deltran")
  
  # Subset samples by required number of time bins with acctran model
  subsamples.acctran <- time.subsamples(data = morphospace, tree = tree, method = "continuous",
                                        time = bins, FADLAD = FADLAD, inc.nodes = inc.nodes,
                                        model = "punctuated")
  
  # Subset samples by required number of time bins with acctran model
  subsamples.acctran <- time.subsamples(data = morphospace, tree = tree, method = "continuous",
                                        time = bins, FADLAD = FADLAD, inc.nodes = inc.nodes,
                                        model = "gradual")
  
  # Obtain a bootstrap matrix for the subsamples
  bootstrapped_data <- boot.matrix(subsamples, bootstraps = 1, rarefaction = FALSE)
  
  # Estimate disparity - observed and bootstrapped
  disparity <- summary(dispRity(bootstrapped_data, metric = metric), round = 10)
  
  # Obtain a rarified bootstrap matrix for the subsamples
  rarefied_data <- boot.matrix(subsamples, bootstraps = 1, rarefaction = TRUE)
  
  # Estimate disparity - observed and bootstrapped with rarefaction
  disparity_rarefied <- summary(dispRity(rarefied_data, metric = metric), round = 10)
  
  # Outputs
  output <- as.data.frame(disparity)
  output$bins <- bins
  output$metric <- metric.name
  output$inc.nodes <- inc.nodes
  readr::write_csv(output, path = paste0("../outputs/timeslices.", slug ,".csv"), 
                   append = TRUE)
  
  output2 <- as.data.frame(disparity_rarefied)
  output2$bins <- bins
  output2$metric <- metric.name
  output2$inc.nodes <- inc.nodes
  readr::write_csv(output2, path = paste0("../outputs/timeslices_rarefied.", slug ,".csv"), 
                   append = TRUE)
  
}

#------------------------------------
# 4. Combination function
# Run all three methods on a dataset
#------------------------------------

run.all.sims <- function(morphospace, tree, FADLAD, bins, metric, 
                         metric.name, inc.nodes = TRUE, slug, model){

  time.bin.sims(morphospace, tree, FADLAD, bins, metric, 
              metric.name, inc.nodes = TRUE, slug)
  time.slice.sims(morphospace, tree, FADLAD, bins, metric, 
              metric.name, inc.nodes = TRUE, slug, model)
}
