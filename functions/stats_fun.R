#' @title Select max min methods
#'
#' @description Selects the maximum/minimum values for each model for each method
#'
#' @param method the objects for a bin type method (e.g. for stratigraphy, data$objects$stratigraphy)
#' @param fun the selection function (e.g. max() or min())
#' @param method.name optional, the method name
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

select.max.min.method <- function(method, fun = max, method.name) {

  ## Selecting the max/min for one disparity object
  select.max.min <- function(disparity, fun = max) {
    ## Get the model
    model <- disparity$call$subsets[length(disparity$call$subsets)]
    model <- ifelse(model == "discrete", "equalbins", model)

    ## Summarise the data
    summary_data <- summary(disparity, round = 10)
    return(cbind(summary_data[which(summary_data[,4] == fun(summary_data[,4], na.rm = TRUE))[1],], "model" = model))
  }  

  ## Apply the select.max.min function to the whole method
  max.min_table <- do.call(rbind, lapply(method, select.max.min, fun = max))

  ## Add the method name (optional)
  if(!missing(method.name)) {
      max.min_table <- cbind(max.min_table, "bin_type" = rep(method.name, nrow(max.min_table)))
  }

  return(max.min_table)
}