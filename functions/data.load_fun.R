#' @title Load FADLAD
#'
#' @description Loading and formatting the First and Last Apparition Data
#'
#' @param chain the chain name
#' @param header logical, whether there is a header to the file
#' @param row.names numeric, which row names to use
#' 
#' @author Thomas Guillerme

load.FADLAD <- function(chain, header = TRUE, row.names = 1) {
    ## Set up the path
    current_wd <- strsplit(getwd(), split = "time-slice")[[1]]
    current <- current_wd[2]
    if(is.na(current)) {
        stop("Wrong directory root (should be '.../time-slice')")
    } else {
        path <- paste0(current_wd[1], "time-slice/")
    }

    ## Get the right file name
    file <- list.files(path = paste0(path, "Data/FADLAD/"), pattern = chain)
    if(length(file) == 0) {
        stop(paste("No FADLAD file found for", chain, "in", path))
    }

    ## Read the file
    FADLAD_table <- read.csv(paste0(path, "Data/FADLAD/", file), header = header, row.names = row.names)

    ## Check if the FADLAD name is correct
    if(class(rownames(FADLAD_table)) != "character") {
        warning("Rownames of the FADLAD object don't have the right class (should be character).")
    }

    if(colnames(FADLAD_table)[1] != "FAD" && colnames(FADLAD_table)[1] != "LAD") {
        warning("The two first column names are in the wrong format (should be 'FAD' and 'LAD').")
    }

    return(FADLAD_table)
}

#' @title Load space
#'
#' @description Loading and formatting the multidimensional space
#'
#' @param chain the chain name
#' 
#' @author Thomas Guillerme

load.space <- function(chain) {
    return()
}

#' @title Load tree
#'
#' @description Loading and formatting the tree
#'
#' @param chain the chain name
#' 
#' @author Thomas Guillerme

load.tree <- function(chain) {
    return()
}

