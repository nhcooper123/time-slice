#' @title Plotting TreeCmp results
#'
#' @description Simple wrapper for plotting the results from TreeCmp
#'
#' @param taxa_list a list of tree metrics from \code{read.TreeCmp} for all the scenario for a number of taxa
#' @param metric a numeric of which metric to use (\code{1} = \code{MatchingCluster}, \code{2} = \code{R.F_Cluster}, \code{3} = \code{NodalSplitted}, \code{4} = \code{Triples})
#' @param what which type of plot? Can be \code{"null"}, \code{"real"}, \code{"true"}, \code{"check"}. See details.
#' @param ylim \code{ylim} parameter from \code{plot} (if missing is automatic)
#' @param legend \code{logical}, whether to display the legend
#' @param NTS \code{logical}, whether NTS was used
#' @param ylab a \code{character} string to be passed to \code{boxplot(..., ylab)}
#' @param xlab a \code{character} string to be passed to \code{boxplot(..., xlab)}
#' @param axislab a \code{character} string to be passed to \code{axis(..., labels)}
#' @param ... any additional argument to be passed to \code{boxplot}
#'
#' @details
#' \code{"null"} = "maxi", "mini", "norm" tree vs. "rand"
#' \code{"best"} = "maxi", "mini", "rand" tree vs. "norm"
#' \code{"true"} = "norm", "rand" tree vs. "true"
#' \code{"check"} = "norm", vs "rand"
#' 
#' @return
#' A list of metrics for the comparisons.
#' 
#' @examples
#' ##
#' 
#' @author Thomas Guillerme
#' @export
#' 


plot.results.single <- function(taxa_list, metric, what, ylim, legend = FALSE, NTS = TRUE, ylab, xlab, axislab, ...) {
    
    ## Graphic parameters
    ## Colours
    cols <- c("red", "orange", "green3", "lightgreen", "blue", "lightblue")
    
    ## Y lab
    if(missing(ylab)) {
        if(metric == 1) ylab <- "Similarity (Matching Cluster)"
        if(metric == 2) ylab <- "Similarity (Robinson-Foulds)"
        if(metric == 3) ylab <- "Similarity (Nodal Split)"
        if(metric == 4) ylab <- "Similarity (Triplets)"
    }

    ## Y lim
    get.min.list <- function(X, metric) {
        return(min(unlist(X[,metric])))
    }

    min <- min(unlist(lapply(unlist(unlist(unlist(taxa_list, recursive = FALSE), recursive = FALSE), recursive = FALSE), get.min.list, metric)))
    if(min < 0) {
        ylim <- c(min,1)
    } else {
        ylim <- c(0,1)
    }

    ## X lab
    if(missing(xlab)) {
        xlab <- "Characters"
    }

    ## axis lab
    if(missing(axislab)) {
        axislab <- c("100", "350", "1000")
    }

    ## Horizontal line
    line <- 0
    

    if(what == "null") {
        boxplot(xlab = xlab, ylab = ylab, xaxt = "n", ylim = ylim, col = rep(cols, 3), las = 1, ...,
            ## c100
            unlist(taxa_list$c100$bayesian$rand$maxi[,metric]), unlist(taxa_list$c100$parsimony$rand$maxi[,metric]),
            unlist(taxa_list$c100$bayesian$rand$mini[,metric]), unlist(taxa_list$c100$parsimony$rand$mini[,metric]),
            unlist(taxa_list$c100$bayesian$rand$norm[,metric]), unlist(taxa_list$c100$parsimony$rand$norm[,metric]),
            ## c350
            unlist(taxa_list$c350$bayesian$rand$maxi[,metric]), unlist(taxa_list$c350$parsimony$rand$maxi[,metric]),
            unlist(taxa_list$c350$bayesian$rand$mini[,metric]), unlist(taxa_list$c350$parsimony$rand$mini[,metric]),
            unlist(taxa_list$c350$bayesian$rand$norm[,metric]), unlist(taxa_list$c350$parsimony$rand$norm[,metric]),
            ## c1000
            unlist(taxa_list$c1000$bayesian$rand$maxi[,metric]), unlist(taxa_list$c1000$parsimony$rand$maxi[,metric]),
            unlist(taxa_list$c1000$bayesian$rand$mini[,metric]), unlist(taxa_list$c1000$parsimony$rand$mini[,metric]),
            unlist(taxa_list$c1000$bayesian$rand$norm[,metric]), unlist(taxa_list$c1000$parsimony$rand$norm[,metric])
            )

        ## X axis
        axis(1, 1:18, labels = FALSE, tick = FALSE)
        axis(1, c(3.5, 9.5, 15.5), tick = FALSE, labels = axislab)
        ## Lines
        abline(v = 6.5) ; abline(v = 12.5)
        abline(h = line, col = "grey", lty = 2)
        ## Legend
        if(legend) {
            legend("bottomleft", col = cols, pch = 19, bty = "n", title = "Character differences:", cex = 0.8,
                legend = c("Maximised (Bayesian)", "Maximised (Parsimony)",
                           "Minimised (Bayesian)", "Minimised (Parsimony)",
                           "Normal (Bayesian)", "Normal (Parsimony)")
                )
        }
    }

    if(what == "best") {
        boxplot(xlab = xlab, ylab = ylab, xaxt = "n", ylim = ylim, col = rep(cols, 3), las = 1, ...,
            ## c100
            unlist(taxa_list$c100$bayesian$norm$maxi[,metric]), unlist(taxa_list$c100$parsimony$norm$maxi[,metric]),
            unlist(taxa_list$c100$bayesian$norm$mini[,metric]), unlist(taxa_list$c100$parsimony$norm$mini[,metric]),
            unlist(taxa_list$c100$bayesian$norm$rand[,metric]), unlist(taxa_list$c100$parsimony$norm$rand[,metric]),
            ## c350
            unlist(taxa_list$c350$bayesian$norm$maxi[,metric]), unlist(taxa_list$c350$parsimony$norm$maxi[,metric]),
            unlist(taxa_list$c350$bayesian$norm$mini[,metric]), unlist(taxa_list$c350$parsimony$norm$mini[,metric]),
            unlist(taxa_list$c350$bayesian$norm$rand[,metric]), unlist(taxa_list$c350$parsimony$norm$rand[,metric]),
            ## c1000
            unlist(taxa_list$c1000$bayesian$norm$maxi[,metric]), unlist(taxa_list$c1000$parsimony$norm$maxi[,metric]),
            unlist(taxa_list$c1000$bayesian$norm$mini[,metric]), unlist(taxa_list$c1000$parsimony$norm$mini[,metric]),
            unlist(taxa_list$c1000$bayesian$norm$rand[,metric]), unlist(taxa_list$c1000$parsimony$norm$rand[,metric])
            )

        ## X axis
        axis(1, 1:18, labels = FALSE, tick = FALSE)
        axis(1, c(3.5, 9.5, 15.5), tick = FALSE, labels = axislab)
        ## Lines
        abline(v = 6.5) ; abline(v = 12.5)
        abline(h = line, col = "grey", lty = 2)
        ## Legend
        if(legend) {
            legend("bottomleft", col = cols, pch = 19, bty = "n", title = "Character differences:", cex = 0.8,
                legend = c("Maximised (Bayesian)", "Maximised (Parsimony)",
                           "Minimised (Bayesian)", "Minimised (Parsimony)",
                           "Randomised (Bayesian)", "Randomised (Parsimony)")
                )
        }
    }

    if(what == "true") {
        boxplot(xlab = xlab, ylab = ylab, xaxt = "n", ylim = ylim, col = rep(cols[1:4], 3), las = 1, ...,
            ## c100
            unlist(taxa_list$c100$bayesian$norm$true[,metric]), unlist(taxa_list$c100$parsimony$norm$true[,metric]),
            unlist(taxa_list$c100$bayesian$rand$true[,metric]), unlist(taxa_list$c100$parsimony$rand$true[,metric]),
            ## c350
            unlist(taxa_list$c350$bayesian$norm$true[,metric]), unlist(taxa_list$c350$parsimony$norm$true[,metric]),
            unlist(taxa_list$c350$bayesian$rand$true[,metric]), unlist(taxa_list$c350$parsimony$rand$true[,metric]),
            ## c1000
            unlist(taxa_list$c1000$bayesian$norm$true[,metric]), unlist(taxa_list$c1000$parsimony$norm$true[,metric]),
            unlist(taxa_list$c1000$bayesian$rand$true[,metric]), unlist(taxa_list$c1000$parsimony$rand$true[,metric])
            )

        ## X axis
        axis(1, 1:12, labels = FALSE, tick = FALSE)
        axis(1, c(2.5, 6.5, 10.5), tick = FALSE, labels = axislab)
        ## Lines
        abline(v = 4.5) ; abline(v = 8.5)
        abline(h = line, col = "grey", lty = 2)
        ## Legend
        if(legend) {
            legend("bottomleft", col = cols[1:4], pch = 19, bty = "n", title = "Character differences:", cex = 0.8,
                legend = c("Normal (Bayesian)", "Normal (Parsimony)",
                           "Randomised (Bayesian)", "Randomised (Parsimony)")
                )
        }
    }

    if(what == "check") {
        boxplot(xlab = xlab, ylab = ylab, xaxt = "n", ylim = ylim, col = rep(cols[1:4], 3), las = 1, ...,
            ## c100
            unlist(taxa_list$c100$bayesian$norm$rand[,metric]), unlist(taxa_list$c100$parsimony$norm$rand[,metric]),
            unlist(taxa_list$c100$bayesian$rand$norm[,metric]), unlist(taxa_list$c100$parsimony$rand$norm[,metric]),
            ## c350
            unlist(taxa_list$c350$bayesian$norm$rand[,metric]), unlist(taxa_list$c350$parsimony$norm$rand[,metric]),
            unlist(taxa_list$c350$bayesian$rand$norm[,metric]), unlist(taxa_list$c350$parsimony$rand$norm[,metric]),
            ## c1000
            unlist(taxa_list$c1000$bayesian$norm$rand[,metric]), unlist(taxa_list$c1000$parsimony$norm$rand[,metric]),
            unlist(taxa_list$c1000$bayesian$rand$norm[,metric]), unlist(taxa_list$c1000$parsimony$rand$norm[,metric])
            )

        ## X axis
        axis(1, 1:12, labels = FALSE, tick = FALSE)
        axis(1, c(2.5, 6.5, 10.5), tick = FALSE, labels = axislab)
        ## Lines
        abline(v = 4.5) ; abline(v = 8.5)
        abline(h = line, col = "grey", lty = 2)
        ## Legend
        if(legend) {
            legend("bottomleft", col = cols[1:4], pch = 19, bty = "n", title = "Character differences:", cex = 0.8,
                legend = c("Normal (Bayesian)", "Normal (Parsimony)",
                           "Randomised (Bayesian)", "Randomised (Parsimony)")
                )
        }
    }
}




#' @title Plotting extinction results
#'
#' @description Wrapper for plotting the results of the extinction analysis
#'
#' @param data an extinction.X.csv table or a list of tables
#' @param type which type of data (age or epoch)
#' @param cols a vector of two colours: significant/non-significant/NA c("blue", "orange", "white")
#' @param xaxis whether to put the labels on the x axis
#' @param yaxis whether to put the labels on the y axis
#' @param xaxis2 whether to put the labels on the second (upper) x axis
#' @param yaxis2 whether to put the labels on the second (right) y axis
#' @param data.names the names of the datasets when data is a list
#' 
#' @examples
#' ##
#' 
#' @author Thomas Guillerme
#' @export
#' 

plot.extinction <- function(data, type, cols = c("blue", "orange", "white"), main = "", xaxis = FALSE, yaxis = FALSE, xaxis2 = FALSE, yaxis2 = FALSE, ...) {

    ## Formatting data data as a matrix for image
    age_rows <- list(c(1:7), c(8:14), c(15:21))
    epoch_rows <- list(c(22:28), c(29:35), c(36:42))
    signif_columns <- c(10:12)

    ## Combining the age matrix
    if(type == "age") {
        data_matrix <- as.matrix(do.call(cbind, lapply(age_rows, function(rows, cols, data) return(data[rows, cols]), cols = signif_columns, data = data)))
    } else {
        data_matrix <- as.matrix(do.call(cbind, lapply(epoch_rows, function(rows, cols, data) return(data[rows, cols]), cols = signif_columns, data = data)))
    }

    ## Convert into binary
    data_matrix <- ifelse(data_matrix, 1, 0)
    data_matrix <- ifelse(is.na(data_matrix), 0.5, data_matrix)

    ## Colours equivalent
    request_colors <- length(unique(as.vector(data_matrix)))
    if(request_colors == 3) {
        ## Reorder colours (damn image()!)
        cols <- cols[c(2,3,1)]
    }

    colours_table <- matrix(c(1,0,0.5), ncol = 1, dimnames = list(cols))
    num_cols <- na.omit(sort(match(unique(as.vector(data_matrix)), colours_table)))

    ## Selecting the colours
    colours <- rownames(colours_table)[num_cols]

    ## Plot the matrix
    image(t(data_matrix[7:1,]), col = rev(colours), xaxt = "n", yaxt = "n", main = main, ...)
    # image(t(data_matrix[7:1,]), col = rev(cols), xaxt = "n", yaxt = "n", main = main)
    ## Add the lines
    abline(v = c(0.3125, 0.6875), lty = 2)
    ## Add the y axis
    if(yaxis) axis(2, at = seq(from = 0, to = 1, by = 1/6), las = 2, label = rev(data$model[age_rows[[1]]]), tick = FALSE)
    ## Add the x axis
    if(xaxis) axis(1, at = c(0.125, 0.5, 0.85), label = c("stratigraphy", "duration", "number"), tick = FALSE)
    ## Add the upper x axis
    if(xaxis2) axis(3, at = c(seq(from = 0, to = 1, by = 1/8)), label = rep(c("e:1", "e:2", "e:3"), 3), tick = FALSE, padj = 1.5)
    ## Add the right y axis
    if(yaxis2) axis(4, at = 0.5, tick = FALSE, labels = type)
}


multi.plot.extinction <- function(data, type, cols = c("blue", "orange", "white"), main = "", data.names, ...) {

    ## Check how many types
    if(length(type) == 1) {

        ## Setting up plot layout
        plot_layout <- layout(matrix(c(1:length(data)), 1, length(data), byrow = TRUE), rep(1, length(data)), rep(1, length(data)), FALSE)
        #layout.show(plot_layout)

        ## First plot
        par(mar = c(4, 6, 4, 0)) #c(bottom, left, top, right)
        plot.extinction(data[[1]], type = type, col = cols, xaxis = TRUE, yaxis = TRUE, xaxis2 = TRUE, main = data.names[[1]], ...)

        ## Other plots
        for(slug in 2:length(data)) {
            par(mar = c(4, 0, 4, 0)) #c(bottom, left, top, right)
            plot.extinction(data[[slug]], type = type, col = cols, main = data.names[[slug]], xaxis = TRUE, xaxis2 = TRUE, ...)
        }
    } else {

        ## Setting up plot layout
        plot_layout <- layout(matrix(c(1:(length(data)*2)), 2, length(data), byrow = TRUE), rep(1, length(data)*2), rep(1, length(data)*2), FALSE)
        #layout.show(plot_layout)

        ## First plot (first row)
        par(mar = c(0, 6, 4, 0)) #c(bottom, left, top, right)
        plot.extinction(data[[1]], type = type[1], col = cols, yaxis = TRUE, xaxis2 = TRUE, main = data.names[[1]], ...)

        ## Other plots (first row)
        for(slug in 2:(length(data)-1)) {
            par(mar = c(0, 0, 4, 0)) #c(bottom, left, top, right)
            plot.extinction(data[[slug]], type = type[1], col = cols, main = data.names[[slug]], xaxis2 = TRUE, ...)
        }

        ## Last plot (first row)
        par(mar = c(0, 0, 4, 3)) #c(bottom, left, top, right)
        plot.extinction(data[[length(data)]], type = type[1], col = cols, main = data.names[[length(data)]], xaxis2 = TRUE, yaxis2 = TRUE, ...)

        ## First plot (second row)
        par(mar = c(4, 6, 0, 0)) #c(bottom, left, top, right)
        plot.extinction(data[[1]], type = type[2], col = cols, yaxis = TRUE, xaxis = TRUE, ...)

        ## Other plots (second row)
        for(slug in 2:(length(data)-1)) {
            par(mar = c(4, 0, 0, 0)) #c(bottom, left, top, right)
            plot.extinction(data[[slug]], type = type[2], col = cols, xaxis = TRUE, ...)
        }

        ## Last plot (first row)
        par(mar = c(4, 0, 0, 3)) #c(bottom, left, top, right)
        plot.extinction(data[[length(data)]], type = type[2], col = cols, xaxis = TRUE, yaxis2 = TRUE, ...)
    }
}








