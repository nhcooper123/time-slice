#' @title Plot max min
#'
#' @description  Plotting the maximum or minimum age/disparity
#'
#' @param data the max or minimum data
#' @param cols the different colours for the models
#' @param lty the different line types for the CIs
#' @param pch the point type for the median
#' @param relative.time the relative time for scaling
#' @param what whether to have the age on the Y axis or the disparity
#' @param adj the size adjustment between the point
#' @param type "age" or "epoch"
#' @param legend whether to plot the legend
#' @param legend.coords where to plot the legend (can be left empty)
#' @param xaxis whether to put the labels on the x axis
#' @param yaxis whether to put the labels on the y axis
#' @param relative.disparity whether to plot the disparity relatively
#' 
#' @examples
#' ##
#' 
#' @author Thomas Guillerme
#' @export
#' 

plot.max.min <- function(data, cols = c("black", "red", "green3", "blue", "cyan", "magenta", "yellow"), lty = c(1,2), pch = 19, what = "age", adj = 0.1, type = "age", legend = TRUE, relative.time, xaxis = FALSE, yaxis = FALSE, legend.coords, relative.disparity = FALSE, ...) {

    ## select the data type
    if(type == "age") {
        data <- data[1:21, ]
    } else {
        data <- data[22:42, ]
    }

    ## Get the type of data
    if(what == "age") {
        make.numeric <- function(X) {
            options(warn = -1)
            num_X <- as.numeric(X)
            options(warn = 0)
            if(is.na(num_X)) {
                return(as.numeric(strsplit(X, split = " - ")[[1]]))
            } else {
                return(num_X)
            }
        }
        ## Convert ages into numeric values
        data_values <- lapply(as.list(data[,1]), make.numeric)

        ## Adjusting ages
        if(!missing(relative.time)) {
            if(relative.time != FALSE) {
                data_values <- lapply(data_values, function(X, relative.time) return(X/relative.time), relative.time)
                ylim <- c(0,1)
            } else {
                ylim <- range(unlist(data_values))
            }
        } else {
            ylim <- range(unlist(data_values))
        }

    } else {
        data_values <- data[,4:8]
        if(relative.disparity) {
            ## Scale disparity
            data_values <- data_values/max(data_values)
            ylim <- range(as.vector(data_values))
        } else {
            ylim <- range(as.vector(data_values))
        }
    }

    ## Get the sorting
    # data_method <- list(which(data$bin_type == "stratigraphy"), which(data$bin_type == "duration"), which(data$bin_type == "number"))

    ## Plot parameters
    xlim <- c(0, 3)

    ## Empty plot
    ylab <- ifelse(yaxis, what, "")
    plot(0,0, col = "white", xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = ylab, ...)
    #plot(0,0, col = "white", xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = ylab)
    ## Add the lines
    abline(v = c(1, 2), lty = 3)
    ## Add the x axis
    if(xaxis) axis(1, at = c(0.5, 1.5, 2.5), label = c("stratigraphy", "duration", "number"), tick = FALSE)
    ## Add the y axis
    if(yaxis) axis(2)

    plot.point.line <- function(y_coordinates, x_loc, col, lty, pch) {
        if(length(y_coordinates) > 1) {
            lines(x = rep(xloc, 2), y_coordinates, lty = lty, col = col, lwd = 5)
        } else {
            points(x = xloc, y = y_coordinates, pch = pch, col = col)
        }
    }

    ## Number of models
    n_models <- length(unique(data$model))
    
    ## Add the data
    if(what == "age") {
        ## Stratigraphy
        xloc <- adj
        for(model in 1:n_models) {
            ## Plot the age
            plot.point.line(data_values[[model]], xloc, col = cols[model], lty = lty, pch = pch)
            ## Increment xloc
            xloc <- xloc + adj
        }

        ## Duration
        xloc <- adj + 1
        for(model in 1:n_models) {
            ## Plot the age
            plot.point.line(data_values[[model+n_models]], xloc, col = cols[model], lty = lty, pch = pch)
            ## Increment xloc
            xloc <- xloc + adj
        }

        ## Number
        xloc <- adj + 2
        for(model in 1:n_models) {
            ## Plot the age
            plot.point.line(data_values[[model+n_models*2]], xloc, col = cols[model], lty = lty, pch = pch)
            ## Increment xloc
            xloc <- xloc + adj
        }
    } else {

        ## Stratigraphy
        xloc <- adj
        for(model in 1:n_models) {
            ## Plot the age
            points(x = xloc, y = data_values$bs.median[model], pch = pch, col = cols[model])
            lines(x = rep(xloc, 2), y = c(data_values[,3][model], data_values[,4][model]), lty = lty[1], col = cols[model])
            lines(x = rep(xloc, 2), y = c(data_values[,2][model], data_values[,5][model]), lty = lty[2], col = cols[model])
            ## Increment xloc
            xloc <- xloc + adj
        }

        ## Duration
        xloc <- adj + 1
        for(model in 1:n_models) {
            ## Plot the age
            points(x = xloc, y = data_values$bs.median[model], pch = pch, col = cols[model])
            lines(x = rep(xloc, 2), y = c(data_values[,3][model+n_models], data_values[,4][model]), lty = lty[1], col = cols[model])
            lines(x = rep(xloc, 2), y = c(data_values[,2][model+n_models], data_values[,5][model]), lty = lty[2], col = cols[model])
            ## Increment xloc
            xloc <- xloc + adj
        }

        ## Number
        xloc <- adj + 2
        for(model in 1:n_models) {
            ## Plot the age
            points(x = xloc, y = data_values$bs.median[model], pch = pch, col = cols[model])
            lines(x = rep(xloc, 2), y = c(data_values[,3][model+n_models*2], data_values[,4][model]), lty = lty[1], col = cols[model])
            lines(x = rep(xloc, 2), y = c(data_values[,2][model+n_models*2], data_values[,5][model]), lty = lty[2], col = cols[model])
            ## Increment xloc
            xloc <- xloc + adj
        }
    }

    ## Add the legend
    if(legend) {
        if(missing(legend.coords)) {
            legend.coords <- "topleft"
        }
        legend(legend.coords, col = cols[1:n_models], pch = pch, bty = "n", cex =0.8, legend = unique(data$model))
    }
}


multi.plot.max.min <- function(data_list, what = c("age"), cols, lty, pch, adj = 0.1, relative.time, legend = FALSE, names.data) {

    length_data <- length(data_list)

    ## Plot layout
    plot_layout <- layout(matrix(c(1:(length_data*2)), 2, length_data, byrow = TRUE), rep(1, length_data*2), rep(1, length_data*2), FALSE)
    # layout.show(plot_layout)


    # if(what == "age") {
    if(!missing(relative.time)) {
        is_relative.time <- TRUE
        relative.disparity <- TRUE
    } else {
        is_relative.time <- FALSE
        relative.disparity <- FALSE
    }
    # }

    ## Set the relative time (if necessary)
    if(is_relative.time) {
        relative_time <- relative.time[[1]]
    }

    ## First plot (first row)
    par(mar = c(0, 6, 4, 0)) #c(bottom, left, top, right)
    plot.max.min(data_list[[1]], cols = colours, lty = lty, pch = pch, what = what, adj = adj, type = "age", xaxis = FALSE, yaxis = TRUE, relative.time = relative_time, relative.disparity = relative.disparity, legend = legend, main = names.data[[1]])

    ## Other plots (first row)
    for(slug in 2:(length_data-1)) {

        if(is_relative.time) {
            relative_time <- relative.time[[slug]]
        }

        par(mar = c(0, 0, 4, 0)) #c(bottom, left, top, right)
        plot.max.min(data_list[[slug]], cols = colours, lty = lty, pch = pch, what = what, adj = adj, type = "age", xaxis = FALSE, yaxis = FALSE, relative.time = relative_time, relative.disparity = relative.disparity, legend = FALSE, main = names.data[[slug]])
    }

    if(is_relative.time) {
        relative_time <- relative.time[[4]]
    }

    ## Last plot (first row)
    par(mar = c(0, 0, 4, 3))
    plot.max.min(data_list[[length_data]], cols = colours, lty = lty, pch = pch, what = what, adj = adj, type = "age", xaxis = FALSE, yaxis = FALSE, relative.time = relative_time, relative.disparity = relative.disparity, legend = FALSE, main = names.data[[length_data]])
    
    if(is_relative.time) {
        axis(4, at = 0.5, tick = FALSE, label = "age")
    } 


    ## Set the relative time (if necessary)
    if(is_relative.time) {
        relative_time <- relative.time[[1]]
    }

    ## First plot (second row)
    par(mar = c(4, 6, 0, 0)) #c(bottom, left, top, right)
    plot.max.min(data_list[[1]], cols = colours, lty = lty, pch = pch, what = what, adj = adj, type = "epoch", xaxis = TRUE, yaxis = TRUE, relative.time = relative_time, relative.disparity = relative.disparity, legend = FALSE)

    ## Other plots (second row)
    for(slug in 2:(length_data-1)) {

        if(is_relative.time) {
            relative_time <- relative.time[[slug]]
        }

        par(mar = c(4, 0, 0, 0)) #c(bottom, left, top, right)
        plot.max.min(data_list[[slug]], cols = colours, lty = lty, pch = pch, what = what, adj = adj, type = "epoch", xaxis = TRUE, yaxis = FALSE, relative.time = relative_time, relative.disparity = relative.disparity, legend = FALSE)
    }

    if(is_relative.time) {
        relative_time <- relative.time[[4]]
    }

    ## Last plot (second row)
    par(mar = c(4, 0, 0, 3))
    plot.max.min(data_list[[length_data]], cols = colours, lty = lty, pch = pch, what = what, adj = adj, type = "epoch", xaxis = TRUE, yaxis = FALSE, relative.time = relative_time, relative.disparity = relative.disparity, legend = FALSE)
    
    if(is_relative.time) {
        axis(4, at = 0.5, tick = FALSE, label = "epoch")
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
    if(xaxis2) axis(3, at = c(seq(from = 0, to = 1, by = 1/8)), label = rep(c("e:1", "e:2", "e:3"), 3), tick = FALSE, padj = 1.5, cex = 0.8)
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

        # ## Other plots (first row)
        # for(slug in 2:(length(data)-1)) {
        #     par(mar = c(0, 0, 4, 0)) #c(bottom, left, top, right)
        #     plot.extinction(data[[slug]], type = type[1], col = cols, main = data.names[[slug]], xaxis2 = TRUE, ...)
        # }

        ## Last plot (first row)
        par(mar = c(0, 0, 4, 3)) #c(bottom, left, top, right)
        plot.extinction(data[[length(data)]], type = type[1], col = cols, main = data.names[[length(data)]], xaxis2 = TRUE, yaxis2 = TRUE, ...)

        ## First plot (second row)
        par(mar = c(4, 6, 0, 0)) #c(bottom, left, top, right)
        plot.extinction(data[[1]], type = type[2], col = cols, yaxis = TRUE, xaxis = TRUE, ...)

        # ## Other plots (second row)
        # for(slug in 2:(length(data)-1)) {
        #     par(mar = c(4, 0, 0, 0)) #c(bottom, left, top, right)
        #     plot.extinction(data[[slug]], type = type[2], col = cols, xaxis = TRUE, ...)
        # }

        ## Last plot (first row)
        par(mar = c(4, 0, 0, 3)) #c(bottom, left, top, right)
        plot.extinction(data[[length(data)]], type = type[2], col = cols, xaxis = TRUE, yaxis2 = TRUE, ...)
    }
}








