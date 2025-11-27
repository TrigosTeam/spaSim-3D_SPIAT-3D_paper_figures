### Poisson point process ----

# 1. Get empty box with borders
plot_bordered_box <- function(border_color = "#01478c") {
  
  fig <- add_trace(plot_ly(),
                   type = "scatter3d",
                   mode = "markers")
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     aspectmode = "cube"))
  
  ### Add border lines
  x <- c(rep(c(0, 500), 2), rep(c(0, 500), each = 2), rep(c(0, 500), each = 4), rep(c(0, 500), 2), rep(c(0, 500), each = 2))
  y <- c(rep(c(0, 500), each = 2), rep(c(0, 500), 2), rep(rep(c(0, 500), each = 2), 2), rep(c(0, 500), each = 2), rep(c(0, 500), 2))
  z <- c(rep(0, 8), rep(c(0, 500), 4), rep(500, 8))
  
  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (i in seq(1, length(x), 2)) {
    x_lines <- c(x_lines, x[i], x[i + 1], NA)
    y_lines <- c(y_lines, y[i], y[i + 1], NA)
    z_lines <- c(z_lines, z[i], z[i + 1], NA)
  }
  
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = "lines",
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = border_color, width = 10)
  )
  
  return(fig)  
}
plot_bordered_box()


# 2. Get empty box with borders and grid
plot_bordered_box_with_grid <- function(border_color = "#01478c",
                                        grid_color = "#007128") {
  
  fig <- add_trace(plot_ly(),
                   type = "scatter3d",
                   mode = "markers")
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     aspectmode = "cube"))
  
  ### Add grid lines
  x <- c(rep(c(rep(c(0, 500), 6), rep(seq(0, 500, 100), each = 2)), 6), rep(c(rep(seq(0, 500, 100), each = 2), rep(c(0, 500), 6)), 6), rep(seq(0, 500, 100), each = 24))
  y <- c(rep(c(rep(seq(0, 500, 100), each = 2), rep(c(0, 500), 6)), 6), rep(seq(0, 500, 100), each = 24), rep(c(rep(c(0, 500), 6), rep(seq(0, 500, 100), each = 2)), 6))
  z <- c(rep(seq(0, 500, 100), each = 24), rep(c(rep(c(0, 500), 6), rep(seq(0, 500, 100), each = 2)), 6), rep(c(rep(seq(0, 500, 100), each = 2), rep(c(0, 500), 6)), 6))
  
  # Define pairs of points to connect
  pairs <- list(c(1, 2))  # Connect point 1 to 2
  
  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (i in seq(1, length(x), 2)) {
    x_lines <- c(x_lines, x[i], x[i + 1], NA)
    y_lines <- c(y_lines, y[i], y[i + 1], NA)
    z_lines <- c(z_lines, z[i], z[i + 1], NA)
  }
  
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = "lines",
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = grid_color, width = 3)
  )
  
  ### Add border lines
  x <- c(rep(c(0, 500), 2), rep(c(0, 500), each = 2), rep(c(0, 500), each = 4), rep(c(0, 500), 2), rep(c(0, 500), each = 2))
  y <- c(rep(c(0, 500), each = 2), rep(c(0, 500), 2), rep(rep(c(0, 500), each = 2), 2), rep(c(0, 500), each = 2), rep(c(0, 500), 2))
  z <- c(rep(0, 8), rep(c(0, 500), 4), rep(500, 8))
  
  # Create coordinate vectors with NA between segments
  x_lines1 <- y_lines1 <- z_lines1 <- c()
  for (i in seq(1, length(x), 2)) {
    x_lines1 <- c(x_lines1, x[i], x[i + 1], NA)
    y_lines1 <- c(y_lines1, y[i], y[i + 1], NA)
    z_lines1 <- c(z_lines1, z[i], z[i + 1], NA)
  }
  
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = "lines",
    x = ~x_lines1,
    y = ~y_lines1,
    z = ~z_lines1,
    line = list(color = border_color, width = 10)
  )
  
  return(fig)  
}
plot_bordered_box_with_grid()


# 3. Get filled box with borders and grid
bg_metadata <- spe_metadata_background_template("random")
bg_metadata$background$n_cells <- 625
bg_metadata$background$length <- 500
bg_metadata$background$width <- 500
bg_metadata$background$height <- 500
bg_metadata$background$minimum_distance_between_cells <- 0
bg_metadata$background$cell_types <- 'O'
bg_metadata$background$cell_proportions <- 1

spe <- simulate_spe_metadata3D(bg_metadata, plot_image = F)

plot_cells_with_grid3D <- function(spe,
                                   plot_cell_types = NULL,
                                   plot_colours = NULL,
                                   grid_color,
                                   border_color = "#01478c",
                                   feature_colname = "Cell.Type") {
  
  # Check input parameters
  if (class(spe) != "SpatialExperiment") {
    stop("`spe` is not a SpatialExperiment object.")
  }
  if (!is.null(plot_cell_types) && !is.character(plot_cell_types)) {
    stop("`plot_cell_types` is not a character vector or NULL.")
  } 
  if (!is.null(plot_colours) && !is.character(plot_colours)) {
    stop("`plot_colours` is not a character vector or NULL.")
  } 
  if (is.character(plot_colours)) {
    non_colours <- plot_colours[which(!(sapply(plot_colours, function(X) {
      tryCatch(is.matrix(col2rgb(X)), 
               error = function(e) FALSE)
    })))]
    if (length(non_colours) > 0) {
      stop(paste("The following plot_colours are not colours:\n   ",
                 paste(non_colours, collapse = ", ")))
    } 
  }
  if (!is.character(feature_colname)) {
    stop("`feature_colname` is not a character.")
  }
  if (is.null(spe[[feature_colname]])) {
    stop(paste(feature_colname, "is not a valid column in your spe object."))
  }
  
  ## Convert spe object to data frame
  df <- data.frame(spatialCoords(spe), "Cell.Type" = spe[[feature_colname]])
  
  ## If no cell types chosen, use all cell types found in data frame
  if (is.null(plot_cell_types)) {
    warning("plot_cell_types not specified, all cell types found in the spe object will be used.")
    plot_cell_types <- unique(df[["Cell.Type"]])
  }
  ## If no colours inputted, use rainbow palette
  if (is.null(plot_colours)) {
    warning("plot_colours not specified, rainbow palette will be used.")
    plot_colours <- rainbow(length(plot_cell_types))
  }
  ## User inputs mismatching cell types and colours
  if (length(plot_cell_types) != length(plot_colours)) {
    stop("Length of plot_cell_types is not equal to length of plot_colours")
  }
  
  ## If cell types have been chosen, check they are found in the spe object
  spe_cell_types <- unique(spe[[feature_colname]])
  unknown_cell_types <- setdiff(plot_cell_types, spe_cell_types)
  
  if (length(unknown_cell_types) == length(plot_cell_types)) {
    stop("None of the plot_cell_types are found in the spe object")
  }
  
  if (length(unknown_cell_types) != 0) {
    warning(paste("The following plot_cell_types are not found in the spe object:\n   ",
                  paste(unknown_cell_types, collapse = ", ")))
    plot_colours <- plot_colours[which(plot_cell_types %in% spe_cell_types)]
    plot_cell_types <- intersect(plot_cell_types, spe_cell_types)
  }
  
  ## Factor for feature column
  df[, "Cell.Type"] <- factor(df[, "Cell.Type"],
                              levels = plot_cell_types)
  
  ## Plot
  fig <- add_trace(plot_ly(),
                   data = df,
                   type = "scatter3d",
                   mode = "markers",
                   x = ~Cell.X.Position,
                   y = ~Cell.Y.Position,
                   z = ~Cell.Z.Position,
                   color = ~Cell.Type,
                   colors = plot_colours,
                   marker = list(size = 4))
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     aspectmode = "cube"))
  
  
  ### ADD GRID LINES
  
  x <- c(rep(c(rep(c(0, 500), 6), rep(seq(0, 500, 100), each = 2)), 6), rep(c(rep(seq(0, 500, 100), each = 2), rep(c(0, 500), 6)), 6), rep(seq(0, 500, 100), each = 24))
  y <- c(rep(c(rep(seq(0, 500, 100), each = 2), rep(c(0, 500), 6)), 6), rep(seq(0, 500, 100), each = 24), rep(c(rep(c(0, 500), 6), rep(seq(0, 500, 100), each = 2)), 6))
  z <- c(rep(seq(0, 500, 100), each = 24), rep(c(rep(c(0, 500), 6), rep(seq(0, 500, 100), each = 2)), 6), rep(c(rep(seq(0, 500, 100), each = 2), rep(c(0, 500), 6)), 6))
  
  # Define pairs of points to connect
  pairs <- list(c(1, 2))  # Connect point 1 to 2
  
  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (i in seq(1, length(x), 2)) {
    x_lines <- c(x_lines, x[i], x[i + 1], NA)
    y_lines <- c(y_lines, y[i], y[i + 1], NA)
    z_lines <- c(z_lines, z[i], z[i + 1], NA)
  }
  
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = "lines",
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = grid_color, width = 3)
  )
  
  ### Add border lines
  x <- c(rep(c(0, 500), 2), rep(c(0, 500), each = 2), rep(c(0, 500), each = 4), rep(c(0, 500), 2), rep(c(0, 500), each = 2))
  y <- c(rep(c(0, 500), each = 2), rep(c(0, 500), 2), rep(rep(c(0, 500), each = 2), 2), rep(c(0, 500), each = 2), rep(c(0, 500), 2))
  z <- c(rep(0, 8), rep(c(0, 500), 4), rep(500, 8))
  
  # Create coordinate vectors with NA between segments
  x_lines1 <- y_lines1 <- z_lines1 <- c()
  for (i in seq(1, length(x), 2)) {
    x_lines1 <- c(x_lines1, x[i], x[i + 1], NA)
    y_lines1 <- c(y_lines1, y[i], y[i + 1], NA)
    z_lines1 <- c(z_lines1, z[i], z[i + 1], NA)
  }
  
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = "lines",
    x = ~x_lines1,
    y = ~y_lines1,
    z = ~z_lines1,
    line = list(color = border_color, width = 10)
  )
  
  return(fig)
}

plot_cells_with_grid3D(spe, 
                       plot_cell_types = c('O'), 
                       plot_colours = c('black'), 
                       grid_color = "#007128")


### Background (random and mixed) ----
plot_cells3D <- function(spe,
                         plot_cell_types = NULL,
                         plot_colours = NULL,
                         feature_colname = "Cell.Type") {
  
  # Check input parameters
  if (class(spe) != "SpatialExperiment") {
    stop("`spe` is not a SpatialExperiment object.")
  }
  if (!is.null(plot_cell_types) && !is.character(plot_cell_types)) {
    stop("`plot_cell_types` is not a character vector or NULL.")
  } 
  if (!is.null(plot_colours) && !is.character(plot_colours)) {
    stop("`plot_colours` is not a character vector or NULL.")
  } 
  if (is.character(plot_colours)) {
    non_colours <- plot_colours[which(!(sapply(plot_colours, function(X) {
      tryCatch(is.matrix(col2rgb(X)), 
               error = function(e) FALSE)
    })))]
    if (length(non_colours) > 0) {
      stop(paste("The following plot_colours are not colours:\n   ",
                 paste(non_colours, collapse = ", ")))
    } 
  }
  if (!is.character(feature_colname)) {
    stop("`feature_colname` is not a character.")
  }
  if (is.null(spe[[feature_colname]])) {
    stop(paste(feature_colname, "is not a valid column in your spe object."))
  }
  
  ## Convert spe object to data frame
  df <- data.frame(spatialCoords(spe), "Cell.Type" = spe[[feature_colname]])
  
  ## If no cell types chosen, use all cell types found in data frame
  if (is.null(plot_cell_types)) {
    warning("plot_cell_types not specified, all cell types found in the spe object will be used.")
    plot_cell_types <- unique(df[["Cell.Type"]])
  }
  ## If no colours inputted, use rainbow palette
  if (is.null(plot_colours)) {
    warning("plot_colours not specified, rainbow palette will be used.")
    plot_colours <- rainbow(length(plot_cell_types))
  }
  ## User inputs mismatching cell types and colours
  if (length(plot_cell_types) != length(plot_colours)) {
    stop("Length of plot_cell_types is not equal to length of plot_colours")
  }
  
  ## If cell types have been chosen, check they are found in the spe object
  spe_cell_types <- unique(spe[[feature_colname]])
  unknown_cell_types <- setdiff(plot_cell_types, spe_cell_types)
  
  if (length(unknown_cell_types) == length(plot_cell_types)) {
    stop("None of the plot_cell_types are found in the spe object")
  }
  
  if (length(unknown_cell_types) != 0) {
    warning(paste("The following plot_cell_types are not found in the spe object:\n   ",
                  paste(unknown_cell_types, collapse = ", ")))
    plot_colours <- plot_colours[which(plot_cell_types %in% spe_cell_types)]
    plot_cell_types <- intersect(plot_cell_types, spe_cell_types)
  }
  
  ## Factor for feature column
  df[, "Cell.Type"] <- factor(df[, "Cell.Type"],
                              levels = plot_cell_types)
  
  ## Plot
  fig <- plot_ly(df,
                 type = "scatter3d",
                 mode = 'markers',
                 x = ~Cell.X.Position,
                 y = ~Cell.Y.Position,
                 z = ~Cell.Z.Position,
                 color = ~Cell.Type,
                 colors = plot_colours,
                 marker = list(size = 2))
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = T, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     aspectmode = "data"))
  
  return(fig)
}

bg_metadata <- spe_metadata_background_template("random")
bg_metadata$background$n_cells <- 5000
bg_metadata$background$length <- 500
bg_metadata$background$width <- 500
bg_metadata$background$height <- 500
bg_metadata$background$minimum_distance_between_cells <- 10
bg_metadata$background$cell_types <- 'O'
bg_metadata$background$cell_proportions <- 1

# Random
spe <- simulate_spe_metadata3D(bg_metadata, 
                               plot_image = T, 
                               plot_cell_types = 'O', 
                               plot_colours = 'lightgray')

# Mixed
bg_metadata$background$cell_types <- c('O', 'A', 'B')
bg_metadata$background$cell_proportions <- c(0.8, 0.1, 0.1)

spe <- simulate_spe_metadata3D(bg_metadata, 
                               plot_image = T, 
                               plot_cell_types = c('O', 'A', 'B'), 
                               plot_colours = c('lightgray', '#f77e3b', '#48bbff'))


### Sphere ----
plot_cells3D <- function(spe,
                         plot_cell_types = NULL,
                         plot_colours = NULL,
                         feature_colname = "Cell.Type") {
  
  # Check input parameters
  if (class(spe) != "SpatialExperiment") {
    stop("`spe` is not a SpatialExperiment object.")
  }
  if (!is.null(plot_cell_types) && !is.character(plot_cell_types)) {
    stop("`plot_cell_types` is not a character vector or NULL.")
  } 
  if (!is.null(plot_colours) && !is.character(plot_colours)) {
    stop("`plot_colours` is not a character vector or NULL.")
  } 
  if (is.character(plot_colours)) {
    non_colours <- plot_colours[which(!(sapply(plot_colours, function(X) {
      tryCatch(is.matrix(col2rgb(X)), 
               error = function(e) FALSE)
    })))]
    if (length(non_colours) > 0) {
      stop(paste("The following plot_colours are not colours:\n   ",
                 paste(non_colours, collapse = ", ")))
    } 
  }
  if (!is.character(feature_colname)) {
    stop("`feature_colname` is not a character.")
  }
  if (is.null(spe[[feature_colname]])) {
    stop(paste(feature_colname, "is not a valid column in your spe object."))
  }
  
  ## Convert spe object to data frame
  df <- data.frame(spatialCoords(spe), "Cell.Type" = spe[[feature_colname]])
  
  ## If no cell types chosen, use all cell types found in data frame
  if (is.null(plot_cell_types)) {
    warning("plot_cell_types not specified, all cell types found in the spe object will be used.")
    plot_cell_types <- unique(df[["Cell.Type"]])
  }
  ## If no colours inputted, use rainbow palette
  if (is.null(plot_colours)) {
    warning("plot_colours not specified, rainbow palette will be used.")
    plot_colours <- rainbow(length(plot_cell_types))
  }
  ## User inputs mismatching cell types and colours
  if (length(plot_cell_types) != length(plot_colours)) {
    stop("Length of plot_cell_types is not equal to length of plot_colours")
  }
  
  ## If cell types have been chosen, check they are found in the spe object
  spe_cell_types <- unique(spe[[feature_colname]])
  unknown_cell_types <- setdiff(plot_cell_types, spe_cell_types)
  
  if (length(unknown_cell_types) == length(plot_cell_types)) {
    stop("None of the plot_cell_types are found in the spe object")
  }
  
  if (length(unknown_cell_types) != 0) {
    warning(paste("The following plot_cell_types are not found in the spe object:\n   ",
                  paste(unknown_cell_types, collapse = ", ")))
    plot_colours <- plot_colours[which(plot_cell_types %in% spe_cell_types)]
    plot_cell_types <- intersect(plot_cell_types, spe_cell_types)
  }
  
  ## Factor for feature column
  df[, "Cell.Type"] <- factor(df[, "Cell.Type"],
                              levels = plot_cell_types)
  
  ## Plot
  fig <- plot_ly(df,
                 type = "scatter3d",
                 mode = 'markers',
                 x = ~Cell.X.Position,
                 y = ~Cell.Y.Position,
                 z = ~Cell.Z.Position,
                 color = ~Cell.Type,
                 colors = plot_colours,
                 marker = list(size = 2))
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = T, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     aspectmode = "data"))
  
  return(fig)
}

bg_metadata <- spe_metadata_background_template("random")
bg_metadata$background$n_cells <- 5000
bg_metadata$background$length <- 500
bg_metadata$background$width <- 500
bg_metadata$background$height <- 500
bg_metadata$background$minimum_distance_between_cells <- 10
bg_metadata$background$cell_types <- 'O'
bg_metadata$background$cell_proportions <- 1

cluster_metadata <- spe_metadata_cluster_template("regular", "sphere", bg_metadata)
cluster_metadata$cluster_1$cluster_cell_types <- "A"
cluster_metadata$cluster_1$cluster_cell_proportions <- 1
cluster_metadata$cluster_1$radius <- 200
cluster_metadata$cluster_1$centre_loc <- c(250, 250, 250)

spe <- simulate_spe_metadata3D(cluster_metadata, 
                               plot_image = T, 
                               plot_cell_types = c('O', 'A', 'B'), 
                               plot_colours = c('lightgray', '#f77e3b', '#48bbff'))


### Ellipsoid transformations ----

### Ellipsoid ----
plot_cells3D <- function(spe,
                         plot_cell_types = NULL,
                         plot_colours = NULL,
                         feature_colname = "Cell.Type") {
  
  # Check input parameters
  if (class(spe) != "SpatialExperiment") {
    stop("`spe` is not a SpatialExperiment object.")
  }
  if (!is.null(plot_cell_types) && !is.character(plot_cell_types)) {
    stop("`plot_cell_types` is not a character vector or NULL.")
  } 
  if (!is.null(plot_colours) && !is.character(plot_colours)) {
    stop("`plot_colours` is not a character vector or NULL.")
  } 
  if (is.character(plot_colours)) {
    non_colours <- plot_colours[which(!(sapply(plot_colours, function(X) {
      tryCatch(is.matrix(col2rgb(X)), 
               error = function(e) FALSE)
    })))]
    if (length(non_colours) > 0) {
      stop(paste("The following plot_colours are not colours:\n   ",
                 paste(non_colours, collapse = ", ")))
    } 
  }
  if (!is.character(feature_colname)) {
    stop("`feature_colname` is not a character.")
  }
  if (is.null(spe[[feature_colname]])) {
    stop(paste(feature_colname, "is not a valid column in your spe object."))
  }
  
  ## Convert spe object to data frame
  df <- data.frame(spatialCoords(spe), "Cell.Type" = spe[[feature_colname]])
  
  ## If no cell types chosen, use all cell types found in data frame
  if (is.null(plot_cell_types)) {
    warning("plot_cell_types not specified, all cell types found in the spe object will be used.")
    plot_cell_types <- unique(df[["Cell.Type"]])
  }
  ## If no colours inputted, use rainbow palette
  if (is.null(plot_colours)) {
    warning("plot_colours not specified, rainbow palette will be used.")
    plot_colours <- rainbow(length(plot_cell_types))
  }
  ## User inputs mismatching cell types and colours
  if (length(plot_cell_types) != length(plot_colours)) {
    stop("Length of plot_cell_types is not equal to length of plot_colours")
  }
  
  ## If cell types have been chosen, check they are found in the spe object
  spe_cell_types <- unique(spe[[feature_colname]])
  unknown_cell_types <- setdiff(plot_cell_types, spe_cell_types)
  
  if (length(unknown_cell_types) == length(plot_cell_types)) {
    stop("None of the plot_cell_types are found in the spe object")
  }
  
  if (length(unknown_cell_types) != 0) {
    warning(paste("The following plot_cell_types are not found in the spe object:\n   ",
                  paste(unknown_cell_types, collapse = ", ")))
    plot_colours <- plot_colours[which(plot_cell_types %in% spe_cell_types)]
    plot_cell_types <- intersect(plot_cell_types, spe_cell_types)
  }
  
  ## Factor for feature column
  df[, "Cell.Type"] <- factor(df[, "Cell.Type"],
                              levels = plot_cell_types)
  
  ## Plot
  fig <- plot_ly(df,
                 type = "scatter3d",
                 mode = 'markers',
                 x = ~Cell.X.Position,
                 y = ~Cell.Y.Position,
                 z = ~Cell.Z.Position,
                 color = ~Cell.Type,
                 colors = plot_colours,
                 marker = list(size = 2))
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = T, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     aspectmode = "data"))
  
  return(fig)
}

bg_metadata <- spe_metadata_background_template("random")
bg_metadata$background$n_cells <- 5000
bg_metadata$background$length <- 500
bg_metadata$background$width <- 500
bg_metadata$background$height <- 500
bg_metadata$background$minimum_distance_between_cells <- 10
bg_metadata$background$cell_types <- 'O'
bg_metadata$background$cell_proportions <- 1

cluster_metadata <- spe_metadata_cluster_template("regular", "ellipsoid", bg_metadata)
cluster_metadata$cluster_1$cluster_cell_types <- "A"
cluster_metadata$cluster_1$cluster_cell_proportions <- 1
cluster_metadata$cluster_1$radii <- c(150, 150, 300)
cluster_metadata$cluster_1$centre_loc <- c(250, 250, 250)
cluster_metadata$cluster_1$axes_rotation <- c(0, 60, 0)

spe <- simulate_spe_metadata3D(cluster_metadata, 
                               plot_image = T, 
                               plot_cell_types = c('O', 'A', 'B'), 
                               plot_colours = c('lightgray', '#f77e3b', '#48bbff'))


### Cylinder justification ----

### Cylinder ----
plot_cells3D <- function(spe,
                         plot_cell_types = NULL,
                         plot_colours = NULL,
                         feature_colname = "Cell.Type") {
  
  # Check input parameters
  if (class(spe) != "SpatialExperiment") {
    stop("`spe` is not a SpatialExperiment object.")
  }
  if (!is.null(plot_cell_types) && !is.character(plot_cell_types)) {
    stop("`plot_cell_types` is not a character vector or NULL.")
  } 
  if (!is.null(plot_colours) && !is.character(plot_colours)) {
    stop("`plot_colours` is not a character vector or NULL.")
  } 
  if (is.character(plot_colours)) {
    non_colours <- plot_colours[which(!(sapply(plot_colours, function(X) {
      tryCatch(is.matrix(col2rgb(X)), 
               error = function(e) FALSE)
    })))]
    if (length(non_colours) > 0) {
      stop(paste("The following plot_colours are not colours:\n   ",
                 paste(non_colours, collapse = ", ")))
    } 
  }
  if (!is.character(feature_colname)) {
    stop("`feature_colname` is not a character.")
  }
  if (is.null(spe[[feature_colname]])) {
    stop(paste(feature_colname, "is not a valid column in your spe object."))
  }
  
  ## Convert spe object to data frame
  df <- data.frame(spatialCoords(spe), "Cell.Type" = spe[[feature_colname]])
  
  ## If no cell types chosen, use all cell types found in data frame
  if (is.null(plot_cell_types)) {
    warning("plot_cell_types not specified, all cell types found in the spe object will be used.")
    plot_cell_types <- unique(df[["Cell.Type"]])
  }
  ## If no colours inputted, use rainbow palette
  if (is.null(plot_colours)) {
    warning("plot_colours not specified, rainbow palette will be used.")
    plot_colours <- rainbow(length(plot_cell_types))
  }
  ## User inputs mismatching cell types and colours
  if (length(plot_cell_types) != length(plot_colours)) {
    stop("Length of plot_cell_types is not equal to length of plot_colours")
  }
  
  ## If cell types have been chosen, check they are found in the spe object
  spe_cell_types <- unique(spe[[feature_colname]])
  unknown_cell_types <- setdiff(plot_cell_types, spe_cell_types)
  
  if (length(unknown_cell_types) == length(plot_cell_types)) {
    stop("None of the plot_cell_types are found in the spe object")
  }
  
  if (length(unknown_cell_types) != 0) {
    warning(paste("The following plot_cell_types are not found in the spe object:\n   ",
                  paste(unknown_cell_types, collapse = ", ")))
    plot_colours <- plot_colours[which(plot_cell_types %in% spe_cell_types)]
    plot_cell_types <- intersect(plot_cell_types, spe_cell_types)
  }
  
  ## Factor for feature column
  df[, "Cell.Type"] <- factor(df[, "Cell.Type"],
                              levels = plot_cell_types)
  
  ## Plot
  fig <- plot_ly(df,
                 type = "scatter3d",
                 mode = 'markers',
                 x = ~Cell.X.Position,
                 y = ~Cell.Y.Position,
                 z = ~Cell.Z.Position,
                 color = ~Cell.Type,
                 colors = plot_colours,
                 marker = list(size = 2))
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = T, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     aspectmode = "data"))
  
  return(fig)
}

bg_metadata <- spe_metadata_background_template("random")
bg_metadata$background$n_cells <- 5000
bg_metadata$background$length <- 500
bg_metadata$background$width <- 500
bg_metadata$background$height <- 500
bg_metadata$background$minimum_distance_between_cells <- 10
bg_metadata$background$cell_types <- 'O'
bg_metadata$background$cell_proportions <- 1

cluster_metadata <- spe_metadata_cluster_template("regular", "cylinder", bg_metadata)
cluster_metadata$cluster_1$cluster_cell_types <- "C"
cluster_metadata$cluster_1$cluster_cell_proportions <- 1
cluster_metadata$cluster_1$radius <- 50
cluster_metadata$cluster_1$start_loc <- c(500, 0, 0)
cluster_metadata$cluster_1$end_loc <- c(0, 500, 500)

spe <- simulate_spe_metadata3D(cluster_metadata, 
                               plot_image = T, 
                               plot_cell_types = c('O', 'A', 'B', 'C'), 
                               plot_colours = c('lightgray', '#f77e3b', '#48bbff', '#bb0036'))

### Prim's algorithm ----

### Network ----
plot_cells3D <- function(spe,
                         plot_cell_types = NULL,
                         plot_colours = NULL,
                         feature_colname = "Cell.Type") {
  
  # Check input parameters
  if (class(spe) != "SpatialExperiment") {
    stop("`spe` is not a SpatialExperiment object.")
  }
  if (!is.null(plot_cell_types) && !is.character(plot_cell_types)) {
    stop("`plot_cell_types` is not a character vector or NULL.")
  } 
  if (!is.null(plot_colours) && !is.character(plot_colours)) {
    stop("`plot_colours` is not a character vector or NULL.")
  } 
  if (is.character(plot_colours)) {
    non_colours <- plot_colours[which(!(sapply(plot_colours, function(X) {
      tryCatch(is.matrix(col2rgb(X)), 
               error = function(e) FALSE)
    })))]
    if (length(non_colours) > 0) {
      stop(paste("The following plot_colours are not colours:\n   ",
                 paste(non_colours, collapse = ", ")))
    } 
  }
  if (!is.character(feature_colname)) {
    stop("`feature_colname` is not a character.")
  }
  if (is.null(spe[[feature_colname]])) {
    stop(paste(feature_colname, "is not a valid column in your spe object."))
  }
  
  ## Convert spe object to data frame
  df <- data.frame(spatialCoords(spe), "Cell.Type" = spe[[feature_colname]])
  
  ## If no cell types chosen, use all cell types found in data frame
  if (is.null(plot_cell_types)) {
    warning("plot_cell_types not specified, all cell types found in the spe object will be used.")
    plot_cell_types <- unique(df[["Cell.Type"]])
  }
  ## If no colours inputted, use rainbow palette
  if (is.null(plot_colours)) {
    warning("plot_colours not specified, rainbow palette will be used.")
    plot_colours <- rainbow(length(plot_cell_types))
  }
  ## User inputs mismatching cell types and colours
  if (length(plot_cell_types) != length(plot_colours)) {
    stop("Length of plot_cell_types is not equal to length of plot_colours")
  }
  
  ## If cell types have been chosen, check they are found in the spe object
  spe_cell_types <- unique(spe[[feature_colname]])
  unknown_cell_types <- setdiff(plot_cell_types, spe_cell_types)
  
  if (length(unknown_cell_types) == length(plot_cell_types)) {
    stop("None of the plot_cell_types are found in the spe object")
  }
  
  if (length(unknown_cell_types) != 0) {
    warning(paste("The following plot_cell_types are not found in the spe object:\n   ",
                  paste(unknown_cell_types, collapse = ", ")))
    plot_colours <- plot_colours[which(plot_cell_types %in% spe_cell_types)]
    plot_cell_types <- intersect(plot_cell_types, spe_cell_types)
  }
  
  ## Factor for feature column
  df[, "Cell.Type"] <- factor(df[, "Cell.Type"],
                              levels = plot_cell_types)
  
  ## Plot
  fig <- plot_ly(df,
                 type = "scatter3d",
                 mode = 'markers',
                 x = ~Cell.X.Position,
                 y = ~Cell.Y.Position,
                 z = ~Cell.Z.Position,
                 color = ~Cell.Type,
                 colors = plot_colours,
                 marker = list(size = 2))
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = T, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     aspectmode = "data"))
  
  return(fig)
}

bg_metadata <- spe_metadata_background_template("random")
bg_metadata$background$n_cells <- 5000
bg_metadata$background$length <- 500
bg_metadata$background$width <- 500
bg_metadata$background$height <- 500
bg_metadata$background$minimum_distance_between_cells <- 10
bg_metadata$background$cell_types <- 'O'
bg_metadata$background$cell_proportions <- 1

cluster_metadata <- spe_metadata_cluster_template("regular", "network", bg_metadata)
cluster_metadata$cluster_1$cluster_cell_types <- "B"
cluster_metadata$cluster_1$cluster_cell_proportions <- 1
cluster_metadata$cluster_1$centre_loc <- c(250, 250, 250)
cluster_metadata$cluster_1$radius <- 300
cluster_metadata$cluster_1$width <- 50
cluster_metadata$cluster_1$n_edges <- 50

spe <- simulate_spe_metadata3D(cluster_metadata, 
                               plot_image = T, 
                               plot_cell_types = c('O', 'A', 'B', 'C'), 
                               plot_colours = c('lightgray', '#f77e3b', '#48bbff', '#bb0036'))
### Mixed, ringed and double ringed clusters ----
plot_cells3D <- function(spe,
                         plot_cell_types = NULL,
                         plot_colours = NULL,
                         feature_colname = "Cell.Type") {
  
  # Check input parameters
  if (class(spe) != "SpatialExperiment") {
    stop("`spe` is not a SpatialExperiment object.")
  }
  if (!is.null(plot_cell_types) && !is.character(plot_cell_types)) {
    stop("`plot_cell_types` is not a character vector or NULL.")
  } 
  if (!is.null(plot_colours) && !is.character(plot_colours)) {
    stop("`plot_colours` is not a character vector or NULL.")
  } 
  if (is.character(plot_colours)) {
    non_colours <- plot_colours[which(!(sapply(plot_colours, function(X) {
      tryCatch(is.matrix(col2rgb(X)), 
               error = function(e) FALSE)
    })))]
    if (length(non_colours) > 0) {
      stop(paste("The following plot_colours are not colours:\n   ",
                 paste(non_colours, collapse = ", ")))
    } 
  }
  if (!is.character(feature_colname)) {
    stop("`feature_colname` is not a character.")
  }
  if (is.null(spe[[feature_colname]])) {
    stop(paste(feature_colname, "is not a valid column in your spe object."))
  }
  
  ## Convert spe object to data frame
  df <- data.frame(spatialCoords(spe), "Cell.Type" = spe[[feature_colname]])
  
  ## If no cell types chosen, use all cell types found in data frame
  if (is.null(plot_cell_types)) {
    warning("plot_cell_types not specified, all cell types found in the spe object will be used.")
    plot_cell_types <- unique(df[["Cell.Type"]])
  }
  ## If no colours inputted, use rainbow palette
  if (is.null(plot_colours)) {
    warning("plot_colours not specified, rainbow palette will be used.")
    plot_colours <- rainbow(length(plot_cell_types))
  }
  ## User inputs mismatching cell types and colours
  if (length(plot_cell_types) != length(plot_colours)) {
    stop("Length of plot_cell_types is not equal to length of plot_colours")
  }
  
  ## If cell types have been chosen, check they are found in the spe object
  spe_cell_types <- unique(spe[[feature_colname]])
  unknown_cell_types <- setdiff(plot_cell_types, spe_cell_types)
  
  if (length(unknown_cell_types) == length(plot_cell_types)) {
    stop("None of the plot_cell_types are found in the spe object")
  }
  
  if (length(unknown_cell_types) != 0) {
    warning(paste("The following plot_cell_types are not found in the spe object:\n   ",
                  paste(unknown_cell_types, collapse = ", ")))
    plot_colours <- plot_colours[which(plot_cell_types %in% spe_cell_types)]
    plot_cell_types <- intersect(plot_cell_types, spe_cell_types)
  }
  
  ## Factor for feature column
  df[, "Cell.Type"] <- factor(df[, "Cell.Type"],
                              levels = plot_cell_types)
  
  ## Plot
  fig <- plot_ly(df,
                 type = "scatter3d",
                 mode = 'markers',
                 x = ~Cell.X.Position,
                 y = ~Cell.Y.Position,
                 z = ~Cell.Z.Position,
                 color = ~Cell.Type,
                 colors = plot_colours,
                 marker = list(size = 2))
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = T, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     aspectmode = "data"))
  
  return(fig)
}

bg_metadata <- spe_metadata_background_template("random")
bg_metadata$background$n_cells <- 5000
bg_metadata$background$length <- 500
bg_metadata$background$width <- 500
bg_metadata$background$height <- 500
bg_metadata$background$minimum_distance_between_cells <- 10
bg_metadata$background$cell_types <- 'O'
bg_metadata$background$cell_proportions <- 1


mixed_metadata <- spe_metadata_cluster_template("regular", "sphere", bg_metadata)
mixed_metadata$cluster_1$cluster_cell_types <- c('A', 'B')
mixed_metadata$cluster_1$cluster_cell_proportions <- c(0.5, 0.5)
mixed_metadata$cluster_1$radius <- 200
mixed_metadata$cluster_1$centre_loc <- c(250, 250, 250)
mixed_metadata$cluster_1$centre_loc <- c(250, 250, 250)

ringed_metadata <- spe_metadata_cluster_template("ring", "sphere", bg_metadata)
ringed_metadata$cluster_1$cluster_cell_types <- 'A'
ringed_metadata$cluster_1$cluster_cell_proportions <- 1
ringed_metadata$cluster_1$radius <- 200
ringed_metadata$cluster_1$centre_loc <- c(250, 250, 250)
ringed_metadata$cluster_1$centre_loc <- c(250, 250, 250)
ringed_metadata$cluster_1$ring_cell_types <- 'B'
ringed_metadata$cluster_1$ring_cell_proportions <- 1
ringed_metadata$cluster_1$ring_width <- 20

double_ringed_metadata <- spe_metadata_cluster_template("double ring", "sphere", bg_metadata)
double_ringed_metadata$cluster_1$cluster_cell_types <- 'A'
double_ringed_metadata$cluster_1$cluster_cell_proportions <- 1
double_ringed_metadata$cluster_1$radius <- 200
double_ringed_metadata$cluster_1$centre_loc <- c(250, 250, 250)
double_ringed_metadata$cluster_1$centre_loc <- c(250, 250, 250)
double_ringed_metadata$cluster_1$inner_ring_cell_types <- 'B'
double_ringed_metadata$cluster_1$inner_ring_cell_proportions <- 1
double_ringed_metadata$cluster_1$inner_ring_width <- 20
double_ringed_metadata$cluster_1$outer_ring_cell_types <- 'D'
double_ringed_metadata$cluster_1$outer_ring_cell_proportions <- 1
double_ringed_metadata$cluster_1$outer_ring_width <- 20


spe <- simulate_spe_metadata3D(mixed_metadata, 
                               plot_image = T, 
                               plot_cell_types = c('O', 'A', 'B'), 
                               plot_colours = c('lightgray', '#f77e3b', '#48bbff'))

spe <- simulate_spe_metadata3D(ringed_metadata, 
                               plot_image = T, 
                               plot_cell_types = c('O', 'A', 'B'), 
                               plot_colours = c('lightgray', '#f77e3b', '#48bbff'))

spe <- simulate_spe_metadata3D(double_ringed_metadata, 
                               plot_image = T, 
                               plot_cell_types = c('O', 'A', 'B', 'D'), 
                               plot_colours = c('lightgray', '#f77e3b', '#48bbff', '#9437a8'))
