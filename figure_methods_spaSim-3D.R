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
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
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
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
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
# 1. unit sphere
plot_unit_sphere <- function(sphere_color = "#0062c5",
                             sphere_axis_color = "#bb0036") {
  
  
  # Get random coords for a unit sphere
  x <- runif(20000, -1, 1)
  y <- runif(20000, -1, 1)
  z <- runif(20000, -1, 1)
  
  coords_to_keep <- x^2 + y^2 + z^2 < 1
  x <- x[coords_to_keep]
  y <- y[coords_to_keep]
  z <- z[coords_to_keep]
  
  # Get axis lines
  x_line_coords <- c(0, 4, 0, 0, 0, 0)
  y_line_coords <- c(0, 0, 0, 4, 0, 0)
  z_line_coords <- c(0, 0, 0, 0, 0, 4)
  
  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (i in seq(1, length(x), 2)) {
    x_lines <- c(x_lines, x_line_coords[i], x_line_coords[i + 1], NA)
    y_lines <- c(y_lines, y_line_coords[i], y_line_coords[i + 1], NA)
    z_lines <- c(z_lines, z_line_coords[i], z_line_coords[i + 1], NA)
  }
  
  fig <- add_trace(plot_ly(),
                   type = "scatter3d",
                   mode = "markers",
                   x = ~x,
                   y = ~y,
                   z = ~z,
                   marker = list(color = sphere_color, size = 1, opacity = 0.1))
  
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = "lines",
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = sphere_axis_color, width = 5)
  )
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = T, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  return(fig)
}

plot_unit_sphere()


# 2. dilated unit sphere
plot_dilated_sphere <- function(sphere_color = "#0062c5",
                                sphere_axis_color = "#bb0036") {
  
  # Get random coords for a unit sphere
  x <- runif(20000, -1, 1)
  y <- runif(20000, -1, 1)
  z <- runif(20000, -1, 1)
  
  coords_to_keep <- x^2 + y^2 + z^2 < 1
  x <- x[coords_to_keep]
  y <- y[coords_to_keep]
  z <- z[coords_to_keep]
  
  # Dilate
  x <- x * 1.5
  y <- y * 1.5
  z <- z * 3
  
  # Get axis lines
  x_line_coords <- c(0, 4, 0, 0, 0, 0)
  y_line_coords <- c(0, 0, 0, 4, 0, 0)
  z_line_coords <- c(0, 0, 0, 0, 0, 4)
  
  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (i in seq(1, length(x), 2)) {
    x_lines <- c(x_lines, x_line_coords[i], x_line_coords[i + 1], NA)
    y_lines <- c(y_lines, y_line_coords[i], y_line_coords[i + 1], NA)
    z_lines <- c(z_lines, z_line_coords[i], z_line_coords[i + 1], NA)
  }
  
  fig <- add_trace(plot_ly(),
                   type = "scatter3d",
                   mode = "markers",
                   x = ~x,
                   y = ~y,
                   z = ~z,
                   marker = list(color = sphere_color, size = 1, opacity = 0.3))
  
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = "lines",
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = sphere_axis_color, width = 5)
  )
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = T, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  return(fig)
}

plot_dilated_sphere()

# 3. dilated and rotated unit sphere
plot_dilated_and_rotated_sphere <- function(sphere_color = "#0062c5",
                                            sphere_axis_color = "#bb0036") {
  
  
  # Get random coords for a unit sphere
  x <- runif(20000, -1, 1)
  y <- runif(20000, -1, 1)
  z <- runif(20000, -1, 1)
  
  coords_to_keep <- x^2 + y^2 + z^2 < 1
  x <- x[coords_to_keep]
  y <- y[coords_to_keep]
  z <- z[coords_to_keep]
  
  # Dilate
  x <- x * 1.5
  y <- y * 1.5
  z <- z * 3
  
  # Rotate
  theta <- -pi/8 # rotation in x-axis
  alpha <- 0 # rotation in y-axis
  beta  <- 0 # rotation in z-axis
  
  # Get rotation matrices for rotation in the y-z plane (T2), x-z plane (T3) and x-y plane (T4)
  T1 <- matrix(data = c(1, 0, 0,
                        0, cos(theta), -sin(theta),
                        0, sin(theta), cos(theta)), nrow = 3, ncol = 3, byrow = TRUE)
  T2 <- matrix(data = c(cos(alpha), 0, -sin(alpha),
                        0, 1, 0,
                        sin(alpha), 0, cos(alpha)), nrow = 3, ncol = 3, byrow = TRUE)
  T3 <- matrix(data = c(cos(beta), -sin(beta), 0,
                        sin(beta), cos(beta), 0,
                        0, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)
  
  coords <- t(as.matrix(data.frame(x = x, y = y, z = z)))
  coords <- solve(T1) %*% solve(T2) %*% solve(T3) %*% (coords)
  
  x <- coords[1, ]
  y <- coords[2, ]
  z <- coords[3, ]
  
  # Get axis lines
  x_line_coords <- c(0, 4, 0, 0, 0, 0)
  y_line_coords <- c(0, 0, 0, 4, 0, 0)
  z_line_coords <- c(0, 0, 0, 0, 0, 4)
  
  line_coords <- t(as.matrix(data.frame(x = x_line_coords, y = y_line_coords, z = z_line_coords)))
  line_coords <- solve(T1) %*% solve(T2) %*% solve(T3) %*% (line_coords)
  
  x_line_coords <- line_coords[1, ]
  y_line_coords <- line_coords[2, ]
  z_line_coords <- line_coords[3, ]
  
  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (i in seq(1, length(x), 2)) {
    x_lines <- c(x_lines, x_line_coords[i], x_line_coords[i + 1], NA)
    y_lines <- c(y_lines, y_line_coords[i], y_line_coords[i + 1], NA)
    z_lines <- c(z_lines, z_line_coords[i], z_line_coords[i + 1], NA)
  }
  
  fig <- add_trace(plot_ly(),
                   type = "scatter3d",
                   mode = "markers",
                   x = ~x,
                   y = ~y,
                   z = ~z,
                   marker = list(color = sphere_color, size = 1, opacity = 0.1))
  
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = "lines",
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = sphere_axis_color, width = 5)
  )
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = T, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  return(fig)
}

plot_dilated_and_rotated_sphere()

# 4. dilated, rotated and translated unit sphere
plot_dilated_and_rotated_and_translated_sphere <- function(sphere_color = "#0062c5",
                                                           sphere_axis_color = "#bb0036") {
  
  
  # Get random coords for a unit sphere
  x <- runif(20000, -1, 1)
  y <- runif(20000, -1, 1)
  z <- runif(20000, -1, 1)
  
  coords_to_keep <- x^2 + y^2 + z^2 < 1
  x <- x[coords_to_keep]
  y <- y[coords_to_keep]
  z <- z[coords_to_keep]
  
  # Dilate
  x <- x * 1.5
  y <- y * 1.5
  z <- z * 3
  
  # Rotate
  theta <- -pi/8 # rotation in x-axis
  alpha <- 0 # rotation in y-axis
  beta  <- 0 # rotation in z-axis
  
  # Get rotation matrices for rotation in the y-z plane (T2), x-z plane (T3) and x-y plane (T4)
  T1 <- matrix(data = c(1, 0, 0,
                        0, cos(theta), -sin(theta),
                        0, sin(theta), cos(theta)), nrow = 3, ncol = 3, byrow = TRUE)
  T2 <- matrix(data = c(cos(alpha), 0, -sin(alpha),
                        0, 1, 0,
                        sin(alpha), 0, cos(alpha)), nrow = 3, ncol = 3, byrow = TRUE)
  T3 <- matrix(data = c(cos(beta), -sin(beta), 0,
                        sin(beta), cos(beta), 0,
                        0, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)
  
  T4 <- c(-2, 2, 2)
  
  coords <- t(as.matrix(data.frame(x = x, y = y, z = z)))
  coords <- solve(T1) %*% solve(T2) %*% solve(T3) %*% (coords - T4)
  
  x <- coords[1, ]
  y <- coords[2, ]
  z <- coords[3, ]
  
  # Get axis lines
  x_line_coords <- c(0, 4, 0, 0, 0, 0)
  y_line_coords <- c(0, 0, 0, 4, 0, 0)
  z_line_coords <- c(0, 0, 0, 0, 0, 4)
  
  line_coords <- t(as.matrix(data.frame(x = x_line_coords, y = y_line_coords, z = z_line_coords)))
  line_coords <- solve(T1) %*% solve(T2) %*% solve(T3) %*% (line_coords - T4)
  
  x_line_coords <- line_coords[1, ]
  y_line_coords <- line_coords[2, ]
  z_line_coords <- line_coords[3, ]
  
  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (i in seq(1, length(x), 2)) {
    x_lines <- c(x_lines, x_line_coords[i], x_line_coords[i + 1], NA)
    y_lines <- c(y_lines, y_line_coords[i], y_line_coords[i + 1], NA)
    z_lines <- c(z_lines, z_line_coords[i], z_line_coords[i + 1], NA)
  }
  
  fig <- add_trace(plot_ly(),
                   type = "scatter3d",
                   mode = "markers",
                   x = ~x,
                   y = ~y,
                   z = ~z,
                   marker = list(color = sphere_color, size = 1, opacity = 0.1))
  
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = "lines",
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = sphere_axis_color, width = 5)
  )
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = T, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  return(fig)
}

plot_dilated_and_rotated_and_translated_sphere()

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
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
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
plot_cylinder_justification <- function(cylinder_color = "#0062c5",
                                        cylinder_axis_color = "#bb0036") {
  
  
  # Get random coords for a cylinder
  p <- 1.5
  theta <- runif(20000, 0, 2 * pi)
  
  x <- p * sin(theta)
  y <- p * cos(theta)
  z <- runif(20000, -3, 3)
  
  # Get axis lines
  x_line_coords <- c(0, 0)
  y_line_coords <- c(0, 0)
  z_line_coords <- c(-4, 4)
  
  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (i in seq(1, length(x), 2)) {
    x_lines <- c(x_lines, x_line_coords[i], x_line_coords[i + 1], NA)
    y_lines <- c(y_lines, y_line_coords[i], y_line_coords[i + 1], NA)
    z_lines <- c(z_lines, z_line_coords[i], z_line_coords[i + 1], NA)
  }
  
  fig <- add_trace(plot_ly(),
                   type = "scatter3d",
                   mode = "markers",
                   x = ~x,
                   y = ~y,
                   z = ~z,
                   marker = list(color = cylinder_color, size = 1, opacity = 0.1))
  
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = "lines",
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = cylinder_axis_color, width = 5)
  )
  
  # Add extra points
  x_extra <- c(0, 1.5, 4)
  y_extra <- c(1, -1.5, 0)
  z_extra <- c(0, 4, -1)
  
  fig <- add_trace(fig,
                   type = "scatter3d",
                   mode = "markers",
                   x = ~x_extra,
                   y = ~y_extra,
                   z = ~z_extra,
                   marker = list(size = 10, color = c("#f77e3b", "#bb0036", "#9437a8")))
  
  
  x_extra <- c(0, 1.5, 4)
  y_extra <- c(1, -1.5, 0)
  z_extra <- c(0, 4, -1)
  
  
  # Add extra lines
  x_extra_line_coords <- c(0, 0, 1.5, 1.5, 0, 4)
  y_extra_line_coords <- c(0, 1, -1.5, -1.5, 0, 0)
  z_extra_line_coords <- c(0, 0, 3, 4, -1, -1)
  
  # Create coordinate vectors with NA between segments
  x_extra_lines <- y_extra_lines <- z_extra_lines <- c()
  for (i in seq(1, length(x), 2)) {
    x_extra_lines <- c(x_extra_lines, x_extra_line_coords[i], x_extra_line_coords[i + 1], NA)
    y_extra_lines <- c(y_extra_lines, y_extra_line_coords[i], y_extra_line_coords[i + 1], NA)
    z_extra_lines <- c(z_extra_lines, z_extra_line_coords[i], z_extra_line_coords[i + 1], NA)
  }
  
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = "lines",
    x = ~x_extra_lines,
    y = ~y_extra_lines,
    z = ~z_extra_lines,
    line = list(color = "black", width = 5)
  )
  
  # Get planes
  index <- 1
  slice_positions <- c(-3, 3)
  for (slice_position in slice_positions) {
    
    slice_position <- slice_position
    
    vertices <- data.frame(x = rep(rep(c(-2, 2), each = 2), 2),
                           y = rep(c(-2, 2), 4),
                           z = rep(slice_position, each = 4))
    
    faces_temp <- data.frame(i = c(1, 4, 5, 8, 1, 6, 1, 1, 4, 4, 2, 2),
                             j = c(2, 2, 6, 6, 5, 5, 3, 5, 8, 3, 4, 6),
                             k = c(3, 3, 7, 7, 2, 2, 7, 7, 7, 7, 8, 8))
    
    fig <- fig %>%
      add_trace(
        type = 'mesh3d',
        x = vertices[, 1], 
        y = vertices[, 2], 
        z = vertices[, 3],
        i = faces_temp[, 1] - 1, 
        j = faces_temp[, 2] - 1, 
        k = faces_temp[, 3] - 1,
        opacity = 0.5,
        facecolor = rep("#007128", nrow(faces_temp))
      )
    
    index <- index + 1
  }
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = T, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  return(fig)
}

plot_cylinder_justification()



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
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
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
# 1. Points in a sphere
get_points_in_a_sphere <- function(n, r) {
  n <- 20
  r <- 3
  x <- runif(n, -r, r)
  y <- runif(n, -r, r)
  z <- runif(n, -r, r)
  chosen_points <- x^2 + y^2 + z^2 < r^2
  x <- x[chosen_points]
  y <- y[chosen_points]
  z <- z[chosen_points]
  
  return(data.frame(x = x, y = y, z = z))
}

plot_points_in_a_sphere <- function(points, 
                                    sphere_color = "#01478c") {
  
  x <- points$x
  y <- points$y
  z <- points$z
  
  
  fig <- add_trace(plot_ly(),
                   type = "scatter3d",
                   mode = "markers",
                   x = ~x,
                   y = ~y,
                   z = ~z,
                   marker = list(color = "black", size = 10))
  
  # Add sphere
  x_sphere <- 0
  y_sphere <- 0
  z_sphere <- 0
  
  fig <- add_trace(fig,
                   type = "scatter3d",
                   mode = 'markers',
                   x = ~x_sphere, 
                   y = ~y_sphere, 
                   z = ~z_sphere,
                   marker = list(size = 120, 
                                 color = sphere_color, 
                                 opacity = 0.5,
                                 line = list(
                                   color = "black",  # border color
                                   width = 10         # border thickness
                                 ))
  )
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = T, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  return(fig)
}


points <- get_points_in_a_sphere(20, 3)
plot_points_in_a_sphere(points)

# 2. Points all connected with MST
plot_points_connected_with_MST <- function(points) {
  
  x <- points$x
  y <- points$y
  z <- points$z
  
  
  fig <- add_trace(plot_ly(),
                   type = "scatter3d",
                   mode = "markers",
                   x = ~x,
                   y = ~y,
                   z = ~z,
                   marker = list(color = "black", size = 10))
  
  # add connections
  v <- 1:length(x)
  
  # all permutations of size 2
  perm <- permutations(n = length(v), r = 2, v = v)
  
  x_line_coords <- y_line_coords <- z_line_coords <- c()
  for (i in seq(nrow(perm))) {
    x_line_coords <- c(x_line_coords, x[perm[i, ]])
    y_line_coords <- c(y_line_coords, y[perm[i, ]])
    z_line_coords <- c(z_line_coords, z[perm[i, ]])
  }

  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (i in seq(1, length(x_line_coords), 2)) {
    x_lines <- c(x_lines, x_line_coords[i], x_line_coords[i + 1], NA)
    y_lines <- c(y_lines, y_line_coords[i], y_line_coords[i + 1], NA)
    z_lines <- c(z_lines, z_line_coords[i], z_line_coords[i + 1], NA)
  }
  
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = "lines",
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = "lightgray", width = 6)
  )
  
  
  # Add MST in different color
  prims_algorithm <- function(graph) {
    
    # Number of vertices is number of points
    num_vertices <- nrow(graph)
    
    # Start with no vertices selected except first
    selected <- rep(FALSE, num_vertices)
    selected[1] <- TRUE
    
    # Create tree_edge matrix. Currently zero, each row represents the two vertices the edge joins
    tree_edges <- matrix(0, 
                         nrow = num_vertices - 1,
                         ncol = 2)
    
    # Iterate until we select enough edges (one less than the number of vertices for a MST)
    num_edges <- 0
    while (num_edges < num_vertices - 1) {
      # Set initial temp values for weight and vertex
      min_weight <- Inf
      min_vertex <- -1
      
      # Iterate through each currently selected vertex
      for (i in seq(num_vertices)) {
        
        # Found a currently selected vertex
        if (selected[i] == TRUE) {
          
          # Iterate through each unselected vertex and find the nearest one
          for (j in seq(num_vertices)) {
            if (!selected[j] && graph[i, j] < min_weight) {
              min_weight <- graph[i, j]
              min_vertex <- j
              curr_vertex <- i
            }
          }
        }
      }
      
      # Current edge connects the min_vertex and curr_vertex
      tree_edges[num_edges + 1, ] <- c(min_vertex, curr_vertex)
      selected[min_vertex] <- TRUE
      num_edges <- num_edges + 1
    }
    return(tree_edges)
  }
  
  adj_mat <- -1 * apcluster::negDistMat(points)
  tree_edges <- prims_algorithm(adj_mat)

  x_MST_coords <- y_MST_coords <- z_MST_coords <- c()
  for (i in seq(nrow(tree_edges))) {
    x_MST_coords <- c(x_MST_coords, x[tree_edges[i, ]])
    y_MST_coords <- c(y_MST_coords, y[tree_edges[i, ]])
    z_MST_coords <- c(z_MST_coords, z[tree_edges[i, ]])
  }
  
  # Create coordinate vectors with NA between segments
  x_MST_lines <- y_MST_lines <- z_MST_lines <- c()
  for (i in seq(1, length(x_MST_coords), 2)) {
    x_MST_lines <- c(x_MST_lines, x_MST_coords[i], x_MST_coords[i + 1], NA)
    y_MST_lines <- c(y_MST_lines, y_MST_coords[i], y_MST_coords[i + 1], NA)
    z_MST_lines <- c(z_MST_lines, z_MST_coords[i], z_MST_coords[i + 1], NA)
  }
  
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = "lines",
    x = ~x_MST_lines,
    y = ~y_MST_lines,
    z = ~z_MST_lines,
    line = list(color = "#bb0036", width = 6)
  )
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = T, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  return(fig)
}

# library(gtools)
plot_points_connected_with_MST(points)

# 3. Points conected via MST only
plot_points_with_MST_only <- function(points) {
  
  x <- points$x
  y <- points$y
  z <- points$z
  
  fig <- add_trace(plot_ly(),
                   type = "scatter3d",
                   mode = "markers",
                   x = ~x,
                   y = ~y,
                   z = ~z,
                   marker = list(color = "black", size = 10))
  
  # Add MST in different color
  prims_algorithm <- function(graph) {
    
    # Number of vertices is number of points
    num_vertices <- nrow(graph)
    
    # Start with no vertices selected except first
    selected <- rep(FALSE, num_vertices)
    selected[1] <- TRUE
    
    # Create tree_edge matrix. Currently zero, each row represents the two vertices the edge joins
    tree_edges <- matrix(0, 
                         nrow = num_vertices - 1,
                         ncol = 2)
    
    # Iterate until we select enough edges (one less than the number of vertices for a MST)
    num_edges <- 0
    while (num_edges < num_vertices - 1) {
      # Set initial temp values for weight and vertex
      min_weight <- Inf
      min_vertex <- -1
      
      # Iterate through each currently selected vertex
      for (i in seq(num_vertices)) {
        
        # Found a currently selected vertex
        if (selected[i] == TRUE) {
          
          # Iterate through each unselected vertex and find the nearest one
          for (j in seq(num_vertices)) {
            if (!selected[j] && graph[i, j] < min_weight) {
              min_weight <- graph[i, j]
              min_vertex <- j
              curr_vertex <- i
            }
          }
        }
      }
      
      # Current edge connects the min_vertex and curr_vertex
      tree_edges[num_edges + 1, ] <- c(min_vertex, curr_vertex)
      selected[min_vertex] <- TRUE
      num_edges <- num_edges + 1
    }
    return(tree_edges)
  }
  
  adj_mat <- -1 * apcluster::negDistMat(points)
  tree_edges <- prims_algorithm(adj_mat)
  
  x_MST_coords <- y_MST_coords <- z_MST_coords <- c()
  for (i in seq(nrow(tree_edges))) {
    x_MST_coords <- c(x_MST_coords, x[tree_edges[i, ]])
    y_MST_coords <- c(y_MST_coords, y[tree_edges[i, ]])
    z_MST_coords <- c(z_MST_coords, z[tree_edges[i, ]])
  }
  
  # Create coordinate vectors with NA between segments
  x_MST_lines <- y_MST_lines <- z_MST_lines <- c()
  for (i in seq(1, length(x_MST_coords), 2)) {
    x_MST_lines <- c(x_MST_lines, x_MST_coords[i], x_MST_coords[i + 1], NA)
    y_MST_lines <- c(y_MST_lines, y_MST_coords[i], y_MST_coords[i + 1], NA)
    z_MST_lines <- c(z_MST_lines, z_MST_coords[i], z_MST_coords[i + 1], NA)
  }
  
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = "lines",
    x = ~x_MST_lines,
    y = ~y_MST_lines,
    z = ~z_MST_lines,
    line = list(color = "#bb0036", width = 6)
  )
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = T, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(-5, 5),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  return(fig)
}

plot_points_with_MST_only(points)

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
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
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
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
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
