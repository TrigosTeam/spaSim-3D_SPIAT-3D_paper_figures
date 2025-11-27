### 0. Example simulation ----
### 1. Cell colocalisation metrics plots ----

# 1.1. First plot - cells with black lines connecting the cells ----
x          <- c(150, 350, 100, 300, 150)
y          <- c(400, 350, 200, 150, 350)
z          <- c(050, 100, 500, 400, 350)
cell_types <- c('B', 'A', 'B', 'A', 'A')

# Define pairs of points to connect
pairs <- list(c(1, 2), c(3, 4), c(3, 5))  # Connect point 1 to 2, 3 to 4, and 3 to 5

# Create coordinate vectors with NA between segments
x_lines <- y_lines <- z_lines <- c()
for (p in pairs) {
  x_lines <- c(x_lines, x[p[1]], x[p[2]], NA)
  y_lines <- c(y_lines, y[p[1]], y[p[2]], NA)
  z_lines <- c(z_lines, z[p[1]], z[p[2]], NA)
}

# Add lines
trace_lines <- add_trace(
  plot_ly(),
  type = "scatter3d",
  mode = 'lines',
  x = ~x_lines,
  y = ~y_lines,
  z = ~z_lines,
  line = list(color = "black", width = 3)
  )

# Add markers
trace_markers <- add_trace(
  trace_lines,
  type = "scatter3d",
  mode = 'markers',
  x = ~x, 
  y = ~y, 
  z = ~z,
  marker = list(size = 10, color = ifelse(cell_types == "A", "#bb0036", "#48bbff"))
)

# Adjust
trace_markers <- trace_markers %>% layout(scene = list(xaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, gridwidth = 5, 
                                                                    titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                                       yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, gridwidth = 5,
                                                                    titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                                       zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, gridwidth = 5,
                                                                    titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                                       aspectmode = 'cube'))

# Plot
trace_markers


# 1.2. Second plot - cells with different colored lines connecting the cells with spheres ----
x          <- c(300, 325, 050, 200, 200)
y          <- c(200, 325, 050, 300, 350)
z          <- c(400, 325, 450, 300, 150)
cell_types <- c('A', 'A', 'B', 'B', 'B')


# Define pairs of points to connect
# Connect point 1 to 2, 1 to 3, 1 to 4, 2 to 4 and 2 to 5
pairs <- list(c(1, 2, '#770026'), c(1, 3, '#0062c5'), c(1, 4, '#0062c5'), c(2, 4, '#0062c5'), c(2, 5, '#0062c5'))  

# Create coordinate vectors with NA between segments
x_lines <- y_lines <- z_lines <- color_lines <- c()
for (p in pairs) {
  x_lines <- c(x_lines, x[as.numeric(p[1])], x[as.numeric(p[2])], NA)
  y_lines <- c(y_lines, y[as.numeric(p[1])], y[as.numeric(p[2])], NA)
  z_lines <- c(z_lines, z[as.numeric(p[1])], z[as.numeric(p[2])], NA)
  color_lines <- c(color_lines, p[3], p[3], 'black')
}

# Add lines
trace_lines <- add_trace(
  plot_ly(),
  type = "scatter3d",
  mode = 'lines',
  x = ~x_lines,
  y = ~y_lines,
  z = ~z_lines,
  line = list(width = 10, color = color_lines)
)


# Add markers
trace_markers <- add_trace(
  trace_lines,
  type = "scatter3d",
  mode = 'markers',
  x = ~x, 
  y = ~y, 
  z = ~z,
  marker = list(size = 10, color = ifelse(cell_types == "A", "#bb0036", "#48bbff"))
)

# Add circles
trace_circles <- add_trace(
  trace_markers,
  type = "scatter3d",
  mode = 'markers',
  x = ~x[cell_types == "A"], 
  y = ~y[cell_types == "A"], 
  z = ~z[cell_types == "A"],
  marker = list(size = 120, 
                color = 'darkgray', 
                opacity = 0.2,
                line = list(
                  color = "black",  # border color
                  width = 10         # border thickness
                ))
)

# Adjust
trace_circles <- trace_circles %>% layout(scene = list(xaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, gridwidth = 5, 
                                                                    titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                                       yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, gridwidth = 5,
                                                                    titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                                       zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, gridwidth = 5,
                                                                    titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                                       aspectmode = 'cube'))

# Plot
trace_circles


### 2. Spatial heterogeneity metrics plots ----
# Alter plot_cells3D function


# 2.1. Simulation ----
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
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     aspectmode = "cube"))
  
  return(fig)
}


bg_metadata <- spe_metadata_background_template("random")
bg_metadata$background$n_cells <- 10000
bg_metadata$background$length <- 500
bg_metadata$background$width <- 500
bg_metadata$background$height <- 500
bg_metadata$background$minimum_distance_between_cells <- 10
bg_metadata$background$cell_types <- 'O'
bg_metadata$background$cell_proportions <- 1

cluster_metadata <- spe_metadata_cluster_template("regular", "ellipsoid", bg_metadata)
cluster_metadata$cluster_1$cluster_cell_types <- c('A', 'B')
cluster_metadata$cluster_1$cluster_cell_proportions <- c(0.2, 0.8)
cluster_metadata$cluster_1$radii <- c(140, 140, 225)
cluster_metadata$cluster_1$axes_rotation <- c(-45, 45, 0)
cluster_metadata$cluster_1$centre_loc <- c(250, 250, 250)

cluster_metadata <- spe_metadata_cluster_template("regular", "sphere", cluster_metadata)
cluster_metadata$cluster_2$cluster_cell_types <- c('A', 'B')
cluster_metadata$cluster_2$cluster_cell_proportions <- c(0.8, 0.2)
cluster_metadata$cluster_2$radius <- 150
cluster_metadata$cluster_2$centre_loc <- c(300, 200, 300)

spe_cluster <- simulate_spe_metadata3D(cluster_metadata)
plot_cells3D(spe_cluster, plot_cell_types = c('A', 'B'), plot_colours = c('#f77e3b', '#48bbff'))

# 2.2. Grid ----
plot_cells_with_grid3D <- function(spe,
                                   plot_cell_types = NULL,
                                   plot_colours = NULL,
                                   grid_color,
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
                   marker = list(size = 2))
  
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
  
  return(fig)
}

plot_cells_with_grid3D(spe_cluster, plot_cell_types = c('A', 'B'), plot_colours = c('#f77e3b', '#48bbff'), grid_color = "#007128")

# 2.3. Spatial heterogenity metric ----
# Alter plot_grid_metrics_continuous3D function
plot_grid_metrics_continuous3D <- function(grid_metrics, metric_colname) {
  
  ## Check input parameters
  if (!(is.character(metric_colname) && metric_colname %in% c("proportion", "entropy"))) {
    stop("`metric_colname` is not 'proportion' or 'entropy'.")
  }
  if (is.null(grid_metrics[[metric_colname]])) {
    stop("`metric_colname` is not a column in `grid_metrics`.")
  }
  
  ## Color of each dot is related to its entropy
  pal <- colorRampPalette(terrain.colors(10, rev = TRUE)[2:10])
  
  ## Add size column and for NA entropy values, make the size small
  grid_metrics$size <- ifelse(is.na(grid_metrics[[metric_colname]]), 0, 10)
  
  fig <- plot_ly(grid_metrics,
                 type = "scatter3d",
                 mode = 'markers',
                 x = ~x_coord,
                 y = ~y_coord,
                 z = ~z_coord,
                 color = as.formula(paste0('~', metric_colname)),
                 colors = pal(nrow(grid_metrics)),
                 marker = list(size = ~size),
                 symbol = 1,
                 symbols = "square")
  
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     aspectmode = "cube"))
  
  return(fig)
}

calculate_cell_proportion_grid_metrics3D(spe_cluster, 10, 'A', 'B')


### 3. Cell clustering algorithms plots ----

# 3.1. Simulation ----
# Alter plot_cells3D function
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
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     aspectmode = "cube"))
  
  return(fig)
}


bg_metadata <- spe_metadata_background_template("random")
cluster_metadata <- spe_metadata_cluster_template("ring", "ellipsoid", bg_metadata)
cluster_metadata <- spe_metadata_cluster_template("regular", "network", cluster_metadata)

cluster_metadata$background$n_cells <- 10000
cluster_metadata$background$length <- 500
cluster_metadata$background$width <- 500
cluster_metadata$background$height <- 500
cluster_metadata$background$minimum_distance_between_cells <- 10
cluster_metadata$background$cell_types <- c('A', 'B', 'O')
cluster_metadata$background$cell_proportions <- c(0.01, 0.01, 0.98)

cluster_metadata$cluster_1$cluster_cell_types <- c('A', 'B')
cluster_metadata$cluster_1$cluster_cell_proportions <- c(0.8, 0.2)
cluster_metadata$cluster_1$radii <- c(100, 100, 180)
cluster_metadata$cluster_1$axes_rotation <- c(-0, 80, -20)
cluster_metadata$cluster_1$centre_loc <- c(200, 350, 125)
cluster_metadata$cluster_1$ring_cell_types <- c('A', 'B')
cluster_metadata$cluster_1$ring_cell_proportions <- c(0, 1)
cluster_metadata$cluster_1$ring_width <- 25

cluster_metadata$cluster_2$cluster_cell_types <- c('A', 'B')
cluster_metadata$cluster_2$cluster_cell_proportions <- c(0.8, 0.2)
cluster_metadata$cluster_2$radius <- 200
cluster_metadata$cluster_2$centre_loc <- c(150, 150, 400)
cluster_metadata$cluster_2$n_edges <- 20
cluster_metadata$cluster_2$width <- 50

spe_cluster <- simulate_spe_metadata3D(cluster_metadata)
plot_cells3D(spe_cluster, plot_cell_types = c('A', 'B'), plot_colours = c('#f77e3b', '#48bbff'))

# 3.2. Clustering algorithm ----
spe_ah <- alpha_hull_clustering3D(spe_cluster, 'A', 28, 100)

# Adjust plot_alpha_hull_cluster3D function to change colors
plot_alpha_hull_clusters_updated3D <- function(spe_with_alpha_hull, 
                                       plot_cell_types = NULL,
                                       plot_colours = NULL,
                                       alpha_hull_colours,
                                       feature_colname = "Cell.Type") {
  
  # Check input parameters
  if (class(spe_with_alpha_hull) != "SpatialExperiment") {
    stop("`spe_with_alpha_hull` is not a SpatialExperiment object.")
  }
  if (!is.character(feature_colname)) {
    stop("`feature_colname` is not a character.")
  }
  if (is.null(spe_with_alpha_hull[[feature_colname]])) {
    stop(paste("No column called", feature_colname, "found in spe object."))
  }
  
  ## If no cell types chosen, use all cell types found in data frame
  if (is.null(plot_cell_types)) plot_cell_types <- unique(spe_with_alpha_hull[[feature_colname]])
  
  ## If cell types have been chosen, check they are found in the spe object
  unknown_cell_types <- setdiff(plot_cell_types, spe_with_alpha_hull[[feature_colname]])
  if (length(unknown_cell_types) != 0) {
    stop(paste("The following plot_cell_types are not found in the spe object:\n   ",
               paste(unknown_cell_types, collapse = ", ")))
  }
  
  ## If no colours inputted, use rainbow palette
  if (is.null(plot_colours)) {
    plot_colours <- rainbow(length(plot_cell_types))
  }
  
  ## User inputs mismatching cell types and colours
  if (length(plot_cell_types) != length(plot_colours)) {
    stop("Length of plot_cell_types is not equal to length of plot_colours")
  }
  
  ## Convert spe object to data frame
  df <- data.frame(spatialCoords(spe_with_alpha_hull), "Cell.Type" = spe_with_alpha_hull[[feature_colname]])
  
  ## Factor for feature column
  df[["Cell.Type"]] <- factor(df[, "Cell.Type"],
                              levels = plot_cell_types)
  
  ## Add points to fig
  fig <- plot_ly() %>%
    add_trace(
      data = df,
      type = "scatter3d",
      mode = 'markers',
      x = ~Cell.X.Position,
      y = ~Cell.Y.Position,
      z = ~Cell.Z.Position,
      marker = list(size = 2),
      color = ~Cell.Type,
      colors = plot_colours
    )
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     aspectmode = "cube"))
  
  
  ## Get alpha hull numbers (ignoring 0)
  alpha_hull_clusters <- spe_with_alpha_hull$alpha_hull_cluster[spe_with_alpha_hull$alpha_hull_cluster != 0]
  
  # Get number of alpha hulls
  n_alpha_hulls <- length(unique(alpha_hull_clusters))
  
  vertices <- spe_with_alpha_hull@metadata$alpha_hull$vertices
  faces <- data.frame(spe_with_alpha_hull@metadata$alpha_hull$faces)

  ## Add alpha hulls to fig, one by one  
  for (i in seq(n_alpha_hulls)) {
    faces_temp <- faces[faces[ , 1] %in% which(alpha_hull_clusters == i) , ]
    
    ## Ignore the weird cases where some cells represent clusters, but no faces are associated with them??
    if (nrow(faces_temp) == 0) next
    
    opacity_level <- 0.20
    
    fig <- fig %>%
      add_trace(
        type = 'mesh3d',
        x = vertices[, 1], 
        y = vertices[, 2], 
        z = vertices[, 3],
        i = faces_temp[, 1] - 1, 
        j = faces_temp[, 2] - 1, 
        k = faces_temp[, 3] - 1,
        opacity = opacity_level,
        facecolor = rep(alpha_hull_colours[i], nrow(faces_temp))
      )
  }
  
  return(fig)
}

plot_alpha_hull_clusters_updated3D(spe_ah, 
                                   plot_cell_types = c('A', 'B'), 
                                   plot_colours = c('#f77e3b', '#48bbff'), 
                                   alpha_hull_colours = c("#d99dff", "#4deeac"))


